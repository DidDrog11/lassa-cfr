# 00_functions.R -- shared settings and helpers, sourced by every other script.

# Seasons used for inference (decomposition, model, phase matching). 2017 is
# loaded and shown in descriptive series as context only (see the analysis
# plan): 18 of its weeks have no sitrep, including the start and peak of the
# season.
inference_years <- 2018:2026

# Where outputs go. Each script saves only what a later script, the figures or
# the manuscript reads, or what takes long to compute.
#   data/intermediate  analysis-ready data built from the ledger by 01, read by 02-11
#   data/qc            validation output from 02, for checking the extract
#   data/results       one file per result, read by the figures and the manuscript
#   data/models        fitted models from 07, which take long to fit
#   tables             supplementary tables, as CSV
save_output <- function(x, folder, name) {
  dir.create(here::here("data", folder), showWarnings = FALSE, recursive = TRUE)
  saveRDS(x, here::here("data", folder, paste0(name, ".rds")))
}

read_output <- function(folder, name) readRDS(here::here("data", folder, paste0(name, ".rds")))

save_table <- function(x, name) {
  dir.create(here::here("tables"), showWarnings = FALSE)
  readr::write_csv(x, here::here("tables", paste0(name, ".csv")))
}

# Adds up a set of counts, but returns NA rather than 0 when every value is NA,
# so "unknown" never turns into "zero" when counts are combined.
sum_or_na <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

nigeria_states <- c(
  "Abia", "Adamawa", "Akwa Ibom", "Anambra", "Bauchi", "Bayelsa", "Benue",
  "Borno", "Cross River", "Delta", "Ebonyi", "Edo", "Ekiti", "Enugu", "FCT",
  "Gombe", "Imo", "Jigawa", "Kaduna", "Kano", "Katsina", "Kebbi", "Kogi",
  "Kwara", "Lagos", "Nasarawa", "Niger", "Ogun", "Ondo", "Osun", "Oyo",
  "Plateau", "Rivers", "Sokoto", "Taraba", "Yobe", "Zamfara")

# Attach each state's share of the year's cases and its own CFR. A state with no
# cases gets weight 0, so it contributes nothing and the identity still holds.
add_shares <- function(d, denom) {
  d |>
    mutate(cases = .data[[denom]]) |>
    mutate(w = cases / sum(cases), cfr = if_else(cases > 0, deaths / cases, 0), .by = year)
}

# Kitagawa split of one transition, state by state: per-state rate and
# composition terms, which sum to the national components.
#
# `composition` is the raw Kitagawa term and misleads state by state: the share
# changes sum to zero, so what actually decides whether a state pushes the
# national CFR up is whether its own CFR sits above the national mean.
# `composition_centred` subtracts that mean and sums to the same total.
state_contributions <- function(d, denom, y0, y1) {
  s <- add_shares(d, denom)
  a <- s |> filter(year == y0) |> arrange(state)
  b <- s |> filter(year == y1) |> arrange(state)
  cfr_bar <- (sum(a$w * a$cfr) + sum(b$w * b$cfr)) / 2
  tibble(state = a$state, cases_from = a$cases, cases_to = b$cases,
         share_from = a$w, share_to = b$w, cfr_from = a$cfr, cfr_to = b$cfr,
         rate = (b$cfr - a$cfr) * (b$w + a$w) / 2,
         composition = (b$w - a$w) * (b$cfr + a$cfr) / 2,
         composition_centred = (b$w - a$w) * ((b$cfr + a$cfr) / 2 - cfr_bar))
}

# --- Redraw deaths allocated from a national total (see the analysis plan) --------
# In a few weeks deaths were given only nationally. 01_load.R shares them among
# states in proportion to that week's confirmed cases. For the bootstrap and the
# model the share is redrawn: each death is assigned to one of that week's
# confirmed cases, drawn without replacement, so a state never gets more deaths
# in the week than it has confirmed cases. The expected share is the same as the
# proportional allocation. `units` needs year, epi_week, confirmed, deaths and
# deaths_imputed.
redraw_allocation <- function(units) {
  imputed_weeks <- units |> filter(deaths_imputed) |> distinct(year, epi_week)

  for (i in seq_len(nrow(imputed_weeks))) {
    rows <- which(units$deaths_imputed & units$year == imputed_weeks$year[i] & units$epi_week == imputed_weeks$epi_week[i])
    total <- round(sum(units$deaths[rows]))

    # One entry per confirmed case, labelled by its row; draw `total` of them.
    cases <- rep(rows, times = units$confirmed[rows])
    # sample.int, not sample(cases, ...): with a single case, sample() would draw
    # from 1:cases instead of from the case itself.
    drawn <- cases[sample.int(length(cases), total, replace = FALSE)]
    units$deaths[rows] <- tabulate(match(drawn, rows), nbins = length(rows))
  }
  units
}


# --- Units and bootstrap replicates, shared by 08 and 10 -------------------------
# The inference units: one row per state per reported week, plus the 2022
# multi-week block entered at its end week. Weeks with an unknown deaths or
# confirmed value are dropped.
analysis_units <- function(state_week, blocks) {
  bind_rows(state_week |> filter(reported) |> select(year, epi_week, state, suspected, confirmed, deaths, deaths_imputed),
            blocks |> transmute(year, epi_week = end_week, state, suspected, confirmed, deaths, deaths_imputed = FALSE)) |>
    filter(year %in% inference_years, !is.na(confirmed), !is.na(deaths))
}

# One bootstrap replicate: redraw allocated deaths, then resample state-weeks
# with replacement within each season. The same scheme as 04_decompose.R, so
# every interval in the paper comes from one resampling design.
resample_units <- function(units) {
  redraw_allocation(units) |> slice_sample(prop = 1, replace = TRUE, by = year)
}

# National running totals by season and week, from a set of units.
national_running_totals <- function(units) {
  units |>
    summarise(confirmed = sum(confirmed), deaths = sum(deaths), .by = c(year, epi_week)) |>
    arrange(year, epi_week) |>
    mutate(cum_confirmed = cumsum(confirmed), cum_deaths = cumsum(deaths), .by = year)
}


# --- Roster per pair of years (see the analysis plan) -----------------------------
# The Kitagawa split compares two adjacent years. For each such pair, a state is
# its own stratum if it has at least `min_confirmed` confirmed cases in BOTH
# years; every other state is pooled into "other" for that pair. The threshold
# is on confirmed cases, not deaths, so strata are never chosen on the outcome.

# States that stand alone for the transition y0 -> y1.
pair_roster <- function(state_season, y0, y1, min_confirmed = 30) {
  state_season |>
    filter(year %in% c(y0, y1)) |>
    summarise(clears = n() == 2 & all(!is.na(confirmed) & confirmed >= min_confirmed), .by = state) |>
    filter(clears) |>
    pull(state) |>
    sort()
}

# Collapse a state x season table for one pair of years to the roster states
# plus "other". Counts are summed, so national totals are unchanged.
pool_to_roster <- function(state_season, y0, y1, roster) {
  state_season |>
    filter(year %in% c(y0, y1)) |>
    mutate(state = if_else(state %in% roster, state, "other")) |>
    summarise(across(c(suspected, confirmed, probable, deaths), sum_or_na), .by = c(year, state))
}
