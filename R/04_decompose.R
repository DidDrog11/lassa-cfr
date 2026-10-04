# 04_decompose.R
#
# Kitagawa split of each adjacent-season change in national reported CFR into:
#   rate        the part due to CFR changing within states
#   composition the part due to cases moving between states with different CFRs
# The two add up exactly to the observed change.
#
# Strata follow the roster rule (see the analysis plan): for each pair of seasons,
# a state is its own stratum if it has at least 30 confirmed cases in both;
# every other state is pooled into "other" for that pair. As a secondary output,
# labelled as noisy, the same split is also run with every state separate, so
# shifts among small states (e.g. the Middle Belt) are visible, with intervals
# so the noise can be seen.
#
# Per-state composition terms are read from `composition_centred`, never
# `composition` (see 00_functions.R).
#
# Uncertainty: cluster bootstrap over state-weeks, resampled within season.
# Primary analysis allocates deaths given only nationally; the sensitivity
# analysis drops those weeks instead.

library(dplyr)
library(tidyr)
library(readr)
library(here)

source(here("R", "00_functions.R"))

state_week <- read_output("intermediate", "state_week")
blocks <- read_output("intermediate", "state_blocks")


# Settings ---------------------------------------------------------------------

# Smoke test: a handful of bootstrap replicates to check the script end to end.
smoke <- as.logical(trimws(Sys.getenv("LASSA_SMOKE", "TRUE")))   # set LASSA_SMOKE=FALSE for the full run
n_boot <- if (smoke) 20 else 1000

roster_min <- 30

# Season window (see the analysis plan). Primary: every season is cut to the
# week the last season reaches in the frozen data (the last sitrep dated on or
# before 1 October 2026, set in 01_load.R), so every pair, including the seven
# reference transitions, compares like with like. Transmission runs roughly
# December to April, so the weeks after the cut hold the quiet months and the
# start of the next season. Sensitivity: full years, with the last season
# necessarily partial.
#
# Deaths given only nationally (2018, 2020) are allocated to states in
# 01_load.R. Sensitivity: those weeks dropped from numerator and denominator
# instead; it only affects transitions involving 2018 or 2020.
runs <- tribble(
  ~run,         ~handling,  ~window,
  "primary",    "allocate", "cut",
  "matched",    "matched",  "cut",
  "full_years", "allocate", "full")


# Units: state-weeks plus state-blocks ---------------------------------------------

# Each row is one state in one week (or in the 2022 multi-week block). These are
# the units resampled by the bootstrap and summed into season totals.
all_units <- bind_rows(state_week |> filter(reported) |> select(year, epi_week, state, confirmed, deaths, deaths_imputed),
                       blocks |> transmute(year, epi_week = end_week, state, confirmed, deaths, deaths_imputed = FALSE)) |>
  filter(year %in% inference_years)

# The roster is picked from each state's full-season confirmed count, as the rule
# defines it and as 02_validate.R reports it, so the deaths handling cannot
# change which states stand alone.
roster_totals <- all_units |>
  summarise(confirmed = sum(confirmed, na.rm = TRUE), .by = c(year, state)) |>
  complete(year = inference_years, state = nigeria_states, fill = list(confirmed = 0))

# The cut week: the last week the final season reached in the frozen data.
last_year <- max(inference_years)
last_week <- max(state_week$epi_week[state_week$year == last_year & state_week$reported])

pairs <- tibble(y0 = head(inference_years, -1), y1 = tail(inference_years, -1)) |>
  mutate(transition = paste0(y0, "-", y1))

rosters <- tibble()
for (i in seq_len(nrow(pairs))) {
  rosters <- bind_rows(rosters, tibble(transition = pairs$transition[i],
                                       state = pair_roster(roster_totals, pairs$y0[i], pairs$y1[i], roster_min)))
}


# Building blocks ---------------------------------------------------------------------

# Season totals by state for one pair of seasons. Every state appears in both
# seasons, with zeros where it had no cases, so the split sees the same set of
# strata on both sides. (The window is applied to the units beforehand.)
pair_totals <- function(units, y0, y1) {
  units |>
    filter(year %in% c(y0, y1)) |>
    summarise(confirmed = sum(confirmed), deaths = sum(deaths), .by = c(year, state)) |>
    complete(year = c(y0, y1), state = nigeria_states, fill = list(confirmed = 0, deaths = 0))
}

# Kitagawa split for one pair, given which states stand alone. One row per
# stratum; the national components are the column sums.
split_pair <- function(totals, y0, y1, roster) {
  strata <- totals |>
    mutate(state = if_else(state %in% roster, state, "other")) |>
    summarise(confirmed = sum(confirmed), deaths = sum(deaths), .by = c(year, state))

  state_contributions(strata, "confirmed", y0, y1)
}

# National components: the stratum terms summed within each transition.
national_components <- function(contributions) {
  contributions |>
    summarise(cfr_from = sum(share_from * cfr_from),
              cfr_to = sum(share_to * cfr_to),
              rate = sum(rate),
              composition = sum(composition_centred),
              .by = transition) |>
    mutate(observed = cfr_to - cfr_from, residual = observed - rate - composition)
}

# Both splits (roster and every state) for every pair, from one set of units.
split_all_pairs <- function(units) {
  roster_rows <- list()
  every_rows <- list()
  for (i in seq_len(nrow(pairs))) {
    totals <- pair_totals(units, pairs$y0[i], pairs$y1[i])
    roster <- rosters$state[rosters$transition == pairs$transition[i]]
    roster_rows[[i]] <- split_pair(totals, pairs$y0[i], pairs$y1[i], roster) |> mutate(transition = pairs$transition[i])
    every_rows[[i]] <- split_pair(totals, pairs$y0[i], pairs$y1[i], nigeria_states) |> mutate(transition = pairs$transition[i])
  }
  list(roster = bind_rows(roster_rows), every_state = bind_rows(every_rows))
}


# Run the decomposition under one deaths handling and season window -----------------------

# Bootstrap: resample state-weeks with replacement within each season (after
# redrawing any allocated deaths), rebuild the totals and re-split. The roster
# is held at its point-estimate membership, so intervals reflect variation in
# the counts, not states crossing the threshold. Limitation for Methods:
# state-weeks are resampled independently, which ignores the correlation
# between a state's consecutive weeks, so intervals are probably somewhat too
# narrow.
run_decomposition <- function(handling, window) {
  units <- all_units
  if (handling == "matched") units <- units |> filter(!deaths_imputed)
  if (window == "cut") units <- units |> filter(epi_week <= last_week)
  units <- units |> filter(!is.na(deaths), !is.na(confirmed))

  # Point estimates.
  point <- split_all_pairs(units)
  decomposition <- national_components(point$roster)
  stopifnot(max(abs(decomposition$residual)) < 1e-10)   # the split is an identity

  # Replicates. National components for the roster split, and per-state terms
  # for the every-state split so its noise can be shown.
  boot_national <- list()
  boot_states <- list()
  for (b in seq_len(n_boot)) {
    resampled <- redraw_allocation(units) |> slice_sample(prop = 1, replace = TRUE, by = year)
    splits <- split_all_pairs(resampled)
    boot_national[[b]] <- national_components(splits$roster) |> mutate(rep = b)
    boot_states[[b]] <- splits$every_state |> select(transition, state, rate, composition_centred) |> mutate(rep = b)
  }
  boot_national <- bind_rows(boot_national)
  boot_states <- bind_rows(boot_states)

  # Percentile intervals and the share of replicates above zero.
  national_ci <- boot_national |>
    pivot_longer(c(observed, rate, composition), names_to = "component", values_to = "value") |>
    summarise(lo = quantile(value, 0.025), hi = quantile(value, 0.975), p_above_zero = mean(value > 0),
              .by = c(transition, component))

  decomposition_long <- decomposition |>
    pivot_longer(c(observed, rate, composition), names_to = "component", values_to = "estimate") |>
    select(transition, component, estimate) |>
    left_join(national_ci, by = c("transition", "component")) |>
    mutate(handling = handling, window = window)

  every_state_ci <- boot_states |>
    pivot_longer(c(rate, composition_centred), names_to = "component", values_to = "value") |>
    summarise(lo = quantile(value, 0.025), hi = quantile(value, 0.975), .by = c(transition, state, component))

  every_state <- point$every_state |>
    select(transition, state, cases_from, cases_to, share_from, share_to, cfr_from, cfr_to, rate, composition_centred) |>
    pivot_longer(c(rate, composition_centred), names_to = "component", values_to = "estimate") |>
    left_join(every_state_ci, by = c("transition", "state", "component")) |>
    mutate(handling = handling, window = window)

  list(decomposition_long = decomposition_long,
       every_state = every_state)
}


# Primary and sensitivity ------------------------------------------------------------------

set.seed(20260929)
results <- list()
for (i in seq_len(nrow(runs))) results[[runs$run[i]]] <- run_decomposition(runs$handling[i], runs$window[i])

primary <- results[["primary"]]
sensitivity_long <- bind_rows(results[["matched"]]$decomposition_long |> mutate(run = "matched"),
                              results[["full_years"]]$decomposition_long |> mutate(run = "full_years"))


# Save ---------------------------------------------------------------------------------

save_output(primary$decomposition_long, "results", "decomposition_ci")
save_output(primary$every_state, "results", "contributions_every_state")
save_output(sensitivity_long, "results", "decomposition_ci_sensitivity")
save_output(rosters, "results", "decomposition_rosters")

message("Transitions: ", nrow(pairs), "; bootstrap replicates: ", n_boot, if (smoke) " (SMOKE TEST)" else "")
message("Seasons cut to week ", last_week, " (primary); sensitivity runs: matched weeks, full years")
