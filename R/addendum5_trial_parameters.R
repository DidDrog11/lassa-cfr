# addendum5_trial_parameters.R
#
# Added by Addendum 5 to the analysis plan (10 November 2026), during revision of
# the paper and after every other result had been seen. Each part tests or
# supports one claim the paper makes; none is a search for new findings.
#
# A10. Where the published 2025-2026 difference sits, by state, at week 36.
#     Tests two claims on the series that actually shows the rise: that a shift
#     of cases between states does not explain it, and that it rests on one
#     state's late-added deaths. Week 36 is the only week for which published 2026
#     state figures were extracted, so the comparison is at a fixed week, not
#     phase-matched.
# A11. What baseline a trial planner would take from the reports. Supports the
#     claim that the CFR one would lift from the reports depends on the series,
#     the year and the state chosen, and that the states one would choose as
#     trial sites change from year to year.
#
# The published figures have no state-weeks to resample, so everything here is a
# point estimate, as in Addendum 3.

library(dplyr)
library(tidyr)
library(readr)
library(here)

source(here("R", "00_functions.R"))

state_week <- read_output("intermediate", "state_week")
blocks <- read_output("intermediate", "state_blocks")
state_season <- read_output("intermediate", "state_season")
national_season <- read_output("results", "national_season")
published_annual <- read_output("results", "published_series_annual")

printed_national <- read_csv(here("data-raw", "lassa_cumulative_national.csv"), show_col_types = FALSE)

# Published state figures: each year's final report from 2020 (for 2026, the week
# 36 report), and the 2025 week 36 report, read for this addendum.
printed_state <- bind_rows(read_csv(here("data-raw", "lassa_cumulative_state.csv"), show_col_types = FALSE),
                           read_csv(here("data-raw", "lassa_cumulative_state_2025_w36.csv"), show_col_types = FALSE) |>
                             select(-cum_suspected)) |>
  mutate(state = if_else(state == "Federal Capital Territory", "FCT", state))

target_year <- max(inference_years)
reference_year <- target_year - 1
cut_week <- max(state_week$epi_week[state_week$year == target_year & state_week$reported])
roster_min <- 30


# A10. Published figures by state at week 36 -------------------------------------------

# Published state figures at the cut week, for both years, with every state
# present in both (a state the table does not list had no cases).
published_w36 <- printed_state |>
  filter(year %in% c(reference_year, target_year), epi_week == cut_week) |>
  select(year, state, confirmed = cum_confirmed, deaths = cum_deaths) |>
  complete(year = c(reference_year, target_year), state = nigeria_states, fill = list(confirmed = 0, deaths = 0))

# Check: the state rows add up to the national figure printed in the same report.
national_w36 <- printed_national |>
  filter(year %in% c(reference_year, target_year), epi_week == cut_week) |>
  select(year, printed_confirmed = cum_confirmed, printed_deaths = cum_deaths)

state_sum_check <- published_w36 |>
  summarise(state_confirmed = sum(confirmed), state_deaths = sum(deaths), .by = year) |>
  left_join(national_w36, by = "year")
print(state_sum_check)
if (any(state_sum_check$state_confirmed != state_sum_check$printed_confirmed |
        state_sum_check$state_deaths != state_sum_check$printed_deaths)) {
  warning("State rows do not add up to the printed national figure; check the state tables.")
}

# The same figures from the weekly reports, summed to the cut week, for comparison
# and to show in which states the published and weekly figures differ.
weekly_w36 <- bind_rows(state_week |> filter(reported) |> select(year, epi_week, state, confirmed, deaths),
                        blocks |> transmute(year, epi_week = end_week, state, confirmed, deaths)) |>
  filter(year %in% c(reference_year, target_year), epi_week <= cut_week) |>
  summarise(confirmed = sum(confirmed, na.rm = TRUE), deaths = sum(deaths, na.rm = TRUE), .by = c(year, state)) |>
  complete(year = c(reference_year, target_year), state = nigeria_states, fill = list(confirmed = 0, deaths = 0))

by_state_w36 <- published_w36 |>
  rename(published_confirmed = confirmed, published_deaths = deaths) |>
  left_join(weekly_w36 |> rename(weekly_confirmed = confirmed, weekly_deaths = deaths), by = c("year", "state")) |>
  mutate(added_confirmed = published_confirmed - weekly_confirmed,
         added_deaths = published_deaths - weekly_deaths) |>
  filter(published_confirmed > 0 | weekly_confirmed > 0) |>
  arrange(year, desc(added_deaths))

# Strata as in A2 for the 2025-2026 pair: a state stands alone if it had at least
# 30 confirmed cases in both full seasons (2026 to the cut week); the rest are
# pooled. Using A2's roster keeps the two splits comparable.
roster <- pair_roster(state_season, reference_year, target_year, roster_min)

# Within-state (rate) and between-state (composition) parts of the 2025-2026
# difference, on the published and on the weekly figures at the cut week.
split_at_w36 <- function(d, series) {
  pooled <- d |>
    mutate(state = if_else(state %in% roster, state, "other")) |>
    summarise(confirmed = sum(confirmed), deaths = sum(deaths), .by = c(year, state))
  terms <- state_contributions(pooled, "confirmed", reference_year, target_year)
  national <- d |>
    summarise(cfr = sum(deaths) / sum(confirmed), .by = year)
  list(by_stratum = terms |> mutate(series = series, .before = 1),
       summary = tibble(series = series,
                        cfr_reference = national$cfr[national$year == reference_year],
                        cfr_target = national$cfr[national$year == target_year],
                        difference = cfr_target - cfr_reference,
                        within_state = sum(terms$rate),
                        between_state = sum(terms$composition)))
}

split_published <- split_at_w36(published_w36, "published")
split_weekly <- split_at_w36(weekly_w36, "weekly")
decomposition_w36 <- bind_rows(split_published$summary, split_weekly$summary)
decomposition_w36_strata <- bind_rows(split_published$by_stratum, split_weekly$by_stratum)

# The national difference with each stand-alone state left out in turn, on the
# published figures and, for comparison, on the weekly figures to the same week.
leave_one_out <- tibble()
for (series in c("published", "weekly")) {
  d <- if (series == "published") published_w36 else weekly_w36
  for (s in c("none", roster)) {
    cfr <- d |>
      filter(state != s) |>
      summarise(cfr = sum(deaths) / sum(confirmed), .by = year)
    leave_one_out <- bind_rows(leave_one_out,
                               tibble(series = series, left_out = s,
                                      cfr_reference = cfr$cfr[cfr$year == reference_year],
                                      cfr_target = cfr$cfr[cfr$year == target_year],
                                      difference = cfr_target - cfr_reference))
  }
}


# A11. Baselines a trial planner would take from the reports ------------------------------

# How the size of a trial with a mortality endpoint depends on the baseline CFR.
# Patients per arm to detect a relative reduction r in the risk of death (normal
# approximation, equal arms) is proportional to
#   [p(1 - p) + q(1 - q)] / (p - q)^2,  with q = (1 - r) p.
# The constant (power and significance level) cancels in a ratio, so the ratio
# between two baselines needs only r. It is shown for r = 25% and 50%; for small
# differences it is close to the ratio of (1 - p) / p.
trial_size_ratio <- function(p_low, p_high, r) {
  n <- function(p) {
    q <- (1 - r) * p
    (p * (1 - p) + q * (1 - q)) / (p - q)^2
  }
  n(p_low) / n(p_high)
}

# National annual CFR on both series. 2026 runs to the cut week.
national_baselines <- national_season |>
  filter(year %in% inference_years) |>
  select(year, confirmed, deaths = deaths_matched, cfr_weekly = cfr_matched) |>
  left_join(published_annual |> select(year, published_week, cfr_published), by = "year") |>
  mutate(partial_year = year == target_year)

# State annual CFR on the weekly series, for states with at least 30 confirmed
# cases that year, with the published year-end figure alongside from 2020.
published_year_end <- printed_state |>
  slice_max(epi_week, n = 1, by = year) |>
  select(year, state, published_confirmed = cum_confirmed, published_deaths = cum_deaths)

state_baselines <- state_season |>
  filter(year %in% inference_years, !is.na(confirmed), confirmed >= roster_min) |>
  transmute(year, state, confirmed, deaths, cfr_weekly = deaths / confirmed) |>
  left_join(published_year_end, by = c("year", "state")) |>
  mutate(cfr_published = published_deaths / published_confirmed,
         partial_year = year == target_year)

# Ranges, and what each range means for trial size. `label` names the year or
# state at each end of the range; `by` lists the grouping columns.
summarise_range <- function(d, value, by) {
  d |>
    filter(!is.na({{ value }})) |>
    summarise(n = n(),
              lowest = min({{ value }}), highest = max({{ value }}),
              where_lowest = paste(label[{{ value }} == min({{ value }})], collapse = ", "),
              where_highest = paste(label[{{ value }} == max({{ value }})], collapse = ", "),
              .by = all_of(by)) |>
    mutate(trial_size_ratio_r25 = trial_size_ratio(lowest, highest, 0.25),
           trial_size_ratio_r50 = trial_size_ratio(lowest, highest, 0.50))
}

# Across years, nationally, on each series (full years 2018-2025, then with 2026).
national_long <- national_baselines |>
  pivot_longer(c(cfr_weekly, cfr_published), names_to = "series", values_to = "cfr", names_prefix = "cfr_") |>
  mutate(label = as.character(year))
range_national <- bind_rows(national_long |> filter(!partial_year) |> mutate(years = "2018-2025"),
                            national_long |> mutate(years = "2018-2026")) |>
  summarise_range(cfr, c("series", "years")) |>
  mutate(comparison = "national, across years", .before = 1)

# Across states within each year (weekly series).
range_within_year <- state_baselines |>
  mutate(label = state) |>
  summarise_range(cfr_weekly, "year") |>
  filter(n >= 2) |>
  mutate(comparison = "across states, within a year", .before = 1)

# Across years within each state (weekly series), for states with at least two
# qualifying years.
range_within_state <- state_baselines |>
  mutate(label = as.character(year)) |>
  summarise_range(cfr_weekly, "state") |>
  filter(n >= 2) |>
  mutate(comparison = "across years, within a state", .before = 1)

baseline_ranges <- bind_rows(range_national, range_within_year, range_within_state)

# Site stability: each state's share of the year's confirmed cases and its rank,
# and which states make up the top three each year (weekly series).
site_shares <- state_season |>
  filter(year %in% inference_years, !is.na(confirmed)) |>
  mutate(share = confirmed / sum(confirmed), rank = min_rank(desc(confirmed)), .by = year) |>
  filter(confirmed > 0) |>
  select(year, state, confirmed, share, rank) |>
  arrange(year, rank)

top_three <- site_shares |>
  filter(rank <= 3) |>
  summarise(top_three = paste(state, collapse = ", "), share_top_three = sum(share), .by = year) |>
  mutate(changed_from_previous = mapply(function(a, b) !setequal(strsplit(a, ", ")[[1]], strsplit(b, ", ")[[1]]),
                                        top_three, lag(top_three, default = top_three[1])))


# Save ----------------------------------------------------------------------------------

save_output(by_state_w36, "results", "addendum5_by_state_w36")
save_output(decomposition_w36, "results", "addendum5_decomposition_w36")
save_output(decomposition_w36_strata, "results", "addendum5_decomposition_w36_strata")
save_output(leave_one_out, "results", "addendum5_leave_one_out")
save_output(national_baselines, "results", "addendum5_national_baselines")
save_output(state_baselines, "results", "addendum5_state_baselines")
save_output(baseline_ranges, "results", "addendum5_baseline_ranges")
save_output(site_shares, "results", "addendum5_site_shares")
save_output(top_three, "results", "addendum5_top_three")

save_table(by_state_w36, "addendum5_by_state_w36")
save_table(decomposition_w36_strata, "addendum5_decomposition_w36_strata")
save_table(state_baselines, "addendum5_state_baselines")
save_table(baseline_ranges, "addendum5_baseline_ranges")
save_table(top_three, "addendum5_top_three")

message("Roster: ", paste(roster, collapse = ", "))
print(decomposition_w36)
print(leave_one_out)
print(baseline_ranges |> filter(comparison != "across years, within a state"), n = Inf, width = Inf)
print(top_three)
