# 03_descriptives.R
#
# The descriptive series the Results open with: national reported CFR by season
# and by week, cumulative and incident, plus the week coverage that qualifies
# each season and the table of which variables the sitreps break down by state.
#
# 2017 is included here as context and marked as a partial year. It is not used
# for inference (see `inference_years` in 00_functions.R). The last season is
# also partial, since it runs only to the latest sitrep.
#
# Reported CFR throughout is deaths among confirmed cases / confirmed cases,
# following NCDC.

library(dplyr)
library(tidyr)
library(readr)
library(here)

source(here("R", "00_functions.R"))

state_week <- read_output("intermediate", "state_week")
state_season <- read_output("intermediate", "state_season")
blocks <- read_output("intermediate", "state_blocks")

core_vars <- c("suspected", "confirmed", "probable", "deaths")

# National weekly counts ---------------------------------------------------------

# Sum states within each week. `n_states_na` counts states whose value is
# unknown that week: where it is above zero but below 37, the national figure
# is a partial sum and is flagged rather than used silently.
# The NA counts come first: summarise works top to bottom, and once `across()`
# has run, `confirmed` and `deaths` hold the national totals, not the states.
national_week <- state_week |>
  summarise(confirmed_states_na = sum(is.na(confirmed)),
            deaths_states_na = sum(is.na(deaths)),
            across(all_of(core_vars), sum_or_na),
            reported = any(reported),
            in_block = any(in_block),
            .by = c(year, epi_week)) |>
  mutate(deaths_partial = deaths_states_na > 0 & deaths_states_na < n_distinct(state_week$state))

# The 2022 weeks 19-23 block enters the cumulative series at its last week
# (see the analysis plan). Its weekly values stay NA; its total is held in
# `block_confirmed` and `block_deaths` on the end week.
national_blocks <- blocks |>
  summarise(block_confirmed = sum_or_na(confirmed), block_deaths = sum_or_na(deaths), .by = c(year, end_week))

national_week <- national_week |>
  left_join(national_blocks, by = c("year", "epi_week" = "end_week")) |>
  mutate(block_confirmed = coalesce(block_confirmed, 0),
         block_deaths = coalesce(block_deaths, 0))


# Cumulative series within each season -------------------------------------------

# A week with no sitrep leaves the running total unknown from that week on: the
# sum of the weeks we have is then a lower bound. `cum_complete` marks where the
# running total is still exact. Block weeks do not break it, because the block
# total is added back at its end week.
# `cum_known` is stricter: it is also FALSE for the block weeks before the end
# week (2022 w19-22), where the running total is not known at all. Plots and
# phase matching should use `cum_known`.
national_cum <- national_week |>
  arrange(year, epi_week) |>
  mutate(gap_week = !reported & !in_block,
         cum_confirmed = cumsum(coalesce(confirmed, 0) + block_confirmed),
         cum_deaths = cumsum(coalesce(deaths, 0) + block_deaths),
         cum_complete = cumsum(gap_week) == 0,
         cum_known = cum_complete & !(in_block & block_confirmed == 0),
         cfr_cumulative = if_else(cum_confirmed > 0, cum_deaths / cum_confirmed, NA_real_),
         cfr_incident = if_else(!is.na(confirmed) & confirmed > 0, deaths / confirmed, NA_real_),
         .by = year)


# Season summary ---------------------------------------------------------------------

# Week coverage for each season, and whether it is used for inference.
coverage <- national_week |>
  summarise(n_weeks = n(),
            weeks_reported = sum(reported),
            weeks_in_block = sum(in_block),
            weeks_missing = sum(!reported & !in_block),
            weeks_deaths_partial = sum(deaths_partial),
            .by = year)

# Deaths given only nationally are allocated to states in
# 01_load.R (see the analysis plan), so in the inference years `cfr` and
# `cfr_matched` agree. They can differ in 2017, where a few weeks give deaths
# with no breakdown and no allocation is made.
# Blocks are added separately: their weekly values are NA, so a filter on known
# deaths would otherwise drop them.
matched_weeks <- national_week |>
  filter(!is.na(deaths), !is.na(confirmed)) |>
  summarise(confirmed_deaths_known = sum(confirmed), deaths_matched = sum(deaths), .by = year)

matched_blocks <- national_blocks |>
  summarise(block_confirmed = sum(block_confirmed), block_deaths = sum(block_deaths), .by = year)

matched <- matched_weeks |>
  left_join(matched_blocks, by = "year") |>
  mutate(confirmed_deaths_known = confirmed_deaths_known + coalesce(block_confirmed, 0),
         deaths_matched = deaths_matched + coalesce(block_deaths, 0)) |>
  select(year, confirmed_deaths_known, deaths_matched)

national_season <- state_season |>
  summarise(across(all_of(core_vars), sum_or_na), .by = year) |>
  left_join(coverage, by = "year") |>
  left_join(matched, by = "year") |>
  mutate(cfr = deaths / confirmed,
         cfr_matched = deaths_matched / confirmed_deaths_known,
         for_inference = year %in% inference_years,
         partial_year = weeks_missing > 0 | year == max(year))


# States reporting ------------------------------------------------------------------

# States with at least one confirmed case in the season: the reporting footprint.
footprint <- state_season |>
  summarise(states_with_cases = sum(!is.na(confirmed) & confirmed > 0),
            states_with_deaths = sum(!is.na(deaths) & deaths > 0),
            .by = year)

national_season <- national_season |>
  left_join(footprint, by = "year")


# Which variables are broken down by state, by year ----------------------------------

# Share of reported state-weeks with a known value. A direct input to the
# evaluability result: it shows what the published reports make available.
availability <- state_week |>
  filter(reported) |>
  summarise(suspected = mean(!is.na(suspected)),
            confirmed = mean(!is.na(confirmed)),
            probable = mean(!is.na(probable)),
            deaths = mean(!is.na(deaths)),
            .by = year) |>
  mutate(across(-year, \(x) round(x, 2)))


# Save ---------------------------------------------------------------------------------

save_output(national_cum, "results", "national_week")
save_output(national_season, "results", "national_season")
save_table(availability, "variable_availability")

message("Seasons: ", nrow(national_season), "; used for inference: ", sum(national_season$for_inference))
message("Season-weeks where the cumulative total is a lower bound: ", sum(!national_cum$cum_complete))
message("Weeks where national deaths are a partial sum: ", sum(national_week$deaths_partial))
