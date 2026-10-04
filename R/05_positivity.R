# 05_positivity.R
#
# Suspected-case positivity: confirmed / suspected, by state-season and
# nationally. A proxy, not test positivity. `suspected` counts cases meeting the
# surveillance case definition, not specimens tested, so the ratio folds the
# decision to sample a suspected case into the test result. That sampling
# decision is itself one of the mechanisms under study, so the proxy is flagged
# wherever it is used.
#
# Suspected cases are not broken down by state in 2018-2019, and only partly in
# 2017 and early 2020. Those state-seasons carry NA rather than being dropped,
# so the gaps stay visible. Each ratio uses only weeks where both counts are
# known, so numerator and denominator cover the same weeks.

library(dplyr)
library(tidyr)
library(readr)
library(here)

source(here("R", "00_functions.R"))

state_week <- read_output("intermediate", "state_week")
blocks <- read_output("intermediate", "state_blocks")


# State-weeks where both counts are known ---------------------------------------

units <- bind_rows(state_week |> filter(reported) |> select(year, epi_week, state, suspected, confirmed),
                   blocks |> transmute(year, epi_week = end_week, state, suspected, confirmed))

known <- units |>
  filter(!is.na(suspected), !is.na(confirmed))


# By state and season --------------------------------------------------------------

# Weeks with both counts known, out of the weeks reported, so a ratio built on
# a fraction of the season is easy to spot.
weeks_known <- units |>
  summarise(weeks_reported = n(), weeks_known = sum(!is.na(suspected) & !is.na(confirmed)), .by = c(year, state))

positivity_state <- known |>
  summarise(suspected = sum(suspected), confirmed = sum(confirmed), .by = c(year, state)) |>
  right_join(weeks_known, by = c("year", "state")) |>
  mutate(positivity = if_else(!is.na(suspected) & suspected > 0, confirmed / suspected, NA_real_),
         for_inference = year %in% inference_years) |>
  arrange(state, year)


# Nationally -------------------------------------------------------------------------

positivity_national <- known |>
  summarise(suspected = sum(suspected), confirmed = sum(confirmed), .by = year) |>
  right_join(units |> distinct(year, epi_week) |> count(year, name = "weeks_reported"), by = "year") |>
  left_join(known |> distinct(year, epi_week) |> count(year, name = "weeks_known"), by = "year") |>
  mutate(weeks_known = coalesce(weeks_known, 0L),
         positivity = if_else(!is.na(suspected) & suspected > 0, confirmed / suspected, NA_real_),
         for_inference = year %in% inference_years) |>
  arrange(year)


# Save ---------------------------------------------------------------------------------

save_output(positivity_state, "results", "positivity_state")
save_output(positivity_national, "results", "positivity_national")

message("State-seasons with a positivity value: ", sum(!is.na(positivity_state$positivity)), " of ", nrow(positivity_state))
message("Seasons with national positivity: ", paste(positivity_national$year[!is.na(positivity_national$positivity)], collapse = ", "))
