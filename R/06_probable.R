# 06_probable.R
#
# Audit of the "probable" category. It is reported as zero in most state-weeks,
# and the paper argues it tracks whether a usable specimen was obtained rather
# than diagnostic certainty. Before writing that, we need to know whether the
# field is a dead column or used unevenly: which states and years ever record a
# probable case.
#
# The second half of the audit is manual: the case-definition box in each
# year's sitrep, to see whether the probable wording changed. This script only
# produces the table to check against.
#
# Note: the extraction protocol records a death described as a probable case as
# not a death, so probable deaths are only recoverable from the sitreps.

library(dplyr)
library(tidyr)
library(readr)
library(here)

source(here("R", "00_functions.R"))

state_week <- read_output("intermediate", "state_week")


# Probable cases by state and season ---------------------------------------------

# For each state-season: weeks where probable is known, weeks with a non-zero
# value, and the total. NA where the season never breaks probable down by state.
probable_state <- state_week |>
  filter(reported) |>
  summarise(weeks_known = sum(!is.na(probable)),
            weeks_nonzero = sum(!is.na(probable) & probable > 0),
            probable = sum_or_na(probable),
            .by = c(year, state)) |>
  filter(weeks_nonzero > 0) |>
  arrange(year, state)


# By season --------------------------------------------------------------------------

# How many states use the field at all each season, and in how many state-weeks.
probable_season <- state_week |>
  filter(reported) |>
  summarise(state_weeks_known = sum(!is.na(probable)),
            state_weeks_nonzero = sum(!is.na(probable) & probable > 0),
            states_using = n_distinct(state[!is.na(probable) & probable > 0]),
            probable = sum_or_na(probable),
            .by = year) |>
  mutate(broken_down_by_state = state_weeks_known > 0) |>
  arrange(year)


# Save ---------------------------------------------------------------------------------

save_table(probable_state, "probable_by_state")
save_table(probable_season, "probable_by_season")

message("State-seasons with any probable case: ", nrow(probable_state))
message("Seasons where probable is broken down by state: ", paste(probable_season$year[probable_season$broken_down_by_state], collapse = ", "))
