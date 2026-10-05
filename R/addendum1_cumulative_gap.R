# addendum1_cumulative_gap.R
#
# A9, added by Addendum 1 to the analysis plan (5 October 2026), after the plan
# was fixed: printed cumulative figures against summed weekly figures.
#
# Each report prints the figures for its week and a running total for the year.
# Deaths added to the running total without appearing in any week's figures
# make the two disagree. This script measures the difference
#   - nationally, at every report: printed cumulative minus the running sum of
#     the weekly figures, and how much the difference grows from one report to
#     the next (the weeks in which deaths were added);
#   - by state, at each year's final report (2020 onwards, when reports carry a
#     state table).
# Descriptive only: counts, no intervals, no tests.
#
# Run after 01 and 03. Reads the printed cumulative figures from data-raw/.

library(dplyr)
library(readr)
library(here)

source(here("R", "00_functions.R"))


# Read ---------------------------------------------------------------------------------

printed_national <- read_csv(here("data-raw", "lassa_cumulative_national.csv"), show_col_types = FALSE)
printed_state <- read_csv(here("data-raw", "lassa_cumulative_state.csv"), show_col_types = FALSE) |>
  mutate(state = if_else(state == "Federal Capital Territory", "FCT", state))

national_week <- read_output("results", "national_week")
state_week <- read_output("intermediate", "state_week")
blocks <- read_output("intermediate", "state_blocks")


# National: every report -----------------------------------------------------------------

# Our running totals already include the 2022 weeks 19-23 block (added at week 23)
# and the deaths reported only nationally (allocated in 01_load.R). After a week
# with no report the running total is a lower bound (`cum_complete` FALSE), so
# a difference there is partly the missing week, not deaths added later.
our_running <- national_week |>
  select(year, epi_week, our_confirmed = cum_confirmed, our_deaths = cum_deaths, cum_complete)

national_gap <- printed_national |>
  select(year, epi_week, source_file, printed_confirmed = cum_confirmed, printed_deaths = cum_deaths) |>
  inner_join(our_running, by = c("year", "epi_week")) |>
  arrange(year, epi_week) |>
  mutate(gap_confirmed = printed_confirmed - our_confirmed,
         gap_deaths = printed_deaths - our_deaths)

# How much the difference grows since the previous report with a printed figure:
# the deaths (or cases) added to the running total in that week.
national_gap <- national_gap |>
  mutate(added_confirmed = gap_confirmed - lag(gap_confirmed),
         added_deaths = gap_deaths - lag(gap_deaths),
         .by = year)

# Each year's last report: the year-end comparison. 2017 counts from the onset
# of the season in December 2016, so it is not comparable with our sums.
year_end <- national_gap |>
  filter(!is.na(printed_deaths)) |>
  slice_max(epi_week, n = 1, by = year) |>
  mutate(comparable = cum_complete & year != 2017,
         reason = case_when(year == 2017 ~ "printed totals count from December 2016",
                            !cum_complete ~ "weeks with no report before this week",
                            .default = ""))


# By state: each year's final report ------------------------------------------------

# Our sums to the week of that report, from the weekly state figures and the
# 2022 block. Deaths reported only nationally have no state, so they are summed
# separately rather than through their allocation.
report_weeks <- printed_state |>
  distinct(year, epi_week)

weekly_by_state <- state_week |>
  inner_join(report_weeks |> rename(report_week = epi_week), by = "year") |>
  filter(epi_week <= report_week) |>
  summarise(our_confirmed = sum(confirmed, na.rm = TRUE),
            our_deaths = sum(if_else(deaths_imputed, 0, deaths), na.rm = TRUE),
            .by = c(year, report_week, state))

blocks_by_state <- blocks |>
  inner_join(report_weeks |> rename(report_week = epi_week), by = "year") |>
  filter(end_week <= report_week) |>
  summarise(block_confirmed = sum(confirmed, na.rm = TRUE), block_deaths = sum(deaths, na.rm = TRUE),
            .by = c(year, report_week, state))

ours_by_state <- weekly_by_state |>
  left_join(blocks_by_state, by = c("year", "report_week", "state")) |>
  mutate(our_confirmed = our_confirmed + coalesce(block_confirmed, 0),
         our_deaths = our_deaths + coalesce(block_deaths, 0)) |>
  select(year, epi_week = report_week, state, our_confirmed, our_deaths)

state_gap <- printed_state |>
  full_join(ours_by_state, by = c("year", "epi_week", "state")) |>
  filter(year %in% report_weeks$year) |>
  mutate(cum_confirmed = coalesce(cum_confirmed, 0), cum_deaths = coalesce(cum_deaths, 0),
         gap_confirmed = cum_confirmed - our_confirmed,
         gap_deaths = cum_deaths - our_deaths) |>
  filter(cum_confirmed > 0 | our_confirmed > 0 | cum_deaths > 0 | our_deaths > 0) |>
  arrange(year, desc(gap_deaths))

# Deaths in the weekly figures with no state, by year, to the same report week.
unattributed_deaths <- state_week |>
  inner_join(report_weeks |> rename(report_week = epi_week), by = "year") |>
  filter(epi_week <= report_week, deaths_imputed) |>
  summarise(deaths_without_state = sum(deaths), .by = year)


# Save ----------------------------------------------------------------------------------

save_output(national_gap, "results", "cumulative_gap_national")
save_output(year_end, "results", "cumulative_gap_year_end")
save_output(state_gap, "results", "cumulative_gap_state")
save_output(unattributed_deaths, "results", "cumulative_gap_unattributed")
save_table(national_gap, "cumulative_gap_national")
save_table(state_gap, "cumulative_gap_state")

message("Reports compared nationally: ", nrow(national_gap), "; years with a state comparison: ",
        paste(sort(unique(state_gap$year)), collapse = ", "))
