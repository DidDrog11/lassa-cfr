# 11_year_end.R
#
# Year-end check: does the cumulative "week 1 to date" line printed in each
# season's final sitrep match the sum of that season's weekly tables?
#
# NCDC updates figures retrospectively as investigations close. Those updates
# reach the cumulative line but never the weekly tables, so our series (built by
# summing weekly tables) can drift from what NCDC publishes. Tracking this week
# by week through every sitrep is out of scope (see the analysis plan); comparing
# once per season, at the last sitrep, measures how large the drift is by year's
# end and whether it grows with recency. A gap in a season with missing weeks
# is partly those weeks, so seasons are flagged where that applies.
#
# For the last season the comparison is at the latest sitrep, which is the
# point the analysis uses.

library(dplyr)
library(tidyr)
library(readr)
library(here)

source(here("R", "00_functions.R"))

national_week <- read_output("results", "national_week")
national_season <- read_output("results", "national_season")


# Published year-to-date figures from each season's final sitrep ------------------------

# 2022, 2023 and 2025 were read from page-1 Table 1 by the extraction's
# parser (map-liberator/analysis/parse_ncdc_benchmark.R). 2018-2021, 2024 and 2026
# were read by hand from the final sitrep.
published <- tribble(
  ~year, ~epi_week, ~suspected, ~confirmed, ~probable, ~deaths, ~source,
  2018L, 52L, 3498, 633,  20, 171, "lassa_sitrep_2018_w52_20181224.pdf, read by hand",
  2019L, 52L, 5057, 833,  19, 174, "lassa_sitrep_2019_w52_20191228.pdf, read by hand",
  2020L, 53L, 6791, 1189, 14, 244, "lassa_sitrep_2020_w53_20201231.pdf, read by hand",
  2021L, 52L, 4654, 510,  6,  102, "lassa_sitrep_2021_w52_20211225.pdf, read by hand",
  2022L, 52L, 8202, 1067, 37, 189, "lassa_sitrep_2022_w52_20221222.pdf, parsed",
  2023L, 52L, 9155, 1270, 12, 227, "lassa_sitrep_2023_w52_20231221.pdf, parsed",
  2024L, 52L, 10098, 1309, 23, 214, "NCDC year-end sitrep 2024, read by hand",
  2025L, 52L, 9389, 1148, 9,  215, "lassa_sitrep_2025_w52_20251227.pdf, parsed",
  2026L, 36L, 7938, 1086, 6,  259, "lassa_sitrep_2026_w36_20260905.pdf, read by hand")


# Our reconstruction at the same week ----------------------------------------------------

# Running totals from the weekly tables, at the week of each final sitrep.
# `cum_complete` is FALSE where a missing week earlier in the season makes the
# running total a lower bound.
# Suspected cases in a multi-week block enter at its end week, as confirmed
# cases and deaths do in 03_descriptives.R.
block_suspected <- read_output("intermediate", "state_blocks") |>
  summarise(block_suspected = sum_or_na(suspected), .by = c(year, end_week))

suspected_cum <- national_week |>
  left_join(block_suspected, by = c("year", "epi_week" = "end_week")) |>
  arrange(year, epi_week) |>
  mutate(cum_suspected = cumsum(coalesce(suspected, 0) + coalesce(block_suspected, 0)), .by = year) |>
  select(year, epi_week, cum_suspected)

reconstructed <- national_week |>
  left_join(suspected_cum, by = c("year", "epi_week")) |>
  select(year, epi_week, cum_suspected, cum_confirmed, cum_deaths, cum_complete)


# Compare -------------------------------------------------------------------------------

# Gap = published minus reconstructed. A positive gap in deaths means NCDC's
# year-end figure includes deaths that never appeared in a weekly table.
year_end <- published |>
  left_join(reconstructed, by = c("year", "epi_week")) |>
  mutate(gap_suspected = suspected - cum_suspected,
         gap_confirmed = confirmed - cum_confirmed,
         gap_deaths = deaths - cum_deaths,
         gap_confirmed_pct = 100 * gap_confirmed / confirmed,
         gap_deaths_pct = 100 * gap_deaths / deaths,
         weeks_missing_before = !cum_complete) |>
  arrange(year)


# Save ---------------------------------------------------------------------------------

save_output(year_end, "results", "year_end_check")

message("Seasons with a published year-end figure: ", sum(!is.na(year_end$confirmed)), " of ", nrow(year_end))
message("Seasons where missing weeks contribute to the gap: ", sum(year_end$weeks_missing_before, na.rm = TRUE))
