# addendum3_published_series.R
#
# Added by Addendum 3 to the analysis plan (5 October 2026), after the plan was
# fixed and after A9 showed that the printed cumulative death counts differ from
# the summed weekly counts by amounts that vary between years.
#
# Repeats two comparisons on the published cumulative series, alongside the
# weekly series used by the pre-specified analyses:
#   - the annual CFR, each year's final printed cumulative figures (for 2026, the
#     last report before the freeze);
#   - the phase-matched comparison (A1): cumulative CFR at the week each year
#     reached 25% and 50% of its confirmed cases within weeks 1 to the last week
#     reported in the target year, using the printed running totals.
# The published series has national totals only, so there are no state-weeks to
# resample: point estimates, no intervals.

library(dplyr)
library(readr)
library(here)

source(here("R", "00_functions.R"))

printed <- read_csv(here("data-raw", "lassa_cumulative_national.csv"), show_col_types = FALSE) |>
  filter(!is.na(cum_confirmed), !is.na(cum_deaths)) |>
  select(year, epi_week, cum_confirmed, cum_deaths)

national_week <- read_output("results", "national_week")
national_season <- read_output("results", "national_season")

target_year <- max(inference_years)
cut_week <- max(national_week$epi_week[national_week$year == target_year & national_week$reported])
percentiles <- c(0.25, 0.5)


# Annual CFR, published and weekly -------------------------------------------------------

# Each year's last printed cumulative figure. 2017 counts from December 2016.
annual_published <- printed |>
  slice_max(epi_week, n = 1, by = year) |>
  transmute(year, published_week = epi_week, published_confirmed = cum_confirmed,
            published_deaths = cum_deaths, cfr_published = cum_deaths / cum_confirmed)

annual <- national_season |>
  select(year, cfr_weekly = cfr_matched) |>
  left_join(annual_published, by = "year") |>
  mutate(note = if_else(year == 2017, "printed totals count from December 2016", ""))


# Phase-matched comparison on the published series ----------------------------------------

# Within weeks 1 to cut_week. The phase point is the first week with a printed
# figure at which the printed cumulative confirmed count reaches the percentile of
# the year's printed total at cut_week (or the last printed week before it).
window <- printed |>
  filter(year %in% inference_years, epi_week <= cut_week)

year_totals <- window |>
  slice_max(epi_week, n = 1, by = year) |>
  select(year, total_week = epi_week, total_confirmed = cum_confirmed)

phase_published <- tibble()
for (p in percentiles) {
  points <- window |>
    left_join(year_totals, by = "year") |>
    filter(cum_confirmed >= p * total_confirmed) |>
    slice_min(epi_week, n = 1, by = year) |>
    transmute(year, comparison = paste0("phase ", 100 * p, "%"), epi_week, cum_confirmed, cum_deaths,
              cfr = cum_deaths / cum_confirmed, total_week)
  phase_published <- bind_rows(phase_published, points)
}

# The fixed week 23 comparison, as the commentary made it.
week23 <- printed |>
  filter(year %in% inference_years, epi_week == 23) |>
  transmute(year, comparison = "fixed week 23", epi_week, cum_confirmed, cum_deaths, cfr = cum_deaths / cum_confirmed)
phase_published <- bind_rows(phase_published, week23) |>
  arrange(comparison, year)

# Target against reference year at each point, published and weekly side by side.
weekly_points <- read_output("results", "phase_points") |>
  select(year, comparison, weekly_week = epi_week, cfr_weekly = cfr)

phase_side_by_side <- phase_published |>
  select(year, comparison, published_week = epi_week, cfr_published = cfr) |>
  full_join(weekly_points, by = c("year", "comparison")) |>
  arrange(comparison, year)

target_vs_reference <- phase_side_by_side |>
  filter(year %in% c(target_year - 1, target_year)) |>
  summarise(published_difference = cfr_published[year == target_year] - cfr_published[year == target_year - 1],
            weekly_difference = cfr_weekly[year == target_year] - cfr_weekly[year == target_year - 1],
            .by = comparison)


# Save ----------------------------------------------------------------------------------

# The printed running CFR at every report, for Figure 1.
published_running <- printed |>
  transmute(year, epi_week, cum_confirmed, cfr_published = cum_deaths / cum_confirmed)

save_output(published_running, "results", "published_series_running")
save_output(annual, "results", "published_series_annual")
save_output(phase_side_by_side, "results", "published_series_phase")
save_output(target_vs_reference, "results", "published_series_phase_difference")
save_table(annual, "published_series_annual")
save_table(phase_side_by_side, "published_series_phase")

message("Annual series: ", nrow(annual), " years; phase points on the published series: ", nrow(phase_published))
