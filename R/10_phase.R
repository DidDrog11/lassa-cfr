# 10_phase.R
#
# Phase-matched comparison of cumulative CFR between seasons.
#
# A cumulative CFR read at a fixed calendar week compares seasons at different
# points in their epidemics: an early-peaking season has accumulated most of its
# cases and deaths by week 23, a late one has not, and deaths lag cases. So each
# season is compared at the week it reaches the same share of its cases.
#
# The last season is incomplete, so a share of its full-season total is not
# defined. Every season is therefore cut to the week the last season has reached
# in the frozen data (last sitrep on or before 1 October 2026, set in
# 01_load.R), and percentiles are taken of that cut total (see the analysis plan).
# This matches the window used for the last decomposition pair. Only the 25th
# and 50th percentiles are used: a later percentile of a cut total describes the
# cut more than the season. Primary: the 50th percentile point.
#
# The naive comparison the commentary made, fixed week 23, is shown alongside.
#
# Where a percentile falls in weeks whose running total is not exactly known
# (after a missing week, or inside the 2022 weeks 19-23 block), the crossing is
# placed at the first week with a known running total at or beyond it, and
# flagged.
#
# Uncertainty: the cluster bootstrap used throughout (resample_units() in
# 00_functions.R). The phase point is found again in every replicate, so the
# interval includes uncertainty in where the point falls.

library(dplyr)
library(tidyr)
library(readr)
library(here)

source(here("R", "00_functions.R"))

state_week <- read_output("intermediate", "state_week")
blocks <- read_output("intermediate", "state_blocks")
national_week <- read_output("results", "national_week")


# Settings ---------------------------------------------------------------------

smoke <- as.logical(trimws(Sys.getenv("LASSA_SMOKE", "TRUE")))   # set LASSA_SMOKE=FALSE for the full run
n_boot <- if (smoke) 20 else 1000

percentiles <- c(0.25, 0.50)
primary_percentile <- 0.50
naive_week <- 23
target_year <- max(inference_years)
reference_year <- target_year - 1

# The week the last season has reached in the frozen data.
cut_week <- national_week |>
  filter(year == target_year, reported) |>
  pull(epi_week) |>
  max()

# Which season-weeks have an exactly known running total. Fixed by the reports
# that exist, so it does not change between bootstrap replicates.
week_grid <- national_week |>
  filter(year %in% inference_years, epi_week <= cut_week) |>
  select(year, epi_week, cum_known, cum_complete)

units <- analysis_units(state_week, blocks) |> filter(epi_week <= cut_week)


# Find the comparison points in one set of units ---------------------------------------

# Returns one row per season and comparison: the week, the running totals there,
# and whether the point is exactly placed.
comparison_points <- function(units) {
  # Running totals on the full week grid; a week with no rows carries the total forward.
  running <- week_grid |>
    left_join(national_running_totals(units) |> select(year, epi_week, cum_confirmed, cum_deaths),
              by = c("year", "epi_week")) |>
    arrange(year, epi_week) |>
    group_by(year) |>
    fill(cum_confirmed, cum_deaths, .direction = "down") |>
    ungroup() |>
    mutate(cum_confirmed = coalesce(cum_confirmed, 0), cum_deaths = coalesce(cum_deaths, 0))

  totals <- running |>
    filter(epi_week == cut_week) |>
    select(year, total_to_cut = cum_confirmed, total_is_exact = cum_complete)

  points <- list()
  for (q in percentiles) {
    for (y in inference_years) {
      s <- running |> filter(year == y)
      threshold <- q * totals$total_to_cut[totals$year == y]

      # First week with a known running total at or above the threshold. It is
      # exact only if the week before is known and still below the threshold.
      crossing <- s |> filter(cum_known, cum_confirmed >= threshold) |> slice(1)

      # After a missing week the running total is never exactly known again that
      # season, so a percentile falling after it cannot be placed. That season's
      # point is then missing (and flagged), rather than stopping the script.
      if (nrow(crossing) == 0) {
        points[[length(points) + 1]] <- tibble(
          year = y, comparison = paste0("phase ", 100 * q, "%"), epi_week = NA_integer_,
          cum_confirmed = NA_real_, cum_deaths = NA_real_,
          exact = FALSE, total_is_exact = totals$total_is_exact[totals$year == y])
        next
      }

      previous <- s |> filter(epi_week == crossing$epi_week - 1)
      exact <- nrow(previous) == 1 && previous$cum_known && previous$cum_confirmed < threshold

      points[[length(points) + 1]] <- tibble(
        year = y, comparison = paste0("phase ", 100 * q, "%"), epi_week = crossing$epi_week,
        cum_confirmed = crossing$cum_confirmed, cum_deaths = crossing$cum_deaths,
        exact = exact, total_is_exact = totals$total_is_exact[totals$year == y])
    }
  }

  naive <- running |>
    filter(epi_week == naive_week) |>
    transmute(year, comparison = paste0("fixed week ", naive_week), epi_week, cum_confirmed, cum_deaths,
              exact = cum_known, total_is_exact = NA)

  bind_rows(points, naive) |> mutate(cfr = cum_deaths / cum_confirmed)
}

# Target minus reference CFR, for each comparison.
season_differences <- function(points) {
  points |>
    filter(year %in% c(reference_year, target_year)) |>
    select(comparison, year, cfr) |>
    pivot_wider(names_from = year, values_from = cfr, names_prefix = "cfr_") |>
    mutate(difference = .data[[paste0("cfr_", target_year)]] - .data[[paste0("cfr_", reference_year)]])
}


# Point estimates and bootstrap ------------------------------------------------------------

points <- comparison_points(units)
differences <- season_differences(points)

set.seed(20260929)
boot <- list()
for (b in seq_len(n_boot)) {
  boot[[b]] <- season_differences(comparison_points(resample_units(units))) |> mutate(rep = b)
}
boot <- bind_rows(boot)

# Placement flags for the two seasons being compared.
placement <- points |>
  filter(year %in% c(reference_year, target_year)) |>
  summarise(week_reference = epi_week[year == reference_year], week_target = epi_week[year == target_year],
            points_exact = all(exact), .by = comparison)

comparisons <- differences |>
  left_join(boot |> summarise(difference_lo = quantile(difference, 0.025), difference_hi = quantile(difference, 0.975),
                              p_above_zero = mean(difference > 0), .by = comparison),
            by = "comparison") |>
  left_join(placement, by = "comparison") |>
  mutate(role = case_when(comparison == paste0("phase ", 100 * primary_percentile, "%") ~ "primary",
                          grepl("^phase", comparison) ~ "secondary",
                          .default = "descriptive (naive comparison)"))


# Save ---------------------------------------------------------------------------------

save_output(points, "results", "phase_points")
save_output(comparisons, "results", "phase_comparisons")

message("Seasons cut to week ", cut_week, "; percentiles: ", paste(100 * percentiles, collapse = ", "),
        "; bootstrap replicates: ", n_boot, if (smoke) " (SMOKE TEST)" else "")
message("Phase points not exactly placed: ", sum(!points$exact, na.rm = TRUE))
