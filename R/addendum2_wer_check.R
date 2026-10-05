# addendum2_wer_check.R
#
# Added by Addendum 2 to the analysis plan (5 October 2026), after the plan was
# fixed. A check only: no analysis uses these values.
#
# NCDC's general Weekly Epidemiological Report also gives national Lassa fever
# counts each week, and survives for some weeks whose situation report does not.
# An independent extraction of those national counts (Niyi-Oriolowo, NCDC Lassa
# fever weekly timeseries dataset, Hugging Face, doi:10.57967/hf/7145, CC BY 4.0)
# fills the weeks missing from the situation-report archive from that report.
# This script compares it with our data for:
#   - the weeks we rebuilt from the difference between two cumulative reports
#     (2022 w14, 2022 w19-23 as one block, 2023 w38, 2024 w16);
#   - the weeks with no situation report, where it shows what those weeks hold.
# The dataset is read at a fixed revision so the check can be repeated.

library(dplyr)
library(readr)
library(here)

source(here("R", "00_functions.R"))

wer_revision <- "6e21e21bd56aabfe05fe592fd4d177077052cf38"
wer_url <- paste0("https://huggingface.co/datasets/EmanuelN/ncdc_lassa_fever_timeseries/resolve/",
                  wer_revision, "/lassa_fever_timeseries_full.csv")

wer <- read_csv(wer_url, show_col_types = FALSE) |>
  select(year = epi_year, epi_week, wer_confirmed = confirmed_cases, wer_deaths = deaths,
         extraction_method, report_pdf_url)

state_week <- read_output("intermediate", "state_week")
blocks <- read_output("intermediate", "state_blocks")
national_week <- read_output("results", "national_week")


# Weeks we rebuilt from cumulative differences ------------------------------------------

# Single rebuilt weeks: national sums of the derived rows.
rebuilt_weeks <- state_week |>
  filter(startsWith(coalesce(source_file, ""), "derived_")) |>
  summarise(our_confirmed = sum(confirmed, na.rm = TRUE), our_deaths = sum(deaths, na.rm = TRUE),
            .by = c(year, epi_week)) |>
  left_join(wer, by = c("year", "epi_week")) |>
  mutate(weeks = as.character(epi_week), .before = our_confirmed) |>
  select(-epi_week)

# The 2022 weeks 19-23 block: compare totals over the five weeks.
rebuilt_block <- blocks |>
  summarise(our_confirmed = sum(confirmed, na.rm = TRUE), our_deaths = sum(deaths, na.rm = TRUE),
            .by = c(year, start_week, end_week)) |>
  left_join(wer, join_by(year, between(y$epi_week, x$start_week, x$end_week))) |>
  summarise(our_confirmed = first(our_confirmed), our_deaths = first(our_deaths),
            wer_confirmed = sum(wer_confirmed), wer_deaths = sum(wer_deaths),
            extraction_method = paste(unique(extraction_method), collapse = "; "),
            report_pdf_url = paste(report_pdf_url, collapse = "; "),
            .by = c(year, start_week, end_week)) |>
  mutate(weeks = paste0(start_week, "-", end_week), .after = year) |>
  select(-start_week, -end_week)

rebuilt_check <- bind_rows(rebuilt_weeks, rebuilt_block) |>
  mutate(diff_confirmed = wer_confirmed - our_confirmed, diff_deaths = wer_deaths - our_deaths) |>
  arrange(year, weeks)


# Weeks with no situation report -----------------------------------------------------

# Not in any block, no report: what the Weekly Epidemiological Report gives.
gap_weeks <- national_week |>
  filter(!reported, !in_block, year %in% unique(wer$year)) |>
  select(year, epi_week) |>
  left_join(wer, by = c("year", "epi_week"))


# Save ----------------------------------------------------------------------------------

save_output(rebuilt_check, "results", "wer_check_rebuilt_weeks")
save_output(gap_weeks, "results", "wer_check_gap_weeks")
save_table(rebuilt_check, "wer_check_rebuilt_weeks")
save_table(gap_weeks, "wer_check_gap_weeks")

message("Rebuilt weeks checked: ", nrow(rebuilt_check), "; weeks with no situation report: ", nrow(gap_weeks),
        " (", sum(!is.na(gap_weeks$wer_confirmed)), " with a value in the Weekly Epidemiological Report data)")
