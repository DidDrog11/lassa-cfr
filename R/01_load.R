# 01_load.R
#
# Turns the long extraction ledger into two analysis tables:
#   - state_week:   one row per state per epi-week, missing weeks kept as NA
#   - state_season: one row per state per year, summed over the weeks
#
# The ledger has one row per state, per variable, per sitrep. Only states with
# something to report were entered, so the blanks have to be filled here. The
# rule comes from the extraction protocol
# (map-liberator/analysis/sitrep_formats.md) and is applied separately for each
# sitrep and each variable:
#
#   what was entered for the variable   states not entered   states entered as NA
#   all NA                              NA                   NA
#   no NA                               0                    -
#   a mix                               0                    stay NA
#
# A "Nothing reported" row speaks for the whole sitrep: 0 means the variable was
# listed by state and nobody had any, NA means there was no state breakdown.
# A week with no sitrep at all stays NA.
#
# Assumption: where a sitrep lists states for a variable, the list is complete.
# This includes the 2017 Highlights sentence for suspected cases, so a state
# named for confirmed cases but not for suspected had 0 new suspected that week.
#
# To run on the checked extract, change `ledger_path` and nothing else.

library(dplyr)
library(tidyr)
library(readr)
library(here)

source(here("R", "00_functions.R"))


# Settings ---------------------------------------------------------------------

# The extraction ledger: the final hand extract in data-raw/. The script stops
# if the file is not there. Code development used a preliminary version of the
# dataset, not shared or reported, reached by setting the environment variable
# LASSA_LEDGER to its path; nothing else changes between the two.
ledger_path <- Sys.getenv("LASSA_LEDGER", here("data-raw", "lassa_state_week_ledger.csv"))
if (!file.exists(ledger_path)) stop("Ledger not found: ", ledger_path)

weeks_in_year <- 52      # years are 52 weeks long unless week 53 appears

core_vars <- c("suspected", "confirmed", "probable", "deaths")
late_vars <- c("confirmed_late", "deaths_late")
nil_label <- "Nothing reported"

# Deaths given only as a national figure. In these sitreps deaths are not broken
# down by state, so the ledger holds NA for every state. The national totals
# come from the extraction's page-1 checks (map-liberator/analysis/output/).
# They are allocated to states further down. Inference years only; 2017 is
# context. 2018 w1 has no deaths clause at all and stays NA. 2018 w46 gives one
# death "in Edo (1) and Ondo (2) state" without saying which; Edo and Ondo are
# the only states with confirmed cases that week, so the rule shares it between
# them.
# 2020 w4 and w5 were read from a Table 1 row the check script marked as
# unreliable, and were checked by hand against the PDFs (cumulative deaths 41
# at w4 and 47 at w5).
national_deaths_only <- tribble(
  ~year, ~epi_week, ~national_deaths, ~source,
  2018L, 3L, 14, "check_highlights_2018.csv, lassa_sitrep_2018_w04_20180121.pdf",
  2018L, 4L, 2,  "check_highlights_2018.csv, lassa_sitrep_2018_w05_20180128.pdf",
  2018L, 46L, 1, "hand, lassa_sitrep_2018_w47_20181118.pdf",
  2020L, 2L, 12, "check_table1_2020.csv, lassa_sitrep_2020_w02_20200111.pdf",
  2020L, 3L, 10, "check_table1_2020.csv, lassa_sitrep_2020_w03_20200118.pdf",
  2020L, 4L, 19, "check_table1_2020.csv, lassa_sitrep_2020_w04_20200125.pdf",
  2020L, 5L, 6,  "check_table1_2020.csv, lassa_sitrep_2020_w05_20200201.pdf")

dir.create(here("data"), showWarnings = FALSE)

# Read the ledger and tidy the state names --------------------------------------

raw <- read_csv(ledger_path,
                col_types = cols(year = col_integer(), epi_week = col_integer(), value = col_double(),
                                 .default = col_character()))

# Corrections to the ledger, found by 02_validate.R and checked against the PDF.
# The file in data-raw/ is never edited; corrections are applied here. Each must
# match exactly one ledger row holding the value it replaces, or the script stops.
corrections <- tribble(
  ~source_file, ~year, ~epi_week, ~state, ~variable, ~value_in_ledger, ~corrected, ~evidence,
  "lassa_sitrep_2020_w03_20200118.pdf", 2020L, 3L, "Delta", "probable", 0, NA,
  "Table 3 lists confirmed cases only; the only probable figure is national (Table 4: 0). All other states are NA that week.",
  "lassa_sitrep_2021_w07_20210212.pdf", 2021L, 7L, "Taraba", "suspected", 0, 5,
  "Table 3 row reads Taraba 5 suspected, 1 confirmed, 1 death. State rows sum to 108 against a printed Total of 113.")

correction_key <- c("source_file", "year", "epi_week", "state", "variable")

matched <- raw |>
  inner_join(corrections, by = correction_key)
if (nrow(matched) != nrow(corrections) || !isTRUE(all(matched$value == matched$value_in_ledger))) {
  print(matched)
  stop("A correction does not match exactly one ledger row with the value it replaces.")
}

raw <- raw |>
  left_join(corrections |> select(all_of(correction_key), corrected) |> mutate(is_corrected = TRUE),
            by = correction_key) |>
  mutate(value = if_else(is.na(is_corrected), value, corrected)) |>
  select(-corrected, -is_corrected)

# GADM calls it "Federal Capital Territory"; the rest of the code uses "FCT".
# "Jos" is a city in Plateau, not a state, so its one row (2025 w47) counts as
# Plateau (see the analysis plan).
# Data freeze (see the analysis plan): only sitreps dated on or before the freeze
# date enter the analysis, so the last season stops at a named report rather
# than at whenever the analysis happens to be run. The date is the last 8
# digits of the sitrep filename: the date NCDC's report listing gives for the
# report. Derived rows (rebuilt missing weeks, filenames starting "derived_")
# carry no date and describe past weeks, so they are kept.
freeze_date <- as.Date("2026-10-01")

raw <- raw |>
  mutate(report_date = as.Date(sub(".*_w[0-9]+_([0-9]{8}).*", "\\1", source_file), format = "%Y%m%d"))

# Every report must have a readable date, or the freeze would silently not apply.
undated <- raw |> filter(is.na(report_date), !startsWith(source_file, "derived_")) |> distinct(source_file)
if (nrow(undated)) { print(undated); stop("Reports above have no date in their filename, so the freeze cannot be applied.") }

raw <- raw |>
  filter(is.na(report_date) | report_date <= freeze_date)

message("Data frozen at ", freeze_date, "; last sitrep included: ",
        raw$source_file[which.max(raw$report_date)])

ledger <- raw |>
  mutate(state = case_when(state == "Federal Capital Territory" ~ "FCT",
                           state == "Jos" ~ "Plateau",
                           .default = state),
         epi_week_span = na_if(epi_week_span, ""),
         block = !is.na(epi_week_span))

# Late reports: in 2017-2019 a sitrep sometimes adds cases "not previously
# reported". They are small in number and are added to the week of the sitrep
# that carried them (see the analysis plan), since the sitreps rarely say which
# earlier week they belong to. The original rows are saved first for the note.
late_reports <- ledger |>
  filter(variable %in% late_vars)

ledger <- ledger |>
  mutate(variable = case_when(variable == "confirmed_late" ~ "confirmed",
                              variable == "deaths_late" ~ "deaths",
                              .default = variable))

# Places outside Nigeria are dropped (see the analysis plan); at present one
# "Cameroon" row, 2024 w14. They are saved for the record. Any other name not on
# the state list is treated as an error, so a misspelled state stops the script
# rather than its cases being dropped.
places_outside_nigeria <- c("Cameroon")

set_aside <- ledger |>
  filter(!state %in% c(nigeria_states, nil_label))

unknown_places <- setdiff(unique(set_aside$state), places_outside_nigeria)
if (length(unknown_places)) stop("Names not on the state list or the outside-Nigeria list: ", paste(unknown_places, collapse = ", "))

ledger <- ledger |>
  filter(state %in% c(nigeria_states, nil_label))

# Recoding Jos to Plateau, or folding in a late report, can put two rows for the
# same state and variable in one sitrep. Add them together, and keep a count of
# how many rows went into each value so the validation script can list these.
ledger <- ledger |>
  summarise(value = sum_or_na(value), n_rows = n(),
            .by = c(year, epi_week, epi_week_span, block, state, variable, extraction, source_file))

# The unit for filling blanks is a sitrep *and the week it reports*. Usually
# that is one sitrep file, but a file can carry rows for two weeks (the 2019
# w23 report has rows filed under week 22, pending in the extraction's
# correction queue). Each file-week is treated as its own document.
ledger <- ledger |>
  mutate(doc_id = paste(source_file, year, coalesce(as.character(epi_week), epi_week_span)))


# Work out how to fill the blanks in each sitrep ----------------------------------

# One row per sitrep (strictly, per file-week; see above).
docs <- ledger |>
  distinct(doc_id, source_file, year, epi_week, epi_week_span, block, extraction)

stopifnot(!anyDuplicated(docs$doc_id))

# Values actually entered for a named state.
entered <- ledger |>
  filter(variable %in% core_vars, state != nil_label)

# "Nothing reported" rows.
nil <- ledger |>
  filter(variable %in% core_vars, state == nil_label) |>
  select(doc_id, variable, nil_value = value)

# For each sitrep and variable, count what was entered and how much of it was NA.
entered_counts <- entered |>
  summarise(n_entered = n(), n_na = sum(is.na(value)), .by = c(doc_id, variable))

# Apply the fill rule from the header. `fill` is the value given to every state
# that was not entered for that variable in that sitrep.
doc_status <- docs |>
  cross_join(tibble(variable = core_vars)) |>
  left_join(entered_counts, by = c("doc_id", "variable")) |>
  left_join(nil, by = c("doc_id", "variable")) |>
  mutate(n_entered = coalesce(n_entered, 0L),
         n_na = coalesce(n_na, 0L),
         has_nil = doc_id %in% nil$doc_id,
         status = case_when(has_nil & !is.na(nil_value) ~ "nil, enumerated",
                            has_nil ~ "nil, no breakdown",
                            n_entered == 0 ~ "variable not entered",
                            n_na == n_entered ~ "no breakdown",
                            n_na == 0 ~ "enumerated",
                            .default = "mixed"),
         fill = if_else(status %in% c("nil, enumerated", "enumerated", "mixed"), 0, NA_real_))


# Fill the blanks ------------------------------------------------------------------

# The entered values, flagged so they can be told apart from filled ones.
entered_values <- entered |>
  select(doc_id, state, variable, value) |>
  mutate(was_entered = TRUE)

# Every sitrep x variable x state. States that were entered keep their value
# (including an entered NA); the rest take the sitrep's fill value.
doc_grid <- doc_status |>
  select(doc_id, source_file, year, epi_week, epi_week_span, block, extraction, variable, fill) |>
  cross_join(tibble(state = nigeria_states)) |>
  left_join(entered_values, by = c("doc_id", "state", "variable")) |>
  mutate(was_entered = coalesce(was_entered, FALSE),
         value = if_else(was_entered, value, fill))

# Sanity check: every entered value made it into the grid exactly once.
stopifnot(sum(doc_grid$was_entered) == nrow(entered))


# Multi-week blocks ----------------------------------------------------------------

# Some missing weeks were rebuilt from the difference between two cumulative
# tables. One of these covers five weeks (2022 weeks 19-23) and so has no single
# week. Blocks are kept out of the weekly grid and never spread across their
# weeks (see the analysis plan). How later scripts use them:
#   season totals       included
#   cumulative series   added at end_week; the weeks before it in the span unknown
#   weekly curve        drawn as one labelled bar over the span
#   isolated detection  every week in the span unobserved
blocks <- doc_grid |>
  filter(block) |>
  select(year, epi_week_span, state, variable, value, extraction, source_file) |>
  pivot_wider(names_from = variable, values_from = value) |>
  separate_wider_delim(epi_week_span, delim = "-", names = c("start_week", "end_week"), cols_remove = FALSE) |>
  mutate(start_week = as.integer(start_week), end_week = as.integer(end_week))

# The weeks each block covers, one row per year x week.
block_weeks <- blocks |>
  distinct(year, start_week, end_week) |>
  reframe(epi_week = start_week:end_week, .by = c(year, start_week, end_week)) |>
  select(year, epi_week)


# Collapse sitreps to weeks --------------------------------------------------------

# Usually one sitrep per week. Where a week has more than one (2019 w22 at
# present), their values are added. 02_validate.R checks that the sitreps cover
# different states, which is what makes adding them safe.
weekly_long <- doc_grid |>
  filter(!block) |>
  summarise(value = sum_or_na(value),
            n_docs = n_distinct(source_file),   # must come before source_file is collapsed below
            source_file = paste(sort(unique(source_file)), collapse = ";"),
            extraction = paste(sort(unique(extraction)), collapse = ";"),
            .by = c(year, epi_week, state, variable))

weekly_wide <- weekly_long |>
  pivot_wider(names_from = variable, values_from = value)


# Build the full state x week grid ---------------------------------------------------

# How many weeks each year should have. The last year runs only to the latest
# week reported; other years run to 52, or 53 if week 53 appears.
last_year <- max(ledger$year)

year_weeks <- ledger |>
  filter(!block) |>
  summarise(max_seen = max(epi_week), .by = year) |>
  mutate(n_weeks = if_else(year == last_year, max_seen, pmax(weeks_in_year, max_seen)))

# Every year x week x state, whether or not a sitrep exists. Weeks with no sitrep
# come through the join as NA and are marked reported = FALSE. `in_block` marks
# weeks whose cases exist only inside a multi-week block: their values are NA
# here, but unlike a true gap the cases are in the season totals.
all_weeks <- year_weeks |>
  reframe(epi_week = seq_len(n_weeks), .by = year) |>
  cross_join(tibble(state = nigeria_states))

state_week <- all_weeks |>
  left_join(weekly_wide, by = c("year", "epi_week", "state")) |>
  mutate(reported = !is.na(source_file),
         in_block = paste(year, epi_week) %in% paste(block_weeks$year, block_weeks$epi_week)) |>
  select(year, epi_week, state, all_of(core_vars), reported, in_block, extraction, source_file, n_docs) |>
  arrange(year, epi_week, state)


# Allocate deaths given only nationally ----------------------------------------------

# Deaths cannot come from a state with no cases, and there are few of them, so
# each week's national total is shared among the states with confirmed cases
# that week, in proportion to their confirmed cases (see the analysis plan). The
# assumption is that in those weeks deaths were spread like cases.
# `deaths` holds the expected (fractional) share; `deaths_imputed` marks these
# cells so 04_decompose.R can redraw them (one of that week's confirmed cases per death) in each bootstrap
# replicate, which carries the allocation's uncertainty into the intervals.
allocation <- state_week |>
  inner_join(national_deaths_only |> select(year, epi_week, national_deaths), by = c("year", "epi_week")) |>
  mutate(weight = confirmed / sum(confirmed), .by = c(year, epi_week)) |>
  transmute(year, epi_week, state, deaths_allocated = national_deaths * weight)

# Only weeks where no state has a deaths value are filled; a week with any
# state-level deaths would mean the national figure is not the whole story.
stopifnot(all(is.na(state_week$deaths[paste(state_week$year, state_week$epi_week) %in% paste(allocation$year, allocation$epi_week)])))

# The reverse check: every report in the inference years that gives no state
# breakdown for deaths must be in `national_deaths_only` (or be the known
# exception, 2018 week 1, which has no deaths clause at all). Otherwise a new
# week of national-only deaths in the final extract would silently stay
# missing instead of being allocated.
no_breakdown_weeks <- doc_status |>
  filter(variable == "deaths", status %in% c("no breakdown", "nil, no breakdown", "variable not entered"),
         year %in% inference_years, !block) |>
  distinct(year, epi_week)
known_exceptions <- tibble(year = 2018L, epi_week = 1L)
unlisted <- no_breakdown_weeks |>
  anti_join(national_deaths_only, by = c("year", "epi_week")) |>
  anti_join(known_exceptions, by = c("year", "epi_week"))
if (nrow(unlisted)) {
  print(unlisted)
  stop("Reports above give deaths with no state breakdown but are not in national_deaths_only. ",
       "Add their national totals (with source) or record them as exceptions.")
}

state_week <- state_week |>
  left_join(allocation, by = c("year", "epi_week", "state")) |>
  mutate(deaths_imputed = !is.na(deaths_allocated),
         deaths = if_else(deaths_imputed, deaths_allocated, deaths)) |>
  select(-deaths_allocated)


# State x season totals --------------------------------------------------------------

# Add up the weekly rows and any blocks. A week with no sitrep is not a zero, so
# alongside each total we record how many weeks were NA: a total with missing
# weeks is a lower bound, and is flagged wherever it is used.
season_totals <- bind_rows(state_week |> select(year, state, all_of(core_vars)),
                           blocks |> select(year, state, all_of(core_vars))) |>
  summarise(across(all_of(core_vars), sum_or_na), .by = c(year, state))

season_missing <- state_week |>
  summarise(n_weeks = n(),
            across(all_of(core_vars), \(x) sum(is.na(x)), .names = "{.col}_weeks_na"),
            .by = c(year, state))

state_season <- season_totals |>
  left_join(season_missing, by = c("year", "state")) |>
  arrange(year, state)


# Save ---------------------------------------------------------------------------------

save_output(ledger, "intermediate", "ledger")
save_output(doc_status, "intermediate", "doc_status")
save_output(state_week, "intermediate", "state_week")
save_output(blocks, "intermediate", "state_blocks")
save_output(state_season, "intermediate", "state_season")

# What the loading rules changed, kept together as the record for Methods.
load_record <- list(corrections = corrections, late_reports = late_reports, set_aside = set_aside,
                    national_deaths_only = national_deaths_only)
save_output(load_record, "intermediate", "load_record")

message("Ledger rows: ", nrow(raw), "; sitreps: ", nrow(docs), "; years ", min(docs$year), "-", max(docs$year))
message("Rows dropped as not a Nigerian state: ", nrow(set_aside))
message("State-weeks in grid: ", nrow(state_week), "; weeks with no sitrep: ",
        nrow(distinct(filter(state_week, !reported, !in_block), year, epi_week)),
        "; weeks covered only by a block: ", nrow(distinct(filter(state_week, in_block), year, epi_week)))
message("Multi-week blocks held separately: ", nrow(distinct(blocks, year, epi_week_span)))
