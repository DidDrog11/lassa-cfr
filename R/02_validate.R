# 02_validate.R
#
# Checks on the loaded data before anything is analysed. 
#
# The extraction has its own checks against the sitrep totals.
# This script covers what those cannot see: how the
# blanks were filled, which weeks are missing, values that contradict each
# other, and how many states have enough cases to analyse on their own.

library(dplyr)
library(tidyr)
library(readr)
library(here)

source(here("R", "00_functions.R"))

ledger <- read_output("intermediate", "ledger")
doc_status <- read_output("intermediate", "doc_status")
state_week <- read_output("intermediate", "state_week")
state_season <- read_output("intermediate", "state_season")

# Minimum confirmed cases per state-season for a state to be its own stratum
# (see the analysis plan).
roster_min <- 30


# 1. Values built from more than one row -------------------------------------------

# Each sitrep should give one value per state per variable. Where 01_load.R had
# to add rows together, list them: a late report ("not previously reported")
# folded into the week of the sitrep that carried it, or a Jos row recoded to
# Plateau. Anything else here would be a value entered twice.
dupes <- ledger |>
  filter(n_rows > 1)

message("1. Sitrep-state-variable values built from more than one row: ", nrow(dupes))
print(dupes |> select(year, epi_week, state, variable, n_rows, source_file))

# 2. Weeks with more than one sitrep -------------------------------------------------

# 01_load.R adds these together. That is only safe if the sitreps cover
# different states, so also list any state that appears in both.
multi_doc_weeks <- state_week |>
  filter(n_docs > 1) |>
  distinct(year, epi_week, source_file)

states_in_both <- ledger |>
  filter(!block, state != "Nothing reported") |>
  distinct(year, epi_week, state, source_file) |>
  filter(n_distinct(source_file) > 1, .by = c(year, epi_week, state))

message("2. Weeks with more than one sitrep: ", nrow(multi_doc_weeks))
print(multi_doc_weeks)
message("   States entered in more than one sitrep for the same week: ", nrow(distinct(states_in_both, year, epi_week, state)))
print(states_in_both)


# 3. How the blanks were filled ------------------------------------------------------

# One row per year x variable, counting sitreps by fill status. "mixed" should be
# rare and needs checking by hand. "variable not entered" means the sitrep has
# rows but none for this variable, which the fill rule does not cover; those
# are left NA.
status_table <- doc_status |>
  count(year, variable, status) |>
  pivot_wider(names_from = status, values_from = n, values_fill = 0)

# "variable not entered" is normal where a whole year never breaks that variable
# down by state (suspected and probable in 2018-2019). It only needs a look when
# other sitreps in the same year do break it down.
enumerated_years <- doc_status |>
  filter(status %in% c("enumerated", "mixed", "nil, enumerated")) |>
  distinct(year, variable) |>
  mutate(enumerated_that_year = TRUE)

needs_a_look <- doc_status |>
  left_join(enumerated_years, by = c("year", "variable")) |>
  filter(status == "mixed" | (status == "variable not entered" & coalesce(enumerated_that_year, FALSE)))

message("3. Fill status of sitreps, by year and variable:")
print(status_table, n = 60)
message("   Sitreps needing a look (mixed, or variable not entered): ", nrow(needs_a_look))
print(needs_a_look |> select(year, epi_week, variable, status, n_entered, n_na, source_file), n = 50)

# 4. Missing weeks -----------------------------------------------------------------

# Weeks with no sitrep. Compare with the archive manifest: a week missing from
# the archive is a true gap; one in the archive but not here is unextracted.
# Weeks covered by a multi-week block (2022 w19-23) are listed separately: their
# cases are in the season totals, they just cannot be placed in a single week.
missing_weeks <- state_week |>
  filter(!reported) |>
  distinct(year, epi_week, in_block) |>
  summarise(n = n(), weeks = paste(epi_week, collapse = ", "), .by = c(year, in_block))

message("4. Weeks with no sitrep (in_block = TRUE: covered by a multi-week block):")
print(missing_weeks)

# 5. Which variables are known, by year -----------------------------------------------

# Share of reported state-weeks with a known value for each variable. This feeds
# the evaluability result: it shows what the sitreps break down by state, when.
availability <- state_week |>
  filter(reported) |>
  summarise(suspected = mean(!is.na(suspected)),
            confirmed = mean(!is.na(confirmed)),
            probable = mean(!is.na(probable)),
            deaths = mean(!is.na(deaths)),
            .by = year) |>
  mutate(across(-year, \(x) round(x, 2)))

message("5. Share of reported state-weeks with a known value:")
print(availability)


# 6. Values that contradict each other --------------------------------------------------

# In a single week, deaths > confirmed can be real: the case may have been
# confirmed in an earlier week. It is listed, not treated as an error. The same
# goes for confirmed > suspected, which is expected in 2017: the suspected list
# is taken as complete, so a state with new confirmed cases but no new suspected
# cases that week records 0 suspected.
# Over a whole season, deaths > confirmed cannot go into a binomial model and has
# to be resolved before any model is fitted.
# Where a variable is NA (e.g. suspected in 2018-2019) the check cannot be made,
# so it is skipped for that row rather than returned as NA.
confirmed_over_suspected <- state_week |>
  filter(!is.na(confirmed), !is.na(suspected), confirmed > suspected) |>
  mutate(reason = "confirmed > suspected")

deaths_over_confirmed <- state_week |>
  filter(!is.na(deaths), !is.na(confirmed), deaths > confirmed) |>
  mutate(reason = "deaths > confirmed (week)")

impossible_week <- bind_rows(confirmed_over_suspected, deaths_over_confirmed)

impossible_season <- state_season |>
  filter(!is.na(deaths), !is.na(confirmed), deaths > confirmed) |>
  mutate(reason = "deaths > confirmed (season)")

message("6. confirmed > suspected, state-weeks: ", nrow(confirmed_over_suspected))
message("   deaths > confirmed, state-weeks: ", nrow(deaths_over_confirmed))
message("   deaths > confirmed, state-seasons: ", nrow(impossible_season))


# 7. Values far outside a state's usual range ------------------------------------------

# A transcription slip tends to show up as one week far above that state's
# normal weeks. Compare each week with the state's median non-zero week.
extreme <- state_week |>
  pivot_longer(c(suspected, confirmed), names_to = "variable", values_to = "value") |>
  filter(!is.na(value), value > 0) |>
  mutate(state_median = median(value), .by = c(state, variable)) |>
  filter(value >= 20, value > 10 * state_median)

message("7. State-weeks at least 20 and more than 10x the state's median week: ", nrow(extreme))


# 8. Which states have enough cases to stand alone ------------------------------------

# Roster per pair of years (see the analysis plan): for each adjacent-year
# transition, a state is its own stratum if it has at least `roster_min`
# confirmed in both years; the rest are pooled into "other" for that pair.
# Reported before any analysis runs, with the share of cases and deaths that
# ends up in "other", since reweighting inside "other" is invisible to the split.
# Only seasons used for inference; 2017 is context only (see 00_functions.R).
years <- sort(intersect(unique(state_season$year), inference_years))

roster <- tibble()

for (i in seq_len(length(years) - 1)) {
  y0 <- years[i]
  y1 <- years[i + 1]

  states <- pair_roster(state_season, y0, y1, roster_min)
  pair <- state_season |> filter(year %in% c(y0, y1))
  in_other <- pair |> filter(!state %in% states)

  this_pair <- tibble(transition = paste0(y0, "-", y1),
                      n_states = length(states),
                      states = paste(states, collapse = ", "),
                      other_share_confirmed = round(sum(in_other$confirmed, na.rm = TRUE) / sum(pair$confirmed, na.rm = TRUE), 3),
                      other_share_deaths = round(sum(in_other$deaths, na.rm = TRUE) / sum(pair$deaths, na.rm = TRUE), 3))

  roster <- bind_rows(roster, this_pair)
}

# Which seasons each state clears the threshold in, for states that ever do.
clears_by_season <- state_season |>
  mutate(clears = as.integer(!is.na(confirmed) & confirmed >= roster_min)) |>
  select(state, year, clears) |>
  pivot_wider(names_from = year, values_from = clears) |>
  filter(if_any(-state, \(x) x == 1)) |>
  arrange(state)

message("8. Roster per transition (at least ", roster_min, " confirmed in both years):")
print(roster)
message("   Seasons in which each state clears the threshold (1 = yes):")
print(clears_by_season)


# 9. Where the numbers came from ---------------------------------------------------------

provenance <- doc_status |>
  distinct(source_file, year, extraction) |>
  count(year, extraction) |>
  pivot_wider(names_from = extraction, values_from = n, values_fill = 0)

message("9. Sitreps by extraction source and year:")
print(provenance)


# Write one flag table -------------------------------------------------------------------

flags <- bind_rows(
  dupes |>
    transmute(year, epi_week, state, reason = "value built from several rows", source_file),
  states_in_both |>
    transmute(year, epi_week, state, reason = "state entered in more than one sitrep for the week", source_file),
  needs_a_look |>
    transmute(year, epi_week, state = NA_character_, reason = paste0(variable, ": ", status), source_file),
  impossible_week |>
    transmute(year, epi_week, state, reason, source_file),
  impossible_season |>
    transmute(year, epi_week = NA_integer_, state, reason, source_file = NA_character_),
  extreme |>
    transmute(year, epi_week, state, reason = paste0(variable, " more than 10x the state's median week"), source_file)) |>
  arrange(year, epi_week, state)

dir.create(here("data", "qc"), showWarnings = FALSE, recursive = TRUE)
write_csv(flags, here("data", "qc", "validation_flags.csv"))
write_csv(roster, here("data", "qc", "roster.csv"))

message("Wrote ", nrow(flags), " flags to data/qc/validation_flags.csv")
