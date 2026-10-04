# 08_tipping.R
#
# Bias tipping point: how many unascertained non-fatal cases would reconcile the
# 2026 CFR with the reference CFR? If D deaths and C confirmed cases give the
# observed CFR, and the reference CFR is r, the number of missed survivors x
# that would bring the two into line solves D / (C + x) = r, so x = D / r - C.
#
# Anchored to the comparison the commentary made (see the analysis plan): the
# cumulative figures in NCDC's week 23 sitrep of 2026 against the same period of
# 2025. Run twice: on the published cumulative figures the commentary quoted,
# and on our reconstruction from the weekly tables, which differ by whatever
# NCDC revised retrospectively. A pooled 2018-2025 reference is shown as a
# secondary comparison.
#
# The reference CFR is uncertain, so x is reported as a distribution. The spread
# of r comes from the cluster bootstrap used throughout (resample_units() in
# 00_functions.R), which respects the clustering of deaths by state and week; a
# beta posterior would treat every case as independent and be too narrow. The
# published figures have no state-week breakdown, so their r is given the same
# bootstrap spread, centred on the published value. Target deaths and confirmed
# cases are observed and taken as given.
#
# x is judged against the pool it would have to come from: suspected cases in
# 2026 that were not confirmed. Primary estimand (see the analysis plan): x as a
# share of those, on the published figures. This covers the ascertainment rows
# of Table 1 that cannot be evaluated directly, by asking how large an
# ascertainment change would have to be.

library(dplyr)
library(tidyr)
library(readr)
library(here)

source(here("R", "00_functions.R"))

national_week <- read_output("results", "national_week")


# Settings ---------------------------------------------------------------------

smoke <- as.logical(trimws(Sys.getenv("LASSA_SMOKE", "TRUE")))   # set LASSA_SMOKE=FALSE for the full run
n_boot <- if (smoke) 20 else 1000

comparison_week <- 23
target_year <- max(inference_years)
reference_year <- target_year - 1

# Figures quoted by the commentary (Abdulrahim et al., Lancet Reg Health Afr
# 2026, citing the NCDC week 23 sitrep of 2026).
published <- tribble(
  ~year, ~confirmed, ~deaths, ~source,
  2025, 758, 143, "Abdulrahim et al. 2026, citing NCDC week 23 sitrep 2026",
  2026, 855, 214, "Abdulrahim et al. 2026, citing NCDC week 23 sitrep 2026")


# Our reconstruction at the comparison week -------------------------------------------

# Cumulative confirmed, deaths and suspected to the comparison week, from the
# weekly tables. `cum_known` must hold: a gap before that week would make the
# running total a lower bound.
# Suspected cases in a multi-week block are added at its end week, as confirmed
# cases and deaths are in 03_descriptives.R.
blocks <- read_output("intermediate", "state_blocks")
block_suspected <- blocks |>
  summarise(block_suspected = sum_or_na(suspected), .by = c(year, end_week))

suspected_cum <- national_week |>
  left_join(block_suspected, by = c("year", "epi_week" = "end_week")) |>
  arrange(year, epi_week) |>
  mutate(cum_suspected = cumsum(coalesce(suspected, 0) + coalesce(block_suspected, 0)), .by = year) |>
  select(year, epi_week, cum_suspected)

reconstructed_all <- national_week |>
  filter(epi_week == comparison_week) |>
  left_join(suspected_cum, by = c("year", "epi_week")) |>
  transmute(year, confirmed = cum_confirmed, deaths = cum_deaths, suspected = cum_suspected, cum_known)

reconstructed <- reconstructed_all |>
  filter(year %in% c(reference_year, target_year))

stopifnot(all(reconstructed$cum_known))

# Suspected cases in the target year that were not confirmed: the pool missed
# survivors would have to come from. Only our reconstruction has this.
not_confirmed <- reconstructed$suspected[reconstructed$year == target_year] - reconstructed$confirmed[reconstructed$year == target_year]


# Tipping point ---------------------------------------------------------------------

pub_ref <- published |> filter(year == reference_year)
pub_tgt <- published |> filter(year == target_year)
rec_ref <- reconstructed |> filter(year == reference_year)
rec_tgt <- reconstructed |> filter(year == target_year)

# Pooled reference: every inference season before the target whose running total
# at the comparison week is exact.
pooled_years <- reconstructed_all |>
  filter(year %in% setdiff(inference_years, target_year), cum_known) |>
  pull(year)
pooled <- reconstructed_all |>
  filter(year %in% pooled_years) |>
  summarise(confirmed = sum(confirmed), deaths = sum(deaths))


# Bootstrap spread of the reference CFRs ------------------------------------------------

# Reference CFRs at the comparison week, from one set of units.
reference_cfrs <- function(units) {
  to_week <- units |>
    filter(epi_week <= comparison_week) |>
    summarise(confirmed = sum(confirmed), deaths = sum(deaths), .by = year)
  ref <- to_week |> filter(year == reference_year)
  pool <- to_week |> filter(year %in% pooled_years)
  c(reference = ref$deaths / ref$confirmed, pooled = sum(pool$deaths) / sum(pool$confirmed))
}

units <- analysis_units(state_week = read_output("intermediate", "state_week"), blocks = blocks)

set.seed(20260929)
boot_r <- tibble()
for (b in seq_len(n_boot)) {
  r <- reference_cfrs(resample_units(units))
  boot_r <- bind_rows(boot_r, tibble(reference = r[["reference"]], pooled = r[["pooled"]]))
}

# Point estimates from the same state-week units the bootstrap resamples, so each
# interval is centred on its own estimate.
r_point <- reference_cfrs(units)
r_reconstructed <- r_point[["reference"]]
r_published <- pub_ref$deaths / pub_ref$confirmed
r_pooled <- r_point[["pooled"]]


# Tipping point ---------------------------------------------------------------------

# x for a reference CFR r: the missed survivors that would bring the target CFR
# down to r.
missed_survivors <- function(r, target_deaths, target_confirmed) target_deaths / r - target_confirmed

summarise_x <- function(x_point, x_boot, target_confirmed, label, role) {
  tibble(comparison = label, role = role,
         x = x_point, x_lo = quantile(x_boot, 0.025), x_hi = quantile(x_boot, 0.975),
         share_of_confirmed = x_point / target_confirmed,
         share_of_not_confirmed = x_point / not_confirmed,
         share_of_not_confirmed_lo = quantile(x_boot / not_confirmed, 0.025),
         share_of_not_confirmed_hi = quantile(x_boot / not_confirmed, 0.975),
         p_x_exceeds_not_confirmed = mean(x_boot > not_confirmed))
}

# Published: bootstrap spread of the reconstructed reference, centred on the
# published value.
x_published <- summarise_x(missed_survivors(r_published, pub_tgt$deaths, pub_tgt$confirmed),
                           missed_survivors(r_published + (boot_r$reference - r_reconstructed), pub_tgt$deaths, pub_tgt$confirmed),
                           pub_tgt$confirmed, "published figures, 2026 vs 2025 (the commentary's comparison)", "primary")
x_reconstructed <- summarise_x(missed_survivors(r_reconstructed, rec_tgt$deaths, rec_tgt$confirmed),
                               missed_survivors(boot_r$reference, rec_tgt$deaths, rec_tgt$confirmed),
                               rec_tgt$confirmed, "reconstructed weekly reports, 2026 vs 2025", "secondary")
x_pooled <- summarise_x(missed_survivors(r_pooled, rec_tgt$deaths, rec_tgt$confirmed),
                        missed_survivors(boot_r$pooled, rec_tgt$deaths, rec_tgt$confirmed),
                        rec_tgt$confirmed, paste0("reconstructed, 2026 vs pooled ", paste(range(pooled_years), collapse = "-"),
                                                  " (seasons: ", paste(pooled_years, collapse = ", "), ")"), "secondary")

tipping <- bind_rows(x_published, x_reconstructed, x_pooled) |>
  mutate(comparison_week = comparison_week)

# The gap between published and reconstructed figures at the comparison week is
# NCDC's retrospective revision plus any extraction difference. Reported
# alongside so the two versions of the comparison can be read together.
revision_gap <- published |>
  select(year, published_confirmed = confirmed, published_deaths = deaths) |>
  left_join(reconstructed |> select(year, confirmed, deaths), by = "year") |>
  mutate(confirmed_gap = published_confirmed - confirmed, deaths_gap = published_deaths - deaths)


# Save ---------------------------------------------------------------------------------

save_output(tipping, "results", "tipping_point")
save_output(revision_gap, "results", "revision_gap_week23")

message("Tipping point computed for ", nrow(tipping), " comparisons at week ", comparison_week,
        "; bootstrap replicates: ", n_boot, if (smoke) " (SMOKE TEST)" else "")
