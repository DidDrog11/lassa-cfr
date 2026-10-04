# 09_isolated.R
#
# Isolated-detection statistic: a proxy for the share of confirmed cases first
# sampled at or after death, which the sitreps do not report.
#
# An isolated detection event is a state-week with confirmed cases after k weeks
# in which that state confirmed none. The lookback clears the pool of earlier
# cases whose deaths could land in this week. The outcome is whether a death is
# reported in the detection week or the j weeks after it, which catches deaths
# that had not yet happened at detection.
#
# If detection were blind to severity, a window holding n confirmed cases would
# include at least one death with probability 1 - (1 - p)^n, where p is the CFR.
# More events with a death than that is the signature of detection triggered by
# death or severe illness. The statistic is sensitive to missed survivors and
# largely blind to missed deaths, and is reported that way.
#
# k and j cannot be estimated from these data, because the case-to-death interval
# is exactly what the sitreps do not report. They are taken from LASCOPE
# (Duvignaud et al., Lancet Glob Health 2021): median admission to death 3 days;
# 51 of 62 deaths within 7 days of admission, 58 within 19 days. So k = 3 weeks
# clears about 94% of earlier cases' deaths, and j = 1 (detection week plus the
# next) catches over 80%. Both are varied.
#
# Scope: high-burden states report almost continuously, so they rarely produce
# an isolated detection (Edo and Ondo give only a handful of events, all in
# mid-season lulls). The statistic therefore speaks mainly to low- and
# mid-burden states. Those may differ in baseline CFR for reasons other than
# detection (for example, no treatment centre), which a national-CFR null does
# not capture: a limitation for Methods.
#
# Weeks that cannot be read are unobserved and break both windows: weeks with
# no sitrep, the 2022 weeks 19-23 block, and weeks whose deaths were allocated
# from a national total.

library(dplyr)
library(tidyr)
library(readr)
library(here)

source(here("R", "00_functions.R"))

state_week <- read_output("intermediate", "state_week")
national_season <- read_output("results", "national_season")
rosters <- read_output("results", "decomposition_rosters")


# Settings ---------------------------------------------------------------------

k_main <- 3
j_main <- 1
k_values <- c(2, 3, 4, 6)
j_values <- c(0, 1, 2)
early_season_weeks <- 1:13   # the "early" phase stratum; later weeks are "late"

# Reporting continuity stratum, a measure of how often a state reports suspected
# cases: a state-season is "frequent" if the state reported at least one
# suspected case in at least this share of the weeks where suspected cases are
# given by state, "intermittent" otherwise. Suspected cases are not given by state
# in 2018-2019 (or in early 2020), so those seasons have no continuity value and
# their events are left out of this stratum only. A week with no suspected
# cases may be a true zero or cases that were not recognised or reported; the
# counts cannot tell these apart, so this describes reporting, not capacity.
continuity_cut <- 0.5

# p in the null (see the analysis plan):
#   primary      each state's own CFR outside the event windows, pooled over
#                2018-2026 (computed below, after the events are found).
#   secondary    the season's national CFR. It cannot separate detection bias
#                from states differing in CFR (design simulations in sim/).
#   secondary    LASCOPE's CFR among patients admitted to a specialist treatment
#                centre, 62 of 510 (12%), an external value not shaped by the
#                surveillance funnel. It is a reference point, not a bound:
#                specialist treatment would make it lower than among untreated or
#                late-presenting cases, and leaving out mild cases would make it
#                higher.
p_lascope <- 62 / 510


# Prepare one row per state per week, in time order ------------------------------------

# `observed` is TRUE only where both confirmed and deaths can be read directly.
# A running week index `t` lets the windows cross from one season into the next.
panel <- state_week |>
  filter(year %in% inference_years) |>
  mutate(observed = reported & !in_block & !deaths_imputed & !is.na(confirmed) & !is.na(deaths)) |>
  arrange(state, year, epi_week) |>
  mutate(t = row_number(), .by = state) |>
  select(state, year, epi_week, t, suspected, confirmed, deaths, observed, reported, in_block)


# Find events for one k and j ----------------------------------------------------------

# For each state, walk through the weeks. A week is an event if it is observed,
# has confirmed cases, and the k weeks before it are all observed with none. The
# outcome window is the event week plus j weeks; if any of those is unobserved
# the event is kept but its outcome is NA, and it is counted as censored.
find_events <- function(panel, k, j) {
  events <- tibble()

  for (s in unique(panel$state)) {
    d <- panel |> filter(state == s)

    for (i in seq_len(nrow(d))) {
      if (!d$observed[i] || d$confirmed[i] == 0 || i <= k) next

      lookback <- (i - k):(i - 1)
      if (!all(d$observed[lookback]) || any(d$confirmed[lookback] > 0)) next

      window <- i:min(i + j, nrow(d))
      complete_window <- length(window) == j + 1 && all(d$observed[window])

      events <- bind_rows(events, tibble(
        state = s, year = d$year[i], epi_week = d$epi_week[i], t = d$t[i],
        cases_at_detection = d$confirmed[i],
        cases_in_window = sum(d$confirmed[window]),
        death_in_window = if (complete_window) any(d$deaths[window] > 0) else NA))
    }
  }
  events |> mutate(k = k, j = j)
}


# Run over the grid of k and j -----------------------------------------------------------

events <- tibble()
for (k in k_values) {
  for (j in j_values) events <- bind_rows(events, find_events(panel, k, j))
}

# Attach the null expectation under both values of p (see Settings).
# States that ever stand alone in a decomposition roster form the "roster" group.
roster_states <- unique(rosters$state)

# Reporting continuity of each state-season: the share of weeks with at least
# one suspected case, among the reported weeks where suspected cases are given
# by state (the 2022 multi-week block is not a week and is left out). Saved as a
# supplementary table; used here as a stratum. Seasons with no such weeks get NA.
reporting_weeks <- panel |>
  filter(reported, !in_block, !is.na(suspected)) |>
  summarise(weeks_known = n(), weeks_with_suspected = sum(suspected > 0), .by = c(state, year)) |>
  mutate(share_weeks_with_suspected = weeks_with_suspected / weeks_known,
         continuity = if_else(share_weeks_with_suspected >= continuity_cut, "frequent", "intermittent"))

# Primary null: each state's own CFR, from its observed state-weeks outside the
# event windows of the same k and j, pooled over 2018-2026, unshrunk. Design
# simulations (sim/sim2_isolated_null.R, sim2b, sim2c) showed that a single
# national CFR cannot separate detection bias from states differing in CFR, and
# that shrinking towards the national CFR brings the bias back, because states
# that report intermittently have most of their cases inside event windows.
# A state with no cases outside windows falls back to the season's national CFR.
own_state_cfr <- function(ev, k_set, j_set) {
  ev_kj <- ev |> filter(k == k_set, j == j_set)
  windows <- ev_kj |> reframe(t = t:(t + j_set), .by = c(state, year, epi_week))
  in_window <- paste(windows$state, windows$t)
  panel |>
    filter(observed, !paste(state, t) %in% in_window) |>
    summarise(x_own = sum(deaths), n_own = sum(confirmed), .by = state) |>
    mutate(k = k_set, j = j_set)
}

own <- tibble()
for (k in k_values) {
  for (j in j_values) own <- bind_rows(own, own_state_cfr(events, k, j))
}

events <- events |>
  left_join(national_season |> select(year, p_national = cfr_matched), by = "year") |>   # weeks with known deaths, as in inference
  left_join(own, by = c("state", "k", "j")) |>
  left_join(reporting_weeks |> select(state, year, continuity), by = c("state", "year")) |>
  mutate(x_own = coalesce(x_own, 0), n_own = coalesce(n_own, 0),
         p_own = if_else(n_own > 0, x_own / n_own, p_national),
         expected = 1 - (1 - p_own)^cases_in_window,
         expected_national = 1 - (1 - p_national)^cases_in_window,
         expected_lascope = 1 - (1 - p_lascope)^cases_in_window,
         group = if_else(state %in% roster_states, "roster states", "other states"),
         phase = if_else(epi_week %in% early_season_weeks, "early", "late"))


# Summaries ----------------------------------------------------------------------------------

# Primary estimand: at k = 3, j = 1, the ratio of the observed share of events
# with a death to the share expected under severity-blind detection with p =
# each state's own CFR outside event windows. `n` in the expectation is the
# confirmed cases in the whole outcome window, since a death in the window can
# come from any of them. Secondary: the same ratio with the season's national
# CFR and with LASCOPE's, and by state group, season phase and reporting
# continuity. Sensitivity: the k x j grid. Descriptive only: by season.
#
# Events cluster within state-seasons (a state can have several in one season),
# so intervals come from resampling whole state-seasons. Each state's own CFR is
# an estimate, so every replicate also redraws it from a Jeffreys beta on its
# deaths and cases outside windows (states with none keep the national value).
# Only events with a complete outcome window count.
smoke <- as.logical(trimws(Sys.getenv("LASSA_SMOKE", "TRUE")))   # set LASSA_SMOKE=FALSE for the full run
n_boot <- if (smoke) 20 else 1000

summarise_with_bootstrap <- function(ev) {
  n_censored <- sum(is.na(ev$death_in_window))
  ev <- ev |> filter(!is.na(death_in_window))
  cluster_key <- paste(ev$state, ev$year)
  clusters <- unique(cluster_key)
  states_ev <- ev |> distinct(state, x_own, n_own)

  ratio_from <- function(rows, expected_col) sum(ev$death_in_window[rows]) / sum(expected_col[rows])

  boot_ratio <- numeric(n_boot)
  boot_ratio_national <- numeric(n_boot)
  boot_ratio_lascope <- numeric(n_boot)
  for (b in seq_len(n_boot)) {
    # Redraw each state's own CFR, then the expected share for every event.
    p_draw <- setNames(rbeta(nrow(states_ev), states_ev$x_own + 0.5, states_ev$n_own - states_ev$x_own + 0.5), states_ev$state)
    p_b <- if_else(ev$n_own > 0, p_draw[ev$state], ev$p_national)
    expected_b <- 1 - (1 - p_b)^ev$cases_in_window

    # Resample whole state-season clusters.
    picked <- sample(clusters, length(clusters), replace = TRUE)
    rows <- unlist(lapply(picked, \(cl) which(cluster_key == cl)))

    boot_ratio[b] <- ratio_from(rows, expected_b)
    boot_ratio_national[b] <- ratio_from(rows, ev$expected_national)
    boot_ratio_lascope[b] <- ratio_from(rows, ev$expected_lascope)
  }

  all_rows <- seq_len(nrow(ev))
  tibble(n_events = nrow(ev) + n_censored, n_censored = n_censored,
         n_clusters = length(clusters),
         observed = mean(ev$death_in_window),
         expected = mean(ev$expected),
         ratio = ratio_from(all_rows, ev$expected),
         ratio_lo = quantile(boot_ratio, 0.025), ratio_hi = quantile(boot_ratio, 0.975),
         ratio_national = ratio_from(all_rows, ev$expected_national),
         ratio_national_lo = quantile(boot_ratio_national, 0.025), ratio_national_hi = quantile(boot_ratio_national, 0.975),
         ratio_lascope = ratio_from(all_rows, ev$expected_lascope),
         ratio_lascope_lo = quantile(boot_ratio_lascope, 0.025), ratio_lascope_hi = quantile(boot_ratio_lascope, 0.975))
}

set.seed(20260929)

# The k x j grid (the main setting is the primary estimand; the rest is the
# sensitivity table).
summary_overall <- tibble()
for (k in k_values) {
  for (j in j_values) {
    ev <- events |> filter(k == !!k, j == !!j)
    summary_overall <- bind_rows(summary_overall,
                                 summarise_with_bootstrap(ev) |> mutate(k = k, j = j, primary = k == k_main & j == j_main))
  }
}

# Strata at the main setting only.
main_events <- events |> filter(k == k_main, j == j_main)
summary_strata <- tibble()
for (stratum in c("group", "phase", "continuity", "year")) {
  for (level in sort(unique(main_events[[stratum]]))) {
    ev <- main_events |> filter(.data[[stratum]] == level)
    summary_strata <- bind_rows(summary_strata,
                                summarise_with_bootstrap(ev) |>
                                  mutate(stratum = stratum, level = as.character(level),
                                         role = if (stratum == "year") "descriptive" else "secondary"))
  }
}


# Save ---------------------------------------------------------------------------------

save_output(events, "intermediate", "isolated_events")
save_output(summary_overall, "results", "isolated_summary")
save_output(summary_strata, "results", "isolated_strata")
save_table(reporting_weeks, "reporting_weeks_supplementary")

main <- summary_overall |> filter(primary)
message("Main setting k = ", k_main, ", j = ", j_main, ": ", main$n_events, " events in ", main$n_clusters,
        " state-seasons, ", main$n_censored, " with an incomplete outcome window")
