# 07_model.R
#
# Hierarchical model of deaths among confirmed cases, by state and season.
#
# Primary (see the analysis plan):
#   deaths | trials(confirmed) ~ 1 + (1 | season) + (1 | state) + (1 | state:season)
#   binomial. There is one row per state-season, so the state:season term is an
#   observation-level effect and carries the overdispersion. A beta-binomial
#   with the same terms would have two overdispersion terms competing for the
#   same noise, so it is a sensitivity analysis, not the primary.
#
# The question: when CFR moves between seasons, does it move for all states
# together (season effect: a system-wide shift such as ascertainment) or state
# by state (state:season effect: localised change)? Read from the posterior
# standard deviations of the two terms. Caveat for Methods: the state:season
# term also absorbs data-quality noise, which tilts the comparison towards
# "state-specific" by construction.
#
# Estimands, all from the same fit:
#   1. P(sd_season > sd_state:season), reported as a probability, no threshold.
#   2. The 2025 -> 2026 contrast in season effects (logit scale), with P(> 0).
#   3. The 2026 state:season deviations for the states in the 2025-2026 roster.
#   4. Each season's effect, the reference distribution 2026 is read against.
#
# Every season is cut to the week the last season reaches in the frozen data.
# Sensitivity fits: beta-binomial; a wider and a tighter prior on the SDs; full
# years instead of the cut; the
# primary model without Edo, and without Ondo. Edo and Ondo hold most of the
# confirmed cases, so they carry most of the information about each season
# effect; a change specific to one of them could be read as "common". The
# refits show whether the reading survives without each.
#
# No within-season week term: phase is handled by the phase-matched comparison.
# Deaths given only nationally (2018, 2020) are redrawn across imputed datasets
# (redraw_allocation() in 00_functions.R), fitted together with brm_multiple.

library(dplyr)
library(tidyr)
library(readr)
library(here)
library(brms)
library(posterior)

source(here("R", "00_functions.R"))

state_week <- read_output("intermediate", "state_week")
blocks <- read_output("intermediate", "state_blocks")
rosters <- read_output("results", "decomposition_rosters")


# Settings ---------------------------------------------------------------------

# Smoke test: short chains, two imputations. Full run: LASSA_SMOKE=FALSE.
smoke <- as.logical(trimws(Sys.getenv("LASSA_SMOKE", "TRUE")))
n_imputations <- if (smoke) 2 else 10
n_chains <- 4
n_warmup <- if (smoke) 200 else 1000
n_iter <- if (smoke) 400 else 3000   # includes warmup

options(mc.cores = 4)
dir.create(here("data", "models"), showWarnings = FALSE)

target_year <- max(inference_years)
reference_year <- target_year - 1

# The fits. `sd_rate` is the rate of the exponential prior on every random-effect
# SD (weakly informative at 1). `drop_state` removes one state from the data.
# `window`: "cut" is every season cut to the week the last season reaches in the
# frozen data (primary, as in the decomposition and phase matching); "full" is
# full years with the last season necessarily partial (sensitivity).
fits_to_run <- tribble(
  ~fit_name,          ~family,          ~sd_rate, ~drop_state, ~window,
  "primary",          "binomial",       1,        NA,          "cut",
  "beta_binomial",    "beta_binomial",  1,        NA,          "cut",
  "prior_wider_sd",   "binomial",       0.5,      NA,          "cut",
  "prior_tighter_sd", "binomial",       2,        NA,          "cut",
  "without_edo",      "binomial",       1,        "Edo",       "cut",
  "without_ondo",     "binomial",       1,        "Ondo",      "cut",
  "full_years",       "binomial",       1,        NA,          "full")


# Units and imputed datasets -------------------------------------------------------

units <- bind_rows(state_week |> filter(reported) |> select(year, epi_week, state, confirmed, deaths, deaths_imputed),
                   blocks |> transmute(year, epi_week = end_week, state, confirmed, deaths, deaths_imputed = FALSE)) |>
  filter(year %in% inference_years, !is.na(confirmed), !is.na(deaths))

# The cut week: the last week the final season reached in the frozen data.
cut_week <- max(state_week$epi_week[state_week$year == target_year & state_week$reported])

# Each dataset redraws the allocated deaths, then sums to state-season totals.
make_dataset <- function(units) {
  redraw_allocation(units) |>
    summarise(confirmed = sum(confirmed), deaths = round(sum(deaths)), .by = c(year, state)) |>
    filter(confirmed > 0) |>
    mutate(season = factor(year), state = factor(state))
}

# One set of imputed datasets per window.
set.seed(20260929)
datasets <- list(cut = list(), full = list())
for (m in seq_len(n_imputations)) {
  datasets$cut[[m]] <- make_dataset(units |> filter(epi_week <= cut_week))
  datasets$full[[m]] <- make_dataset(units)
}

# A binomial likelihood cannot take more deaths than trials.
for (w in names(datasets)) {
  for (m in seq_len(n_imputations)) stopifnot(all(datasets[[w]][[m]]$deaths <= datasets[[w]][[m]]$confirmed))
}


# Fit one model and pull out its estimands ------------------------------------------------

fit_one <- function(fit_name, family, sd_rate, drop_state, window) {
  data_used <- datasets[[window]]
  if (!is.na(drop_state)) {
    for (m in seq_along(data_used)) data_used[[m]] <- data_used[[m]] |> filter(state != drop_state) |> droplevels()
  }

  priors <- c(prior(student_t(3, 0, 2.5), class = "Intercept"),
              set_prior(paste0("exponential(", sd_rate, ")"), class = "sd"))
  if (family == "beta_binomial") priors <- c(priors, prior(gamma(2, 0.1), class = "phi"))

  fit <- brm_multiple(deaths | trials(confirmed) ~ 1 + (1 | season) + (1 | state) + (1 | state:season),
                      data = data_used,
                      family = if (family == "binomial") binomial() else beta_binomial(),
                      prior = priors,
                      chains = n_chains, warmup = n_warmup, iter = n_iter,
                      control = list(adapt_delta = 0.99),
                      backend = "cmdstanr", seed = 20260929, refresh = 0)

  saveRDS(fit, here("data", "models", paste0(fit_name, if (smoke) "_smoke", ".rds")))

  # Convergence within each imputation: brm_multiple stacks the chains, 1-4
  # for the first imputation, 5-8 for the second, and so on.
  all_draws <- as_draws_array(fit)
  worst_rhat <- 0
  lowest_ess <- Inf
  for (m in seq_len(n_imputations)) {
    chains_m <- ((m - 1) * n_chains + 1):(m * n_chains)
    diag_m <- summarise_draws(subset_draws(all_draws, chain = chains_m), "rhat", "ess_bulk")
    worst_rhat <- max(worst_rhat, diag_m$rhat, na.rm = TRUE)
    lowest_ess <- min(lowest_ess, diag_m$ess_bulk, na.rm = TRUE)
  }
  divergences <- sum(subset(nuts_params(fit), Parameter == "divergent__")$Value)

  # Estimand 1: common against state-specific.
  draws <- as_draws_df(fit)
  sd_season <- draws$`sd_season__Intercept`
  sd_state_season <- draws$`sd_state:season__Intercept`

  # Estimand 2: the target season's effect minus the reference season's.
  contrast <- draws[[paste0("r_season[", target_year, ",Intercept]")]] -
    draws[[paste0("r_season[", reference_year, ",Intercept]")]]

  summary <- tibble(fit_name = fit_name,
                    sd_season_median = median(sd_season),
                    sd_state_season_median = median(sd_state_season),
                    p_sd_season_larger = mean(sd_season > sd_state_season),
                    contrast_median = median(contrast),
                    contrast_lo = quantile(contrast, 0.025),
                    contrast_hi = quantile(contrast, 0.975),
                    p_contrast_above_zero = mean(contrast > 0),
                    max_rhat = worst_rhat, min_ess_bulk = lowest_ess, divergences = divergences)

  # Estimands 3 and 4: season effects, and each state's deviation by season.
  season_effects <- ranef(fit, groups = "season")$season[, , "Intercept"] |>
    as_tibble(rownames = "season") |>
    mutate(fit_name = fit_name)

  state_season_effects <- ranef(fit, groups = "state:season")$`state:season`[, , "Intercept"] |>
    as_tibble(rownames = "state_season") |>
    separate_wider_delim(state_season, delim = "_", names = c("state", "season")) |>
    mutate(fit_name = fit_name)

  list(summary = summary, season_effects = season_effects, state_season_effects = state_season_effects)
}


# Run every fit --------------------------------------------------------------------------

model_summary <- tibble()
season_effects <- tibble()
state_season_effects <- tibble()

for (i in seq_len(nrow(fits_to_run))) {
  spec <- fits_to_run[i, ]
  message("Fitting ", spec$fit_name, " ...")
  result <- fit_one(spec$fit_name, spec$family, spec$sd_rate, spec$drop_state, spec$window)

  model_summary <- bind_rows(model_summary, result$summary)
  season_effects <- bind_rows(season_effects, result$season_effects)
  state_season_effects <- bind_rows(state_season_effects, result$state_season_effects)
}

# The target season's state deviations, for the states in the last pair's roster.
last_roster <- rosters$state[rosters$transition == paste0(reference_year, "-", target_year)]
target_state_deviations <- state_season_effects |>
  filter(season == as.character(target_year), state %in% last_roster)


# Save ---------------------------------------------------------------------------------

save_output(model_summary, "results", "model_summary")
save_output(season_effects, "results", "model_season_effects")
save_output(target_state_deviations, "results", "model_target_state_deviations")

message("brms ", packageVersion("brms"), "; CmdStan ", cmdstanr::cmdstan_version())
message("Fits: ", nrow(fits_to_run), "; imputations: ", n_imputations, "; iterations per chain: ", n_iter,
        if (smoke) " (SMOKE TEST)" else "")
message("Worst R-hat across fits: ", round(max(model_summary$max_rhat), 3),
        "; lowest bulk ESS: ", round(min(model_summary$min_ess_bulk)),
        "; divergent transitions: ", sum(model_summary$divergences))
