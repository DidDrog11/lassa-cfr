# sim1_model_estimand.R
#
# Design simulation for A4. Can the primary estimand, P(sd_season > sd_state:season),
# tell a system-wide shift from a localised one with nine seasons?
#
# Uses only the real structure: confirmed cases by state and season (2018-2026,
# cut to the freeze week). Deaths are simulated from a known model, so no
# outcome data are used. For each scenario the model is fitted to simulated
# deaths and the estimand recorded.
#
# Scenarios (logit scale; state effects sd 0.5 throughout, baseline CFR 20%):
#   common      season sd 0.4, state:season sd 0.1  (system-wide variation)
#   localised   season sd 0.1, state:season sd 0.4  (state-specific variation)
#   equal       season sd 0.25, state:season sd 0.25
#   shift_all   no other variation except a +0.5 shift in 2026 for every state
#   shift_few   no other variation except a +0.5 shift in 2026 for three states
#
# Smaller MCMC settings than the analysis (one dataset, 1,000 iterations); the
# model is compiled once and refitted with update().

library(dplyr)
library(tidyr)
library(here)
library(brms)
library(posterior)

source(here("R", "00_functions.R"))

n_reps <- as.integer(Sys.getenv("SIM_REPS", "10"))
options(mc.cores = 4)
set.seed(1)

# Real structure: confirmed cases by state-season, cut to the freeze week.
state_week <- read_output("intermediate", "state_week")
target_year <- max(inference_years)
cut_week <- max(state_week$epi_week[state_week$year == target_year & state_week$reported])

cells <- state_week |>
  filter(year %in% inference_years, reported, epi_week <= cut_week, !is.na(confirmed)) |>
  summarise(confirmed = sum(confirmed), .by = c(year, state)) |>
  filter(confirmed > 0)

states <- sort(unique(cells$state))
seasons <- sort(unique(cells$year))
big_states <- cells |> summarise(n = sum(confirmed), .by = state) |> slice_max(n, n = 3) |> pull(state)

scenarios <- tribble(
  ~scenario,    ~sd_season, ~sd_ss, ~shift, ~shift_states,
  "common",     0.4,        0.1,    0,      "none",
  "localised",  0.1,        0.4,    0,      "none",
  "equal",      0.25,       0.25,   0,      "none",
  "shift_all",  0,          0,      0.5,    "all",
  "shift_few",  0,          0,      0.5,    "few")

# Simulate one dataset of deaths for a scenario.
simulate_deaths <- function(sc) {
  a_state <- setNames(rnorm(length(states), 0, 0.5), states)
  b_season <- setNames(rnorm(length(seasons), 0, sc$sd_season), seasons)
  d <- cells |>
    mutate(c_ss = rnorm(n(), 0, sc$sd_ss),
           shift = case_when(year != target_year ~ 0,
                             sc$shift_states == "all" ~ sc$shift,
                             sc$shift_states == "few" & state %in% big_states ~ sc$shift,
                             .default = 0),
           eta = qlogis(0.2) + a_state[state] + b_season[as.character(year)] + c_ss + shift,
           deaths = rbinom(n(), confirmed, plogis(eta)),
           season = factor(year), state = factor(state))
  d
}

# Compile once.
template <- brm(deaths | trials(confirmed) ~ 1 + (1 | season) + (1 | state) + (1 | state:season),
                data = simulate_deaths(scenarios[1, ]), family = binomial(),
                prior = c(prior(student_t(3, 0, 2.5), class = "Intercept"), prior(exponential(1), class = "sd")),
                chains = 4, iter = 1000, warmup = 500, control = list(adapt_delta = 0.99),
                backend = "cmdstanr", refresh = 0, seed = 1)

results <- tibble()
for (i in seq_len(nrow(scenarios))) {
  for (r in seq_len(n_reps)) {
    d <- simulate_deaths(scenarios[i, ])
    fit <- update(template, newdata = d, refresh = 0, seed = r)
    dr <- as_draws_df(fit)
    contrast <- dr[[paste0("r_season[", target_year, ",Intercept]")]] - dr[[paste0("r_season[", target_year - 1, ",Intercept]")]]
    results <- bind_rows(results, tibble(
      scenario = scenarios$scenario[i], rep = r,
      p_sd_season_larger = mean(dr$`sd_season__Intercept` > dr$`sd_state:season__Intercept`),
      p_contrast_above_zero = mean(contrast > 0)))
    message(scenarios$scenario[i], " rep ", r, " done")
  }
}

summary <- results |>
  summarise(median_p_sd = median(p_sd_season_larger), lo_p_sd = min(p_sd_season_larger), hi_p_sd = max(p_sd_season_larger),
            median_p_contrast = median(p_contrast_above_zero), .by = scenario)

dir.create(here("sim", "output"), showWarnings = FALSE)
saveRDS(results, here("sim", "output", "sim1_results.rds"))
print(summary)
