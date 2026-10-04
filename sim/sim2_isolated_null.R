# sim2_isolated_null.R
#
# Design simulation for A5. With no detection bias, does the observed/expected
# ratio stay near 1 when state CFRs differ? And how large is it when detection
# is triggered by death?
#
# Uses only the real structure: the isolated-detection events (state, season,
# confirmed cases in the outcome window, k = 3, j = 1) and confirmed cases by
# state and season. Whether each window contains a death is simulated; the
# real outcome column is dropped on reading.
#
# The expected share uses the season's national CFR, here the case-weighted
# mean of the simulated state CFRs, as in the analysis.

library(dplyr)
library(tidyr)
library(here)

source(here("R", "00_functions.R"))
set.seed(2)

n_reps <- 2000      # for the distribution of the ratio
n_reps_ci <- 300    # for the false-positive rate of the bootstrap interval
n_boot <- 300

events <- read_output("intermediate", "isolated_events") |>
  filter(k == 3, j == 1, !is.na(death_in_window)) |>   # complete windows only, as in the analysis
  select(state, year, cases_in_window, group)

state_week <- read_output("intermediate", "state_week")
season_cases <- state_week |>
  filter(year %in% inference_years, reported, !is.na(confirmed)) |>
  summarise(confirmed = sum(confirmed), .by = c(year, state))

roster_states <- unique(read_output("results", "decomposition_rosters")$state)
states <- sort(unique(season_cases$state))

# State CFR patterns (logit scale around 20%), no detection bias unless stated.
scenarios <- tribble(
  ~scenario,             ~pattern,     ~bias,
  "homogeneous",         "homog",      0,
  "random states",       "random",     0,
  "small states higher", "smallhigh",  0,
  "small states lower",  "smalllow",   0,
  "detection bias 10%",  "homog",      0.10,
  "detection bias 20%",  "homog",      0.20)

state_cfr <- function(pattern) {
  base <- setNames(rep(qlogis(0.2), length(states)), states)
  if (pattern == "random") base <- base + rnorm(length(states), 0, 0.5)
  if (pattern == "smallhigh") base[!states %in% roster_states] <- base[!states %in% roster_states] + 0.5
  if (pattern == "smalllow") base[!states %in% roster_states] <- base[!states %in% roster_states] - 0.5
  plogis(base)
}

# One simulated ratio. `bias`: share of events where detection was triggered by
# a death, so the window contains one regardless of the CFR.
simulate_once <- function(pattern, bias, ev = events) {
  p_state <- state_cfr(pattern)
  p_national <- season_cases |>
    mutate(p = p_state[state]) |>
    summarise(p_nat = sum(confirmed * p) / sum(confirmed), .by = year)
  ev |>
    left_join(p_national, by = "year") |>
    mutate(p = p_state[state],
           death = rbinom(n(), 1, 1 - (1 - p)^cases_in_window),
           death = if_else(runif(n()) < bias, 1L, death),
           expected = 1 - (1 - p_nat)^cases_in_window)
}

ratio <- function(sim) sum(sim$death) / sum(sim$expected)

# Distribution of the ratio under each scenario.
dist <- tibble()
for (i in seq_len(nrow(scenarios))) {
  r <- replicate(n_reps, ratio(simulate_once(scenarios$pattern[i], scenarios$bias[i])))
  dist <- bind_rows(dist, tibble(scenario = scenarios$scenario[i], median = median(r),
                                 lo = quantile(r, 0.025), hi = quantile(r, 0.975)))
}

# How often the analysis's cluster-bootstrap interval excludes 1.
excludes_one <- tibble()
for (i in seq_len(nrow(scenarios))) {
  hits <- 0
  for (r in seq_len(n_reps_ci)) {
    sim <- simulate_once(scenarios$pattern[i], scenarios$bias[i])
    clusters <- sim |> summarise(d = sum(death), e = sum(expected), .by = c(state, year))
    boot <- replicate(n_boot, { pick <- clusters[sample(nrow(clusters), replace = TRUE), ]; sum(pick$d) / sum(pick$e) })
    ci <- quantile(boot, c(0.025, 0.975))
    hits <- hits + (ci[1] > 1 || ci[2] < 1)
  }
  excludes_one <- bind_rows(excludes_one, tibble(scenario = scenarios$scenario[i], share_excluding_1 = hits / n_reps_ci))
}

out <- dist |> left_join(excludes_one, by = "scenario") |> mutate(across(where(is.numeric), \(x) round(x, 3)))
dir.create(here("sim", "output"), showWarnings = FALSE)
saveRDS(out, here("sim", "output", "sim2_results.rds"))
cat("Events used:", nrow(events), "\n")
print(out)
