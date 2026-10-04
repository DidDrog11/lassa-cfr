# sim2b_isolated_state_null.R
#
# Design simulation for A5, second stage. sim2 showed that a single national CFR
# as the null cannot separate detection bias from states differing in CFR. Here
# the null is each state's own CFR, estimated from its state-weeks outside event
# windows, pooled over 2018-2026, and shrunk towards the national CFR with a
# prior weight of 30 cases. Does it stay near 1 without detection bias, and
# still detect bias when there is some?
#
# Uses only the real structure: confirmed cases by state and week, and the
# isolated-detection events (k = 3, j = 1) with their outcome windows. Deaths
# are simulated in every state-week; the real outcome is never read.

library(dplyr)
library(tidyr)
library(here)

source(here("R", "00_functions.R"))
set.seed(4)

n_reps <- 1000
n_reps_ci <- 200
n_boot <- 200
prior_cases <- as.numeric(Sys.getenv("SIM_PRIOR", "30"))   # 0 = unshrunk own-state CFR

state_week <- read_output("intermediate", "state_week")
weeks <- state_week |>
  filter(year %in% inference_years, reported, !in_block, !is.na(confirmed)) |>
  select(year, epi_week, state, confirmed)

events <- read_output("intermediate", "isolated_events") |>
  filter(k == 3, j == 1, !is.na(death_in_window)) |>
  select(state, year, epi_week, cases_in_window)

# The state-weeks inside each event's outcome window (event week and the next).
windows <- events |>
  mutate(event_id = row_number()) |>
  reframe(epi_week = c(epi_week, epi_week + 1), .by = c(event_id, state, year)) |>
  inner_join(weeks, by = c("state", "year", "epi_week"))
in_window <- paste(windows$state, windows$year, windows$epi_week)

roster_states <- unique(read_output("results", "decomposition_rosters")$state)
states <- sort(unique(weeks$state))

scenarios <- tribble(
  ~scenario,             ~pattern,     ~bias,
  "homogeneous",         "homog",      0,
  "random states",       "random",     0,
  "small states higher", "smallhigh",  0,
  "small states lower",  "smalllow",   0,
  "detection bias 10%",  "homog",      0.10,
  "detection bias 20%",  "homog",      0.20,
  "small higher + bias 10%", "smallhigh", 0.10)

state_cfr <- function(pattern) {
  base <- setNames(rep(qlogis(0.2), length(states)), states)
  if (pattern == "random") base <- base + rnorm(length(states), 0, 0.5)
  if (pattern == "smallhigh") base[!states %in% roster_states] <- base[!states %in% roster_states] + 0.5
  if (pattern == "smalllow") base[!states %in% roster_states] <- base[!states %in% roster_states] - 0.5
  plogis(base)
}

# One simulation: deaths in every state-week, then the window outcome, then the
# two nulls. `bias` is the share of events whose window gains a death because
# detection was triggered by one.
simulate_once <- function(pattern, bias) {
  p_state <- state_cfr(pattern)
  w <- weeks |> mutate(deaths = rbinom(n(), confirmed, p_state[state]))

  # Outcome: any death in the window, or a death-triggered detection.
  outcome <- windows |>
    left_join(w |> select(state, year, epi_week, deaths), by = c("state", "year", "epi_week")) |>
    summarise(death = as.integer(any(deaths > 0)), .by = c(event_id, state, year))
  outcome$death[runif(nrow(outcome)) < bias] <- 1L

  # National CFR by season (all weeks), and each state's CFR from weeks outside
  # windows, pooled over seasons and shrunk towards the overall national CFR.
  p_national <- w |> summarise(p_nat = sum(deaths) / sum(confirmed), .by = year)
  overall <- sum(w$deaths) / sum(w$confirmed)
  p_own <- w |>
    filter(!paste(state, year, epi_week) %in% in_window) |>
    summarise(x = sum(deaths), n = sum(confirmed), .by = state) |>
    mutate(p_own = (x + prior_cases * overall) / (n + prior_cases))

  events |>
    mutate(event_id = row_number()) |>
    left_join(outcome |> select(event_id, death), by = "event_id") |>
    left_join(p_national, by = "year") |>
    left_join(p_own |> select(state, p_own, x_own = x, n_own = n), by = "state") |>
    mutate(p_own = coalesce(p_own, overall),
           expected_national = 1 - (1 - p_nat)^cases_in_window,
           expected_own = 1 - (1 - p_own)^cases_in_window)
}

dist <- tibble()
for (i in seq_len(nrow(scenarios))) {
  r_nat <- numeric(n_reps); r_own <- numeric(n_reps)
  for (r in seq_len(n_reps)) {
    s <- simulate_once(scenarios$pattern[i], scenarios$bias[i])
    r_nat[r] <- sum(s$death) / sum(s$expected_national)
    r_own[r] <- sum(s$death) / sum(s$expected_own)
  }
  dist <- bind_rows(dist, tibble(scenario = scenarios$scenario[i],
                                 national_median = median(r_nat), own_median = median(r_own),
                                 own_lo = quantile(r_own, 0.025), own_hi = quantile(r_own, 0.975)))
}

# How often the cluster-bootstrap interval for the own-state ratio excludes 1.
excl <- tibble()
for (i in seq_len(nrow(scenarios))) {
  hits <- 0
  for (r in seq_len(n_reps_ci)) {
    s <- simulate_once(scenarios$pattern[i], scenarios$bias[i])
    # Each replicate redraws every state's own CFR from its uncertainty (Jeffreys
    # beta from the deaths and cases outside windows), then resamples clusters.
    s <- s |> mutate(x_own = coalesce(x_own, 0), n_own = coalesce(n_own, 0))
    states_s <- distinct(s, state, x_own, n_own, p_own)
    b <- replicate(n_boot, {
      p_draw <- setNames(ifelse(states_s$n_own > 0, rbeta(nrow(states_s), states_s$x_own + 0.5, states_s$n_own - states_s$x_own + 0.5), states_s$p_own), states_s$state)
      e <- 1 - (1 - p_draw[s$state])^s$cases_in_window
      cl <- s |> mutate(e = e) |> summarise(d = sum(death), e = sum(e), .by = c(state, year))
      p <- cl[sample(nrow(cl), replace = TRUE), ]
      sum(p$d) / sum(p$e) })
    ci <- quantile(b, c(0.025, 0.975))
    hits <- hits + (ci[1] > 1 || ci[2] < 1)
  }
  excl <- bind_rows(excl, tibble(scenario = scenarios$scenario[i], own_share_excluding_1 = hits / n_reps_ci))
}

out <- dist |> left_join(excl, by = "scenario") |> mutate(across(where(is.numeric), \(x) round(x, 3)))
dir.create(here("sim", "output"), showWarnings = FALSE)
saveRDS(out, here("sim", "output", paste0("sim2b_results_prior", prior_cases, ".rds")))
cat("Events:", nrow(events), " state-weeks:", nrow(weeks), "\n")
print(out)
