# sim2c_isolated_null_variants.R
#
# Design simulation for A5, third stage: which null for the expected share?
# Compares, under the same simulated data as sim2b, the observed/expected ratio
# with five nulls:
#   nat    the season's national CFR
#   own30  the state's own CFR outside event windows, pooled over seasons, shrunk
#          towards the national CFR with a prior weight of 30 cases
#   own10  the same with a prior weight of 10 cases
#   own0   the state's own CFR outside event windows, unshrunk
#   group  the CFR outside event windows of the state's group (decomposition
#          roster states, or all others), pooled over seasons
# A good null gives a median ratio near 1 in every scenario without detection
# bias, and stays above 1 when detection is biased.
#
# Uses only the real structure (confirmed cases by state and week, and the
# isolated-detection events with their windows); deaths are simulated.

library(dplyr)
library(tidyr)
library(here)

source(here("R", "00_functions.R"))
set.seed(5)

n_reps <- 500

state_week <- read_output("intermediate", "state_week")
weeks <- state_week |>
  filter(year %in% inference_years, reported, !in_block, !is.na(confirmed)) |>
  select(year, epi_week, state, confirmed)

events <- read_output("intermediate", "isolated_events") |>
  filter(k == 3, j == 1, !is.na(death_in_window)) |>
  select(state, year, epi_week, cases_in_window)

windows <- events |>
  mutate(event_id = row_number()) |>
  reframe(epi_week = c(epi_week, epi_week + 1), .by = c(event_id, state, year)) |>
  inner_join(weeks, by = c("state", "year", "epi_week"))
in_window <- paste(windows$state, windows$year, windows$epi_week)

roster_states <- unique(read_output("results", "decomposition_rosters")$state)
states <- sort(unique(weeks$state))

scenarios <- tribble(
  ~scenario,                 ~pattern,     ~bias,
  "homogeneous",             "homog",      0,
  "random states",           "random",     0,
  "small states higher",     "smallhigh",  0,
  "small states lower",      "smalllow",   0,
  "detection bias 10%",      "homog",      0.10,
  "small higher + bias 10%", "smallhigh",  0.10)

state_cfr <- function(pattern) {
  base <- setNames(rep(qlogis(0.2), length(states)), states)
  if (pattern == "random") base <- base + rnorm(length(states), 0, 0.5)
  if (pattern == "smallhigh") base[!states %in% roster_states] <- base[!states %in% roster_states] + 0.5
  if (pattern == "smalllow") base[!states %in% roster_states] <- base[!states %in% roster_states] - 0.5
  plogis(base)
}

simulate_ratios <- function(pattern, bias) {
  p_state <- state_cfr(pattern)
  w <- weeks |> mutate(deaths = rbinom(n(), confirmed, p_state[state]))

  outcome <- windows |>
    left_join(w |> select(state, year, epi_week, deaths), by = c("state", "year", "epi_week")) |>
    summarise(death = as.integer(any(deaths > 0)), .by = c(event_id, state, year))
  outcome$death[runif(nrow(outcome)) < bias] <- 1L

  overall <- sum(w$deaths) / sum(w$confirmed)
  p_national <- w |> summarise(p_nat = sum(deaths) / sum(confirmed), .by = year)
  outside <- w |> filter(!paste(state, year, epi_week) %in% in_window)
  own <- outside |> summarise(x = sum(deaths), n = sum(confirmed), .by = state)
  grp <- outside |> mutate(g = state %in% roster_states) |> summarise(p_grp = sum(deaths) / sum(confirmed), .by = g)

  s <- events |>
    mutate(event_id = row_number(), g = state %in% roster_states) |>
    left_join(outcome |> select(event_id, death), by = "event_id") |>
    left_join(p_national, by = "year") |>
    left_join(own, by = "state") |>
    left_join(grp, by = "g") |>
    mutate(x = coalesce(x, 0), n = coalesce(n, 0),
           p30 = (x + 30 * overall) / (n + 30),
           p10 = (x + 10 * overall) / (n + 10),
           p0 = if_else(n > 0, x / n, overall))

  d <- sum(s$death)
  expected <- function(p) sum(1 - (1 - p)^s$cases_in_window)
  c(nat = d / expected(s$p_nat), own30 = d / expected(s$p30), own10 = d / expected(s$p10),
    own0 = d / expected(s$p0), group = d / expected(s$p_grp))
}

out <- tibble()
for (i in seq_len(nrow(scenarios))) {
  r <- replicate(n_reps, simulate_ratios(scenarios$pattern[i], scenarios$bias[i]))
  out <- bind_rows(out, tibble(scenario = scenarios$scenario[i], !!!as.list(round(apply(r, 1, median), 3))))
}

# How much of each small state's case count lies outside event windows: the
# information the own-state null has to work with.
outside_share <- weeks |>
  mutate(outside = !paste(state, year, epi_week) %in% in_window) |>
  summarise(share_outside = sum(confirmed[outside]) / sum(confirmed), .by = state) |>
  mutate(group = if_else(state %in% roster_states, "roster", "other")) |>
  summarise(median_share_outside = round(median(share_outside), 2), .by = group)

dir.create(here("sim", "output"), showWarnings = FALSE)
saveRDS(out, here("sim", "output", "sim2c_results.rds"))
print(out)
print(outside_share)
