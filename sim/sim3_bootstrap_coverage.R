# sim3_bootstrap_coverage.R
#
# Design simulation for the bootstrap used in A1, A2 and A3. Resampling
# state-weeks independently ignores correlation between a state's consecutive
# weeks. How far do the intervals fall short, and does resampling blocks of
# consecutive weeks fix it?
#
# Uses only the real structure: weekly confirmed cases by state for 2024 and
# 2025, cut to the freeze week. Both seasons are simulated from the same
# process, so the true within-state (rate) component of the change is zero.
# Deaths in each state-week have a CFR with a state effect and a week effect
# that follows an AR(1) process within the state-season.
#
# Coverage is the share of replicates whose 95% interval for the rate
# component contains zero.

library(dplyr)
library(tidyr)
library(here)

source(here("R", "00_functions.R"))
set.seed(3)

n_reps <- as.integer(Sys.getenv("SIM_REPS", "150"))
n_boot <- 150
block_length <- as.integer(Sys.getenv("SIM_BLOCK", "4"))
# One correlation level per run when SIM_RHO is set, so levels can run in parallel.
rhos <- if (nzchar(Sys.getenv("SIM_RHO"))) as.numeric(Sys.getenv("SIM_RHO")) else c(0, 0.5, 0.8)
sd_week <- 0.5

state_week <- read_output("intermediate", "state_week")
cut_week <- max(state_week$epi_week[state_week$year == max(inference_years) & state_week$reported])

grid <- state_week |>
  filter(year %in% c(2024, 2025), reported, epi_week <= cut_week, !is.na(confirmed)) |>
  select(year, epi_week, state, confirmed) |>
  arrange(year, state, epi_week)

# Strata: states with at least 30 confirmed in both seasons stand alone.
totals <- grid |> summarise(n = sum(confirmed), .by = c(year, state))
roster <- totals |> summarise(ok = n() == 2 && all(n >= 30), .by = state) |> filter(ok) |> pull(state)
grid <- grid |> mutate(stratum = if_else(state %in% roster, state, "other"))

# Rate component for the pair, from state-week rows with simulated deaths.
rate_component <- function(d) {
  s <- d |> summarise(c = sum(confirmed), x = sum(deaths), .by = c(year, stratum))
  s <- s |> mutate(w = c / sum(c), cfr = if_else(c > 0, x / c, 0), .by = year)
  a <- s |> filter(year == 2024) |> arrange(stratum)
  b <- s |> filter(year == 2025) |> arrange(stratum)
  b <- b[match(a$stratum, b$stratum), ]
  sum((b$cfr - a$cfr) * (b$w + a$w) / 2, na.rm = TRUE)
}

# Simulate deaths: state effect fixed across both seasons, AR(1) week effect.
p_state <- setNames(qlogis(0.2) + rnorm(length(unique(grid$state)), 0, 0.5), unique(grid$state))
simulate <- function(rho) {
  grid |>
    mutate(e = {
      out <- numeric(n())
      key <- paste(year, state)
      for (k in unique(key)) {
        idx <- which(key == k)
        z <- numeric(length(idx))
        z[1] <- rnorm(1, 0, sd_week)
        if (length(idx) > 1) for (t in 2:length(idx)) z[t] <- rho * z[t - 1] + rnorm(1, 0, sd_week * sqrt(1 - rho^2))
        out[idx] <- z
      }
      out
    }, deaths = rbinom(n(), confirmed, plogis(p_state[state] + e))) |>
    select(-e)
}

# Current scheme: state-weeks resampled independently within season.
boot_independent <- function(d) slice_sample(d, prop = 1, replace = TRUE, by = year)

# Alternative: moving blocks of consecutive weeks within each state-season.
boot_blocks <- function(d) {
  pieces <- list()
  for (k in unique(paste(d$year, d$state))) {
    rows <- d[paste(d$year, d$state) == k, ]
    n <- nrow(rows)
    if (n <= block_length) { pieces[[k]] <- rows[sample(n, n, replace = TRUE), ]; next }
    starts <- sample(seq_len(n - block_length + 1), ceiling(n / block_length), replace = TRUE)
    idx <- as.vector(sapply(starts, \(s) s:(s + block_length - 1)))[seq_len(n)]
    pieces[[k]] <- rows[idx, ]
  }
  bind_rows(pieces)
}

coverage <- tibble()
for (rho in rhos) {
  hits_ind <- 0
  hits_blk <- 0
  for (r in seq_len(n_reps)) {
    d <- simulate(rho)
    ci_ind <- quantile(replicate(n_boot, rate_component(boot_independent(d))), c(0.025, 0.975))
    ci_blk <- quantile(replicate(n_boot, rate_component(boot_blocks(d))), c(0.025, 0.975))
    hits_ind <- hits_ind + (ci_ind[1] <= 0 && ci_ind[2] >= 0)
    hits_blk <- hits_blk + (ci_blk[1] <= 0 && ci_blk[2] >= 0)
  }
  coverage <- bind_rows(coverage, tibble(rho = rho, coverage_independent = hits_ind / n_reps, coverage_blocks = hits_blk / n_reps))
  message("rho ", rho, " done")
}

dir.create(here("sim", "output"), showWarnings = FALSE)
saveRDS(coverage, here("sim", "output", paste0("sim3_results_rho", paste(rhos, collapse = "_"), "_block", block_length, ".rds")))
print(coverage)
