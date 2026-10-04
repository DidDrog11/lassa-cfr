# 12_figures.R
#
# The three main-text figures, for the primary analyses, and two supplementary
# figures, for the secondary analyses. Presentation only: every value is read
# from an object written by 01-11, so a figure can never disagree with a number
# in the text. Each figure answers one Results question:
#
#   Figure 1   Is 2026 unusual once season timing is matched?
#   Figure 2   Is it where the cases are? (decomposition)
#   Figure 3   How large would an ascertainment change need to be?
#   Figure S1  Is it everywhere or somewhere? (model, secondary)
#   Figure S2  Is detection triggered by death? (secondary)
#
# Colour: 2026 blue and 2025 orange throughout (the first two slots of a
# validated colour-blind-safe palette); other seasons grey. Identity is never
# carried by colour alone: series are labelled directly.

library(dplyr)
library(tidyr)
library(readr)
library(here)
library(ggplot2)
library(patchwork)
library(posterior)

source(here("R", "00_functions.R"))

dir.create(here("figures"), showWarnings = FALSE)

target_year <- max(inference_years)
reference_year <- target_year - 1

col_target <- "#2a78d6"
col_reference <- "#eb6834"
col_other <- "grey70"
col_ink <- "grey20"

season_colour <- function(year) {
  case_when(year == target_year ~ "target", year == reference_year ~ "reference", .default = "other")
}
season_palette <- c(target = col_target, reference = col_reference, other = col_other)

theme_paper <- theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey92", linewidth = 0.3),
        axis.title = element_text(colour = col_ink),
        plot.tag = element_text(face = "bold"),
        legend.position = "none")

# Two-digit season labels ('18, '19, ...) so nine or ten seasons fit on an axis.
short_year <- function(x) paste0("'", substr(x, 3, 4))

save_figure <- function(plot, name, width = 180, height = 120) {
  ggsave(here("figures", paste0(name, ".png")), plot, width = width, height = height, units = "mm", dpi = 300, bg = "white")
  ggsave(here("figures", paste0(name, ".pdf")), plot, width = width, height = height, units = "mm", bg = "white")
}


# Figure 1: reported CFR, naive and phase-matched ----------------------------------------

national_week <- read_output("results", "national_week")
national_season <- read_output("results", "national_season")
phase_points <- read_output("results", "phase_points")

# (a) Cumulative CFR through each season, where the running total is known.
cumulative <- national_week |>
  filter(cum_known, cum_confirmed >= 20) |>   # very early weeks are too small to read
  mutate(season = season_colour(year))

last_points <- cumulative |> filter(year %in% c(reference_year, target_year)) |> slice_max(epi_week, by = year)

fig1a <- ggplot(cumulative, aes(epi_week, cfr_cumulative, group = year, colour = season)) +
  geom_vline(xintercept = 23, linetype = "dashed", colour = "grey60", linewidth = 0.3) +
  geom_line(data = filter(cumulative, season == "other"), linewidth = 0.4) +
  geom_line(data = filter(cumulative, season != "other"), linewidth = 0.8) +
  geom_point(data = phase_points |> filter(year %in% c(reference_year, target_year), grepl("^phase", comparison)) |>
               mutate(season = season_colour(year)),
             aes(epi_week, cfr), size = 2.2, shape = 21, fill = "white", stroke = 0.9) +
  geom_text(data = last_points, aes(label = year), hjust = -0.2, size = 3, show.legend = FALSE) +
  annotate("text", x = 23, y = Inf, label = "week 23", vjust = 1.5, hjust = -0.1, size = 2.8, colour = "grey40") +
  scale_colour_manual(values = season_palette) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.12))) +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(clip = "off") +
  labs(x = "Epidemiological week", y = "Cumulative reported CFR",
       subtitle = "Open circles: 25th and 50th percentile phase points") +
  theme_paper

# (b) Season CFR, 2017-2026. Partial seasons drawn hollow.
season_points <- national_season |>
  mutate(season = season_colour(year), fill_group = if_else(partial_year, "partial", "complete"))

fig1b <- ggplot(season_points, aes(factor(year), cfr, colour = season)) +
  geom_point(aes(shape = fill_group), size = 2.6, stroke = 0.9, fill = "white") +
  scale_shape_manual(values = c(complete = 16, partial = 21)) +
  scale_colour_manual(values = season_palette) +
  scale_x_discrete(labels = short_year) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Season CFR", subtitle = "Hollow: incomplete season") +
  theme_paper

figure1 <- fig1a + fig1b + plot_layout(widths = c(1.8, 1.2)) + plot_annotation(tag_levels = "a")
save_figure(figure1, "figure1_cfr")


# Figure 2: decomposition ----------------------------------------------------------------

decomposition <- read_output("results", "decomposition_ci")
every_state <- read_output("results", "contributions_every_state")

last_transition <- paste0(reference_year, "-", target_year)

# (a) Rate and composition for every transition, with bootstrap intervals.
components <- decomposition |>
  filter(component %in% c("rate", "composition")) |>
  mutate(component = factor(component, levels = c("rate", "composition"),
                            labels = c("Within-state (rate)", "Between-state (composition)")),
         highlight = transition == last_transition)

fig2a <- ggplot(components, aes(estimate, transition, colour = component)) +
  geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_pointrange(aes(xmin = lo, xmax = hi), position = position_dodge(width = 0.6), size = 0.25, linewidth = 0.5) +
  scale_colour_manual(values = c(col_target, col_reference)) +
  scale_x_continuous(labels = scales::label_number(scale = 100, suffix = " pp")) +
  labs(x = "Contribution to change in national CFR", y = NULL, subtitle = "Each pair of seasons") +
  theme_paper

# (b) Per-state contributions to the last transition, every state separate
# (secondary, noisy). The ten largest by absolute total contribution.
last_states <- every_state |>
  filter(transition == last_transition) |>
  mutate(total = sum(estimate), .by = state) |>
  filter(state %in% (distinct(pick(state, total)) |> slice_max(abs(total), n = 10) |> pull(state))) |>
  mutate(component = factor(component, levels = c("rate", "composition_centred"),
                            labels = c("Within-state (rate)", "Between-state (composition)")),
         state = reorder(state, total))

fig2b <- ggplot(last_states, aes(estimate, state, colour = component)) +
  geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_pointrange(aes(xmin = lo, xmax = hi), position = position_dodge(width = 0.6), size = 0.25, linewidth = 0.5) +
  scale_colour_manual(values = c(col_target, col_reference)) +
  scale_x_continuous(labels = scales::label_number(scale = 100, suffix = " pp")) +
  labs(x = paste("Contribution to the", last_transition, "change"), y = NULL,
       subtitle = "Every state separate (secondary, noisy)") +
  theme_paper

# One shared legend for both panels, below them.
figure2 <- fig2a + fig2b + plot_layout(guides = "collect") + plot_annotation(tag_levels = "a") &
  theme(legend.position = "bottom", legend.title = element_blank())
save_figure(figure2, "figure2_decomposition", height = 110)


# Figure 3: how large an ascertainment change would need to be ---------------------------------

tipping <- read_output("results", "tipping_point")
positivity_national <- read_output("results", "positivity_national")
positivity_state <- read_output("results", "positivity_state")
roster_states <- unique(read_output("results", "decomposition_rosters")$state)

# (a) Missed survivors needed, as a share of suspected cases not confirmed.
# Short labels here; the full description of each comparison is in the caption.
tipping_plot <- tipping |>
  mutate(label = case_when(role == "primary" ~ paste0("Published, ", target_year, " vs ", reference_year),
                           grepl("pooled", comparison) ~ paste0("Reconstructed, ", target_year, " vs pooled"),
                           .default = paste0("Reconstructed, ", target_year, " vs ", reference_year)),
         label = reorder(label, role == "primary"))

fig3a <- ggplot(tipping_plot, aes(share_of_not_confirmed, label, colour = role)) +
  geom_pointrange(aes(xmin = share_of_not_confirmed_lo, xmax = share_of_not_confirmed_hi), size = 0.3, linewidth = 0.6) +
  scale_colour_manual(values = c(primary = col_target, secondary = col_other)) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = stringr::str_wrap(paste("Missed survivors needed at week 23, as a share of", target_year,
                                   "suspected cases not confirmed"), 45), y = NULL) +
  theme_paper

# (b) Suspected-case positivity (a proxy, not test positivity) by season:
# nationally, and for each roster state from 2020.
state_lines <- positivity_state |>
  filter(state %in% roster_states, year >= 2020, !is.na(positivity))

fig3b <- ggplot() +
  geom_line(data = state_lines, aes(year, positivity, group = state), colour = col_other, linewidth = 0.4) +
  geom_text(data = state_lines |> slice_max(year, by = state), aes(year, positivity, label = state),
            hjust = -0.1, size = 2.6, colour = "grey45") +
  geom_line(data = positivity_national |> filter(!is.na(positivity), year %in% inference_years),
            aes(year, positivity), colour = col_ink, linewidth = 0.9) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = inference_years, labels = short_year, expand = expansion(mult = c(0.02, 0.25))) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = "Suspected-case positivity (proxy)", subtitle = "Black: national. Grey: roster states") +
  theme_paper

figure3 <- fig3a + fig3b + plot_layout(widths = c(1.1, 1)) + plot_annotation(tag_levels = "a")
save_figure(figure3, "figure3_ascertainment", height = 100)


# Supplementary figure S1: common or localised change (secondary) ------------------------------------------------------

model_path <- here("data", "models", "primary.rds")
if (!file.exists(model_path)) {
  # Fall back to the smoke-test fit only in a smoke run; a full run must use the full fit.
  if (!as.logical(trimws(Sys.getenv("LASSA_SMOKE", "TRUE")))) stop("Full model fit not found: ", model_path)
  model_path <- here("data", "models", "primary_smoke.rds")
}
primary_fit <- readRDS(model_path)
draws <- as_draws_df(primary_fit)

season_effects <- read_output("results", "model_season_effects") |> filter(fit_name == "primary")
target_deviations <- read_output("results", "model_target_state_deviations") |> filter(fit_name == "primary")

# (a) Posterior of the two standard deviations.
sds <- tibble(`Season (common)` = draws$`sd_season__Intercept`,
              `State by season (localised)` = draws$`sd_state:season__Intercept`) |>
  pivot_longer(everything(), names_to = "term", values_to = "sd")

figs1a <- ggplot(sds, aes(sd, fill = term, colour = term)) +
  geom_density(alpha = 0.25, linewidth = 0.5) +
  scale_fill_manual(values = c(col_target, col_reference)) +
  scale_colour_manual(values = c(col_target, col_reference)) +
  labs(x = "Posterior SD (logit scale)", y = "Density") +
  theme_paper +
  theme(legend.position = "inside", legend.position.inside = c(0.97, 0.97), legend.justification = c(1, 1),
        legend.key.size = unit(3, "mm"), legend.title = element_blank(), legend.text = element_text(size = 7))

# (b) Season effects, the reference distribution for the last season.
figs1b <- ggplot(season_effects |> mutate(season_group = season_colour(as.integer(season))),
                aes(season, Estimate, colour = season_group)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_pointrange(aes(ymin = Q2.5, ymax = Q97.5), size = 0.3, linewidth = 0.5) +
  scale_colour_manual(values = season_palette) +
  scale_x_discrete(labels = short_year) +
  labs(x = NULL, y = "Season effect (logit)") +
  theme_paper

# (c) Last-season deviations for the states in the last pair's roster.
figs1c <- ggplot(target_deviations, aes(Estimate, reorder(state, Estimate))) +
  geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_pointrange(aes(xmin = Q2.5, xmax = Q97.5), colour = col_target, size = 0.3, linewidth = 0.5) +
  labs(x = paste(target_year, "state deviation (logit)"), y = NULL) +
  theme_paper

figure_s1 <- figs1a + figs1b + figs1c + plot_layout(widths = c(1.2, 1.2, 1)) + plot_annotation(tag_levels = "a")
save_figure(figure_s1, "figureS1_model", height = 95)


# Supplementary figure S2: is detection triggered by death? (secondary) ------------------------------------------------

isolated <- read_output("results", "isolated_summary") |> filter(primary)
strata <- read_output("results", "isolated_strata") |> filter(role == "secondary")

ratios <- bind_rows(
  isolated |> transmute(label = "All events", null = "Own state CFR (primary)", ratio, lo = ratio_lo, hi = ratio_hi),
  isolated |> transmute(label = "All events", null = "National season CFR", ratio = ratio_national, lo = ratio_national_lo, hi = ratio_national_hi),
  isolated |> transmute(label = "All events", null = "LASCOPE hospitalised CFR", ratio = ratio_lascope, lo = ratio_lascope_lo, hi = ratio_lascope_hi),
  strata |> transmute(label = paste0(stringr::str_to_sentence(stratum), ": ", level), null = "Own state CFR (primary)",
                      ratio, lo = ratio_lo, hi = ratio_hi))

# Legend order matches the order of the points in the dodged row.
null_levels <- c("LASCOPE hospitalised CFR", "National season CFR", "Own state CFR (primary)")
ratios <- ratios |> mutate(null = factor(null, levels = null_levels))

figure_s2 <- ggplot(ratios, aes(ratio, label, colour = null)) +
  geom_vline(xintercept = 1, colour = "grey60", linewidth = 0.3) +
  geom_pointrange(aes(xmin = lo, xmax = hi), position = position_dodge(width = 0.6), size = 0.3, linewidth = 0.6) +
  scale_colour_manual(values = c(`Own state CFR (primary)` = col_target, `National season CFR` = col_reference,
                                 `LASCOPE hospitalised CFR` = col_other), breaks = rev(null_levels)) +
  # Always put a tick at 1, the value expected under severity-blind detection.
  scale_x_continuous(breaks = \(lims) sort(unique(c(1, scales::extended_breaks()(lims))))) +
  labs(x = "Observed / expected share of isolated detections with a death", y = NULL,
       subtitle = "Main setting: 3-week lookback, detection week plus one") +
  theme_paper +
  theme(legend.position = "top", legend.title = element_blank())

save_figure(figure_s2, "figureS2_isolated", width = 140, height = 80)

message("Figures written to figures/ (model from ", basename(model_path), ")")
