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

# Two series of the same CFR (see Addendum 3 of the analysis plan): the running sum
# of the weekly reports, used by the pre-specified analyses, and the cumulative
# figures printed in each report, which also hold deaths added later.
published_running <- read_output("results", "published_series_running")
published_annual <- read_output("results", "published_series_annual")

min_cases <- 50   # the running CFR swings widely while there are few cases; start each line here
context_labels <- c(2018, 2020)   # the highest and lowest context years, labelled directly

# (a) Cumulative CFR through each year: the other years as grey context, the
# reference and target years in colour, weekly (solid) and published (dotted).
cumulative <- national_week |>
  filter(cum_known, cum_confirmed >= min_cases) |>
  transmute(year, epi_week, cfr = cfr_cumulative, season = season_colour(year), series = "Summed weekly reports")

published_lines <- published_running |>
  filter(year %in% c(reference_year, target_year), cum_confirmed >= min_cases) |>
  transmute(year, epi_week, cfr = cfr_published, season = season_colour(year), series = "Published cumulative")

highlight <- bind_rows(filter(cumulative, season != "other"), published_lines)

end_labels <- highlight |>
  filter(series == "Summed weekly reports") |>
  slice_max(epi_week, by = year)

context_ends <- cumulative |>
  filter(year %in% context_labels) |>
  slice_max(epi_week, by = year)

phase_marks <- phase_points |>
  filter(year %in% c(reference_year, target_year), grepl("^phase", comparison)) |>
  mutate(season = season_colour(year), percentile = sub("phase ", "", comparison))

fig1a <- ggplot(mapping = aes(epi_week, cfr, group = interaction(year, series))) +
  geom_vline(xintercept = 23, colour = "grey75", linewidth = 0.3) +
  geom_line(data = filter(cumulative, season == "other"), colour = col_other, linewidth = 0.4) +
  geom_line(data = highlight, aes(colour = season, linetype = series), linewidth = 0.8) +
  geom_point(data = phase_marks, aes(epi_week, cfr, colour = season, shape = percentile),
             inherit.aes = FALSE, size = 2.4, fill = "white", stroke = 0.9) +
  geom_text(data = end_labels, aes(label = year, colour = season), hjust = -0.2, size = 3, show.legend = FALSE) +
  geom_text(data = context_ends, aes(label = year), colour = "grey45", hjust = -0.2, size = 2.6) +
  annotate("text", x = 23, y = Inf, label = "week 23", vjust = 1.5, hjust = -0.1, size = 2.8, colour = "grey40") +
  scale_colour_manual(values = season_palette, guide = "none") +
  scale_linetype_manual(values = c("Summed weekly reports" = "solid", "Published cumulative" = "11"), name = NULL,
                        guide = guide_legend(keywidth = unit(1.2, "cm"))) +
  scale_shape_manual(values = c("25%" = 21, "50%" = 24), name = "Phase point") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.12))) +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(clip = "off") +
  labs(x = "Epidemiological week", y = "Cumulative CFR") +
  theme_paper +
  theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin(0, 0, 0, 0))

# (b) Annual CFR, 2017-2026, in both series. Weekly: from weeks where both deaths
# and confirmed cases are known (cfr_matched; see 03_descriptives.R), hollow where
# the year has missing weeks or is incomplete. Published: each year's final printed
# cumulative figure (2017 counts from December 2016).
annual_points <- bind_rows(
  national_season |>
    transmute(year, cfr = cfr_matched, marker = if_else(partial_year, "Summed weekly, incomplete year", "Summed weekly")),
  published_annual |>
    transmute(year, cfr = cfr_published, marker = "Published year-end")) |>
  mutate(season = season_colour(year))

fig1b <- ggplot(annual_points, aes(factor(year), cfr, colour = season, shape = marker)) +
  geom_point(size = 2.4, stroke = 0.9, fill = "white") +
  scale_shape_manual(values = c("Summed weekly" = 16, "Summed weekly, incomplete year" = 21, "Published year-end" = 4),
                     name = NULL) +
  scale_colour_manual(values = season_palette, guide = "none") +
  scale_x_discrete(labels = short_year) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Annual CFR") +
  theme_paper +
  theme(legend.position = "bottom", legend.margin = margin(0, 0, 0, 0))

figure1 <- fig1a / fig1b + plot_layout(heights = c(1.5, 1)) + plot_annotation(tag_levels = "a")
save_figure(figure1, "figure1_cfr", width = 160, height = 210)


# Figure 2: decomposition ----------------------------------------------------------------

decomposition <- read_output("results", "decomposition_ci")
every_state <- read_output("results", "contributions_every_state")

last_transition <- paste0(reference_year, "-", target_year)

# Points are hollow where the 95% interval includes zero, solid where it excludes it.
interval_shape <- function(lo, hi) if_else(lo > 0 | hi < 0, "95% interval excludes zero", "95% interval includes zero")
interval_shapes <- c("95% interval excludes zero" = 16, "95% interval includes zero" = 21)

# (a) Rate and composition for every transition, with bootstrap intervals.
components <- decomposition |>
  filter(component %in% c("rate", "composition")) |>
  mutate(component = factor(component, levels = c("rate", "composition"),
                            labels = c("Within-state (rate)", "Between-state (composition)")),
         highlight = transition == last_transition,
         interval = interval_shape(lo, hi))

fig2a <- ggplot(components, aes(estimate, transition, colour = component)) +
  geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_linerange(aes(xmin = lo, xmax = hi), position = position_dodge(width = 0.6), linewidth = 0.5) +
  geom_point(aes(shape = interval), position = position_dodge(width = 0.6), size = 1.8, fill = "white", stroke = 0.8) +
  scale_shape_manual(values = interval_shapes) +
  scale_colour_manual(values = c(col_target, col_reference)) +
  scale_x_continuous(labels = scales::label_number(scale = 100, suffix = " pp")) +
  labs(x = "Contribution to change in national CFR", y = NULL, subtitle = "Each pair of years") +
  theme_paper

# (b) Per-state contributions to the last transition, every state separate. A
# secondary output, imprecise because each state has few cases (the caption says
# so, as the analysis plan requires). The ten largest by absolute total contribution.
last_states <- every_state |>
  filter(transition == last_transition) |>
  mutate(total = sum(estimate), .by = state) |>
  filter(state %in% (distinct(pick(state, total)) |> slice_max(abs(total), n = 10) |> pull(state))) |>
  mutate(component = factor(component, levels = c("rate", "composition_centred"),
                            labels = c("Within-state (rate)", "Between-state (composition)")),
         state = reorder(state, total),
         interval = interval_shape(lo, hi))

fig2b <- ggplot(last_states, aes(estimate, state, colour = component)) +
  geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_linerange(aes(xmin = lo, xmax = hi), position = position_dodge(width = 0.6), linewidth = 0.5) +
  geom_point(aes(shape = interval), position = position_dodge(width = 0.6), size = 1.8, fill = "white", stroke = 0.8) +
  scale_shape_manual(values = interval_shapes) +
  scale_colour_manual(values = c(col_target, col_reference)) +
  scale_x_continuous(labels = scales::label_number(scale = 100, suffix = " pp")) +
  labs(x = paste("Contribution to the", last_transition, "change"), y = NULL,
       subtitle = paste("Each state,", last_transition)) +
  theme_paper

# One shared legend for both panels, below them.
figure2 <- fig2a + fig2b + plot_layout(guides = "collect") + plot_annotation(tag_levels = "a") &
  theme(legend.position = "bottom", legend.title = element_blank(), legend.box = "vertical")
save_figure(figure2, "figure2_decomposition", height = 120)


# Figure 3: how large an ascertainment change would need to be ---------------------------------

tipping <- read_output("results", "tipping_point")
positivity_national <- read_output("results", "positivity_national")
positivity_state <- read_output("results", "positivity_state")
roster_states <- unique(read_output("results", "decomposition_rosters")$state)

# (a) Missed survivors needed at week 23, as a number of people. The main estimand
# is the share of suspected cases not confirmed, so each point is labelled with both.
# Short labels here; the full description of each comparison is in the caption.
tipping_plot <- tipping |>
  mutate(label = case_when(role == "primary" ~ paste0("Published, ", target_year, " vs ", reference_year),
                           grepl("pooled", comparison) ~ paste0("Reconstructed, ", target_year, " vs pooled"),
                           .default = paste0("Reconstructed, ", target_year, " vs ", reference_year)),
         label = reorder(label, role == "primary"),
         value_label = paste0(round(x), " (", scales::percent(share_of_not_confirmed, accuracy = 0.1), ")"))

fig3a <- ggplot(tipping_plot, aes(x, label, colour = role)) +
  geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_pointrange(aes(xmin = x_lo, xmax = x_hi), size = 0.3, linewidth = 0.6) +
  geom_label(aes(label = value_label), vjust = -0.6, size = 2.8, fill = "white", label.size = 0, show.legend = FALSE) +
  scale_colour_manual(values = c(primary = col_target, secondary = "grey45")) +
  labs(x = "Missed survivors needed at week 23", y = NULL) +
  theme_paper

# (b) Suspected-case positivity (a proxy, not test positivity) by season:
# nationally, and for each roster state from 2020.
state_lines <- positivity_state |>
  filter(state %in% roster_states, year >= 2020, !is.na(positivity))

national_line <- positivity_national |> filter(!is.na(positivity), year %in% inference_years)

# End labels, national included, spread apart where they would overlap: sort by
# height and push each label up until it is at least min_gap above the one below.
min_gap <- 0.018
end_labels3 <- bind_rows(state_lines |> slice_max(year, by = state) |> transmute(year, positivity, label = state, national = FALSE),
                         national_line |> slice_max(year) |> transmute(year, positivity, label = "National", national = TRUE)) |>
  arrange(positivity) |>
  mutate(label_y = positivity)
for (i in seq_len(nrow(end_labels3))[-1]) {
  end_labels3$label_y[i] <- max(end_labels3$label_y[i], end_labels3$label_y[i - 1] + min_gap)
}

fig3b <- ggplot() +
  geom_line(data = state_lines, aes(year, positivity, group = state), colour = col_other, linewidth = 0.4) +
  geom_text(data = filter(end_labels3, !national), aes(year, label_y, label = label),
            hjust = -0.15, size = 2.6, colour = "grey45") +
  geom_text(data = filter(end_labels3, national), aes(year, label_y, label = label),
            hjust = -0.15, size = 2.8, colour = col_ink, fontface = "bold") +
  geom_line(data = national_line, aes(year, positivity), colour = col_ink, linewidth = 0.9) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = inference_years, labels = short_year, expand = expansion(mult = c(0.02, 0.25))) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = "Suspected-case positivity (proxy)") +
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
sds <- tibble(`Year (common)` = draws$`sd_season__Intercept`,
              `State by year (localised)` = draws$`sd_state:season__Intercept`) |>
  pivot_longer(everything(), names_to = "term", values_to = "sd") |>
  mutate(term = factor(term, levels = c("Year (common)", "State by year (localised)")))

figs1a <- ggplot(sds, aes(sd, fill = term, colour = term)) +
  geom_density(alpha = 0.25, linewidth = 0.5) +
  scale_fill_manual(values = c(col_target, col_reference)) +
  scale_colour_manual(values = c(col_target, col_reference)) +
  labs(x = "Posterior SD (logit scale)", y = "Density") +
  theme_paper +
  theme(legend.position = "bottom", legend.direction = "vertical",
        legend.key.size = unit(3, "mm"), legend.title = element_blank(), legend.text = element_text(size = 7))

# (b) Season effects, the reference distribution for the last season.
figs1b <- ggplot(season_effects |> mutate(season_group = season_colour(as.integer(season))),
                aes(season, Estimate, colour = season_group)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_pointrange(aes(ymin = Q2.5, ymax = Q97.5), size = 0.3, linewidth = 0.5) +
  scale_colour_manual(values = season_palette) +
  scale_x_discrete(labels = short_year) +
  labs(x = NULL, y = "Year effect (logit)") +
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
  isolated |> transmute(label = "All events", null = "National CFR for the year", ratio = ratio_national, lo = ratio_national_lo, hi = ratio_national_hi),
  isolated |> transmute(label = "All events", null = "LASCOPE CFR, admitted patients", ratio = ratio_lascope, lo = ratio_lascope_lo, hi = ratio_lascope_hi),
  strata |> transmute(label = paste0(stringr::str_to_sentence(stratum), ": ", level), null = "Own state CFR (primary)",
                      ratio, lo = ratio_lo, hi = ratio_hi))

# Legend order matches the order of the points in the dodged row.
null_levels <- c("LASCOPE CFR, admitted patients", "National CFR for the year", "Own state CFR (primary)")
ratios <- ratios |> mutate(null = factor(null, levels = null_levels))

figure_s2 <- ggplot(ratios, aes(ratio, label, colour = null)) +
  geom_vline(xintercept = 1, colour = "grey60", linewidth = 0.3) +
  geom_pointrange(aes(xmin = lo, xmax = hi), position = position_dodge(width = 0.6), size = 0.3, linewidth = 0.6) +
  scale_colour_manual(values = c(`Own state CFR (primary)` = col_target, `National CFR for the year` = col_reference,
                                 `LASCOPE CFR, admitted patients` = col_other), breaks = rev(null_levels),
                      guide = guide_legend(nrow = 2)) +
  # Always put a tick at 1, the value expected under severity-blind detection.
  scale_x_continuous(breaks = \(lims) sort(unique(c(1, scales::extended_breaks()(lims))))) +
  labs(x = "Observed / expected share of isolated detections with a death", y = NULL,
       subtitle = "Main setting: 3-week lookback, detection week plus one") +
  theme_paper +
  theme(legend.position = "top", legend.title = element_blank())

save_figure(figure_s2, "figureS2_isolated", width = 140, height = 80)

message("Figures written to figures/ (model from ", basename(model_path), ")")
