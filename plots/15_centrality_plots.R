# plots/15_centrality_plots.R — Centrality Scatter Plots and Bar Charts
#
# Produces all centrality-related visualisations:
#   A. Brokerage vs influence scatter (eigenvector × betweenness, 2022)
#   B. R&D intensity vs eigenvector centrality (faceted by layer, 2022)
#   C. Norway's centrality change 2019 → 2022 (line plot, faceted by measure)
#   D. Multiplex cross-layer centrality scatter (FE vs BE out-strength & betweenness)
#   E. Top-N countries bar chart (out-strength, BE and FE 2022)
#
# No igraph dependency — all inputs are plain data frames from centrality_all.rds
# and node_geopolitical.rds. No figures are printed to screen.
#
# Inputs (all via readRDS):
#   data/processed/centrality_all.rds      — produced by analyses/09_centrality.R
#   data/processed/node_geopolitical.rds   — node attributes incl. patents_log
#
# Outputs (PDF, no screen display):
#   plots/output/cent_scatter_2022.pdf         — brokerage vs influence (2022)
#   plots/output/cent_rd_eigen.pdf             — R&D vs eigenvector
#   plots/output/cent_norway_change.pdf        — Norway centrality change
#   plots/output/cent_multiplex_scatter.pdf    — cross-layer scatter
#   plots/output/cent_topN_be22.pdf            — top-N bar, backend 2022
#   plots/output/cent_topN_fe22.pdf            — top-N bar, frontend 2022
#
# Run from project root: Rscript plots/15_centrality_plots.R

library(ggplot2)
library(ggrepel)
library(patchwork)
library(dplyr)
library(tidyr)
library(forcats)

source("config.R")

# ── Guard: check inputs ───────────────────────────────────────────────────────
inputs <- c(
  centrality = "data/processed/centrality_all.rds",
  node_geo   = "data/processed/node_geopolitical.rds"
)

missing <- inputs[!file.exists(inputs)]
if (length(missing) > 0) {
  stop("Missing inputs:\n",
       paste(" ", names(missing), "->", missing, collapse = "\n"),
       "\nRun analyses/09_centrality.R and create_data/06_geopolitical_attrs.R first.")
}

dir.create("plots/output", recursive = TRUE, showWarnings = FALSE)

# ── Load inputs ───────────────────────────────────────────────────────────────
centrality_all <- readRDS(inputs["centrality"])
node_geo       <- readRDS(inputs["node_geo"])

message("Inputs loaded: ", nrow(centrality_all), " centrality observations, ",
        nrow(node_geo), " nodes.")

# ── Shared constants ──────────────────────────────────────────────────────────
COL_FE     <- "#2C7BB6"   # frontend blue (matches Rmd)
COL_BE     <- "#D7191C"   # backend red
COL_NOR    <- "red3"      # Norway highlight
COL_OTHER  <- "steelblue"
CAPTION_SRC <- "Sources: UN Comtrade; OECD BTIGE; OECD patents. Author's calculations."

# Convenience subsets
cent_fe_22 <- centrality_all |> filter(layer == "Front-end", year == 2022)
cent_be_22 <- centrality_all |> filter(layer == "Back-end",  year == 2022)

# =============================================================================
# A. Brokerage vs influence scatter — eigenvector × betweenness (2022)
#
#   One panel per layer. Norway highlighted in red3.
#   High betweenness = structural broker; high eigenvector = well-connected
#   to other well-connected countries (influence).
# =============================================================================

scatter_cent <- function(df, layer_colour, title_str) {
  ggplot(df, aes(x = eigenvector, y = betweenness,
                 label = iso3, colour = is_norway, size = is_norway)) +
    geom_point(alpha = 0.8) +
    geom_text_repel(size = 3, max.overlaps = 20, seed = 42) +
    scale_colour_manual(
      values = c("FALSE" = layer_colour, "TRUE" = COL_NOR),
      guide  = "none"
    ) +
    scale_size_manual(
      values = c("FALSE" = 2.5, "TRUE" = 4.5),
      guide  = "none"
    ) +
    labs(
      title = title_str,
      x     = "Eigenvector centrality (influence)",
      y     = "Betweenness centrality (brokerage)"
    ) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())
}

p_fe_sc <- scatter_cent(cent_fe_22, COL_FE, "Front-end (2022)")
p_be_sc <- scatter_cent(cent_be_22, COL_BE, "Back-end (2022)")

p_scatter_2022 <- (p_fe_sc | p_be_sc) +
  plot_annotation(
    title    = "Brokerage vs Influence — Global Semiconductor Trade Networks (2022)",
    subtitle = paste0("Norway (", FOCAL_COUNTRY, ") highlighted in red. ",
                      "High betweenness = structural broker."),
    caption  = CAPTION_SRC
  )

ggsave("plots/output/cent_scatter_2022.pdf", plot = p_scatter_2022,
       width = 13, height = 6, device = cairo_pdf)
message("Saved: plots/output/cent_scatter_2022.pdf")

# =============================================================================
# B. R&D intensity vs eigenvector centrality (2022, faceted by layer)
#
#   Tests whether R&D-intensive countries occupy more central network
#   positions — a key structural hypothesis in the thesis.
#   patents_log joined from node_geo; OLS trend line added.
# =============================================================================

cent_2022_rd <- centrality_all |>
  filter(year == 2022) |>
  left_join(node_geo |> select(iso3, patents_log), by = "iso3")

p_rd_eigen <- ggplot(cent_2022_rd,
                      aes(x = patents_log, y = eigenvector,
                          label = iso3, colour = is_norway, size = is_norway)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, colour = "grey40",
              linewidth = 0.7, linetype = "dashed",
              # fit OLS across all countries (not separately by Norway)
              inherit.aes = FALSE,
              aes(x = patents_log, y = eigenvector)) +
  geom_text_repel(size = 3, max.overlaps = 20, seed = 42) +
  facet_wrap(~layer) +
  scale_colour_manual(
    values = c("FALSE" = COL_OTHER, "TRUE" = COL_NOR),
    guide  = "none"
  ) +
  scale_size_manual(
    values = c("FALSE" = 2.5, "TRUE" = 4.5),
    guide  = "none"
  ) +
  labs(
    title    = "R&D Intensity vs Network Centrality (2022)",
    subtitle = paste0("Eigenvector centrality as measure of influence. ",
                      FOCAL_COUNTRY, " in red. OLS trend with 95% CI."),
    x        = "R&D intensity (log patent count)",
    y        = "Eigenvector centrality",
    caption  = CAPTION_SRC
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("plots/output/cent_rd_eigen.pdf", plot = p_rd_eigen,
       width = 11, height = 5, device = cairo_pdf)
message("Saved: plots/output/cent_rd_eigen.pdf")

# =============================================================================
# C. Norway's centrality change 2019 → 2022 (line plot)
#
#   One facet per measure. Captures the COVID-period disruption (2019→2022)
#   and whether Norway's position strengthened or weakened in each layer.
# =============================================================================

norway_change <- centrality_all |>
  filter(iso3 == FOCAL_COUNTRY) |>
  select(layer, year, strength_out, betweenness, eigenvector) |>
  pivot_longer(
    cols      = c(strength_out, betweenness, eigenvector),
    names_to  = "measure",
    values_to = "value"
  ) |>
  mutate(
    measure = dplyr::recode(measure,
      strength_out = "Out-strength",
      betweenness  = "Betweenness",
      eigenvector  = "Eigenvector"
    ),
    measure = factor(measure, levels = c("Out-strength", "Betweenness", "Eigenvector"))
  )

p_norway_change <- ggplot(norway_change,
                           aes(x = factor(year), y = value,
                               colour = layer, group = layer)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3.5, shape = 16) +
  facet_wrap(~measure, scales = "free_y", ncol = 3) +
  scale_colour_manual(
    values = c("Front-end" = COL_FE, "Back-end" = COL_BE),
    name   = "Layer"
  ) +
  labs(
    title    = paste0("Norway's Centrality: 2019 → 2022"),
    subtitle = "Pre-COVID baseline (2019) vs post-disruption (2022)",
    x        = "Year",
    y        = "Centrality score",
    caption  = CAPTION_SRC
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text       = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom"
  )

ggsave("plots/output/cent_norway_change.pdf", plot = p_norway_change,
       width = 11, height = 5, device = cairo_pdf)
message("Saved: plots/output/cent_norway_change.pdf")

# =============================================================================
# D. Multiplex cross-layer centrality scatter (FE vs BE, 2022)
#
#   Two panels: out-strength and betweenness.
#   Tests whether positions are consistent across the two trade layers.
#   Pearson r is computed and shown in the panel title.
# =============================================================================

multiplex_22 <- centrality_all |>
  filter(year == 2022) |>
  select(iso3, layer, is_norway, strength_out, betweenness, eigenvector) |>
  pivot_wider(
    names_from  = layer,
    values_from = c(strength_out, betweenness, eigenvector)
  )

# Compute correlations for titles
r_str  <- cor(multiplex_22$`strength_out_Front-end`,
               multiplex_22$`strength_out_Back-end`, use = "complete.obs")
r_btwn <- cor(multiplex_22$`betweenness_Front-end`,
               multiplex_22$`betweenness_Back-end`,  use = "complete.obs")

multiplex_scatter <- function(df, x_col, y_col, r_val, measure_label, pt_col) {
  ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]],
                 label = iso3, colour = is_norway, size = is_norway)) +
    geom_point(alpha = 0.8) +
    geom_text_repel(size = 3, max.overlaps = 25, seed = 42) +
    geom_smooth(method = "lm", se = TRUE, colour = "grey40",
                linewidth = 0.7, linetype = "dashed",
                inherit.aes = FALSE,
                aes(x = .data[[x_col]], y = .data[[y_col]])) +
    scale_colour_manual(
      values = c("FALSE" = pt_col, "TRUE" = COL_NOR),
      guide  = "none"
    ) +
    scale_size_manual(values = c("FALSE" = 2.5, "TRUE" = 4.5), guide = "none") +
    labs(
      title = sprintf("%s  (r = %.3f)", measure_label, r_val),
      x     = "Front-end (Layer 1)",
      y     = "Back-end (Layer 2)"
    ) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())
}

p_mp_str  <- multiplex_scatter(multiplex_22,
                                "strength_out_Front-end", "strength_out_Back-end",
                                r_str,  "Out-strength", COL_OTHER)
p_mp_btwn <- multiplex_scatter(multiplex_22,
                                "betweenness_Front-end",  "betweenness_Back-end",
                                r_btwn, "Betweenness",   COL_BE)

p_multiplex <- (p_mp_str | p_mp_btwn) +
  plot_annotation(
    title    = "Multiplex Centrality: Frontend vs Backend Positions (2022)",
    subtitle = paste0("Norway (", FOCAL_COUNTRY,
                      ") highlighted in red. OLS trend with 95% CI."),
    caption  = CAPTION_SRC
  )

ggsave("plots/output/cent_multiplex_scatter.pdf", plot = p_multiplex,
       width = 13, height = 6, device = cairo_pdf)
message("Saved: plots/output/cent_multiplex_scatter.pdf")

# =============================================================================
# E. Top-N countries by out-strength (bar charts, 2022)
#
#   Horizontal bar chart showing the most trade-central countries.
#   Norway is highlighted; bars sorted by out-strength.
# =============================================================================

topN_bar <- function(cent_df, layer_label, layer_colour, n = 15) {
  cent_df |>
    slice_max(strength_out, n = n) |>
    mutate(
      iso3      = fct_reorder(iso3, strength_out),
      highlight = iso3 == FOCAL_COUNTRY
    ) |>
    ggplot(aes(x = strength_out, y = iso3, fill = highlight)) +
    geom_col(width = 0.7, alpha = 0.9) +
    geom_text(aes(label = round(strength_out, 3)),
              hjust = -0.1, size = 3, colour = "grey30") +
    scale_fill_manual(
      values = c("TRUE" = COL_NOR, "FALSE" = layer_colour),
      guide  = "none"
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
      title   = paste0("Top ", n, " countries by out-strength — ",
                       layer_label, " (2022)"),
      subtitle = paste0(FOCAL_COUNTRY, " highlighted in red."),
      x       = "Out-strength (market-share weighted)",
      y       = NULL,
      caption = CAPTION_SRC
    ) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank())
}

p_topN_be22 <- topN_bar(cent_be_22, "Back-end",  COL_BE)
p_topN_fe22 <- topN_bar(cent_fe_22, "Front-end", COL_FE)

ggsave("plots/output/cent_topN_be22.pdf", plot = p_topN_be22,
       width = 8, height = 7, device = cairo_pdf)
message("Saved: plots/output/cent_topN_be22.pdf")

ggsave("plots/output/cent_topN_fe22.pdf", plot = p_topN_fe22,
       width = 8, height = 7, device = cairo_pdf)
message("Saved: plots/output/cent_topN_fe22.pdf")

message("\n15_centrality_plots.R complete — 6 PDFs written to plots/output/")
