# plots/15_centrality_plots.R — Centrality Scatter Plots and Bar Charts
#
# Produces all centrality-related visualisations:
#   A. Brokerage vs influence scatter (eigenvector × betweenness, 2022)
#   B. Norway's centrality change 2019 → 2022 (line plot, faceted by measure)
#   C. Out-strength rankings dot plot — all 30 countries, both layers (2022)

# No igraph dependency — all inputs are plain data frames from centrality_all.rds.
# No figures are printed to screen.
#
# Inputs:
#   data/processed/centrality_all.rds      — produced by analyses/09_centrality.R
#
# Outputs (PDF, no screen display):
#   thesis_project/plots/output/cent_scatter_2022.pdf         — brokerage vs influence (2022)
#   thesis_project/plots/output/cent_norway_change.pdf        — Norway centrality change
#   thesis_project/plots/output/cent_ranks_2022.pdf     — out-strength rankings
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
  centrality  = "data/processed/centrality_all.rds"
)

missing <- inputs[!file.exists(inputs)]
if (length(missing) > 0) {
  stop("Missing inputs:\n",
       paste(" ", names(missing), "->", missing, collapse = "\n"),
       "\nRun analyses/09_centrality.R first.")
}

dir.create(DIRS$figures, recursive = TRUE, showWarnings = FALSE)

# ── Load inputs ───────────────────────────────────────────────────────────────
centrality_all <- readRDS(inputs["centrality"])

message("Inputs loaded: ", nrow(centrality_all), " centrality observations.")

# ── Shared constants ──────────────────────────────────────────────────────────
COL_FE     <- "#2C7BB6"   # frontend blue 
COL_BE     <- "#D7191C"   # backend red
COL_NOR    <- "red3"      # Norway highlight
COL_NOR_RANK    <- "#E69F00"  # Norway highlight for dot plots against coloured segments
COL_OTHER  <- "steelblue"
CAPTION_SRC <- "Sources: UN Comtrade; Taiwan ITA. Author's calculations."

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

scatter_cent <- function(df, layer_colour) {
  ggplot(df, aes(x = eigenvector, y = betweenness,
                 label = iso3, colour = is_norway, size = is_norway)) +
    geom_point(alpha = 0.8) +
    geom_text_repel(size = 3, max.overlaps = 50, seed = 42) +
    scale_colour_manual(
      values = c("FALSE" = layer_colour, "TRUE" = COL_NOR_RANK),
      guide  = "none"
    ) +
    scale_size_manual(
      values = c("FALSE" = 2.5, "TRUE" = 4.5),
      guide  = "none"
    ) +
    scale_x_continuous(trans = "log1p") +
    scale_y_continuous(trans = "log1p") +
    labs(
      x     = "Eigenvector centrality (influence)",
      y     = "Betweenness centrality (brokerage)"
    ) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())
}

p_fe_sc <- scatter_cent(cent_fe_22, COL_FE)
p_be_sc <- scatter_cent(cent_be_22, COL_BE)

p_scatter_2022 <- (p_fe_sc | p_be_sc) +
  plot_annotation(
    caption  = CAPTION_SRC
  )

ggsave(file.path(DIRS$figures, "cent_scatter_2022.pdf"), plot = p_scatter_2022,
       width = 13, height = 6, device = "pdf")
message("Saved: ", file.path(DIRS$figures, "cent_scatter_2022.pdf"))



# =============================================================================
# B. Norway's centrality change 2019 → 2022 (line plot)
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

ggsave(file.path(DIRS$figures, "cent_norway_change.pdf"), plot = p_norway_change,
       width = 11, height = 5, device = "pdf")
message("Saved: ", file.path(DIRS$figures, "cent_norway_change.pdf"))


# =============================================================================
# C. Top-N countries by out-strength (bar charts, 2022)
#
#   Horizontal bar chart showing the most trade-central countries.
#   Norway is highlighted; bars sorted by out-strength.
# =============================================================================

dotplot_rank <- function(cent_df, layer_colour) {
  cent_df |>
    arrange(desc(strength_out)) |>
    mutate(
      rank      = row_number(),
      iso3      = fct_reorder(iso3, strength_out),
      highlight = iso3 == FOCAL_COUNTRY
    ) |>
    ggplot(aes(x = strength_out, y = iso3,
               colour = highlight, size = highlight)) +
    geom_point() +
    geom_segment(aes(x = 0, xend = strength_out,
                     y = iso3, yend = iso3),
                 linewidth = 0.3, alpha = 0.4) +
    scale_colour_manual(
      values = c("FALSE" = layer_colour, "TRUE" = COL_NOR_RANK),
      guide  = "none"
    ) +
    scale_size_manual(
      values = c("FALSE" = 2, "TRUE" = 3.5),
      guide  = "none"
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
      x       = "Out-strength (market-share weighted)",
      y       = NULL,
      caption = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text.y        = element_text(size = 8)
    )
}

p_dot_fe22 <- dotplot_rank(cent_fe_22, COL_FE)
p_dot_be22 <- dotplot_rank(cent_be_22,  COL_BE)

p_cent_ranks <- p_dot_fe22 | p_dot_be22

p_cent_ranks <- p_cent_ranks +
  plot_annotation(
    caption = CAPTION_SRC   # ← single caption here only
  )

ggsave(file.path(DIRS$figures, "cent_ranks_2022.pdf"),
       plot = p_cent_ranks, width = 16, height = 9, device = "pdf")


message("\n15_centrality_plots.R complete — PDFs written to ", DIRS$figures)
