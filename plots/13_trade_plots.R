# plots/13_trade_plots.R — Norway Descriptive Trade Figures
#
# Inputs:
#   data/processed/edges_raw.rds
#
# Outputs (thesis_project/plots/output/):
#   fig_norway_hs_combined.pdf  — top HS exports vs imports (2022)
#   fig_norway_partners.pdf     — top bilateral partners by layer (2022)
#
# Run from project root: Rscript plots/13_trade_plots.R

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(forcats)
library(stringr)
library(patchwork)
library(scales)

source("config.R")

# -- Guards -------------------------------------------------------------------
edges_path <- file.path("data/processed", "edges_raw.rds")
if (!file.exists(edges_path))
  stop("edges_raw.rds not found. Run create_data/05_build_network_data.R first.")

dir.create(DIRS$figures, recursive = TRUE, showWarnings = FALSE)

# -- Load data ----------------------------------------------------------------
edges_all <- readRDS(edges_path)
message("Loaded edges_all: ", nrow(edges_all), " rows, ",
        n_distinct(edges_all$year), " years")

# -- Shared constants ---------------------------------------------------------
COL_FE      <- "#2166ac"
COL_BE      <- "#d6604d"
CAPTION_FIG <- "Sources: UN Comtrade; Taiwan ITA. Author's calculations."

theme_nor <- theme_minimal(base_size = 12) +
  theme(
    plot.title        = element_text(face = "bold", size = 13),
    plot.subtitle     = element_text(colour = "grey40", size = 10),
    strip.text        = element_text(face = "bold"),
    legend.position   = "bottom",
    panel.grid.minor  = element_blank()
  )

# =============================================================================
# FIGURE 1 -- Norway top HS codes: exports vs imports side-by-side (2022)
#
#   Left panel  = exports (reporter_code == NOR)
#   Right panel = imports (partner_code  == NOR)
#   Bars coloured by layer. Top 8 products per direction.
# =============================================================================

make_hs_panel <- function(direction_col, panel_title) {
  edges_all |>
    filter(.data[[direction_col]] == FOCAL_COUNTRY,
           year == 2022, !is.na(layer), source != "OECD_BTIGE") |>
    group_by(hs_code, hs_desc, layer) |>
    summarise(trade_m = sum(trade_value_usd, na.rm = TRUE) / 1e6, .groups = "drop") |>
    mutate(
      layer_label = if_else(layer == "layer1_frontend", "Frontend (L1)", "Backend (L2)"),
      hs_label    = paste0(hs_code, "\n", str_trunc(hs_desc, 35))
    ) |>
    slice_max(trade_m, n = 8, with_ties = FALSE) |>
    mutate(hs_label = fct_reorder(hs_label, trade_m)) |>
    ggplot(aes(x = trade_m, y = hs_label, fill = layer_label)) +
    geom_col(width = 0.7, alpha = 0.9) +
    geom_text(aes(label = paste0("$", round(trade_m), "M")),
              hjust = -0.05, size = 2.8) +
    scale_fill_manual(
      values = c("Frontend (L1)" = COL_FE, "Backend (L2)" = COL_BE),
      name   = "Layer"
    ) +
    scale_x_continuous(
      expand = expansion(mult = c(0, 0.28)),
      labels = label_number(suffix = "M", prefix = "$", scale = 1)
    ) +
    labs(title = panel_title, x = "USD million", y = NULL) +
    theme_nor +
    theme(axis.text.y = element_text(size = 7.5, lineheight = 1.1),
          panel.grid.major.y = element_blank())
}

p_hs_exp <- make_hs_panel("reporter_code", "Exports")
p_hs_imp <- make_hs_panel("partner_code",  "Imports")

fig_hs_combined <- (p_hs_exp | p_hs_imp) +
  plot_layout(guides = "collect") &
  plot_annotation(
    title   = "Norway's top semiconductor products by HS code (2022)",
    caption = CAPTION_FIG,
    theme   = theme(
      plot.title   = element_text(size = 12, face = "bold"),
      plot.caption = element_text(size = 8,  colour = "grey50")
    )
  ) &
  theme(legend.position = "bottom")

ggsave(file.path(DIRS$figures, "fig_norway_hs_combined.pdf"),
       fig_hs_combined, width = 12, height = 7)
message("Saved: ", file.path(DIRS$figures, "fig_norway_hs_combined.pdf"))

# =============================================================================
# FIGURE 2 -- Norway's top trade partners by layer (2022)
#
#   Left panel: export destinations | Right panel: import sources
#   Bars stacked by layer (Frontend L1 = blue, Backend L2 = coral)
# =============================================================================

nor_exp_partners <- edges_all |>
  filter(year == 2022, reporter_code == FOCAL_COUNTRY,
         !is.na(layer), source != "OECD_BTIGE") |>
  mutate(layer_label = if_else(layer == "layer1_frontend", "Frontend (L1)", "Backend (L2)")) |>
  group_by(partner_code, layer_label) |>
  summarise(trade_m = sum(trade_value_usd, na.rm = TRUE) / 1e6, .groups = "drop") |>
  group_by(partner_code) |>
  mutate(total = sum(trade_m)) |>
  ungroup() |>
  slice_max(total, n = 20) |>
  mutate(partner_code = fct_reorder(partner_code, total))

nor_imp_partners <- edges_all |>
  filter(year == 2022, partner_code == FOCAL_COUNTRY,
         !is.na(layer), source != "OECD_BTIGE") |>
  mutate(layer_label = if_else(layer == "layer1_frontend", "Frontend (L1)", "Backend (L2)")) |>
  group_by(reporter_code, layer_label) |>
  summarise(trade_m = sum(trade_value_usd, na.rm = TRUE) / 1e6, .groups = "drop") |>
  group_by(reporter_code) |>
  mutate(total = sum(trade_m)) |>
  ungroup() |>
  slice_max(total, n = 20) |>
  mutate(reporter_code = fct_reorder(reporter_code, total))

p_exp <- ggplot(nor_exp_partners,
                aes(x = trade_m, y = partner_code, fill = layer_label)) +
  geom_col(width = 0.65, alpha = 0.9) +
  geom_text(
    data = ~ filter(.x, trade_m >= 1),
    aes(label = paste0("$", round(trade_m), "M")),
    position = position_stack(vjust = 0.5),
    size = 2.6, colour = "white", fontface = "bold"
  ) +
  scale_fill_manual(values = c("Frontend (L1)" = COL_FE, "Backend (L2)" = COL_BE),
                    name = "Layer") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Export destinations (2022)", x = "USD million", y = NULL) +
  theme_nor

p_imp <- ggplot(nor_imp_partners,
                aes(x = trade_m, y = reporter_code, fill = layer_label)) +
  geom_col(width = 0.65, alpha = 0.9) +
  geom_text(
    data = ~ filter(.x, trade_m >= 1),
    aes(label = paste0("$", round(trade_m), "M")),
    position = position_stack(vjust = 0.5),
    size = 2.6, colour = "white", fontface = "bold"
  ) +
  scale_fill_manual(values = c("Frontend (L1)" = COL_FE, "Backend (L2)" = COL_BE),
                    name = "Layer") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Import sources (2022)", x = "USD million", y = NULL) +
  theme_nor

fig_partners <- (p_exp | p_imp) +
  plot_layout(guides = "collect") &
  plot_annotation(
    title   = "Norway's top semiconductor trade partners by layer (2022)",
    caption = CAPTION_FIG,
    theme   = theme(
      plot.title    = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 9,  colour = "grey40"),
      plot.caption  = element_text(size = 8,  colour = "grey50")
    )
  ) &
  theme(legend.position = "bottom")

ggsave(file.path(DIRS$figures, "fig_norway_partners.pdf"),
       fig_partners, width = 14, height = 7)
message("Saved: ", file.path(DIRS$figures, "fig_norway_partners.pdf"))

message("\n13_trade_plots.R complete.")
