# 13_trade_plots.R — Norway Descriptive Semiconductor Trade Plots
#
# Four descriptive plots of Norway's semiconductor trade position
# for inclusion in the thesis before the SNA section.
#
# Inputs (loaded from RDS — run the create_data/ pipeline first):
#   data/processed/edges_raw.rds   — pre-aggregation combined edge table
#   data/processed/nodes.rds       — node attribute table with RCA columns
#
# Outputs saved to plots/output/:
#   fig01_norway_trade_balance.pdf
#   fig02_norway_fe_export_partners.pdf
#   fig03_norway_be_import_sources.pdf
#   fig04_nordic_rca_comparison.pdf
#   fig_combined_2x2.pdf           — patchwork 2×2 layout for appendix
#
# Run from project root: Rscript plots/13_trade_plots.R

library(tidyverse)
library(ggrepel)
library(patchwork)

# ── Load inputs from RDS ──────────────────────────────────────────────────────
edges_raw_path <- file.path("data/processed", "edges_raw.rds")
nodes_path     <- file.path("data/processed", "nodes.rds")

if (!file.exists(edges_raw_path)) {
  stop("edges_raw.rds not found.\nRun create_data/05_build_network_data.R first.")
}
if (!file.exists(nodes_path)) {
  stop("nodes.rds not found.\nRun create_data/05_build_network_data.R first.")
}

edges_raw  <- readRDS(edges_raw_path)
node_attrs <- readRDS(nodes_path)

message("Loaded edges_raw: ", nrow(edges_raw), " rows")
message("Loaded node_attrs: ", nrow(node_attrs), " countries")

# ── Output directory ──────────────────────────────────────────────────────────
dir.create("plots/output", recursive = TRUE, showWarnings = FALSE)

# ── Shared theme ──────────────────────────────────────────────────────────────
theme_nor <- theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 10),
    axis.text     = element_text(size = 9),
    strip.text    = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# Colour palette — consistent across all plots
col_fe   <- "#2166ac"   # frontend blue
col_be   <- "#d6604d"   # backend red
col_nor  <- "#e07b39"   # Norway orange highlight
col_grey <- "#aaaaaa"

# ─────────────────────────────────────────────────────────────────────────────
# PLOT 1 — Norway trade balance by layer (2022)
# ─────────────────────────────────────────────────────────────────────────────

nor_balance <- edges_raw |>
  filter(year == 2022) |>
  mutate(
    direction = case_when(
      reporter_code == "NOR" ~ "Export",
      partner_code  == "NOR" ~ "Import",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(direction)) |>
  group_by(layer, direction) |>
  summarise(
    trade_bn = sum(trade_value_usd, na.rm = TRUE) / 1e9,
    .groups = "drop"
  ) |>
  mutate(
    layer_label = if_else(layer == "layer1_frontend",
                          "Frontend (Layer 1)", "Backend (Layer 2)"),
    trade_signed = if_else(direction == "Import", -trade_bn, trade_bn)
  )

p1 <- nor_balance |>
  ggplot(aes(x = layer_label, y = trade_signed, fill = direction)) +
  geom_col(width = 0.55, alpha = 0.9) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey30") +
  scale_fill_manual(
    values = c("Export" = col_fe, "Import" = col_be),
    name = NULL
  ) +
  scale_y_continuous(
    labels = function(x) paste0(abs(x), "B"),
    breaks = scales::pretty_breaks(6)
  ) +
  annotate("text", x = 0.55, y = max(nor_balance$trade_bn) * 0.85,
           label = "Exports →", size = 3, color = col_fe, hjust = 0) +
  annotate("text", x = 0.55, y = -max(abs(nor_balance$trade_signed)) * 0.85,
           label = "← Imports", size = 3, color = col_be, hjust = 0) +
  labs(
    title    = "Figure 1 — Norway semiconductor trade balance by layer (2022)",
    subtitle = "USD billions | Exports above zero, imports below",
    x = NULL, y = "Trade value (USD billion)"
  ) +
  theme_nor +
  theme(legend.position = "none")

ggsave("plots/output/fig01_norway_trade_balance.pdf",
       plot = p1, width = 7, height = 5)
message("Saved: plots/output/fig01_norway_trade_balance.pdf")

# ─────────────────────────────────────────────────────────────────────────────
# PLOT 2 — Norway top export partners, frontend layer (2022)
# ─────────────────────────────────────────────────────────────────────────────

nor_fe_exports <- edges_raw |>
  filter(
    year == 2022,
    layer == "layer1_frontend",
    reporter_code == "NOR"
  ) |>
  group_by(partner_code) |>
  summarise(
    trade_m = sum(trade_value_usd, na.rm = TRUE) / 1e6,
    .groups = "drop"
  ) |>
  arrange(desc(trade_m)) |>
  slice_head(n = 12) |>
  mutate(
    partner_code = fct_reorder(partner_code, trade_m),
    highlight    = partner_code %in% c("TWN", "KOR", "JPN", "USA", "CHN",
                                       "DEU", "NLD")
  )

p2 <- nor_fe_exports |>
  ggplot(aes(x = trade_m, y = partner_code, fill = highlight)) +
  geom_col(width = 0.65, alpha = 0.9) +
  geom_text(aes(label = paste0("$", round(trade_m), "M")),
            hjust = -0.1, size = 3, color = "grey30") +
  scale_fill_manual(
    values = c("TRUE" = col_fe, "FALSE" = col_grey),
    guide  = "none"
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.25)),
    labels = scales::dollar_format(suffix = "M", prefix = "$")
  ) +
  labs(
    title    = "Figure 2 — Norway's top frontend export destinations (2022)",
    subtitle = "Layer 1: silicon wafers, elemental silicon, fab equipment\nKey semiconductor producers highlighted in blue",
    x = "Export value (USD million)",
    y = NULL
  ) +
  theme_nor

ggsave("plots/output/fig02_norway_fe_export_partners.pdf",
       plot = p2, width = 7, height = 5)
message("Saved: plots/output/fig02_norway_fe_export_partners.pdf")

# ─────────────────────────────────────────────────────────────────────────────
# PLOT 3 — Norway top import partners, backend layer (2022)
# ─────────────────────────────────────────────────────────────────────────────

nor_be_imports <- edges_raw |>
  filter(
    year == 2022,
    layer == "layer2_backend",
    partner_code == "NOR"
  ) |>
  group_by(reporter_code) |>
  summarise(
    trade_m = sum(trade_value_usd, na.rm = TRUE) / 1e6,
    .groups = "drop"
  ) |>
  arrange(desc(trade_m)) |>
  slice_head(n = 12) |>
  mutate(
    reporter_code = fct_reorder(reporter_code, trade_m),
    highlight     = reporter_code %in% c("TWN", "KOR", "CHN", "USA",
                                         "MYS", "SGP", "VNM")
  )

p3 <- nor_be_imports |>
  ggplot(aes(x = trade_m, y = reporter_code, fill = highlight)) +
  geom_col(width = 0.65, alpha = 0.9) +
  geom_text(aes(label = paste0("$", round(trade_m), "M")),
            hjust = -0.1, size = 3, color = "grey30") +
  scale_fill_manual(
    values = c("TRUE" = col_be, "FALSE" = col_grey),
    guide  = "none"
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.25)),
    labels = scales::dollar_format(suffix = "M", prefix = "$")
  ) +
  labs(
    title    = "Figure 3 — Norway's top backend import sources (2022)",
    subtitle = "Layer 2: packaged ICs, memory, logic chips\nAsian assembly economies highlighted in red",
    x = "Import value (USD million)",
    y = NULL
  ) +
  theme_nor

ggsave("plots/output/fig03_norway_be_import_sources.pdf",
       plot = p3, width = 7, height = 5)
message("Saved: plots/output/fig03_norway_be_import_sources.pdf")

# ─────────────────────────────────────────────────────────────────────────────
# PLOT 4 — RCA comparison: Norway vs Nordic peers (2019 and 2022)
# ─────────────────────────────────────────────────────────────────────────────

nordic_iso3 <- c("NOR", "SWE", "DNK", "FIN")

rca_nordic <- node_attrs |>
  filter(iso3 %in% nordic_iso3) |>
  select(iso3,
         `Frontend 2019` = rca_fe_2019,
         `Frontend 2022` = rca_fe_2022,
         `Backend 2019`  = rca_be_2019,
         `Backend 2022`  = rca_be_2022) |>
  pivot_longer(
    -iso3,
    names_to  = "panel",
    values_to = "rca"
  ) |>
  mutate(
    highlight = iso3 == "NOR",
    iso3      = factor(iso3, levels = nordic_iso3)
  )

p4 <- rca_nordic |>
  filter(!is.na(rca)) |>
  ggplot(aes(x = iso3, y = rca, fill = highlight)) +
  geom_col(width = 0.6, alpha = 0.9) +
  geom_hline(yintercept = 1, linetype = "dashed",
             color = "firebrick", linewidth = 0.7) +
  scale_fill_manual(
    values = c("TRUE" = col_nor, "FALSE" = col_grey),
    guide  = "none"
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(5)) +
  facet_wrap(~panel, ncol = 2) +
  labs(
    title    = "Figure 4 — Revealed Comparative Advantage: Nordic comparison",
    subtitle = "Dashed line = RCA threshold (1.0) | RCA > 1 indicates comparative advantage | Norway in orange",
    x = NULL,
    y = "RCA score"
  ) +
  theme_nor +
  theme(strip.text = element_text(face = "bold", size = 9))

ggsave("plots/output/fig04_nordic_rca_comparison.pdf",
       plot = p4, width = 8, height = 6)
message("Saved: plots/output/fig04_nordic_rca_comparison.pdf")

# ─────────────────────────────────────────────────────────────────────────────
# COMBINED — 2×2 patchwork layout for thesis appendix
# ─────────────────────────────────────────────────────────────────────────────

combined <- (p1 | p2) / (p3 | p4) +
  plot_annotation(
    title   = "Norway — Descriptive Semiconductor Trade Profile (2022)",
    caption = "Sources: UN Comtrade, Taiwan ITA, OECD BTIGE. Flows < $1M excluded."
  )

ggsave("plots/output/fig_combined_2x2.pdf",
       plot = combined, width = 14, height = 10)
message("Saved: plots/output/fig_combined_2x2.pdf")
message("All plots complete.")
