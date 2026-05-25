# analyses/07_descriptive_trade.R — Descriptive Trade Position Analysis
#
# Computes Norway's semiconductor trade volumes and position across layers
# and years from the pre-aggregation edge table.
#
# BTIGE note: edges_raw.rds contains only ITA HS6 and UN Comtrade flows;
# BTIGE aggregate flows are excluded by the guard in 05_build_network_data.R.
# All tables in this script therefore exclude BTIGE automatically.
#
# Inputs:
#   data/processed/edges_raw.rds         — combined pre-aggregation edge table
#   data/processed/node_geopolitical.rds — node attributes incl. alliance blocs
#
# Outputs:
#   thesis_project/analyses/output/table_norway_position.tex  — Norway flow summary by layer/year
#   thesis_project/analyses/output/table_top_partners.tex     — Norway's top 10 partners (2022)
#   thesis_project/analyses/output/table_layer_asymmetry.tex  — export/import ratio by layer
#   thesis_project/analyses/output/table_hs_exports.tex       — top export products by HS6 (2022)
#   thesis_project/analyses/output/table_hs_imports.tex       — top import products by HS6 (2022)
#
# Run from project root: Rscript analyses/07_descriptive_trade.R

library(dplyr)
library(tidyr)
library(readr)
source("config.R")
source("analyses/table_helpers.R")

# ── Guard: check inputs ───────────────────────────────────────────────────────
edges_path   <- file.path("data/processed", "edges_raw.rds")
geo_path     <- file.path("data/processed", "node_geopolitical.rds")

if (!file.exists(edges_path)) {
  stop("edges_raw.rds not found. Run create_data/05_build_network_data.R first.")
}
if (!file.exists(geo_path)) {
  stop("node_geopolitical.rds not found. Run create_data/06_geopolitical_attrs.R first.")
}

dir.create(DIRS$tables, recursive = TRUE, showWarnings = FALSE)

# ── Load inputs ───────────────────────────────────────────────────────────────
edges_all <- readRDS(edges_path)
node_geo  <- readRDS(geo_path)

message("Loaded edges_all: ", nrow(edges_all), " rows across ",
        n_distinct(edges_all$year), " years")

# ── Shared layer label helper ─────────────────────────────────────────────────
layer_label <- function(layer) {
  if_else(layer == "layer1_frontend", "Frontend (L1)", "Backend (L2)")
}

# =============================================================================
# TABLE 1 — Norway trade flow summary by layer and year
#
#   For each combination of layer × year × direction (export/import), report:
#   total value (USD bn) and number of distinct trading partners.
# =============================================================================

norway_flows <- edges_all |>
  mutate(
    direction = case_when(
      reporter_code == FOCAL_COUNTRY ~ "Export",
      partner_code  == FOCAL_COUNTRY ~ "Import",
      TRUE                           ~ NA_character_
    )
  ) |>
  filter(!is.na(direction)) |>
  group_by(year, layer, direction) |>
  summarise(
    trade_bn   = round(sum(trade_value_usd, na.rm = TRUE) / 1e9, 3),
    n_partners = n_distinct(
      if_else(direction == "Export", partner_code, reporter_code)
    ),
    .groups = "drop"
  ) |>
  mutate(Layer = layer_label(layer)) |>
  arrange(Layer, year, direction) |>
  select(Layer, Year = year, Direction = direction,
         `Trade (USD bn)` = trade_bn, `N partners` = n_partners)

message("\nNorway flow summary:")
print(norway_flows)

write_tex(
  norway_flows,
  path    = file.path(DIRS$tables, "table_norway_position.tex"),
  caption = paste0("Norway semiconductor trade by layer and year (flows $\\geq$ \\$",
                   MIN_FLOW / 1e6, "M)."),
  label   = "tab:norway-position"
)

# =============================================================================
# TABLE 2 — Norway's top 10 partners by trade value (2022, both layers)
#
#   Partners ranked by combined export + import value across both layers.
#   BTIGE flows not present in edges_raw.rds (excluded upstream).
# =============================================================================

norway_bilateral <- edges_all |>
  filter(year == 2022) |>
  mutate(
    direction   = case_when(
      reporter_code == FOCAL_COUNTRY ~ "Export",
      partner_code  == FOCAL_COUNTRY ~ "Import",
      TRUE                           ~ NA_character_
    ),
    partner_iso = case_when(
      reporter_code == FOCAL_COUNTRY ~ partner_code,
      partner_code  == FOCAL_COUNTRY ~ reporter_code,
      TRUE                           ~ NA_character_
    )
  ) |>
  filter(!is.na(direction)) |>
  group_by(partner_iso, direction) |>
  summarise(trade_m = sum(trade_value_usd, na.rm = TRUE) / 1e6, .groups = "drop") |>
  pivot_wider(names_from = direction, values_from = trade_m, values_fill = 0) |>
  mutate(
    Total   = round(Export + Import, 1),
    Export  = round(Export, 1),
    Import  = round(Import, 1),
    # Trade balance: positive = net exporter to this partner
    Balance = round(Export - Import, 1)
  ) |>
  arrange(desc(Total)) |>
  slice_head(n = 10) |>
  select(Partner = partner_iso, `Export (M)` = Export,
         `Import (M)` = Import, `Total (M)` = Total, `Balance (M)` = Balance)

message("\nNorway top 10 bilateral partners (2022):")
print(norway_bilateral)

write_tex(
  norway_bilateral,
  path    = file.path(DIRS$tables, "table_top_partners.tex"),
  caption = paste0(
    "Norway's top 10 bilateral semiconductor trade partners (2022, USD million, both layers). ",
    "OECD BTIGE flows excluded."
  ),
  label   = "tab:top-partners"
)

# =============================================================================
# TABLE 3 — Layer asymmetry: Norway's export/import ratio by layer and year
#
#   Export/Import ratio > 1 = net exporter.
#   Frontend surplus vs backend deficit captures the silicon-upstream /
#   chip-downstream asymmetry central to the thesis argument.
# =============================================================================

layer_asymmetry <- norway_flows |>
  pivot_wider(
    id_cols     = c(Layer, Year),
    names_from  = Direction,
    values_from = `Trade (USD bn)`,
    values_fill = 0
  ) |>
  mutate(
    `Export/Import ratio` = round(Export / pmax(Import, 1e-9), 2),
    `Net position`        = if_else(Export > Import, "Net exporter", "Net importer")
  ) |>
  arrange(Layer, Year) |>
  select(Layer, Year, `Export (bn)` = Export, `Import (bn)` = Import,
         `Export/Import ratio`, `Net position`)

message("\nLayer asymmetry:")
print(layer_asymmetry)

write_tex(
  layer_asymmetry,
  path    = file.path(DIRS$tables, "table_layer_asymmetry.tex"),
  caption = "Norway semiconductor trade balance by layer and year (USD billion).",
  label   = "tab:layer-asymmetry"
)

# =============================================================================
# TABLE 4 — Norway's top export products by HS6 code (2022)
#
#   Aggregated across all partner countries, 2022 only.
#   hs_desc is sourced from the UN Comtrade commodity description field;
#   rows where hs_desc is NA (rare Taiwan ITA edge cases) fall back to
#   the hs_code string. BTIGE aggregate flows not present in edges_raw.rds.
# =============================================================================

norway_hs_exports <- edges_all |>
  filter(year == 2022, reporter_code == FOCAL_COUNTRY) |>
  group_by(layer, hs_code) |>
  summarise(
    Description    = dplyr::first(na.omit(hs_desc)),
    `Trade (USD M)` = round(sum(trade_value_usd, na.rm = TRUE) / 1e6, 1),
    .groups = "drop"
  ) |>
  filter(`Trade (USD M)` > 0) |>
  mutate(Layer = layer_label(layer)) |>
  arrange(Layer, desc(`Trade (USD M)`)) |>
  select(Layer, `HS code` = hs_code, Description, `Trade (USD M)`)

message("\nNorway HS export products (2022):")
print(norway_hs_exports)

write_tex(
  norway_hs_exports,
  path    = file.path(DIRS$tables, "table_hs_exports.tex"),
  caption = paste0(
    "Norway's top semiconductor export products by HS6 code (2022, USD million). ",
    "OECD BTIGE aggregate flows excluded."
  ),
  label   = "tab:hs-exports"
)

# =============================================================================
# TABLE 5 — Norway's top import products by HS6 code (2022)
#
#   partner_code == FOCAL_COUNTRY: Norway is the importing country,
#   reporter_code is the exporting country (Comtrade convention).
#   Aggregated across all exporting partners.
# =============================================================================

norway_hs_imports <- edges_all |>
  filter(year == 2022, partner_code == FOCAL_COUNTRY) |>
  group_by(layer, hs_code) |>
  summarise(
    Description    = dplyr::first(na.omit(hs_desc)),
    `Trade (USD M)` = round(sum(trade_value_usd, na.rm = TRUE) / 1e6, 1),
    .groups = "drop"
  ) |>
  filter(`Trade (USD M)` > 0) |>
  mutate(Layer = layer_label(layer)) |>
  arrange(Layer, desc(`Trade (USD M)`)) |>
  select(Layer, `HS code` = hs_code, Description, `Trade (USD M)`)

message("\nNorway HS import products (2022):")
print(norway_hs_imports)

write_tex(
  norway_hs_imports,
  path    = file.path(DIRS$tables, "table_hs_imports.tex"),
  caption = paste0(
    "Norway's top semiconductor import products by HS6 code (2022, USD million). ",
    "OECD BTIGE aggregate flows excluded."
  ),
  label   = "tab:hs-imports"
)

message("\n07_descriptive_trade.R complete.")
message("Next: run analyses/08_network_summary.R")
