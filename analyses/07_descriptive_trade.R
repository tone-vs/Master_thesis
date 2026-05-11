# analyses/07_descriptive_trade.R — Descriptive Trade Position Analysis
#
# Computes Norway's semiconductor trade volumes and position across layers
# and years from the pre-aggregation edge table.
#
# Inputs:
#   data/processed/edges_raw.rds        — combined pre-aggregation edge table
#   data/processed/node_geopolitical.rds — node attributes incl. alliance blocs
#
# Outputs:
#   analyses/output/table_norway_position.tex   — Norway flow summary by layer/year
#   analyses/output/table_top_partners.tex      — Norway's top 10 partners (2022)
#   analyses/output/table_layer_asymmetry.tex   — export/import ratio by layer
#
# Run from project root: Rscript analyses/07_descriptive_trade.R

library(dplyr)
library(tidyr)
library(readr)
library(knitr)
library(kableExtra)

source("config.R")

# ── Guard: check inputs ───────────────────────────────────────────────────────
edges_path   <- file.path("data/processed", "edges_raw.rds")
geo_path     <- file.path("data/processed", "node_geopolitical.rds")

if (!file.exists(edges_path)) {
  stop("edges_raw.rds not found. Run create_data/05_build_network_data.R first.")
}
if (!file.exists(geo_path)) {
  stop("node_geopolitical.rds not found. Run create_data/06_geopolitical_attrs.R first.")
}

dir.create("analyses/output", recursive = TRUE, showWarnings = FALSE)

# ── Load inputs ───────────────────────────────────────────────────────────────
edges_all <- readRDS(edges_path)
node_geo  <- readRDS(geo_path)

message("Loaded edges_all: ", nrow(edges_all), " rows across ",
        n_distinct(edges_all$year), " years")

# ── Helper: write a LaTeX table to analyses/output/ ──────────────────────────
write_tex <- function(tbl, path, caption, label) {
  tex <- knitr::kable(
    tbl,
    format   = "latex",
    booktabs = TRUE,
    caption  = caption,
    label    = label,
    linesep  = ""
  )
  writeLines(as.character(tex), path)
  message("Saved: ", path)
}

# =============================================================================
# TABLE 1 — Norway trade flow summary by layer and year
#
#   For each combination of layer × year × direction (export/import), report:
#   total value (USD bn), number of partners, and share of network total.
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
    trade_bn  = round(sum(trade_value_usd, na.rm = TRUE) / 1e9, 3),
    n_partners = n_distinct(
      if_else(direction == "Export", partner_code, reporter_code)
    ),
    .groups = "drop"
  ) |>
  mutate(
    layer_label = if_else(layer == "layer1_frontend",
                          "Frontend (L1)", "Backend (L2)")
  ) |>
  arrange(layer_label, year, direction) |>
  select(Layer = layer_label, Year = year, Direction = direction,
         `Trade (USD bn)` = trade_bn, `N partners` = n_partners)

message("\nNorway flow summary:")
print(norway_flows)

write_tex(
  norway_flows,
  path    = "analyses/output/table_norway_position.tex",
  caption = paste0("Norway semiconductor trade by layer and year (flows $\\geq$ \\$",
                   MIN_FLOW / 1e6, "M)."),
  label   = "tab:norway-position"
)

# =============================================================================
# TABLE 2 — Norway's top 10 partners by trade value (2022, both layers)
#
#   Shows Norway's most significant bilateral relationships in 2022.
#   Partners are ranked by combined export + import value.
# =============================================================================

norway_bilateral <- edges_all |>
  filter(year == 2022) |>
  mutate(
    direction    = case_when(
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
    Total     = round(Export + Import, 1),
    Export    = round(Export, 1),
    Import    = round(Import, 1),
    # Trade balance: positive = net exporter to this partner
    Balance   = round(Export - Import, 1)
  ) |>
  arrange(desc(Total)) |>
  slice_head(n = 10) |>
  select(Partner = partner_iso, `Export (M)` = Export,
         `Import (M)` = Import, `Total (M)` = Total, `Balance (M)` = Balance)

message("\nNorway top 10 bilateral partners (2022):")
print(norway_bilateral)

write_tex(
  norway_bilateral,
  path    = "analyses/output/table_top_partners.tex",
  caption = "Norway's top 10 bilateral semiconductor trade partners (2022, USD million, both layers).",
  label   = "tab:top-partners"
)

# =============================================================================
# TABLE 3 — Layer asymmetry: Norway's export/import ratio by layer and year
#
#   RCA > 1 in frontend but import-dominant in backend captures the
#   silicon-upstream / chip-downstream asymmetry central to the thesis argument.
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
  path    = "analyses/output/table_layer_asymmetry.tex",
  caption = "Norway semiconductor trade balance by layer and year (USD billion).",
  label   = "tab:layer-asymmetry"
)

message("\n07_descriptive_trade.R complete.")
message("Next: run analyses/08_network_summary.R")
