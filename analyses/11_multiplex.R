# analyses/11_multiplex.R — Multiplex / Inter-Layer Centrality Analysis
#
# Tests whether a country's structural position in the frontend layer predicts
# its position in the backend layer (and vice versa). The Pearson correlations
# operationalise the "multiplex consistency" hypothesis: if semiconductor value
# chains are hierarchically integrated, positions should be correlated across
# layers but not identical.
#
# No igraph package is loaded here — all inputs come from centrality_all.rds
# (a plain data frame produced by 09_centrality.R). This script is purely
# tidyverse + stats.
#
# Inputs:
#   data/processed/centrality_all.rds   — produced by 09_centrality.R
#
# Outputs:
#   thesis_project/analyses/output/table_multiplex_cor.tex    — Pearson r matrix (2022)
#   thesis_project/analyses/output/table_multiplex_nor.tex    — Norway's cross-layer position
#   thesis_project/analyses/output/table_multiplex_change.tex — centrality change 2019→2022
#
# Run from project root: Rscript analyses/11_multiplex.R

library(dplyr)
library(tidyr)
source("config.R")
source("analyses/table_helpers.R")

# ── Guard: check inputs ───────────────────────────────────────────────────────
cent_path <- file.path("data/processed", "centrality_all.rds")

if (!file.exists(cent_path)) {
  stop("centrality_all.rds not found. Run analyses/09_centrality.R first.")
}

dir.create(DIRS$tables, recursive = TRUE, showWarnings = FALSE)

# ── Load centrality data ──────────────────────────────────────────────────────
centrality_all <- readRDS(cent_path)

message("Loaded centrality_all: ", nrow(centrality_all), " observations")
message("Layers: ", paste(unique(centrality_all$layer), collapse = ", "))
message("Years:  ", paste(unique(centrality_all$year),  collapse = ", "))

# =============================================================================
# TABLE 1 — Inter-layer Pearson correlations (2022)
#
#  Pivot to wide format so each country has one row with frontend and backend
#  centrality scores side by side, then compute pairwise correlations.
#  Correlations are reported for out-strength, betweenness, and eigenvector
#  (degree is omitted — it is integer-valued and less informative in dense nets).
# =============================================================================

multiplex_22 <- centrality_all |>
  filter(year == 2022) |>
  select(iso3, layer, is_norway, strength_out, betweenness, eigenvector) |>
  pivot_wider(
    names_from  = layer,
    values_from = c(strength_out, betweenness, eigenvector)
  )

# Column names after pivot: "strength_out_Front-end", "strength_out_Back-end", …
fe_col <- function(measure) paste0(measure, "_Front-end")
be_col <- function(measure) paste0(measure, "_Back-end")

cor_row <- function(measure, label) {
  x <- multiplex_22[[fe_col(measure)]]
  y <- multiplex_22[[be_col(measure)]]
  r <- cor(x, y, use = "complete.obs")
  p <- cor.test(x, y, method = "pearson")$p.value
  tibble(
    Measure     = label,
    `Pearson r` = round(r, 3),
    `p-value`   = round(p, 4),
    `Sig.`      = case_when(p < 0.001 ~ "***", p < 0.01 ~ "**",
                            p < 0.05  ~ "*",   TRUE      ~ "")
  )
}

cor_table <- bind_rows(
  cor_row("strength_out", "Out-strength"),
  cor_row("betweenness",  "Betweenness"),
  cor_row("eigenvector",  "Eigenvector")
)

message("\nInter-layer centrality correlations (2022):")
print(cor_table)

write_tex(
  cor_table,
  path    = file.path(DIRS$tables, "table_multiplex_cor.tex"),
  caption = paste0(
    "Inter-layer centrality correlations (Pearson $r$), frontend vs.\\ backend, 2022 ($n = ",
    nrow(multiplex_22), "$ countries). ",
    "*** $p<0.001$, ** $p<0.01$, * $p<0.05$."
  ),
  label   = "tab:multiplex-cor"
)

# =============================================================================
# TABLE 2 — Norway's cross-layer position (both years)
#
#  Shows Norway's rank and raw score in each measure, each layer, each year.
#  Rank is within-layer-year (1 = most central).
# =============================================================================

add_rank <- function(df, measure) {
  df |>
    group_by(layer, year) |>
    mutate(!!paste0("rank_", measure) := rank(-!!sym(measure), ties.method = "min")) |>
    ungroup()
}

cent_ranked <- centrality_all |>
  add_rank("strength_out") |>
  add_rank("betweenness") |>
  add_rank("eigenvector")

norway_position <- cent_ranked |>
  filter(iso3 == FOCAL_COUNTRY) |>
  mutate(
    across(c(strength_out, betweenness, eigenvector), ~round(.x, 4))
  ) |>
  arrange(layer, year) |>
  select(
    Layer    = layer,
    Year     = year,
    `Str. out`     = strength_out,
    `Rank str.`    = rank_strength_out,
    `Betweenness`  = betweenness,
    `Rank btwn.`   = rank_betweenness,
    `Eigenvector`  = eigenvector,
    `Rank eigen.`  = rank_eigenvector
  )

message("\nNorway's cross-layer position:")
print(norway_position)

write_tex(
  norway_position,
  path    = file.path(DIRS$tables, "table_multiplex_nor.tex"),
  caption = paste0(
    "Norway's centrality scores and within-network ranks across layers and years. ",
    "Rank 1 = most central. $n = ",
    n_distinct(centrality_all$iso3), "$ countries per network."
  ),
  label   = "tab:multiplex-nor"
)

# =============================================================================
# TABLE 3 — Centrality change 2019 → 2022
#
#  For each country, computes the absolute and relative change in out-strength
#  and betweenness across years within each layer. Norway highlighted.
#  Captures COVID and CHIPS Act effects on network position.
# =============================================================================

change_tbl <- centrality_all |>
  select(iso3, layer, year, strength_out, betweenness, eigenvector) |>
  pivot_wider(
    names_from  = year,
    values_from = c(strength_out, betweenness, eigenvector)
  ) |>
  mutate(
    d_strength = round(strength_out_2022 - strength_out_2019, 4),
    d_btwn     = round(betweenness_2022  - betweenness_2019,  4),
    d_eigen    = round(eigenvector_2022  - eigenvector_2019,  4),
    is_norway  = iso3 == FOCAL_COUNTRY
  )

# Top movers (by absolute out-strength change) per layer
top_movers <- change_tbl |>
  group_by(layer) |>
  slice_max(abs(d_strength), n = 5) |>
  ungroup() |>
  arrange(layer, desc(abs(d_strength))) |>
  select(
    Layer      = layer,
    Country    = iso3,
    `Str 2019` = strength_out_2019,
    `Str 2022` = strength_out_2022,
    `Δ Str.`   = d_strength,
    `Δ Btwn.`  = d_btwn,
    `Δ Eigen.` = d_eigen
  ) |>
  mutate(across(where(is.double), ~round(.x, 4)))

# Always include Norway even if not in top 5
norway_change <- change_tbl |>
  filter(is_norway) |>
  select(
    Layer      = layer,
    Country    = iso3,
    `Str 2019` = strength_out_2019,
    `Str 2022` = strength_out_2022,
    `Δ Str.`   = d_strength,
    `Δ Btwn.`  = d_btwn,
    `Δ Eigen.` = d_eigen
  ) |>
  mutate(across(where(is.double), ~round(.x, 4)))

change_output <- bind_rows(top_movers, norway_change) |>
  distinct(Layer, Country, .keep_all = TRUE) |>
  arrange(Layer, desc(abs(`Δ Str.`)))

message("\nTop centrality movers (2019 → 2022):")
print(change_output)

write_tex(
  change_output,
  path    = file.path(DIRS$tables, "table_multiplex_change.tex"),
  caption = paste0(
    "Largest changes in out-strength centrality, 2019--2022, by layer ",
    "(top 5 per layer plus Norway). ",
    "$\\Delta$ = 2022 score minus 2019 score. ",
    "Taiwan (TWN) is excluded from the 2019 baseline (ITA data is 2022-only; ",
    "TWN had no edges in 2019 and does not appear in this table)."
  ),
  label   = "tab:multiplex-change"
)

message("\n11_multiplex.R complete.")
message("Next: run analyses/12_ergm.R")
