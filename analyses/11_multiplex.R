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
#   thesis_project/analyses/output/table_multiplex_cor.tex        — Pearson r matrix (2022)
#   thesis_project/analyses/output/table_multiplex_change.tex     — centrality change 2019→2022
#   thesis_project/analyses/output/table_multiplex_crosslayer.tex — cross-layer rankings (2022)
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
# TABLE 2 — Centrality change 2019 → 2022
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

# =============================================================================
# TABLE 3 — Cross-layer centrality rankings (2022)
#
#  Top 5 countries by average eigenvector centrality across front-end and
#  back-end, plus Norway (always included). Sorted descending by average
#  eigenvector. Reveals which countries hold central positions in both layers.
# =============================================================================

crosslayer_wide <- centrality_all |>
  filter(year == 2022) |>
  select(iso3, is_norway, layer, strength_out, eigenvector) |>
  pivot_wider(
    id_cols     = c(iso3, is_norway),
    names_from  = layer,
    values_from = c(strength_out, eigenvector)
  ) |>
  rename(
    `FE Out-strength` = `strength_out_Front-end`,
    `BE Out-strength` = `strength_out_Back-end`,
    `FE Eigenvector`  = `eigenvector_Front-end`,
    `BE Eigenvector`  = `eigenvector_Back-end`
  ) |>
  mutate(
    avg_eigen = (`FE Eigenvector` + `BE Eigenvector`) / 2,
    Country   = countrycode::countrycode(iso3, "iso3c", "country.name")
  )

crosslayer_top5  <- crosslayer_wide |> slice_max(avg_eigen, n = 5)
crosslayer_nor   <- crosslayer_wide |> filter(is_norway)
crosslayer_22    <- bind_rows(crosslayer_top5, crosslayer_nor) |>
  distinct(iso3, .keep_all = TRUE) |>
  arrange(desc(avg_eigen)) |>
  select(
    ISO3              = iso3,
    Country,
    `FE Out-strength`,
    `BE Out-strength`,
    `FE Eigenvector`,
    `BE Eigenvector`
  ) |>
  mutate(across(where(is.double), ~round(.x, 4)))

message("\nCross-layer centrality rankings (2022, top 5 + Norway):")
print(crosslayer_22)

write_tex(
  crosslayer_22,
  path    = file.path(DIRS$tables, "table_multiplex_crosslayer.tex"),
  caption = paste0(
    "Cross-layer out-strength and eigenvector centrality, 2022 ",
    "(top 5 countries by average eigenvector centrality across layers, plus Norway). ",
    "Sorted by average eigenvector centrality (descending)."
  ),
  label   = "tab:multiplex-crosslayer"
)

message("\n11_multiplex.R complete.")
message("Next: run analyses/12_ergm.R")
