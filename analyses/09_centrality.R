# analyses/09_centrality.R — Centrality Analysis
#
# Computes four centrality measures for all nodes in all four layer × year
# graphs. Saves the combined data frame as an RDS (for downstream scripts)
# and LaTeX tables for the thesis.
#
# Measures:
#   degree_in / degree_out  — in- and out-degree (unweighted)
#   strength_in / out       — weighted degree using weight_marketshare
#   betweenness             — weighted betweenness (normalised); weights
#                             inverted so strong trade = short distance
#   eigenvector             — eigenvector centrality (directed, weighted)
#
# igraph functions use the full igraph:: namespace throughout. Never call
# bare degree(), strength(), betweenness(), or eigen_centrality() — these
# are masked by statnet when both packages are loaded.
#
# Inputs:
#   data/processed/graph_frontend_2019.rds
#   data/processed/graph_frontend_2022.rds
#   data/processed/graph_backend_2019.rds
#   data/processed/graph_backend_2022.rds
#
# Outputs:
#   data/processed/centrality_all.rds                                    — full centrality data frame
#   thesis_project/analyses/output/table_centrality_fe22.tex             — frontend 2022 rankings (longtable)
#   thesis_project/analyses/output/table_centrality_be22.tex             — backend 2022 rankings (longtable)
#   thesis_project/analyses/output/table_centrality_full.tex             — both layers combined, 2022 (longtable)
#   thesis_project/analyses/output/table_centrality_norway.tex           — Norway centrality, 2022 (primary)
#   thesis_project/analyses/output/table_centrality_norway_appendix.tex  — Norway centrality, 2019 (robustness)
#
# Run from project root: Rscript analyses/09_centrality.R

library(igraph)      # loaded for class dispatch; all calls use igraph:: prefix
library(dplyr)
library(countrycode)
source("config.R")
source("analyses/table_helpers.R")

# ── Guard: check inputs ───────────────────────────────────────────────────────
graph_files <- c(
  fe_2019 = file.path("data/processed", "graph_frontend_2019.rds"),
  fe_2022 = file.path("data/processed", "graph_frontend_2022.rds"),
  be_2019 = file.path("data/processed", "graph_backend_2019.rds"),
  be_2022 = file.path("data/processed", "graph_backend_2022.rds")
)

missing <- graph_files[!file.exists(graph_files)]
if (length(missing) > 0) {
  stop("Missing graph files: ", paste(names(missing), collapse = ", "),
       "\nRun create_data/05_build_network_data.R first.")
}

dir.create(DIRS$tables, recursive = TRUE, showWarnings = FALSE)

# ── Load graphs ───────────────────────────────────────────────────────────────
g_fe_19 <- readRDS(graph_files["fe_2019"])
g_fe_22 <- readRDS(graph_files["fe_2022"])
g_be_19 <- readRDS(graph_files["be_2019"])
g_be_22 <- readRDS(graph_files["be_2022"])

message("Graphs loaded.")

# ── Helper: compute distance weights (betweenness) ───────────────────────────
#
# betweenness() uses edge weights as distances, so strong trade ties should
# become short distances. Invert: dist = 1 / (market_share + epsilon).
# The epsilon prevents division-by-zero on zero-weight edges.

add_dist_weight <- function(g) {
  igraph::E(g)$dist <- 1 / (igraph::E(g)$weight_marketshare + 1e-9)
  g
}

g_fe_19 <- add_dist_weight(g_fe_19)
g_fe_22 <- add_dist_weight(g_fe_22)
g_be_19 <- add_dist_weight(g_be_19)
g_be_22 <- add_dist_weight(g_be_22)

# ── Core centrality function ──────────────────────────────────────────────────
#
# All igraph functions called with igraph:: prefix.
# V(g)$name stores ISO3 codes (set by 06_geopolitical_attrs.R via attach_node_attrs).
# eigen_centrality() returns a list; extract $vector for the node scores.

make_cent <- function(g, layer_label, yr) {

  # ── Name normalisation ──────────────────────────────────────────────────────
  # Graphs built by the old pipeline store full country names ("Norway",
  # "China") as vertex names instead of ISO3 codes. Detect and convert here
  # so all downstream tibble columns use ISO3 regardless of graph provenance.
  raw_names <- igraph::V(g)$name
  if (any(nchar(raw_names) > 3)) {
    message("  [make_cent] Vertex names appear to be full country names; ",
            "converting to ISO3 via countrycode().")
    raw_names <- countrycode::countrycode(
      raw_names,
      origin      = "country.name",
      destination = "iso3c",
      custom_match = c(
        "CHINESE TAIPEI"       = "TWN",
        "CHINA, HONG KONG SAR" = "HKG",
        "REP. OF KOREA"        = "KOR",
        "VIET NAM"             = "VNM",
        "CZECHIA"              = "CZE"
      )
    )
    n_fail <- sum(is.na(raw_names))
    if (n_fail > 0)
      warning("  make_cent(): ", n_fail, " vertex name(s) could not be converted to ISO3 ",
              "and will appear as NA.")
  }

  tibble(
    iso3         = raw_names,   # normalised ISO3 codes
    degree_in    = igraph::degree(g, mode = "in"),
    degree_out   = igraph::degree(g, mode = "out"),
    strength_in  = igraph::strength(g, mode = "in",
                                    weights = igraph::E(g)$weight_marketshare),
    strength_out = igraph::strength(g, mode = "out",
                                    weights = igraph::E(g)$weight_marketshare),
    betweenness  = igraph::betweenness(
      g,
      directed   = TRUE,
      weights    = igraph::E(g)$dist,   # inverted weights → shorter = stronger
      normalized = TRUE
    ),
    eigenvector  = igraph::eigen_centrality(
      g,
      directed = TRUE,
      weights  = igraph::E(g)$weight_marketshare
    )$vector
  ) |>
    mutate(
      country   = countrycode::countrycode(
        iso3, "iso3c", "country.name",
        custom_match = c(TWN = "Taiwan", NOR = "Norway")
      ),
      layer     = layer_label,
      year      = yr,
      is_norway = iso3 == FOCAL_COUNTRY
    )
}

# ── Compute centrality for all four networks ──────────────────────────────────
message("Computing centrality (4 networks) ...")

cent_fe_19 <- make_cent(g_fe_19, "Front-end", 2019)
cent_fe_22 <- make_cent(g_fe_22, "Front-end", 2022)
cent_be_19 <- make_cent(g_be_19, "Back-end",  2019)
cent_be_22 <- make_cent(g_be_22, "Back-end",  2022)

centrality_all <- dplyr::bind_rows(cent_fe_19, cent_fe_22, cent_be_19, cent_be_22)

message("Centrality computed for ", nrow(centrality_all), " node-layer-year observations.")

# Norway summary (console)
message("\nNorway centrality across all networks:")
centrality_all |>
  filter(iso3 == FOCAL_COUNTRY) |>
  select(layer, year, degree_out, degree_in, strength_out, strength_in,
         betweenness, eigenvector) |>
  mutate(across(where(is.double), ~round(.x, 4))) |>
  print()

# ── Save combined RDS (consumed by 11_multiplex.R) ───────────────────────────
saveRDS(centrality_all, file.path("data/processed", "centrality_all.rds"))
message("Saved: data/processed/centrality_all.rds")

# ── Helpers: format centrality data frames for LaTeX ─────────────────────────
#
#  format_cent_table()      — single-layer table: Country, measures (no Layer col)
#  format_cent_table_full() — combined table: Layer, ISO3, measures

format_cent_table <- function(cent_df) {
  cent_df |>
    arrange(desc(strength_out)) |>
    mutate(across(c(strength_in, strength_out, betweenness, eigenvector),
                  ~round(.x, 4))) |>
    select(
      Country     = iso3,
      `In-deg`    = degree_in,
      `Out-deg`   = degree_out,
      `Str. in`   = strength_in,
      `Str. out`  = strength_out,
      Betweenness = betweenness,
      Eigenvector = eigenvector
    )
}

format_cent_table_full <- function(cent_22) {
  # Both layers combined; Layer + ISO3 as identifiers, no Country name column.
  # Sorted by layer then out-strength descending.
  cent_22 |>
    arrange(layer, desc(strength_out)) |>
    mutate(across(c(strength_in, strength_out, betweenness, eigenvector),
                  ~round(.x, 4))) |>
    select(
      Layer       = layer,
      ISO3        = iso3,
      `In-deg`    = degree_in,
      `Out-deg`   = degree_out,
      `Str. in`   = strength_in,
      `Str. out`  = strength_out,
      Betweenness = betweenness,
      Eigenvector = eigenvector
    )
}

# ── TABLE: Frontend 2022 centrality rankings (longtable — 30 rows) ────────────
write_tex_long(
  format_cent_table(cent_fe_22),
  path    = file.path(DIRS$tables, "table_centrality_fe22.tex"),
  caption = paste0(
    "Frontend layer (2022) centrality rankings, sorted by out-strength. ",
    "Strength = weighted degree (market-share weights). ",
    "Betweenness normalised; weights inverted so strong ties = short distances. ",
    "Norway (NOR) highlighted."
  ),
  label   = "tab:centrality-fe22"
)

# ── TABLE: Backend 2022 centrality rankings (longtable — 30 rows) ─────────────
write_tex_long(
  format_cent_table(cent_be_22),
  path    = file.path(DIRS$tables, "table_centrality_be22.tex"),
  caption = paste0(
    "Backend layer (2022) centrality rankings, sorted by out-strength. ",
    "See Table~\\ref{tab:centrality-fe22} for column definitions."
  ),
  label   = "tab:centrality-be22"
)

# ── TABLE: Full rankings — both layers combined (2022, longtable) ─────────────
#
# Combined table for cross-layer comparison; one row per country per layer.
# Intended for the appendix: 60 rows (30 countries × 2 layers).

write_tex_long(
  format_cent_table_full(bind_rows(cent_fe_22, cent_be_22)),
  path    = file.path(DIRS$tables, "table_centrality_full.tex"),
  caption = paste0(
    "Full centrality rankings for all countries, front-end and back-end layers (2022). ",
    "Sorted by layer then out-strength (descending). ",
    "Betweenness normalised to $[0,1]$; weights inverted so strong ties $=$ short distances."
  ),
  label   = "tab:centrality-full"
)

# ── TABLE: Norway centrality — primary (2022 only) ────────────────────────────
#
# Primary table for the main analysis chapter.
# 2019 results reported separately as a robustness check (appendix).

norway_cent_format <- function(df) {
  df |>
    filter(iso3 == FOCAL_COUNTRY) |>
    mutate(across(c(strength_in, strength_out, betweenness, eigenvector),
                  ~round(.x, 4))) |>
    arrange(layer) |>
    select(
      Layer        = layer,
      Year         = year,
      `In-degree`  = degree_in,
      `Out-degree` = degree_out,
      `Str. in`    = strength_in,
      `Str. out`   = strength_out,
      Betweenness  = betweenness,
      Eigenvector  = eigenvector
    )
}

write_tex(
  norway_cent_format(centrality_all |> filter(year == 2022)),
  path    = file.path(DIRS$tables, "table_centrality_norway.tex"),
  caption = paste0(
    "Norway's centrality scores by layer (2022). ",
    "Strength weighted by bilateral market share. ",
    "Betweenness normalised to $[0,1]$. ",
    "2019 results in Table~\\ref{tab:centrality-norway-appendix}."
  ),
  label   = "tab:centrality-norway"
)

# ── TABLE: Norway centrality — appendix (2019 robustness check) ───────────────
#
# Taiwan (TWN) absent from 2019 networks: ITA data is 2022-only; node count = 29.
# 2019 results confirm that 2022 findings are not driven by post-COVID disruptions.

write_tex(
  norway_cent_format(centrality_all |> filter(year == 2019)),
  path    = file.path(DIRS$tables, "table_centrality_norway_appendix.tex"),
  caption = paste0(
    "Norway's centrality scores by layer (2019). Reported as robustness check. ",
    "2019 results confirm that 2022 findings are not driven by post-COVID supply chain disruptions. ",
    "Taiwan (TWN) excluded from 2019 networks (ITA data is 2022-only; ",
    "TWN had no edges and was removed to avoid an isolated node)."
  ),
  label   = "tab:centrality-norway-appendix"
)

message("\n09_centrality.R complete.")
message("Next: run analyses/10_community_detection.R")
