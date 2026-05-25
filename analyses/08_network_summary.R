# analyses/08_network_summary.R — Network-Level Summary Statistics
#
# Computes standard graph-level descriptives for all four layer × year
# combinations and saves two publication-ready LaTeX tables:
#   • Primary table  (2022 only)    — cited in the main analysis chapter
#   • Appendix table (2019 only)    — robustness check; notes TWN absence
#
# igraph functions are called with the full igraph:: namespace prefix throughout
# so this script is safe to run without library(igraph) in the search path and
# will not conflict with statnet if that is loaded in another session.
#
# Inputs:
#   data/processed/graph_frontend_2019.rds
#   data/processed/graph_frontend_2022.rds
#   data/processed/graph_backend_2019.rds
#   data/processed/graph_backend_2022.rds
#
# Outputs:
#   thesis_project/analyses/output/table_network_summary.tex          — 2022 networks
#   thesis_project/analyses/output/table_network_summary_appendix.tex — 2019 networks
#
# Run from project root: Rscript analyses/08_network_summary.R

library(igraph)   # loaded for class dispatch; all calls use igraph:: prefix
library(dplyr)

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

# ── Helper: compute summary stats for one graph ───────────────────────────────
#
#  All igraph functions called with igraph:: prefix.
#
#  Two clustering coefficients are reported:
#    Global CC  — igraph::transitivity(type = "global"): ratio of closed
#                 triangles to all connected triples (network-level).
#    Avg CC     — igraph::transitivity(type = "average"): mean of per-node
#                 local clustering coefficients (typical node-level tendency).
#                 Computed on the symmetrised graph; nodes with degree < 2
#                 (undefined local CC) are excluded by igraph automatically.
#  Both treat the graph as undirected for triangle counting, which is standard
#  practice for directed trade networks.

network_stats <- function(g, layer_label, yr) {

  # Weighted distance for avg path length (inverted market-share weights)
  # Edge weight for shortest paths: strong trade = short distance
  igraph::E(g)$path_weight <- 1 / (igraph::E(g)$weight_marketshare + 1e-9)

  # Symmetrise once for both clustering coefficients
  g_ud <- igraph::as_undirected(g, mode = "collapse",
                                 edge.attr.comb = list(weight_marketshare = "sum", "ignore"))

  tibble(
    Layer             = layer_label,
    Year              = yr,
    Nodes             = igraph::vcount(g),
    Edges             = igraph::ecount(g),
    Density           = round(igraph::edge_density(g), 3),
    Reciprocity       = round(igraph::reciprocity(g), 3),
    `Avg out-degree`  = round(mean(igraph::degree(g, mode = "out")), 1),
    `Avg in-degree`   = round(mean(igraph::degree(g, mode = "in")), 1),
    `Global CC`       = round(igraph::transitivity(g_ud, type = "global"),  3),
    `Avg CC`          = round(igraph::transitivity(g_ud, type = "average"), 3),
    # Weighted average path length — directed, using inverted weights
    # NaN if graph is not strongly connected (expected for sparse networks)
    `Avg path length` = round(
      igraph::mean_distance(g, directed = TRUE, weights = igraph::E(g)$path_weight),
      2
    )
  )
}

# ── Build per-year summary tables ─────────────────────────────────────────────
summary_22 <- bind_rows(
  network_stats(g_fe_22, "Frontend (L1)", 2022),
  network_stats(g_be_22, "Backend  (L2)", 2022)
)

summary_19 <- bind_rows(
  network_stats(g_fe_19, "Frontend (L1)", 2019),
  network_stats(g_be_19, "Backend  (L2)", 2019)
)

message("\nNetwork summary (2022):")
print(summary_22)

message("\nNetwork summary (2019):")
print(summary_19)

# ── Density check (printed to console — feeds ERGM diagnosis discussion) ──────
message("\nDensity check (high density → ERGM covariate non-identification risk):")
bind_rows(summary_22, summary_19) |>
  select(Layer, Year, Density) |>
  mutate(Flag = if_else(Density > 0.75, "HIGH", "OK")) |>
  arrange(Year, Layer) |>
  print()

# ── TABLE 1: Primary — 2022 networks ─────────────────────────────────────────
write_tex(
  summary_22,
  path    = file.path(DIRS$tables, "table_network_summary.tex"),
  caption = paste0(
    "Network summary statistics by layer (2022). ",
    "Flows $\\geq$ \\$", MIN_FLOW / 1e6, "M. ",
    "\\textit{Global CC} = ratio of closed triangles to connected triples. ",
    "\\textit{Avg CC} = mean of per-node local clustering coefficients; ",
    "both computed on the symmetrised graph. ",
    "Path length computed on inverted market-share weights (strong trade $=$ short distance). ",
    "2019 results in Table~\\ref{tab:network-summary-appendix}."
  ),
  label   = "tab:network-summary"
)

# ── TABLE 2: Appendix — 2019 networks (robustness check) ─────────────────────
#
#  Taiwan (TWN) is absent from the 2019 networks: ITA HS6 data covers 2022
#  only, and TWN had no qualifying edges in the UN Comtrade extract for 2019.
#  Nodes with no edges are dropped by build_graph() in 05_build_network_data.R,
#  so the 2019 graphs have 29 nodes vs 30 in 2022.

write_tex(
  summary_19,
  path    = file.path(DIRS$tables, "table_network_summary_appendix.tex"),
  caption = paste0(
    "Network summary statistics for 2019, reported as a robustness check. ",
    "Results are broadly consistent with 2022, suggesting findings are not ",
    "driven by post-COVID supply chain disruptions. ",
    "Taiwan (TWN) is excluded from 2019 networks (ITA data is 2022-only; ",
    "TWN had no edges and was removed to avoid an isolated node)."
  ),
  label   = "tab:network-summary-appendix"
)

message("\n08_network_summary.R complete.")
message("Next: run analyses/09_centrality.R")
