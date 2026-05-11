# analyses/08_network_summary.R — Network-Level Summary Statistics
#
# Computes standard graph-level descriptives for all four layer × year
# combinations and saves a publication-ready LaTeX table.
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
#   analyses/output/table_network_summary.tex
#
# Run from project root: Rscript analyses/08_network_summary.R

library(igraph)   # loaded for class dispatch; all calls use igraph:: prefix
library(dplyr)
library(knitr)
library(kableExtra)

source("config.R")

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

dir.create("analyses/output", recursive = TRUE, showWarnings = FALSE)

# ── Load graphs ───────────────────────────────────────────────────────────────
g_fe_19 <- readRDS(graph_files["fe_2019"])
g_fe_22 <- readRDS(graph_files["fe_2022"])
g_be_19 <- readRDS(graph_files["be_2019"])
g_be_22 <- readRDS(graph_files["be_2022"])

message("Graphs loaded.")

# ── Helper: compute summary stats for one graph ───────────────────────────────
#
#  All igraph functions called with igraph:: prefix.
#  transitivity() computes the global clustering coefficient (ratio of closed
#  triangles to connected triples); works on directed graphs by default.

network_stats <- function(g, layer_label, yr) {

  # Weighted distance for avg path length (inverted market-share weights)
  # Edge weight for shortest paths: strong trade = short distance
  igraph::E(g)$path_weight <- 1 / (igraph::E(g)$weight_marketshare + 1e-9)

  tibble(
    Layer             = layer_label,
    Year              = yr,
    Nodes             = igraph::vcount(g),
    Edges             = igraph::ecount(g),
    Density           = round(igraph::edge_density(g), 3),
    Reciprocity       = round(igraph::reciprocity(g), 3),
    `Avg out-degree`  = round(mean(igraph::degree(g, mode = "out")), 1),
    `Avg in-degree`   = round(mean(igraph::degree(g, mode = "in")), 1),
    `Transitivity`    = round(igraph::transitivity(g, type = "global"), 3),
    # Weighted average path length — directed, using inverted weights
    # NaN if graph is not strongly connected (expected for sparse networks)
    `Avg path length` = round(
      igraph::mean_distance(g, directed = TRUE, weights = igraph::E(g)$path_weight),
      2
    )
  )
}

# ── Build summary table ───────────────────────────────────────────────────────
summary_tbl <- bind_rows(
  network_stats(g_fe_19, "Frontend (L1)", 2019),
  network_stats(g_fe_22, "Frontend (L1)", 2022),
  network_stats(g_be_19, "Backend  (L2)", 2019),
  network_stats(g_be_22, "Backend  (L2)", 2022)
)

message("\nNetwork summary:")
print(summary_tbl)

# ── Density check (printed to console — feeds ERGM diagnosis discussion) ──────
message("\nDensity check (high density → ERGM covariate non-identification risk):")
summary_tbl |>
  select(Layer, Year, Density) |>
  mutate(Flag = if_else(Density > 0.75, "⚠ HIGH", "OK")) |>
  print()

# ── Save LaTeX table ──────────────────────────────────────────────────────────
tex <- knitr::kable(
  summary_tbl,
  format   = "latex",
  booktabs = TRUE,
  caption  = paste0(
    "Network summary statistics by layer and year. ",
    "Flows $\\geq$ \\$", MIN_FLOW / 1e6,
    "M. Transitivity = global clustering coefficient. ",
    "Path length computed on inverted market-share weights."
  ),
  label    = "tab:network-summary",
  linesep  = ""
)

writeLines(as.character(tex), "analyses/output/table_network_summary.tex")
message("Saved: analyses/output/table_network_summary.tex")
message("\n08_network_summary.R complete.")
message("Next: run analyses/09_centrality.R")
