# analyses/12_ergm.R — Exponential Random Graph Models
#
# NAMESPACE ISOLATION: statnet and ergm are loaded ONLY in this script.
# Scripts 07–11 must never load statnet/ergm. Loading statnet masks igraph
# functions (degree(), strength(), betweenness(), etc.); isolating it here
# prevents pollution of the rest of the analysis pipeline.
#
# igraph objects are loaded and converted to network objects inside this
# script. All igraph functions are called with the igraph:: prefix so they
# continue to work correctly even after statnet masks the unqualified names.
#
# Model structure:
#   BE 2022 M1: edges + mutual                         (structural baseline)
#   BE 2022 M2: + nodecov(rca) + nodecov(gdp)          (full — no M2 intermediate;
#               + nodecov(patents) + edgecov(unga)       ~88% density precludes it)
#   FE 2022 M1: edges + mutual
#   FE 2022 M2: edges + mutual + nodecov(rca) + nodecov(gdp)
#               + nodecov(patents) + edgecov(unga)
#   BE 2019 M2: same spec as BE 2022 M2 (temporal comparison)
#
# Inputs:
#   data/processed/graph_frontend_2019.rds
#   data/processed/graph_frontend_2022.rds
#   data/processed/graph_backend_2019.rds
#   data/processed/graph_backend_2022.rds
#   data/processed/node_geopolitical.rds
#   data/processed/dyad_unga_similarity.rds
#   data/processed/unga_similarity_matrix.rds
#
# Outputs:
#   analyses/output/table_ergm_backend.tex    — BE M1 vs M2 (2022)
#   analyses/output/table_ergm_layer.tex      — BE M2 vs FE M2 (2022)
#   analyses/output/table_ergm_temporal.tex   — BE 2019 vs BE 2022 (M2)
#
# Run from project root: Rscript analyses/12_ergm.R
# WARNING: MCMC estimation is slow — allow 30–90 min on a laptop.

# =============================================================================
# 1. Load igraph objects BEFORE loading statnet (igraph:: prefix used below)
# =============================================================================

library(igraph)   # load first; calls below use igraph:: so masking is safe

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

geo_path   <- file.path("data/processed", "node_geopolitical.rds")
unga_d_path <- file.path("data/processed", "dyad_unga_similarity.rds")
unga_m_path <- file.path("data/processed", "unga_similarity_matrix.rds")

for (p in c(geo_path, unga_d_path, unga_m_path)) {
  if (!file.exists(p)) stop(basename(p), " not found. Run create_data/06_geopolitical_attrs.R first.")
}

# Load igraph objects
g_fe_19 <- readRDS(graph_files["fe_2019"])
g_fe_22 <- readRDS(graph_files["fe_2022"])
g_be_19 <- readRDS(graph_files["be_2019"])
g_be_22 <- readRDS(graph_files["be_2022"])

# Load geopolitical attributes and UNGA data
node_geo  <- readRDS(geo_path)
dyad_unga <- readRDS(unga_d_path)

message("igraph objects and geopolitical data loaded.")

# =============================================================================
# 2. NOW load statnet/ergm (masks igraph generics — use igraph:: hereafter)
# =============================================================================

library(statnet)
library(ergm)
library(dplyr)
library(readr)
library(stargazer)

source("config.R")
dir.create("analyses/output", recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# 3. igraph → network conversion
#
#  igraph::as_adjacency_matrix() and igraph::V() are used with full prefix
#  so they dispatch to igraph even though statnet has been loaded.
#
#  node_order  = V(g)$name (ISO3 codes, set by 06_geopolitical_attrs.R)
#  rca_col     = selects the layer- and year-specific RCA column
#  unga_mat    = dyadic UNGA similarity matrix, subset to graph node order
# =============================================================================

igraph_to_network <- function(g, node_geo, dyad_unga, layer_label, yr) {

  node_order <- igraph::V(g)$name   # ISO3 codes

  # Adjacency matrix from binary edge weights
  adj <- igraph::as_adjacency_matrix(
    g, attr = "weight_binary", sparse = FALSE
  )
  adj <- adj[node_order, node_order]

  # network object (from the network package, loaded via statnet)
  net <- network::as.network(adj, directed = TRUE, matrix.type = "adjacency")

  # Node attributes — match by iso3
  nd      <- node_geo[match(node_order, node_geo$iso3), ]
  rca_col <- paste0("rca_",
                    if_else(layer_label == "Frontend", "fe", "be"),
                    "_", yr)

  # RCA: pull from igraph vertex attributes (set by 06_geopolitical_attrs.R)
  rca_vals <- igraph::vertex_attr(g, rca_col)
  if (is.null(rca_vals)) rca_vals <- rep(0, length(node_order))

  network::set.vertex.attribute(net, "rca_log",
    as.numeric(tidyr::replace_na(log1p(rca_vals), 0)))
  network::set.vertex.attribute(net, "gdp_log",
    as.numeric(tidyr::replace_na(nd$gdp_log, 0)))
  network::set.vertex.attribute(net, "patents_log",
    as.numeric(tidyr::replace_na(
      igraph::vertex_attr(g, "patents_log"), 0
    )))
  network::set.vertex.attribute(net, "iso3",
    as.character(tidyr::replace_na(node_order, "UNK")))

  # Dyadic UNGA similarity matrix — padded to graph node set
  unga_mat <- matrix(0,
    nrow = length(node_order), ncol = length(node_order),
    dimnames = list(node_order, node_order)
  )
  dyad_sub <- dyad_unga |>
    filter(iso3_i %in% node_order, iso3_j %in% node_order)
  for (i in seq_len(nrow(dyad_sub))) {
    unga_mat[dyad_sub$iso3_i[i], dyad_sub$iso3_j[i]] <- dyad_sub$unga_sim[i]
  }

  list(net = net, unga_mat = unga_mat)
}

# =============================================================================
# 4. Prepare network objects for all four layer × year combinations
# =============================================================================

message("Converting igraph objects to network objects ...")

prep_be_22 <- igraph_to_network(g_be_22, node_geo, dyad_unga, "Backend",  2022)
prep_fe_22 <- igraph_to_network(g_fe_22, node_geo, dyad_unga, "Frontend", 2022)
prep_be_19 <- igraph_to_network(g_be_19, node_geo, dyad_unga, "Backend",  2019)
prep_fe_19 <- igraph_to_network(g_fe_19, node_geo, dyad_unga, "Frontend", 2019)

message("Network objects ready.")

# Density check — high density signals potential non-identification
for (item in list(
  list("BE 2022", prep_be_22$net),
  list("FE 2022", prep_fe_22$net),
  list("BE 2019", prep_be_19$net),
  list("FE 2019", prep_fe_19$net)
)) {
  message(sprintf("%s density: %.3f", item[[1]], network::network.density(item[[2]])))
}

# =============================================================================
# 5. MCMC control — shared across all models
# =============================================================================

ergm_ctrl <- ergm::control.ergm(
  MCMC.burnin     = 50000,
  MCMC.samplesize = 10000,
  MCMC.interval   = 1000,
  MCMLE.maxit     = 60,
  seed            = 42
)

# =============================================================================
# 6. Fit ERGM models
#
#  BE 2022: two models only — M1 structural, M2 full.
#  No intermediate M2 is reported: at ~88% density single nodecov terms
#  produce non-varying MCMC statistics (see thesis Section 4.3).
#
#  FE 2022: M1 structural + M2 full (same spec as BE for layer comparison).
#
#  BE 2019: M2 full (same spec as BE 2022 M2 for temporal comparison).
# =============================================================================

message("\n=== Fitting BE 2022 M1 (structural baseline) ===")
ergm_be22_m1 <- ergm::ergm(
  prep_be_22$net ~ edges + mutual,
  control = ergm_ctrl
)

message("\n=== Fitting BE 2022 M2 (full model) ===")
ergm_be22_m2 <- ergm::ergm(
  prep_be_22$net ~ edges + mutual +
    nodecov("rca_log") +
    nodecov("gdp_log") +
    nodecov("patents_log") +
    edgecov(prep_be_22$unga_mat),
  control = ergm_ctrl
)

message("\n=== Fitting FE 2022 M1 (structural baseline) ===")
ergm_fe22_m1 <- ergm::ergm(
  prep_fe_22$net ~ edges + mutual,
  control = ergm_ctrl
)

message("\n=== Fitting FE 2022 M2 (full model) ===")
ergm_fe22_m2 <- ergm::ergm(
  prep_fe_22$net ~ edges + mutual +
    nodecov("rca_log") +
    nodecov("gdp_log") +
    nodecov("patents_log") +
    edgecov(prep_fe_22$unga_mat),
  control = ergm_ctrl
)

message("\n=== Fitting BE 2019 M2 (temporal comparison) ===")
ergm_be19_m2 <- ergm::ergm(
  prep_be_19$net ~ edges + mutual +
    nodecov("rca_log") +
    nodecov("gdp_log") +
    nodecov("patents_log") +
    edgecov(prep_be_19$unga_mat),
  control = ergm_ctrl
)

message("\nAll models fitted.")

# =============================================================================
# 7. Diagnostics (run before interpreting coefficients)
#
#  Trace plots should be stationary. Sample statistics should centre near
#  observed values (difference ≈ 0). If they don't converge, increase
#  MCMC.burnin / MCMC.samplesize and re-run.
# =============================================================================

message("\nRunning convergence diagnostics ...")
ergm::mcmc.diagnostics(ergm_be22_m2)
ergm::mcmc.diagnostics(ergm_fe22_m2)
ergm::mcmc.diagnostics(ergm_be19_m2)

# =============================================================================
# 8. Goodness of fit
#
#  Checks whether models reproduce structural features not explicitly modelled:
#  degree distribution, geodesic distance, edgewise shared partners.
# =============================================================================

message("\nComputing goodness of fit ...")

gof_be22 <- ergm::gof(ergm_be22_m2,
                      GOF     = ~ idegree + odegree + distance + espartners,
                      control = ergm::control.gof.ergm(seed = 42))
plot(gof_be22, main = "GoF — BE 2022 M2")

gof_fe22 <- ergm::gof(ergm_fe22_m2,
                      GOF     = ~ idegree + odegree + distance + espartners,
                      control = ergm::control.gof.ergm(seed = 42))
plot(gof_fe22, main = "GoF — FE 2022 M2")

# =============================================================================
# 9. Results tables — stargazer() to LaTeX
# =============================================================================

# ── TABLE A: Backend 2022 — M1 vs M2 ─────────────────────────────────────────
sink("analyses/output/table_ergm_backend.tex")
stargazer::stargazer(
  ergm_be22_m1, ergm_be22_m2,
  type          = "latex",
  title         = "ERGM Results: Back-end Layer (2022)",
  column.labels = c("BE-M1 (Structural)", "BE-M2 (Full)"),
  dep.var.caption        = "P(trade tie = 1) — Back-end layer",
  dep.var.labels.include = FALSE,
  covariate.labels = c(
    "Edges (baseline density)",
    "Mutual (reciprocity)",
    "RCA (log)",
    "GDP (log)",
    "Patents (log)",
    "UNGA similarity (friendshoring)"
  ),
  keep.stat    = c("aic", "bic", "n"),
  ci           = FALSE,
  star.cutoffs = c(0.05, 0.01, 0.001),
  notes = paste(
    "No intermediate M2 reported: at $\\sim$88\\% density single",
    "nodecov terms produce non-varying MCMC statistics.",
    "GWESP omitted following Ou et al.\\ (2024)."
  ),
  header = FALSE
)
sink()
message("Saved: analyses/output/table_ergm_backend.tex")

# ── TABLE B: Layer comparison — BE M2 vs FE M2 (2022) ────────────────────────
sink("analyses/output/table_ergm_layer.tex")
stargazer::stargazer(
  ergm_be22_m2, ergm_fe22_m2,
  type          = "latex",
  title         = "ERGM Results: Layer Comparison (2022)",
  column.labels = c("Back-end (M2)", "Front-end (M2)"),
  dep.var.caption        = "P(trade tie = 1)",
  dep.var.labels.include = FALSE,
  covariate.labels = c(
    "Edges (baseline density)",
    "Mutual (reciprocity)",
    "RCA (log)",
    "GDP (log)",
    "Patents (log)",
    "UNGA similarity (friendshoring)"
  ),
  keep.stat    = c("aic", "bic", "n"),
  ci           = FALSE,
  star.cutoffs = c(0.05, 0.01, 0.001),
  notes        = "Identical specification enables direct cross-layer coefficient comparison.",
  header       = FALSE
)
sink()
message("Saved: analyses/output/table_ergm_layer.tex")

# ── TABLE C: Temporal comparison — BE 2019 vs BE 2022 ────────────────────────
sink("analyses/output/table_ergm_temporal.tex")
stargazer::stargazer(
  ergm_be19_m2, ergm_be22_m2,
  type          = "latex",
  title         = "Back-end ERGM: Temporal Comparison (2019 vs.\\ 2022)",
  column.labels = c("2019 (pre-COVID)", "2022 (post-CHIPS Act)"),
  dep.var.caption        = "P(trade tie = 1) — Back-end layer",
  dep.var.labels.include = FALSE,
  covariate.labels = c(
    "Edges (baseline density)",
    "Mutual (reciprocity)",
    "RCA (log)",
    "GDP (log)",
    "Patents (log)",
    "UNGA similarity (friendshoring)"
  ),
  keep.stat    = c("aic", "bic", "n"),
  ci           = FALSE,
  star.cutoffs = c(0.05, 0.01, 0.001),
  notes        = paste(
    "Identical specification across years enables direct coefficient comparison.",
    "AIC/BIC not comparable across columns (different data).",
    "GWESP omitted following Ou et al.\\ (2024)."
  ),
  header = FALSE
)
sink()
message("Saved: analyses/output/table_ergm_temporal.tex")

message("\n12_ergm.R complete — all ERGM tables written to analyses/output/")
