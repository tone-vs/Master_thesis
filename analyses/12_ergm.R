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
# Scope:
#   2022 is the primary analysis year.
#   BE 2019 is included solely for the temporal comparison (pre-disruption
#   baseline). FE 2019 is excluded: Taiwan is absent from the front-end 2019
#   network due to OECD BTIGE coverage limitations (Taiwan ITA data covers
#   2022 only), making a cross-year front-end ERGM comparison unreliable.
#
#   Temporal consistency: the BE temporal comparison (2019 vs 2022) uses
#   OECD BTIGE for Taiwan's back-end edges in BOTH years. ITA HS6 data exists
#   only for 2022, so using it would create a measurement inconsistency in the
#   2019 vs 2022 comparison. The standard BE 2022 graph (ITA-based) is used
#   for all non-temporal tables (M1 vs M2, layer comparison).
#
# Model structure (three-model progression for each layer × year):
#   M1 — structural baseline:  edges + mutual
#   M2 — economic + gravity:   edges + mutual + nodecov(rca) + nodecov(gdp)
#                              + nodecov(patents) + edgecov(dist)
#   M3 — full model:           M2 + edgecov(unga)
#
#   Fitted for: BE 2022 (ITA), FE 2022 (ITA),
#               BE 2022 (BTIGE, temporal), BE 2019 (BTIGE, temporal).
#   FE 2019 is not modelled: Taiwan absent from front-end 2019 network.
#
# Inputs:
#   data/processed/graph_frontend_2022.rds           — ITA-based (FE M1–M3 + layer)
#   data/processed/graph_backend_2022.rds            — ITA-based (BE M1–M3 + layer)
#   data/processed/graph_backend_2022_ergm.rds       — BTIGE Taiwan (BE 2022 temporal)
#   data/processed/graph_backend_2019_ergm.rds       — BTIGE Taiwan (BE 2019 temporal)
#   data/processed/node_geopolitical.rds
#   data/processed/node_geopolitical_2019.csv  — 2019 GDP for temporal comparison
#   data/processed/dyad_unga_similarity.csv    — penalised UNGA similarity, 2017–2019
#   data/processed/unga_similarity_matrix.rds  — shared across all ERGM specifications
#   data/processed/dist_matrix_log.rds
#
# Outputs:
#   thesis_project/analyses/output/table_ergm_backend.tex    — BE M1, M2, M3 (2022)
#   thesis_project/analyses/output/table_ergm_layer.tex      — BE M3 vs FE M3 (2022)
#   thesis_project/analyses/output/table_ergm_temporal.tex   — BE 2019 M3 vs BE 2022 M3
#   thesis_project/plots/output/fig_ergm_gof_be.pdf          — GoF diagnostics BE 2022 M3
#   thesis_project/plots/output/fig_ergm_gof_fe.pdf          — GoF diagnostics FE 2022 M3
#
# Run from project root: Rscript analyses/12_ergm.R
# WARNING: MCMC estimation is slow — allow 30–90 min on a laptop.

# =============================================================================
# 1. Load igraph objects BEFORE loading statnet (igraph:: prefix used below)
# =============================================================================

library(igraph)   # load first; calls below use igraph:: so masking is safe

# Standard graphs (ITA-based Taiwan 2022) — used for BE M1 vs M2 vs M3 and
# layer comparisons.
graph_files <- c(
  fe_2022 = file.path("data/processed", "graph_frontend_2022.rds"),
  be_2022 = file.path("data/processed", "graph_backend_2022.rds")
  # fe_2019 intentionally excluded: Taiwan absent from front-end 2019 network
  # (OECD BTIGE coverage gap) — cross-year FE comparison would be unreliable.
)

# ERGM-specific BE graphs (BTIGE Taiwan, both years) — used only for the
# temporal comparison so that Taiwan's measurement basis is identical in both
# years.
ergm_graph_files <- c(
  be_2022_ergm = file.path("data/processed", "graph_backend_2022_ergm.rds"),
  be_2019_ergm = file.path("data/processed", "graph_backend_2019_ergm.rds")
)

missing <- c(graph_files, ergm_graph_files)[!file.exists(c(graph_files, ergm_graph_files))]
if (length(missing) > 0) {
  stop("Missing graph files: ", paste(names(missing), collapse = ", "),
       "\nRun create_data/05_build_network_data.R first.")
}

geo_path      <- file.path("data/processed", "node_geopolitical.rds")
unga_d_path   <- file.path("data/processed", "dyad_unga_similarity.csv")
unga_m_path   <- file.path("data/processed", "unga_similarity_matrix.rds")
dist_path     <- file.path("data/processed", "dist_matrix_log.rds")
gdp_2019_path <- file.path("data/processed", "node_geopolitical_2019.csv")

for (p in c(geo_path, unga_d_path, unga_m_path, dist_path)) {
  if (!file.exists(p)) stop(basename(p), " not found. Run create_data/06_geopolitical_attrs.R first.")
}
if (!file.exists(gdp_2019_path)) {
  stop("node_geopolitical_2019.csv not found. Run create_data/06_geopolitical_attrs.R first.")
}

# Load igraph objects
g_fe_22      <- readRDS(graph_files["fe_2022"])
g_be_22      <- readRDS(graph_files["be_2022"])
g_be_22_ergm <- readRDS(ergm_graph_files["be_2022_ergm"])  # temporal comparison
g_be_19_ergm <- readRDS(ergm_graph_files["be_2019_ergm"])  # temporal comparison

# Load geopolitical attributes, UNGA data, and geographic distance matrix.
# UNGA similarity uses penalised formula over votes 2017–2019 (the most recent
# complete three-year window in the unvotes package; data ends in 2019).
# A single shared dyad_unga is used for all ERGM specifications.
node_geo        <- readRDS(geo_path)
dyad_unga       <- readr::read_csv(unga_d_path, show_col_types = FALSE)
dist_matrix_log <- readRDS(dist_path)

# Load 2019 GDP for temporal ERGM comparison (year-specific values for BE 2019)
gdp_2019 <- readr::read_csv(gdp_2019_path, show_col_types = FALSE) |>
  select(iso3, gdp_log_2019 = gdp_log)

message("igraph objects and geopolitical data loaded.")

# =============================================================================
# 2. NOW load statnet/ergm (masks igraph generics — use igraph:: hereafter)
# =============================================================================

library(statnet)
library(ergm)
library(dplyr)
library(countrycode)  # needed for vertex-name normalisation fallback

source("config.R")
dir.create(DIRS$tables,   recursive = TRUE, showWarnings = FALSE)
dir.create(DIRS$figures,  recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# 3. igraph → network conversion
#
#  node_order   = V(g)$name (ISO3 codes, set by 06_geopolitical_attrs.R)
#  rca_col      = selects the layer- and year-specific RCA column
#  unga_mat     = dyadic UNGA similarity matrix, subset to graph node order;
#                 built from dyad_unga (penalised similarity, votes 2017–2019,
#                 shared across all ERGM specifications)
#  dist_mat     = geographic distance matrix, reordered to match node_order
#  gdp_override = optional data frame with columns iso3 and gdp_log_2019;
#                 when provided, replaces nd$gdp_log for the gdp_log vertex
#                 attribute. Used for the BE 2019 temporal comparison only
#                 so that GDP values match the network year.
# =============================================================================

igraph_to_network <- function(g, node_geo, dyad_unga, dist_mat_log,
                               layer_label, yr, gdp_override = NULL) {

  # node_order must be ISO3 codes for all node_geo and dyad_unga joins.
  # Graphs from the old pipeline use full country names — convert if needed.
  node_order <- igraph::V(g)$name
  if (any(nchar(node_order) > 3)) {
    message("  [igraph_to_network] Converting vertex names to ISO3 ...")
    node_order <- countrycode::countrycode(
      node_order,
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
    n_fail <- sum(is.na(node_order))
    if (n_fail > 0)
      warning("igraph_to_network(): ", n_fail, " vertex name(s) could not be converted.")
  }

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
  gdp_vals <- if (!is.null(gdp_override)) {
    gdp_override$gdp_log_2019[match(node_order, gdp_override$iso3)]
  } else {
    nd$gdp_log
  }
  network::set.vertex.attribute(net, "gdp_log",
    as.numeric(tidyr::replace_na(gdp_vals, 0)))
  network::set.vertex.attribute(net, "patents_log",
    as.numeric(tidyr::replace_na(
      igraph::vertex_attr(g, "patents_log"), 0
    )))
  network::set.vertex.attribute(net, "iso3",
    as.character(tidyr::replace_na(node_order, "UNK")))

  # Dyadic UNGA similarity matrix — padded to graph node set.
  # Uses the shared dyad_unga (penalised formula, votes 2017–2019) for all models.
  unga_mat <- matrix(0,
    nrow = length(node_order), ncol = length(node_order),
    dimnames = list(node_order, node_order)
  )
  dyad_sub <- dyad_unga |>
    filter(iso3_i %in% node_order, iso3_j %in% node_order)
  for (i in seq_len(nrow(dyad_sub))) {
    unga_mat[dyad_sub$iso3_i[i], dyad_sub$iso3_j[i]] <- dyad_sub$unga_sim[i]
  }

  # Geographic distance matrix — reordered to match node_order
  dist_mat <- dist_mat_log[node_order, node_order]

  list(net = net, unga_mat = unga_mat, dist_mat = dist_mat)
}

# =============================================================================
# 4. Prepare network objects
# =============================================================================

message("Converting igraph objects to network objects ...")

# Standard (ITA-based) — used for BE M1–M3 and layer comparisons
prep_be_22 <- igraph_to_network(g_be_22,      node_geo, dyad_unga, dist_matrix_log, "Backend",  2022)
prep_fe_22 <- igraph_to_network(g_fe_22,      node_geo, dyad_unga, dist_matrix_log, "Frontend", 2022)

# ERGM-specific (BTIGE-based) — used only for temporal comparison
# 2022 temporal — uses 2022 GDP (default; no override needed)
prep_be_22_ergm <- igraph_to_network(g_be_22_ergm, node_geo, dyad_unga,
                                      dist_matrix_log, "Backend", 2022)
# 2019 temporal — uses 2019 GDP to match the network year
prep_be_19_ergm <- igraph_to_network(g_be_19_ergm, node_geo, dyad_unga,
                                      dist_matrix_log, "Backend", 2019,
                                      gdp_override = gdp_2019)

message("Network objects ready.")

# Density check — high density signals potential non-identification
for (item in list(
  list("BE 2022 (ITA)",        prep_be_22$net),
  list("FE 2022",              prep_fe_22$net),
  list("BE 2022 (BTIGE ergm)", prep_be_22_ergm$net),
  list("BE 2019 (BTIGE ergm)", prep_be_19_ergm$net)
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
#  For each layer × year: M1 (structural), M2 (economic + gravity), M3 (full).
#
#  Layer comparison table uses M3 for both BE and FE 2022.
#  Temporal comparison table uses M3 for BE 2019 and BE 2022 (BTIGE).
#  FE 2019 is not modelled: Taiwan absent from front-end 2019 network
#  due to OECD BTIGE coverage limitations.
# =============================================================================

# ── BE 2022 (ITA-based) ───────────────────────────────────────────────────────

# M1 — structural baseline: edges + mutual
message("\n=== Fitting BE 2022 M1 — structural baseline ===")
ergm_be22_m1 <- ergm::ergm(
  prep_be_22$net ~ edges + mutual,
  control = ergm_ctrl
)

# M2 — economic + gravity: edges + mutual + nodecov(rca) + nodecov(gdp) + nodecov(patents) + edgecov(dist)
message("\n=== Fitting BE 2022 M2 — economic + gravity ===")
ergm_be22_m2 <- ergm::ergm(
  prep_be_22$net ~ edges + mutual +
    nodecov("rca_log") +
    nodecov("gdp_log") +
    nodecov("patents_log") +
    edgecov(prep_be_22$dist_mat),
  control = ergm_ctrl
)

# M3 — full model: M2 + edgecov(unga)
message("\n=== Fitting BE 2022 M3 — full model ===")
ergm_be22_m3 <- ergm::ergm(
  prep_be_22$net ~ edges + mutual +
    nodecov("rca_log") +
    nodecov("gdp_log") +
    nodecov("patents_log") +
    edgecov(prep_be_22$dist_mat) +
    edgecov(prep_be_22$unga_mat),
  control = ergm_ctrl
)

# ── FE 2022 (ITA-based) ───────────────────────────────────────────────────────

# M1 — structural baseline: edges + mutual
message("\n=== Fitting FE 2022 M1 — structural baseline ===")
ergm_fe22_m1 <- ergm::ergm(
  prep_fe_22$net ~ edges + mutual,
  control = ergm_ctrl
)

# M2 — economic + gravity: edges + mutual + nodecov(rca) + nodecov(gdp) + nodecov(patents) + edgecov(dist)
message("\n=== Fitting FE 2022 M2 — economic + gravity ===")
ergm_fe22_m2 <- ergm::ergm(
  prep_fe_22$net ~ edges + mutual +
    nodecov("rca_log") +
    nodecov("gdp_log") +
    nodecov("patents_log") +
    edgecov(prep_fe_22$dist_mat),
  control = ergm_ctrl
)

# M3 — full model: M2 + edgecov(unga)
message("\n=== Fitting FE 2022 M3 — full model ===")
ergm_fe22_m3 <- ergm::ergm(
  prep_fe_22$net ~ edges + mutual +
    nodecov("rca_log") +
    nodecov("gdp_log") +
    nodecov("patents_log") +
    edgecov(prep_fe_22$dist_mat) +
    edgecov(prep_fe_22$unga_mat),
  control = ergm_ctrl
)

# ── BE 2019 — temporal comparison (BTIGE-consistent) ─────────────────────────

# M1 — structural baseline: edges + mutual
message("\n=== Fitting BE 2019 M1 — structural baseline (BTIGE Taiwan) ===")
ergm_be19_m1_ergm <- ergm::ergm(
  prep_be_19_ergm$net ~ edges + mutual,
  control = ergm_ctrl
)

# M2 — economic + gravity: edges + mutual + nodecov(rca) + nodecov(gdp) + nodecov(patents) + edgecov(dist)
message("\n=== Fitting BE 2019 M2 — economic + gravity (BTIGE Taiwan) ===")
ergm_be19_m2_ergm <- ergm::ergm(
  prep_be_19_ergm$net ~ edges + mutual +
    nodecov("rca_log") +
    nodecov("gdp_log") +
    nodecov("patents_log") +
    edgecov(prep_be_19_ergm$dist_mat),
  control = ergm_ctrl
)

# M3 — full model: M2 + edgecov(unga)
message("\n=== Fitting BE 2019 M3 — full model (BTIGE Taiwan) ===")
ergm_be19_m3_ergm <- ergm::ergm(
  prep_be_19_ergm$net ~ edges + mutual +
    nodecov("rca_log") +
    nodecov("gdp_log") +
    nodecov("patents_log") +
    edgecov(prep_be_19_ergm$dist_mat) +
    edgecov(prep_be_19_ergm$unga_mat),
  control = ergm_ctrl
)

# ── BE 2022 — temporal comparison (BTIGE-consistent) ─────────────────────────

# M1 — structural baseline: edges + mutual
message("\n=== Fitting BE 2022 M1 — structural baseline (BTIGE Taiwan) ===")
ergm_be22_m1_ergm <- ergm::ergm(
  prep_be_22_ergm$net ~ edges + mutual,
  control = ergm_ctrl
)

# M2 — economic + gravity: edges + mutual + nodecov(rca) + nodecov(gdp) + nodecov(patents) + edgecov(dist)
message("\n=== Fitting BE 2022 M2 — economic + gravity (BTIGE Taiwan) ===")
ergm_be22_m2_ergm <- ergm::ergm(
  prep_be_22_ergm$net ~ edges + mutual +
    nodecov("rca_log") +
    nodecov("gdp_log") +
    nodecov("patents_log") +
    edgecov(prep_be_22_ergm$dist_mat),
  control = ergm_ctrl
)

# M3 — full model: M2 + edgecov(unga)
message("\n=== Fitting BE 2022 M3 — full model (BTIGE Taiwan) ===")
ergm_be22_m3_ergm <- ergm::ergm(
  prep_be_22_ergm$net ~ edges + mutual +
    nodecov("rca_log") +
    nodecov("gdp_log") +
    nodecov("patents_log") +
    edgecov(prep_be_22_ergm$dist_mat) +
    edgecov(prep_be_22_ergm$unga_mat),
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
ergm::mcmc.diagnostics(ergm_be22_m3)
ergm::mcmc.diagnostics(ergm_fe22_m3)
ergm::mcmc.diagnostics(ergm_be22_m3_ergm)
ergm::mcmc.diagnostics(ergm_be19_m3_ergm)

# =============================================================================
# 8. Goodness of fit
#
#  Checks whether models reproduce structural features not explicitly modelled:
#  degree distribution, geodesic distance, edgewise shared partners.
# =============================================================================

message("\nComputing goodness of fit ...")

gof_be22 <- ergm::gof(ergm_be22_m3,
                      GOF     = ~ idegree + odegree + distance + espartners,
                      control = ergm::control.gof.ergm(seed = 42))

pdf(file.path(DIRS$figures, "fig_ergm_gof_be.pdf"), width = 10, height = 8)
plot(gof_be22, main = "GoF — BE 2022 M3")
dev.off()

gof_fe22 <- ergm::gof(ergm_fe22_m3,
                      GOF     = ~ idegree + odegree + distance + espartners,
                      control = ergm::control.gof.ergm(seed = 42))

pdf(file.path(DIRS$figures, "fig_ergm_gof_fe.pdf"), width = 10, height = 8)
plot(gof_fe22, main = "GoF — FE 2022 M3")
dev.off()

message("Saved: fig_ergm_gof_be.pdf and fig_ergm_gof_fe.pdf")

# GoF for temporal comparison models (BTIGE-based)
gof_be19_ergm <- ergm::gof(ergm_be19_m3_ergm,
                           GOF     = ~ idegree + odegree + distance + espartners,
                           control = ergm::control.gof.ergm(seed = 42))

pdf(file.path(DIRS$figures, "fig_ergm_gof_be19.pdf"), width = 10, height = 8)
plot(gof_be19_ergm, main = "GoF — BE 2019 M3 (BTIGE)")
dev.off()

gof_be22_ergm <- ergm::gof(ergm_be22_m3_ergm,
                           GOF     = ~ idegree + odegree + distance + espartners,
                           control = ergm::control.gof.ergm(seed = 42))

pdf(file.path(DIRS$figures, "fig_ergm_gof_be22_btige.pdf"), width = 10, height = 8)
plot(gof_be22_ergm, main = "GoF — BE 2022 M3 (BTIGE)")
dev.off()

message("Saved: fig_ergm_gof_be19.pdf and fig_ergm_gof_be22_btige.pdf")

# =============================================================================
# 9. Results tables — texreg (has a native extract.ergm method)
# =============================================================================

library(texreg)

# ── Coefficient name map (covers all possible term names across models) ────────
ergm_coef_names <- c(
  "edges"               = "Edges (baseline density)",
  "mutual"              = "Mutual (reciprocity)",
  "nodecov.rca_log"     = "RCA (log)",
  "nodecov.gdp_log"     = "GDP (log)",
  "nodecov.patents_log" = "Patents (log)"
)

# Build the custom.coef.names vector for a list of models:
# texreg needs names for every term that appears, including the dynamic edgecov.
make_coef_names <- function(model_list) {
  all_terms <- unique(unlist(lapply(model_list, function(m) names(coef(m)))))
  nms <- ifelse(
    all_terms %in% names(ergm_coef_names),
    ergm_coef_names[all_terms],
    ifelse(grepl("unga_mat",  all_terms), "UNGA similarity (dyadic)",
    ifelse(grepl("dist_mat",  all_terms), "Geographic distance (log km)",
           all_terms))
  )
  setNames(nms, all_terms)
}

# ── Shared texreg arguments ────────────────────────────────────────────────────
texreg_common <- list(
  stars        = c(0.001, 0.01, 0.05),
  symbol       = "*",
  booktabs     = TRUE,
  use.packages = FALSE,     # do not emit \usepackage{} lines
  dcolumn      = FALSE,
  include.nobs = TRUE,
  include.aic  = TRUE,
  include.bic  = TRUE,
  digits       = 3,
  float.pos    = "H"      
)

# ── TABLE A: Backend 2022 — M1, M2, M3 ───────────────────────────────────────
models_a <- list(
  "BE-M1 (Structural)" = ergm_be22_m1,
  "BE-M2"    = ergm_be22_m2,
  "BE-M3 (Full)"       = ergm_be22_m3
)
do.call(texreg::texreg, c(
  list(
    l                  = models_a,
    file               = file.path(DIRS$tables, "table_ergm_backend.tex"),
    caption            = "ERGM Results: Back-end Layer (2022) — Structural, Gravity, and Full Models",
    label              = "tab:ergm-backend",
    custom.coef.names  = make_coef_names(models_a),
    custom.note        = paste(
      "Standard errors in parentheses. %stars.",
      "M1: structural baseline. M2: adds RCA (log), GDP (log), patents (log), and geographic distance.",
      "M3: adds UNGA voting similarity.",
      "Trade data: UN Comtrade; Taiwan ITA.",
      "GDP: World Bank WDI.",
      "Patents: OECD patent database.",
      "Geographic distance: CEPII GeoDist.",
      "UNGA voting similarity: United Nations General Assembly Voting Data.",
      "Author's calculations."
    )
  ),
  texreg_common
))
message("Saved: ", file.path(DIRS$tables, "table_ergm_backend.tex"))

# ── TABLE B: Layer comparison — BE M3 vs FE M3 (2022) ────────────────────────
models_b <- list(
  "Back-end M3 (2022)"  = ergm_be22_m3,
  "Front-end M3 (2022)" = ergm_fe22_m3
)
do.call(texreg::texreg, c(
  list(
    l                  = models_b,
    file               = file.path(DIRS$tables, "table_ergm_layer.tex"),
    caption            = "ERGM Results: Layer Comparison — Full Model (2022)",
    label              = "tab:ergm-layer",
    custom.coef.names  = make_coef_names(models_b),
    custom.note        = paste(
      "Standard errors in parentheses. %stars.",
      "Identical M3 specification enables direct cross-layer coefficient comparison.",
      "Sources: UN Comtrade; Taiwan ITA. Author's calculations."
    )
  ),
  texreg_common
))
message("Saved: ", file.path(DIRS$tables, "table_ergm_layer.tex"))

# ── TABLE C: Temporal comparison — BE 2019 M3 vs BE 2022 M3 (BTIGE-consistent)
models_c <- list(
  "2019 M3 (baseline)" = ergm_be19_m3_ergm,
  "2022 M3" = ergm_be22_m3_ergm
)
do.call(texreg::texreg, c(
  list(
    l                  = models_c,
    file               = file.path(DIRS$tables, "table_ergm_temporal.tex"),
    caption            = "Back-end ERGM: Temporal Comparison (2019 vs.\\ 2022)",
    label              = "tab:ergm-temporal",
    custom.coef.names  = make_coef_names(models_c),
    custom.note        = paste(
      "Standard errors in parentheses. %stars.",
      "Taiwan back-end edges use OECD BTIGE (CPA C261+C265) in both years",
      "for measurement consistency; ITA HS6 data is 2022-only and is excluded here.",
      "Front-end temporal comparison omitted: Taiwan absent from 2019 front-end network."
    )
  ),
  texreg_common
))
message("Saved: ", file.path(DIRS$tables, "table_ergm_temporal.tex"))

message("\n12_ergm.R complete — 3 ERGM tables written to ", DIRS$tables)
