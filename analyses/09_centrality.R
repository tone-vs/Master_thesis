# analyses/09_centrality.R — Centrality and Core-Periphery Analysis
#
# Computes four centrality measures plus k-core decomposition for all nodes
# in all four layer × year graphs. Saves the combined data frame as an RDS
# (for downstream scripts) and LaTeX tables for the thesis.
#
# Measures:
#   degree_in / degree_out  — in- and out-degree (unweighted)
#   strength_in / out       — weighted degree using weight_marketshare
#   betweenness             — weighted betweenness (normalised); weights
#                             inverted so strong trade = short distance
#   eigenvector             — eigenvector centrality (directed, weighted)
#   coreness                — k-core shell index (igraph::coreness())
#
# Core-periphery approach:
#   igraph::coreness() performs k-core decomposition on the symmetrised graph
#   (directed → undirected, edge weights summed). The maximum coreness shell
#   defines the "core"; all other nodes are "periphery". Norway's shell index,
#   its rank relative to all network members, and whether it is classified as
#   core are reported.
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
#   data/processed/centrality_all.rds               — full centrality data frame
#                                                      (includes coreness column)
#   analyses/output/table_centrality_fe22.tex        — frontend 2022 rankings
#   analyses/output/table_centrality_be22.tex        — backend 2022 rankings
#   analyses/output/table_centrality_norway.tex      — Norway centrality, all networks
#   analyses/output/table_core_periphery_norway.tex  — Norway core-periphery position
#   analyses/output/table_core_periphery_full.tex    — all nodes, 2022 networks
#
# Run from project root: Rscript analyses/09_centrality.R

library(igraph)      # loaded for class dispatch; all calls use igraph:: prefix
library(dplyr)
library(countrycode)
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
#
# coreness is computed on the symmetrised (undirected) graph so that the
# k-core shell index reflects overall embeddedness, not directionality.
# igraph::as.undirected(mode = "collapse") sums parallel edge weights.

make_cent <- function(g, layer_label, yr) {

  # Symmetrise for coreness (k-core is defined on undirected graphs)
  g_ud  <- igraph::as.undirected(
    g,
    mode           = "collapse",
    edge.attr.comb = list(weight_marketshare = "sum", "ignore")
  )
  cores <- igraph::coreness(g_ud)                # named integer vector

  tibble(
    iso3         = igraph::V(g)$name,
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
    )$vector,
    coreness     = as.integer(cores[igraph::V(g)$name])
  ) |>
    mutate(
      country      = countrycode::countrycode(
        iso3, "iso3c", "country.name",
        custom_match = c(TWN = "Taiwan", NOR = "Norway")
      ),
      layer        = layer_label,
      year         = yr,
      is_norway    = iso3 == FOCAL_COUNTRY,
      # Core classification: node is "core" if it is in the maximum k-shell
      core_max     = max(coreness),
      is_core      = coreness == core_max,
      coreness_pct = round(100 * coreness / core_max, 1)   # % of max shell
    ) |>
    select(-core_max)   # drop helper column; keep is_core and coreness_pct
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

# Norway core-periphery summary (console)
message("\nNorway core-periphery position (k-core decomposition):")
centrality_all |>
  filter(iso3 == FOCAL_COUNTRY) |>
  select(layer, year, coreness, coreness_pct, is_core) |>
  print()

message("\nNetwork max k-shell and number of core nodes:")
centrality_all |>
  group_by(layer, year) |>
  summarise(
    n_nodes    = n(),
    max_shell  = max(coreness),
    n_core     = sum(is_core),
    nor_shell  = coreness[iso3 == FOCAL_COUNTRY],
    nor_pct    = coreness_pct[iso3 == FOCAL_COUNTRY],
    nor_is_core = is_core[iso3 == FOCAL_COUNTRY],
    .groups = "drop"
  ) |>
  print()

# ── Save combined RDS (consumed by 11_multiplex.R) ───────────────────────────
saveRDS(centrality_all, file.path("data/processed", "centrality_all.rds"))
message("Saved: data/processed/centrality_all.rds")

# ── Helper: write a LaTeX table to analyses/output/ ──────────────────────────
write_tex <- function(tbl, path, caption, label) {
  tex <- knitr::kable(
    tbl,
    format   = "latex",
    booktabs = TRUE,
    digits   = 4,
    caption  = caption,
    label    = label,
    linesep  = ""
  )
  writeLines(as.character(tex), path)
  message("Saved: ", path)
}

# ── Helper: format a centrality data frame for a LaTeX rankings table ─────────
format_cent_table <- function(cent_df) {
  cent_df |>
    arrange(desc(strength_out)) |>
    mutate(across(c(strength_in, strength_out, betweenness, eigenvector),
                  ~round(.x, 4))) |>
    select(
      Country      = iso3,
      `In-deg`     = degree_in,
      `Out-deg`    = degree_out,
      `Str. in`    = strength_in,
      `Str. out`   = strength_out,
      Betweenness  = betweenness,
      Eigenvector  = eigenvector
    )
}

# ── TABLE: Frontend 2022 centrality rankings ──────────────────────────────────
write_tex(
  format_cent_table(cent_fe_22),
  path    = "analyses/output/table_centrality_fe22.tex",
  caption = paste0(
    "Frontend layer (2022) centrality rankings, sorted by out-strength. ",
    "Strength = weighted degree (market-share weights). ",
    "Betweenness normalised; weights inverted so strong ties = short distances. ",
    "Norway (NOR) highlighted."
  ),
  label   = "tab:centrality-fe22"
)

# ── TABLE: Backend 2022 centrality rankings ───────────────────────────────────
write_tex(
  format_cent_table(cent_be_22),
  path    = "analyses/output/table_centrality_be22.tex",
  caption = "Backend layer (2022) centrality rankings, sorted by out-strength. See Table~\\ref{tab:centrality-fe22} for column definitions.",
  label   = "tab:centrality-be22"
)

# ── TABLE: Norway across all layer-year combinations ─────────────────────────
norway_cent <- centrality_all |>
  filter(iso3 == FOCAL_COUNTRY) |>
  mutate(across(c(strength_in, strength_out, betweenness, eigenvector),
                ~round(.x, 5))) |>
  arrange(layer, year) |>
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

write_tex(
  norway_cent,
  path    = "analyses/output/table_centrality_norway.tex",
  caption = paste0(
    "Norway's centrality scores across all layer--year combinations. ",
    "Strength weighted by bilateral market share. ",
    "Betweenness normalised to $[0,1]$."
  ),
  label   = "tab:centrality-norway"
)

# =============================================================================
# Core-periphery tables
# =============================================================================

# ── TABLE: Norway's core-periphery position across all networks ───────────────
#
# Shows Norway's k-shell index, the maximum shell in that network,
# Norway's shell as a percentage of the max, and whether Norway is
# classified as a core member.

norway_cp <- centrality_all |>
  group_by(layer, year) |>
  mutate(
    n_nodes   = n(),
    max_shell = max(coreness),
    n_core    = sum(is_core)
  ) |>
  ungroup() |>
  filter(iso3 == FOCAL_COUNTRY) |>
  arrange(layer, year) |>
  select(
    Layer          = layer,
    Year           = year,
    `k-shell`      = coreness,
    `Max shell`    = max_shell,
    `Shell %`      = coreness_pct,
    `Core?`        = is_core,
    `N core nodes` = n_core,
    `N nodes`      = n_nodes
  ) |>
  mutate(`Core?` = if_else(`Core?`, "Yes", "No"))

write_tex(
  norway_cp,
  path    = "analyses/output/table_core_periphery_norway.tex",
  caption = paste0(
    "Norway's structural position in the k-core decomposition of each ",
    "semiconductor trade network. ",
    "\\textit{k-shell} = Norway's coreness index; ",
    "\\textit{Max shell} = highest k-shell in the network (defines the core); ",
    "\\textit{Shell \\%} = Norway's shell as a percentage of the maximum; ",
    "\\textit{Core?} = whether Norway is in the maximum k-shell. ",
    "Coreness computed on the symmetrised (undirected) graph."
  ),
  label   = "tab:core-periphery-norway"
)

# ── TABLE: Full core-periphery rankings — 2022 networks ───────────────────────
#
# All nodes, both 2022 layers, sorted by coreness descending.
# Gives the thesis a complete cross-reference for the Norway position.

cp_2022 <- centrality_all |>
  filter(year == 2022) |>
  arrange(layer, desc(coreness), desc(strength_out)) |>
  mutate(
    `Core?`   = if_else(is_core, "Core", "Periphery"),
    coreness  = as.integer(coreness)
  ) |>
  select(
    Layer      = layer,
    ISO3       = iso3,
    Country    = country,
    `k-shell`  = coreness,
    `Shell %`  = coreness_pct,
    `Core?`,
    `Str. out` = strength_out,
    Eigenvector = eigenvector
  ) |>
  mutate(across(c(`Str. out`, Eigenvector), ~round(.x, 4)))

write_tex(
  cp_2022,
  path    = "analyses/output/table_core_periphery_full.tex",
  caption = paste0(
    "K-core decomposition of the front-end and back-end semiconductor trade networks (2022). ",
    "Nodes classified as \\textit{Core} if they belong to the maximum k-shell. ",
    "Sorted by layer, k-shell (descending), then out-strength."
  ),
  label   = "tab:core-periphery-full"
)

message("\n09_centrality.R complete.")
message("Next: run analyses/10_community_detection.R")
