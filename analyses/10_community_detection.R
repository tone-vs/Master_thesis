# analyses/10_community_detection.R — Louvain Community Detection
#
# Runs the Louvain algorithm on each of the four layer × year networks.
# Louvain requires an undirected graph; directed graphs are symmetrised by
# collapsing antiparallel edges (summing market-share weights).
#
# igraph functions use the full igraph:: namespace throughout to prevent
# masking conflicts if statnet is loaded in the same session.
#
# Inputs:
#   data/processed/graph_frontend_2019.rds
#   data/processed/graph_frontend_2022.rds
#   data/processed/graph_backend_2019.rds
#   data/processed/graph_backend_2022.rds
#
# Outputs:
#   data/processed/communities.rds             — named list of igraph community objects
#   analyses/output/table_community_summary.tex — modularity + N communities by network
#   analyses/output/table_community_norway.tex  — Norway's community membership
#   analyses/output/table_community_2022.tex    — full community structure (2022)
#
# Run from project root: Rscript analyses/10_community_detection.R

library(igraph)       # loaded for class dispatch; all calls use igraph:: prefix
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)
library(countrycode)  # needed for vertex-name normalisation fallback

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

# ── Vertex-name normalisation ─────────────────────────────────────────────────
#
# Graphs built by the old pipeline store full country names ("Norway", "China")
# as V(g)$name instead of ISO3 codes. This helper converts them in-place so
# all membership lookups using FOCAL_COUNTRY ("NOR") work correctly.

normalise_vertex_names <- function(g) {
  raw <- igraph::V(g)$name
  if (all(nchar(raw) <= 3)) return(g)   # already ISO3 — nothing to do
  message("  [normalise] Converting full country names to ISO3 codes ...")
  converted <- countrycode::countrycode(
    raw,
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
  failed <- raw[is.na(converted)]
  if (length(failed) > 0)
    warning("normalise_vertex_names(): could not convert: ",
            paste(failed, collapse = ", "))
  igraph::V(g)$name <- ifelse(is.na(converted), raw, converted)
  g
}

g_fe_19 <- normalise_vertex_names(g_fe_19)
g_fe_22 <- normalise_vertex_names(g_fe_22)
g_be_19 <- normalise_vertex_names(g_be_19)
g_be_22 <- normalise_vertex_names(g_be_22)

message("Vertex names normalised.")

# ── Symmetrise directed → undirected ─────────────────────────────────────────
#
# Louvain is defined on undirected graphs. Antiparallel edges are collapsed
# by summing their weight_marketshare values; all other edge attributes are
# dropped ("ignore") to avoid ambiguity.

symmetrise <- function(g) {
  igraph::as.undirected(
    g,
    mode           = "collapse",
    edge.attr.comb = list(weight_marketshare = "sum", "ignore")
  )
}

g_fe_19_ud <- symmetrise(g_fe_19)
g_fe_22_ud <- symmetrise(g_fe_22)
g_be_19_ud <- symmetrise(g_be_19)
g_be_22_ud <- symmetrise(g_be_22)

# ── Run Louvain ───────────────────────────────────────────────────────────────
#
# set.seed() immediately before each call for reproducibility.
# weight = weight_marketshare so economically stronger ties have more influence
# on community assignment.

message("Running Louvain (seed = 42) ...")
set.seed(42); comm_fe_19 <- igraph::cluster_louvain(g_fe_19_ud, weights = igraph::E(g_fe_19_ud)$weight_marketshare)
set.seed(42); comm_fe_22 <- igraph::cluster_louvain(g_fe_22_ud, weights = igraph::E(g_fe_22_ud)$weight_marketshare)
set.seed(42); comm_be_19 <- igraph::cluster_louvain(g_be_19_ud, weights = igraph::E(g_be_19_ud)$weight_marketshare)
set.seed(42); comm_be_22 <- igraph::cluster_louvain(g_be_22_ud, weights = igraph::E(g_be_22_ud)$weight_marketshare)

message("Louvain complete.")

# Console summary
for (label_comm in list(
  list("FRONTEND 2019", comm_fe_19, g_fe_19),
  list("FRONTEND 2022", comm_fe_22, g_fe_22),
  list("BACKEND  2019", comm_be_19, g_be_19),
  list("BACKEND  2022", comm_be_22, g_be_22)
)) {
  lbl  <- label_comm[[1]]
  comm <- label_comm[[2]]
  g    <- label_comm[[3]]
  nor_comm <- igraph::membership(comm)[igraph::V(g)$name == FOCAL_COUNTRY]
  message(sprintf(
    "\n=== %s === communities: %d | modularity: %.3f | Norway in community %d",
    lbl, igraph::components(g, mode = "weak")$no,
    igraph::modularity(comm), nor_comm
  ))
  print(igraph::sizes(comm))
}

# ── Save community objects as RDS ─────────────────────────────────────────────
communities <- list(
  fe_2019 = comm_fe_19,
  fe_2022 = comm_fe_22,
  be_2019 = comm_be_19,
  be_2022 = comm_be_22
)

saveRDS(communities, file.path("data/processed", "communities.rds"))
message("\nSaved: data/processed/communities.rds")

# ── Helper: write a LaTeX table ───────────────────────────────────────────────
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

# ── TABLE 1 — High-level community summary (2022 primary; 2019 robustness) ────
#
# The primary LaTeX table reports 2022 only. A separate robustness table
# covering 2019 is written to the same directory for appendix use.
# The full communities list (including 2019) is saved to communities.rds
# so that plots/14_network_viz.R can render the 2019 robustness figures.

comm_summary_2022 <- tibble(
  Layer          = c("Frontend (L1)", "Backend (L2)"),
  Year           = c(2022, 2022),
  `N communities`= c(length(comm_fe_22), length(comm_be_22)),
  Modularity     = round(c(
    igraph::modularity(comm_fe_22),
    igraph::modularity(comm_be_22)
  ), 3),
  `Largest comm` = c(
    max(igraph::sizes(comm_fe_22)),
    max(igraph::sizes(comm_be_22))
  )
)

# Robustness: 2019 summary (for appendix)
comm_summary_2019 <- tibble(
  Layer          = c("Frontend (L1)", "Backend (L2)"),
  Year           = c(2019, 2019),
  `N communities`= c(length(comm_fe_19), length(comm_be_19)),
  Modularity     = round(c(
    igraph::modularity(comm_fe_19),
    igraph::modularity(comm_be_19)
  ), 3),
  `Largest comm` = c(
    max(igraph::sizes(comm_fe_19)),
    max(igraph::sizes(comm_be_19))
  )
)

comm_summary <- comm_summary_2022  # alias used in console print below

message("\nCommunity summary:")
print(comm_summary)

write_tex(
  comm_summary_2022,
  path    = "analyses/output/table_community_summary.tex",
  caption = paste0(
    "Louvain community detection results by layer (2022). ",
    "Weights = bilateral market share. Seed = 42. ",
    "2019 results (robustness check) in Table~\\ref{tab:community-summary-robustness}."
  ),
  label   = "tab:community-summary"
)

write_tex(
  comm_summary_2019,
  path    = "analyses/output/table_community_summary_robustness.tex",
  caption = paste0(
    "Louvain community detection results by layer (2019, robustness check). ",
    "Weights = bilateral market share. Seed = 42."
  ),
  label   = "tab:community-summary-robustness"
)

# ── TABLE 2 — Norway's community membership ───────────────────────────────────
norway_comm_id <- function(comm, g) {
  igraph::membership(comm)[igraph::V(g)$name == FOCAL_COUNTRY]
}
norway_comm_members <- function(comm, g) {
  nor_id  <- norway_comm_id(comm, g)
  members <- igraph::V(g)$name[igraph::membership(comm) == nor_id]
  paste(sort(members), collapse = ", ")
}

# Primary table: 2022 only
norway_community <- tibble(
  Layer              = c("Frontend (L1)", "Backend (L2)"),
  Year               = c(2022, 2022),
  `Norway community` = c(
    norway_comm_id(comm_fe_22, g_fe_22),
    norway_comm_id(comm_be_22, g_be_22)
  ),
  `Community size`   = c(
    sum(igraph::membership(comm_fe_22) == norway_comm_id(comm_fe_22, g_fe_22)),
    sum(igraph::membership(comm_be_22) == norway_comm_id(comm_be_22, g_be_22))
  ),
  `Community members` = c(
    norway_comm_members(comm_fe_22, g_fe_22),
    norway_comm_members(comm_be_22, g_be_22)
  )
)

# Robustness table: 2019
norway_community_2019 <- tibble(
  Layer              = c("Frontend (L1)", "Backend (L2)"),
  Year               = c(2019, 2019),
  `Norway community` = c(
    norway_comm_id(comm_fe_19, g_fe_19),
    norway_comm_id(comm_be_19, g_be_19)
  ),
  `Community size`   = c(
    sum(igraph::membership(comm_fe_19) == norway_comm_id(comm_fe_19, g_fe_19)),
    sum(igraph::membership(comm_be_19) == norway_comm_id(comm_be_19, g_be_19))
  ),
  `Community members` = c(
    norway_comm_members(comm_fe_19, g_fe_19),
    norway_comm_members(comm_be_19, g_be_19)
  )
)

message("\nNorway community membership:")
print(norway_community)

write_tex(
  norway_community,
  path    = "analyses/output/table_community_norway.tex",
  caption = paste0(
    "Norway's Louvain community membership by layer (2022). ",
    "2019 robustness results in Table~\\ref{tab:community-norway-robustness}."
  ),
  label   = "tab:community-norway"
)

write_tex(
  norway_community_2019,
  path    = "analyses/output/table_community_norway_robustness.tex",
  caption = "Norway's Louvain community membership by layer (2019, robustness check).",
  label   = "tab:community-norway-robustness"
)

# ── TABLE 3 — Full community structure (2022 only) ────────────────────────────
#
# Lists every detected community with its members for 2022 networks.
# This is the primary substantive table — community composition reveals
# geopolitical clustering (US-allied vs China-centric blocs).

make_comm_df <- function(comm, g, layer_label) {
  tibble(
    iso3      = igraph::V(g)$name,
    community = igraph::membership(comm)
  ) |>
    group_by(community) |>
    summarise(
      `N members` = n(),
      Members     = paste(sort(iso3), collapse = ", "),
      `Norway`    = if_else(any(iso3 == FOCAL_COUNTRY), "\\checkmark", ""),
      .groups     = "drop"
    ) |>
    mutate(
      Layer      = layer_label,
      Modularity = round(igraph::modularity(comm), 3)
    ) |>
    arrange(desc(`N members`)) |>
    select(Layer, Community = community,
           `N members`, Members, `Norway`, Modularity)
}

comm_2022 <- bind_rows(
  make_comm_df(comm_fe_22, g_fe_22, "Frontend (L1)"),
  make_comm_df(comm_be_22, g_be_22, "Backend (L2)")
)

message("\nFull community structure (2022):")
print(comm_2022)

write_tex(
  comm_2022,
  path    = "analyses/output/table_community_2022.tex",
  caption = paste0(
    "Full Louvain community structure (2022). ",
    "Communities sorted by size within each layer. ",
    "Modularity is network-level (same for all rows within a layer)."
  ),
  label   = "tab:community-2022"
)

message("\n10_community_detection.R complete.")
message("Next: run analyses/11_multiplex.R")
