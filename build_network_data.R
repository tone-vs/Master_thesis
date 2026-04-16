# =============================================================================
# build_network_data.R — Combine trade sources into analysis-ready network data
# Project: Mapping Norway in the Global Semiconductor Value Chain
# =============================================================================
# Run order:
#   1. semiconductor_bilateral_pull.R   → data/semiconductor/semiconductor_network.csv
#   2. taiwan_data.R                    → data/processed/taiwan_full.csv
#   3. patent_data.R                    → patents_avg (tibble in environment)
#   4. THIS SCRIPT
#
# Outputs:
#   data/processed/edges_frontend.csv      — Layer 1 edge list
#   data/processed/edges_backend.csv       — Layer 2 edge list
#   data/processed/node_attributes.csv     — Node attribute table (with patents)
#   data/processed/graph_frontend.rds      — igraph object, Layer 1
#   data/processed/graph_backend.rds       — igraph object, Layer 2
#
# Edge weight strategy (two weights, used for different analyses):
#   weight_binary       — 1 if edge exists; used for Louvain community detection
#                         and as ERGM outcome variable. Captures network topology
#                         only (who trades with whom).
#   weight_marketshare  — sum of (bilateral flow / world total) across HS codes
#                         within a layer. Removes product price differences:
#                         a billion-dollar ASML shipment and a silicon wafer
#                         export are both expressed as shares of their respective
#                         global markets before summing. Used for strength
#                         centrality and edge thickness in visualisations.
#   trade_value_usd     — raw summed USD value; retained for diagnostics and
#                         as a covariate in ERGM (log-transformed).
#
# Layer definitions (canonical, used throughout all downstream scripts):
#   "layer1_frontend" — silicon wafers, fab equipment, measurement instruments
#                       HS: 381800, 848610-848690, 903082
#   "layer2_backend"  — packaged chips, ICs, memory, logic, smart cards
#                       HS: 854110-854290, 852351-852359
#
# Taiwan note:
#   Taiwan does not report to UN Comtrade. Flows are sourced from OECD BTIGE
#   (CPA codes C261 + C265, aggregated). Both codes are assigned to
#   layer2_backend because Taiwan's dominant semiconductor export role is
#   finished chips (TSMC, memory). CPA C265 (instruments) would ideally belong
#   to layer1_frontend but cannot be separated post-aggregation in taiwan_full.
#   This is documented as a known limitation in the thesis.
# =============================================================================

library(dplyr)
library(tidyr)
library(readr)
library(igraph)

# -----------------------------------------------------------------------------
# SETTINGS — match bilateral pull and taiwan scripts
# -----------------------------------------------------------------------------

MIN_FLOW <- 1e6   # $1M threshold: noise filter (OECD 2025; Amador & Cabral 2016)
OUT_DIR  <- "data/processed"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)


# -----------------------------------------------------------------------------
# 0. Source patent data
#    Provides: patents_avg (tibble with REF_AREA, patents, patents_share,
#              patents_log)
# -----------------------------------------------------------------------------

source("patent_data.R")

stopifnot(
  "patents_avg not found — check patent_data.R ran correctly" =
    exists("patents_avg") && nrow(patents_avg) > 0
)


# -----------------------------------------------------------------------------
# 1. Load Comtrade data
#    Produced by semiconductor_bilateral_pull.R
#    Columns: layer, hs_code, hs_desc, year, reporter_code, reporter,
#             partner_code, partner, trade_value_usd
#    Layer values: "layer1_frontend" | "layer2_backend"
# -----------------------------------------------------------------------------

comtrade_path <- "data/semiconductor/semiconductor_network.csv"

if (!file.exists(comtrade_path)) {
  stop("Comtrade data not found. Run semiconductor_bilateral_pull.R first.")
}

comtrade_raw <- read_csv(comtrade_path, show_col_types = FALSE)

message("Comtrade rows loaded: ", nrow(comtrade_raw))
message("Comtrade layers: ", paste(unique(comtrade_raw$layer), collapse = ", "))


# -----------------------------------------------------------------------------
# 2. Load Taiwan BTIGE data
#    Produced by taiwan_data.R
#    Columns: layer, hs_code, hs_desc, year, reporter_code, reporter,
#             partner_code, partner, trade_value_usd, source
#    Layer values: "manufacturing" (needs remapping — see header note)
# -----------------------------------------------------------------------------

taiwan_path <- "data/processed/taiwan_full.csv"

if (!file.exists(taiwan_path)) {
  stop("Taiwan data not found. Run taiwan_data.R first.")
}

taiwan_raw <- read_csv(taiwan_path, show_col_types = FALSE)

message("Taiwan BTIGE rows loaded: ", nrow(taiwan_raw))

# Remap Taiwan layer to canonical names used by bilateral pull
taiwan_harmonised <- taiwan_raw |>
  mutate(
    layer  = "layer2_backend",   # see header for justification
    source = "OECD_BTIGE"
  )


# -----------------------------------------------------------------------------
# 3. Combine sources
#    TWN rows in Comtrade are empty (Taiwan does not report to Comtrade), so
#    there is no duplication between Comtrade and BTIGE for Taiwan flows.
#    For all other countries: bilateral pull captures reporter → partner exports
#    only. Taiwan BTIGE captures TWN → partner (exports) and partner → TWN
#    (imports, reversed in taiwan_data.R) — no overlap.
# -----------------------------------------------------------------------------

edges_raw <- bind_rows(
  comtrade_raw      |> mutate(source = "Comtrade",  hs_code = as.character(hs_code)),
  taiwan_harmonised |> mutate(hs_code = as.character(hs_code))
) |>
  filter(
    !is.na(trade_value_usd),
    trade_value_usd >= MIN_FLOW,
    reporter_code != partner_code       # drop self-loops
  )

message("Combined rows after threshold filter: ", nrow(edges_raw))


# -----------------------------------------------------------------------------
# 4. Market-share normalisation at HS-code level
#
#    Problem: raw USD values conflate relationship intensity with product price.
#    A single ASML lithography machine shipment (HS 848620, NLD → TWN) is worth
#    billions and dominates any value-weighted edge, while Norway's silicon wafer
#    exports (HS 381800) look trivial — even if structurally essential.
#
#    Solution: for each HS code within each layer, divide each bilateral flow
#    by the total world trade in that product. This puts all HS codes on a
#    comparable scale before summing across codes within a layer.
#
#    For Taiwan BTIGE rows, hs_code = "BTIGE_C261_C265" (one aggregated code).
#    World total for this pseudo-code is computed from BTIGE rows only, so all
#    Taiwan flows are expressed relative to the Taiwan BTIGE total — consistent.
# -----------------------------------------------------------------------------

world_hs_totals <- edges_raw |>
  group_by(hs_code, layer) |>
  summarise(world_hs_total = sum(trade_value_usd, na.rm = TRUE), .groups = "drop")

edges_raw <- edges_raw |>
  left_join(world_hs_totals, by = c("hs_code", "layer")) |>
  mutate(flow_share = trade_value_usd / world_hs_total)


# -----------------------------------------------------------------------------
# 5. Aggregate: collapse HS codes → one directed edge per (from, to, layer)
#
#    Three weight columns are produced:
#      trade_value_usd    — raw summed USD; retained for diagnostics and as
#                           log-transformed covariate in ERGM
#      weight_marketshare — sum of HS-level flow shares; removes product-price
#                           confound; used for strength centrality and edge
#                           thickness in visualisations (Hidalgo et al. 2007)
#      weight_binary      — always 1; used for Louvain community detection and
#                           as ERGM binary outcome variable
# -----------------------------------------------------------------------------

edges_agg <- edges_raw |>
  group_by(layer, reporter_code, reporter, partner_code, partner) |>
  summarise(
    trade_value_usd    = sum(trade_value_usd, na.rm = TRUE),
    weight_marketshare = sum(flow_share,      na.rm = TRUE),
    source             = first(source),
    .groups            = "drop"
  ) |>
  filter(trade_value_usd >= MIN_FLOW) |>    # re-apply threshold after aggregation
  mutate(
    weight_binary = 1L,
    weight_log    = log1p(trade_value_usd), # log USD retained for reference
    from          = reporter_code,
    to            = partner_code
  )

message("\nEdges after aggregation:")
message("  layer1_frontend: ", sum(edges_agg$layer == "layer1_frontend"))
message("  layer2_backend:  ", sum(edges_agg$layer == "layer2_backend"))


# -----------------------------------------------------------------------------
# 6. Split by layer
# -----------------------------------------------------------------------------

edges_frontend <- edges_agg |> filter(layer == "layer1_frontend")
edges_backend  <- edges_agg |> filter(layer == "layer2_backend")


# -----------------------------------------------------------------------------
# 7. Node attribute table
#    One row per country appearing as reporter OR partner in either layer.
#    Attributes: patent counts (OECD WIPO, 3-year avg 2020-2022, log-normalised)
#    Norway flagged explicitly for downstream filtering.
# -----------------------------------------------------------------------------

all_nodes <- bind_rows(
  edges_agg |> select(iso3 = from, name = reporter),
  edges_agg |> select(iso3 = to,   name = partner)
) |>
  distinct(iso3, .keep_all = TRUE) |>
  arrange(iso3)

message("\nTotal unique countries in network: ", nrow(all_nodes))

# Per-layer degree centrality (simple counts — igraph will compute full
# centrality measures in the analysis script, these are for quick inspection)
degree_attrs <- bind_rows(
  edges_frontend |>
    group_by(iso3 = from) |> summarise(out_deg_fe = n(), .groups = "drop"),
  edges_frontend |>
    group_by(iso3 = to)   |> summarise(in_deg_fe  = n(), .groups = "drop"),
  edges_backend |>
    group_by(iso3 = from) |> summarise(out_deg_be = n(), .groups = "drop"),
  edges_backend |>
    group_by(iso3 = to)   |> summarise(in_deg_be  = n(), .groups = "drop")
) |>
  group_by(iso3) |>
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)), .groups = "drop")

nodes <- all_nodes |>
  left_join(degree_attrs, by = "iso3") |>
  left_join(
    patents_avg |> select(REF_AREA, patents, patents_share, patents_log),
    by = c("iso3" = "REF_AREA")
  ) |>
  mutate(
    across(starts_with("out_deg") | starts_with("in_deg"),
           \(x) replace_na(x, 0)),
    patents      = replace_na(patents, 0),
    patents_log  = replace_na(patents_log, 0),
    is_focal     = iso3 == "NOR"          # Norway flag for analysis scripts
  )


# -----------------------------------------------------------------------------
# 8. Norway diagnostic — check before building graphs
# -----------------------------------------------------------------------------

message("\n========================================")
message("NORWAY DIAGNOSTIC")
message("========================================")

norway_fe <- edges_frontend |>
  filter(from == "NOR" | to == "NOR") |>
  arrange(desc(trade_value_usd))

norway_be <- edges_backend |>
  filter(from == "NOR" | to == "NOR") |>
  arrange(desc(trade_value_usd))

message("Norway edges in layer1_frontend: ", nrow(norway_fe))
if (nrow(norway_fe) > 0) {
  message("  As exporter (out): ", sum(norway_fe$from == "NOR"))
  message("  As importer (in):  ", sum(norway_fe$to   == "NOR"))
  message("  Top partners (trade_value_usd | weight_marketshare):")
  norway_fe |>
    mutate(direction = if_else(from == "NOR", "→", "←"),
           partner   = if_else(from == "NOR", to, from)) |>
    select(direction, partner, trade_value_usd, weight_marketshare) |>
    head(8) |>
    print()
}

message("\nNorway edges in layer2_backend: ", nrow(norway_be))
if (nrow(norway_be) > 0) {
  message("  As exporter (out): ", sum(norway_be$from == "NOR"))
  message("  As importer (in):  ", sum(norway_be$to   == "NOR"))
}

message("\nNorway node attributes:")
nodes |> filter(is_focal) |> glimpse()


# -----------------------------------------------------------------------------
# 9. Build igraph objects
#    Directed graphs, one per layer. All three weight columns attached as edge
#    attributes so downstream scripts can select the appropriate weight without
#    rebuilding the graph object.
#
#    Usage guide:
#      E(g)$weight_binary       → Louvain, ERGM outcome
#      E(g)$weight_marketshare  → strength centrality, plot edge thickness
#      E(g)$weight_log          → ERGM continuous covariate (log USD)
# -----------------------------------------------------------------------------

build_graph <- function(edge_df, node_df, layer_label) {
  g <- graph_from_data_frame(
    d = edge_df |>
      select(from, to,
             trade_value_usd,
             weight_binary,
             weight_marketshare,
             weight_log,
             source, layer),
    directed = TRUE,
    vertices = node_df
  )
  g$layer   <- layer_label
  g$n_nodes <- vcount(g)
  g$n_edges <- ecount(g)
  message("\n", layer_label, " graph: ", vcount(g), " nodes, ", ecount(g), " edges")
  g
}

g_frontend <- build_graph(edges_frontend, nodes, "layer1_frontend")
g_backend  <- build_graph(edges_backend,  nodes, "layer2_backend")


# -----------------------------------------------------------------------------
# 10. Save outputs
# -----------------------------------------------------------------------------

write_csv(edges_frontend, file.path(OUT_DIR, "edges_frontend.csv"))
write_csv(edges_backend,  file.path(OUT_DIR, "edges_backend.csv"))
write_csv(nodes,          file.path(OUT_DIR, "node_attributes.csv"))
saveRDS(g_frontend,       file.path(OUT_DIR, "graph_frontend.rds"))
saveRDS(g_backend,        file.path(OUT_DIR, "graph_backend.rds"))

message("\n========================================")
message("OUTPUT SUMMARY")
message("========================================")
message("edges_frontend.csv     — ", nrow(edges_frontend), " edges")
message("edges_backend.csv      — ", nrow(edges_backend),  " edges")
message("node_attributes.csv    — ", nrow(nodes),          " countries")
message("graph_frontend.rds     — igraph, Layer 1")
message("graph_backend.rds      — igraph, Layer 2")
message("\nNext: source analysis script (centrality + Louvain + ERGM)")
message("========================================")