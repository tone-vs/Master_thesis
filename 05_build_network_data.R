
# 05_build_network_data.R — Combine trade sources into analysis-ready network data

# Run order:
#   1. 01_country_selection.R   -> data/processed/country_selection.csv
#   2. 02_comtrade_pull.R       -> data/semiconductor/semiconductor_network.csv
#   3. 03_taiwan_data.R         -> data/processed/taiwan_full.csv
#   4. 04_patent_data.R         -> data/processed/patents_avg.csv
#   5. THIS SCRIPT
#
# Outputs (one set per year detected in the data):
#   data/processed/edges_frontend_{year}.csv
#   data/processed/edges_backend_{year}.csv
#   data/processed/graph_frontend_{year}.rds   — igraph, Layer 1
#   data/processed/graph_backend_{year}.rds    — igraph, Layer 2
#   data/processed/node_attributes.csv         — one row per country,
#                                                 degree columns wide by year,
#                                                 patents averaged over all years
#
# Edge weight strategy:
#   weight_binary       — 1 if edge exists; used for Louvain community detection
#                         and as ERGM outcome variable.
#   weight_marketshare  — sum of (bilateral flow / year world total) across HS
#                         codes. Normalised within each year so years are
#                         comparable despite price changes. Used for strength
#                         centrality and edge thickness in visualisations.
#   trade_value_usd     — raw summed USD; retained for diagnostics and as
#                         log-transformed covariate in ERGM.
#
# Layer definitions:
#   "layer1_frontend" — silicon wafers, fab equipment, measurement instruments
#                       HS: 381800, 848610-848690, 903082
#   "layer2_backend"  — packaged chips, ICs, memory, logic, smart cards
#                       HS: 854110-854290, 852351-852359
#
# Taiwan note:
#   Taiwan does not report to UN Comtrade. Backend flows from OECD BTIGE
#   (CPA C261+C265, both years). Frontend flows from Taiwan ITA customs
#   portal (2022 only) — 2019 frontend absence documented as limitation.


library(dplyr)
library(tidyr)
library(readr)
library(igraph)

source("config.R")

dir.create(DIRS$processed, recursive = TRUE, showWarnings = FALSE)



# 1. Load Comtrade data
#    Produced by 02_comtrade_pull.R — contains all years defined in YEARS.


comtrade_path <- file.path(DIRS$network, "semiconductor_network.csv")

if (!file.exists(comtrade_path)) {
  stop("Comtrade data not found. Run 02_comtrade_pull.R first.")
}

comtrade_raw <- read_csv(comtrade_path, show_col_types = FALSE)
message("Comtrade rows loaded: ", nrow(comtrade_raw))
message("Comtrade years:  ", paste(sort(unique(comtrade_raw$year)), collapse = ", "))
message("Comtrade layers: ", paste(unique(comtrade_raw$layer), collapse = ", "))



# 2. Load Taiwan data
#    Produced by 03_taiwan_data.R — BTIGE (backend, both years) + ITA (frontend, 2022).


taiwan_path <- file.path(DIRS$processed, "taiwan_full.csv")

if (!file.exists(taiwan_path)) {
  stop("Taiwan data not found. Run 03_taiwan_data.R first.")
}

taiwan_raw <- read_csv(taiwan_path, show_col_types = FALSE)
message("Taiwan rows loaded: ", nrow(taiwan_raw))
message("Taiwan years: ", paste(sort(unique(taiwan_raw$year)), collapse = ", "))

taiwan_harmonised <- taiwan_raw |>
  mutate(source = if_else(is.na(source), "OECD_BTIGE", source))
# layer is already set correctly in 03_taiwan_data.R — do not override



# 3. Combine sources


edges_raw <- bind_rows(
  comtrade_raw      |> mutate(source = "Comtrade", hs_code = as.character(hs_code)),
  taiwan_harmonised |> mutate(hs_code = as.character(hs_code))
) |>
  filter(
    !is.na(trade_value_usd),
    trade_value_usd >= MIN_FLOW,
    reporter_code   != partner_code    # drop self-loops
  )

# YEARS derived from the data — serves as a data integrity check
YEARS <- sort(unique(edges_raw$year))
message("\nYears detected in combined data: ", paste(YEARS, collapse = ", "))
message("Combined rows after threshold filter: ", nrow(edges_raw))



# 4. Per-year processing helper
#
#    Market-share normalisation is computed within each year so that edge
#    weights are comparable across years despite nominal price changes.
#    (A 2019 ASML shipment and a 2022 ASML shipment are both expressed as
#    a share of their respective year's world trade in that HS code.)


process_year <- function(yr, edges_raw) {

  edges_yr <- edges_raw |> filter(year == yr)

  # Market-share normalisation within this year
  world_hs_totals <- edges_yr |>
    group_by(hs_code, layer) |>
    summarise(world_hs_total = sum(trade_value_usd, na.rm = TRUE), .groups = "drop")

  edges_yr <- edges_yr |>
    left_join(world_hs_totals, by = c("hs_code", "layer")) |>
    mutate(flow_share = trade_value_usd / world_hs_total)

  # Aggregate: one directed edge per (layer, reporter, partner)
  edges_agg <- edges_yr |>
    group_by(layer, reporter_code, reporter, partner_code, partner) |>
    summarise(
      trade_value_usd    = sum(trade_value_usd, na.rm = TRUE),
      weight_marketshare = sum(flow_share,      na.rm = TRUE),
      source             = first(source),
      .groups            = "drop"
    ) |>
    filter(trade_value_usd >= MIN_FLOW) |>
    mutate(
      weight_binary = 1L,
      weight_log    = log1p(trade_value_usd),
      from          = reporter_code,
      to            = partner_code,
      year          = yr
    )

  list(
    frontend = edges_agg |> filter(layer == "layer1_frontend"),
    backend  = edges_agg |> filter(layer == "layer2_backend"),
    all      = edges_agg
  )
}



# 5. Run per-year processing


year_results <- lapply(YEARS, process_year, edges_raw = edges_raw)
names(year_results) <- as.character(YEARS)

for (yr in YEARS) {
  r <- year_results[[as.character(yr)]]
  message(sprintf("\n%d edges — layer1_frontend: %d | layer2_backend: %d",
                  yr, nrow(r$frontend), nrow(r$backend)))
}



# 6. All-nodes union
#    Union of all countries appearing in any year, any layer.
#    This ensures all graphs share a consistent vertex set.


all_nodes <- bind_rows(lapply(year_results, \(r) bind_rows(
  r$all |> select(iso3 = from, name = reporter),
  r$all |> select(iso3 = to,   name = partner)
))) |>
  distinct(iso3, .keep_all = TRUE) |>
  arrange(iso3)

message("\nTotal unique countries across all years: ", nrow(all_nodes))

# -----------------------------------------------------------------------------
# 6b. Revealed Comparative Advantage (RCA)
#     Formula: RCA_i = (X_semi_i / X_total_i) / (X_semi_world / X_total_world)
#     Source: OECD (2025) Box 1; Balassa (1965)
#     RCA > 1 = comparative advantage in this segment.
#     Denominator: total exports across all goods from Comtrade (commodity = TOTAL)
#     produced by 01_country_selection.R
# -----------------------------------------------------------------------------

total_exports_path <- file.path(DIRS$processed, "total_exports.csv")
if (!file.exists(total_exports_path)) {
  stop("total_exports.csv not found. Run 01_country_selection.R first.")
}
total_exports <- read_csv(total_exports_path, show_col_types = FALSE)

compute_rca <- function(edges_raw, total_exports, layer_name, yr) {
  
  # Semiconductor exports in this layer for this year
  segment_exports <- edges_raw |>
    filter(layer == layer_name, year == yr) |>
    group_by(reporter_code) |>
    summarise(segment_exports = sum(trade_value_usd, na.rm = TRUE),
              .groups = "drop")
  
  # Total exports across all goods for this year
  total_by_country <- total_exports |>
    filter(year == yr) |>
    group_by(reporter_code) |>
    summarise(total_exports = sum(total_exports, na.rm = TRUE),
              .groups = "drop")
  
  world_segment <- sum(segment_exports$segment_exports)
  world_total   <- sum(total_by_country$total_exports)
  
  segment_exports |>
    left_join(total_by_country, by = "reporter_code") |>
    mutate(
      rca     = (segment_exports / total_exports) / (world_segment / world_total),
      has_rca = rca > 1,
      layer   = layer_name,
      year    = yr
    ) |>
    rename(iso3 = reporter_code)
}

rca_all <- bind_rows(
  compute_rca(edges_raw, total_exports, "layer1_frontend", 2019),
  compute_rca(edges_raw, total_exports, "layer1_frontend", 2022),
  compute_rca(edges_raw, total_exports, "layer2_backend",  2019),
  compute_rca(edges_raw, total_exports, "layer2_backend",  2022)
)

rca_wide <- rca_all |>
  mutate(col = paste0("rca_",
                      if_else(layer == "layer1_frontend", "fe", "be"),
                      "_", year)) |>
  select(iso3, col, rca) |>
  pivot_wider(names_from = col, values_from = rca)

message("\nNorway RCA:")
rca_all |>
  filter(iso3 == "NOR") |>
  select(layer, year, rca, has_rca) |>
  print()

# 7. Load patent data
#    Produced by 04_patent_data.R — read from disk (no sourcing).
#    Window: 2019-2022 (4-year average, matches trade data).


patents_path <- file.path(DIRS$processed, "patents_avg.csv")

if (!file.exists(patents_path)) {
  stop("patents_avg.csv not found. Run 04_patent_data.R first.")
}

patents_avg <- read_csv(patents_path, show_col_types = FALSE)

stopifnot("patents_avg.csv is empty" = nrow(patents_avg) > 0)



# 8. Node attribute table
#    Degree counts are stored in wide format with _{year} suffixes so the
#    Rmd can compare structural positions across years without reshaping.


# Helper: degree counts for one year, returns wide columns named *_{year}
degree_for_year <- function(yr, year_results) {
  r   <- year_results[[as.character(yr)]]
  sfx <- paste0("_", yr)

  bind_rows(
    r$frontend |> group_by(iso3 = from) |> summarise("out_deg_fe{sfx}" := n(), .groups = "drop"),
    r$frontend |> group_by(iso3 = to)   |> summarise("in_deg_fe{sfx}"  := n(), .groups = "drop"),
    r$backend  |> group_by(iso3 = from) |> summarise("out_deg_be{sfx}" := n(), .groups = "drop"),
    r$backend  |> group_by(iso3 = to)   |> summarise("in_deg_be{sfx}"  := n(), .groups = "drop")
  ) |>
    group_by(iso3) |>
    summarise(across(everything(), \(x) sum(x, na.rm = TRUE)), .groups = "drop")
}

degree_attrs <- Reduce(
  \(a, b) full_join(a, b, by = "iso3"),
  lapply(YEARS, degree_for_year, year_results = year_results)
)

nodes <- all_nodes |>
  left_join(degree_attrs, by = "iso3") |>
  left_join(rca_wide,     by = "iso3") |>        
  left_join(
    patents_avg |> select(REF_AREA, patents, patents_share, patents_log),
    by = c("iso3" = "REF_AREA")
  ) |>
  mutate(
    across(starts_with("out_deg") | starts_with("in_deg"), \(x) replace_na(x, 0)),
    across(starts_with("rca_"),    \(x) replace_na(x, 0)), # <- add this
    patents     = replace_na(patents, 0),
    patents_log = replace_na(patents_log, 0),
    is_focal    = iso3 == FOCAL_COUNTRY
  )


# 9. Norway diagnostic (per year)


message("\n========================================")
message("NORWAY DIAGNOSTIC")
message("========================================")

for (yr in YEARS) {
  r <- year_results[[as.character(yr)]]
  nor_fe <- r$frontend |> filter(from == "NOR" | to == "NOR")
  nor_be <- r$backend  |> filter(from == "NOR" | to == "NOR")

  message(sprintf("\n--- %d ---", yr))
  message("Layer1 frontend edges: ", nrow(nor_fe),
          "  (out: ", sum(nor_fe$from == "NOR"),
          ", in: ",  sum(nor_fe$to   == "NOR"), ")")
  message("Layer2 backend edges:  ", nrow(nor_be),
          "  (out: ", sum(nor_be$from == "NOR"),
          ", in: ",  sum(nor_be$to   == "NOR"), ")")

  if (nrow(nor_fe) > 0) {
    nor_fe |>
      mutate(direction = if_else(from == "NOR", "->", "<-"),
             partner   = if_else(from == "NOR", to, from)) |>
      select(direction, partner, trade_value_usd, weight_marketshare) |>
      head(6) |>
      print()
  }
}

message("\nNorway node attributes:")
nodes |> filter(is_focal) |> glimpse()



# 10. Build igraph objects (one per layer per year)
#
#    All graphs share the same vertex set (all_nodes union) so community
#    membership and centrality scores can be compared across years directly.
#
#    Usage guide:
#      E(g)$weight_binary       -> Louvain, ERGM outcome
#      E(g)$weight_marketshare  -> strength centrality, edge thickness in plots
#      E(g)$weight_log          -> ERGM continuous covariate (log USD)


build_graph <- function(edge_df, node_df, layer_label, yr) {
  g <- graph_from_data_frame(
    d = edge_df |>
      select(from, to, trade_value_usd, weight_binary,
             weight_marketshare, weight_log, source, layer),
    directed = TRUE,
    vertices = node_df
  )
  g$layer <- layer_label
  g$year  <- yr
  g$n_nodes <- vcount(g)
  g$n_edges <- ecount(g)
  message(sprintf("%d %s: %d nodes, %d edges", yr, layer_label, vcount(g), ecount(g)))
  g
}



# 11. Save outputs


message("\n========================================")
message("SAVING OUTPUTS")
message("========================================")

for (yr in YEARS) {
  r <- year_results[[as.character(yr)]]

  # Edge CSVs
  write_csv(r$frontend, file.path(DIRS$processed, paste0("edges_frontend_", yr, ".csv")))
  write_csv(r$backend,  file.path(DIRS$processed, paste0("edges_backend_",  yr, ".csv")))

  # igraph objects
  g_fe <- build_graph(r$frontend, nodes, "layer1_frontend", yr)
  g_be <- build_graph(r$backend,  nodes, "layer2_backend",  yr)

  saveRDS(g_fe, file.path(DIRS$processed, paste0("graph_frontend_", yr, ".rds")))
  saveRDS(g_be, file.path(DIRS$processed, paste0("graph_backend_",  yr, ".rds")))
}

# Node attributes (single table, all years)
write_csv(nodes, file.path(DIRS$processed, "node_attributes.csv"))

message("\n========================================")
message("OUTPUT SUMMARY")
message("========================================")
for (yr in YEARS) {
  r <- year_results[[as.character(yr)]]
  message(sprintf("%d: edges_frontend_%d.csv (%d edges) | edges_backend_%d.csv (%d edges)",
                  yr, yr, nrow(r$frontend), yr, nrow(r$backend)))
  message(sprintf("     graph_frontend_%d.rds | graph_backend_%d.rds", yr, yr))
}
message("node_attributes.csv — ", nrow(nodes), " countries, degree wide by year")
message("========================================")
message("Next: run 06_geopolitical_attrs.R, then knit final_analysis.Rmd")
