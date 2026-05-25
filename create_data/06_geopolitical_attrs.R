# 06_geopolitical_attrs.R — Node and Dyadic Geopolitical and Geographical Distance Attributes
# Purpose: Attach GDP, alliance membership (NATO/EU), and UN General Assembly
#          voting similarity to the node and dyad tables produced by
#          05_build_network_data.R.
#          Build two matrices, one for political alignment and one for geographical distance 
#
# Inputs:  data/processed/node_attributes.csv — produced by 05_build_network_data.R
#          World Bank WDI API (GDP)
#          unvotes package (UNGA roll-call data)
#
# Outputs:
#   data/processed/node_geopolitical.csv
#   data/processed/dyad_unga_similarity.csv
#   data/processed/unga_similarity_matrix.rds
#   data/processed/node_geopolitical.rds
#   data/processed/dyad_unga_similarity.rds
#   data/processed/graph_*.rds  — graphs updated with geopolitical attributes
#   data/processed/dist_matrix_log.rds
# Run from project root: Rscript create_data/06_geopolitical_attrs.R

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(countrycode)
library(WDI)
library(cepiigeodist)
library(unvotes)
library(igraph)

source("config.R")


node_path <- file.path("data/processed", "node_attributes.csv")

if (!file.exists(node_path)) {
  stop(
    "node_attributes.csv not found.\n",
    "Run create_data/05_build_network_data.R first."
  )
}

nodes <- read_csv(node_path, show_col_types = FALSE)

stopifnot(
  "node_attributes.csv is empty — run 05_build_network_data.R first" =
    nrow(nodes) > 0
)

iso3_set <- nodes$iso3
message("Country set loaded: ", length(iso3_set), " countries")



# 1. GDP — World Bank via WDI package
#    If node_geopolitical.csv already exists, pull GDP from it to skip the API call.

geo_csv_path <- file.path("data/processed", "node_geopolitical.csv")

if (file.exists(geo_csv_path)) {
  message("node_geopolitical.csv found — loading GDP from CSV (skipping WDI API call).")
  gdp <- read_csv(geo_csv_path, show_col_types = FALSE) |>
    select(iso3, gdp_usd, gdp_pc_usd, gdp_log, gdp_pc_log)
} else {
  gdp_raw <- WDI(
    country   = iso3_set,
    indicator = c(gdp_usd    = "NY.GDP.MKTP.CD",
                  gdp_pc_usd = "NY.GDP.PCAP.CD"),
    start     = YEAR_GDP,
    end       = YEAR_GDP,
    extra     = FALSE
  )
  gdp <- gdp_raw |>
    filter(year == YEAR_GDP) |>
    select(iso3 = iso3c, gdp_usd, gdp_pc_usd)

  # Taiwan is not in the World Bank database — add manually.
  # Source: IMF World Economic Outlook 2022 (GDP in current USD)
  taiwan_gdp <- tibble(
    iso3       = "TWN",
    gdp_usd    = 761.43e9,
    gdp_pc_usd = 32315.0
  )
  gdp <- bind_rows(gdp, taiwan_gdp) |>
    mutate(
      gdp_log    = log1p(gdp_usd),
      gdp_pc_log = log1p(gdp_pc_usd)
    )
}

message("GDP rows matched: ", nrow(gdp), " / ", length(iso3_set))
message("Norway GDP (USD): ", gdp |> filter(iso3 == "NOR") |> pull(gdp_usd))



# 2. Alliance / bloc membership — coded manually
#    Source: NATO (nato.int), EU (europa.eu) membership as of 2022
#    Note: SWE and FIN formally joined NATO after 2022.

alliance_df <- tribble(
  ~iso3,  ~nato, ~eu_member, 
  # Core GVC nodes
  "USA",     1L,         0L,          
  "KOR",     0L,         0L,              
  "JPN",     0L,         0L,              
  "CHN",     0L,         0L,          
  "TWN",     0L,         0L,             
  "NLD",     1L,         1L,            
  "DEU",     1L,         1L,             
  "MYS",     0L,         0L,           
  "SGP",     0L,         0L,              
  "VNM",     0L,         0L,              
  "IRL",     0L,         1L,              
  "ISR",     0L,         0L,     
  "FRA",     1L,         1L,             
  "AUT",     0L,         1L,             
  "IND",     0L,         0L,              
  "NOR",     1L,         0L,            
  "SWE",     0L,         1L,           
  "FIN",     0L,         1L,         
  "DNK",     1L,         1L,              
  "BEL",     1L,         1L,               
  "CAN",     1L,         0L,               
  "CHE",     0L,         0L,             
  "CZE",     1L,         1L,             
  "GBR",     1L,         0L,              
  "HKG",     0L,         0L,              
  "HUN",     1L,         1L,             
  "ITA",     1L,         1L,            
  "MEX",     0L,         0L,             
  "PHL",     0L,         0L,            
  "THA",     0L,         0L,              
)

# Notes:
# NATO and EU membership coded as of 2022.
# Note: SWE and FIN formally joined NATO after the 2022 data period.




# 3. UN General Assembly voting similarity — dyadic covariate
#    If dyad_unga_similarity.csv already exists, load from it (no recomputation needed).

unga_csv_path <- file.path("data/processed", "dyad_unga_similarity.csv")

if (file.exists(unga_csv_path)) {
  message("dyad_unga_similarity.csv found — loading UNGA similarity from CSV.")
  dyad_unga <- read_csv(unga_csv_path, show_col_types = FALSE)
} else {
  unga_similarity <- un_votes |>
    inner_join(un_roll_calls |> select(rcid, date), by = "rcid") |>
    mutate(year = year(date)) |>
    filter(year %in% 2019:2022) |>
    mutate(iso3 = countrycode(country_code, "iso2c", "iso3c")) |>
    filter(iso3 %in% iso3_set) |>
    select(rcid, iso3, vote)

  vote_pairs <- unga_similarity |>
    rename(iso3_i = iso3, vote_i = vote) |>
    inner_join(
      unga_similarity |> rename(iso3_j = iso3, vote_j = vote),
      by = "rcid",
      relationship = "many-to-many"
    ) |>
    filter(iso3_i != iso3_j)

  dyad_unga <- vote_pairs |>
    group_by(iso3_i, iso3_j) |>
    summarise(
      n_votes  = n(),
      n_agree  = sum(vote_i == vote_j, na.rm = TRUE),
      unga_sim = n_agree / n_votes,
      .groups  = "drop"
    )
}

message("UNGA dyads: ", nrow(dyad_unga))
message("Norway–USA alignment:   ",
        dyad_unga |> filter(iso3_i == "NOR", iso3_j == "USA") |> pull(unga_sim) |> round(3))
message("Norway–China alignment: ",
        dyad_unga |> filter(iso3_i == "NOR", iso3_j == "CHN") |> pull(unga_sim) |> round(3))



# 4. Combine node-level attributes


node_geo <- nodes |>
  select(iso3, name, is_focal) |>
  left_join(gdp,         by = "iso3") |>
  left_join(alliance_df, by = "iso3") |>
  mutate(
    nato      = replace_na(nato,      0L),
    eu_member = replace_na(eu_member, 0L)
  )

message("\nNode geo coverage: ", nrow(node_geo), " countries")
message("Norway profile:")
node_geo |> filter(is_focal) |> glimpse()



# 5. Save CSVs


write_csv(node_geo,  file.path("data/processed", "node_geopolitical.csv"))
write_csv(dyad_unga, file.path("data/processed", "dyad_unga_similarity.csv"))

message("\nSaved: node_geopolitical.csv   (", nrow(node_geo),  " countries)")
message("Saved: dyad_unga_similarity.csv (", nrow(dyad_unga), " dyads)")


# =============================================================================
# 6. Attach geopolitical attributes to igraph objects; build UNGA and
#    geographic distance matrices
# =============================================================================

node_attrs_path <- file.path("data/processed", "node_attributes.csv")
if (!file.exists(node_attrs_path)) {
  stop("node_attributes.csv not found. Run create_data/05_build_network_data.R first.")
}
node_attrs <- read_csv(node_attrs_path, show_col_types = FALSE)

graph_names <- c("frontend_2019", "frontend_2022", "backend_2019", "backend_2022")

missing_graphs <- graph_names[
  !file.exists(file.path("data/processed", paste0("graph_", graph_names, ".rds")))
]
if (length(missing_graphs) > 0) {
  stop("Missing graph files: ", paste(missing_graphs, collapse = ", "),
       "\nRun create_data/05_build_network_data.R first.")
}

graphs <- setNames(
  lapply(graph_names, \(nm)
         readRDS(file.path("data/processed", paste0("graph_", nm, ".rds")))),
  graph_names
)

message("Graphs loaded: ", paste(graph_names, collapse = ", "))

# -----------------------------------------------------------------------------
# 6a. Attach node-level geopolitical attributes
# -----------------------------------------------------------------------------

attach_node_attrs <- function(g, node_geo, node_attrs) {
  v_desc      <- V(g)$name                                    # ISO3 codes
  geo_matched <- node_geo[match(v_desc, node_geo$iso3), ]     # match on iso3, not name
  iso3_vec    <- geo_matched$iso3
  att_matched <- node_attrs[match(iso3_vec, node_attrs$iso3), ]

  V(g)$iso3        <- iso3_vec
  V(g)$gdp_usd     <- geo_matched$gdp_usd
  V(g)$gdp_log     <- geo_matched$gdp_log
  V(g)$nato        <- geo_matched$nato
  V(g)$eu_member   <- geo_matched$eu_member
  V(g)$rca_fe_2019 <- att_matched$rca_fe_2019
  V(g)$rca_fe_2022 <- att_matched$rca_fe_2022
  V(g)$rca_be_2019 <- att_matched$rca_be_2019
  V(g)$rca_be_2022 <- att_matched$rca_be_2022
  g
}

graphs <- lapply(graphs, attach_node_attrs, node_geo = node_geo, node_attrs = node_attrs)

nor_check <- V(graphs[["backend_2022"]])$iso3
message("Vertex attributes after attachment: ",
        paste(vertex_attr_names(graphs[["backend_2022"]]), collapse = ", "))
message("iso3 NAs after attach (expect 0): ", sum(is.na(nor_check)))
message("Norway present (expect TRUE): ", "NOR" %in% nor_check)

for (nm in names(graphs)) {
  saveRDS(graphs[[nm]],
          file.path("data/processed", paste0("graph_", nm, ".rds")))
}

message("Geopolitical attributes attached and graphs re-saved")


# -----------------------------------------------------------------------------
# 6b. UNGA similarity matrix
# -----------------------------------------------------------------------------

iso3_ordered <- sort(unique(c(dyad_unga$iso3_i, dyad_unga$iso3_j)))

unga_matrix_raw <- dyad_unga |>
  pivot_wider(
    id_cols     = iso3_i,
    names_from  = iso3_j,
    values_from = unga_sim,
    values_fill = NA_real_
  ) |>
  tibble::column_to_rownames("iso3_i") |>
  as.matrix()

unga_matrix_raw <- unga_matrix_raw[iso3_ordered, iso3_ordered]

all_iso3 <- sort(V(graphs[["backend_2022"]])$iso3)

unga_matrix <- matrix(
  NA_real_,
  nrow = length(all_iso3),
  ncol = length(all_iso3),
  dimnames = list(all_iso3, all_iso3)
)

common <- intersect(all_iso3, iso3_ordered)
unga_matrix[common, common] <- unga_matrix_raw[common, common]

saveRDS(unga_matrix, file.path("data/processed", "unga_similarity_matrix.rds"))

message("UNGA matrix: ", nrow(unga_matrix), "x", ncol(unga_matrix),
        " (", length(common), " countries with UNGA data)")
message("NAs in matrix: ", sum(is.na(unga_matrix)))

# Save key R objects for downstream scripts
saveRDS(node_geo,  file.path("data/processed", "node_geopolitical.rds"))
saveRDS(dyad_unga, file.path("data/processed", "dyad_unga_similarity.rds"))
message("Saved: node_geopolitical.rds, dyad_unga_similarity.rds")

# -----------------------------------------------------------------------------
# 6c. Geographic distance matrix (CEPII GeoDist)
# -----------------------------------------------------------------------------

vertex_order <- sort(V(graphs[["backend_2022"]])$iso3)

dist_matrix_log <- dist_cepii |>
  filter(iso_o %in% iso3_set,
         iso_d %in% iso3_set) |>
  select(iso_o, iso_d, distw) |>
  pivot_wider(
    names_from  = iso_d,
    values_from = distw
  ) |>
  column_to_rownames("iso_o") |>
  as.matrix()

storage.mode(dist_matrix_log) <- "numeric"
dist_matrix_log <- dist_matrix_log[vertex_order, vertex_order]

# take logs while preserving matrix structure
dist_matrix_log <- log(dist_matrix_log)

# set diagonal to zero
diag(dist_matrix_log) <- 0

saveRDS(
  dist_matrix_log,
  file.path("data/processed", "dist_matrix_log.rds")
)
message("Distance matrix: ", nrow(dist_matrix_log), "x", ncol(dist_matrix_log))
message("NOR-DEU (log km, expect ~7.6): ", round(dist_matrix_log["NOR", "DEU"], 3))
message("NOR-SGP (log km, expect ~9.1): ", round(dist_matrix_log["NOR", "SGP"], 3))

message("Pipeline complete. Saved: node_geopolitical.rds, dyad_unga_similarity.rds, dist_matrix_log.rds\nRun plots/13_trade_plots.R")
