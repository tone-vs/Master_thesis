# 06_geopolitical_attrs.R — Node and Dyadic Geopolitical and Geographical Distance Attributes
# Purpose: Attach GDP, and UN General Assembly
#          voting similarity to the node and dyad tables produced by
#          05_build_network_data.R.
#          Build two matrices, one for political alignment and one for geographical distance 
# # 2. Alliance / bloc membership — coded manually
#    Source: NATO (nato.int), EU (europa.eu) membership as of 2022
#    Note: SWE and FIN formally joined NATO after 2022.
#    NOT used as ERGM covariates — geopolitical alignment in ERGMs is
#    operationalised through UNGA voting similarity (dyadic, section 3 below).
#
# Inputs:  data/processed/node_attributes.csv — produced by 05_build_network_data.R
#          World Bank WDI API (GDP)
#          unvotes package (UNGA roll-call data)
#
# Outputs:
#   data/processed/node_geopolitical.csv
#   data/processed/node_geopolitical_2019.csv  — 2019 GDP for temporal ERGM comparison
#   data/processed/dyad_unga_similarity.csv    — UNGA penalised similarity, votes 2017–2019
#   data/processed/unga_similarity_matrix.rds
#   data/processed/node_geopolitical.rds
#   data/processed/dyad_unga_similarity.rds
#   data/processed/graph_*.rds  — graphs updated with geopolitical attributes
#   data/processed/dist_matrix_log.rds
# Run from project root: Rscript create_data/06_geopolitical_attrs.R

library(dplyr)
library(tibble)
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
#    2022 GDP: primary measure used in all ERGM specifications
#    2019 GDP: used only for the temporal ERGM 2019 model
#    Cache: if node_geopolitical.csv exists, load 2022 GDP from it to skip API call.
#    Taiwan not in World Bank — added manually from IMF World Economic Outlook.

geo_csv_path <- file.path("data/processed", "node_geopolitical.csv")

if (file.exists(geo_csv_path)) {
  message("node_geopolitical.csv found — loading 2022 GDP from CSV (skipping WDI API call).")
  gdp_2022 <- read_csv(geo_csv_path, show_col_types = FALSE) |>
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
  gdp_2022 <- gdp_raw |>
    filter(year == YEAR_GDP) |>
    select(iso3 = iso3c, gdp_usd, gdp_pc_usd)
  taiwan_gdp_2022 <- tibble(
    iso3       = "TWN",
    gdp_usd    = 761.43e9,
    gdp_pc_usd = 32315.0
  )
  gdp_2022 <- bind_rows(gdp_2022, taiwan_gdp_2022) |>
    mutate(
      gdp_log    = log1p(gdp_usd),
      gdp_pc_log = log1p(gdp_pc_usd)
    )
}

message("2022 GDP rows matched: ", nrow(gdp_2022), " / ", length(iso3_set))
message("Norway 2022 GDP (USD): ", gdp_2022 |> filter(iso3 == "NOR") |> pull(gdp_usd))

# -- 2019 GDP (temporal ERGM comparison only) ----------------------------------
# Cache: if node_geopolitical_2019.csv exists, load from it to skip API call.

geo_2019_csv_path <- file.path("data/processed", "node_geopolitical_2019.csv")

if (file.exists(geo_2019_csv_path)) {
  message("node_geopolitical_2019.csv found — loading 2019 GDP from CSV (skipping WDI API call).")
  gdp_2019 <- read_csv(geo_2019_csv_path, show_col_types = FALSE) |>
    select(iso3, gdp_usd, gdp_pc_usd, gdp_log, gdp_pc_log)
} else {
  gdp_raw_2019 <- WDI(
    country   = iso3_set,
    indicator = c(gdp_usd    = "NY.GDP.MKTP.CD",
                  gdp_pc_usd = "NY.GDP.PCAP.CD"),
    start     = YEAR_GDP_2019,
    end       = YEAR_GDP_2019,
    extra     = FALSE
  )
  gdp_2019 <- gdp_raw_2019 |>
    filter(year == YEAR_GDP_2019) |>
    select(iso3 = iso3c, gdp_usd, gdp_pc_usd)
  taiwan_gdp_2019 <- tibble(
    iso3       = "TWN",
    gdp_usd    = 613.51e9,
    gdp_pc_usd = NA_real_   # unavailable from IMF World Economic Outlook for 2019
  )
  gdp_2019 <- bind_rows(gdp_2019, taiwan_gdp_2019) |>
    mutate(
      gdp_log    = log1p(gdp_usd),
      gdp_pc_log = log1p(gdp_pc_usd)
    )
  write_csv(gdp_2019, geo_2019_csv_path)
  message("Saved: node_geopolitical_2019.csv")
}

message("2019 GDP rows matched: ", nrow(gdp_2019), " / ", length(iso3_set))
message("Norway 2019 GDP (USD): ", gdp_2019 |> filter(iso3 == "NOR") |> pull(gdp_usd))

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
#    Formula: penalised similarity = ((agreements - disagreements) / n_mutual + 1) / 2
#      agreements    = both vote yes, or both vote no
#      disagreements = one votes yes and the other votes no
#      n_mutual      = votes where both countries cast a non-abstention vote
#    Abstentions are excluded from agreements and disagreements entirely.
#
#    A single similarity score is computed using votes from 2017–2019, which is
#    the most recent complete three-year window available in the unvotes package
#    (data ends in 2019). This single score is shared across all ERGM models —
#    both 2022 specifications and the 2019 temporal comparison.
#    Cache: if dyad_unga_similarity.csv exists, load from it; otherwise recompute.

# 3. UN General Assembly voting similarity — dyadic covariate
#    Formula: penalised similarity = ((agreements - disagreements) / n_mutual + 1) / 2
#      agreements    = both vote yes, or both vote no (non-abstain)
#      disagreements = one votes yes and the other votes no
#      n_mutual      = votes where both countries cast a non-abstention vote
#    Votes 2017-2019 used (unvotes package data ends at 2019).
#    Single score used for all ERGM specifications.
#    If dyad_unga_similarity.csv already exists, load from it to skip recomputation.

unga_csv_path <- file.path("data/processed", "dyad_unga_similarity.csv")
if (file.exists(unga_csv_path)) {
  message("dyad_unga_similarity.csv found — loading UNGA similarity from CSV.")
  dyad_unga <- read_csv(unga_csv_path, show_col_types = FALSE)
} else {
  unga_similarity <- un_votes |>
    inner_join(un_roll_calls |> select(rcid, date), by = "rcid") |>
    mutate(year = year(date)) |>
    filter(year %in% 2017:2019) |>
    mutate(iso3 = countrycode(country_code, "iso2c", "iso3c",
                              custom_match = c("YU" = NA_character_))) |>
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
      n_agree    = sum(vote_i == vote_j & vote_i != "abstain", na.rm = TRUE),
      n_disagree = sum(
        (vote_i == "yes" & vote_j == "no") |
          (vote_i == "no"  & vote_j == "yes"), na.rm = TRUE
      ),
      n_mutual   = sum(vote_i != "abstain" & vote_j != "abstain", na.rm = TRUE),
      unga_sim   = ((n_agree - n_disagree) / n_mutual + 1) / 2,
      .groups    = "drop"
    )
}
message("UNGA dyads: ", nrow(dyad_unga))
message("Norway-USA alignment:   ",
        dyad_unga |> filter(iso3_i == "NOR", iso3_j == "USA") |> pull(unga_sim) |> round(3))
message("Norway-China alignment: ",
        dyad_unga |> filter(iso3_i == "NOR", iso3_j == "CHN") |> pull(unga_sim) |> round(3))


# 4. Combine node-level attributes


node_geo <- nodes |>
  select(iso3, name, is_focal) |>
  left_join(gdp_2022,    by = "iso3") |>
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
  # gdp_log uses 2022 values (from gdp_2022) for all standard graphs.
  # 2019 GDP is attached separately in analyses/12_ergm.R via gdp_override
  # in igraph_to_network() for the temporal ERGM comparison only.
  V(g)$gdp_log     <- geo_matched$gdp_log
  # Alliance membership attached for community composition table only.
  # Not used in ERGM model specifications.
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
#     Built from dyad_unga (votes 2017–2019; single shared score for all models).
#     Padded to the full graph node set; countries without UNGA data receive NA.
# -----------------------------------------------------------------------------

build_unga_matrix <- function(dyad_unga, all_iso3) {
  iso3_ordered <- sort(unique(c(dyad_unga$iso3_i, dyad_unga$iso3_j)))
  unga_raw <- dyad_unga |>
    select(iso3_i, iso3_j, unga_sim) |>
    pivot_wider(
      id_cols     = iso3_i,
      names_from  = iso3_j,
      values_from = unga_sim,
      values_fill = NA_real_
    ) |>
    tibble::column_to_rownames("iso3_i") |>
    as.matrix()
  unga_raw <- unga_raw[iso3_ordered, iso3_ordered]

  mat <- matrix(
    NA_real_,
    nrow = length(all_iso3),
    ncol = length(all_iso3),
    dimnames = list(all_iso3, all_iso3)
  )
  common <- intersect(all_iso3, iso3_ordered)
  mat[common, common] <- unga_raw[common, common]
  list(mat = mat, n_common = length(common))
}

all_iso3 <- sort(V(graphs[["backend_2022"]])$iso3)

result      <- build_unga_matrix(dyad_unga, all_iso3)
unga_matrix <- result$mat

saveRDS(unga_matrix, file.path("data/processed", "unga_similarity_matrix.rds"))

message("UNGA matrix: ", nrow(unga_matrix), "x", ncol(unga_matrix),
        " (", result$n_common, " countries with UNGA data)")
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
  tibble::column_to_rownames("iso_o") |>
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


message("Pipeline complete. Saved: node_geopolitical.rds, dyad_unga_similarity.rds, dist_matrix_log.rds\nRun analyses/07_descriptive_trade.R")
