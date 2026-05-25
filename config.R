
# config.R — Shared constants for the semiconductor GVC pipeline
# Project: Mapping Norway in the Global Semiconductor Value Chain


# Pipeline run order:
# source("config.R")           — loaded by all scripts below
# 01_country_selection.R       -> data/processed/country_selection.csv
# 02_comtrade_pull.R           -> data/raw/semiconductor/semiconductor_network.csv
# 03_taiwan_data.R             -> data/processed/taiwan_full.csv
# 04_patent_data.R             -> data/processed/patents_avg.csv
# 05_build_network_data.R      -> data/processed/edges_*.csv, graph_*.rds,
#                                  node_attributes.csv
# 06_geopolitical_attrs.R      -> data/processed/node_geopolitical.csv,
#                                  dyad_unga_similarity.csv


YEARS              <- c(2019, 2022)
MIN_FLOW           <- 1e6
COVERAGE_THRESHOLD <- 0.99
FOCAL_COUNTRY      <- "NOR"
FORCED_INCLUSIONS <- c(
  "NOR", "SWE", "FIN", "DNK"       # focal + Nordic comparators
)
YEAR_GDP           <- 2022   # used by 06_geopolitical_attrs.R

hs_layer1 <- c("280461","381800","811292","848610","848620","848630","848640","848690","903082")
hs_layer2 <- c("854110","854121","854129","854130","854160","854190",
               "854231","854232","854233","854239","854290",
               "852351","852352","852359")
all_hs       <- c(hs_layer1, hs_layer2)
hs_layer_map <- data.frame(
  hs_code = c(hs_layer1, hs_layer2),
  layer   = c(rep("layer1_frontend", length(hs_layer1)),
              rep("layer2_backend",  length(hs_layer2)))
)

THESIS_DIR <- "thesis_project"

DIRS <- list(
  raw       = "data/raw",
  processed = "data/processed",
  network   = "data/raw/semiconductor",
  tables    = file.path(THESIS_DIR, "analyses/output"),
  figures   = file.path(THESIS_DIR, "plots/output")
)
