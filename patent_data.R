# =============================================================================
# patent_data.R — Patent Node Attribute Construction
# Project: Mapping Norway in the Global Semiconductor Value Chain
# =============================================================================
# Author:  [Your name]
# Date:    2025
# Purpose: Load and clean OECD WIPO patent counts; compute 3-year average
#          (2020–2022) and log-normalised scores for use as node attributes.
#          Country set is derived dynamically from the network (all_nodes)
#          so patent coverage always matches the analysis exactly.
# Inputs:  data/raw/oecd_patents_wipo.csv
#          patent_countries — ISO3 vector passed from build_network_data.R,
#            OR data/processed/node_attributes.csv for standalone runs
# Outputs: data/processed/patents_avg.csv
#          patents_avg — tibble, joined onto node attribute table
#          Used in:    build_network_data.R / final_analysis.Rmd
# =============================================================================

# --- Libraries ---------------------------------------------------------------
library(dplyr)
library(readr)

# --- Country set -------------------------------------------------------------
# When sourced by build_network_data.R, `patent_countries` is passed in from
# the environment (set just after all_nodes is built) so the patent country
# set matches the network exactly.
# When run standalone after a prior full pipeline run, fall back to loading
# the country set from the saved node_attributes.csv.
if (exists("patent_countries")) {
  countries <- patent_countries
} else if (file.exists("data/processed/node_attributes.csv")) {
  countries <- read_csv(
    "data/processed/node_attributes.csv",
    show_col_types = FALSE
  ) |> pull(iso3)
} else {
  stop(paste(
    "Country set not available.",
    "Either run build_network_data.R (which sources this script automatically),",
    "or ensure data/processed/node_attributes.csv exists from a prior run."
  ))
}

# --- Load --------------------------------------------------------------------
# Original OECD query filename preserved in data/raw/ for provenance.
# Renamed to oecd_patents_wipo.csv for usability.
patent <- read_csv("data/raw/oecd_patents_wipo.csv", show_col_types = FALSE)

# --- Clean -------------------------------------------------------------------
patents_clean <- patent |>
  select(REF_AREA, `Reference area`, TIME_PERIOD, OBS_VALUE, AGENT_ROLE, DATE_TYPE) |>
  filter(
    AGENT_ROLE  == "INVENTOR",
    DATE_TYPE   == "APPLICATION",
    TIME_PERIOD %in% 2020:2022,
    REF_AREA    %in% countries
  )

# --- Aggregate (3-year average) ----------------------------------------------
patents_avg <- patents_clean |>
  group_by(REF_AREA, `Reference area`) |>
  summarise(patents = mean(OBS_VALUE, na.rm = TRUE), .groups = "drop")

# --- Normalise ---------------------------------------------------------------
total_patents <- sum(patents_avg$patents, na.rm = TRUE)

patents_avg <- patents_avg |>
  mutate(
    patents_share = patents / total_patents,
    patents_log   = log1p(patents)
  )

# --- Save --------------------------------------------------------------------
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
write_csv(patents_avg, "data/processed/patents_avg.csv")


