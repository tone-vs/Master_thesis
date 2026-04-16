# =============================================================================
# patent_data.R — Patent Node Attribute Construction
# Project: Mapping Norway in the Global Semiconductor Value Chain
# =============================================================================
# Author:  [Your name]
# Date:    2025
# Purpose: Load and clean OECD WIPO patent counts for semiconductor-relevant
#          economies; compute 3-year average (2020–2022) and log-normalised
#          scores for use as node attributes in the network analysis.
# Inputs:  OECD WIPO patent CSV (placed in project root)
# Outputs: data/processed/patents_avg.csv
#          patents_avg — tibble, joined onto node attribute table
#          Used in:    combining_data.R / final_analysis.Rmd
# =============================================================================

# --- Libraries ---------------------------------------------------------------
library(dplyr)
library(readr)

# --- Country set -------------------------------------------------------------
countries <- c(
  "USA", "KOR", "TWN", "JPN", "CHN", "NLD", "DEU",
  "MYS", "SGP", "VNM", "IRL", "ISR", "FRA", "AUT",
  "IND", "NOR", "SWE", "FIN", "DNK"
)

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


