# 04_patent_data.R — Patent Node Attribute Construction
#  Load and clean OECD WIPO patent counts; compute 4-year average
#  (2019–2022) and log-normalised scores for use as node attributes.
#  Country set is derived from country_selection.csv + FORCED_INCLUSIONS
#  + TWN (Taiwan — present via Taiwan ITA data; BTIGE retained separately for ERGM)
#
# Inputs:  data/raw/oecd_patents_wipo.csv
#          data/processed/country_selection.csv  (produced by 01_country_selection.R)
#
# Outputs:
#   data/processed/patents_avg.csv
#   data/processed/patents_avg.rds
#
# Run from project root: Rscript create_data/04_patent_data.R

library(dplyr)
library(readr)

source("config.R")

selection_path <- file.path("data/processed", "country_selection.csv")

if (!file.exists(selection_path)) {
  stop(
    "country_selection.csv not found.\n",
    "Run create_data/01_country_selection.R first."
  )
}

# Country set
# Union of coverage-threshold reporters, forced inclusions, and Taiwan.

country_selection <- read_csv(selection_path, show_col_types = FALSE)

threshold_reporters <- country_selection |>
  filter(selected) |>
  pull(reporter_code)

countries <- union(union(threshold_reporters, FORCED_INCLUSIONS), "TWN")

message("Patent country set: ", length(countries), " countries (including TWN)")

# Load
# Original OECD query filename preserved in data/raw/ for provenance.
# Renamed to oecd_patents_wipo.csv for usability.
patent <- read_csv(file.path("data/raw", "oecd_patents_wipo.csv"), show_col_types = FALSE)

# Clean
patents_clean <- patent |>
  select(REF_AREA, `Reference area`, TIME_PERIOD, OBS_VALUE, AGENT_ROLE, DATE_TYPE) |>
  filter(
    AGENT_ROLE  == "INVENTOR",
    DATE_TYPE   == "APPLICATION",
    TIME_PERIOD %in% seq(min(YEARS), max(YEARS)),
    REF_AREA    %in% countries
  )

# Aggregate (4-year average)
patents_avg <- patents_clean |>
  group_by(REF_AREA, `Reference area`) |>
  summarise(patents = mean(OBS_VALUE, na.rm = TRUE), .groups = "drop")

# Normalise
total_patents <- sum(patents_avg$patents, na.rm = TRUE)

patents_avg <- patents_avg |>
  mutate(
    patents_share = patents / total_patents,
    patents_log   = log1p(patents)
  )

# Save
write_csv(patents_avg, file.path("data/processed", "patents_avg.csv"))
message("Saved: patents_avg.csv — ", nrow(patents_avg), " countries")
message("Norway patents (avg): ",
        patents_avg |> filter(REF_AREA == "NOR") |> pull(patents) |> round(1))

saveRDS(patents_avg, file.path("data/processed", "patents_avg.rds"))
message("Saved: patents_avg.rds")
message("Next: run create_data/05_build_network_data.R")
