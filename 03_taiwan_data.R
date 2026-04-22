
# 03_taiwan_data.R — Taiwan Semiconductor Trade (OECD BTIGE)


# Process OECD Bilateral Trade in Goods by Industry and End-use
# (BTIGE) data for Taiwan across all years in YEARS, and output a
# tibble with the same column structure as the Comtrade edge lists
# so both can be combined with bind_rows() in 05_build_network_data.R.
# Inputs:  data/raw/oecd_btige_taiwan.csv
#          data/processed/country_selection.csv  (produced by 01_country_selection.R)
# Outputs: data/processed/taiwan_full.csv  (all years, long format)
#          Used in:    05_build_network_data.R / final_analysis.Rmd


library(dplyr)
library(readr)

source("config.R")

selection_path <- file.path(DIRS$processed, "country_selection.csv")

if (!file.exists(selection_path)) {
  stop(
    "country_selection.csv not found.\n",
    "Run 01_country_selection.R first."
  )
}

#  Partner list
# Derived from country_selection.csv + forced inclusions — same pattern as
# 02_comtrade_pull.R so Taiwan's partner set matches the Comtrade reporter set.

country_selection <- read_csv(selection_path, show_col_types = FALSE)

threshold_reporters <- country_selection |>
  filter(selected) |>
  pull(reporter_code)

partner_list <- union(threshold_reporters, FORCED_INCLUSIONS)

message("Taiwan partner list: ", length(partner_list), " countries")

# HS layer assignment: BTIGE uses CPA product codes
# CPA_2_1_C261 = Electronic components & boards  -> manufacturing
# CPA_2_1_C265 = Instruments & measurement        -> manufacturing
cpa_layer_map <- c(
  "CPA_2_1_C261" = "manufacturing",
  "CPA_2_1_C265" = "manufacturing"
)

#  Load 
# Original OECD query filename preserved in data/raw/ for provenance.
# Renamed to oecd_btige_taiwan.csv for usability.
taiwandata <- read_csv(file.path(DIRS$raw, "oecd_btige_taiwan.csv"), show_col_types = FALSE)

# Clean 
taiwan_clean <- taiwandata |>
  filter(
    TRADE_FLOW       %in% c("X", "M"),
    TIME_PERIOD      %in% YEARS,
    COUNTERPART_AREA %in% partner_list,
    !is.na(OBS_VALUE),
    OBS_VALUE        > 0
  ) |>
  mutate(
    trade_value_usd = OBS_VALUE * (10 ^ UNIT_MULT),
    layer = recode(PRODUCT, !!!cpa_layer_map, .default = "manufacturing")
  ) |>
  filter(trade_value_usd >= MIN_FLOW)

# Exports: TWN -> partner 
taiwan_exports <- taiwan_clean |>
  filter(TRADE_FLOW == "X") |>
  group_by(TIME_PERIOD, COUNTERPART_AREA, `Counterpart area`, layer) |>
  summarise(trade_value_usd = sum(trade_value_usd), .groups = "drop") |>
  transmute(
    layer,
    hs_code         = "BTIGE_C261_C265",
    hs_desc         = "OECD BTIGE CPA C261+C265",
    year            = TIME_PERIOD,
    reporter_code   = "TWN",
    reporter        = "Chinese Taipei",
    partner_code    = COUNTERPART_AREA,
    partner         = `Counterpart area`,
    trade_value_usd,
    source          = "OECD_BTIGE"
  )

# Imports: partner -> TWN
taiwan_imports <- taiwan_clean |>
  filter(TRADE_FLOW == "M") |>
  group_by(TIME_PERIOD, COUNTERPART_AREA, `Counterpart area`, layer) |>
  summarise(trade_value_usd = sum(trade_value_usd), .groups = "drop") |>
  transmute(
    layer,
    hs_code         = "BTIGE_C261_C265",
    hs_desc         = "OECD BTIGE CPA C261+C265",
    year            = TIME_PERIOD,
    reporter_code   = COUNTERPART_AREA,
    reporter        = `Counterpart area`,
    partner_code    = "TWN",
    partner         = "Chinese Taipei",
    trade_value_usd,
    source          = "OECD_BTIGE"
  )

# Combine and save 
taiwan_full <- bind_rows(taiwan_exports, taiwan_imports)

write_csv(taiwan_full, file.path(DIRS$processed, "taiwan_full.csv"))

message("Saved: taiwan_full.csv — ", nrow(taiwan_full), " rows, years: ",
        paste(sort(unique(taiwan_full$year)), collapse = ", "))
