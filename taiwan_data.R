# =============================================================================
# taiwan_data.R — Taiwan Semiconductor Trade (OECD BTIGE)
# Project: Mapping Norway in the Global Semiconductor Value Chain
# =============================================================================
# Author:  [Your name]
# Date:    2025
# Purpose: Process OECD Bilateral Trade in Goods by Industry and End-use
#          (BTIGE) data for Taiwan, and output a tibble with the same column
#          structure as the Comtrade edge lists so both can be combined with
#          bind_rows() in combining_data.R.
# Inputs:  OECD BTIGE CSV (placed in project root)
# Outputs: data/processed/taiwan_full.csv
#          taiwan_full — tibble (exports + imports, long format)
#          Used in:    combining_data.R / final_analysis.Rmd
# =============================================================================

# --- Libraries ---------------------------------------------------------------
library(dplyr)
library(readr)

# --- Settings ----------------------------------------------------------------
YEAR     <- 2022
MIN_FLOW <- 1e6       # $1M threshold — matches Comtrade data

# Reporter list (same economies as Comtrade partner list)
partner_list <- c(
  "USA", "KOR", "JPN", "CHN", "NLD", "DEU",
  "MYS", "SGP", "VNM", "IRL", "ISR", "FRA",
  "AUT", "IND", "CAN", "NOR", "SWE", "DNK", "FIN"
)

# HS layer assignment: BTIGE uses CPA product codes
# CPA_2_1_C261 = Electronic components & boards  -> manufacturing
# CPA_2_1_C265 = Instruments & measurement        -> manufacturing
cpa_layer_map <- c(
  "CPA_2_1_C261" = "manufacturing",
  "CPA_2_1_C265" = "manufacturing"
)

# --- Load --------------------------------------------------------------------
# Original OECD query filename preserved in data/raw/ for provenance.
# Renamed to oecd_btige_taiwan.csv for usability.
taiwandata <- read_csv("data/raw/oecd_btige_taiwan.csv", show_col_types = FALSE)

# --- Clean -------------------------------------------------------------------
taiwan_clean <- taiwandata |>
  filter(
    TRADE_FLOW       %in% c("X", "M"),
    TIME_PERIOD      == YEAR,
    COUNTERPART_AREA %in% partner_list,
    !is.na(OBS_VALUE),
    OBS_VALUE        > 0
  ) |>
  mutate(
    trade_value_usd = OBS_VALUE * (10 ^ UNIT_MULT),
    layer = recode(PRODUCT, !!!cpa_layer_map, .default = "manufacturing")
  ) |>
  filter(trade_value_usd >= MIN_FLOW)

# --- Exports: TWN -> partner -------------------------------------------------
taiwan_exports <- taiwan_clean |>
  filter(TRADE_FLOW == "X") |>
  group_by(COUNTERPART_AREA, `Counterpart area`, layer) |>
  summarise(trade_value_usd = sum(trade_value_usd), .groups = "drop") |>
  transmute(
    layer,
    hs_code         = "BTIGE_C261_C265",
    hs_desc         = "OECD BTIGE CPA C261+C265",
    year            = YEAR,
    reporter_code   = "TWN",
    reporter        = "Chinese Taipei",
    partner_code    = COUNTERPART_AREA,
    partner         = `Counterpart area`,
    trade_value_usd,
    source          = "OECD_BTIGE"
  )

# --- Imports: partner -> TWN (reverse direction) -----------------------------
taiwan_imports <- taiwan_clean |>
  filter(TRADE_FLOW == "M") |>
  group_by(COUNTERPART_AREA, `Counterpart area`, layer) |>
  summarise(trade_value_usd = sum(trade_value_usd), .groups = "drop") |>
  transmute(
    layer,
    hs_code         = "BTIGE_C261_C265",
    hs_desc         = "OECD BTIGE CPA C261+C265",
    year            = YEAR,
    reporter_code   = COUNTERPART_AREA,
    reporter        = `Counterpart area`,
    partner_code    = "TWN",
    partner         = "Chinese Taipei",
    trade_value_usd,
    source          = "OECD_BTIGE"
  )

# --- Combine and save --------------------------------------------------------
taiwan_full <- bind_rows(taiwan_exports, taiwan_imports)

write_csv(taiwan_full, "data/processed/taiwan_full.csv")
