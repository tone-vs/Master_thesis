# 03_taiwan_data.R — Taiwan Semiconductor Trade
#
# Sources:
#   1. OECD BTIGE (CPA C261+C265) — backend, 2019 + 2022
#      RETAINED FOR ERGM ONLY. Excluded from all descriptive network analyses.
#   2. Taiwan ITA customs portal — frontend, 2022 only
#      https://portal.sw.nat.gov.tw/APGA/GA30E
#      Query: Total Exports + Imports | Annual 2022 | Partner = World
#      CCC codes: 381800,848610,848620,848630,848640,848690,903082
#   3. Taiwan ITA customs portal — backend, 2022 only
#      Same portal, same query structure
#      CCC codes: 852351,852352,852359,854110,854121,854129,854130,
#                 854160,854190,854231,854232,854233,854239,854290
#
# Taiwan is absent from BOTH layers for 2019 — ITA data is 2022 only.
# The 2019 networks are therefore Taiwan-free Comtrade-only networks.
# This is documented as a data limitation in the thesis.
#
# Inputs:
#   data/raw/oecd_btige_taiwan.csv   — OECD BTIGE (kept for ERGM)
#   data/raw/frontend_taiwan.csv     — ITA customs frontend
#   data/raw/backend_taiwan.csv      — ITA customs backend
#   data/processed/country_selection.csv
#
# Outputs:
#   data/processed/taiwan_ita_full.csv / .rds  — ITA only (primary, both layers)
#   data/processed/taiwan_btige_only.csv / .rds — BTIGE only (ERGM use only)
#   data/processed/taiwan_full.csv / .rds       — ITA only (used by 05_build_network_data.R)
#
# Run from project root: Rscript create_data/03_taiwan_data.R

library(dplyr)
library(readr)
library(countrycode)

source("config.R")

selection_path <- file.path("data/processed", "country_selection.csv")

if (!file.exists(selection_path)) {
  stop("country_selection.csv not found.\nRun create_data/01_country_selection.R first.")
}

country_selection <- read_csv(selection_path, show_col_types = FALSE)

partner_list <- union(
  country_selection |> filter(selected) |> pull(reporter_code),
  FORCED_INCLUSIONS
)

message("Taiwan partner list: ", length(partner_list), " countries")

# =============================================================================
# PART 1 — OECD BTIGE (layer2_backend, 2019 + 2022)
#
# BTIGE retained for ERGM estimation only — excluded from all descriptive
# network analyses. Do not merge into main edge list.
# =============================================================================

btige_path <- file.path("data/raw", "oecd_btige_taiwan.csv")

if (!file.exists(btige_path)) {
  stop("oecd_btige_taiwan.csv not found in data/raw/")
}

taiwandata <- read_csv(btige_path, show_col_types = FALSE)

taiwan_clean <- taiwandata |>
  filter(
    TRADE_FLOW       %in% c("X", "M"),
    TIME_PERIOD      %in% YEARS,
    COUNTERPART_AREA %in% partner_list,
    !is.na(OBS_VALUE),
    OBS_VALUE        > 0
  ) |>
  mutate(trade_value_usd = OBS_VALUE * (10 ^ UNIT_MULT)) |>
  filter(trade_value_usd >= MIN_FLOW)

btige_exports <- taiwan_clean |>
  filter(TRADE_FLOW == "X") |>
  group_by(TIME_PERIOD, COUNTERPART_AREA, `Counterpart area`) |>
  summarise(trade_value_usd = sum(trade_value_usd), .groups = "drop") |>
  transmute(
    layer           = "layer2_backend",
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

btige_imports <- taiwan_clean |>
  filter(TRADE_FLOW == "M") |>
  group_by(TIME_PERIOD, COUNTERPART_AREA, `Counterpart area`) |>
  summarise(trade_value_usd = sum(trade_value_usd), .groups = "drop") |>
  transmute(
    layer           = "layer2_backend",
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

taiwan_btige_only <- bind_rows(btige_exports, btige_imports)

message("BTIGE rows: ", nrow(taiwan_btige_only),
        " | years: ", paste(sort(unique(taiwan_btige_only$year)), collapse = ", "))

write_csv(taiwan_btige_only, file.path("data/processed", "taiwan_btige_only.csv"))
saveRDS(taiwan_btige_only,   file.path("data/processed", "taiwan_btige_only.rds"))
message("Saved: taiwan_btige_only.csv / .rds (ERGM use only)")

# =============================================================================
# PART 2 — ITA helper: shared parsing logic for both frontend and backend
# =============================================================================

parse_ita <- function(path, layer_name, layer_code) {

  if (!file.exists(path)) {
    warning(basename(path), " not found in data/raw/ — Taiwan absent from ",
            layer_name, " layer.")
    return(tibble())
  }

  ita_raw <- read_csv(path, show_col_types = FALSE) |>
    rename(
      flow         = `Imports/Exports`,
      year         = Time,
      hs_code      = `Commodity Code`,
      hs_desc      = `Description of Good`,
      partner_name = `Country(Area)`,
      value_usd_k  = `Value(USD$ 1000)`
    ) |>
    mutate(
      trade_value_usd = value_usd_k * 1000,
      hs_code         = as.character(hs_code),
      year            = as.integer(year)
    ) |>
    filter(
      !is.na(trade_value_usd),
      trade_value_usd > 0,
      partner_name != "Taiwan ROC"
    )

  ita_raw <- ita_raw |>
    mutate(
      partner_name_clean = case_when(
        partner_name == "Korea Republic of" ~ "South Korea",
        partner_name == "Viet Nam"          ~ "Vietnam",
        partner_name == "T?rkiye"           ~ "Turkey",
        partner_name == "Türkiye"           ~ "Turkey",
        TRUE                                ~ partner_name
      ),
      partner_code = countrycode(
        partner_name_clean, "country.name", "iso3c", warn = FALSE
      )
    )

  unmatched <- ita_raw |>
    filter(is.na(partner_code)) |>
    distinct(partner_name)

  if (nrow(unmatched) > 0) {
    message("ITA ", layer_name, " unmatched names (add overrides if any are in your network):")
    print(unmatched)
  }

  ita_filtered <- ita_raw |>
    filter(
      !is.na(partner_code),
      partner_code %in% partner_list,
      trade_value_usd >= MIN_FLOW
    )

  ita_exports <- ita_filtered |>
    filter(flow == "Exports") |>
    group_by(hs_code, hs_desc, year, partner_code) |>
    summarise(trade_value_usd = sum(trade_value_usd), .groups = "drop") |>
    transmute(
      layer           = layer_code,
      hs_code,
      hs_desc         = trimws(hs_desc),
      year,
      reporter_code   = "TWN",
      reporter        = "Chinese Taipei",
      partner_code,
      partner         = countrycode(partner_code, "iso3c", "country.name"),
      trade_value_usd,
      source          = "Taiwan_ITA"
    )

  ita_imports <- ita_filtered |>
    filter(flow == "Imports") |>
    group_by(hs_code, hs_desc, year, partner_code) |>
    summarise(trade_value_usd = sum(trade_value_usd), .groups = "drop") |>
    transmute(
      layer           = layer_code,
      hs_code,
      hs_desc         = trimws(hs_desc),
      year,
      reporter_code   = partner_code,
      reporter        = countrycode(partner_code, "iso3c", "country.name"),
      partner_code    = "TWN",
      partner         = "Chinese Taipei",
      trade_value_usd,
      source          = "Taiwan_ITA"
    )

  result <- bind_rows(ita_exports, ita_imports)

  message("ITA ", layer_name, " rows: ", nrow(result),
          " | TWN->partner: ", sum(result$reporter_code == "TWN"),
          " | partner->TWN: ", sum(result$partner_code  == "TWN"))

  message("Top 5 Taiwan ", layer_name, " export destinations (2022):")
  result |>
    filter(reporter_code == "TWN") |>
    group_by(partner_code) |>
    summarise(total = sum(trade_value_usd), .groups = "drop") |>
    arrange(desc(total)) |>
    head(5) |>
    print()

  result
}

# =============================================================================
# PART 2a — ITA frontend (layer1_frontend, 2022 only)
# =============================================================================

taiwan_ita_frontend <- parse_ita(
  path       = file.path("data/raw", "frontend_taiwan.csv"),
  layer_name = "frontend",
  layer_code = "layer1_frontend"
)

# =============================================================================
# PART 2b — ITA backend (layer2_backend, 2022 only)
# =============================================================================

taiwan_ita_backend <- parse_ita(
  path       = file.path("data/raw", "backend_taiwan.csv"),
  layer_name = "backend",
  layer_code = "layer2_backend"
)

# =============================================================================
# PART 3 — Combine ITA layers and save
#
# taiwan_ita_full is the primary Taiwan data for all descriptive analyses.
# taiwan_full (= taiwan_ita_full) is what 05_build_network_data.R reads.
# BTIGE is NOT included here — it is saved separately for ERGM only.
# =============================================================================

taiwan_ita_full <- bind_rows(taiwan_ita_frontend, taiwan_ita_backend)

message("\n========================================")
message("TAIWAN ITA FULL SUMMARY")
message("========================================")
message("Total ITA rows: ", nrow(taiwan_ita_full))
taiwan_ita_full |> count(layer, year, source) |> print()

write_csv(taiwan_ita_full, file.path("data/processed", "taiwan_ita_full.csv"))
saveRDS(taiwan_ita_full,   file.path("data/processed", "taiwan_ita_full.rds"))
message("Saved: taiwan_ita_full.csv / .rds")

# taiwan_full = ITA only — this is what 05_build_network_data.R reads
taiwan_full <- taiwan_ita_full
write_csv(taiwan_full, file.path("data/processed", "taiwan_full.csv"))
saveRDS(taiwan_full,   file.path("data/processed", "taiwan_full.rds"))
message("Saved: taiwan_full.csv / .rds (ITA only — no BTIGE)")
message("========================================")

# =============================================================================
# PART 4 — RCA total exports for Taiwan 
# =============================================================================

taiwan_rca <- read_csv("data/raw/oecd_rca_taiwan.csv", show_col_types = FALSE)

taiwan_total_exports <- taiwan_rca |>
  filter(
    TRADE_FLOW         == "X",
    `Counterpart area` == "World",
    TIME_PERIOD        %in% YEARS
  ) |>
  group_by(TIME_PERIOD) |>
  summarise(
    total_exports = sum(OBS_VALUE * (10 ^ UNIT_MULT), na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(reporter_code = "TWN") |>
  rename(year = TIME_PERIOD)

total_exports <- read_csv("data/processed/total_exports.csv",
                          show_col_types = FALSE)

total_exports_updated <- bind_rows(
  total_exports |> filter(reporter_code != "TWN"),
  taiwan_total_exports
)

write_csv(total_exports_updated, "data/processed/total_exports.csv")
message("TWN total exports added for years: ",
        paste(taiwan_total_exports$year, collapse = ", "))

message("\n03_taiwan_data.R complete.")
message("Next: run create_data/04_patent_data.R")
