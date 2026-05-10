# 03_taiwan_data.R — Taiwan Semiconductor Trade
#
# Sources:
#   1. OECD BTIGE (CPA C261+C265) — backend layer, both years (2019 + 2022)
#   2. Taiwan ITA customs portal    — frontend layer, 2022 only
#      https://portal.sw.nat.gov.tw/APGA/GA30E
#      Query: Total Exports + Imports | Annual 2022 | Partner = World
#      CCC codes: 381800,848610,848620,848630,848640,848690,903082
#      Re-exports/re-imports: EXCLUDED
#
# Inputs:
#   data/raw/oecd_btige_taiwan.csv         (OECD BTIGE — backend)
#   data/raw/taiwan_ita_frontend_2022.csv  (ITA customs — frontend, rename download)
#   data/processed/country_selection.csv
#
# Output:
#   data/processed/taiwan_full.csv  — all Taiwan flows, all layers, all years
#
# Layer note:
#   BTIGE rows -> layer2_backend   (set here directly)
#   ITA rows   -> layer1_frontend  (set here directly)
#   05_build_network_data.R must NOT override the layer col for Taiwan rows.
#   Remove the mutate(layer = "layer2_backend") block from that script.
#
# ITA data note:
#   Values in raw file are USD THOUSANDS — multiplied by 1000 here.
#   Frontend ITA data is 2022 only. The 2019 frontend network therefore
#   excludes Taiwan — documented as a limitation in the thesis.


library(dplyr)
library(readr)
library(countrycode)

source("config.R")

selection_path <- file.path(DIRS$processed, "country_selection.csv")

if (!file.exists(selection_path)) {
  stop("country_selection.csv not found.\nRun 01_country_selection.R first.")
}

country_selection <- read_csv(selection_path, show_col_types = FALSE)

partner_list <- union(
  country_selection |> filter(selected) |> pull(reporter_code),
  FORCED_INCLUSIONS
)

message("Taiwan partner list: ", length(partner_list), " countries")


# PART 1 — OECD BTIGE (layer2_backend, 2019 + 2022)


taiwandata <- read_csv(file.path(DIRS$raw, "oecd_btige_taiwan.csv"),
                       show_col_types = FALSE)

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

taiwan_btige <- bind_rows(btige_exports, btige_imports)

message("BTIGE rows: ", nrow(taiwan_btige),
        " | years: ", paste(sort(unique(taiwan_btige$year)), collapse = ", "))



# PART 2 — ITA Customs Portal (layer1_frontend, 2022 only)


ita_path <- file.path(DIRS$raw, "frontend_taiwan.csv")

if (!file.exists(ita_path)) {
  
  warning(
    "frontend_taiwan.csv not found in data/raw/\n",
    "Rename your ITA portal download to this filename and re-run.\n",
    "Proceeding with BTIGE only — Taiwan absent from frontend layer."
  )
  taiwan_ita <- tibble()
  
} else {
  
  ita_raw <- read_csv(ita_path, show_col_types = FALSE) |>
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
    message("ITA unmatched names (add overrides if any are in your network):")
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
      layer           = "layer1_frontend",
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
      layer           = "layer1_frontend",
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
  
  taiwan_ita <- bind_rows(ita_exports, ita_imports)
  
  message("ITA frontend rows: ", nrow(taiwan_ita),
          " | TWN->partner: ", sum(taiwan_ita$reporter_code == "TWN"),
          " | partner->TWN: ", sum(taiwan_ita$partner_code  == "TWN"))
  
  message("\nTop 5 Taiwan frontend export destinations (2022):")
  taiwan_ita |>
    filter(reporter_code == "TWN") |>
    group_by(partner_code) |>
    summarise(total = sum(trade_value_usd), .groups = "drop") |>
    arrange(desc(total)) |>
    head(5) |>
    print()
}



# PART 3 — Combine and save


taiwan_full <- bind_rows(taiwan_btige, taiwan_ita)

message("\n========================================")
message("TAIWAN FULL SUMMARY")
message("========================================")
message("Total rows: ", nrow(taiwan_full))
taiwan_full |> count(layer, year, source) |> print()

write_csv(taiwan_full, file.path(DIRS$processed, "taiwan_full.csv"))

message("Saved: taiwan_full.csv")
message("========================================")



# PART 4 — RCA total exports for Taiwan (unchanged)


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