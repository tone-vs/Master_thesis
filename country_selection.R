# =============================================================================
# country_selection.R — Data-driven country set via 90% export coverage rule
# Project: Mapping Norway in the Global Semiconductor Value Chain
# =============================================================================
# Strategy: pull aggregate world exports per reporter for each HS code in one
#   pass (reporter = "all", partner = "World"). This costs 21 API calls total.
#   Countries are then ranked by cumulative semiconductor export share and
#   selected iteratively until the sample covers ≥90% of world trade.
#   Norway is included by research design regardless of rank.
#
# Output:
#   data/processed/country_selection.csv  — full ranking with cumulative shares
#   reporters                             — character vector, ready for use in
#                                           semiconductor_bilateral_pull.R
#
# Citation: OECD (2025) uses 90% coverage threshold for country set definition.
#           Method follows Amador & Cabral (2016) trade network literature.
# =============================================================================

library(comtradr)
library(dplyr)
library(readr)
library(purrr)

set_primary_comtrade_key(Sys.getenv("COMTRADE_PRIMARY_KEY"))

COVERAGE_THRESHOLD <- 0.99   # ≥90% of world semiconductor exports
FOCAL_COUNTRY      <- "NOR"  # always included regardless of rank
YEAR               <- 2022
OUT_DIR            <- "data/processed"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# HS codes — full set across both layers
all_hs <- c(
  # Layer 1: front-end
  "381800","848610","848620","848630","848640","848690","903082",
  # Layer 2: back-end
  "854110","854121","854129","854130","854160","854190",
  "854231","854232","854233","854239","854290",
  "852351","852352","852359"
)

# -----------------------------------------------------------------------------
# 1. Pull aggregate exports: all reporters → world, per HS code
#    21 calls total — fast and well within free tier
# -----------------------------------------------------------------------------

fetch_world_totals <- function(hs_code) {
  message("Fetching world totals for HS ", hs_code, " ...")
  tryCatch({
    ct_get_data(
      reporter       = "all_countries",
      partner        = "all_countries",
      commodity_code = hs_code,
      start_date     = YEAR,
      end_date       = YEAR,
      flow_direction = "export"
    ) |>
      select(
        reporter_code   = reporter_iso,
        reporter        = reporter_desc,
        trade_value_usd = primary_value
      ) |>
      filter(
        !is.na(trade_value_usd),
        trade_value_usd > 0,
        !reporter_code %in% c("WLD", "W00", "_X", "")
      ) |>
      # Sum across all partners to get each reporter's total exports
      group_by(reporter_code, reporter) |>
      summarise(trade_value_usd = sum(trade_value_usd, na.rm = TRUE),
                .groups = "drop") |>
      mutate(hs_code = hs_code)
  }, error = function(e) {
    message("  Error on HS ", hs_code, ": ", e$message)
    NULL
  })
}

world_raw <- map(all_hs, fetch_world_totals, .progress = TRUE) |>
  bind_rows()

message("\nWorld total rows fetched: ", nrow(world_raw))
message("Countries reporting: ", n_distinct(world_raw$reporter_code))


# -----------------------------------------------------------------------------
# 2. Aggregate across HS codes → total semiconductor exports per country
# -----------------------------------------------------------------------------

country_totals <- world_raw |>
  group_by(reporter_code, reporter) |>
  summarise(
    total_semiconductor_exports = sum(trade_value_usd, na.rm = TRUE),
    n_hs_codes_reported         = n_distinct(hs_code),
    .groups = "drop"
  ) |>
  arrange(desc(total_semiconductor_exports)) |>
  mutate(
    world_total       = sum(total_semiconductor_exports),
    export_share      = total_semiconductor_exports / world_total,
    cumulative_share  = cumsum(export_share),
    rank              = row_number()
  )

message("\nTop 20 semiconductor exporters:")
country_totals |>
  select(rank, reporter_code, reporter,
         total_semiconductor_exports, export_share, cumulative_share) |>
  head(20) |>
  mutate(
    export_share     = scales::percent(export_share,     accuracy = 0.1),
    cumulative_share = scales::percent(cumulative_share, accuracy = 0.1)
  ) |>
  print()


# -----------------------------------------------------------------------------
# 3. Select countries iteratively until ≥90% coverage
#    Norway forced in regardless of rank (focal country by research design)
# -----------------------------------------------------------------------------

# Countries that cross the threshold when added
above_threshold <- country_totals |>
  filter(cumulative_share <= COVERAGE_THRESHOLD | 
           lag(cumulative_share, default = 0) < COVERAGE_THRESHOLD)

# Find the cutoff rank (first country that pushes cumulative share over 99%)
cutoff_rank <- country_totals |>
  filter(cumulative_share >= COVERAGE_THRESHOLD) |>
  slice(1) |>
  pull(rank)

selected <- country_totals |>
  filter(rank <= cutoff_rank)

# Force Norway in if not already selected
norway_in_selection <- FOCAL_COUNTRY %in% selected$reporter_code
if (!norway_in_selection) {
  norway_row <- country_totals |> filter(reporter_code == FOCAL_COUNTRY)
  selected   <- bind_rows(selected, norway_row)
  message("\nNorway not in top ", cutoff_rank, " — added by research design")
  message("Norway rank: ", norway_row$rank,
          " | export share: ", scales::percent(norway_row$export_share, accuracy = 0.01))
} else {
  message("\nNorway is within the ", COVERAGE_THRESHOLD * 100,
          "% coverage threshold (rank ", 
          country_totals |> filter(reporter_code == FOCAL_COUNTRY) |> pull(rank), ")")
}

# Tag selection reason
country_selection <- country_totals |>
  mutate(
    selected = reporter_code %in% selected$reporter_code,
    selection_reason = case_when(
      reporter_code == FOCAL_COUNTRY & rank <= cutoff_rank ~ "coverage_threshold",
      reporter_code == FOCAL_COUNTRY                        ~ "focal_country",
      rank <= cutoff_rank                                   ~ "coverage_threshold",
      TRUE                                                  ~ "not_selected"
    )
  )

message("\n--- Selection summary ---")
message("Coverage threshold:  ", COVERAGE_THRESHOLD * 100, "%")
message("Countries selected:  ", sum(country_selection$selected))
message("Actual coverage:     ",
        scales::percent(
          sum(selected$total_semiconductor_exports) /
            unique(country_totals$world_total),
          accuracy = 0.01
        ))
message("Norway included:     YES (rank ",
        country_totals |> filter(reporter_code == FOCAL_COUNTRY) |> pull(rank), ")")


# -----------------------------------------------------------------------------
# 4. Output: reporter vector + documented selection table
# -----------------------------------------------------------------------------

reporters <- country_selection |>
  filter(selected) |>
  pull(reporter_code)

message("\nFinal reporter set (N = ", length(reporters), "):")
message(paste(reporters, collapse = ", "))

write_csv(
  country_selection |>
    select(rank, reporter_code, reporter,
           total_semiconductor_exports, export_share,
           cumulative_share, selected, selection_reason),
  file.path(OUT_DIR, "country_selection.csv")
)

message("\nSaved: country_selection.csv — use for thesis appendix table")
message("Object `reporters` ready — pass to semiconductor_bilateral_pull.R")