# =============================================================================
# semiconductor_bilateral_pull.R — Bilateral Trade Data Pull
# Thesis: Mapping Norway in the Global Semiconductor Value Chain
# =============================================================================
#
# Reporter selection — two criteria, applied jointly:
#
#   1. Coverage threshold (data-driven):
#      Countries are ranked by total semiconductor exports across all 21 HS
#      codes (2022). Countries are added iteratively in descending order until
#      the sample accounts for ≥90% of world semiconductor trade, following
#      OECD (2025). The threshold country set is loaded from:
#        data/processed/country_selection.csv  (produced by country_selection.R)
#
#   2. Forced inclusions (research design):
#      Norway    — focal country (REC Silicon, Elkem; Ponte & Sturgeon 2014)
#      Sweden    — Nordic comparator (Ericsson, Nordic Chip Collaboration 2024)
#      Finland   — Nordic comparator (Nokia, Nordic Chip Collaboration 2024)
#      Denmark   — Nordic comparator (Nordic Chip Collaboration 2024)
#      These four are included regardless of their export rank.
#
#   Flow threshold: bilateral flows below $1M USD are dropped as noise
#   (standard in Comtrade-based network studies; Amador & Cabral 2016).
#
# API: UN Comtrade v2 — free tier: 250 calls/day, 1 call/sec
#   Get a key at: https://comtradeplus.un.org/
#   Set key:      usethis::edit_r_environ() → COMTRADE_PRIMARY_KEY=your_key
#
# Run order:
#   1. country_selection.R              → data/processed/country_selection.csv
#   2. THIS SCRIPT                      → data/semiconductor/semiconductor_network.csv
#   3. taiwan_data.R                    → data/processed/taiwan_full.csv
#   4. patent_data.R                    → patents_avg (in environment)
#   5. build_network_data.R             → igraph objects + edge/node CSVs
# =============================================================================

library(comtradr)
library(dplyr)
library(readr)
library(purrr)
library(cli)

# -----------------------------------------------------------------------------
# SETTINGS
# -----------------------------------------------------------------------------

set_primary_comtrade_key(Sys.getenv("COMTRADE_PRIMARY_KEY"))

YEAR       <- 2022
MIN_FLOW   <- 1e6
OUTPUT_DIR <- "data/semiconductor"
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Countries forced in by research design regardless of export rank
FORCED_INCLUSIONS <- c("NOR", "SWE", "FIN", "DNK")


# -----------------------------------------------------------------------------
# HS CODES — canonical layer definitions (do not change; used downstream)
#
# Layer 1 (front-end): upstream materials, fab equipment, measurement
# Layer 2 (back-end):  packaged chips, ICs, memory, logic, smart cards
# Source: OECD (2025) Annexes D1–D7; Haramboure et al. (2023)
# -----------------------------------------------------------------------------

hs_layer1 <- c(
  "381800",  # Silicon wafers (key for Norway: REC Silicon, Elkem)
  "848610",  # Machines — semiconductor boules/wafers
  "848620",  # Machines — semiconductor devices/ICs
  "848630",  # Machines — flat panel displays
  "848640",  # Machines — mask/reticle repair
  "848690",  # Parts & accessories for 8486 equipment
  "903082"   # Instruments for checking semiconductor wafers/devices
)

hs_layer2 <- c(
  "854110",  # Diodes
  "854121",  # Transistors < 1W
  "854129",  # Transistors >= 1W
  "854130",  # Thyristors, diacs, triacs
  "854160",  # Mounted piezo-electric crystals
  "854190",  # Parts for diodes/transistors/photosensitive devices
  "854231",  # Processors and controllers
  "854232",  # Memories
  "854233",  # Amplifiers
  "854239",  # ICs n.e.c.
  "854290",  # Parts of ICs
  "852351",  # Flash memory cards
  "852352",  # Smart cards
  "852359"   # Semiconductor media (unrecorded)
)

all_hs <- c(hs_layer1, hs_layer2)

hs_layer_map <- bind_rows(
  tibble(hs_code = hs_layer1, layer = "layer1_frontend"),
  tibble(hs_code = hs_layer2, layer = "layer2_backend")
)


# -----------------------------------------------------------------------------
# REPORTER LIST
# Load from country_selection.csv (produced by country_selection.R).
# Force Nordic comparators and Norway in regardless of coverage rank.
# -----------------------------------------------------------------------------

selection_path <- "data/processed/country_selection.csv"

if (!file.exists(selection_path)) {
  stop(
    "country_selection.csv not found.\n",
    "Run country_selection.R first to generate the data-driven country set."
  )
}

country_selection <- read_csv(selection_path, show_col_types = FALSE)

# Countries selected by the 90% coverage threshold
threshold_reporters <- country_selection |>
  filter(selected) |>
  pull(reporter_code)

# Merge with forced inclusions — union of both sets
reporters <- union(threshold_reporters, FORCED_INCLUSIONS)

# Identify which forced countries were NOT already in the threshold set
forced_additions <- setdiff(FORCED_INCLUSIONS, threshold_reporters)

# Summary
n_threshold <- length(threshold_reporters)
n_forced    <- length(forced_additions)
n_total     <- length(reporters)

actual_coverage <- country_selection |>
  filter(reporter_code %in% threshold_reporters) |>
  summarise(coverage = sum(export_share, na.rm = TRUE)) |>
  pull(coverage)

cli::cli_inform(c(
  "Reporter set finalised:",
  "  Coverage threshold (>=90%): {n_threshold} countries ({scales::percent(actual_coverage, accuracy = 0.1)} of world semiconductor exports)",
  "  Forced additions (research design): {n_forced} country/countries — {paste(forced_additions, collapse = ', ')}",
  "  Total reporters: {n_total}",
  "  Estimated API calls: {n_total} reporters x {length(all_hs)} HS codes = {n_total * length(all_hs)} calls"
))

cli::cli_inform("Reporter ISO3 codes: {paste(sort(reporters), collapse = ', ')}")


# -----------------------------------------------------------------------------
# FETCH FUNCTION
# One call per (reporter, hs_code) — pulls all bilateral partners then filters
# to the reporter set. This ensures only within-network edges are retained.
#
# Rate limit: 1 second sleep between calls (free tier = 1 call/sec).
# Progress is printed so you can resume manually if interrupted.
# -----------------------------------------------------------------------------

fetch_one <- function(reporter, hs_code) {
  tryCatch({
    ct_get_data(
      reporter       = reporter,
      partner        = "all_countries",
      commodity_code = hs_code,
      start_date     = YEAR,
      end_date       = YEAR,
      flow_direction = "export"
    ) |>
      filter(partner_iso %in% reporters)
  }, error = function(e) {
    message("  [ERROR] ", reporter, " / HS ", hs_code, " — ", e$message)
    return(NULL)
  })
}


# -----------------------------------------------------------------------------
# MAIN LOOP
# Loops over reporters × HS codes. Results accumulated in list, then bound.
# Saves a checkpoint CSV every 50 calls so progress is not lost on interruption.
# -----------------------------------------------------------------------------

results    <- list()
call_count <- 0
checkpoint_every <- 50
checkpoint_path  <- file.path(OUTPUT_DIR, "semiconductor_network_checkpoint.csv")

for (rep in reporters) {
  for (hs in all_hs) {
    
    call_count <- call_count + 1
    message(sprintf("[%d/%d] %s — HS %s",
                    call_count, n_total * length(all_hs), rep, hs))
    
    dat <- fetch_one(rep, hs)
    
    if (!is.null(dat) && nrow(dat) > 0) {
      results[[length(results) + 1]] <- dat
    }
    
    # Checkpoint save every N calls 
    if (call_count %% checkpoint_every == 0) {
      bind_rows(results) |>
        write_csv(checkpoint_path)
      message("  [checkpoint saved — ", call_count, " calls complete]")
    }
    
    Sys.sleep(1)   # respect 1 call/sec rate limit
  }
}

cli::cli_inform("Loop complete: {call_count} API calls made")


# -----------------------------------------------------------------------------
# CLEAN & SAVE
# -----------------------------------------------------------------------------

network_df <- bind_rows(results) |>
  left_join(hs_layer_map, by = c("cmd_code" = "hs_code")) |>
  select(
    layer,
    hs_code         = cmd_code,
    hs_desc         = cmd_desc,
    year            = ref_year,
    reporter_code   = reporter_iso,
    reporter        = reporter_desc,
    partner_code    = partner_iso,
    partner         = partner_desc,
    trade_value_usd = primary_value
  ) |>
  filter(
    !is.na(trade_value_usd),
    trade_value_usd >= MIN_FLOW,
    reporter_code   != partner_code,      # drop self-loops
    reporter_code   %in% reporters,
    partner_code    %in% reporters
  )

# Attach selection metadata for provenance tracking
network_df <- network_df |>
  left_join(
    country_selection |>
      select(reporter_code, reporter_rank = rank,
             reporter_export_share = export_share,
             reporter_selection_reason = selection_reason),
    by = "reporter_code"
  ) |>
  mutate(
    reporter_selection_reason = case_when(
      reporter_code %in% forced_additions ~ "forced_nordic_comparator",
      TRUE ~ reporter_selection_reason
    )
  )

output_path <- file.path(OUTPUT_DIR, "semiconductor_network.csv")
write_csv(network_df, output_path)

# Remove checkpoint now that full file is saved
if (file.exists(checkpoint_path)) file.remove(checkpoint_path)

cli::cli_inform(c(
  "Saved: {output_path}",
  "  Rows: {nrow(network_df)}",
  "  Reporter-partner pairs: {n_distinct(paste(network_df$reporter_code, network_df$partner_code))}",
  "  Layer 1 (frontend) rows: {sum(network_df$layer == 'layer1_frontend', na.rm = TRUE)}",
  "  Layer 2 (backend) rows:  {sum(network_df$layer == 'layer2_backend',  na.rm = TRUE)}"
))

# Quick Norway check
cli::cli_inform("\n--- Norway flows ---")
network_df |>
  filter(reporter_code == "NOR" | partner_code == "NOR") |>
  mutate(direction = if_else(reporter_code == "NOR", "export", "import")) |>
  group_by(direction, layer) |>
  summarise(
    n_partners      = n_distinct(if_else(direction == "export", partner_code, reporter_code)),
    total_trade_usd = sum(trade_value_usd),
    .groups = "drop"
  ) |>
  print()