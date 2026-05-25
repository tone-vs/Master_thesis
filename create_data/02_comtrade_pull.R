# 02_comtrade_pull.R — Bilateral Trade Data Pull

# Reporter selection — two criteria, applied jointly:
#
#   1. Coverage threshold (data-driven):
#      Countries are ranked by total semiconductor exports across all 22 HS
#      codes and added iteratively in descending order until
#      the sample accounts for ≥99% of world semiconductor trade, following
#      OECD (2025). The threshold country set is loaded from:
#        data/processed/country_selection.csv  (produced by 01_country_selection.R)
#
#   2. Forced inclusions:
#      Norway    — focal country (REC Silicon, Elkem; Ponte & Sturgeon 2014)
#      Sweden    — Nordic comparator (Ericsson, Nordic Chip Collaboration 2024)
#      Finland   — Nordic comparator (Nokia, Nordic Chip Collaboration 2024)
#      Denmark   — Nordic comparator (Nordic Chip Collaboration 2024)
#      These four are included regardless of their export rank.
#
#   Flow threshold: bilateral flows below $1M USD are dropped as noise

# API: UN Comtrade v2 — free tier: 250 calls/day, 1 call/sec
#   Get a key at: https://comtradeplus.un.org/
#   Set key:      usethis::edit_r_environ() → COMTRADE_PRIMARY_KEY=your_key
#
# Outputs:
#   data/semiconductor/semiconductor_network.csv
#   data/processed/semiconductor_network.rds
#
# Run from project root: Rscript create_data/02_comtrade_pull.R

library(comtradr)
library(dplyr)
library(readr)
library(purrr)
library(cli)

source("config.R")

set_primary_comtrade_key(Sys.getenv("COMTRADE_PRIMARY_KEY"))

dir.create(DIRS$network, recursive = TRUE, showWarnings = FALSE)

checkpoint_every <- 50



# REPORTER LIST
# Load from country_selection.csv (produced by 01_country_selection.R).


selection_path <- file.path("data/processed", "country_selection.csv")

if (!file.exists(selection_path)) {
  stop(
    "country_selection.csv not found.\n",
    "Run create_data/01_country_selection.R first to generate the data-driven country set."
  )
}

country_selection <- read_csv(selection_path, show_col_types = FALSE)

# Countries selected by the 99% coverage threshold
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
  "  Coverage threshold (>=99%): {n_threshold} countries ({scales::percent(actual_coverage, accuracy = 0.1)} of world semiconductor exports)",
  "  Forced additions (research design): {n_forced} country/countries — {paste(forced_additions, collapse = ', ')}",
  "  Total reporters: {n_total}",
  "  Estimated API calls: {n_total} calls per year x {length(YEARS)} years = {n_total * length(YEARS)} calls total"
))

cli::cli_inform("Reporter ISO3 codes: {paste(sort(reporters), collapse = ', ')}")



# FETCH FUNCTION
# One call per reporter — fetches all HS codes in one request then filters
# to the reporter set. This ensures only within-network edges are retained.
#
# Rate limit: 1 second sleep between calls (free tier = 1 call/sec).


fetch_one <- function(reporter, yr) {
  tryCatch({
    ct_get_data(
      reporter       = reporter,
      partner        = "all_countries",
      commodity_code = all_hs,
      start_date     = yr,
      end_date       = yr,
      flow_direction = "export"
    ) |>
      filter(partner_iso %in% reporters) |>
      mutate(period = as.character(period))
  }, error = function(e) {
    message("  [ERROR] ", reporter, " — ", e$message)
    return(NULL)
  })
}



# PULL FUNCTION (per year)
# Wraps the fetch loop with resume-from-checkpoint logic.
# Each year gets its own checkpoint file so years are independent.
# Checkpoint is deleted on successful completion of that year.


pull_year <- function(yr) {

  checkpoint_path <- file.path(
    "data/semiconductor",
    paste0("semiconductor_network_", yr, "_checkpoint.csv")
  )

  results        <- list()
  call_count     <- 0
  done_reporters <- character(0)

  # Resume from checkpoint if it exists
  if (file.exists(checkpoint_path)) {
    checkpoint_df  <- read_csv(checkpoint_path, show_col_types = FALSE) |>
      mutate(across(everything(), as.character))
    results        <- list(checkpoint_df)
    done_reporters <- checkpoint_df |>
      distinct(reporter_iso) |>
      pull(reporter_iso)
    message("Resuming ", yr, " from checkpoint: ",
            nrow(checkpoint_df), " rows, ",
            length(done_reporters), " reporters already fetched. ",
            "Remaining: ", n_total - length(done_reporters))
  } else {
    message("No checkpoint for ", yr, " — starting from scratch.")
  }

  for (rep in reporters) {

    call_count <- call_count + 1

    if (rep %in% done_reporters) {
      message(sprintf("[%d/%d] %s %d — all HS codes [SKIP]",
                      call_count, n_total, rep, yr))
      next
    }

    message(sprintf("[%d/%d] %s %d — all HS codes",
                    call_count, n_total, rep, yr))

    dat <- fetch_one(rep, yr)

    if (!is.null(dat) && nrow(dat) > 0) {
      results[[length(results) + 1]] <- dat
    }

    if (call_count %% checkpoint_every == 0) {
      results |>
        lapply(\(df) mutate(df, across(everything(), as.character))) |>
        bind_rows() |>
        write_csv(checkpoint_path)
      message("  [checkpoint saved — ", call_count, " calls complete]")
    }

    Sys.sleep(1)   # respect 1 call/sec rate limit
  }

  cli::cli_inform("Year {yr} complete: {call_count} API calls made")

  if (file.exists(checkpoint_path)) file.remove(checkpoint_path)

  results
}



# MAIN — loop over all years


all_results <- list()

for (yr in YEARS) {
  cli::cli_inform("\n========== Fetching {yr} ==========")
  year_results <- pull_year(yr)
  all_results  <- c(all_results, year_results)
}

cli::cli_inform("All years fetched.")



# CLEAN & SAVE
# Combine all years, apply layer join, select and filter to final columns.
# The year column is preserved from ref_year in the raw API response.


network_df <- all_results |>
  lapply(\(df) mutate(df, across(everything(), as.character))) |>
  bind_rows() |>
  mutate(
    ref_year      = as.integer(ref_year),
    primary_value = as.numeric(primary_value)
  ) |>
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

output_path <- file.path(DIRS$network, "semiconductor_network.csv")
write_csv(network_df, output_path)

cli::cli_inform(c(
  "Saved: {output_path}",
  "  Rows: {nrow(network_df)}",
  "  Years: {paste(sort(unique(network_df$year)), collapse = ', ')}",
  "  Reporter-partner pairs: {n_distinct(paste(network_df$reporter_code, network_df$partner_code))}",
  "  Layer 1 (frontend) rows: {sum(network_df$layer == 'layer1_frontend', na.rm = TRUE)}",
  "  Layer 2 (backend) rows:  {sum(network_df$layer == 'layer2_backend',  na.rm = TRUE)}"
))

# Norway diagnostic by year
cli::cli_inform("\n--- Norway flows by year ---")
network_df |>
  filter(reporter_code == "NOR" | partner_code == "NOR") |>
  mutate(direction = if_else(reporter_code == "NOR", "export", "import")) |>
  group_by(year, direction, layer) |>
  summarise(
    n_partners      = n_distinct(if_else(direction == "export", partner_code, reporter_code)),
    total_trade_usd = sum(trade_value_usd),
    .groups = "drop"
  ) |>
  print()

# Save key R object for downstream scripts
saveRDS(network_df, file.path("data/processed", "semiconductor_network.rds"))
message("Saved: semiconductor_network.rds")
message("Next: run create_data/03_taiwan_data.R")
