# create_data/02b_patch_811292.R
#
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# DO NOT RUN if 02_comtrade_pull.R was executed after 811292 was added to
# hs_layer1 in config.R. Running this patch on data that already contains
# 811292 will produce duplicate rows in semiconductor_network.csv.
#
# This script is retained for reproducibility documentation only. It was a
# one-time fix applied when 811292 was missing from the original pull.
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# Targeted patch: add HS 811292 (gallium, germanium, hafnium, rhenium, vanadium)
# to the existing semiconductor network data.
#
# Why: China produces 98% of world gallium and used export controls in 2023,
# making this a critical weaponized interdependence chokepoint omitted from
# the original pull. See OECD (2025) pp.21-22.
#
# API calls: ~30 reporters × 2 years = ~60 calls (~1 minute)
# ============================================================

library(comtradr)
library(dplyr)
library(readr)
library(purrr)

source("config.R")

set_primary_comtrade_key(Sys.getenv("COMTRADE_PRIMARY_KEY"))

# ── Load existing data ────────────────────────────────────────────────────────
existing_path <- file.path("data/raw/semiconductor", "semiconductor_network.csv")
if (!file.exists(existing_path)) {
  stop("semiconductor_network.csv not found. Run 02_comtrade_pull.R first.")
}

existing <- read_csv(existing_path, show_col_types = FALSE)
cat("Existing rows:", nrow(existing), "\n")

# ── Load reporter set ─────────────────────────────────────────────────────────
country_selection <- read_csv("data/processed/country_selection.csv",
                              show_col_types = FALSE)
threshold_reporters <- country_selection |>
  filter(selected) |>
  pull(reporter_code)
reporters <- union(threshold_reporters, FORCED_INCLUSIONS)

cat("Reporters:", length(reporters), "\n")
cat("Estimated API calls:", length(reporters) * length(YEARS), "\n")

# ── Fetch 811292 only ─────────────────────────────────────────────────────────
fetch_811292 <- function(reporter, yr) {
  message(sprintf("Fetching 811292 — %s %d", reporter, yr))
  tryCatch({
    ct_get_data(
      reporter       = reporter,
      partner        = "all_countries",
      commodity_code = "811292",
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

# Pull for all reporters and years
patch_raw <- list()
for (yr in YEARS) {
  for (rep in reporters) {
    dat <- fetch_811292(rep, yr)
    if (!is.null(dat) && nrow(dat) > 0) {
      patch_raw[[length(patch_raw) + 1]] <- dat
    }
    Sys.sleep(1)  # 1 call/sec rate limit
  }
  cat("Year", yr, "complete\n")
}

if (length(patch_raw) == 0) {
  stop("No data returned for 811292. Check API key and reporter set.")
}

# ── Clean patch data ──────────────────────────────────────────────────────────
patch_df <- patch_raw |>
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
    reporter_code   != partner_code,
    reporter_code   %in% reporters,
    partner_code    %in% reporters
  ) |>
  # Add selection metadata to match existing structure
  left_join(
    country_selection |>
      select(reporter_code,
             reporter_rank             = rank,
             reporter_export_share     = export_share,
             reporter_selection_reason = selection_reason),
    by = "reporter_code"
  )

cat("\nPatch rows fetched:", nrow(patch_df), "\n")
cat("By year:\n")
print(table(patch_df$year))
cat("Top exporters of 811292:\n")
patch_df |>
  group_by(reporter_code, year) |>
  summarise(total = sum(trade_value_usd, na.rm = TRUE) / 1e6, .groups = "drop") |>
  arrange(desc(total)) |>
  head(10) |>
  print()

# ── Append to existing data ───────────────────────────────────────────────────
# Ensure column types match before binding
existing <- existing |>
  mutate(
    hs_code              = as.character(hs_code),
    reporter_rank        = as.numeric(reporter_rank),
    reporter_export_share = as.numeric(reporter_export_share)
  )

patch_df <- patch_df |>
  mutate(
    hs_code              = as.character(hs_code),
    reporter_rank        = as.numeric(reporter_rank),
    reporter_export_share = as.numeric(reporter_export_share)
  )

network_updated <- bind_rows(existing, patch_df)

cat("\nUpdated rows:", nrow(network_updated),
    "(added", nrow(patch_df), "rows)\n")

# Verify no duplicates
dup_check <- network_updated |>
  group_by(year, reporter_code, partner_code, hs_code) |>
  filter(n() > 1) |>
  nrow()
cat("Duplicate rows:", dup_check, "(should be 0)\n")

# ── Save updated files ────────────────────────────────────────────────────────
write_csv(network_updated, existing_path)
saveRDS(network_updated, file.path("data/processed", "semiconductor_network.rds"))

cat("\nSaved updated semiconductor_network.csv and .rds\n")
cat("Layer 1 rows now:", sum(network_updated$layer == "layer1_frontend", na.rm = TRUE), "\n")
cat("Layer 2 rows now:", sum(network_updated$layer == "layer2_backend",  na.rm = TRUE), "\n")
cat("\nNext: re-run create_data/05_build_network_data.R\n")