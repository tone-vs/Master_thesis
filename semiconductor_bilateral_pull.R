# =============================================================================
# Semiconductor Value Chain — Bilateral Trade Data
# Thesis: Mapping Norway in the Global Semiconductor Value Chain
# =============================================================================
# Reporter selection justified by two criteria:
#   1. Literature list  — countries identified as core GVC nodes in
#      Varas et al. (2021) BCG/SIA report and OECD (2023) semiconductor
#      GVC mapping. Norway added as focal country (Ponte & Sturgeon 2014).
#   2. Flow threshold   — bilateral flows below $1M USD dropped as noise
#      (standard in Comtrade-based network studies, e.g. Amador & Cabral 2016)
#
# API: UN Comtrade v2 — free tier: 250 calls/day, 1 call/sec
# Get a key at: https://comtradeplus.un.org/
# Install: install.packages(c("httr2", "dplyr", "readr", "purrr", "cli"))
# =============================================================================

library(httr2)
library(dplyr)
library(readr)
library(purrr)
library(cli)
library(comtradr)

# -----------------------------------------------------------------------------
# SETTINGS
# -----------------------------------------------------------------------------

set_primary_comtrade_key(Sys.getenv("COMTRADE_PRIMARY_KEY"))  # add to ~/.Renviron
YEAR       <- 2022
FLOW       <- "X"          # X = exports (who sells to whom)
MIN_FLOW   <- 1e6          # drop flows below $1M (noise filter)
OUTPUT_DIR <- "data/semiconductor"
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# HS CODES — two layers
# -----------------------------------------------------------------------------

hs_layer1 <- c(
  "381800",  # Silicon wafers
  "848610",  # Machines — semiconductor boules/wafers
  "848620",  # Machines — semiconductor devices/ICs
  "848630",  # Machines — flat panel displays
  "848640",  # Machines — mask/reticle repair
  "848690",  # Parts & accessories for 8486 equipment
  "903082"   # Instruments for checking wafers/devices
)

hs_layer2 <- c(
  "854110",  # Diodes
  "854121",  # Transistors < 1W
  "854129",  # Transistors >= 1W
  "854130",  # Thyristors, diacs, triacs
  "854160",  # Mounted piezo-electric crystals
  "854190",  # Parts for diodes/transistors/photosensitive
  "854231",  # Processors and controllers
  "854232",  # Memories
  "854233",  # Amplifiers
  "854239",  # ICs n.e.c.
  "854290",  # Parts of ICs
  "852351",  # Flash memory cards
  "852352",  # Smart cards
  "852359"   # Semiconductor media (unrecorded)
)

hs_layer_map <- bind_rows(
  tibble(hs_code = hs_layer1, layer = "layer1_frontend"),
  tibble(hs_code = hs_layer2, layer = "layer2_backend")
)

# -----------------------------------------------------------------------------
# REPORTER LIST
# Justification 1: literature-anchored core GVC nodes
# Source: Varas et al. (2021) BCG/SIA; OECD (2023) semiconductor GVC mapping
# Norway: focal country, included by research design (Ponte & Sturgeon 2014)
#         Role: REC Silicon (polysilicon/wafers); Elkem (silicon metal) —
#         upstream materials supplier in the front-end layer
# -----------------------------------------------------------------------------


reporters <- c(
  "USA","KOR","TWN","JPN","CHN","NLD","DEU",
  "MYS","SGP","VNM","IRL","ISR","FRA","AUT",
  "IND","NOR","SWE","FIN","DNK"
)

cli::cli_inform("Reporter set: {nrow(reporters)} countries (15 literature-anchored + Norway as focal country + 3 nordic countries)")

# -----------------------------------------------------------------------------
# FETCH FUNCTION — one call per (reporter, hs_code)
# Returns all bilateral partners in a single call
# Total calls: 16 reporters x 21 HS codes = 336 calls (~2 days free tier)
# -----------------------------------------------------------------------------

# --- FIXED FETCH FUNCTION ---
fetch_one <- function(reporter, hs_code) {
  tryCatch({
    ct_get_data(
      reporter       = reporter,
      partner        = "all_countries",
      commodity_code = hs_code,
      start_date     = 2022,
      end_date       = 2022,
      flow_direction = "export"
    ) |>
      dplyr::filter(partner_iso %in% reporters)
  }, error = function(e) {
    message("Error: ", reporter, " / ", hs_code)
    return(NULL)
  })
}

# --- MAIN LOOP ---
all_hs <- c(hs_layer1, hs_layer2)
results <- list()

for (rep in reporters) {
  for (hs in all_hs) {
    message("Running: ", rep, " - ", hs)
    dat <- fetch_one(rep, hs)
    if (!is.null(dat) && nrow(dat) > 0) {
      results[[length(results) + 1]] <- dat
    }
    Sys.sleep(1)
  }
}

final_data <- bind_rows(results)

# --- CLEAN & SAVE ---
network_df <- final_data |>
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
    partner_code %in% reporters
  )

write_csv(network_df, file.path(OUTPUT_DIR, "semiconductor_network.csv"))
cli::cli_inform("Saved: {OUTPUT_DIR}/semiconductor_network.csv  ({nrow(network_df)} rows)")