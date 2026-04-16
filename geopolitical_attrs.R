# =============================================================================
# geopolitical_attrs.R — Node and dyadic geopolitical attributes
# Project: Mapping Norway in the Global Semiconductor Value Chain
# =============================================================================
# Outputs:
#   node_geo   — tibble, one row per country, node-level attributes
#   dyad_geo   — tibble, one row per directed dyad, dyadic covariates
#   Used in:   build_network_data.R (node join) and ERGM script (dyadic matrix)
#
# Run AFTER build_network_data.R so `nodes` tibble is available for
# the country set.
# =============================================================================

library(dplyr)
library(tidyr)
library(WDI)        # install.packages("WDI")
library(unvotes)    # install.packages("unvotes")

YEAR_GDP  <- 2022
OUT_DIR   <- "data/processed"

# Country ISO3 set — pull from nodes tibble (already in environment)
stopifnot(exists("nodes"))
iso3_set <- nodes$iso3


# -----------------------------------------------------------------------------
# 1. GDP — World Bank via WDI package
# -----------------------------------------------------------------------------

gdp_raw <- WDI(
  country   = iso3_set,
  indicator = c(gdp_usd    = "NY.GDP.MKTP.CD",
                gdp_pc_usd = "NY.GDP.PCAP.CD"),
  start     = YEAR_GDP,
  end       = YEAR_GDP,
  extra     = FALSE
)

gdp <- gdp_raw |>
  filter(year == YEAR_GDP) |>
  select(iso3 = iso3c, gdp_usd, gdp_pc_usd) |>
  mutate(
    gdp_log    = log1p(gdp_usd),
    gdp_pc_log = log1p(gdp_pc_usd)
  )

# WDI uses ISO2 internally but returns iso3c — verify NOR is present
message("GDP rows matched: ", nrow(gdp), " / ", length(iso3_set))
message("Norway GDP (USD): ", gdp |> filter(iso3 == "NOR") |> pull(gdp_usd))


# -----------------------------------------------------------------------------
# 2. Alliance / bloc membership — coded manually
#    Source: NATO (nato.int), EU (europa.eu) membership as of 2022
#    Rationale: semiconductor friendshoring literature focuses on NATO/EU
#    alignment as the key Western alliance structure (Babones 2023;
#    Bown 2023 PIIE)
# -----------------------------------------------------------------------------

alliance_df <- tribble(
  ~iso3,  ~nato, ~eu_member,
  "USA",     1L,         0L,
  "KOR",     0L,         0L,
  "JPN",     0L,         0L,
  "CHN",     0L,         0L,
  "TWN",     0L,         0L,
  "NLD",     1L,         1L,
  "DEU",     1L,         1L,
  "MYS",     0L,         0L,
  "SGP",     0L,         0L,
  "VNM",     0L,         0L,
  "IRL",     0L,         1L,
  "ISR",     0L,         0L,
  "FRA",     1L,         1L,
  "AUT",     0L,         1L,
  "IND",     0L,         0L,
  "NOR",     1L,         0L,   # NATO yes, EU no (EEA member)
  "SWE",     1L,         1L,   # NATO member from March 2024 — code as 1
  "FIN",     1L,         1L,   # NATO member from April 2023 — code as 1
  "DNK",     1L,         1L
)

# Note: SWE and FIN formally joined NATO after 2022. For 2022 baseline,
# consider coding them 0 and running sensitivity check with 1.


# -----------------------------------------------------------------------------
# 3. UN General Assembly voting similarity — dyadic covariate
#    Package: unvotes (Robison & Voeten 2021)
#    Metric:  agreement rate on all UNGA votes in a given year range
#    Used in: ERGM as dyadic covariate for friendshoring hypothesis
# -----------------------------------------------------------------------------

# un_votes and un_roll_calls are data objects loaded with the unvotes package
unga_similarity <- un_votes |>
  inner_join(un_roll_calls |> select(rcid, date), by = "rcid") |>
  mutate(year = lubridate::year(date)) |>
  filter(year %in% 2019:2022) |>
  # Keep only countries in our set — convert ISO2 to ISO3 for matching
  mutate(iso3 = countrycode::countrycode(country_code, "iso2c", "iso3c")) |>
  filter(iso3 %in% iso3_set) |>
  select(rcid, iso3, vote)

# Compute pairwise agreement rate for all directed dyads
# Agreement = both voted yes, both voted no, or both abstained
country_pairs <- expand_grid(iso3_i = iso3_set, iso3_j = iso3_set) |>
  filter(iso3_i != iso3_j)

vote_pairs <- unga_similarity |>
  rename(iso3_i = iso3, vote_i = vote) |>
  inner_join(
    unga_similarity |> rename(iso3_j = iso3, vote_j = vote),
    by = "rcid"
  ) |>
  filter(iso3_i != iso3_j)

dyad_unga <- vote_pairs |>
  group_by(iso3_i, iso3_j) |>
  summarise(
    n_votes   = n(),
    n_agree   = sum(vote_i == vote_j, na.rm = TRUE),
    unga_sim  = n_agree / n_votes,   # ranges 0–1; higher = more aligned
    .groups   = "drop"
  )

message("UNGA dyads computed: ", nrow(dyad_unga))
message("Norway–USA alignment:  ",
        dyad_unga |> filter(iso3_i == "NOR", iso3_j == "USA") |> pull(unga_sim) |> round(3))
message("Norway–China alignment: ",
        dyad_unga |> filter(iso3_i == "NOR", iso3_j == "CHN") |> pull(unga_sim) |> round(3))


# -----------------------------------------------------------------------------
# 4. Combine node-level attributes
# -----------------------------------------------------------------------------

node_geo <- nodes |>
  select(iso3, name, is_focal) |>
  left_join(gdp,         by = "iso3") |>
  left_join(alliance_df, by = "iso3") |>
  mutate(
    nato       = replace_na(nato,       0L),
    eu_member  = replace_na(eu_member,  0L),
    # Western bloc: NATO or EU membership — used as community prior in analysis
    western    = as.integer(nato == 1 | eu_member == 1)
  )


# -----------------------------------------------------------------------------
# 5. Save
# -----------------------------------------------------------------------------

write_csv(node_geo,      file.path(OUT_DIR, "node_geopolitical.csv"))
write_csv(dyad_unga,     file.path(OUT_DIR, "dyad_unga_similarity.csv"))

message("\nSaved: node_geopolitical.csv (", nrow(node_geo), " countries)")
message("Saved: dyad_unga_similarity.csv (", nrow(dyad_unga), " dyads)")
message("Join node_geo onto node_attributes.csv in analysis script.")