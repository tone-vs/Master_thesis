
# 06_geopolitical_attrs.R — Node and Dyadic Geopolitical Attributes
# Purpose: Attach GDP, alliance membership (NATO/EU), and UN General Assembly
#          voting similarity to the node and dyad tables produced by
#          05_build_network_data.R.
# Inputs:  data/processed/node_attributes.csv — produced by 05_build_network_data.R
#          World Bank WDI API (GDP)
#          unvotes package (UNGA roll-call data)
# Outputs: data/processed/node_geopolitical.csv  — node-level attributes
#          data/processed/dyad_unga_similarity.csv — directed dyad covariates
#          Used in:  final_analysis.Rmd


# Libraries 
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)      # year()
library(countrycode)    # countrycode()
library(WDI)            
library(unvotes)        

source("config.R")


node_path <- file.path(DIRS$processed, "node_attributes.csv")

if (!file.exists(node_path)) {
  stop(
    "node_attributes.csv not found.\n",
    "Run 05_build_network_data.R first."
  )
}

# Load node table (produced by 05_build_network_data.R)
nodes <- read_csv(node_path, show_col_types = FALSE)

stopifnot(
  "node_attributes.csv is empty — run 05_build_network_data.R first" =
    nrow(nodes) > 0
)

iso3_set <- nodes$iso3
message("Country set loaded: ", length(iso3_set), " countries")



# 1. GDP — World Bank via WDI package


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
  select(iso3 = iso3c, gdp_usd, gdp_pc_usd)

# Taiwan is not in the World Bank database — add manually.
# Source: IMF World Economic Outlook 2022 (GDP in current USD)
taiwan_gdp <- tibble(
  iso3       = "TWN",
  gdp_usd    = 761.43e9,    # IMF WEO 2022
  gdp_pc_usd = 32315.0      # IMF WEO 2022
)

gdp <- bind_rows(gdp, taiwan_gdp) |>
  mutate(
    gdp_log    = log1p(gdp_usd),
    gdp_pc_log = log1p(gdp_pc_usd)
  )

message("GDP rows matched: ", nrow(gdp), " / ", length(iso3_set))
message("Norway GDP (USD): ", gdp |> filter(iso3 == "NOR") |> pull(gdp_usd))



# 2. Alliance / bloc membership — coded manually
#    Source: NATO (nato.int), EU (europa.eu) membership as of 2022
#    Rationale: semiconductor friendshoring literature focuses on NATO/EU
#    alignment as the key Western alliance structure (Babones 2023;
#    Bown 2023 PIIE)

alliance_df <- tribble(
  ~iso3,  ~nato, ~eu_member, ~chip4, ~wassenaar,
  # Core reporter set
  "USA",     1L,         0L,     1L,         1L,
  "KOR",     0L,         0L,     1L,         1L,
  "JPN",     0L,         0L,     1L,         1L,
  "CHN",     0L,         0L,     0L,         0L,
  "TWN",     0L,         0L,     1L,         0L,  # Chip 4 yes; cannot join Wassenaar (not UN member)
  "NLD",     1L,         1L,     0L,         1L,  # ASML country — Wassenaar unilateral semi controls 2022
  "DEU",     1L,         1L,     0L,         1L,
  "MYS",     0L,         0L,     0L,         0L,
  "SGP",     0L,         0L,     0L,         0L,
  "VNM",     0L,         0L,     0L,         0L,
  "IRL",     0L,         1L,     0L,         1L,
  "ISR",     0L,         0L,     0L,         0L,  # Notable Wassenaar non-member
  "FRA",     1L,         1L,     0L,         1L,
  "AUT",     0L,         1L,     0L,         1L,
  "IND",     0L,         0L,     0L,         1L,  # Joined Wassenaar 2017
  "NOR",     1L,         0L,     0L,         1L,  # NATO + Wassenaar; not Chip 4; not EU (EEA)
  "SWE",     0L,         1L,     0L,         1L,
  "FIN",     0L,         1L,     0L,         1L,
  "DNK",     1L,         1L,     0L,         1L,
  "BEL",     1L,         1L,     0L,         1L,
  "CAN",     1L,         0L,     0L,         1L,
  "CHE",     0L,         0L,     0L,         1L,
  "CZE",     1L,         1L,     0L,         1L,
  "GBR",     1L,         0L,     0L,         1L,
  "HKG",     0L,         0L,     0L,         0L,
  "HUN",     1L,         1L,     0L,         1L,
  "ITA",     1L,         1L,     0L,         1L,
  "MEX",     0L,         0L,     0L,         1L,
  "PHL",     0L,         0L,     0L,         0L,
  "THA",     0L,         0L,     0L,         0L
)


# Note: SWE and FIN formally joined NATO after 2022. 
# Chip 4 (Fab 4): announced May 2022, USA-JPN-KOR-TWN.
# Source: CHIPS and Science Act (2022); CSIS commentary.
#
# Wassenaar Arrangement: 42 members as of 2022.
# Source: wassenaar.org/participating-states/
# Notable non-members in network: CHN, TWN, ISR, MYS, SGP, VNM, HKG, PHL, THA

missing_alliances <- setdiff(iso3_set, alliance_df$iso3)
if (length(missing_alliances) > 0) {
  message("Countries in network missing from alliance_df: ",
          paste(missing_alliances, collapse = ", "))
}


# -----------------------------------------------------------------------------
# 3. UN General Assembly voting similarity — dyadic covariate
#    Package: unvotes (Robison & Voeten 2021)
#    Metric:  agreement rate on all UNGA votes 2019–2022
#    Used in: ERGM as dyadic covariate for friendshoring hypothesis
# -----------------------------------------------------------------------------

unga_similarity <- un_votes |>
  inner_join(un_roll_calls |> select(rcid, date), by = "rcid") |>
  mutate(year = year(date)) |>
  filter(year %in% 2019:2022) |>
  mutate(iso3 = countrycode(country_code, "iso2c", "iso3c")) |>
  filter(iso3 %in% iso3_set) |>
  select(rcid, iso3, vote)

# Pairwise agreement rate for all directed dyads
# Agreement = both voted yes, both voted no, or both abstained
country_pairs <- expand_grid(iso3_i = iso3_set, iso3_j = iso3_set) |>
  filter(iso3_i != iso3_j)

vote_pairs <- unga_similarity |>
  rename(iso3_i = iso3, vote_i = vote) |>
  inner_join(
    unga_similarity |> rename(iso3_j = iso3, vote_j = vote),
    by = "rcid",
    relationship = "many-to-many"
  ) |>
  filter(iso3_i != iso3_j)

dyad_unga <- vote_pairs |>
  group_by(iso3_i, iso3_j) |>
  summarise(
    n_votes  = n(),
    n_agree  = sum(vote_i == vote_j, na.rm = TRUE),
    unga_sim = n_agree / n_votes,   # 0–1; higher = more aligned
    .groups  = "drop"
  )

message("UNGA dyads computed: ", nrow(dyad_unga))
message("Norway–USA alignment:   ",
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
    chip4      = replace_na(chip4,      0L),
    wassenaar  = replace_na(wassenaar,  0L),
    # Western bloc: NATO or EU — general political alignment
    western    = as.integer(nato == 1 | eu_member == 1),
    # Semiconductor bloc: Chip 4 or Wassenaar — semiconductor-specific alignment
    # High semi_bloc + trade tie = friendshoring signal (Bown 2023; Babones 2023)
    semi_bloc  = as.integer(chip4 == 1 | wassenaar == 1)
  )

message("\nNode geo coverage: ", nrow(node_geo), " countries")
message("Norway profile:")
node_geo |> filter(is_focal) |> glimpse()


# -----------------------------------------------------------------------------
# 5. Save
# -----------------------------------------------------------------------------

write_csv(node_geo,  file.path(DIRS$processed, "node_geopolitical.csv"))
write_csv(dyad_unga, file.path(DIRS$processed, "dyad_unga_similarity.csv"))

message("\nSaved: node_geopolitical.csv   (", nrow(node_geo),  " countries)")
message("Saved: dyad_unga_similarity.csv (", nrow(dyad_unga), " dyads)")
message("Join node_geo onto node_attributes.csv in final_analysis.Rmd")
