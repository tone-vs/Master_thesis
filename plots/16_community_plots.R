# plots/16_community_plots.R — Community Structure Visualisations
#
# Produces all community-detection and geopolitical alignment plots:
#   A. Community size distribution — bar chart (faceted by layer × year)
#   B. Community membership table — heatmap of country membership across layers
#   C. Norway's community neighbours — bar chart of co-membership counts
#   D. UNGA similarity distribution — histogram, all dyads vs Norway dyads
#   E. Norway UNGA alignment — top-N partners bar chart (FE and BE, 2022)
#   F. Modularity comparison — grouped bar (all four layer × year networks)
#
# No igraph dependency for plot rendering — community membership is extracted
# to plain data frames. igraph:: prefix used for any igraph calls.
#
# Inputs (all via readRDS):
#   data/processed/communities.rds         — named list: fe_2019, fe_2022, be_2019, be_2022
#   data/processed/centrality_all.rds      — node iso3, layer, year, is_norway
#   data/processed/dyad_unga_similarity.rds — long-form dyad UNGA similarity
#
# Outputs (PDF, no screen display):
#   plots/output/comm_size_dist.pdf        — community size distribution
#   plots/output/comm_membership_heat.pdf  — country × network membership heatmap
#   plots/output/comm_norway_neighbours.pdf — Norway co-membership bar chart
#   plots/output/comm_unga_hist.pdf        — UNGA similarity histogram
#   plots/output/comm_norway_unga.pdf      — Norway UNGA alignment bar chart
#   plots/output/comm_modularity.pdf       — modularity comparison bar chart
#
# Run from project root: Rscript plots/16_community_plots.R

library(ggplot2)
library(ggrepel)
library(patchwork)
library(dplyr)
library(tidyr)
library(forcats)
library(igraph)    # loaded for igraph:: calls; all calls use igraph:: prefix

source("config.R")

# ── Guard: check inputs ───────────────────────────────────────────────────────
inputs <- c(
  communities  = "data/processed/communities.rds",
  centrality   = "data/processed/centrality_all.rds",
  dyad_unga    = "data/processed/dyad_unga_similarity.rds"
)

missing <- inputs[!file.exists(inputs)]
if (length(missing) > 0) {
  stop("Missing inputs:\n",
       paste(" ", names(missing), "->", missing, collapse = "\n"),
       "\nRun analyses/10_community_detection.R and create_data/06_geopolitical_attrs.R first.")
}

dir.create("plots/output", recursive = TRUE, showWarnings = FALSE)

# ── Load inputs ───────────────────────────────────────────────────────────────
communities    <- readRDS(inputs["communities"])   # named list: fe_2019, fe_2022, be_2019, be_2022
centrality_all <- readRDS(inputs["centrality"])
dyad_unga      <- readRDS(inputs["dyad_unga"])

message("Inputs loaded: ", length(communities), " community objects, ",
        nrow(centrality_all), " centrality observations, ",
        nrow(dyad_unga), " dyadic UNGA pairs.")

# ── Shared constants ──────────────────────────────────────────────────────────
COL_FE     <- "#2C7BB6"   # frontend blue
COL_BE     <- "#D7191C"   # backend red
COL_NOR    <- "red3"      # Norway highlight
COL_OTHER  <- "steelblue"
CAPTION_SRC <- "Sources: UN Comtrade; OECD BTIGE; UNGA voting data. Author's calculations."

# ── Helper: extract membership to a tidy data frame ──────────────────────────
#
# igraph::membership() returns a named integer vector (node → community ID).
# igraph::modularity() returns the modularity score Q.
# igraph::sizes() returns the number of nodes per community.

comm_to_df <- function(comm_obj, layer_label, yr) {
  mem  <- igraph::membership(comm_obj)          # named integer vector
  tibble(
    iso3      = names(mem),
    community = as.integer(mem),
    layer     = layer_label,
    year      = yr
  )
}

comm_df <- bind_rows(
  comm_to_df(communities$fe_2019, "Front-end", 2019),
  comm_to_df(communities$fe_2022, "Front-end", 2022),
  comm_to_df(communities$be_2019, "Back-end",  2019),
  comm_to_df(communities$be_2022, "Back-end",  2022)
) |>
  mutate(
    network   = paste0(layer, " (", year, ")"),
    is_norway = iso3 == FOCAL_COUNTRY
  )

# Modularity scores
modularity_df <- tibble(
  network = c("Front-end (2019)", "Front-end (2022)",
              "Back-end (2019)",  "Back-end (2022)"),
  layer   = c("Front-end", "Front-end", "Back-end", "Back-end"),
  year    = c(2019L, 2022L, 2019L, 2022L),
  Q       = c(
    igraph::modularity(communities$fe_2019),
    igraph::modularity(communities$fe_2022),
    igraph::modularity(communities$be_2019),
    igraph::modularity(communities$be_2022)
  )
)

message("Community membership extracted. Modularity scores:")
print(modularity_df)

# =============================================================================
# A. Community size distribution
#
#   How many countries are in each Louvain community?
#   Faceted by layer × year; communities on x-axis ordered by size.
# =============================================================================

comm_sizes <- comm_df |>
  count(network, layer, year, community, name = "n_countries") |>
  mutate(community = factor(community))

p_size_dist <- ggplot(comm_sizes,
                       aes(x = fct_reorder(community, n_countries, .desc = TRUE),
                           y = n_countries,
                           fill = layer)) +
  geom_col(width = 0.7, alpha = 0.9) +
  facet_wrap(~network, scales = "free_x", ncol = 2) +
  scale_fill_manual(
    values = c("Front-end" = COL_FE, "Back-end" = COL_BE),
    name   = "Layer"
  ) +
  labs(
    title    = "Louvain Community Sizes across Semiconductor Trade Networks",
    subtitle = "Each bar = one community; height = number of member countries",
    x        = "Community ID (ordered by size)",
    y        = "Number of countries",
    caption  = CAPTION_SRC
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text         = element_text(face = "bold"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position    = "bottom"
  )

ggsave("plots/output/comm_size_dist.pdf", plot = p_size_dist,
       width = 11, height = 7, device = cairo_pdf)
message("Saved: plots/output/comm_size_dist.pdf")

# =============================================================================
# B. Country × network membership heatmap
#
#   Rows = countries; columns = the four networks.
#   Fill = community ID. Norway row is highlighted.
#   Restricted to countries present in ALL four networks for clarity.
# =============================================================================

# Pivot wide so each column is a network
comm_wide <- comm_df |>
  select(iso3, network, community) |>
  pivot_wider(names_from = network, values_from = community)

# Keep countries present in all four networks
four_nets   <- c("Front-end (2019)", "Front-end (2022)",
                 "Back-end (2019)",  "Back-end (2022)")
comm_wide_complete <- comm_wide |>
  filter(if_all(all_of(four_nets), ~ !is.na(.)))

# Pivot back to long for ggplot
comm_heat <- comm_wide_complete |>
  pivot_longer(all_of(four_nets), names_to = "network", values_to = "community") |>
  mutate(
    community   = as.factor(community),
    network     = factor(network, levels = four_nets),
    iso3_sorted = fct_reorder(iso3, as.integer(community))
  )

p_membership_heat <- ggplot(comm_heat,
                             aes(x = network, y = iso3_sorted, fill = community)) +
  geom_tile(colour = "white", linewidth = 0.3) +
  # Highlight Norway with a border
  geom_tile(data = filter(comm_heat, iso3 == FOCAL_COUNTRY),
            colour = COL_NOR, linewidth = 1.2, fill = NA) +
  scale_fill_brewer(palette = "Set2", name = "Community") +
  labs(
    title    = "Community Membership across All Four Networks",
    subtitle = paste0(FOCAL_COUNTRY, " row outlined in red. Countries in all four networks only."),
    x        = NULL,
    y        = "Country (ISO3)",
    caption  = CAPTION_SRC
  ) +
  theme_minimal(base_size = 9) +
  theme(
    axis.text.x      = element_text(angle = 25, hjust = 1),
    axis.text.y      = element_text(size = 6),
    panel.grid       = element_blank(),
    legend.position  = "right"
  )

ggsave("plots/output/comm_membership_heat.pdf", plot = p_membership_heat,
       width = 9, height = 12, device = cairo_pdf)
message("Saved: plots/output/comm_membership_heat.pdf")

# =============================================================================
# C. Norway's community neighbours
#
#   Which countries are co-members with Norway in the same community?
#   Bar chart of countries co-assigned to Norway's community across networks.
#   Counts how many of the four networks each country shares with Norway.
# =============================================================================

norway_comm <- comm_df |>
  filter(iso3 == FOCAL_COUNTRY) |>
  select(network, community) |>
  rename(norway_comm = community)

co_members <- comm_df |>
  filter(iso3 != FOCAL_COUNTRY) |>
  left_join(norway_comm, by = "network") |>
  filter(community == norway_comm) |>
  count(iso3, name = "n_networks") |>
  arrange(desc(n_networks))

p_norway_neighbours <- co_members |>
  slice_max(n_networks, n = 30, with_ties = TRUE) |>
  mutate(iso3 = fct_reorder(iso3, n_networks)) |>
  ggplot(aes(x = n_networks, y = iso3)) +
  geom_col(fill = COL_OTHER, width = 0.7, alpha = 0.9) +
  geom_text(aes(label = n_networks), hjust = -0.2, size = 3, colour = "grey30") +
  scale_x_continuous(breaks = 1:4, limits = c(0, 4.5)) +
  labs(
    title    = paste0("Countries co-assigned to ", FOCAL_COUNTRY, "'s Louvain Community"),
    subtitle = "Count of networks (out of 4) where the country shares a community with Norway",
    x        = "Number of networks with co-membership",
    y        = NULL,
    caption  = CAPTION_SRC
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank()
  )

ggsave("plots/output/comm_norway_neighbours.pdf", plot = p_norway_neighbours,
       width = 8, height = 9, device = cairo_pdf)
message("Saved: plots/output/comm_norway_neighbours.pdf")

# =============================================================================
# D. UNGA similarity distribution
#
#   Histogram of all bilateral UNGA similarity scores.
#   Norway's dyads overlaid in red to show where Norway sits
#   relative to the global distribution (friendshoring hypothesis).
# =============================================================================

unga_all <- dyad_unga |>
  filter(!is.na(unga_sim))

unga_norway <- unga_all |>
  filter(iso3_i == FOCAL_COUNTRY | iso3_j == FOCAL_COUNTRY)

p_unga_hist <- ggplot(unga_all, aes(x = unga_sim)) +
  geom_histogram(
    binwidth = 0.02, fill = COL_OTHER, alpha = 0.5,
    colour = "white"
  ) +
  geom_histogram(
    data     = unga_norway,
    aes(x    = unga_sim),
    binwidth = 0.02, fill = COL_NOR, alpha = 0.7,
    colour   = "white"
  ) +
  labs(
    title    = "Distribution of UNGA Voting Similarity Scores",
    subtitle = paste0("All dyads (blue) vs dyads involving ", FOCAL_COUNTRY,
                      " (red). Higher = more aligned."),
    x        = "UNGA cosine similarity",
    y        = "Number of dyads",
    caption  = CAPTION_SRC
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave("plots/output/comm_unga_hist.pdf", plot = p_unga_hist,
       width = 9, height = 5, device = cairo_pdf)
message("Saved: plots/output/comm_unga_hist.pdf")

# =============================================================================
# E. Norway UNGA alignment — top-N trading partners by UNGA similarity
#
#   Horizontal bar chart showing Norway's UNGA alignment score with its
#   most important trading partners (identified via centrality_all).
#   Two panels: highest vs lowest UNGA alignment among Norway's partners.
# =============================================================================

# Identify Norway's top trading partners from 2022 centrality data
# (countries that are nodes in the network)
network_countries <- centrality_all |>
  filter(year == 2022, layer == "Front-end") |>
  pull(iso3) |>
  unique()

# Norway's UNGA scores with all in-network countries
norway_unga <- unga_all |>
  filter(iso3_i == FOCAL_COUNTRY | iso3_j == FOCAL_COUNTRY) |>
  mutate(partner = if_else(iso3_i == FOCAL_COUNTRY, iso3_j, iso3_i)) |>
  filter(partner %in% network_countries, partner != FOCAL_COUNTRY) |>
  select(partner, unga_sim) |>
  distinct()

n_show <- 20

p_nor_unga_top <- norway_unga |>
  slice_max(unga_sim, n = n_show) |>
  mutate(partner = fct_reorder(partner, unga_sim)) |>
  ggplot(aes(x = unga_sim, y = partner)) +
  geom_col(fill = COL_FE, width = 0.7, alpha = 0.9) +
  geom_text(aes(label = round(unga_sim, 3)), hjust = -0.1, size = 3, colour = "grey30") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)), limits = c(0, 1)) +
  labs(title = paste0("Highest UNGA Alignment with ", FOCAL_COUNTRY),
       x     = "UNGA cosine similarity", y = NULL)

p_nor_unga_bot <- norway_unga |>
  slice_min(unga_sim, n = n_show) |>
  mutate(partner = fct_reorder(partner, unga_sim)) |>
  ggplot(aes(x = unga_sim, y = partner)) +
  geom_col(fill = COL_BE, width = 0.7, alpha = 0.9) +
  geom_text(aes(label = round(unga_sim, 3)), hjust = -0.1, size = 3, colour = "grey30") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)), limits = c(0, 1)) +
  labs(title = paste0("Lowest UNGA Alignment with ", FOCAL_COUNTRY),
       x     = "UNGA cosine similarity", y = NULL)

p_norway_unga <- (p_nor_unga_top | p_nor_unga_bot) +
  plot_annotation(
    title    = paste0(FOCAL_COUNTRY, ": UNGA Voting Alignment with Semiconductor Network Partners"),
    subtitle = "Top 20 most-aligned (blue) and least-aligned (red) countries in the trade network",
    caption  = CAPTION_SRC
  )

ggsave("plots/output/comm_norway_unga.pdf", plot = p_norway_unga,
       width = 13, height = 8, device = cairo_pdf)
message("Saved: plots/output/comm_norway_unga.pdf")

# =============================================================================
# F. Modularity comparison — grouped bar chart
#
#   Compares Louvain modularity Q across all four layer × year combinations.
#   Higher Q = more clearly delineated community structure.
# =============================================================================

p_modularity <- ggplot(modularity_df,
                        aes(x = factor(year), y = Q, fill = layer)) +
  geom_col(position = position_dodge(width = 0.7),
           width = 0.6, alpha = 0.9) +
  geom_text(
    aes(label = round(Q, 3)),
    position = position_dodge(width = 0.7),
    vjust = -0.4, size = 3.5, colour = "grey30"
  ) +
  scale_fill_manual(
    values = c("Front-end" = COL_FE, "Back-end" = COL_BE),
    name   = "Layer"
  ) +
  scale_y_continuous(limits = c(0, max(modularity_df$Q) * 1.2)) +
  labs(
    title    = "Louvain Modularity across Semiconductor Trade Networks",
    subtitle = "Higher Q indicates more distinct community structure",
    x        = "Year",
    y        = "Modularity (Q)",
    caption  = CAPTION_SRC
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position    = "bottom"
  )

ggsave("plots/output/comm_modularity.pdf", plot = p_modularity,
       width = 7, height = 5, device = cairo_pdf)
message("Saved: plots/output/comm_modularity.pdf")

message("\n16_community_plots.R complete — 6 PDFs written to plots/output/")
