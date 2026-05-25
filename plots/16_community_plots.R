# plots/16_community_plots.R — Community Structure Visualisations
#
# Produces community-detection visualisations:
#   A. Community membership heatmap — country × network membership across layers
#
# No igraph dependency for plot rendering — community membership is extracted
# to plain data frames. igraph:: prefix used for any igraph calls.
#
# Inputs (all via readRDS):
#   data/processed/communities.rds         — named list: fe_2019, fe_2022, be_2019, be_2022
#
# Outputs (PDF, no screen display):
#   thesis_project/plots/output/comm_membership_heat.pdf  — country × network membership heatmap
#
# Run from project root: Rscript plots/16_community_plots.R

library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(igraph)    # loaded for igraph:: calls; all calls use igraph:: prefix

source("config.R")

# ── Guard: check inputs ───────────────────────────────────────────────────────
inputs <- c(
  communities  = "data/processed/communities.rds"
)

missing <- inputs[!file.exists(inputs)]
if (length(missing) > 0) {
  stop("Missing inputs:\n",
       paste(" ", names(missing), "->", missing, collapse = "\n"),
       "\nRun analyses/10_community_detection.R first.")
}

dir.create(DIRS$figures, recursive = TRUE, showWarnings = FALSE)

# ── Load inputs ───────────────────────────────────────────────────────────────
communities    <- readRDS(inputs["communities"])   # named list: fe_2019, fe_2022, be_2019, be_2022

message("Inputs loaded: ", length(communities), " community objects.")

# ── Shared constants ──────────────────────────────────────────────────────────
COL_NOR     <- "red3"      # Norway highlight
CAPTION_SRC <- "Sources: UN Comtrade; OECD BTIGE. Author's calculations."

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
# A. Country × network membership heatmap CONSIDER REMOVING??
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

ggsave(file.path(DIRS$figures, "comm_membership_heat.pdf"), plot = p_membership_heat,
       width = 9, height = 12, device = "pdf")
message("Saved: ", file.path(DIRS$figures, "comm_membership_heat.pdf"))


message("\n16_community_plots.R complete — 1 PDF written to ", DIRS$figures)
