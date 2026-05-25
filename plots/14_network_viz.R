# plots/14_network_viz.R — ggraph Network Visualisations
#
# Renders Fruchterman-Reingold network layouts for all four layer × year
# combinations. Node size = out-strength; fill = Louvain community; Norway
# is highlighted as a diamond shape. Edge width and alpha scale with
# bilateral market share.
#
# All igraph vertex/edge attribute access uses the igraph:: prefix so this
# script is safe after statnet has been loaded elsewhere in the project.
# ggraph's own functions (ggraph(), geom_edge_link(), etc.) need no prefix.
#
# Inputs (all via readRDS):
#   data/processed/graph_frontend_2019.rds
#   data/processed/graph_frontend_2022.rds
#   data/processed/graph_backend_2019.rds
#   data/processed/graph_backend_2022.rds
#   data/processed/centrality_all.rds      — for str_out node sizes
#   data/processed/communities.rds         — named list of community objects
#
# Outputs (PDF, no screen display):
#   thesis_project/plots/output/net_fe_2019.pdf           — frontend 2019 (robustness)
#   thesis_project/plots/output/net_be_2019.pdf           — backend 2019 (robustness)
#   thesis_project/plots/output/net_combined_2022.pdf     — patchwork 2-panel (2022 only)
#   thesis_project/plots/output/net_combined_2019.pdf     — patchwork 2-panel (2019 robustness)
#
# Run from project root: Rscript plots/14_network_viz.R

library(igraph)       # loaded for class dispatch; all calls use igraph:: prefix
library(ggraph)
library(ggplot2)
library(patchwork)
library(dplyr)

source("config.R")

# ── Guard: check inputs ───────────────────────────────────────────────────────
inputs <- c(
  fe_2019   = "data/processed/graph_frontend_2019.rds",
  fe_2022   = "data/processed/graph_frontend_2022.rds",
  be_2019   = "data/processed/graph_backend_2019.rds",
  be_2022   = "data/processed/graph_backend_2022.rds",
  centrality = "data/processed/centrality_all.rds",
  communities = "data/processed/communities.rds"
)

missing <- inputs[!file.exists(inputs)]
if (length(missing) > 0) {
  stop("Missing inputs:\n",
       paste(" ", names(missing), "->", missing, collapse = "\n"),
       "\nRun create_data/05-06 and analyses/09-10 first.")
}

dir.create(DIRS$figures, recursive = TRUE, showWarnings = FALSE)

# ── Load inputs ───────────────────────────────────────────────────────────────
g_fe_19 <- readRDS(inputs["fe_2019"])
g_fe_22 <- readRDS(inputs["fe_2022"])
g_be_19 <- readRDS(inputs["be_2019"])
g_be_22 <- readRDS(inputs["be_2022"])

centrality_all <- readRDS(inputs["centrality"])
communities    <- readRDS(inputs["communities"])   # named list: fe_2019, fe_2022, be_2019, be_2022

message("All inputs loaded.")

# ── attach plot attributes ─────────────────────────────────────
#
# Attach three vertex attributes to each graph before passing to ggraph:
#   community — integer community ID from Louvain (igraph::membership())
#   str_out   — out-strength from centrality_all (matched on iso3)
#   is_norway — logical: TRUE for the focal country
#
# igraph::V() and igraph::membership() used throughout.

decorate <- function(g, comm, cent_df, layer_label, yr) {
  node_names <- igraph::V(g)$name

  # Community membership — igraph::membership() returns a named integer vector
  igraph::V(g)$community <- as.integer(
    igraph::membership(comm)[node_names]
  )

  # Out-strength from centrality data frame
  cent_sub <- cent_df |>
    filter(layer == layer_label, year == yr) |>
    select(iso3, strength_out)

  igraph::V(g)$str_out <- cent_sub$strength_out[
    match(node_names, cent_sub$iso3)
  ]

  # Norway flag
  igraph::V(g)$is_norway <- node_names == FOCAL_COUNTRY

  g
}

g_fe_22 <- decorate(g_fe_22, communities$fe_2022, centrality_all, "Front-end", 2022)
g_be_22 <- decorate(g_be_22, communities$be_2022, centrality_all, "Back-end",  2022)
g_fe_19 <- decorate(g_fe_19, communities$fe_2019, centrality_all, "Front-end", 2019)
g_be_19 <- decorate(g_be_19, communities$be_2019, centrality_all, "Back-end",  2019)

message("Graph attributes decorated.")

# ── Shared theme ──────────────────────────────────────────────────────────────
NET_CAPTION <- "Sources: UN Comtrade; Taiwan ITA. igraph + ggraph. Author's calculations."
notaiwan_CAPTION <- "Sources: UN Comtrade. Taiwan (TWN) excluded — ITA data 2022 only. igraph + ggraph. Author's calculations."
SUBTITLE    <- "Node size = out-strength  │  Fill = Louvain community │  Edge thickness = trade volume "

# ── Core plot function ────────────────────────────────────────────────────────
#
# layout = "fr" (Fruchterman-Reingold) is set INSIDE ggraph() so the same
# seed controls the layout; set.seed() is called immediately before ggraph().
# The layout is not stored as a separate object — ggraph computes it
# when the plot is built by ggsave().

network_plot <- function(g, title_str, caption_str = NET_CAPTION) {
  set.seed(42)
  ggraph(g, layout = "fr") +
    geom_edge_link(
      aes(width = weight_marketshare, alpha = weight_marketshare),
      colour      = "grey60",
      show.legend = FALSE
    ) +
    geom_node_point(
      aes(size   = str_out,
          fill   = as.factor(community),
          shape  = is_norway),
      colour = "grey30",
      stroke = 0.4
    ) +
    geom_node_text(
      aes(label    = name,
          colour   = is_norway,
          fontface = ifelse(is_norway, "bold", "plain")),
      repel        = TRUE,
      size         = 3,
      max.overlaps = 50
    ) +
    scale_edge_width(range = c(0.2, 2.5)) +
    scale_edge_alpha(range = c(0.1, 0.5)) +
    scale_size(range = c(2, 12), guide = "none") +
    scale_fill_brewer(palette = "Set2", name = "Community") +
    scale_shape_manual(
      values = c("FALSE" = 21, "TRUE" = 23),
      guide  = "none"
    ) +
    scale_colour_manual(
      values = c("FALSE" = "grey20", "TRUE" = "red3"),
      guide  = "none"
    ) +
    guides(
      fill = guide_legend(
        override.aes = list(shape = 21, size = 5, colour = "grey20")
      )
    ) +
    labs(
      title    = title_str,
      subtitle = SUBTITLE,
      caption  = caption_str
    ) +
    theme_graph(base_family = "sans") +
    theme(legend.position = "right")
}

# ── Build all four individual plots ──────────────────────────────────────────

p_fe_22 <- network_plot(g_fe_22, "Front-end Layer — 2022")
p_be_22 <- network_plot(g_be_22, "Back-end Layer — 2022")
p_fe_19 <- network_plot(g_fe_19, "Front-end Layer — 2019 (Robustness)", 
                        caption_str = notaiwan_CAPTION)
p_be_19 <- network_plot(g_be_19, "Back-end Layer — 2019 (Robustness)",  
                        caption_str = notaiwan_CAPTION)

# ── Save individual plots ─────────────────────────────────────────────────────

ggsave(file.path(DIRS$figures, "net_fe_2019.pdf"), plot = p_fe_19,
       width = 11, height = 9, device = "pdf")
message("Saved: ", file.path(DIRS$figures, "net_fe_2019.pdf"))

ggsave(file.path(DIRS$figures, "net_be_2019.pdf"), plot = p_be_19,
       width = 11, height = 9, device = "pdf")
message("Saved: ", file.path(DIRS$figures, "net_be_2019.pdf"))

# ── Combined 2022 (side-by-side, thesis figure) ───────────────────────────────
#
# patchwork combines ggraph objects like any ggplot. The legend from the right
# panel is kept; the left panel's legend is suppressed to avoid duplication.

p_combined_22 <- (
  p_fe_22 + theme(legend.position = "none") |
  p_be_22
) +
  plot_annotation(
    title   = "Global Semiconductor Trade Networks — 2022",
    caption = NET_CAPTION
  )

ggsave(file.path(DIRS$figures, "net_combined_2022.pdf"), plot = p_combined_22,
       width = 18, height = 9, device = "pdf")
message("Saved: ", file.path(DIRS$figures, "net_combined_2022.pdf"))

# ── Combined 2019 (side-by-side, robustness appendix figure) ────────────────
p_combined_19 <- (
  p_fe_19 + theme(legend.position = "none") |
    p_be_19
) +
  plot_annotation(
    title   = "Global Semiconductor Trade Networks — 2019 (Robustness)",
    caption = "Sources: UN Comtrade. Taiwan (TWN) excluded from 2019 networks (ITA data 2022 only). igraph + ggraph. Author's calculations."
  )

ggsave(file.path(DIRS$figures, "net_combined_2019.pdf"), 
       plot = p_combined_19,
       width = 18, height = 9, device = "pdf")
message("Saved: ", file.path(DIRS$figures, "net_combined_2019.pdf"))

message("\n14_network_viz.R complete — 4 PDFs written to ", DIRS$figures)
