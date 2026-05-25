# plots/18_core_periphery_viz.R
# Core-periphery visualisation following Ou et al. (2024)
# Run AFTER 14_network_viz.R and analyses/09_centrality.R have completed

library(igraph)
library(ggraph)
library(ggplot2)
library(patchwork)
library(cowplot)
library(dplyr)
library(tibble)

source("config.R")

# ── Guard: check inputs ───────────────────────────────────────────────────────
inputs <- c(
  fe_2022    = "data/processed/graph_frontend_2022.rds",
  be_2022    = "data/processed/graph_backend_2022.rds",
  centrality = "data/processed/centrality_all.rds"
)

missing <- inputs[!file.exists(inputs)]
if (length(missing) > 0) {
  stop("Missing inputs:\n",
       paste(" ", names(missing), "->", missing, collapse = "\n"),
       "\nRun create_data/05 and analyses/09 first.")
}

dir.create(DIRS$figures, recursive = TRUE, showWarnings = FALSE)

# ── Load inputs ───────────────────────────────────────────────────────────────
g_fe_22        <- readRDS(inputs["fe_2022"])
g_be_22        <- readRDS(inputs["be_2022"])
centrality_all <- readRDS(inputs["centrality"])

message("All inputs loaded.")

# ── Classify k-shell into core / semi-periphery / periphery ──────────────────
classify_cp <- function(cent_df) {
  shells     <- sort(unique(cent_df$coreness), decreasing = TRUE)
  max_shell  <- shells[1]
  semi_shell <- shells[2]
  cent_df |>
    mutate(cp_class = factor(case_when(
      coreness == max_shell  ~ "Core",
      coreness == semi_shell ~ "Semi-periphery",
      TRUE                   ~ "Periphery"
    ), levels = c("Core", "Semi-periphery", "Periphery")))
}

# ── Attach cp_class to graph ──────────────────────────────────────────────────
decorate_cp <- function(g, cent_df, layer_label, yr) {
  node_iso3 <- igraph::vertex_attr(g, "iso3")
  cent_sub  <- classify_cp(
    cent_df |> filter(layer == layer_label, year == yr)
  )
  idx <- match(node_iso3, cent_sub$iso3)
  
  unmatched <- node_iso3[is.na(idx)]
  if (length(unmatched) > 0)
    message("  [decorate_cp] Unmatched nodes: ", paste(unmatched, collapse = ", "))
  
  igraph::V(g)$cp_class  <- as.character(cent_sub$cp_class[idx])
  igraph::V(g)$str_out   <- as.numeric(cent_sub$strength_out[idx])
  igraph::V(g)$is_norway <- node_iso3 == FOCAL_COUNTRY
  
  igraph::V(g)$str_out[is.na(igraph::V(g)$str_out)]   <- 0
  igraph::V(g)$cp_class[is.na(igraph::V(g)$cp_class)] <- "Periphery"
  
  g
}

g_fe_22 <- decorate_cp(g_fe_22, centrality_all, "Front-end", 2022)
g_be_22 <- decorate_cp(g_be_22, centrality_all, "Back-end",  2022)

# ── Plot function — no legend ─────────────────────────────────────────────────
cp_plot <- function(g, title_str) {
  set.seed(42)
  ggraph(g, layout = "fr") +
    geom_edge_link(
      aes(alpha = weight_marketshare),
      colour      = "grey65",
      width       = 0.35,
      show.legend = FALSE
    ) +
    geom_node_point(
      aes(size  = .data[["str_out"]],
          fill  = .data[["cp_class"]],
          shape = .data[["is_norway"]]),
      colour = "grey30",
      stroke = 0.4
    ) +
    geom_node_text(
      aes(label    = .data[["iso3"]],
          fontface = ifelse(.data[["is_norway"]], "bold", "plain")),
      repel        = TRUE,
      size         = 3,
      max.overlaps = 50
    ) +
    scale_fill_manual(
      values = c("Core"           = "#D7191C",
                 "Semi-periphery" = "#1A9641",
                 "Periphery"      = "#2B83BA"),
      guide  = "none"
    ) +
    scale_shape_manual(
      values = c("FALSE" = 21, "TRUE" = 23),
      guide  = "none"
    ) +
    scale_size(range = c(2, 12), guide = "none") +
    scale_edge_alpha(range = c(0.05, 0.4), guide = "none") +
    labs(title = title_str) +
    theme_graph(base_family = "sans")
}

p_cp_fe <- cp_plot(g_fe_22, "Front-end (2022)")
p_cp_be <- cp_plot(g_be_22, "Back-end (2022)")

# ── Standalone legend ─────────────────────────────────────────────────────────
legend_data <- tibble(
  x        = 1:3,
  y        = 1:3,
  Position = factor(c("Core", "Semi-periphery", "Periphery"),
                    levels = c("Core", "Semi-periphery", "Periphery"))
)

p_legend_only <- ggplot(legend_data,
                        aes(x = x, y = y, fill = Position)) +
  geom_point(shape = 21, size = 4) +
  scale_fill_manual(
    values = c("Core"           = "#D7191C",
               "Semi-periphery" = "#1A9641",
               "Periphery"      = "#2B83BA"),
    name   = "Position"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title    = element_text(face = "bold", size = 11),
    legend.text     = element_text(size = 10),
    legend.key.size = unit(0.8, "cm")
  ) |>
  cowplot::get_legend()

# ── Combined plot ─────────────────────────────────────────────────────────────
p_cp_combined <- (p_cp_fe | p_cp_be) +
  plot_annotation(
    title    = "Core-Periphery Structure of the Global Semiconductor Trade Network (2022)",
    subtitle = "Node size = out-strength. Norway = diamond (◆).",
    caption  = "Sources: UN Comtrade; OECD BTIGE; Taiwan ITA. K-core decomposition following Ou et al. (2024). Author's calculations."
  )

# ── Add legend to the right ───────────────────────────────────────────────────
p_final <- cowplot::plot_grid(
  p_cp_combined,
  p_legend_only,
  ncol       = 2,
  rel_widths = c(0.92, 0.08)
)

ggsave(file.path(DIRS$figures, "fig_cp_combined_2022.pdf"),
       plot   = p_final,
       width  = 19,
       height = 9,
       device = "pdf")

message("Saved: fig_cp_combined_2022.pdf")