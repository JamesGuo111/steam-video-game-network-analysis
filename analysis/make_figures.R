# Regenerates every figure in figures/ from the Shiny app's own objects, so the
# README always shows exactly what app.R computes.
# Usage (from the repository root):  Rscript analysis/make_figures.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(igraph)
  library(tidygraph)
  library(ggraph)
  library(ggrepel)
})

# Work from the repository root (also works when called from analysis/)
if (!file.exists("app.R") && file.exists("../app.R")) setwd("..")
if (!file.exists("app.R")) {
  stop("Run from the repository root: Rscript analysis/make_figures.R", call. = FALSE)
}
dir.create("figures", showWarnings = FALSE)

# ---- 1. Build the networks exactly as the app does ---------------------------
# Sourcing app.R creates game_tidy, tag_tidy, bb_g_tidy, bb_t_tidy and the
# summary statistics; it builds the app object but does not launch it.
app <- new.env()
suppressMessages(sys.source("app.R", envir = app))

# ---- 2. Sanity checks against the numbers reported in the app ---------------
expect_value <- function(label, actual, expected, tol = 1e-3) {
  if (abs(as.numeric(actual) - expected) > tol) {
    stop(sprintf("%s: expected %s but got %s", label, expected, actual), call. = FALSE)
  }
}
with(app, {
  expect_value("game edges",                ecount(game_net),            2926)
  expect_value("game density",              game_density,                0.591)
  expect_value("game diameter",             game_diameter,               3)
  expect_value("game mean distance",        game_meandist,               1.414)
  expect_value("game degree assortativity", game_assort_deg,             0.227)
  expect_value("game communities",          n_distinct(game_df$cluster), 5)
  expect_value("tag edges",                 ecount(tag_net),             573)
  expect_value("tag density",               tag_density,                 0.073)
  expect_value("tag diameter",              tag_diameter,                4)
  expect_value("tag mean distance",         tag_meandist,                2.223)
  expect_value("tag communities",           tag_n_communities,           6)
})

# The Findings tab describes six tag communities by their member tags. Stop if
# the seeded Louvain run no longer reproduces them (e.g. after a package update).
findings_communities <- list(
  "Online shooters"         = c("Multiplayer", "FPS", "Shooter", "Action", "Free to Play"),
  "Open-world survival"     = c("Open World", "Survival", "Co-op", "Open World Survival Craft", "Zombies", "Horror"),
  "Single-player adventure" = c("Singleplayer", "Adventure", "Classic", "Sci-fi"),
  "RPG & Souls-like"        = c("RPG", "Action RPG", "Souls-like", "Hack and Slash", "Loot", "Difficult"),
  "Simulation & strategy"   = c("Simulation", "Automobile Sim"),
  "Sports & tactics"        = c("Sports", "Football (Soccer)", "PvP", "Tactical", "Competitive", "e-sports")
)
tag_cluster <- setNames(app$tag_df$cluster, app$tag_df$label)
for (k in seq_along(findings_communities)) {
  off <- findings_communities[[k]][tag_cluster[findings_communities[[k]]] != k]
  if (length(off) > 0) {
    stop(sprintf("Tag community %d (%s) no longer matches the Findings tab: %s",
                 k, names(findings_communities)[k], paste(off, collapse = ", ")), call. = FALSE)
  }
}
tag_community_names <- names(findings_communities)  # position = Louvain cluster id

# Game communities are not described in the app; each is labelled after its most
# frequent tags and identified by an anchor game, so labels survive renumbering.
game_anchors <- c(
  "Counter-Strike: Global Offensive" = "Shooters",
  "Rust"                             = "Open world & survival",
  "HELLDIVERS 2"                     = "Co-op & party",
  "Baldur's Gate 3"                  = "RPG & single-player",
  "Warframe"                         = "Free-to-play live service"
)
anchor_cluster <- app$game_df$cluster[match(names(game_anchors), app$game_df$label)]
if (anyNA(anchor_cluster) || n_distinct(anchor_cluster) != length(game_anchors)) {
  stop("Game communities changed: the anchor games no longer fall in five distinct communities.",
       call. = FALSE)
}
game_community_names <- setNames(unname(game_anchors), anchor_cluster)

# ---- 3. Shared style -----------------------------------------------------------
font <- if ("Helvetica Neue" %in% systemfonts::system_fonts()$family) "Helvetica Neue" else "sans"

# Okabe-Ito colours; matching meta-genres share a colour across the two networks
tag_palette  <- setNames(c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#56B4E9", "#D55E00"),
                         tag_community_names)
game_palette <- setNames(c("#0072B2", "#E69F00", "#D55E00", "#CC79A7", "#56B4E9"),
                         unname(game_anchors))
primary_levels  <- c(app$top_tags, "Other")
primary_palette <- setNames(c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F", "#EDC948",
                              "#B07AA1", "#FF9DA7", "#9C755F", "#86BCB6", "#CFCFCF"),
                            primary_levels)

theme_network <- function() {
  theme_void(base_family = font, base_size = 13) +
    theme(plot.background = element_rect(fill = "white", colour = NA),
          plot.title      = element_text(face = "bold", size = 18, margin = margin(b = 4)),
          plot.subtitle   = element_text(colour = "grey30", size = 12.5, margin = margin(b = 8)),
          plot.caption    = element_text(colour = "grey40", size = 10, hjust = 0, margin = margin(t = 8)),
          legend.position = "bottom",
          legend.title    = element_text(face = "bold", size = 11.5),
          legend.text     = element_text(size = 11),
          strip.text      = element_text(face = "bold", size = 12.5, margin = margin(b = 6)),
          plot.margin     = margin(18, 18, 14, 18))
}

theme_ranking <- function() {
  theme_minimal(base_family = font, base_size = 12.5) +
    theme(plot.background    = element_rect(fill = "white", colour = NA),
          panel.grid.major.y = element_blank(),
          panel.grid.minor   = element_blank(),
          axis.title         = element_blank(),
          strip.text         = element_text(face = "bold", size = 13, hjust = 0),
          plot.title         = element_text(face = "bold", size = 18, margin = margin(b = 4)),
          plot.subtitle      = element_text(colour = "grey30", size = 12.5, margin = margin(b = 10)),
          legend.position    = "bottom",
          legend.title       = element_text(face = "bold", size = 11.5),
          plot.margin        = margin(18, 18, 14, 18))
}

save_png <- function(plot, file, width = 12, height = 8.5) {
  ggsave(file.path("figures", file), plot, width = width, height = height, dpi = 200,
         device = ragg::agg_png, bg = "white")
  message("  wrote figures/", file)
}

# Node positions are computed once per network and shared by all its figures
layout_nodes <- function(graph, layout = "stress", seed = 780) {
  set.seed(seed)
  create_layout(graph, layout = layout) |>
    as_tibble() |>
    select(-starts_with(".ggraph"), -circular)
}

layout_edges <- function(graph, nodes) {
  graph |>
    activate(edges) |>
    as_tibble() |>
    mutate(x = nodes$x[from], y = nodes$y[from], xend = nodes$x[to], yend = nodes$y[to])
}

top_by <- function(data, var, n) slice_max(data, {{ var }}, n = n, with_ties = FALSE)

repel_labels <- function(data, size = 3.3) {
  geom_text_repel(data = data, aes(x, y, label = label), family = font, size = size,
                  colour = "grey10", bg.color = "white", bg.r = 0.15,
                  segment.colour = "grey55", segment.size = 0.3, min.segment.length = 0,
                  box.padding = 0.35, max.overlaps = Inf, seed = 780)
}

plot_network <- function(nodes, edges, fill, palette, fill_name, labels,
                         size_range = c(1.5, 9), edge_alpha = c(0.04, 0.25)) {
  ggplot() +
    geom_segment(data = edges, aes(x, y, xend = xend, yend = yend, alpha = weight),
                 colour = "grey55", linewidth = 0.25, show.legend = FALSE) +
    scale_alpha(range = edge_alpha) +
    geom_point(data = nodes, aes(x, y, fill = {{ fill }}, size = degree),
               shape = 21, colour = "white", stroke = 0.35) +
    scale_fill_manual(values = palette, name = fill_name, drop = FALSE) +
    scale_size(range = size_range, name = "Degree") +
    repel_labels(labels) +
    coord_equal(clip = "off") +
    guides(fill = guide_legend(override.aes = list(size = 4.5), nrow = 2, order = 1),
           size = guide_legend(order = 2)) +
    theme_network()
}

plot_backbone <- function(nodes, edges, fill, palette, fill_name, labels) {
  ggplot() +
    geom_segment(data = edges, aes(x, y, xend = xend, yend = yend, linewidth = oldweight),
                 colour = "grey70", alpha = 0.8, lineend = "round", show.legend = FALSE) +
    scale_linewidth(range = c(0.3, 2.2)) +
    geom_point(data = nodes, aes(x, y, fill = {{ fill }}, size = degree),
               shape = 21, colour = "white", stroke = 0.4) +
    scale_fill_manual(values = palette, name = fill_name, drop = FALSE) +
    scale_size(range = c(2.5, 12), guide = "none") +
    repel_labels(labels, size = 3.6) +
    coord_equal(clip = "off") +
    guides(fill = guide_legend(override.aes = list(size = 4.5), nrow = 2)) +
    theme_network()
}

# Layouts shared by the figures below
game_nodes <- layout_nodes(app$game_tidy) |>
  mutate(community = factor(game_community_names[as.character(cluster)], levels = unname(game_anchors)),
         primary   = factor(primary_tag_grouped, levels = primary_levels))
game_edges <- layout_edges(app$game_tidy, game_nodes)
tag_nodes  <- layout_nodes(app$tag_tidy) |>
  mutate(community = factor(tag_community_names[cluster], levels = tag_community_names))
tag_edges  <- layout_edges(app$tag_tidy, tag_nodes)

message("Checks passed: networks match the app.")
