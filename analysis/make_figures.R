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

# nodes.csv is UTF-8; declare it so titles such as Civilization VI (curly
# apostrophe) render correctly even when R runs under a non-UTF-8 locale
as_utf8 <- function(x) {
  Encoding(x) <- "UTF-8"
  x
}
for (g in c("game_tidy", "tag_tidy", "bb_g_tidy", "bb_t_tidy")) {
  if (!is.null(app[[g]])) app[[g]] <- app[[g]] |> activate(nodes) |> mutate(label = as_utf8(label))
}
app$game_df$label <- as_utf8(app$game_df$label)
app$tag_df$label  <- as_utf8(app$tag_df$label)

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
                              "#B07AA1", "#FF9DA7", "#9C755F", "#4B3B8F", "#CFCFCF"),
                            primary_levels)

theme_network <- function() {
  theme_void(base_family = font, base_size = 13) +
    theme(plot.background = element_rect(fill = "white", colour = NA),
          plot.title      = element_text(face = "bold", size = 18, margin = margin(b = 4)),
          plot.subtitle   = element_text(colour = "grey30", size = 12.5, margin = margin(b = 8)),
          plot.caption    = element_text(colour = "grey40", size = 10, hjust = 0, margin = margin(t = 8)),
          legend.position = "bottom",
          legend.box      = "vertical",
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
    scale_fill_manual(values = palette, name = fill_name) +
    scale_size(range = size_range, name = "Degree") +
    repel_labels(labels) +
    coord_equal(clip = "off") +
    guides(fill = guide_legend(override.aes = list(size = 4.5), nrow = 2, order = 1),
           size = guide_legend(order = 2, override.aes = list(fill = "grey65"))) +
    theme_network()
}

plot_backbone <- function(nodes, edges, fill, palette, fill_name, labels) {
  ggplot() +
    geom_segment(data = edges, aes(x, y, xend = xend, yend = yend, linewidth = oldweight),
                 colour = "grey70", alpha = 0.8, lineend = "round", show.legend = FALSE) +
    scale_linewidth(range = c(0.3, 2.2)) +
    geom_point(data = nodes, aes(x, y, fill = {{ fill }}, size = degree),
               shape = 21, colour = "white", stroke = 0.4) +
    scale_fill_manual(values = palette, name = fill_name) +
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

# ---- 4. Game network figures ---------------------------------------------------
message("Game network figures")

# 4a. Communities
game_labels <- bind_rows(top_by(game_nodes, ccu, 8), top_by(game_nodes, degree, 8)) |>
  distinct(name, .keep_all = TRUE)
p <- plot_network(game_nodes, game_edges, community, game_palette, "Louvain community", game_labels) +
  labs(title = "The game network: 100 games linked by shared tags",
       subtitle = sprintf("%s edges \u00b7 density %.3f \u00b7 diameter %d \u00b7 %d Louvain communities",
                          scales::comma(ecount(app$game_net)), app$game_density, app$game_diameter,
                          n_distinct(app$game_df$cluster)),
       caption = paste("Node size = degree; edge opacity = number of shared tags.",
                       "Labelled: the eight most-played and the eight best-connected games."))
save_png(p, "game-communities.png", width = 10.5, height = 9.5)

# 4b. Popularity vs. centrality: same layout, two size encodings
panels <- c("Node size = degree", "Node size = concurrent players (log scale)")
pop_labelled <- c("Counter-Strike: Global Offensive", "PUBG: BATTLEGROUNDS", "Baldur's Gate 3",
                  "Wallpaper Engine", "Monster Hunter: World", "Grand Theft Auto IV: Complete Edition")
pop_label_ids <- game_nodes$name[game_nodes$label %in% pop_labelled]
stopifnot(length(pop_label_ids) == length(pop_labelled))
pop_nodes <- bind_rows(
  game_nodes |> mutate(panel = panels[1], s = scales::rescale(degree)),
  game_nodes |> mutate(panel = panels[2], s = scales::rescale(log10(ccu + 1)))) |>
  mutate(panel = factor(panel, levels = panels))
pop_edges <- bind_rows(game_edges |> mutate(panel = panels[1]),
                       game_edges |> mutate(panel = panels[2])) |>
  mutate(panel = factor(panel, levels = panels))
p <- ggplot() +
  geom_segment(data = pop_edges, aes(x, y, xend = xend, yend = yend, alpha = weight),
               colour = "grey60", linewidth = 0.2, show.legend = FALSE) +
  scale_alpha(range = c(0.03, 0.18)) +
  geom_point(data = pop_nodes, aes(x, y, fill = primary, size = s),
             shape = 21, colour = "white", stroke = 0.3) +
  scale_size(range = c(0.8, 10), guide = "none") +
  scale_fill_manual(values = primary_palette, name = "Primary tag") +
  geom_point(data = pop_nodes |> filter(name %in% pop_label_ids), aes(x, y, size = s),
             shape = 21, fill = NA, colour = "grey10", stroke = 0.9) +
  repel_labels(pop_nodes |> filter(name %in% pop_label_ids), size = 3.4) +
  facet_wrap(~panel) +
  coord_equal(clip = "off") +
  guides(fill = guide_legend(override.aes = list(size = 4.5), nrow = 2)) +
  labs(title = "Popularity is not centrality",
       subtitle = "The most-played games are scattered across the network rather than concentrated in its core",
       caption = "Outlined and labelled: the games discussed in the text.") +
  theme_network()
save_png(p, "game-popularity-vs-centrality.png", width = 13, height = 7.5)

# 4c. Centrality rankings
metric_labels <- c(degree = "Degree", betweenness = "Betweenness (normalized)",
                   eigenvector = "Eigenvector")
ranking <- function(df, metrics) {
  df |>
    pivot_longer(all_of(metrics), names_to = "metric") |>
    group_by(metric) |>
    slice_max(value, n = 15, with_ties = FALSE) |>
    ungroup() |>
    mutate(metric = factor(metric_labels[metric], levels = metric_labels[metrics]),
           item   = fct_reorder(paste(label, metric, sep = "___"), value))
}
plot_ranking <- function(data, fill, palette, fill_name) {
  ggplot(data, aes(value, item, fill = {{ fill }})) +
    geom_col(width = 0.72) +
    facet_wrap(~metric, scales = "free", nrow = 1) +
    scale_y_discrete(labels = function(x) str_wrap(sub("___.*$", "", x), 24)) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.04))) +
    scale_fill_manual(values = palette, name = fill_name) +
    guides(fill = guide_legend(nrow = 2)) +
    theme_ranking()
}
game_rank <- app$game_df |>
  mutate(primary = factor(primary_tag_grouped, levels = primary_levels)) |>
  ranking(c("degree", "betweenness", "eigenvector"))
p <- plot_ranking(game_rank, primary, primary_palette, "Primary tag") +
  labs(title = "Who is central depends on how you measure it",
       subtitle = "Top 15 games under each centrality measure, coloured by primary tag")
save_png(p, "game-centrality.png", width = 14, height = 7.5)

# ---- 5. Tag network, backbones and ego networks --------------------------------
message("Tag network, backbone and ego figures")

# 5a. Tag network communities
p <- plot_network(tag_nodes, tag_edges, community, tag_palette, "Louvain community",
                  labels = top_by(tag_nodes, degree, 28),
                  size_range = c(1.2, 11), edge_alpha = c(0.06, 0.45)) +
  labs(title = "The tag network: 126 tags linked by co-occurrence",
       subtitle = sprintf("%d edges \u00b7 density %.3f \u00b7 degree assortativity %.3f \u00b7 %d Louvain communities",
                          ecount(app$tag_net), app$tag_density, app$tag_assort_deg, app$tag_n_communities),
       caption = paste("Node size = degree; edge opacity = number of games in which both tags appear.",
                       "Labelled: the 28 highest-degree tags."))
save_png(p, "tag-communities.png", width = 10.5, height = 9.5)

# 5b. Tag centrality rankings
tag_rank <- app$tag_df |>
  mutate(community = factor(tag_community_names[cluster], levels = tag_community_names)) |>
  ranking(c("degree", "betweenness"))
p <- plot_ranking(tag_rank, community, tag_palette, "Louvain community") +
  labs(title = "Multiplayer leads every tag ranking",
       subtitle = "Top 15 tags by degree and betweenness, coloured by Louvain community")
save_png(p, "tag-centrality.png", width = 12, height = 6.5)

# 5c. Backbones (disparity filter, alpha = 0.2, largest component), as in the app
bb_tag_nodes <- layout_nodes(app$bb_t_tidy) |>
  mutate(community = factor(tag_community_names[cluster], levels = tag_community_names))
bb_tag_edges <- layout_edges(app$bb_t_tidy, bb_tag_nodes)
p <- plot_backbone(bb_tag_nodes, bb_tag_edges, community, tag_palette, "Louvain community",
                   labels = bb_tag_nodes) +
  labs(title = "Backbone of the tag network",
       subtitle = sprintf("Disparity filter, \u03b1 = 0.2 \u00b7 largest component: %d tags and %d of %d edges",
                          nrow(bb_tag_nodes), nrow(bb_tag_edges), ecount(app$tag_net)),
       caption = "Node size = degree in the backbone; edge width = number of co-occurring games.")
save_png(p, "tag-backbone.png", height = 9)

bb_game_nodes <- layout_nodes(app$bb_g_tidy) |>
  mutate(primary = factor(primary_tag_grouped, levels = primary_levels))
bb_game_edges <- layout_edges(app$bb_g_tidy, bb_game_nodes)
p <- plot_backbone(bb_game_nodes, bb_game_edges, primary, primary_palette, "Primary tag",
                   labels = top_by(bb_game_nodes, degree, 12)) +
  labs(title = "Backbone of the game network",
       subtitle = sprintf("Disparity filter, \u03b1 = 0.2 \u00b7 largest component: %d games and %d of %s edges",
                          nrow(bb_game_nodes), nrow(bb_game_edges), scales::comma(ecount(app$game_net))),
       caption = paste("Node size = degree in the backbone; edge width = number of shared tags.",
                       "Labelled: the 12 best-connected games."))
save_png(p, "game-backbone.png", height = 9)

# 5d. Ego networks: neighbours at or above a minimum edge weight, as with the app's slider
ego_star <- function(graph, focal, min_weight) {
  inc <- incident(graph, focal)
  ab  <- ends(graph, inc)
  tibble(name = ifelse(ab[, 1] == focal, ab[, 2], ab[, 1]), weight = E(graph)$weight[inc]) |>
    filter(weight >= min_weight) |>
    arrange(desc(weight), name) |>
    mutate(angle = pi / 2 - 2 * pi * (row_number() - 1) / n(),
           x = cos(angle), y = sin(angle), rel = weight / max(weight))
}
mhw <- ego_star(app$game_net, "g_582010", min_weight = 3) |>
  left_join(app$game_df |> select(name, label), by = "name")
mp  <- ego_star(app$tag_net, "t_Multiplayer", min_weight = 5) |>
  mutate(label = sub("^t_", "", name))
ego_panels <- c(
  sprintf("Monster Hunter: World\n%d of its %d neighbouring games share \u2265 3 tags",
          nrow(mhw), degree(app$game_net, "g_582010")),
  sprintf("Multiplayer\n%d of its %d neighbouring tags co-occur in \u2265 5 games",
          nrow(mp), degree(app$tag_net, "t_Multiplayer")))
ego <- bind_rows(mhw |> mutate(panel = ego_panels[1], colour = "steelblue"),
                 mp  |> mutate(panel = ego_panels[2], colour = "darkorange")) |>
  mutate(panel = factor(panel, levels = ego_panels),
         text  = str_wrap(sprintf("%s (%d)", label, weight), 18),
         hjust = case_when(x > 0.1 ~ 0, x < -0.1 ~ 1, TRUE ~ 0.5),
         vjust = case_when(y > 0.9 ~ 0, y < -0.9 ~ 1, TRUE ~ 0.5))
focal <- tibble(panel  = factor(ego_panels, levels = ego_panels),
                label  = c("Monster Hunter:\nWorld", "Multiplayer"),
                colour = c("steelblue", "darkorange"))
p <- ggplot(ego) +
  geom_segment(aes(x = 0, y = 0, xend = x, yend = y, linewidth = rel, colour = colour),
               alpha = 0.35, lineend = "round") +
  geom_point(aes(x, y, fill = colour), size = 5, shape = 21, colour = "white", stroke = 0.6) +
  geom_point(data = focal, aes(0, 0, fill = colour), size = 18, shape = 21,
             colour = "white", stroke = 1) +
  geom_label(data = focal, aes(0, 0, label = label), family = font, fontface = "bold",
             size = 3.9, linewidth = 0, fill = alpha("white", 0.9), lineheight = 0.9) +
  geom_text(aes(1.13 * x, 1.13 * y, label = text, hjust = hjust, vjust = vjust),
            family = font, size = 3.4, colour = "grey15", lineheight = 0.9) +
  scale_linewidth(range = c(0.6, 3.2), guide = "none") +
  scale_colour_identity() +
  scale_fill_identity() +
  facet_wrap(~panel) +
  coord_equal(xlim = c(-1.9, 1.9), ylim = c(-1.35, 1.35), clip = "off") +
  labs(title = "Neighbourhoods as recommendations",
       subtitle = "Edges filtered by a minimum weight, as with the app's slider; edge weights in brackets") +
  theme_network()
save_png(p, "ego-networks.png", width = 13, height = 5.8)
