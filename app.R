# Steam Video Game Network - Shiny App
# Zijing Guo
# Final Project for Sociology Social Networks

# ===== Libraries =====
library(shiny)
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(backbone)

# ===== Setup: build everything once at startup =====
games <- read.csv("~/Desktop/SOCI 0226 Social Networks/nodes.csv")

long <- games |>
  separate_rows(tags, sep = "; ") |>
  rename(tag = tags) |>
  mutate(game_id = paste0("g_", node_id),
         tag_id  = paste0("t_", tag))

nodes_g <- long |>
  select(name = game_id, label = name,
         rank, ccu, review_score, primary_tag, price_cents) |>
  distinct() |>
  mutate(type = FALSE)

nodes_t <- long |>
  select(name = tag_id) |>
  distinct() |>
  mutate(label = sub("^t_", "", name),
         rank = NA_integer_, ccu = NA_integer_,
         review_score = NA_real_, primary_tag = NA_character_,
         price_cents = NA_integer_,
         type = TRUE)

nodes_bp <- bind_rows(nodes_g, nodes_t)
edges_bp <- long |> select(from = game_id, to = tag_id)
bp_net <- graph_from_data_frame(d = edges_bp, vertices = nodes_bp, directed = FALSE)
bp <- bipartite_projection(bp_net)
game_net <- bp$proj1
tag_net  <- bp$proj2

# Game tidy + centralities + community
game_tidy <- as_tbl_graph(game_net) |>
  activate(nodes) |>
  mutate(degree      = centrality_degree(),
         betweenness = centrality_betweenness(normalized = TRUE),
         eigenvector = centrality_eigen(weights = weight),
         cluster     = group_louvain(weights = weight))

top_tags <- game_tidy |> activate(nodes) |> as_tibble() |>
  count(primary_tag, sort = TRUE) |> slice_head(n = 10) |> pull(primary_tag)

game_tidy <- game_tidy |>
  activate(nodes) |>
  mutate(primary_tag_grouped = ifelse(primary_tag %in% top_tags, primary_tag, "Other"))

game_df <- game_tidy |> activate(nodes) |> as_tibble()

# Tag tidy + centralities + community
tag_freq <- long |> count(tag_id, name = "frequency") |> rename(name = tag_id)
tag_tidy <- as_tbl_graph(tag_net) |>
  activate(nodes) |>
  left_join(tag_freq, by = "name") |>
  mutate(degree      = centrality_degree(),
         betweenness = centrality_betweenness(normalized = TRUE),
         cluster     = group_louvain(weights = weight))

tag_df <- tag_tidy |> activate(nodes) |> as_tibble()

# Backbones
bb_g_tidy <- tryCatch({
  as_tbl_graph(backbone_from_weighted(game_tidy, alpha = 0.2, model = "disparity")) |>
    activate(nodes) |>
    mutate(degree = centrality_degree()) |>
    filter(degree > 0) |>
    mutate(comp = group_components()) |>
    filter(comp == 1)
}, error = function(e) NULL)

bb_t_tidy <- tryCatch({
  as_tbl_graph(backbone_from_weighted(tag_tidy, alpha = 0.2, model = "disparity")) |>
    activate(nodes) |>
    mutate(degree = centrality_degree()) |>
    filter(degree > 0) |>
    mutate(comp = group_components()) |>
    filter(comp == 1)
}, error = function(e) NULL)

# Pre-compute headline stats
game_density     <- round(edge_density(game_net), 3)
game_diameter    <- diameter(game_net, weights = NA)
game_meandist    <- round(mean_distance(game_net, weights = NA, unconnected = TRUE), 3)
game_assort_pt   <- round(assortativity_nominal(game_net,
                                                as.integer(as.factor(V(game_net)$primary_tag)),
                                                directed = FALSE), 3)
game_assort_deg  <- round(assortativity_degree(game_net, directed = FALSE), 3)
game_mod_pt      <- round(modularity(game_net,
                                     membership = as.integer(as.factor(V(game_net)$primary_tag))), 3)
game_louvain_mod <- round(modularity(cluster_louvain(game_net)), 3)
game_cor_deg     <- round(cor(game_df$ccu, game_df$degree), 3)
game_cor_btw     <- round(cor(game_df$ccu, game_df$betweenness), 3)
game_cor_eig     <- round(cor(game_df$ccu, game_df$eigenvector), 3)

tag_density       <- round(edge_density(tag_net), 3)
tag_diameter      <- diameter(tag_net, weights = NA)
tag_meandist      <- round(mean_distance(tag_net, weights = NA, unconnected = TRUE), 3)
tag_assort_deg    <- round(assortativity_degree(tag_net, directed = FALSE), 3)
tag_louvain_mod   <- round(modularity(cluster_louvain(tag_net)), 3)
tag_n_communities <- length(unique(tag_df$cluster))

# ===== UI =====
ui <- fluidPage(
  titlePanel("Steam Video Game Network"),
  
  tabsetPanel(
    
    # ---- Tab 1: Introduction ----
  tabPanel("Introduction",
               br(),
               wellPanel(
                 h3("About this project"),
                 HTML(paste0("
            <p>This app explores how the Top 100 most-played Steam games are structurally
            related through their gameplay tags. The underlying data is a
            <b>bipartite network</b> with two node types — <b>games</b> (100) and
            <b>tags</b> (126) — connected by edges that record which tags belong to
            which game. The bipartite is projected two ways: a <b>Game network</b>
            (games linked by shared tags) and a <b>Tag network</b> (tags linked by
            co-occurrence in games).</p>
            <p><b>Three patterns the app brings out:</b></p>
            <ul>
              <li><b>Popularity is decoupled from structural centrality.</b>
                Several of the most-played games sit at the periphery, while many
                low-CCU games occupy central positions (correlation between CCU and
                degree is only ", game_cor_deg, ").</li>
              <li><b>The two projections have opposite shapes.</b> The Game network
                is very dense (density ", game_density, ", diameter ", game_diameter,
                ") — almost any two top-100 games share at least one tag. The Tag
                network is much sparser (density ", tag_density, ", diameter ",
                tag_diameter, "), with longer paths between tags.</li>
              <li><b>Degree assortativity flips sign across projections.</b> The Game
                network is degree-assortative (", game_assort_deg, ": hubs connect
                to hubs), while the Tag network is degree-disassortative (",
                tag_assort_deg, ": hubs connect to peripheral tags). Same underlying
                data, two opposite mixing patterns.</li>
            </ul>
            <p><b>How to engage:</b> use the dropdowns to switch which centrality
            measure ranks the bar chart, toggle between Top 15 and all nodes, and
            select different ego networks to drill into individual games or tags.</p>
          "))
               ),
               wellPanel(
                 h3("Data and method"),
                 HTML("
            <p><b>Source.</b> Steam Top 100 most-played games as of May 2026, with
            rank, concurrent users (CCU), review scores, prices, and 5 gameplay tags
            each.</p>
            <p><b>Collection.</b> Data scraped from SteamDB and SteamSpy
            via Python API calls.</p>
          ")
               )
      ),
    
    # ---- Tab 2: Game Section ----
    tabPanel("Game Section",
             br(),
             wellPanel(
               h4("Game-game tag-similarity network (color = primary_tag)"),
               radioButtons("game_size_by", "Size nodes by:",
                            choices = c("Degree" = "degree",
                                        "CCU (popularity, log10)" = "ccu"),
                            selected = "degree", inline = TRUE),
               plotOutput("game_network", height = "700px")
             ),
             wellPanel(
               h4("Game network coloured by Louvain community"),
               plotOutput("game_network_louvain", height = "700px")
             ),
             wellPanel(
               h4("Games by centrality"),
               selectInput("game_metric", "Choose centrality measure:",
                           choices = c("Degree" = "degree",
                                       "Betweenness" = "betweenness",
                                       "Eigenvector" = "eigenvector"),
                           selected = "degree"),
               radioButtons("game_bar_view", "View:",
                            choices = c("Top 15" = "top15",
                                        "All games" = "all"),
                            selected = "top15", inline = TRUE),
               plotOutput("game_bar", height = "700px")
             ),
             wellPanel(
               h4("Network statistics"),
               HTML(paste0(
                 "<ul>",
                 "<li>Nodes: 100, Edges: ", ecount(game_net), "</li>",
                 "<li>Density: <b>", game_density, "</b> (very dense)</li>",
                 "<li>Diameter: ", game_diameter, ", Mean distance: ", game_meandist, "</li>",
                 "<li>Assortativity by primary_tag: <b>", game_assort_pt, "</b></li>",
                 "<li>Degree assortativity: ", game_assort_deg, "</li>",
                 "<li>Modularity by primary_tag: ", game_mod_pt, "</li>",
                 "<li>Louvain modularity: ", game_louvain_mod, "</li>",
                 "<li>Correlation (CCU, degree): ", game_cor_deg, "</li>",
                 "<li>Correlation (CCU, betweenness): ", game_cor_btw, "</li>",
                 "<li>Correlation (CCU, eigenvector): ", game_cor_eig, "</li>",
                 "</ul>"
               ))
             ),
             wellPanel(
               h4("Backbone of the game network (disparity filter, alpha = 0.2)"),
               plotOutput("game_backbone", height = "700px")
             ),
             wellPanel(
               h4("Ego network of selected game"),
               selectInput("game_ego", "Choose a game:",
                           choices = c(
                             "Monster Hunter: World (highest degree)" = "g_582010",
                             "NBA 2K20 (high betweenness, niche tag)" = "g_1089350",
                             "Baldur's Gate 3 (popular but distinctive)" = "g_1086940"
                           ),
                           selected = "g_582010"),
               plotOutput("game_ego_plot", height = "600px")
             )
    ),
    
    # ---- Tab 3: Tag Section ----
    tabPanel("Tag Section",
             br(),
             wellPanel(
               h4("Steam Tag Co-occurrence Network (color = Louvain community)"),
               plotOutput("tag_network", height = "800px")
             ),
             wellPanel(
               h4("Tags by centrality"),
               selectInput("tag_metric", "Choose centrality measure:",
                           choices = c("Degree" = "degree",
                                       "Betweenness" = "betweenness"),
                           selected = "degree"),
               radioButtons("tag_bar_view", "View:",
                            choices = c("Top 15" = "top15",
                                        "All tags" = "all"),
                            selected = "top15", inline = TRUE),
               plotOutput("tag_bar", height = "800px")
             ),
             wellPanel(
               h4("Network statistics"),
               HTML(paste0(
                 "<ul>",
                 "<li>Nodes: ", vcount(tag_net), ", Edges: ", ecount(tag_net), "</li>",
                 "<li>Density: <b>", tag_density, "</b></li>",
                 "<li>Diameter: ", tag_diameter, ", Mean distance: ", tag_meandist, "</li>",
                 "<li>Degree assortativity: <b>", tag_assort_deg, "</b></li>",
                 "<li>Louvain modularity: <b>", tag_louvain_mod, "</b> (",
                 tag_n_communities, " communities)</li>",
                 "</ul>"
               ))
             ),
             wellPanel(
               h4("Backbone of the tag network (disparity filter, alpha = 0.2)"),
               plotOutput("tag_backbone", height = "800px")
             ),
             wellPanel(
               h4("Ego network of selected tag"),
               selectInput("tag_ego", "Choose a tag:",
                           choices = c(
                             "Multiplayer (universal modifier)" = "t_Multiplayer",
                             "Souls-like (well-defined sub-genre)" = "t_Souls-like"
                           ),
                           selected = "t_Multiplayer"),
               plotOutput("tag_ego_plot", height = "600px")
             )
    )
  )
)

# ===== Server =====
server <- function(input, output, session) {
  
  # ---- Game Section ----
  output$game_network <- renderPlot({
    if (input$game_size_by == "degree") {
      ggraph(game_tidy, layout = "stress") +
        geom_edge_link(aes(alpha = weight), color = "grey60") +
        scale_edge_alpha(range = c(0.2, 0.5)) +
        geom_node_point(aes(color = primary_tag_grouped, size = degree), alpha = 0.85) +
        scale_size(range = c(1, 8)) +
        geom_node_text(aes(label = label),
                       size = 2.5, repel = TRUE, max.overlaps = Inf) +
        scale_color_brewer(palette = "Paired") +
        theme_void() +
        labs(color = "Primary tag", size = "Degree")
    } else {
      ggraph(game_tidy, layout = "stress") +
        geom_edge_link(aes(alpha = weight), color = "grey60") +
        scale_edge_alpha(range = c(0.2, 0.5)) +
        geom_node_point(aes(color = primary_tag_grouped,
                            size = log10(ccu + 1)), alpha = 0.85) +
        scale_size_continuous(range = c(2, 10),
                              breaks = log10(c(1, 100, 10000, 1000000) + 1),
                              labels = c("0", "100", "10K", "1M"),
                              name = "CCU") +
        geom_node_text(aes(label = label),
                       size = 2.5, repel = TRUE, max.overlaps = Inf) +
        scale_color_brewer(palette = "Paired") +
        theme_void() +
        labs(color = "Primary tag")
    }
  })
  
  output$game_network_louvain <- renderPlot({
    ggraph(game_tidy, layout = "stress") +
      geom_edge_link(aes(alpha = weight), color = "grey60") +
      scale_edge_alpha(range = c(0.1, 0.4)) +
      geom_node_point(aes(color = as.factor(cluster), size = degree), alpha = 0.85) +
      scale_size(range = c(2, 10)) +
      geom_node_text(aes(label = label),
                     size = 2.5, repel = TRUE) +
      theme_void() +
      labs(color = "Cluster", size = "Degree")
  })
  
  output$game_bar <- renderPlot({
    metric <- input$game_metric
    view_mode <- input$game_bar_view
    
    df <- game_df |> arrange(desc(.data[[metric]]))
    if (view_mode == "top15") {
      df <- df |> slice_head(n = 15)
    }
    
    ggplot(df, aes(x = reorder(label, .data[[metric]]),
                   y = .data[[metric]],
                   fill = primary_tag_grouped)) +
      geom_col() + coord_flip() +
      scale_fill_brewer(palette = "Paired") +
      labs(x = "Game", y = metric) +
      theme(legend.position = "none",
            axis.text.y = element_text(size = if (view_mode == "all") 5 else 9))
  })
  
  output$game_backbone <- renderPlot({
    if (is.null(bb_g_tidy)) {
      plot.new()
      text(0.5, 0.5, "Backbone returned empty - try a different alpha")
      return()
    }
    ggraph(bb_g_tidy, layout = "stress") +
      geom_edge_link(aes(width = oldweight), color = "grey80", alpha = 0.7) +
      scale_edge_width(range = c(0.2, 1.5)) +
      geom_node_point(aes(color = primary_tag_grouped, size = degree), alpha = 0.85) +
      scale_size(range = c(2, 10)) +
      geom_node_text(aes(label = label), size = 4, repel = TRUE, max.overlaps = Inf) +
      scale_color_brewer(palette = "Paired") +
      theme_void() +
      labs(color = "Primary tag", size = "Degree")
  })
  
  output$game_ego_plot <- renderPlot({
    eg <- make_ego_graph(game_net, order = 1, nodes = input$game_ego)
    plot(eg[[1]],
         layout = layout_with_kk(eg[[1]]),
         vertex.size = 5,
         vertex.label = V(eg[[1]])$label,
         vertex.label.cex = 0.8,
         edge.color = "grey",
         edge.width = E(eg[[1]])$weight * 0.5,
         asp = 0)
  })
  
  # ---- Tag Section ----
  output$tag_network <- renderPlot({
    ggraph(tag_tidy, layout = "stress") +
      geom_edge_link(aes(width = weight, alpha = weight), color = "black") +
      scale_edge_width(range = c(0.1, 1.4)) +
      scale_edge_alpha(range = c(0.2, 0.55)) +
      geom_node_point(aes(color = as.factor(cluster), size = degree), alpha = 0.85) +
      scale_size(range = c(2, 10)) +
      geom_node_text(aes(label = label),
                     size = 2.3, repel = TRUE, max.overlaps = Inf,
                     color = "black", bg.color = "white", bg.r = 0.05) +
      scale_color_brewer(palette = "Set2") +
      theme_void() +
      labs(color = "Louvain community", size = "Degree")
  })
  
  output$tag_bar <- renderPlot({
    metric <- input$tag_metric
    view_mode <- input$tag_bar_view
    
    df <- tag_df |> arrange(desc(.data[[metric]]))
    if (view_mode == "top15") {
      df <- df |> slice_head(n = 15)
    }
    
    ggplot(df, aes(x = reorder(label, .data[[metric]]),
                   y = .data[[metric]],
                   fill = as.factor(cluster))) +
      geom_col() + coord_flip() +
      scale_fill_brewer(palette = "Set2") +
      labs(x = "Tag", y = metric, fill = "Community") +
      theme(legend.position = "none",
            axis.text.y = element_text(size = if (view_mode == "all") 4 else 9))
  })
  
  output$tag_backbone <- renderPlot({
    if (is.null(bb_t_tidy)) {
      plot.new()
      text(0.5, 0.5, "Backbone returned empty - try a different alpha")
      return()
    }
    ggraph(bb_t_tidy, layout = "stress") +
      geom_edge_link(aes(width = oldweight), color = "grey80", alpha = 0.7) +
      scale_edge_width(range = c(0.2, 1.5)) +
      geom_node_point(aes(color = as.factor(cluster), size = degree), alpha = 0.85) +
      scale_size(range = c(2, 10)) +
      geom_node_text(aes(label = label), size = 4, repel = TRUE, max.overlaps = Inf) +
      scale_color_brewer(palette = "Set2") +
      theme_void() +
      labs(color = "Louvain community", size = "Degree")
  })
  
  output$tag_ego_plot <- renderPlot({
    eg <- make_ego_graph(tag_net, order = 1, nodes = input$tag_ego)
    plot(eg[[1]],
         layout = layout_with_kk(eg[[1]]),
         vertex.size = 5,
         vertex.label = V(eg[[1]])$label,
         vertex.label.cex = 0.8,
         edge.color = "grey",
         edge.width = E(eg[[1]])$weight * 0.4,
         asp = 0)
  })
}

# ===== Run =====
shinyApp(ui, server)