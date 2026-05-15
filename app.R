# Steam Video Game Network - Shiny App
# Zijing Guo
# Final Project

# Libraries 
library(shiny)
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(backbone)
library(visNetwork)
library(plotly)

# Setup: build everything once at startup 
games <- read.csv("nodes.csv")

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

# Pre-compute network stats
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

tag_density       <- round(edge_density(tag_net), 3)
tag_diameter      <- diameter(tag_net, weights = NA)
tag_meandist      <- round(mean_distance(tag_net, weights = NA, unconnected = TRUE), 3)
tag_assort_deg    <- round(assortativity_degree(tag_net, directed = FALSE), 3)
tag_louvain_mod   <- round(modularity(cluster_louvain(tag_net)), 3)
tag_n_communities <- length(unique(tag_df$cluster))

# Max edge weights for similarity neighborhood threshold sliders
game_max_weight <- max(E(game_net)$weight)
tag_max_weight  <- max(E(tag_net)$weight)

# UI Interface
ui <- fluidPage(
  titlePanel("Steam Video Game Network"),
  
  tabsetPanel(
    
    #  Tab 1: Introduction 
    tabPanel("Introduction",
             br(),
             wellPanel(
               h3("Introduction"),
               HTML("
            <p>This app explores how the Top 100 most-played Steam games are structurally related through their gameplay tags. Steam is the largest PC gaming platform in the world. Every game on it carries a handful of gameplay tags (RPG, Multiplayer, Open World, Souls-like, and so on) that describe what kind of experience it offers. These tags are how the platform organizes its catalog, and the way they overlap across games reveals how the contemporary video game landscape is structured.</p>
            <p>The underlying data is a bipartite network with two node types — games (100) and tags (126) — connected by edges that record which tags belong to which game. The bipartite is projected two ways: a Game network (games linked by shared tags) and a Tag network (tags linked by co-occurrence in games).</p>
            <p>In this project, both the Game network and the Tag network are first visualized as a whole, and their basic structural properties such as density, diameter, mean distance, and assortativity are computed. Louvain community detection is applied to surface the latent clusters that emerge from the tag-overlap structure. Nodes centrality are measured and visualized as bar charts, so that nodes can be ranked along each dimension. The backbone of each network is then extracted using a disparity filter, which strips out the weakest edges and exposes the underlying skeleton of statistically significant connections. Finally, individual nodes are examined in detail through the tag-similarity neighborhood of any chosen game and the co-occurrence neighborhood of any chosen tag.</p>
          ")
             ),
             wellPanel(
               h3("How to Use"),
               HTML("
            <p>The app is organized into two parallel sections: the Game Section and the Tag Section. You can compare these two projections to observe their similarities and differences.</p>
            <ol>
              <li>At the top, you can see the full network and observe the density and shape of the game and tag projections. In the Game Section, you can also toggle the node size from degree to CCU (a measure of a game's relative popularity) to see whether more popular games tend to occupy more central positions.</li>
              <li>Below that, you can see the network coloured by Louvain community detection. This view lets you observe which games tend to cluster together, and which tags tend to bundle into broader genre families — a kind of meta-genre inherited from several constituent tags.</li>
              <li>Further down are the centrality measurements for games and tags. You can choose between different centrality measures and observe which games or tags rank highest under each one. Because the bars are coloured by primary tag (for games) and Louvain community (for tags), you can also see which kinds of tags or which communities tend to have higher centrality.</li>
              <li>Next, you can find the backbone of each network, which highlights the most structurally important games and tags. For the Tag network in particular, you can observe which kinds of tags or which communities anchor the network.</li>
              <li>At the bottom, you can find game- and tag-specific networks. You can select any game or tag you are interested in (or have played), then observe how it connects to others, and use the slider to filter by edge weight to find the games or tags most deeply related to your choice. This last view doubles as a way to find recommended games or genres.</li>
            </ol>
          ")
             ),
             wellPanel(
               h3("Findings"),
               HTML("
            <h4>Game section</h4>
            <ol>
              <li>Looking at network statistics, the Game network is strikingly dense. With 2,926 edges among 100 nodes, its density of 0.591 means roughly six out of every ten possible game pairs share at least one tag, and the diameter of just 3 (mean distance 1.414) makes it an extremely tight small world. Almost any two top-100 games can be reached in one or two hops. This density shows that the most-played games on Steam, despite spanning very different genres on the surface, occupy a heavily interconnected tag space. Degree assortativity is moderately positive (0.227), indicating that hub games (those linked to many others) tend to connect to other hub games, while peripheral games connect to peripheral ones. The popular core is, to some extent, structurally insular.</li>
              <li>Sizing the nodes by CCU rather than by degree reveals that commercial popularity does not map onto structural centrality. Several of the largest nodes sit near the periphery of the network, while many smaller nodes occupy positions deep in its core. Being a hit on Steam clearly does not require being structurally central. Despite the network's high density and modest overall modularity, Louvain community detection still recovers five visible communities that carve the Game network into coherent groupings.</li>
              <li>The three centrality measures highlight very different kinds of primary tags. Under degree, the top is dominated by Open World, Survival, and Other, suggesting that the most broadly connected games are those whose tags span common gameplay conventions shared across many genres. Under betweenness, the Other category swells further while Open World and Free to Play also appear prominently, indicating that the bridging positions in the network are held by games with unusual or hybrid tag combinations that link otherwise separate clusters. For eigenvector centrality, the top becomes almost exclusively FPS, Action, and Free to Play, reflecting a tightly-knit shooter core where high-centrality games are connected mainly to other high-centrality games of the same kind.</li>
            </ol>
            <h4>Tag Section</h4>
            <ol>
              <li>In comparison, the Tag network is far sparser than its Game counterpart. With 573 edges among 126 nodes, its density of 0.073 is roughly an eighth of the Game network's, and the diameter of 4 (mean distance 2.223) means tag pairs are noticeably further apart on average.</li>
              <li>A standout observation in the Tag network is the dominance of Multiplayer, which holds by far the highest degree of any tag. This shows that a large share of Steam's most popular titles carry this tag, suggesting that multiplayer is one of the strongest correlates of commercial success on the platform. This makes a lot of sense considering the network effect such games have.</li>
              <li>Examining the backbone reveals what each Louvain community substantively represents. Community 1 (the largest and most central) is the shooter cluster, anchored by Multiplayer, FPS, Shooter, Action, and Free to Play. It captures the dominant family of competitive online shooters like CS:GO. Community 2 groups the open-world survival genre, tying together Open World, Survival, Co-op, Open World Survival Craft, Zombies, and Horror. Community 3 corresponds to singleplayer adventure games, organized around the Singleplayer tag with Adventure, Classic, and Sci-fi attached. Community 4 is the RPG and Souls-like cluster, pulling together RPG, Action RPG, Souls-like, Hack and Slash, Loot, and Difficult, capturing the deep-progression action subgenre. Community 5 represents simulation games, built around Simulation and Automobile Sim. Community 6 is the sports and strategy tactical cluster, gathering Sports, Football (Soccer), PvP, Tactical, Competitive, and e-sports.</li>
            </ol>
          ")
             ),
             wellPanel(
               h3("Data Collection"),
               HTML("
            <p>The data underlying this app was collected through a Python script that called the SteamSpy API to retrieve the Top 100 most-played games on Steam as of May 2026. For each game, the script pulled its rank, concurrent users (CCU), review score, price, and five gameplay tags. The resulting dataset was then exported as a CSV and loaded into R, where the bipartite network of games and tags was constructed and projected into the two one-mode networks analyzed throughout this app.</p>
          ")
             )
    ),
    
    #  Tab 2: Game Section 
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
               plotlyOutput("game_bar", height = "700px")
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
                 "</ul>"
               ))
             ),
             wellPanel(
               h4("Backbone of the game network (disparity filter, alpha = 0.2)"),
               plotOutput("game_backbone", height = "700px")
             ),
             wellPanel(
               h4("Tag-similarity neighborhood of selected game"),
               selectizeInput("game_ego", "Choose a game:",
                              choices = setNames(
                                game_df |> arrange(rank) |> pull(name),
                                game_df |> arrange(rank) |> 
                                  mutate(disp = paste0("#", rank, " ", label)) |> pull(disp)
                              ),
                              selected = "g_582010",
                              options = list(placeholder = "Type to search...")),
               sliderInput("game_ego_threshold",
                           "Minimum edge weight (shared tags):",
                           min = 1, max = game_max_weight, value = 1, step = 1),
               plotOutput("game_ego_plot", height = "600px")
             )
    ),
    
    # Tab 3: Tag Section 
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
               plotlyOutput("tag_bar", height = "800px")
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
               h4("Co-occurrence neighborhood of selected tag"),
               selectizeInput("tag_ego", "Choose a tag:",
                              choices = setNames(
                                tag_df |> arrange(desc(degree)) |> pull(name),
                                tag_df |> arrange(desc(degree)) |> 
                                  mutate(disp = paste0(label, " (degree ", degree, ")")) |> pull(disp)
                              ),
                              selected = "t_Multiplayer",
                              options = list(placeholder = "Type to search...")),
               sliderInput("tag_ego_threshold",
                           "Minimum edge weight (co-occurring games):",
                           min = 1, max = tag_max_weight, value = 1, step = 1),
               plotOutput("tag_ego_plot", height = "600px")
             )
    )
  )
)


# Server
server <- function(input, output, session) {
  
  # ---- Game Section ----
  # Main game-game network, with toggle to size nodes by degree or by CCU
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
  
  # Same game network coloured by Louvain community
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
  
  # Bar chart of games ranked by chosen centrality, with hover tooltips
  output$game_bar <- renderPlotly({
    metric <- input$game_metric
    view_mode <- input$game_bar_view
    
    df <- game_df |> arrange(desc(.data[[metric]]))
    if (view_mode == "top15") {
      df <- df |> slice_head(n = 15)
    }
    
    p <- ggplot(df, aes(x = reorder(label, .data[[metric]]),
                        y = .data[[metric]],
                        fill = primary_tag_grouped,
                        text = paste0(label, "<br>",
                                      metric, ": ", round(.data[[metric]], 3), "<br>",
                                      "Primary tag: ", primary_tag_grouped))) +
      geom_col() + coord_flip() +
      scale_fill_brewer(palette = "Paired") +
      labs(x = "Game", y = metric) +
      theme(axis.text.y = element_text(size = if (view_mode == "all") 5 else 9))
    
    ggplotly(p, tooltip = "text")
  })
  # Backbone of the game network using the disparity filter
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
  # Tag-similarity neighborhood of the focal game, filtered by minimum shared tags
  output$game_ego_plot <- renderPlot({
    # Get the 1-order subgraph around the focal game
    eg <- make_ego_graph(game_net, order = 1, nodes = input$game_ego)[[1]]
    
    # Find the focal game's index within the subgraph
    focal_idx <- which(V(eg)$name == input$game_ego)
    
    # Move to tidygraph, then filter edges, keep only edges touching the focal game
    # meeting the weight threshold 
    eg_tidy <- as_tbl_graph(eg) |>
      activate(edges) |> 
      filter((from == focal_idx | to == focal_idx) & weight >= input$game_ego_threshold)
    
    # Drop isolated nodes
    eg_tidy <- eg_tidy |>
      activate(nodes) |>
      mutate(degree = centrality_degree()) |>
      filter(degree > 0)
    
    # If nothing left, show a message
    if (eg_tidy |> activate(nodes) |> as_tibble() |> nrow() == 0) {
      plot.new()
      text(0.5, 0.5, "No neighbors meet this threshold.\nTry lowering the slider.")
      return()
    }
    
    ggraph(eg_tidy, layout = "kk") +
      geom_edge_link(aes(width = weight), color = "grey60", alpha = 0.7) +
      scale_edge_width(range = c(0.3, 2)) +
      geom_node_point(aes(size = degree), color = "steelblue", alpha = 0.85) +
      scale_size(range = c(3, 10)) +
      geom_node_text(aes(label = label), size = 3, repel = TRUE, max.overlaps = Inf) +
      theme_void() +
      labs(edge_width = "Shared tags", size = "Degree")
  })
  
  # ---- Tag Section ----
  # Main tag co-occurrence network coloured by Louvain community
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
  
  # Bar chart of tags ranked by chosen centrality, with hover tooltips
  output$tag_bar <- renderPlotly({
    metric <- input$tag_metric
    view_mode <- input$tag_bar_view
    
    df <- tag_df |> arrange(desc(.data[[metric]]))
    if (view_mode == "top15") {
      df <- df |> slice_head(n = 15)
    }
    
    p <- ggplot(df, aes(x = reorder(label, .data[[metric]]),
                        y = .data[[metric]],
                        fill = as.factor(cluster),
                        text = paste0(label, "<br>",
                                      metric, ": ", round(.data[[metric]], 3), "<br>",
                                      "Community: ", cluster))) +
      geom_col() + coord_flip() +
      scale_fill_brewer(palette = "Set2") +
      labs(x = "Tag", y = metric, fill = "Community") +
      theme(axis.text.y = element_text(size = if (view_mode == "all") 4 else 9))
    
    ggplotly(p, tooltip = "text")
  })
  
  # Backbone of the tag network using the disparity filter
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
  # Co-occurrence neighborhood of the focal tag, filtered by minimum co-occurring games
  output$tag_ego_plot <- renderPlot({
    # Get the 1-order subgraph around the focal tag
    eg <- make_ego_graph(tag_net, order = 1, nodes = input$tag_ego)[[1]]
    
    # Find the focal tag's index within the subgraph
    focal_idx <- which(V(eg)$name == input$tag_ego)
    
    # Move to tidygraph, then filter edges: keep only edges touching the focal tag
    eg_tidy <- as_tbl_graph(eg) |>
      activate(edges) |> 
      filter((from == focal_idx | to == focal_idx) & weight >= input$tag_ego_threshold)
    
    # Drop isolated nodes
    eg_tidy <- eg_tidy |>
      activate(nodes) |>
      mutate(degree = centrality_degree()) |>
      filter(degree > 0)
    
    if (eg_tidy |> activate(nodes) |> as_tibble() |> nrow() == 0) {
      plot.new()
      text(0.5, 0.5, "No neighbors meet this threshold.\nTry lowering the slider.")
      return()
    }
    
    ggraph(eg_tidy, layout = "kk") +
      geom_edge_link(aes(width = weight), color = "grey60", alpha = 0.7) +
      scale_edge_width(range = c(0.3, 2)) +
      geom_node_point(aes(size = degree), color = "darkorange", alpha = 0.85) +
      scale_size(range = c(3, 10)) +
      geom_node_text(aes(label = label), size = 3, repel = TRUE, max.overlaps = Inf) +
      theme_void() +
      labs(edge_width = "Co-occurring games", size = "Degree")
  })
}

# Run
shinyApp(ui, server)