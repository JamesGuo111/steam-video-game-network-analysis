# Shiny App 
# Section 1. First install and activate all your required packages. 

library(shiny)
library(bslib)

library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)

library(visNetwork)

# Section 2. Design the site in the UI section (US = User Interface). This is where we define how everything looks and 
# how people can use the app. 

ui <-fluidPage(
  
  titlePanel("This is where your title goes"),
  
  page_sidebar(
    title = "subtitle here", 
    sidebar = sidebar ("Menu options"), 
    card(
      card_header("Here's where I would introduce your project"), "put some content here"),
    card(
      card_header("Dynamic Demo 1"), "you could put a caption like so",
      selectInput("select", 
                  "select an option", 
                  choices = list("Option A" = "A", 
                                 "Option B" = "B"),
                  selected =1), 
      textOutput("ourVariable")
      ), 
    
    
    card(card_header("here's a network!"),
         selectInput("size",
                     "choose a centrality measure", 
                     choices = list("Degree Centrality" = "degree", 
                                    "Betweenness Centrality" = "betweenness"), 
                     selected = 1), 
         plotOutput("example_network"), height = "400px"),
    
    
    card(card_header("An interactive network?!"), 
         "we can use the package VisNetwork to make it happen", 
         radioButtons("size_by", "Centrality Measure", 
                      choices = c("Degree" = "degree", 
                      "Betweenness Centrality" = "betweenness"), 
         selected = "degree"),
         visNetworkOutput("int_network"), height = "600px")
    )
  )


# Section 2. The server section defines how our app works. Here's where we will put all the network analysis. 

server <- function(input, output) {
  
  # CARD 1 
  
  output$ourVariable <- renderText({
    paste("Our selected option is", input$select)
  })
  
# let's create a simple example network with 10 nodes and calulate the degree centrality

  # CARD 2 
  
network <- reactive({
  ex_net <- play_gnp(n = 10, p = 0.15, directed = FALSE)
  
  ex_net <- ex_net |> 
    as_tbl_graph()|> 
    activate(nodes) |> 
    mutate(
      degree = centrality_degree(), 
      betweenness = centrality_betweenness())
  
  ex_net
})

# now let's get it visualized and reactive to our choice from above! 

output$example_network <- renderPlot({
  ex_net <- network() 
  
  p<- ggraph(ex_net, layout = "auto") +
    geom_edge_link(alpha = 0.3, color = "grey80") + 
    geom_node_point(aes(size = .data[[input$size]]),
                    color = "pink") + 
    scale_size_continuous(range = c(.5, 10)) + 
    labs(Nodes = input$size) + 
    theme_graph()
  
  p
})

# CARD 3 

# we're going to use another example network like from above but visNetwork requires separate edge and nodes lists 

network2 <- reactive({
  set.seed(123)
  ex_net2 <- play_gnp(n = 15, p = 0.25, directed = FALSE)
  
  ex_net2 <- ex_net2 |> 
    as_tbl_graph()|> 
    activate(nodes) |> 
    mutate(
      degree = centrality_degree(), 
      betweenness = centrality_betweenness())
  
  nodes_df <- ex_net2 |> 
    activate(nodes) |> 
    as_tibble() |> 
    rowid_to_column("id") |> 
    mutate(value = if (input$size_by == "degree") degree else betweenness) # have to give size based on "value" for visNetwork
  
  edges_df <- ex_net2 |> 
    activate(edges) |> 
    as_tibble() |> 
    rename(from = 1, to =2 )
  
  list(nodes = nodes_df, edges = edges_df)
})

output$int_network <- renderVisNetwork({
   net2 <- network2()
   nodes <- net2$nodes
   edges <- net2$edges 
  
   
  visNetwork(nodes, net2$edges) |> 
    
    visNodes(borderWidth = 1, 
             color = list(
               background= "pink", 
               border = "red", 
               highlight =  "purple"))|>
    
    visEdges(
      color = list(color = "purple", highlight = "black")) |> 
    
    visOptions(
      highlightNearest = list(enabled = TRUE, hover = TRUE), 
      nodesIdSelection = FALSE) |>
    
    visInteraction(
      dragNodes = TRUE, 
      dragView = TRUE, 
      zoomView = TRUE) |> 
    
    visPhysics(stabilization = TRUE)
    
})

}

# Run the application 
shinyApp(ui = ui, server = server)


