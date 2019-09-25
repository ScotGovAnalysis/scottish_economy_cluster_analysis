library(shiny)
library(igraph)
library(plotly)
library(dplyr)
library(shinydashboard)
library(DT)

#run generate_louvain_cluster.R first!
load("cluster_data.RData")

cluster_data <- community_names
louvain_results <- cluster_metrics
leontief_data <- leon2

cluster_list<- cluster_data$cluster_id %>%
  unique() %>%
  as.numeric() %>%
  sort()

#cluster_data3 <- cluster_data2 %>%
#  mutate(sector = as.character(sector)) %>%
#  left_join(weights, by = c("sector" = "name"))

sector_names <- (1:nrow(louvain_results))
names(sector_names) <-  row.names(louvain_results)

#results reactive
results_reactive <- reactiveValues(obj = louvain_results)
community_reactive <- reactiveValues(obj = cluster_data)


panel <- dashboardSidebar(
  
  selectInput("main_1",
              label="Cluster",
              choices = cluster_list
  ),
  selectInput("sector_select",
              label="Economy Sector",
              choices = sector_names
  ),
  actionButton("merge_to_cluster", "Join to cluster"),
  actionButton("split_from_cluster", "Split from cluster")
)

###############################################################
body <- dashboardBody(
  fluidRow(
    box(title = "Modularity score for each sector in cluster",
        plotlyOutput("ClusterPlot")
    ),
    box(title = "Graph of cluster",
        plotOutput("igraph"))

    ),
  fluidRow(
    box(title = "Modularity score for sector against each cluster",
        plotlyOutput("sector_plot")),
    box(title = "Heatmap of cluster plus sector trade",
        plotlyOutput("heatmap_local"))
  )
    
)

ui <- dashboardPage (
  dashboardHeader(title = "Cluster analysis"),
  panel,
  body
)

#######################################################################
server <- function(input, output, session) {
  
  observeEvent(input$merge_to_cluster, {
    
    req(input$main_1)
    
    #cluster <- event_data("plotly_click", source = "barPlot")$x
    cluster <- input$main_1
    
    community_reactive$obj <- community_reactive$obj %>%
      mutate(cluster_id = ifelse(
        sector == names(sector_names[as.numeric(input$sector_select)]),
        cluster,
        cluster_id)
      )
    print(community_reactive$obj)
    
    print(paste(input$sector_select, "cluster in ", cluster))
  })
  
  output$ClusterPlot <- renderPlotly({
    
    data_choice <- input$main_2
    
    temp_results <- louvain_results
    
    number_clusters <- ncol(temp_results)
    
    cluster <- input$main_1
    
    cluster_data <- community_reactive$obj
    
    cluster_name <- get_cluster_names(cluster_data, cluster)
    
    cluster_fit <- louvain_results[cluster_name,cluster]
    
    plot_ly(x = cluster_name, y = cluster_fit,
            name = "Louvain score",
            type = "bar",
            source = "barPlot"
    ) %>%
      config(displayModeBar = F)%>% 
      layout(xaxis=list(title = "Cluster",
                        fixedrange=TRUE,
                        dtick = 1.0)) %>% 
      layout(yaxis=list(title = "Modularity score",
                        tickformat = "",
                        fixedrange=TRUE))
  })
  
  output$sector_plot <- renderPlotly({
    
    data_choice <- input$sector_select
    

      temp_results <- louvain_results

    
    number_clusters <- ncol(temp_results)

    
    plot_ly(x = 1:number_clusters, y = temp_results[as.numeric(input$sector_select),],
            name = "Louvain score",
            type = "bar",
            source = "barPlot"
    ) %>%
      config(displayModeBar = F)%>% 
      layout(xaxis=list(title = "Cluster",
                        fixedrange=TRUE,
                        dtick = 1.0)) %>% 
      layout(yaxis=list(title = "Modularity score",
                        tickformat = "",
                        fixedrange=TRUE))
  })
  
  output$igraph <- renderPlot({
    
    cluster_data <- community_reactive$obj
    cluster <- get_cluster_names(cluster_data, input$main_1)

    
    
    new_mat <- select_cluster_matrix(leontief_data, cluster)
    my_graph <- graph_from_adjacency_matrix(as.matrix(new_mat), weighted = TRUE)

    plot(my_graph, vertex.size = 15,
         vertex.color = if(input$main_1 == 2){
           rgb(91,155,213, max=255)
         } else if (input$main_1 == 11){
           rgb(237,125,49, max=255)
         }else if (input$main_1 == 13){
           rgb(165,165,165, max=255)
         }else if (input$main_1 == 6){
           rgb(255,192,0, max=255)
         }else if (input$main_1 == 5){
           rgb(68,114,196, max=255)
         }else if (input$main_1 == 1){
           rgb(112,173,71, max=255)
         }else if (input$main_1 == 12){
           rgb(31,78,120, max=255)
         }else if (input$main_1 == 10){
           rgb(131,60,12, max=255)
         }else if (input$main_1 == 7){
           rgb(82,82,82, max=255)
         }else {
           rgb(128,96,0, max=255)
         },
         vertex.label= NA,
         vertex.frame.color = NA,
         edge.color = "darkgray",
         edge.width=edge.attributes(my_graph)$weight/10,
         edge.arrow.size=0.5,
         edge.curved = 0.1,
         #edge.curved=seq(-0.7, 0.7, length = ecount(my_graph)),
         layout=layout_in_circle
         #layout=layout_with_kk
    )
    

    
  })
  
  output$selection <- renderPrint({
    print(event_data("plotly_click"))
    s <- event_data("plotly_click", source = "barPlot")
    if (length(s) == 0) {
      "Click on a cell in the heatmap to display a scatterplot"
    } else {
      cat("You selected: \n\n")
      as.list(s)
    }
  })
  
  output$heatmap_local <- renderPlotly({
    
    sector_select <- names(sector_names[as.numeric(input$sector_select)])
    
    new_mat <- select_cluster_matrix(leontief_data,
                                     unique(
                                       c(
                                         get_cluster_names(cluster_data, input$main_1),
                                         sector_select
                                       )
                                     )
    )
                                     #get_cluster_names(cluster_data, input$main_1))
    
    
    axis_template_x <- list(title = "Destination",
                            showgrid = F ,
                            zeroline = F , 
                            nticks = 20 ,
                            size = 80,
                            showline = T ,
                            #title = "AXIS" , 
                            mirror = "all",
                            side = "top",
                            #size = 6,
                            tickangle = 25,
                            tickfont = list(
                              #family = "Old Standard TT, serif",
                              size = 10,
                              color = "black"
                            ))
    
    axis_template_y <- list(title = "Source",
                            showgrid = F ,
                            zeroline = F , 
                            nticks = 20 ,
                            size = 80,
                            showline = T ,
                            #title = "AXIS" , 
                            mirror = "all",
                            autorange = "reversed")
    
    m <- list(
      l = 200,
      r = 10,
      b = 10,
      t = 100,
      pad = 2
    )
    
    plot_ly(x = colnames(new_mat),
            y= colnames(new_mat), 
            z = as.matrix(new_mat),
            type="heatmap",
            colors = colorRamp(c("white", 
                                 "blue")) ) %>%
      config(displayModeBar = F) %>%
      layout(xaxis = axis_template_x,
             yaxis = axis_template_y,
             margin = m
      ) %>% 
      layout(xaxis=list(fixedrange=TRUE)) %>% 
      layout(yaxis=list(fixedrange=TRUE))
    
  })
  
  
  output$full_heat <- renderPlotly({
    
    cluster_data <- community_reactive$obj
    
    ordered_community <- cluster_data %>%
      arrange(cluster_id) %>%
      select(sector) %>%
      pull() %>%
      lapply(as.character) %>%
      unlist()
    
    reordered <- (louvain_results[match(ordered_community, my_names),ordered_community])
    
    plot_ly(x = colnames(reordered),
            y= colnames(reordered), 
            z = as.matrix(reordered),
            type="heatmap",
            colors = colorRamp(c("white", 
                                 "blue")) ) %>%
      config(displayModeBar = F) %>% 
      layout(xaxis=list(title = "Destination",
                        fixedrange=TRUE,
                        dtick = 1.0))
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
