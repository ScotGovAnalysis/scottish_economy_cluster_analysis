#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(igraph)
library(plotly)
library(dplyr)

cluster_list<-1:18
sector_names <- (1:95)
names(sector_names) <-  row.names(results)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Cluster analysis"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("main_1",
                    label="Economy Sector",
                    choices = sector_names
        )#,
        #selectInput("main_2",
        #            label="Cluster",
        #            choices = cluster_list
        #)
        ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("distPlot"),
         #verbatimTextOutput("selection"),
         plotOutput("igraph"),
         plotOutput("group_graph")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
   output$distPlot <- renderPlotly({
     
     #print(input$main_1)
     #s <- event_data("plotly_click", source = "distPlot")
     
     plot_ly(x = 1:18, y = results[as.numeric(input$main_1),],
             name = "Louvain score",
             type = "bar",
             source = "barPlot"
     )
   })
   
   output$igraph <- renderPlot({
     
     #try(
     #cluster <- input$main_2
     cluster <- event_data("plotly_click", source = "barPlot")$x

     
     new_mat <- select_cluster_matrix(leon2, get_cluster_names(community_names, cluster))
     my_graph <- graph_from_adjacency_matrix(as.matrix(new_mat), weighted = TRUE)
     #plot.igraph(simplify(my_graph))
     plot(my_graph, vertex.size = 10,
          edge.width=edge.attributes(my_graph)$weight/10,
          edge.arrow.size=0.5,
          edge.curved = 0.1,
          #edge.curved=seq(-0.7, 0.7, length = ecount(my_graph)),
          layout=layout_in_circle
          #layout=layout_with_kk
     )
     
     # generate bins based on input$bins from ui.R

     #)
     
   })
   
   output$group_graph <- renderPlot({
     
     new_mat <- agg_matrix
     new_mat[new_mat <= 100] <- 0
     colnames(new_mat) <- 1:18
     my_graph <- graph_from_adjacency_matrix(as.matrix(new_mat), weighted = TRUE)
     
     plot(my_graph, vertex.size = 10,
          edge.width=edge.attributes(my_graph)$weight/50,
          edge.arrow.size=0.5,
          edge.curved = 0.1,
          layout=layout_in_circle
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
}



# Run the application 
shinyApp(ui = ui, server = server)

