library(shiny)
library(ggplot2)
library(tidyverse)
library(magrittr)
library(knitr)

df<-read.csv("C:\\Users\\admin\\OneDrive\\Documents\\VIT\\Sem 6\\D- DV\\J Comp\\SpotifyFeatures.csv")
Df<-df[,c("popularity","acousticness","danceability","energy")]

genres <- df$genre

pca <- prcomp(Df)

kmeans_clustering <- function(df, K) {
  # Fit the k-means model
  clustering_fit <- kmeans(df, centers = K)
  
  # Cluster assignments
  cluster_assignments <- clustering_fit$cluster
  
  return(list(cluster_assignments, clustering_fit))
}

# UI
ui <- fluidPage(
  titlePanel("K-Means Clustering"),
  
  fluidRow(
    #selecting number of clusters
    column(2,numericInput("K", "Number of Clusters", 
                          value = 4, min = 1, max = 9,
                          step = 1)),
    #clustering assignment displayed in plot
    column(9,plotOutput("plot"))
  ),
  fluidRow(column(3,tableOutput("table1")),
           column(7,tableOutput("table2"))
  ),
  
  hr(),
  
  
)

K = 3
cluster_results <- kmeans_clustering(pca$x[, 1:2], K)
# in the server function, you would need to wrap the right side with reactive ({ })

ggplot(data.frame(pca$x[, 1:2]), 
       aes(x = PC1, y = PC2, 
           color = genres, # color by species
           shape = factor(cluster_results[[1]]))) +
  #shape representing cluster assignment
  geom_point(size = 3, show.legend=TRUE) + 
  scale_color_brewer(type = "qual", palette = "Set2") + 
  # change color palette
  scale_shape_manual("cluster",values = c(1:K))+
  ggtitle( paste("K-Means Clustering with K =", K))+
  theme_bw()


df_res <- df # make a copy of the original data used 
df_res$cluster = cluster_results[[1]] # add cluster assignment to df_res
df_res %>%
  group_by(cluster,genre) %>% 
  tally() %>%
  kable()


df_res %>% group_by(cluster) %>%
  summarise(n = n(),
            avg.popularity = mean(popularity),
            avg.acousticness = mean(acousticness),
            avg.danceability = mean(danceability),
            avg.energy = mean(energy)) %>%
  kable()


# Server
server <- function(input, output) {
  # Cluster the data
  cluster_results <- reactive({
    kmeans_clustering(pca$x[, 1:2], input$K)
  })
  
  # Plot the results
  output$plot <- renderPlot({
    # use PCA result to plot (for the purpose of dimensional reduction)
    ggplot(data.frame(pca$x[, 1:2]), aes(x = PC1, y = PC2, 
                                         color = genres, 
                                         # color by species
                                         shape = factor(cluster_results()[[1]]))) + 
      # shape representing cluster assignment
      geom_point(size = 3, show.legend=TRUE) + 
      scale_color_brewer(type = "qual", palette = "Set2") + 
      # change color palette
      scale_shape_manual("cluster",values = c(1:input$K))+
      ggtitle( paste("K-Means Clustering with K =", input$K))+
      theme_bw()
  })
  
  df_res <- df # make a copy of the original data used 
  
  # Table1 showing how many species in each cluster
  output$table1 <- renderTable({
    df_res$cluster = cluster_results()[[1]] # add cluster assignment to df_res
    df_res %>%
      group_by(cluster,genre) %>% 
      tally() # count how many Species in each cluster
  })
  
  # Table2 showing the summary statistics of each cluster
  output$table2 <- renderTable({
    df_res$cluster = cluster_results()[[1]] # add cluster assignment to df_res
    df_res %>% group_by(cluster) %>%
      summarise(n = n(),
                avg.popularity = mean(popularity),
                avg.acousticness = mean(acousticness),
                avg.danceability = mean(danceability),
                avg.energy = mean(energy)) 
  })
  
}

shinyApp(ui, server)