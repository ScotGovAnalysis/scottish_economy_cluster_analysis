library(dplyr)
library(NetworkToolbox)

#Set working directory to the network project folder
setwd("\\\\s0177a\\datashare\\OCEA\\Temp work area\\fergus\\network_project")

#load data
leontief <- read.csv("./data/leon2.csv")

#remove diagonals and name column
leontief <- leontief %>% select(-Row.Labels)
leontief[leontief >= 1000] <- 0

# remove uninformative sectors
leon2 <- leontief %>% 
  slice(-match(c("Tobacco",
                 "Imputed.rent",
                 "Households.as.employers"),
               colnames(leontief))) %>%
  select(-Tobacco,
         -Imputed.rent,
         -Households.as.employers)

#run community function
community <- louvain(leon2, 1.8)$community

#create user readable tables
community_names <- community %>%
  cbind(colnames(leon2)) %>%
  as.data.frame()

colnames(community_names) <- c("cluster_id","sector")

#write to csv
write.csv(community_names,"new_clusters.csv", row.names = FALSE)

#extra - how to select a community
filter_list <- community_names %>%
  filter(. == 2) %>%
  select(V2) %>%
  pull() %>%
  as.vector()

new_matrix <- leontief %>% slice(match(filter_list,
                                   colnames(leontief))) %>%
  select(filter_list)

community_names2 <- louvain(new_matrix, 1.1)$community %>%
  cbind(colnames(new_matrix)) %>%
  as.data.frame()

community_names<- data.frame(lapply(community_names, as.character), stringsAsFactors=FALSE)


select_cluster_matrix <- function(matrix, cluster_names) {
  
  # filter_list <- community_names %>%
  #   filter(cluster_id == cluster) %>%
  #   select(sector) %>%
  #   pull() %>%
  #   as.vector()
  
  new_matrix <- matrix %>% slice(match(cluster_names,
                                         colnames(matrix))) %>%
    select(cluster_names)
  
  return(new_matrix)
  
}

select_cluster_matrix(leon2,"Wholesale...excl.vehicles")

get_cluster_names <- function(community_names, cluster) {
  
  filter_list <- community_names %>%
    filter(cluster_id == cluster) %>%
    select(sector) %>%
    pull() %>%
    as.vector()
  
  return(filter_list)
  
}

check_for_presence <- function(cluster_names, sector) {
  
  if(sector %in% cluster_names){
    new_cluster <- cluster_names[cluster_names != sector]
    return(new_cluster)
  } else {
    return(cluster_names)
  }
  
}
  
cluster_names<-get_cluster_names(community_names,2)  
check_for_presence(cluster_names, "Accommodation")


#new_matrix <- select_cluster_matrix(leon2, 1)

sigma_in <- function(matrix, cluster_names) {
  
  total_sum <- select_cluster_matrix(matrix, cluster_names) %>%
    sum()
  
  return(total_sum)
  
}

#the_boys <- get_cluster_names(community_names,2)

#sigma_in(leon2,the_boys)

#m <- sum(leon2)
#ki <- 0

sigma_tot <- function(matrix, cluster_names) {
  
  column_total <- matrix %>% 
    select(cluster_names) %>%
    sum()
  
  row_total <- matrix %>%
    slice(match(cluster_names, colnames(matrix))) %>%
    sum()
  
  #this method double counts the internal weights, so we subtract them
  
  incident_sum <- column_total + row_total - 2 * sigma_in(matrix, cluster_names)
  
  return(incident_sum)
}

#sigma_tot(leon2,the_boys)

kiin <- function(matrix, node, cluster_names) {
  
  submatrix <- select_cluster_matrix(matrix, c(node, cluster_names))
  
  column_total <- submatrix %>% 
    select(node) %>%
    sum()
  
  row_total <- submatrix %>%
    slice(match(node, colnames(submatrix))) %>%
    sum()
  
  incident_sum <- column_total + row_total
  
  return(incident_sum)
  
}
kiin(leon2, "Accommodation", the_boys)

ki <- function(matrix, node) {
  
  column_total <- matrix %>% 
    select(node) %>%
    sum()
  
  row_total <- matrix %>%
    slice(match(node), colnames(matrix)) %>%
    sum()
  
  incident_sum <- column_total + row_total
  
  return(incident_sum)
  
}

ki(leon2,"Accommodation")


calculate_delta_q <- function(matrix, sector, cluster_names) {
  
  unita <- (sigma_in(matrix,cluster_names) + kiin(matrix, sector, cluster_names)) / (2 * sum(matrix))
  
  unitb <- ((sigma_tot(matrix,cluster_names) + ki(matrix, sector)) / (2 * sum(matrix)))  ^ 2
  
  unitc <- (sigma_in(matrix,cluster_names) / (2 * sum(matrix)))
  
  unitd <- (sigma_tot(matrix, cluster_names) / (2 * sum(matrix))) ^ 2
  
  unite <- (ki(matrix, sector) / (2 * sum(matrix))) ^ 2
  
  q_in_c <- unita - unitb
  q_out_c <- unitc - unitd - unite
  
  delta_q <- q_in_c - q_out_c
  
  return(delta_q)
  
}

test_names <- community_names
test_names$cluster_id <- 1:95


calculate_louvain_fit <- function(matrix, community_names) {
  
  list_of_sectors <- community_names %>%
    select(sector) %>%
    pull %>%
    as.vector()
  
  list_of_clusters <- community_names %>%
    select(cluster_id) %>%
    pull %>%
    as.vector() %>%
    unique() %>%
    as.numeric()
  
  results <- matrix(nrow = length(list_of_sectors), ncol = length(list_of_clusters))
  row.names(results) <- list_of_sectors
  colnames(results) <- list_of_clusters
  
  for(sector in list_of_sectors) {
    
    print(sector)
    
    for(cluster in list_of_clusters) {
      
    #print(cluster)
      
      original_cluster <- get_cluster_names(community_names, cluster)
      cluster_filter <- check_for_presence(original_cluster,sector)
      
      if(length(cluster_filter) != 0) {
      
      d_q <- calculate_delta_q(matrix, sector, cluster_filter)
      
      results[match(sector,list_of_sectors),cluster] <- d_q
      
      #print(results[match(sector,list_of_sectors),cluster])
      
      } else {
        results[match(sector,list_of_sectors),cluster] <- 0
      } 
      
    }
    
  }
   return(results)
}

results <- calculate_louvain_fit(leon2,community_names)
results2 <- calculate_louvain_fit(leon2,test_names)

#calculate_delta_q(leon2,"Accommodation","Wearing.apparel")


library(tidyr)

long <- results %>%
  as.data.frame() %>%
  tibble::rownames_to_column() %>%
  group_by(rowname) %>%
  gather(cluster,delta_q,2:19)

new_results <- long %>% group_by(rowname) %>% slice(which.max(delta_q))
write.csv(results, "new_results.csv")

#create graphs

for(i in 1:95) {
  
  print(i)
  png(paste0("./graphs2/", rownames(results2[,])[i],".png"))
  
  barplot(results2[i,])
  
  dev.off()
  
}

results3 <- results


my_names <- colnames(results)
my_names %>%
  tibble() %>%
  rename(cluster = ".") %>%
  mutate(new_names = paste0(get_cluster_names(community_names, cluster), collapse = " & "))


for(i in 1:18) {
  my_names[i] <- paste0(get_cluster_names(community_names, i), collapse = " & ")
}



library(plotly)
plot_ly(x = my_names, y = results[1,],
  name = "SF Zoo",
  type = "bar"
)

aggregate_sectors <- function(matrix, sector_names) {
  
  new_column <- rowSums(matrix[sector_names])
  
}

aggregate_sectors <- function(long_io, new_group) {
  
  source_agg <- long_io %>%
    filter(source %in% c(new_group)) %>%
    filter(!target %in% new_group) %>%
    group_by(target) %>%
    summarise(supply = sum(supply)) %>%
    mutate(source = paste(new_group,collapse="+"))
  
  target_agg <- long_io %>%
    filter(target %in% c(new_group)) %>%
    filter(!source %in% new_group) %>%
    group_by(source) %>%
    summarise(supply = sum(supply)) %>%
    mutate(target = paste(new_group,collapse="+"))
  
  new_long_io <- long_io %>%
    filter(!source %in% c(new_group)) %>%
    filter(!target %in% c(new_group)) %>%
    bind_rows(source_agg) %>%
    bind_rows(target_agg)
  
  return(new_long_io)
  
}

convert_matrix_to_long <- function(my_matrix_data) {
  
  matrix_size <- length(my_matrix_data)
  
  long_io <- my_matrix_data %>%
    mutate(source = colnames(my_matrix_data)) %>%
    gather(key=target, value = supply, 1:matrix_size)
  
  return(long_io)
  
}

convert_long_to_matrix <- function(my_long_data) {
  
  new_matrix <- my_long_data %>%
    spread(target,supply, fill = 0) %>%
    ungroup() %>%
    select(-source) %>%
    as.matrix()
  
  return(new_matrix)
  
}

agg_matrix <- leon2
i <- 2

for(i in 18) {
  
  cluster <- get_cluster_names(community_names, i)
  
  print(cluster)
  
  agg_matrix <- agg_matrix %>%
    convert_matrix_to_long() %>%
    aggregate_sectors(cluster) %>%
    convert_long_to_matrix() %>%
    data.frame()
  
}

library(shiny)
library(plotly)

plot_ly(x = my_names, y = results[1,],
        name = "SF Zoo",
        type = "bar"
)

cluster_list<-1:18
cluster<- 1
new_mat <- select_cluster_matrix(leon2, get_cluster_names(community_names, cluster))
my_graph <- graph_from_adjacency_matrix(as.matrix(new_mat), weighted = TRUE)
plot.igraph(simplify(my_graph))

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
