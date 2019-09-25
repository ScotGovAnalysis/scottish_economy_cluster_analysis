library(dplyr)

select_cluster_matrix <- function(matrix, cluster_names) {
   
  new_matrix <- matrix %>% slice(match(cluster_names,
                                         colnames(matrix))) %>%
    select(cluster_names)
  
  return(new_matrix)
  
}

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

sigma_in <- function(matrix, cluster_names) {
  #the sum of all connections within a cluster
  
  total_sum <- select_cluster_matrix(matrix, cluster_names) %>%
    sum()
  
  return(total_sum)
  
}

sigma_tot <- function(matrix, cluster_names) {
  
  #sum of all weights from the cluster
  
  column_total <- matrix %>% 
    select(cluster_names) %>%
    sum()
  
  row_total <- matrix %>%
    slice(match(cluster_names, colnames(matrix))) %>%
    sum()
  
  #this method double counts the internal weights, so we subtract them (may need to subtract them twice)
  
  incident_sum <- column_total + row_total - 2 * sigma_in(matrix, cluster_names)
  
  return(incident_sum)
}

kiin <- function(matrix, node, cluster_names) {
  
  #sum of all links from node to cluster
  
  submatrix <- select_cluster_matrix(matrix, c(node, cluster_names))
  
  column_total <- submatrix %>% 
    select(node) %>%
    sum()
  
  row_total <- submatrix %>%
    slice(match(node, colnames(submatrix))) %>%
    sum()
  
  incident_sum <- column_total + row_total - 0
  
  return(incident_sum)
  
}

ki <- function(matrix, node) {
  
  #sum of all links from node
  
  column_total <- matrix %>% 
    select(node) %>%
    sum()
  
  row_total <- matrix %>%
    slice(match(node, colnames(matrix))) %>%
    sum()
  
  incident_sum <- column_total + row_total - 0
  
  return(incident_sum)
  
}

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
    as.numeric() %>%
    sort()
  
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
