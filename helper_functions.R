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
