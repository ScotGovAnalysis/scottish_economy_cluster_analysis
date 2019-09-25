library(dplyr)
library(NetworkToolbox)

#load data
leontief <- read.csv("./data/leon4_no_bad_data.csv")

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

source("cluster_metric_functions")

cluster_metrics <- calculate_louvain_fit(leon2,community_names)

save.image(file = "cluster_data.RData")
