#decomposition
library(memisc)
library(readr)
library(dplyr)
library(tidyr)

tt <- read_csv("./data/leon2.csv")
tt <- read_csv("./data/industryxindustry.csv")

tt2 <- tt%>%
  select(-`source`)
name_r <- colnames(tt2)
tt2[tt2==1] <- 0
tt2[tt2>=1000] <- 0

x <- as.matrix(t(tt2))


matrix_size <- 95

# decomposed_x <- matrixcalc::lu.decomposition(x)
# 
# test<-data.frame(decomposed_x[["U"]])
# test2<-data.frame(decomposed_x[["L"]])
# test3<- decomposed_x$L %*% decomposed_x$U

name_list <- colnames(x)
row.names(x) <- name_r
colnames(x) <- name_r
index_order <- 1:matrix_size

rearrange_index <- function(item_to_move, new_location, matrix_size){

if(item_to_move == 1 && new_location < matrix_size) {
  #do this thing
  new_index <- c(2:new_location,
                 1,
                 (new_location+1):matrix_size)
} else if (item_to_move == 1 && new_location == matrix_size) {
  #do this thing
  new_index <- c(2:matrix_size,
                 1)
} else if (item_to_move != 1 && new_location < matrix_size) {
  
  #general solution
  new_index <- c(1:(item_to_move - 1),
                 (item_to_move + 1):new_location,
                 item_to_move,
                 (new_location+1):matrix_size)
} else {
  new_index <- c(1:(item_to_move - 1),
                 (item_to_move + 1):matrix_size,
                 item_to_move)
}
  return(new_index)
}
  
#pre-process matrix
for(i in 1:matrix_size){
  for (j in 1:matrix_size) {
    if(x[i,j] > x[j,i]){
      #y<-x
      x[i,j] <- x[i,j] - x[j,i]
      x[j,i] <- 0
    } else {
      x[j,i] <- x[j,i] - x[i,j]
      x[i,j] <- 0
    }
      
  }
}

triangulize_matrix <- function(x) {

matrix_size <- length(colnames(x))

repeat {
  
iter_complete = TRUE

for (i in 1:(matrix_size-1)){
  #print(paste("testing", name_list[i]))
  for (n in (i+1):(matrix_size)){
    
    j <- (i+1):n
    iter_number <- length(j)
    if (sum(x[i,j]) > sum(x[j,i])) {
      print(paste(colnames(x)[i], "to move up", iter_number))
      iter_complete <- FALSE
      
      y <- x
      
      #first shift our row to it's new destination
      
      new_index <- rearrange_index(i,i+iter_number,matrix_size)
      
      x <- x %>%
        reorder(1, indices = new_index) %>%
        reorder(2, indices = new_index)
      
      
      
      
      
    
      break
    }
  }
}
  if(iter_complete){
    break
  }
  
}

return(x)

}

write.csv(x,file="x2.csv")


