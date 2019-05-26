



### Dataset to work with

words <- c("hello", "world", "my", "old")
userWords <- data.frame(c("hello", "oldy"))

scoring <- function(){
  distances <- NULL
  score <- 0
  
  for (i in 1:length(t(userWords))){
    for (j in 1:length(words)){
      distances <<- c(distances, adist(userWords[[i]], words[j]))
    }
  }
  
  distances <- matrix(distances, nrow = length(t(userWords)), ncol = length(words))
  
  
  for (k in 1:nrow(distances)){
    if (((0 %in% distances[k, ]) | (1 %in% distances[k, ])) == TRUE){
      score <<- score + 1
    }
  }
}





