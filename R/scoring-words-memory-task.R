
### TO DO : Make it possible to decide a different distance.


### Dataset to work with

words <- c("hello", "world", "my", "old")
userWords <- data.frame(c("hello", "oldy"))


scoring <- function(system = words, user = userWords, ){
  distances <- NULL
  score <- 0
  tUser <- t(user)

  for (i in 1:length(t(user))){
    for (j in 1:length(system)){
      distances <- c(distances, adist(tUser[[i]], system[j]))
    }
  }

  mDistances <- matrix(distances, nrow = length(tUser), ncol = length(system))


  for (k in 1:nrow(mDistances)){
    if (((0 %in% mDistances[k, ]) | (1 %in% mDistances[k, ])) == TRUE){
      score <- score + 1
    }
  }
  score
}

scoring()






