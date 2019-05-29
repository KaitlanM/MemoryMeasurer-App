#' Title
#'
#' @param system
#' @param user
#'
#' @return
#' @export
#'
#' @examples
#'
scoring <- function(system, user){
  distances <- NULL
  score <- 0
  user <- sort(user)
  tUser <- t(user)

  for (i in 1:length(tUser)) {
    for (j in 1:length(system)) {
      distances <- c(distances, adist(tUser[[i]], system[j]))
    }
  }

  mDistances <- matrix(distances, nrow = length(tUser), ncol = length(system), byrow = TRUE)

  k <- 1
  score_index <- NULL
  for (k in 1:nrow(mDistances)) {
    # if (any(obj %in% mDistances[k, ])) ####
    if ((0 %in% mDistances[k, ]) || (1 %in% mDistances[k, ])) {
      score_index <- c(score_index, k)
      score <- score + 1
    }
  }

  scored_words <- user[score_index]
  l <- 1
  while (l <= (length(score_index) - 1)) {
    for (m in (l + 1):length(score_index)) {
      if((adist(scored_words[l], scored_words[m]) == 0 ||
         adist(scored_words[l], scored_words[m]) == 1) &&
         m != l) {
        score <- score - 1
        l <- l + as.numeric(table(scored_words[l])) - 1
      }
      l <- l + 1
    }
  }
  return (score)
}
