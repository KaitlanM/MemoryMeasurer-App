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
scoring <- function(system, user, wordLength){
  distances <- NULL
  score <- 0
  user <- sort(user)
  tUser <- t(user)
  tolerance <- 0

  if (wordLength == "easy") {
    tolerance <- c(0, 1)
  } else if (wordLength == "medium") {
    tolerance <- c(0, 1, 2)
  } else if (wordLength == "hard") {
    tolerance <- c(0, 1, 2, 3)
  }

  for (i in 1:length(tUser)) {
    for (j in 1:length(system)) {
      distances <- c(distances, adist(tUser[[i]], system[j]))
    }
  }

  mDistances <- matrix(distances, nrow = length(tUser), ncol = length(system), byrow = TRUE)

  k <- 1
  score_index <- NULL
  for (k in 1:nrow(mDistances)) {
    if (any(tolerance %in% mDistances[k, ])) {
      score_index <- c(score_index, k)
      score <- score + 1
    }
  }
  ### THIS IS PROBLEMATIC!!!
  scored_words <- user[score_index]
  l <- 1
  while (l <= (length(score_index) - 1)) {
    for (m in (l + 1):length(score_index)) {
      if (wordLength == "easy" &&
                        ((adist(scored_words[l], scored_words[m])) == 0 ||
                         (adist(scored_words[l], scored_words[m]) == 1)) &&
                        m != l) {
        score <- score - 1
        l <- l + as.numeric(table(scored_words[l])) - 1
      } else if (wordLength == "medium" &&
                 ((adist(scored_words[l], scored_words[m])) == 0 ||
                  (adist(scored_words[l], scored_words[m]) == 1) ||
                  (adist(scored_words[l], scored_words[m]) == 2)) &&
                 m != l) {
        score <- score - 1
        l <- l + as.numeric(table(scored_words[l])) - 1
      } else if (wordLength == "hard" &&
                 ((adist(scored_words[l], scored_words[m])) == 0 ||
                  (adist(scored_words[l], scored_words[m]) == 1) ||
                  (adist(scored_words[l], scored_words[m]) == 2) ||
                  (adist(scored_words[l], scored_words[m]) == 3)) &&
                 m != l){
        score <- score - 1
        l <- l + as.numeric(table(scored_words[l])) - 1
      }
      l <- l + 1
    }
  }
  return (score)
}

system <- c("hello", "hi", "there")
user <- c("hi", "hi", "hi", "thats")

wordLength <- ("easy")
scoring(system, user, wordLength)


