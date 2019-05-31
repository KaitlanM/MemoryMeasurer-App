#' Scoring Words
#'
#' @description Checks the similarity between system-presented words and user-inputted words using the Levenshtein distance. Checks if words differ by a variable amount of typos. Checks if user words are repeated in order to score accurately.
#'
#' @param system A character vector
#' @param user A single-column dataframe of character strings representing words the user remembered
#' @param wordLength A character vector of either "easy", "medium", or "hard" which determines the number typos to allow. ("easy" = 1 typo, "medium" = 2 typos, "hard" = 3 typos.)
#'
#' @return A single numeric value representing the number of words that were similar.
#'
#' @examples scoring(system = c("hello", "world"), user = c("helo", "you", "there"), wordLength = "easy")
#'
#' @export
scoring <- function(system, user, wordLength){
  distances <- NULL
  score <- 0
  user <- sort(user) # Sort the words so that we can skip duplicates later (see line 53)
  tUser <- t(user)
  tolerance <- 0

  # Allow for more mistakes with harder words
  if (wordLength == "easy") {
    tolerance <- c(0, 1)
  } else if (wordLength == "medium") {
    tolerance <- c(0, 1, 2)
  } else if (wordLength == "hard") {
    tolerance <- c(0, 1, 2, 3)
  }

  # Compute the distance between the user's word and the word which was shown
  for (i in 1:length(tUser)) {
    for (j in 1:length(system)) {
      distances <- c(distances, adist(tUser[[i]], system[j]))
    }
  }

  # Convert to a matrix so that we can later check by row
  mDistances <- matrix(distances, nrow = length(tUser), ncol = length(system), byrow = TRUE)


  # Index the words which have a distance equal to or smaller than the tolerance
  score_index <- NULL
  for (k in 1:nrow(mDistances)) {
    if (any(tolerance %in% mDistances[k, ])) {
      score_index <- c(score_index, k)
      score <- score + 1
    }
  }
  scored_words <- user[score_index]

  # Create a while loop so that if the user repeats a word, the repeats can be skipped rather than scored twice
  l <- 1
  while (l <= (length(score_index) - 1)) {
    for (m in (l + 1):length(score_index)) {
      if (wordLength == "easy" &&
                        ((adist(scored_words[l], scored_words[m])) == 0 ||
                         (adist(scored_words[l], scored_words[m]) == 1)) &&
                        m != l) {
        score <- score - 1 # Preventing double scoring for repeated words
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



