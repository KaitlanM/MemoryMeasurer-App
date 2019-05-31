#' Load words
#'
#' @description Imports a file of every English nouns that are either one, two, or three syllables in length. In the case of two or three syllables, a sample of 5000 is chosen to prevent large vectors.
#'
#' @references \url{http://www.ashley-bovan.co.uk/words/partsofspeech.html}
#'
#' @param wordLength A character vector of either "easy", "medium", or "hard" which determines the number of syllables to use.
#'
#' @return A vector of character strings of length 5000 or 5865 representing English nouns.
#'
#' @examples load_words(wordLength = "easy")
#' @examples load_words(wordLength = "hard")
#'
#' @export
load_words <- function(wordLength) {
  setwd("~/MemoryMeasurer/R")
  ### Reading in the files to sample words from
  if (wordLength == "easy") {
    oneSyllable <- read.table(file = "1syllablenouns.txt")
    oneSyllable <- as.vector(oneSyllable[, 1]) # This needs to be a vector so that we can sample from it later
    return (oneSyllable)}

  if (wordLength == "medium") {
    twoSyllable <- read.table(file = "2syllablenouns.txt")
    twoSyllable <- as.vector(twoSyllable[, 1])
    # The vector is too long  for some Shiny functionality so we can take a random sample to work with
    twoSyllableSmall <- sample(twoSyllable, 5000)
    return (twoSyllableSmall)
  }

  if (wordLength == "hard") {
    threeSyllable <- read.table(file = "3syllablenouns.txt")
    threeSyllable <- as.vector(threeSyllable[, 1])
    threeSyllableSmall <- sample(threeSyllable, 5000)
    return (threeSyllableSmall)
  }
}



