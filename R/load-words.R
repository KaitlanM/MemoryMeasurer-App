load_words <- function(wordLength) {
  setwd("~/MemoryMeasurer/R")
  ### Reading in the files to sample words from (credit to http://www.ashley-bovan.co.uk/words/partsofspeech.html for the word list)
  if (wordLength == "easy") {
    oneSyllable <- read.table(file = "1syllablenouns.txt")
    oneSyllable <- as.vector(oneSyllable[, 1]) # This needs to be a vector so that we can sample from it
    return (oneSyllable)}

  if (wordLength == "medium") {
    twoSyllable <- read.table(file = "2syllablenouns.txt")
    twoSyllable <- as.vector(twoSyllable[, 1])
    twoSyllableSmall <- sample(twoSyllable, 5000) # The vector is too long so we can take a random sample to work with
    return (twoSyllable)
  }

  if (wordLength == "hard") {
    threeSyllable <- read.table(file = "3syllablenouns.txt")
    threeSyllable <- as.vector(threeSyllable[, 1])
    threeSyllableSmall <- sample(threeSyllable, 5000)
    return (threeSyllable)
  }
}

