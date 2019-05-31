source("~/MemoryMeasurer/R/load-words.R")

test_that("scoring is accurate", {
  # Test succeeds
  user1 <- matrix(c("go", "phone", "mouse", "ball", "flight", "hi", "lulun", "dogo", "bird"), ncol = 1)
  system <- matrix(c("phone", "lulu", "mario", "luigi", "boo", "waluigi", "bowser", "donki", "kong", "hi"))
  wordLength <- "easy"

  expect_equal(scoring(system, user1, wordLength), 3)
})

test_that("score does not exceed number of user inputs", {
  # Test succeeds
  system <- matrix(c("phone", "lulu", "mario", "luigi", "boo", "waluigi", "bowser", "donki", "kong", "hi"))
  user2 <- sample(system, 6, replace = T)
  wordLength <- "medium"
  expect_lt(scoring(system, user2, wordLength), 7)
})

test_that("score is the right format", {
  # All tests succeed
  wordLength <- "hard"
  system2 <- sample(load_words(wordLength), 50)
  user3 <- sample(load_words(wordLength), 50)

  expect_length(scoring(system2, user3, wordLength), 1) # Score is always a singular value
  expect_gt(scoring(system2, user3, wordLength), -1) # Score is always 0 or positive
  expect_true(is.numeric(scoring(system2, user3, wordLength))) # Score is always numeric
})

