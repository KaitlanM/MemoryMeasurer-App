source("~/MemoryMeasurer/R/load-words.R")

test_that("scoring works", {
  user1 <- matrix(c("go", "phone", "mouse", "ball", "flight", "hi", "lulun", "dogo", "bird"), ncol = 1)
  system <- matrix(c("phone", "lulu", "mario", "luigi", "boo", "waluigi", "bowser", "donki", "kong", "hi"))
  wordLength <- "easy"

  expect_equal(scoring(system, user1, wordLength), 3)

  user2 <- sample(system, 6, replace = T)
  expect_lt(scoring(system, user2, wordLength), 7)

  system2 <- sample(load_words(wordLength), 50)
  user3 <- sample(load_words(wordLength), 50)

  expect_length(scoring(system2, user3, wordLength), 1)
  expect_gt(scoring(system2, user3, wordLength), -1)
})


