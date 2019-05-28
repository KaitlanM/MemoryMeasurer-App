source("~/MemoryMeasurer/R/load-words.R")

test_that("scoring works", {
  user1 <- matrix(c("go", "phone", "mouse", "ball", "flight", "hi", "lulun", "dogo", "bird"), ncol = 1)
  system <- matrix(c("phone", "lulu", "mario", "luigi", "boo", "waluigi", "bowser", "donki", "kong", "hi"))

  expect_equal(scoring(system, user1), 3)

  user2 <- sample(system, 6, replace = T)
  expect_lt(scoring(system, user2), 7)

  system <- sample(load_words(), 50)
  user3 <- sample(load_words(), 50)

  # expect_true(is.integer(scoring(system, user3)))
  expect_length(scoring(system, user3), 1)
  expect_gt(scoring(system, user3), -1)
})
