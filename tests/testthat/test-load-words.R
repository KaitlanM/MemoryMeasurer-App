test_that("data is correct length", {
  # All tests succeed
  expect_equal(length(load_words(wordLength = "easy")), 5865)
  expect_equal(length(load_words(wordLength = "medium")), 5000)
  expect_equal(length(load_words(wordLength = "hard")), 5000)
})

test_that("data is correct type", {
  # All tests succeed
  expect_true(is.character(load_words(wordLength = "easy")))
  expect_true(is.character(load_words(wordLength = "medium")))
  expect_true(is.character(load_words(wordLength = "hard")))
})

test_that("data is visible", {
  # All tests succeed
  expect_visible(load_words(wordLength = "easy"))
  expect_visible(load_words(wordLength = "medium"))
  expect_visible(load_words(wordLength = "hard"))
})

test_that("data is a vector", {
  # All tests succeed
  expect_true(is.vector(load_words(wordLength = "easy")))
  expect_true(is.vector(load_words(wordLength = "medium")))
  expect_true(is.vector(load_words(wordLength = "hard")))
})

