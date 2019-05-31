test_that("plot is produced", {
  # All tests succeed
 set.seed(100)
  numCirc <- 20
  expect_invisible(draw_circle_plot(numCirc))
  expect_silent(draw_circle_plot(numCirc))
})
