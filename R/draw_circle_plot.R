draw_circle_plot <- function(numCirc) {
  plot(
    0:11,
    type = "n",
    xlab = "",
    ylab = "",
    main = "",
    tck = 0,
    xaxt = "n",
    yaxt = "n"
  ) # The empty plot
  
  for (i in 1:numCirc) {
    # Plot the circles on it
    draw.circle(
      runif(1, min = 1, max = 10),
      runif(1, min = 1, max = 10),
      radius = 0.4,
      col = rgb(
        red = runif(1),
        green = runif(1),
        blue = runif(1)
      )
    )
  }
}