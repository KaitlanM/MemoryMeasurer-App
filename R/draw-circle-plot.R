#' Draw a Plot of Circles
#'
#' @description Creates a blank plot with a border and then draws circles at random locations on the x and y axis. The circles each have a random color created using RGB values.
#'
#' @param numCirc This argument is a vector of type numeric which defines the number of circles to draw
#'
#' @return None
#'
#' @examples draw_circle_plot(numCirc = 20)
#'
#' @export
draw_circle_plot <- function(numCirc) {
  # The empty plot
  plot(
    0:11,
    type = "n",
    xlab = "",
    ylab = "",
    main = "",
    tck = 0,
    xaxt = "n",
    yaxt = "n"
  )

  # Plot numCirc amount of circles on it
  for (i in 1:numCirc) {
    plotrix::draw.circle(
      # Plot them at a random place on the x-y axis
      runif(1, min = 1, max = 10),
      runif(1, min = 1, max = 10),
      radius = 0.4,
      # Give each circle a random color
      col = rgb(
        red = runif(1),
        green = runif(1),
        blue = runif(1)
      )
    )
  }
}
