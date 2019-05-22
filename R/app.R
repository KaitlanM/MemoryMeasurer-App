### Package info:
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

### Packages used:
#install.packages("shiny")
library(shiny)
#install.packages("plotrix")
library(plotrix)

### Plot random number of circles for intermediate activity

plot(1:10, type = "n", bty = "o", xlab = "", ylab = "", main = "") # The empty plot

numCirc <- sample(20:50, 1) # The number of circles

for (i in 1:numCirc){ # Plot them
  draw.circle(runif(1, min = 1, max = 10), runif(1, min = 1, max = 10), radius = 0.4,
              col = rgb(red = runif(1), green = runif(1), blue = runif(1))
              )
}




library(shiny)
ui <- navbarPage(title = "Memory Measurer",
                 tabPanel("Memorizing Phase",
                         "Memorize the following words:"
                         ),
                 tabPanel("Intermediate Task",
                           sliderInput(inputId = "circleGuess",
                                       label = "Count the circles and indicate on the slider how many there are.",
                                       min = 0, max = 100, value = 0),
                          plotOutput("circles")


                          ),
                 tabPanel("Reciting",
                          textInput(inputId = "wordsRemembered",
                                    label = "Please type the words that you remember and press enter after each one",
                                    value = ""))

)

server <- function(input, output){
  output$circles <- renderPlot(1:10, type = "n") # The empty plot

                                numCirc <- sample(20:50, 1) # The number of circles

                                for (i in 1:numCirc){ # Plot them
                                  draw.circle(runif(1, min = 1, max = 10), runif(1, min = 1, max = 10), radius = 0.4,
                                              col = rgb(red = runif(1), green = runif(1), blue = runif(1))
                                  )
                                }

}

shinyApp(ui = ui, server = server)








