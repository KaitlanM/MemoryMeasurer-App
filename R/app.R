# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# Define the player's score and input the data

#install.packages("shiny")
library(shiny)
ui <- navbarPage(title = "Memory Measurer",
                 tabPanel("Memorizing Phase",
                         "Memorize the following words: bat, rat, fat, tap"
                         ),
                 tabPanel("Intermediate Task",
                           sliderInput(inputId = "circleGuess", label = "Count the circles and indicate on the slider how many there are.",
                          min = 0, max = 100, value = 0)
                          ),
                 tabPanel("Reciting",
                          textInput(inputId = "wordsRemembered",
                                    label = "Please type the words that you remember and press enter after each one",
                                    value = ""))

)

server <- function(input, output){}

shinyApp(ui = ui, server = server)








