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

### Reading in the files to sample words from

file1 <- file.choose()
file2 <- file.choose()
file3 <- file.choose()
oneSyllable <- read.table(file = file1)
twoSyllable <- read.table(file = file2)
threeSyllable <- read.table(file = file3)
# credit to http://www.ashley-bovan.co.uk/words/partsofspeech.html for the word list

oneSyllable <- as.vector(oneSyllable[, 1])
twoSyllable <- as.vector(twoSyllable[, 1])
threeSyllable <- as.vector(threeSyllable[, 1])

twoSyllableSmall <- sample(twoSyllable, 5000) # The vector is too long so we can take a random sample to work with
threeSyllableSmall <- sample(threeSyllable, 5000)


library(shiny)
ui <- navbarPage(title = "Memory Measurer",
                 tabPanel("Memorizing Phase",
                         "Memorize the following words:"
                         ),
                 tabPanel("Intermediate Task",
                           sliderInput(inputId = "circleGuess",
                                       label = "Count the circles and indicate on the slider how many there are.",
                                       min = 0, max = 50, value = 0),

                          plotOutput("circles"),
                          actionButton(inputId = "circleDone", label = "Done")
                          ),
                 tabPanel("Reciting",
                          textInput(inputId = "wordsRemembered",
                                    label = "Please type the words that you remember and press enter after each one",
                                    value = "")
                          )

)

server <- function(input, output){
  ### Plot random circles for the intermediate task
  output$circles <- renderPlot({plot(0:11, type = "n", xlab = "", ylab = "", main = "", tck = 0,
                                     xaxt = "n", yaxt = "n") # The empty plot

                    numCirc <- sample(20:50, 1) # The number of circles

                    for (i in 1:numCirc){ # Plot them
                          draw.circle(runif(1, min = 1, max = 10), runif(1, min = 1, max = 10), radius = 0.4,
                                      col = rgb(red = runif(1), green = runif(1), blue = runif(1)))
                                }

  })
}

shinyApp(ui = ui, server = server)








