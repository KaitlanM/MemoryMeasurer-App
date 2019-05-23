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
#install.packages("lubridate")
library(lubridate)

ui <- navbarPage(title = "Memory Measurer",
                 tabPanel("Memorizing Phase",
                          numericInput(inputId = "timerIn", label = "Seconds", value = 30,
                                       min = 20, max = 60, step = 1),
                          textOutput(outputId = "timeleft"),
                          actionButton(inputId = "start", label = "Start!"),
                         "Memorize the following words:",
                         dataTableOutput("wordTable")
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

server <- function(input, output, session){
  ### Reading in the files to sample words from (credit to http://www.ashley-bovan.co.uk/words/partsofspeech.html for the word list)
  oneSyllable <- read.table(file = "1syllablenouns.txt")
  twoSyllable <- read.table(file = "2syllablenouns.txt")
  threeSyllable <- read.table(file = "3syllablenouns.txt")

  oneSyllable <- as.vector(oneSyllable[, 1]) # This needs to be a vector so that we can sample from it
  twoSyllable <- as.vector(twoSyllable[, 1])
  threeSyllable <- as.vector(threeSyllable[, 1])

  twoSyllableSmall <- sample(twoSyllable, 5000) # The vector is too long so we can take a random sample to work with
  threeSyllableSmall <- sample(threeSyllable, 5000)


  output$wordTable <- renderDataTable({data.frame(sample(oneSyllable, 20))})

  ### Timer
  timer <- reactiveVal(20)
  active <- reactiveVal(FALSE)

  output$timeleft <- renderText({
    paste("Time left: ", seconds_to_period(timer()))
  })

  observe({
    invalidateLater(1000, session)
    isolate({
      if(active()) {
        timer(timer() - 1)
        if(timer() < 1){
          active(FALSE)
          showModal(modalDialog(
            title = "Important!", "Time's Up!"
          ))
        }
      }
    })
  })

  observeEvent(input$start, {active(TRUE)})

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








