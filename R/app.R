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
                                       min = 0, max = 120, step = 1),
                          numericInput(inputId = "numWords", "Number of Words", value = 15,
                                       min = 5, max = 100, step = 1),
                          actionButton(inputId = "start", label = "Start!"),
                          textOutput(outputId = "timeleft"),
                         "Memorize the following words:",
                         dataTableOutput(outputId = "wordTable")
                         ),
                 tabPanel("Intermediate Task",
                          sliderInput(inputId = "circleGuess",
                                       label = "Count the circles and indicate on the slider how many there are.",
                                       min = 0, max = 50, value = 0),
                          plotOutput(outputId = "circles"),
                          actionButton(inputId = "circleDone", label = "Done"),
                          textOutput(outputId = "circleAccuracy")
                          ),
                 tabPanel("Reciting",
                          textInput(inputId = "wordsRemembered",
                                    label = "Please type the words that you remember and press the Submit button after each one",
                                    value = ""),
                          actionButton(inputId = "submitWord", label = "Submit"),
                          dataTableOutput(outputId = "tableRemembered"),
                          actionButton(inputId = "finishSubmit", label = "I'm Finished")
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

  words <- eventReactive(input$start, {
    wordData <<- sample(oneSyllable, size = input$numWords)})

  output$wordTable <- renderDataTable({
    data.frame(matrix(words(), ncol = 5))
    })

  ### Timer (adapted from https://stackoverflow.com/questions/49250167/how-to-create-a-countdown-timer-in-shiny)
  timer <- reactiveVal(30)
  activeTimer <- reactiveVal(FALSE)

  observe({
    invalidateLater(1000, session)
    isolate({
      if(activeTimer()) {
        timer(timer() - 1)
        if(timer() < 1){
          activeTimer(FALSE)
          showModal(modalDialog(
            title = "Important!", "Time's Up!"
          ))
          output$wordTable <- renderDataTable({
            data.frame(matrix(ncol = 0, nrow = 0))
          })
        }
      }
    })
  })

  observeEvent(input$start, {activeTimer(TRUE)})
  observeEvent(input$start, {timer(input$timerIn)})
  observeEvent(input$start, {
    output$timeleft <- renderText({
        paste("Time left: ", seconds_to_period(timer()))
    })
  })


  ### Plot random circles for the intermediate task
  numCirc <- NULL
  numCircTolerance <- NULL

  output$circles <- renderPlot({plot(0:11, type = "n", xlab = "", ylab = "", main = "", tck = 0,
                                     xaxt = "n", yaxt = "n") # The empty plot

                    numCirc <<- sample(20:50, 1) # The number of circles
                    numCircTolerance <<- seq(from = (numCirc - 2), to = (numCirc + 2), by = 1)

                    for (i in 1:numCirc){ # Plot them
                          draw.circle(runif(1, min = 1, max = 10), runif(1, min = 1, max = 10), radius = 0.4,
                                      col = rgb(red = runif(1), green = runif(1), blue = runif(1)))
                                }

  })

  ### Give the user feedback about whether the count was accurate or not. (This is still buggy***)

  accuracyText <- NULL

  observeEvent(input$circleDone, {
    if((input$circleGuess %in% numCircTolerance) == TRUE){
      accuracyText <- ("That's correct! Move on to the Reciting tab.")
    } else if ((input$circleGuess %in% numCircTolerance) == FALSE){
      accuracyText <- ("That's not correct. Try again.")
    }
  })

  observeEvent(input$circleDone, {
    output$circleAccuracy <- renderText(accuracyText)
  })

  ### Print the user's words into a table

  data <- NULL

  userWords <- eventReactive(input$submitWord,{
    data <<- c(data, input[["wordsRemembered"]])
    data
  })

  output$tableRemembered <- renderDataTable({
      data.frame(matrix(userWords()))
     })


  ### Evaluate the words for accuracy


}

shinyApp(ui = ui, server = server)








