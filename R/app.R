
#' @import shiny plotrix lubridate
### Packages used:
#install.packages("shiny")
library(shiny)
#install.packages("plotrix")
library(plotrix)
#install.packages("lubridate")
library(lubridate)

source("~/MemoryMeasurer/R/scoring-words-memory-task.R")
source("~/MemoryMeasurer/R/load-words.R")
source("~/MemoryMeasurer/R/draw_circle_plot.R")


ui <- navbarPage(title = "Memory Measurer",
                 tabPanel("Instructions", # Replace with own insntructions for the task
                          "This is the Memory Measurer!
                          The goal of the task is to remember as many words as you can.
                          Start by choosing your difficulty level and how much time you would like to spend.
                          Then memorize!
                          In between the memorizing and the reciting, there will be a distractor task -- don't let it stump you!
                          Good luck and have fun :)"
                          ),
                 tabPanel("Memorizing Phase",
                          selectInput(inputId = "sylChoice", label = "Word Difficulty", # User customizes word difficulty
                                      choices = c("Easy -- One Syllable" = "easy",
                                                  "Medium -- Two Syllables" = "medium",
                                                  "Hard -- Three Syllables" = "hard")),
                          numericInput(inputId = "timerIn", label = "Seconds", value = 30, # Choose how much time to spend
                                       min = 0, max = 120, step = 1),
                          numericInput(inputId = "numWords", "Number of Words", value = 15, # Choose the number of words
                                       min = 5, max = 100, step = 1),
                          actionButton(inputId = "start", label = "Start!"),
                          textOutput(outputId = "timeleft"), # Print how much time is left
                         "Memorize the following words:",
                         tableOutput(outputId = "wordTable") # Show the words
                         ),
                 tabPanel("Intermediate Task",
                          sliderInput(inputId = "circleGuess", # Indicate the number of circles
                                       label = "Count the circles and indicate on the slider how many there are.",
                                       min = 0, max = 50, value = 0),
                          plotOutput(outputId = "circles"), # Show the circles
                          actionButton(inputId = "circleDone", label = "Done"),
                          verbatimTextOutput(outputId = "circleAccuracy") # Feedback for guess
                          ),
                 tabPanel("Reciting",
                          textInput(inputId = "wordsRemembered", # User types remembered words
                                    label = "Please type the words that you remember and press the Submit button after each one",
                                    value = ""),
                          actionButton(inputId = "submitWord", label = "Submit"),
                          tableOutput(outputId = "tableRemembered"), # Typed words appear underneath
                          actionButton(inputId = "finishSubmit", label = "I'm Finished"),
                          verbatimTextOutput(outputId = "scoreText") # Feedback about score
                          )

)

server <- function(input, output, session){

  ### Loading the word data and tabling it
  observeEvent(input$start, {
    oneSyllable <<- load_words(wordLength = input$sylChoice)
  })

  displayWords <- eventReactive(input$start, {
    wordData <<- sample(oneSyllable, size = input$numWords)
   })

  output$wordTable <- renderTable({
    data.frame(matrix(displayWords(), ncol = 5))
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
          output$wordTable <- renderTable({
            data.frame(matrix(ncol = 0, nrow = 0))
          })
          activeTimer(FALSE)
          showModal(modalDialog(
            title = "Important!", "Time's Up!"
          ))

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

  numCirc <- sample(20:50, 1) # The number of circles
  numCircTolerance <- seq(from = (numCirc - 2), to = (numCirc + 2), by = 1) # We give the user a buffer of two when counting

  output$circles <- renderPlot({
    draw_circle_plot(numCirc)
  })

  ### Give the user feedback about whether the count was accurate or not.

  accuracyText <- NULL
  makeReactiveBinding("accuracyText")

  observe({
    if (input$circleGuess %in% numCircTolerance) {
      accuracyText <<- ("That's correct! Move on to the Reciting tab.")
    } else {
      accuracyText <<- ("That's not correct. Try again.")
    }
  })

  observeEvent(input$circleDone, {
    output$circleAccuracy <- renderText(accuracyText)
  })

  ### Print the user's words into a table

  data <- matrix()

  userWords <- eventReactive(input$submitWord, {
    data <<- rbind(data, input[["wordsRemembered"]])
    return(data)
  })

  observeEvent(input$submitWord, {
    output$tableRemembered <- renderTable({
      userData <<- data.frame(userWords())[-1, , drop = FALSE]
      colnames(userData) <- ("Guesses")
      return(userData)
      })
  })


  ### Evaluate the words for accuracy
  observeEvent(input$finishSubmit, {
    output$scoreText <- renderText({paste("Your score is", scoring(system = wordData, user = data,
                                                                   wordLength = input$sylChoice), "words. Good job!")})
    write.csv(c(input$sylChoice,
                input$timerIn,
                input$numWords,
                scoring(system = wordData, user = data, wordLength = input$sylChoice)),
                file = "UserScore.csv",
                row.names = c("Difficulty", "Time", "Number of words", "Score"))
  })
}

# runApp(
  shinyApp(ui = ui, server = server)
# )

# MemoryMeasurer:::runApp()



