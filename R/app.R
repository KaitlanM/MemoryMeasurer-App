#' @import shiny plotrix lubridate
#'
source("~/MemoryMeasurer/R/scoring-words.R")
source("~/MemoryMeasurer/R/load-words.R")
source("~/MemoryMeasurer/R/draw_circle_plot.R")
ui <- navbarPage(title = "Memory Measurer",
     tabPanel("Instructions",
              tags$h1("This is the Memory Measurer!"),
              tags$h4(
                  tags$p("The goal of the task is to remember as many words as you can."),
                  tags$p("Start by choosing your difficulty level and how much time you would like to spend.
                         Then", tags$strong("memorize!")),
                  tags$p("In between the memorizing and the reciting, there will be a", tags$em("distractor"), "task --
                         don't let it stump you!"),
                  tags$p("Good luck and have fun!")
                )
              ),
     tabPanel("Memorizing Phase",
              sidebarLayout(position = "left",
                sidebarPanel(
                  selectInput(inputId  = "sylChoice",   # User customizes word difficulty
                              label    = "Word Difficulty",
                              choices  = c("Easy -- One Syllable" = "easy",
                                          "Medium -- Two Syllables" = "medium",
                                          "Hard -- Three Syllables" = "hard"),
                              selected = "medium"),
                  numericInput(inputId = "timerIn", # Choose how much time to spend
                               label   = "Seconds",
                               value   = 30,
                               min     = 0,
                               max     = 120,
                               step    = 1),
                  numericInput(inputId = "numWords",    # Choose the number of words
                               label   = "Number of Words",
                               value   = 15,
                               min     = 5,
                               max     = 100,
                               step    = 1),
                  actionButton(inputId = "start",
                               label   = "Start!")
                ),
                mainPanel(
                  tags$h4(textOutput(outputId  = "timeleft")),   # Print how much time is left
                  tags$h2("Memorize the following words:"),
                  column(tableOutput(outputId  = "wordTable"), width = 6) # Show the words
                )
              )
             ),
     tabPanel("Intermediate Task",
              sliderInput(inputId  = "circleGuess", # User can guess the number of circles
                           label   = "Count the circles and indicate on the slider how many there are.",
                           min     = 0, max = 50, value = 0),
              plotOutput(outputId  = "circles"),    # Show the circles
              actionButton(inputId = "circleDone", label = "Done"),
              verbatimTextOutput(outputId = "circleAccuracy") # Feedback for guess
              ),
     tabPanel("Reciting",
              textInput(inputId = "wordsRemembered",    # User types remembered words
                        label = "Please type the words that you remember and press the Submit button after
                                 each one",
                        value = ""),
              actionButton(inputId = "submitWord",
                           label   = "Submit"),
              tableOutput(outputId = "tableRemembered"), # Typed words appear underneath
              actionButton(inputId = "finishSubmit",
                           label   = "I'm Finished"),
              verbatimTextOutput(outputId = "scoreText") # Feedback about score
              )
)

server <- function(input, output, session){

# Memorizing Phase -----------------------------------------------------------

  ### Loading the word data and tabling them
  allWords <- NULL
  observeEvent(input$start, {
    allWords <<- load_words(wordLength = input$sylChoice)
  })

  displayWords <- eventReactive(input$start, {
    wordData <<- sample(allWords, size = input$numWords)
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
        paste("Time left: ", lubridate::seconds_to_period(timer()))
    })
  })

# Intermediate Phase ---------------------------------------------------------

  ### Plot random circles for the intermediate task

  numCirc <- sample(20:30, 1) # The number of circles
  # Some circles may overlap, so the user has a buffer of two when counting
  numCircTolerance <- seq(from = (numCirc - 2), to = (numCirc + 2), by = 1)

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

# Reciting Phase -------------------------------------------------------------

  ### Print the user's words into a table

  data <- matrix()

  # Wait for click to record word
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


  ### Evaluate the words for accuracy and output data
  observeEvent(input$finishSubmit, {
    output$scoreText <- renderText({paste("Your score is", scoring(system = wordData, user = data,
                                                                   wordLength = input$sylChoice), "words. Good job!")})
    write.csv(c(input$sylChoice,
                input$timerIn,
                input$numWords,
                scoring(system = wordData, user = data, wordLength = input$sylChoice)),
                file      = "UserScore.csv",
                row.names = c("Difficulty", "Time", "Number of words", "Score"))
  })
}
runApp(
  shinyApp(ui = ui, server = server)
)

MemoryMeasurer:::runApp()



