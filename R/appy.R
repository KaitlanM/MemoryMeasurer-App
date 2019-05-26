library(shiny)
ui <- fluidPage(
  sliderInput(inputId = "circleGuess",
              label = "Count the circles and indicate on the slider how many there are.",
              min = 0, max = 50, value = 0),
  actionButton(inputId = "circleDone", label = "Done"),
  textOutput(outputId = "circleAccuracy")
)

server <- function(input, output) {
  numCirc <<- sample(20:50, 1) # The number of circles
  numCircTolerance <<- seq(from = (numCirc - 2), to = (numCirc + 2), by = 1)
  
  correct <- reactiveVal(FALSE)
  accuracyText <- NULL
  
  isolate({
    while (correct() == FALSE){
        if((input$circleGuess %in% numCircTolerance) == TRUE){
          accuracyText <- reactive("That's correct! Move on to the Reciting tab.")
          correct(TRUE)
        } else if ((input$circleGuess %in% numCircTolerance) == FALSE){
          accuracyText <- reactive("That's not correct. Try again.")
        }
      }
  
  })
     
  eventReactive(input$circleDone, {
    output$circleAccuracy <- renderText(accuracyText())
  })
}

shinyApp(ui = ui, server = server)




