shinyApp(
  ui = shinyUI( fluidPage(
    actionButton("button", "don't press the button"),
    verbatimTextOutput("text")
  )
  ),
  server = function(input, output, session){
    observeEvent(input$button, {
      output$text <- renderText({"ahh you pressed it"})
    })
  }
)

