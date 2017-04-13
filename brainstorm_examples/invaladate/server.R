library(shiny)



shinyServer(function(input, output, session) {
  
  # Partial example
  output$meh <- renderPrint({
    print("Press enter or focusout to update --- ")
    print(input$myTextInput )
  })
  
})