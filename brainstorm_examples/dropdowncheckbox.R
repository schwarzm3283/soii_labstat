# func --------------------------------------------------------------------

dropdownButton <- function(label = "", status = c("default", "primary", "success", "info", "warning", "danger"), ..., width = NULL) {
  
  status <- match.arg(status)
  # dropdown button content
  html_ul <- list(
    class = "dropdown-menu",
    style = if (!is.null(width)) 
      paste0("width: ", validateCssUnit(width), ";"),
    lapply(X = list(...), FUN = tags$li, style = "margin-left: 10px; margin-right: 10px;")
  )
  # dropdown button apparence
  html_button <- list(
    class = paste0("btn btn-", status," dropdown-toggle"),
    type = "button", 
    `data-toggle` = "dropdown"
  )
  html_button <- c(html_button, list(label))
  html_button <- c(html_button, list(tags$span(class = "caret")))
  # final result
  tags$div(
    class = "dropdown",
    do.call(tags$button, html_button),
    do.call(tags$ul, html_ul),
    tags$script(
      "$('.dropdown-menu').click(function(e) {
      e.stopPropagation();
});")
  )
}

# app ---------------------------------------------------------------------

library("shiny")
ui <- fluidPage(
  tags$h1("Example dropdown button"),
  br(),
  fluidRow(
    column(
      width = 6,
      dropdownButton(
        label = "Check some boxes", status = "default", width = 80,
        checkboxGroupInput(inputId = "check1", label = "Choose", choices = paste(1:26, ") Choice ", LETTERS))
      ),
      verbatimTextOutput(outputId = "res1")
    ),
    column(
      width = 6,
      dropdownButton(
        label = "Check some boxes", status = "default", width = 80,
        shiny::actionButton(inputId = "a2z", label = "Sort A to Z", icon = icon("sort-alpha-asc")),
        shiny::actionButton(inputId = "z2a", label = "Sort Z to A", icon = icon("sort-alpha-desc")),
        br(),
        shiny::actionButton(inputId = "all", label = "(Un)select all"),
        checkboxGroupInput(inputId = "check2", label = "Choose", choices = paste(1:26, ") Choice ", LETTERS))
      ),
      verbatimTextOutput(outputId = "res2")
    )
  )
)
server <- function(input, output, session) {
  output$res1 <- renderPrint({
    input$check1
  })
  
  # Sorting asc
  observeEvent(input$a2z, {
    updateCheckboxGroupInput(
      session = session, inputId = "check2", choices = paste(1:26, ") Choice ", LETTERS), selected = input$check2
    )
  })
  # Sorting desc
  observeEvent(input$z2a, {
    updateCheckboxGroupInput(
      session = session, inputId = "check2", choices = paste(26:1, ") Choice ", rev(LETTERS)), selected = input$check2
    )
  })
  output$res2 <- renderPrint({
    input$check2
  })
  # Select all / Unselect all
  observeEvent(input$all, {
    if (is.null(input$check2)) {
      updateCheckboxGroupInput(
        session = session, inputId = "check2", selected = paste(1:26, ") Choice ", LETTERS)
      )
    } else {
      updateCheckboxGroupInput(
        session = session, inputId = "check2", selected = ""
      )
    }
  })
}
shinyApp(ui = ui, server = server)