####Industry Section######

##################timeframe input####################

output$industry_tf <- renderUI({
  selectInput("industry_timeframe", "Select Time Frame",
              c("2014 - Forward",
                "2003 - 2013",
                "2002",
                "1989 - 2001",
                "Pre 1989",
                "Please select timeframe"),
              selected = "Please select timeframe")
})

##############import series scripts#####################################

source(file.path("server/industry", "is_series.R"),  local = TRUE)$value
source(file.path("server/industry", "ii_series.R"),  local = TRUE)$value
source(file.path("server/industry", "si_series.R"),  local = TRUE)$value
source(file.path("server/industry", "hs_series.R"),  local = TRUE)$value
source(file.path("server/industry", "sh_series.R"),  local = TRUE)$value

########################toggle_download############################

observe({
  shinyjs::hide("downloadBtn")
})

########################dynamic filter boxes############################

output$is_statetext <- renderText({ input$is_state })
output$ii_statetext <- renderText({ input$ii_state })

output$industry_filters <- renderUI({
  if (!is.null(input$industry_timeframe)) {
    
    
    if (input$industry_timeframe == "2014 - Forward") {
      if (!exists("is.area")) {
        get_is_data()
      }
      box(
        title = "2014 - Forward Filters",
        status = "primary",
        solidHeader = TRUE,
        width = NULL,
        collapsible = F,
        uiOutput("is_0"),
        div(style = "display: inline-block;vertical-align:top;", uiOutput("is_1")),
        div(style = "display: inline-block;vertical-align:top;", verbatimTextOutput("is_statetext")),
        HTML("<hr>"),
        shinyjs::hidden(
        uiOutput("is_2")),
        uiOutput("is_3"),
        HTML("<hr>"),
        uiOutput("is_4"),
        div(style = "display: inline-block;vertical-align:top;", uiOutput("is_5")),
        div(tags$button(id="is_submit", 
                                                                             type="button", 
                                                                             class="btn action-button btn-success", "Submit"))
      )
      
      
    } else if (input$industry_timeframe == "2003 - 2013") {
      if (!exists("ii.area")) {
        get_ii_data()
      }
      
      box(
        title = "2003 - 2013 Filters",
        status = "primary",
        solidHeader = TRUE,
        width = NULL,
        collapsible = F,
        uiOutput("ii_0"),
        div(style = "display: inline-block;vertical-align:top;", uiOutput("ii_1")),
        div(style = "display: inline-block;vertical-align:top;", verbatimTextOutput("ii_statetext")),
        HTML("<hr>"),
        uiOutput("ii_2"),
        uiOutput("ii_3"),
        HTML("<hr>"),
        uiOutput("ii_4"),
        div(style = "display: inline-block;vertical-align:top;", uiOutput("ii_5")),
        div(tags$button(id="ii_submit", 
                           type="button", 
                           class="btn action-button btn-success", "Submit"))
      )
    } else if (input$industry_timeframe == "2002") {
      if (!exists("si.division")) {
        get_si_data()
      }
      
      box(
        title = "2002 Filters",
        status = "primary",
        solidHeader = TRUE,
        width = NULL,
        collapsible = F,
        uiOutput("si_1"),
        uiOutput("si_2"),
        uiOutput("si_3"),
        uiOutput("si_4"),
        div(tags$button(id="si_submit", 
                        type="button", 
                        class="btn action-button btn-success", "Submit"))
      )
    }
    else if (input$industry_timeframe == "1989 - 2001") {
      if (!exists("sh.division")) {
        get_sh_data()
      }
      
      box(
        title = "1989 - 2001 Filters",
        status = "primary",
        solidHeader = TRUE,
        width = NULL,
        collapsible = F,
        uiOutput("sh_1"),
        uiOutput("sh_2"),
        uiOutput("sh_3"),
        uiOutput("sh_4"),
        div(tags$button(id="sh_submit", 
                        type="button", 
                        class="btn action-button btn-success", "Submit"))
      )
    }
    
    else if (input$industry_timeframe == "Pre 1989") {
      if (!exists("hs.division")) {
        get_hs_data()
      }
      
      box(
        title = "Pre 1989 Filters",
        status = "primary",
        solidHeader = TRUE,
        width = NULL,
        collapsible = F,
        uiOutput("hs_1"),
        uiOutput("hs_2"),
        uiOutput("hs_3"),
        uiOutput("hs_4"),
        div(tags$button(id="hs_submit", 
                        type="button", 
                        class="btn action-button btn-success", "Submit"))
      )
    }
  }
  
})





