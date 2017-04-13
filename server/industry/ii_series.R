#########################################################################II DATA###################################################################

##############reactive variables to get codes from text#####################

ii_area_selected <- eventReactive( c(input$ii_state_submit, input$ii_ownership),{
  
  ii.area %>% 
    filter(area_name %in% paste0(input$ii_ownership,",",input$ii_state)) %>% 
    select(area_code) %>% 
    unlist(use.names = FALSE)
})

ii_supersector_selected <- reactive({
  ii.supersector %>% 
    filter(supersector_name %in% input$ii_supersector) %>% 
    select(supersector_code) %>% 
    unlist(use.names = FALSE)
})

ii_industry_selected <- reactive({
  ii.industry %>%
    filter(industry_name %in% input$ii_industry) %>%
    select(industry_code) %>%
    unlist(use.names = FALSE)
})

ii_casetype_selected <- reactive({
  ii.casetype %>% 
    filter(case_type_text %in% input$ii_casetype) %>% 
    select(case_type_code) %>% 
    unlist(use.names = FALSE)
})

ii_datatype_selected <- reactive({
  ii.datatype %>% 
    filter(data_type_text %in% input$ii_datatype) %>% 
    select(data_type_code) %>% 
    unlist(use.names = FALSE)
})

#------------------Datatype rates and numbers button reactives---------------

ii_datatype_casetype_code <- reactive({
  ii.series %>%
    filter(
      case_type_code %in% ii_casetype_selected() &
        area_code %in% ii_area_selected() &
        industry_code %in% ii_industry_selected() &
        supersector_code %in% ii_supersector_selected()
    )
})

ii_datatype_casetype_rates <- reactive({
  ii.datatype %>%
    filter(data_type_code %in% ii_datatype_casetype_code()[['data_type_code']]) %>%
    filter(str_detect(data_type_text, "Rate"))
})

ii_datatype_casetype_numbers <- reactive({
  ii.datatype %>%
    filter(data_type_code %in% ii_datatype_casetype_code()[['data_type_code']]) %>%
    filter(str_detect(data_type_text, "Number"))
})

#----------------Title reactive--------------------------

ii_tabletitle <- reactive({
  if(length(input$ii_state) > 1) {
    statetitle <- "by selected states"
  }
  else {
    statetitle <- input$ii_state
  }
  paste(input$ii_ownership, input$ii_industry, input$ii_casetype, statetitle, sep = " - ")
})

##############filter inputs series 'ii' (2003 - 2013)#####################

#----Ownership
output$ii_0 <- renderUI({
  selectizeInput("ii_ownership", "Ownwership and State",
                 sort(unique(ii.area$ownership)),
                 options = list(placeholder = 'Choose Ownership'),
                 selected = input$ii_ownership)
})

#-----State
output$ii_1 <- renderUI({
  tags$div(
    id = "statedrop",
    dropdownButton(
      label = "Choose States", status = "primary", width = 300,
      tags$div(
        class = "container",
        div(style = "display: inline-block;vertical-align:top; float:right;", tags$button(id="ii_state_submit", 
                                                                                          type="button", 
                                                                                          class="btn action-button btn-primary dropdown-toggle",
                                                                                          HTML("<i class='fa fa-step-forward'></i> Continue"))),
        checkboxGroupInput("ii_state", "State",
                           sort(unique(ii.area$state)),
                           selected = "")
      ),
      tags$script(
        "$('#ii_state_submit').click(function() {
    $('#industry_filters').click();
  });")
    )
  )
})

#-----Supersector
observeEvent(input$ii_ownership, {
  output$ii_2 <- renderUI({
    supersector_area_code <- ii.series %>%
      filter(area_code %in% ii_area_selected())
    
    supersector_area <- ii.supersector %>%
      filter(supersector_code %in% supersector_area_code$supersector_code)
    
    selectizeInput("ii_supersector",
                   "Supersector",
                   multiple = TRUE,
                   sort(unique(supersector_area$supersector_name)),
                   options = list(placeholder = 'Choose a Supersector',
                                  maxItems = 1)
    )
  })
})

#-------Industry
observeEvent(input$ii_ownership, {
  output$ii_3 <- renderUI({
    industry_subsector_code <- ii.series %>%
      filter(
        supersector_code %in% ii_supersector_selected() &
          area_code %in% ii_area_selected()
      )
    
    industry_subsector <- ii.industry %>%
      filter(industry_code %in% industry_subsector_code$industry_code)
    
    selectizeInput("ii_industry",
                   "Industry",
                   multiple = TRUE,
                   sort(unique(industry_subsector$industry_name)),
                   options = list(placeholder = 'Choose an Industry',
                                  maxItems = 1))
  })
})

#--------Case Type
observeEvent(input$ii_ownership, {
  output$ii_4 <- renderUI({
    casetype_industry_code <- ii.series %>%
      filter(
        industry_code %in% ii_industry_selected() &
          area_code %in% ii_area_selected() &
          supersector_code %in% ii_supersector_selected()
      )
    
    casetype_industry <- ii.casetype %>%
      filter(case_type_code %in% casetype_industry_code$case_type_code)
    
    selectizeInput("ii_casetype",
                   "Case and Data Type",
                   multiple = TRUE,
                   sort(unique(casetype_industry$case_type_text)),
                   options = list(placeholder = 'Choose Case Type',
                                  maxItems = 1))
  })
})

#---------Data Type
observeEvent(input$ii_ownership, {
  output$ii_5 <- renderUI({
    
    tags$div(
      id = "ii_datadrop",
      dropdownButton(
        label = "Choose Data Type",
        status = "primary",
        width = 600,
        tags$div(
          class = "container",
          div(style = "display: inline-block;vertical-align:top;", shiny::actionButton("ii_rates", "Rates")),
          div(style = "display: inline-block;vertical-align:top;", shiny::actionButton("ii_numbers", "Numbers")),
          div(style = "display: inline-block;vertical-align:top;", tags$button(id="ii_datadone", 
                                                                               type="button", 
                                                                               class="btn action-button btn-primary",
                                                                               HTML("<i class='fa fa-step-forward'></i> Done"))),
          checkboxGroupInput("ii_datatype",
                             NULL,
                             sort(
                               unique(ii_datatype_casetype_rates()$data_type_text)
                             ),
                             selected = "")
        ),
        tags$script(
          "$('#ii_datadone').click(function() {
    $('#industry_filters').click();
  });")
      )
    )
  })
})

#####################observers_ii##########################

#------------------------------datatype filters-------------------------------

observeEvent(input$ii_rates, {
  updateCheckboxGroupInput(
    session = session, inputId = "ii_datatype", choices = sort(unique(ii_datatype_casetype_rates()[['data_type_text']])), selected = NULL)
})

# Sorting desc
observeEvent(input$ii_numbers, {
  updateCheckboxGroupInput(
    session = session, inputId = "ii_datatype", choices = sort(unique(ii_datatype_casetype_numbers()[['data_type_text']])), selected = NULL)
})

observeEvent(input$ii_ownership, {
  updateCheckboxGroupInput(
    session = session, inputId = "ii_state", choices = sort(unique(ii.area$state)),
    selected = "")
})

#-----------------------------main submit---------------------------------------

observeEvent(input$ii_submit, {
  withProgress(
    message = 'Loading Data',
    detail = 'This may take a few seconds...',
    max = 1,
    value = 1,
    {
      if (is.null(input$ii_state)) {
        showModal(
          modalDialog(
            title = "Error",
            "Please select at least one state!",
            easyClose = TRUE,
            footer = NULL
          )
        )
      }
      
      else if (is.null(input$ii_datatype)) {
        showModal(
          modalDialog(
            title = "Error",
            "Please select at least one Data Type!",
            easyClose = TRUE,
            footer = NULL
          )
        )
      }
      
      else {
        ii_series_current_id <-
          apply(
            expand.grid(
              "IIU",
              ii_supersector_selected(),
              ii_industry_selected(),
              ii_datatype_selected(),
              ii_casetype_selected(),
              str_pad(ii_area_selected(), width = 3, pad = "0")
            ),
            1,
            paste,
            collapse = ""
          )
        
        bls_raw <- bls_api(ii_series_current_id, registrationKey = "c739296b855144a696b037d306a18720", startyear = "2003", endyear = "2013") %>% 
          select(seriesID, year, value)
        
        widths =  c(3, 3, 6, 1, 1)
        names = c("pre", "sub", "ind", "data_type_code", "case", "area_code")
        
        ii_series_data <-
          series_split(bls_raw,
                       TRUE,
                       seriescol = "seriesID",
                       w = widths,
                       n = names) %>%
          left_join(ii.datatype, by = "data_type_code") %>%
          left_join(ii.area, by = "area_code") %>% 
          select(year, seriesID, state, data_type_text, value) %>% 
          mutate(seriesID = as.character(seriesID))
        names(ii_series_data) <-
          c("Year", "SeriesID", "State", "Data Type", "Estimate")
        
        output$downloadBtn <- downloadHandler(
          filename = function() { 
            paste0("SOII_data_", get_time_human(), '.csv')
          },
          content = function(file) {
            my.write(ii_series_data, file, header = ii_tabletitle(), row.names = FALSE)
          }
        )
        
        output$industry_data_table <-
          DT::renderDataTable(ii_series_data, caption = ii_tabletitle(), rownames = FALSE)
        
        updateTabItems(session, "industry_tabbox", "Data")
        shinyjs::show("downloadBtn")
        
        plot_data <- ii_series_data %>%
          spread(SeriesID, Estimate) %>% 
          select(-State) %>% 
          arrange(Year)
        
        #output plot
        output$industry_plot <- renderPlotly({
          industry_plot(df = plot_data)
        })
      }
    }
  )
})
