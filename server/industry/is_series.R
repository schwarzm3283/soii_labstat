############################################################################################IS DATA########################################################

##############reactive variables to store inputs#####################

is_area_selected <- eventReactive( c(input$is_state_submit, input$is_ownership),{
  
  is.area %>% 
    filter(area_text %in% paste0(input$is_ownership,",",input$is_state)) %>% 
    select(area_code) %>% 
    unlist(use.names = FALSE)
})

is_supersector_selected <- reactive({
  input$is_supersector
})

is_industry_selected <- reactive({
  input$is_industry
})

is_casetype_selected <- reactive({
  input$is_casetype
})

is_datatype_selected <- reactive({
  is.datatype %>% 
    filter(data_type_text %in% input$is_datatype) %>% 
    select(data_type_code) %>% 
    unlist(use.names = FALSE)
})

##############filter inputs series 'is' (2014 - forward)#####################

#----Ownership
output$is_0 <- renderUI({
  selectizeInput("is_ownership", "Ownwership and State",
                 sort(unique(is.area$ownership)),
                 options = list(placeholder = 'Choose Ownership'),
                 selected = input$is_ownership)
})

#-----State
output$is_1 <- renderUI({
  tags$div(
    id = "statedrop",
    dropdownButton(
      label = "Choose States", status = "primary", width = 300,
      tags$div(
        class = "container",
        div(style = "display: inline-block;vertical-align:top; float:right;", 
            tags$button(id="is_state_submit", 
              type="button", 
              class="btn action-button btn-primary dropdown-toggle",
              HTML("<i class='fa fa-step-forward'></i> Continue"))),
        checkboxGroupInput("is_state", "State",
                           sort(unique(is.area$state)),
                           selected = "")
      ),
      tags$script(
        "$('#is_state_submit').click(function() {
        $('#industry_filters').click();
});")
  
      
    )
  )
})

#-----Supersector
observe({
output$is_2 <- renderUI({
  busyIndicator("Loading Data",wait = 0)
  
  supersector_area_code <- dbGetQuery(pool, paste("select distinct supersector_code from is_alldata where area_code IN ('",
                                                  paste(is_area_selected(), collapse = "','"),"')",sep = ""))
  
  supersector_area <- is.supersector %>%
    filter(supersector_code %in% supersector_area_code$supersector_code)
  
  choices_var_sup <- as.list(supersector_area$supersector_code)
  names(choices_var_sup) <-  supersector_area$supersector_text
  
  selectizeInput("is_supersector",
                 "Supersector",
                 multiple = TRUE,
                 choices = choices_var_sup,
                 options = list(placeholder = 'Choose a Supersector',
                                maxItems = 1)
    )
  })
})

#-------Industry
observeEvent(input$is_supersector, {
  busyIndicator("Loading Data",wait = 0)
  shinyjs::show(id = "is_3", anim = TRUE)
  output$is_3 <- renderUI({
    
    industry_subsector_code <- dbGetQuery(pool, paste("select distinct industry_code from is_alldata where supersector_code IN ('",
                           paste(is_supersector_selected(), collapse = "','"),"') AND area_code IN ('",
                           paste(is_area_selected(), collapse = "','"),"')",sep = ""))

    
    industry_subsector <- is.industry %>%
      filter(industry_code %in% industry_subsector_code$industry_code)
    
    choices_var_ind <- as.list(industry_subsector$industry_code)
    names(choices_var_ind) <-  industry_subsector$industry_text
    
    selectizeInput("is_industry",
                   "Industry",
                   multiple = TRUE,
                   choices = choices_var_ind,
                   options = list(placeholder = 'Choose an Industry',
                                  maxItems = 1))
  })
})

#--------Case Type
observeEvent(input$is_industry, {
  busyIndicator("Loading Data",wait = 0)
  shinyjs::show(id = "is_4", anim = TRUE)
  output$is_4 <- renderUI({
    
    casetype_industry_code <- dbGetQuery(pool, paste("select distinct case_type_code from is_alldata where supersector_code IN ('",
                           paste(is_supersector_selected(), collapse = "','"),"') AND area_code IN ('",
                           paste(is_area_selected(), collapse = "','"),"') AND industry_code IN ('",
                           paste(is_industry_selected(), collapse = "','"),"')",sep = ""))
    
    casetype_industry <- is.casetype %>%
      filter(case_type_code %in% casetype_industry_code$case_type_code)
    
    choices_var_case <- as.list(casetype_industry$case_type_code)
    names(choices_var_case) <-  casetype_industry$case_type_text
    
    selectizeInput("is_casetype",
                   "Case and Data Type",
                   multiple = TRUE,
                   choices = choices_var_case,
                   options = list(placeholder = 'Choose Case Type',
                                  maxItems = 1))
  })
})

#---------Data Type
observeEvent(input$is_casetype, {
  busyIndicator("Loading Data",wait = 0)
  shinyjs::show(id = "is_5", anim = TRUE)
  output$is_5 <- renderUI({
    
    tags$div(
      id = "is_datadrop",
      dropdownButton(
        label = "Choose Data Type",
        status = "primary",
        width = 600,
        tags$div(
          class = "container",
          div(style = "display: inline-block;vertical-align:top;", shiny::actionButton("is_rates", "Rates")),
          div(style = "display: inline-block;vertical-align:top;", shiny::actionButton("is_numbers", "Numbers")),
          div(style = "display: inline-block;vertical-align:top;", tags$button(id="is_datadone", 
                                                                               type="button", 
                                                                               class="btn action-button btn-primary",
                                                                               HTML("<i class='fa fa-step-forward'></i> Done"))),
          checkboxGroupInput("is_datatype",
                             NULL,
                             sort(
                               unique(is_datatype_casetype_rates()$data_type_text)
                             ),
                             selected = NULL)
        ),
        tags$script(
          "$('#is_datadone').click(function() {
          $('#industry_filters').click();
        });")
     )
      )
  })
  })

#------------------Datatype rates and numbers button reactives---------------

is_datatype_casetype_code <- reactive({
  
  dbGetQuery(pool, paste("select distinct data_type_code from is_alldata where supersector_code IN ('",
                         paste(is_supersector_selected(), collapse = "','"),"') AND area_code IN ('",
                         paste(is_area_selected(), collapse = "','"),"') AND industry_code IN ('",
                         paste(is_industry_selected(), collapse = "','"),"') AND case_type_code IN ('",
                         paste(is_casetype_selected(), collapse = "','"),"')",sep = ""))
  
})

is_datatype_casetype_rates <- reactive({
  is.datatype %>%
    filter(data_type_code %in% is_datatype_casetype_code()[['data_type_code']]) %>%
    filter(str_detect(data_type_text, "Rate"))
})

is_datatype_casetype_numbers <- reactive({
  is.datatype %>%
    filter(data_type_code %in% is_datatype_casetype_code()[['data_type_code']]) %>%
    filter(str_detect(data_type_text, "Number"))
})

#------------------------------datatype filter updates-------------------------------

observeEvent(input$is_rates, {
  updateCheckboxGroupInput(
    session = session, inputId = "is_datatype", choices = is_datatype_casetype_rates()[['data_type_text']], selected = NULL)
})

# Sorting desc
observeEvent(input$is_numbers, {
  updateCheckboxGroupInput(
    session = session, inputId = "is_datatype", choices = is_datatype_casetype_numbers()[['data_type_text']], selected = NULL)
})

# observeEvent(input$is_ownership, {
#   updateCheckboxGroupInput(
#     session = session, inputId = "is_state", choices = sort(unique(is.area$state)),
#     selected = "")
# })


#---------show supersector after state submit and disable submit for required fields

shinyjs::onclick("is_state_submit",
                 shinyjs::show(id = "is_2", anim = TRUE))

observe({
  shinyjs::toggleState("is_submit", !is.null(input$is_state) && !is.null(input$is_datatype))
})

#-----------------------------main submit---------------------------------------

observeEvent(input$is_submit, {
  
      #---------------------Begin Progress indicator---------------------------------
  withProgress(
    message = 'Loading Data',
    detail = 'This may take a few seconds...',
    max = 1,
    value = 1,
    {
      
      #----------------------Generate table title-------------------------------------
      industry_title <- is.industry %>% filter(industry_code == is_industry_selected())
      casetype_title <- is.casetype %>% filter(case_type_code == is_casetype_selected())
      
      is_tabletitle <- 
        if(length(input$is_state) > 1) {
          statetitle <- "by selected states"
          paste(input$is_ownership, industry_title$industry_text, casetype_title$case_type_text, statetitle, sep = " - ")
        }
      else {
        statetitle <- input$is_state
        paste(input$is_ownership, industry_title$industry_text, casetype_title$case_type_text, statetitle, sep = " - ")
      }
      
      
      #------------------Build list of series id's--------------------------------------
      is_series_current_id <-
        apply(
          expand.grid(
            "ISU",
            is_supersector_selected(),
            is_industry_selected(),
            is_datatype_selected(),
            is_casetype_selected(),
            str_pad(is_area_selected(), width = 3, pad = "0")
          ),
          1,
          paste,
          collapse = ""
        )
      
      #------------------------Get estimates from database-----------------------------------
      bls_raw <- dbGetQuery(pool, paste("select series_id, year, value from is_alldata where series_id IN ('",
                                        paste(is_series_current_id, collapse = "','"),"')",sep = "")) %>% 
        select(seriesID = series_id, year, value)
      
      # bls_raw <-
      #   bls_api(is_series_current_id, registrationKey = "c739296b855144a696b037d306a18720") %>%
      #   select(seriesID, year, value)
      
      
      #------------------Join data with variable names to create table dataset---------------
      widths =  c(3, 3, 6, 1, 1)
      names = c("pre", "sub", "ind", "data_type_code", "case", "area_code")
      
      is_series_data <-
        series_split(bls_raw,
                     TRUE,
                     seriescol = "seriesID",
                     w = widths,
                     n = names) %>%
        left_join(is.datatype, by = "data_type_code") %>%
        left_join(is.area, by = "area_code") %>% 
        select(year, seriesID, state, data_type_text, value) %>% 
        mutate(seriesID = as.character(seriesID))
      names(is_series_data) <-
        c("Year", "SeriesID", "State", "Data Type", "Estimate")
      
      #-------------------csv download button handler---------------------------
      output$downloadBtn <- downloadHandler(
        filename = function() { 
          paste0("SOII_data_", get_time_human(), '.csv')
        },
        content = function(file) {
          my.write(is_series_data, file, header = is_tabletitle, row.names = FALSE)
        }
      )
      #-------------------------output datatable activate data tab and download button       
      output$industry_data_table <- industry_datatable(is_series_data, is_tabletitle)
      
      
      updateTabItems(session, "industry_tabbox", "Data")
      shinyjs::show("downloadBtn")
      
      #---------------------Generate plot dataset-----------------------------------------
      plot_data <- is_series_data %>%
        spread(SeriesID, Estimate) %>% 
        select(-State) %>% 
        arrange(Year)
      
      #-----------------------Output plot-------------------------------------------------
      output$industry_plot <- renderPlotly({
        industry_plot(df = plot_data)
      })
  })#end progress indicator
})

