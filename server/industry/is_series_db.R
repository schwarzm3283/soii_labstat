############################################################################################IS DATA########################################################

##############reactive variables to get codes from text#####################

is_area_selected <- eventReactive( c(input$is_state_submit),{
  
  if(length(input$is_state) == 1){
    
    collect(src_pool(pool) %>% tbl("is_area") %>%
              filter(area_text == local(paste0(input$is_ownership,",",input$is_state))) %>%
              select(area_code)) %>% 
      unlist(use.names = FALSE)
  }
  
  else{
    
    collect(src_pool(pool) %>% tbl("is_area") %>%
              filter(area_text %in% local(paste0(input$is_ownership,",",input$is_state))) %>%
              select(area_code)) %>% 
      unlist(use.names = FALSE)
  }
  
})

is_supersector_selected <- reactive({
  collect(src_pool(pool) %>% tbl("is_supersector") %>%
            filter(supersector_text == input$is_supersector) %>%
            select(supersector_code)) %>% 
    unlist(use.names = FALSE)
})

is_industry_selected <- reactive({
  collect(src_pool(pool) %>% tbl("is_industry") %>%
            filter(industry_text == input$is_industry) %>%
            select(industry_code)) %>% 
    unlist(use.names = FALSE)
})

is_casetype_selected <- reactive({
  collect(src_pool(pool) %>% tbl("is_casetype") %>%
            filter(case_type_text == input$is_casetype) %>%
            select(case_type_code)) %>% 
    unlist(use.names = FALSE)
})

is_datatype_selected <- reactive({
  
  if(length(input$is_datatype) == 1){
    collect(src_pool(pool) %>% tbl("is_datatype") %>%
              filter(data_type_text == input$is_datatype) %>%
              select(data_type_code)) %>% 
      unlist(use.names = FALSE)
  }
  else
    collect(src_pool(pool) %>% tbl("is_datatype") %>%
              filter(data_type_text %in% input$is_datatype) %>%
              select(data_type_code)) %>% 
    unlist(use.names = FALSE)
})


#------------------Datatype rates and numbers button reactives---------------

is_datatype_casetype_code <- reactive({
  
  collect(src_pool(pool) %>% tbl("is_alldata") %>%
            filter(
              case_type_code == local(is_casetype_selected()) &
                area_code == local(is_area_selected()) &
                industry_code == local(is_industry_selected()) &
                supersector_code == local(is_supersector_selected())
            ))
})

is_datatype_casetype_rates <- reactive({
  
  collect(src_pool(pool) %>% tbl("is_datatype") %>%
            filter(data_type_code %in% local(is_datatype_casetype_code()[['data_type_code']])) %>%  
            filter(data_type_text %like% "%Rate%"))
})

is_datatype_casetype_numbers <- reactive({
  is.datatype %>%
    filter(data_type_code %in% is_datatype_casetype_code()[['data_type_code']]) %>%
    filter(str_detect(data_type_text, "Number"))
})

#----------------Title reactive--------------------------

is_tabletitle <- reactive({
  if(length(input$is_state) > 1) {
    statetitle <- "by selected states"
  }
  else {
    statetitle <- input$is_state
  }
  paste(input$is_ownership, input$is_industry, input$is_casetype, statetitle, sep = " - ")
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
        div(style = "display: inline-block;vertical-align:top; float:right;", tags$button(id="is_state_submit", 
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
observeEvent(input$is_state_submit, {
  output$is_2 <- renderUI({
    supersector_area_code <- is.series %>%
      filter(area_code %in% is_area_selected())
    
    supersector_area <- is.supersector %>%
      filter(supersector_code %in% supersector_area_code$supersector_code)
    
    selectizeInput("is_supersector",
                   "Supersector",
                   multiple = TRUE,
                   sort(unique(supersector_area$supersector_text)),
                   options = list(placeholder = 'Choose a Supersector',
                                  maxItems = 1)
    )
  })
})

#-------Industry
observeEvent(input$is_supersector, {
  output$is_3 <- renderUI({
    industry_subsector_code <- is.series %>%
      filter(
        supersector_code %in% is_supersector_selected() &
          area_code %in% is_area_selected()
      )
    
    industry_subsector <- is.industry %>%
      filter(industry_code %in% industry_subsector_code$industry_code)
    
    selectizeInput("is_industry",
                   "Industry",
                   multiple = TRUE,
                   sort(unique(industry_subsector$industry_text)),
                   options = list(placeholder = 'Choose an Industry',
                                  maxItems = 1))
  })
})

#--------Case Type
observeEvent(input$is_industry, {
  output$is_4 <- renderUI({
    casetype_industry_code <- is.series %>%
      filter(
        industry_code %in% is_industry_selected() &
          area_code %in% is_area_selected() &
          supersector_code %in% is_supersector_selected()
      )
    
    casetype_industry <- is.casetype %>%
      filter(case_type_code %in% casetype_industry_code$case_type_code)
    
    selectizeInput("is_casetype",
                   "Case and Data Type",
                   multiple = TRUE,
                   sort(unique(casetype_industry$case_type_text)),
                   options = list(placeholder = 'Choose Case Type',
                                  maxItems = 1))
  })
})

#---------Data Type
observeEvent(input$is_casetype, {
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
                             selected = "")
        ),
        tags$script(
          "$('#is_datadone').click(function() {
          $('#industry_filters').click();
  });")
     )
      )
  })
  })


#####################observers_is##########################

#------------------------------datatype filters-------------------------------

observeEvent(input$is_rates, {
  updateCheckboxGroupInput(
    session = session, inputId = "is_datatype", choices = sort(unique(is_datatype_casetype_rates()[['data_type_text']])), selected = NULL)
})

# Sorting desc
observeEvent(input$is_numbers, {
  updateCheckboxGroupInput(
    session = session, inputId = "is_datatype", choices = sort(unique(is_datatype_casetype_numbers()[['data_type_text']])), selected = NULL)
})

observeEvent(input$is_ownership, {
  updateCheckboxGroupInput(
    session = session, inputId = "is_state", choices = sort(unique(is.area$state)),
    selected = "")
})

#-------disable download until submit----

#-----------------------------main submit---------------------------------------

observeEvent(input$is_submit, {
  withProgress(
    message = 'Loading Data',
    detail = 'This may take a few seconds...',
    max = 1,
    value = 1,
    {
      if (is.null(input$is_state)) {
        showModal(
          modalDialog(
            title = "Error",
            "Please select at least one state!",
            easyClose = TRUE,
            footer = NULL
          )
        )
      }
      
      else if (is.null(input$is_datatype)) {
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
        
        bls_raw <-
          bls_api(is_series_current_id, registrationKey = "c739296b855144a696b037d306a18720") %>%
          select(seriesID, year, value)
        
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
        
        output$downloadBtn <- downloadHandler(
          filename = function() { 
            paste0("SOII_data_", get_time_human(), '.csv')
          },
          content = function(file) {
            my.write(is_series_data, file, header = is_tabletitle(), row.names = FALSE)
          }
        )
        #-------------------------output datatable active data tab        
        output$industry_data_table <-
          DT::renderDataTable(is_series_data, caption = is_tabletitle(), rownames = FALSE)
        
        updateTabItems(session, "industry_tabbox", "Data")
        
        #---------------------output plot
        plot_data <- is_series_data %>%
          spread(SeriesID, Estimate) %>% 
          select(-State) %>% 
          arrange(Year)
        
        output$industry_plot <- renderPlotly({
          industry_plot(df = plot_data)
        })
      }
    }
  )
})

