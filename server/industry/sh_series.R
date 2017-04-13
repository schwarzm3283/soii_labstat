#####################################################################################SI DATA####################################################################

##############reactive variables to get codes from text######################

sh_division_selected <- reactive({
input$sh_division
})

sh_industry_selected <- reactive({
 input$sh_industry
})

sh_casetype_selected <- reactive({
  sh.casetype %>% 
    filter(case_type_text %in% input$sh_casetype) %>% 
    select(case_type_code) %>% 
    unlist(use.names = FALSE)
})

sh_datatype_selected <- reactive({
  sh.datatype %>% 
    filter(date_type_text %in% input$sh_datatype) %>% 
    select(data_type_code) %>% 
    unlist(use.names = FALSE)
})

#----------------Title reactive--------------------------

sh_tabletitle <- reactive({
  paste(input$sh_division, input$sh_industry, input$sh_casetype, sep = " - ")
})



##############filter inputs series 'sh' (1989-2001)#####################

output$sh_1 <- renderUI({
  
  choices_var_div <- as.list(sh.division$division_code)
  names(choices_var_div) <-  sh.division$division_name
  
  selectizeInput("sh_division", "Division",
                 choices = choices_var_div,
                 multiple = TRUE, 
                 options = list(placeholder = 'Choose a Division',
                                maxItems = 1))
})

observeEvent(input$sh_division, {
  output$sh_2 <- renderUI({
    
    sh_division_selected <- sh_division_selected()
    
    industry_division_code <- dbGetQuery(pool, paste("SELECT distinct industry_code FROM sh_alldata WHERE division_code =",
                                                     paste(sh_division_selected),sep = ""))
    
    
    industry_division <- sh.industry %>% 
      filter(industry_code %in% str_pad(industry_division_code$industry_code, 4, side = "left", pad = "0"))
    
    choices_var_ind <- as.list(industry_division$industry_code)
    names(choices_var_ind) <-  industry_division$industry_name
    
    selectizeInput("sh_industry", "Industry",
                   choices = choices_var_ind,
                   multiple = TRUE, 
                   options = list(placeholder = 'Choose an Industry',
                                  maxItems = 1))
  })
})

output$sh_3 <- renderUI({
  
  casetype_industry_code <- sh.series %>% 
    filter(industry_code %in% sh_industry_selected() & division_code %in% sh_division_selected()) %>% 
    select(case_type_code)
  
  casetype_industry <- sh.casetype %>% 
    filter(case_type_code %in% casetype_industry_code$case_type_code) %>% 
    select(case_type_text)
  
  selectInput("sh_casetype", "Case Type",
              sort(unique(casetype_industry$case_type_text)),
              selected = input$sh_casetype)
})


output$sh_4 <- renderUI({
  
  datatype_casetype_code <- sh.series %>% 
    filter(case_type_code %in% sh_casetype_selected() & industry_code %in% sh_industry_selected() & division_code %in% sh_division_selected()) %>% 
    select(data_type_code)
  
  datatype_casetype <- sh.datatype %>% 
    filter(data_type_code %in% datatype_casetype_code$data_type_code) %>% 
    select(date_type_text)
  
  checkboxGroupInput("sh_datatype", 
                     "Data Type",
                     sort(unique(datatype_casetype$date_type_text)),
                     selected = NULL)
})


#####################observers_si##########################

observeEvent(input$sh_submit, {
  withProgress(message = 'Loading Data',
               detail = 'This may take a few seconds...', max = 1, value = 1, {
                 
                 
                 if(!is.null(input$sh_datatype)) {
                   
                   sh_series_current_id <- paste("SHU", sh_division_selected(), sh_industry_selected(), sh_datatype_selected(), sh_casetype_selected(), sep = "")
                   SQL <- paste("SELECT series_id, year, value FROM sh_alldata WHERE series_id IN ('",
                                paste(sh_series_current_id,collapse = "','"),"')",sep = "")
                   
                   
                   bls_raw <- dbGetQuery(pool, SQL)
                   
                   widths =  c(3,2,4,1)
                   names = c("pre", "division_code", "industry_code", "data_type_code", "case_type_code")
                   
                   sh_series_data <- series_split(bls_raw, usedatabase = TRUE, seriescol = "series_id", w = widths, n = names) %>%
                     left_join(sh.datatype, by = "data_type_code") %>%
                     
                     select(year, series_id, date_type_text, value)
                   names(sh_series_data) <- c("Year", "SeriesID", "Data Type", "Estimate")
                   
                   output$industry_data_table <- renderDataTable({
                     busyIndicator("Calculation In progress",wait = 0)
                     sh_series_data
                   })
                   updateTabItems(session, "industry_tabbox", "Data")
                   shinyjs::show("downloadBtn")
                   
                   plot_data <- sh_series_data %>%
                     spread(SeriesID, Estimate)
                   
                   #output plot
                   output$industry_plot <- renderPlotly({
                     industry_plot(df = plot_data)
                   })
                   
                 }
                 else {
                   showModal(modalDialog(
                     title = "Error",
                     "Please select at least one Data Type!",
                     easyClose = TRUE,
                     footer = NULL
                   ))
                 }
               })
})