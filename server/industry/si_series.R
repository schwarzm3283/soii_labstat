#####################################################################################SI DATA####################################################################

##############reactive variables to get codes from text######################

si_division_selected <- reactive({
  input$si_division
})

si_industry_selected <- reactive({
 input$si_industry
})

si_casetype_selected <- reactive({
  input$si_casetype
})

si_datatype_selected <- reactive({
  si.datatype %>% 
    filter(data_type_text %in% input$si_datatype) %>% 
    select(data_type_code) %>% 
    unlist(use.names = FALSE)
})

#----------------Title reactive--------------------------

si_tabletitle <- reactive({
  paste(input$si_division, input$si_industry, input$si_casetype, sep = " - ")
})


##############filter inputs series 'si' (2002)#####################

output$si_1 <- renderUI({
  
  choices_var_div <- as.list(si.division$division_code)
  names(choices_var_div) <-  si.division$division_name
  
  selectizeInput("si_division", "Division",
              choices = choices_var_div,
              multiple = TRUE, 
              options = list(placeholder = 'Choose a Division',
                             maxItems = 1))
})


observeEvent(input$si_division, {
  output$si_2 <- renderUI({
  
  si_division_selected <- si_division_selected()
  
 industry_division_code <- dbGetQuery(pool, paste("SELECT distinct industry_code FROM si_alldata WHERE division_code =",
        paste(si_division_selected),sep = ""))


  industry_division <- si.industry %>% 
    filter(industry_code %in% str_pad(industry_division_code$industry_code, 4, side = "left", pad = "0"))
  
  choices_var_ind <- as.list(industry_division$industry_code)
  names(choices_var_ind) <-  industry_division$industry_name
  
  selectizeInput("si_industry", "Industry",
                 choices = choices_var_ind,
                 multiple = TRUE, 
                 options = list(placeholder = 'Choose an Industry',
                                maxItems = 1))
})
})


output$si_3 <- renderUI({
  
  if(!is.null(input$si_industry)){
  casetype_industry_code <- si.series %>% 
    filter(industry_code == as.numeric(si_industry_selected()) & division_code == as.numeric(si_division_selected())) %>% 
    select(case_type_code)
  
  casetype_industry <- si.casetype %>% 
    filter(case_type_code %in% casetype_industry_code$case_type_code)
  
  choices_var_cas <- as.list(casetype_industry$case_type_code)
  names(choices_var_cas) <-  casetype_industry$case_type_text
  
  selectizeInput("si_casetype", "Case Type",
              choices = choices_var_cas,
              multiple = TRUE, 
              options = list(placeholder = 'Choose a case type',
                             maxItems = 1))
  }
})


output$si_4 <- renderUI({
  
  if(!is.null(input$si_casetype)){
  datatype_casetype_code <- si.series %>% 
    filter(case_type_code == as.numeric(si_casetype_selected()) & industry_code == as.numeric(si_industry_selected()) & division_code == as.numeric(si_division_selected())) %>% 
    select(data_type_code)
  
  datatype_casetype <- si.datatype %>% 
    filter(data_type_code %in% datatype_casetype_code$data_type_code) %>% 
    select(data_type_text)
  
  checkboxGroupInput("si_datatype", 
                     "Data Type",
                     sort(unique(datatype_casetype$data_type_text)),
                     selected = NULL)
  }
})


#####################observers_si##########################

observeEvent(input$si_submit, {
  withProgress(message = 'Loading Data',
               detail = 'This may take a few seconds...', max = 1, value = 1, {
                 

                 if(!is.null(input$si_datatype)) {
                   
                   si_series_current_id <- paste("SIU", si_division_selected(), si_industry_selected(), si_datatype_selected(), si_casetype_selected(), sep = "")
                   series_sql <- paste("SELECT series_id, year, value FROM si_alldata WHERE series_id IN ('",
                                paste(si_series_current_id,collapse = "','"),"')",sep = "")
                   
                   
                   bls_raw <- dbGetQuery(pool, series_sql)
                   
                   widths =  c(3,2,4,1)
                   names = c("pre", "division_code", "industry_code", "data_type_code", "case_type_code")
                   
                   si_series_data <- series_split(bls_raw, usedatabase = TRUE, seriescol = "series_id", w = widths, n = names) %>%
                     left_join(si.datatype, by = "data_type_code") %>%
                     
                     select(year, series_id, data_type_text, value)
                   names(si_series_data) <- c("Year", "SeriesID", "Data Type", "Estimate")
                   
                   output$industry_data_table <- industry_datatable(si_series_data)
                   
                   updateTabItems(session, "industry_tabbox", "Data")
                   shinyjs::show("downloadBtn")
                   
                   plot_data <- si_series_data %>%
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