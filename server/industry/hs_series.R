#####################################################################################SI DATA####################################################################

##############reactive variables to get codes from text######################

hs_division_selected <- reactive({
  hs.division %>% 
    filter(division_name %in% input$hs_division) %>% 
    select(division_code) %>% 
    unlist(use.names = FALSE)
})

hs_industry_selected <- reactive({
  hs.industry %>% 
    filter(industry_name %in% input$hs_industry) %>% 
    select(industry_code) %>% 
    unlist(use.names = FALSE)
})

hs_casetype_selected <- reactive({
  hs.casetype %>% 
    filter(case_type_text %in% input$hs_casetype) %>% 
    select(case_type_code) %>% 
    unlist(use.names = FALSE)
})

hs_datatype_selected <- reactive({
  hs.datatype %>% 
    filter(data_type_text %in% input$hs_datatype) %>% 
    select(data_type_code) %>% 
    unlist(use.names = FALSE)
})

#----------------Title reactive--------------------------

hs_tabletitle <- reactive({
  paste(input$hs_division, input$hs_industry, input$hs_casetype, sep = " - ")
})


##############filter inputs series 'hs' (pre 1989)#####################

output$hs_1 <- renderUI({
  selectInput("hs_division", "Division",
              sort(unique(hs.division$division_name)),
              selected = input$hs_division)
})

output$hs_2 <- renderUI({
  
  industry_division_code <- hs.series %>% 
    filter(division_code %in% hs_division_selected()) %>% 
    select(industry_code)
  
  industry_division <- hs.industry %>% 
    filter(industry_code %in% industry_division_code$industry_code) %>% 
    select(industry_name)
  
  selectInput("hs_industry", "Industry",
              sort(unique(industry_division$industry_name)),
              selected = input$hs_industry)
})

output$hs_3 <- renderUI({
  
  casetype_industry_code <- hs.series %>% 
    filter(industry_code %in% hs_industry_selected() & division_code %in% hs_division_selected()) %>% 
    select(case_type_code)
  
  casetype_industry <- hs.casetype %>% 
    filter(case_type_code %in% casetype_industry_code$case_type_code) %>% 
    select(case_type_text)
  
  selectInput("hs_casetype", "Case Type",
              sort(unique(casetype_industry$case_type_text)),
              selected = input$hs_casetype)
})


output$hs_4 <- renderUI({
  
  datatype_casetype_code <- hs.series %>% 
    filter(case_type_code %in% hs_casetype_selected() & industry_code %in% hs_industry_selected() & division_code %in% hs_division_selected()) %>% 
    select(data_type_code)
  
  datatype_casetype <- hs.datatype %>% 
    filter(data_type_code %in% datatype_casetype_code$data_type_code) %>% 
    select(data_type_text)
  
  checkboxGroupInput("hs_datatype", 
                     "Data Type",
                     sort(unique(datatype_casetype$data_type_text)),
                     selected = NULL)
})


#####################observers_hs##########################

observeEvent(input$hs_submit, {
  withProgress(
    message = 'Loading Data',
    detail = 'This may take a few seconds...',
    max = 1,
    value = 1,
    {
      if (is.null(input$hs_datatype)) {
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
        hs_series_current_id <- paste("HSU", hs_division_selected(), hs_industry_selected(), hs_datatype_selected(), hs_casetype_selected(), sep = "")
        
        
        bls_raw <<- bls_api(hs_series_current_id, registrationKey = "c739296b855144a696b037d306a18720", startyear = "1970", endyear = "1989") %>% 
          select(seriesID, year, value)
        
        widths =  c(3,2,4,1)
        names = c("pre", "division_code", "industry_code", "data_type_code", "case_type_code")
        
        hs_series_data <-
          series_split(bls_raw,
                       TRUE,
                       seriescol = "seriesID",
                       w = widths,
                       n = names) %>%
          left_join(hs.datatype, by = "data_type_code")%>% 
          select(year, seriesID, data_type_text, value) %>% 
          mutate(seriesID = as.character(seriesID))
        names(hs_series_data) <-
          c("Year", "SeriesID", "Data Type", "Estimate")
        
        hs_series_data <- as.data.frame(apply(hs_series_data,2,function(x)gsub('\\s+', '',x)))
        
        output$downloadBtn <- downloadHandler(
          filename = function() { 
            paste0("SOII_data_", get_time_human(), '.csv')
          },
          content = function(file) {
            my.write(hs_series_data, file, header = hs_tabletitle(), row.names = FALSE)
          }
        )
        
        output$industry_data_table <-
          DT::renderDataTable(hs_series_data, caption = hs_tabletitle(), rownames = FALSE)
        
        updateTabItems(session, "industry_tabbox", "Data")
        shinyjs::show("downloadBtn")
        
        plot_data <- hs_series_data %>%
          spread(SeriesID, Estimate) %>% 
          arrange(Year)
        
        #output plot
        output$industry_plot <- renderPlotly({
          industry_plot(df = plot_data)
        })
      }
    })
})