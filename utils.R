# Michael Schwarz
# March 08 2017

# Helper functions for the IIF_labstat tool
library(tidyverse)
library(data.table)

get_time_human <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

# Function ------------------------------------
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

########################Get data functions#############################

get_is_data <- function() {

  setwd("./data")
  temp = list.files(pattern="is\\.\\w*.csv")
  list2env(
    lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
          fread, sep = ";"), envir = .GlobalEnv)
  
  setwd("..")
}

#######################


get_ii_data <- function() {
  
  setwd("./data")
  temp = list.files(pattern="ii\\.\\w*.csv")
  list2env(
    lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
           fread, sep = ";"), envir = .GlobalEnv)
  
  setwd("..")
}

#######################

get_si_data <- function() {
  
  setwd("./data")
  temp = list.files(pattern="si\\.\\w*.csv")
  list2env(
    lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
           fread, sep = ";"), envir = .GlobalEnv)
  
  setwd("..")
}

#######################

get_hs_data <- function() {
  
  setwd("./data")
  temp = list.files(pattern="hs\\.\\w*.csv")
  list2env(
    lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
           fread, sep = ";"), envir = .GlobalEnv)
  
  setwd("..")
}

#######################

get_sh_data <- function() {
  
  setwd("./data")
  temp = list.files(pattern="sh\\.\\w*.csv")
  list2env(
    lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
           fread, sep = ";"), envir = .GlobalEnv)
  
  setwd("..")
}

#######################

#--------------------------------series ID split function ------------------------------------
series_split <- function(df, usedatabase = FALSE, seriescol = "", w = c(), n = c()) {

if (usedatabase){
  temp <- df %>% 
             separate_(seriescol, 
             n, 
             sep=cumsum(w)) %>% 
    select(-year, -value)
  
  if("area_code" %in% colnames(temp)){
  if (seriescol == "seriesID"){
  temp$area_code <- as.integer(temp$area_code)
  }}
  else{
    temp$data_type_code <- as.integer(temp$data_type_code)
  }
    bind_cols(df, temp)
}
else {
end = cumsum(w)
start = c(1, head(end, -1) + 1)
temp <- as.data.frame(mapply(substr, start, end, MoreArgs = list(x=df[[seriescol]])))
names(temp) <- n
bind_cols(df, temp)
}
}

#-----------------custom download output function -------------------------------------
my.write <- function(x, file, header, f = write.csv, ...){
  # create and open the file connection
  datafile <- file(file, open = 'wt')
  # close on exit
  on.exit(close(datafile))
  # if a header is defined, write it to the file (@CarlWitthoft's suggestion)
  if(!missing(header)) writeLines(header,con=datafile)
  # write the file using the defined function and required addition arguments  
  f(x, datafile,...)
}

#-------------------Generate Industry Plots--------------------
industry_plot <- function(df, start_col = 3){
  p <- plot_ly() %>% 
    config(
      displayModeBar = 'hover', showLink = FALSE, displaylogo = FALSE, collaborate = FALSE
    )
  i = start_col
  plot_data <- df
  while (i <= ncol(plot_data)) {
    p <-
      add_trace(
        p,
        x = plot_data$Year,
        y = plot_data[, colnames(plot_data)[i]],
        name = colnames(plot_data)[i],
        type = 'scatter',
        mode = 'lines+markers',
        connectgaps = TRUE
      )
    i = i + 1
  }
  
  p <-
    layout(
      p,
      autosize = TRUE,
      hovermode = "closest",
      showlegend = TRUE,
      xaxis = list(autotick = F, categoryorder = "array", categoryarray = unique(df$year)),
      updatemenus = list(
        list(
          y = 0.8,
          buttons = list(
            
            list(method = "restyle",
                 args = list("type", "scatter"),
                 label = "Line"),
            
            list(method = "restyle",
                 args = list("type", "bar"),
                 label = "Bar")))
        
    )
  )

}

#----------------Industry datatable-------------------------
industry_datatable <- function(df, table_title=NULL){
  if(is.null(table_title)){
    DT::renderDataTable(df, caption = "SOII DATA", options = list(searching = FALSE), rownames = FALSE)
    }
  else{
  DT::renderDataTable(df, caption = table_title, options = list(searching = FALSE), rownames = FALSE)
    }
}
