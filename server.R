# Michael Schwarz
# Michael 08 2017

# This is the server portion of a shiny app that returns IIF lab stat tdata

source("utils.R")
#import lookup tables data
options(shiny.trace = FALSE)


library(shiny)
library(shinyjs)
library(tidyverse)
library(stringr)
library(blscrapeR)
library(plotly)
library(lazyeval)
library(shinysky)
library(RPostgreSQL)
library(DT)
library(htmltools)
library(pool)
library(DBI)


#------------------db connection------------------  
source(file.path("config", "database_config.R"),  local = TRUE)$value

shinyServer(function(input, output, session) {
  busyIndicator("Loading Data",wait = 0)
  

#---------------Create sidebar menu
  
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("About", tabName = "info", icon = icon("info")),
      menuItem("Industry", tabName = "industry",  icon = icon("database")),
      menuItem("Case and Demographics", tabName = "caseanddemo", icon = icon("database")),
      menuItem("Fatal", tabName = "fatal", icon = icon("database"))
    )
  })
  isolate({updateTabItems(session, "tabs", "info")})
  #include server source files
  source(file.path("server", "industry.R"),  local = TRUE)$value
 
 
  })
