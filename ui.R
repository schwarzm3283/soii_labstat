# Michael Schwarz
# March 08 2017

# This is the ui portion of a shiny app which returns IIF labstat data

library(shiny)
library(shinyjs)
library(shinydashboard)
library(plotly)
library(blscrapeR)
library(shinysky)
library(shinyBS)
library(DT)

dashboardPage(
  # Dashboard header
  
  dashboardHeader(title = "BLS SOII - ORS"),
  
  # Dashboard sidebar
  dashboardSidebar(sidebarMenu(id = "tabs", sidebarMenuOutput("menu"))),
  
  # Dashboard body
  dashboardBody(
    shinyjs::useShinyjs(),
    busyIndicator("Loading Data",wait = 0),
    
    HTML("<link rel='stylesheet' href='custom.css'>"),
    HTML("<link rel='stylesheet' href='//maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css'>"),

    tabItems(source(file.path("ui", "about.R"),  local = TRUE)$value,
             source(file.path("ui", "industry.R"),  local = TRUE)$value)
  )
)

