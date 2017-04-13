library(shiny)
library(plotly)

# make a chart that we will use throughout
#  this is just for convenience in this example
p <- ggplotly(
  ggplot(mtcars, aes(x=mpg,y=hp,color=factor(cyl))) +
    geom_point() +
    facet_wrap(~cyl, ncol=1)
) %>%
  layout(margin=list(r=100, l=70, t=20, b=70))

# for better layout
#   fluidPage and flexdashboard offers lots of helpers
#   but let's see how we can do it in old-school html/css
ui <- tagList(
  numericInput("nplot","Number of plots",2),
  uiOutput(
    'chartcontainer'
  )
)

server <- function(input, output, session) {
  output$chartcontainer <- renderUI({
    tagList(
      lapply(
        seq_len(input$nplot),
        function(x){
          htmltools::tags$div(
            style="display:block;float:left;width:45%;height:50%;",
            tags$h3(paste0("plot #",x)),
            #NOTE: inside of renderUI, need to wrap plotly chart with as.tags
            htmltools::as.tags(p)
          )
        }
      )
    )
  })
}

shinyApp(ui,server)