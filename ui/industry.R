

tabItem(
  tabName = "industry",
  busyIndicator("Loading Data",wait = 0),
  fluidRow(
    column(3,
           fluidRow(
             column(12,
                    box(
                      title = "Time Period",
                      status = "primary", 
                      busyIndicator("Loading Data",wait = 500),
                      selected = "Description",
                      solidHeader = TRUE, 
                      width = NULL,
                      height = NULL,
                      collapsible = F,
                      uiOutput("industry_tf")
                    ))),
           fluidRow(
             tags$style(".container { border:2px solid steelblue; width: 100%; height: 200px; overflow-y: scroll; }"),
             column(12, uiOutput("industry_filters")
             ))
           ),
    column(9,
           fluidRow(
             column(12,
                    busyIndicator("Loading Data",wait = 500),
                    tabBox(
                      id = "industry_tabbox",
                      width = 12,
                      height = NULL,
                      tabPanel("Description",
                               div(id = "industry_desc",
                        includeMarkdown(file.path("text", "industry_desc.md")))),
                      tabPanel("Data",
                               downloadButton("downloadBtn", "Download CSV"), br(), br(),
                               DT::dataTableOutput("industry_data_table")),
                      tabPanel("Plots", plotlyOutput("industry_plot"))
                    )
             )
           )
    )
  )
)