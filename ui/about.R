tabItem(
  tabName = "info",
  fluidRow(
    box(
      title = "About SOII",
      solidHeader = TRUE,
      status = "primary",
      width = 10, 
      div(
      id = "aboutDesc",
      includeMarkdown(file.path("text", "about.md"))
    )
    )
  )
)