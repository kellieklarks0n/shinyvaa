library(shiny)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  titlePanel("Embargo Breach: Tracing TenantThread's AI Crisis"),
  mainPanel(
    p("Shiny scaffold is working.")
  )
)

server <- function(input, output, session) {
}

shinyApp(ui, server)
