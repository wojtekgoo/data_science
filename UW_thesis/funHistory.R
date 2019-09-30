funHistoryUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(12,
           uiOutput(ns("ui_DIV_history")),
           uiOutput(ns("ui_DIV_warn"))
    )
  )
}



funHistory <- function(input, output, session, histo = NULL) {
  
  ns <- session$ns
  
  # Create tags$li from histo() parameter
  output$ui_DIV_history <- renderUI({
    if (length(histo()) > 0) {
      tags$div(
        tags$ul(HTML(sapply(histo(), function(x) as.character(tags$li(x)))))
      )
    } else {
      tags$div(
        span(class = "warn", "No function used")
      )
    }
  })
}