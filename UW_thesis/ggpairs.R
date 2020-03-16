ggPairsUI <- function(id) {
  ns <- NS(id)
  
  div(
    checkboxInput(ns("CB_stopRefreshing"), "Stop refreshing", value = TRUE),
    fluidRow(
      column(width = 12,
        plotOutput(ns("PO_ggpairs"), height = 1000) %>% withSpinner(color="#0dc5c1")
      )
    ),
    
    br(),
    
    fluidRow(
      column(12,
        uiOutput(ns("ui_DIV_warn"))
      )
    )
  ) # div 
}


ggPairs <- function(input, output, session, id, dataset = NULL) {
  
  ns <- session$ns
  
  output$PO_ggpairs = renderPlot({
    req(dataset())
    val = dataset()
    
    if(input$CB_stopRefreshing == FALSE) {
      ggpairs(val) + ggplot2::theme(axis.text = ggplot2::element_text(size = 3))
    }
  })

  
  # Warning if no data loaded
  output$ui_DIV_warn <- renderUI({
    if ( is.null(dataset()) ) {
      tags$div(
        span(class = "warn", "No dataset loaded")
      )
    }
  })
}