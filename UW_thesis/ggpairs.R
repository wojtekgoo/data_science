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
  
  check_levels = function(x) {
    result = 0
    if(is.character(x)) result = unique(x)
    if(is.factor(x)) result = length(levels(x))
    
    return(result)
  }
  
  output$PO_ggpairs = renderPlot({
    req(dataset())
    val = dataset()
    
    if(input$CB_stopRefreshing == FALSE) {
      levels = sapply(dataset(), check_levels)
      if(all(levels < 6)) {
        ggpairs(val) + ggplot2::theme(axis.text = ggplot2::element_text(size = 3))
      } else { # print empty plot with text output only
        plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
        text(x = 0.5, y = 0.5, paste("Some columns have more than 6 levels. Plot will not be printed."), 
                              cex = 1.6, col = "black")
      }
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