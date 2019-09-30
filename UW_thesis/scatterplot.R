scatterplotUI <- function(id) {
  ns <- NS(id)
  
  div(
    fluidRow(
      column(12,
        uiOutput(ns("ui_PL_scatterplot"))
      )
    ),
    
    br(),
  
    fluidRow(
      column(4,
        selectInput(ns("xvar"), label = "Choose OX", choices = NULL) 
      ),
      column(4,
        selectInput(ns("yvar"), label = "Choose OY", choices = NULL)
      ),
      column(4,
        selectInput(ns("colour"), label = "Colour", choices = NULL)
      ),
      column(4,
        selectInput(ns("size"), label = "Size", choices = NULL)
      ),
      column(4,
        selectInput(ns("shape"), label = "Shape (max 6 levels)", choices = NULL)
      ),
      column(4,
        sliderInput(ns("alpha"), label = "Alpha (transparency)", min = 0.0, max = 1, value = 1, step = 0.1)
      )
    ),
    
    fluidRow(
      column(12,
        uiOutput(ns("ui_DIV_warn"))
      )
    )
  ) 
}


scatterplot <- function(input, output, session, id, dataset = NULL, OX = NULL, OY = NULL) {
  
  ns <- session$ns
  
  # enable selection from current dataset variables
  choices <- colnames(dataset())
  updateSelectInput(session, "xvar", choices = choices, selected = '')
  updateSelectInput(session, "yvar", choices = choices, selected = '')
  
  # add NULL to be able to deselect variable with selectInput
  choicesWithNone = c("None", choices)
  updateSelectInput(session, "colour", choices = choicesWithNone, selected = '')
  updateSelectInput(session, "size", choices = choicesWithNone, selected = '')
  updateSelectInput(session, "shape", choices = choicesWithNone, selected = '')
    
  # if written as below, every time dataset variable is updated, plot OX and OY selection will default back to
  # first var in dataset and user has to select his vars again
  # observe({
  #   choices <- colnames(dataset())
  #   updateSelectInput(session, "xvar", choices = choices)
  # })

  
  output$PL_scatterplot = renderPlot({
      
      if( !is.null(dataset()) && !(input$xvar == "") && !(input$yvar == "") ) {
        
        p = ggplot( na.omit(dataset()), aes_string(x = input$xvar, y = input$yvar) ) + 
                geom_point(aes_string(
                # factor(input$variable) is not working
                # "aes_string evaluates the entire string, so if you do sprintf("factor(%s)",Variable1) you get the desired result."
                color = ifelse( !(input$colour == "None" || input$colour == ""), sprintf("factor(%s)", input$colour), "NULL" ),
                size  = ifelse( !(input$size == "None" || input$size == ""), sprintf("factor(%s)", input$size), "NULL" ),
                shape = ifelse( !(input$shape == "None" || input$shape == ""), sprintf("factor(%s)", input$shape), "NULL" )
              ),
              alpha = input$alpha
            )
        
        p = p + labs(
          color = ifelse( !(input$colour == "None" || input$colour == ""), input$colour, "NULL"),
          size = ifelse( !(input$size == "None" || input$size == ""), input$size, "NULL"),
          shape = ifelse( !(input$shape == "None" || input$shape == ""), input$shape, "NULL")
        )
        
        p
      }
  })
  
  # Display plot if dataset loaded
  output$ui_PL_scatterplot <- renderUI({
    if ( !is.null(dataset()) ) 
      plotOutput(ns("PL_scatterplot"))
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