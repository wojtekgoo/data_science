boxplotUI <- function(id) {
  ns <- NS(id)
  
  div(
    fluidRow(
      column(12,
             uiOutput(ns("ui_PL_boxplot"))
      )
    ),
    
    br(),
    
    fluidRow(
      column(4,
        selectInput(ns("xvar"), label = "Choose OX (factors only)", choices = NULL) 
      ),
      column(4,
        selectInput(ns("yvar"), label = "Choose OY", choices = NULL)
      ),
      column(4,
        selectInput(ns("fill"), label = "Fill (factors only)", choices = NULL)
      )

    ),
    
    fluidRow(
      column(12,
             uiOutput(ns("ui_DIV_warn"))
      )
    )
  ) 
}


boxplot <- function(input, output, session, id, dataset = NULL) {
  
  ns <- session$ns
  
  # enable selection from current dataset variables
  choices <- colnames(dataset())
  # add NULL to be able to deselect variable with selectInput
  choices = c("None", choices)
  updateSelectInput(session, "yvar", choices = choices, selected = '')
  
  # on OX and Fill permit only factor variables
  factorChoices <- colnames(dataset()[sapply(dataset(), is.factor)])
  factorChoices = c("None", factorChoices)
  updateSelectInput(session, "xvar", choices = factorChoices, selected = '')
  updateSelectInput(session, "fill", choices = factorChoices, selected = '')
  
  output$PL_boxplot = renderPlot({
    
    if( !is.null(dataset()) && !(input$xvar == "") && !(input$yvar == "") ) {
      # change OX var to factor
      p = ggplot( dataset(), aes_string(x = sprintf("factor(%s)", input$xvar), y = input$yvar) ) + 
        geom_boxplot(aes_string(
          fill = ifelse( !(input$fill == "None" || input$fill == ""), sprintf("factor(%s)", input$fill), "NULL" ) 
                    )
        )
      
      p = p + labs(
        x = input$xvar,
        fill = ifelse( !(input$fill == "None" || input$fill == ""), input$fill, "NULL")
      )
      
      p
    }
  })
  
  # Display plot if dataset loaded
  output$ui_PL_boxplot <- renderUI({
    if ( !is.null(dataset()) ) 
      plotOutput(ns("PL_boxplot"))
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