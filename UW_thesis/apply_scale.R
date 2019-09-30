apply_scaleUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(12,
      selectInput(ns("si_variable"), label = "Choose variable", choices = NULL),
      uiOutput(ns("ui_AB_scale")),
      uiOutput(ns("ui_DIV_warn"))
    )
  )
}



apply_scale <- function(input, output, session, dataset) {
# dataset passed as not reactive:
# -> if there was no dataset loaded, loading it after tab was generated will not make any difference
# -> scale function is applied to the original value of chosen variable, you cannot do eg. scale on scale of var
  
  ns <- session$ns
  
  # Warning if no data loaded
  output$ui_DIV_warn <- renderUI({
    if (is.null(dataset)) {
      div(
        tags$br(),
        span(class = "warn", "No dataset loaded")
      )
    }
  })
  
  # select variable
  choices <- colnames(dataset[sapply(dataset, is.numeric)])
  updateSelectInput(session, "si_variable", choices = choices)
  
  # observe({
  #   choices <- colnames(dataset[sapply(dataset, is.numeric)])
  #   updateSelectInput(session, "si_variable", choices = choices)
  # })
  
  # Action button with custom label
  output$ui_AB_scale <- renderUI({
    if (!is.null(dataset)) {
      actionButton(ns("AB_scale"), label = "Scale data")
    } else {
      shinyjs::disabled(
        actionButton(ns("AB_scale"), label = "Scale data")
      )
    }
  })
  
  # ReactiveValue to return
  toReturn <- reactiveValues(result = NULL,   # new values of updated variable
                             variable = NULL, # name of variable being updated 
                             trigger = NULL)
  
  # Apply function on variable
  observeEvent(input$AB_scale, {
    toReturn$trigger <- ifelse(is.null(toReturn$trigger), 0, toReturn$trigger) + 1
    toReturn$result  <- as.numeric( scale(dataset[, input$si_variable]) ) # convert back to numeric, otherwise Hmisc::describe doesn't work
    toReturn$variable = input$si_variable
  })
  
  return(toReturn)
}