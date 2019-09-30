apply_functionUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(12,
        selectInput(ns("si_variable"), label = "Choose variable", choices = NULL),
        uiOutput(ns("ui_RB_function")),
        uiOutput(ns("ui_AB_apply")),
        uiOutput(ns("ui_DIV_warn"))
    )
  )
}


apply_function <- function(input, output, session, id, dataset) {
  
  ns <- session$ns
  
  # Warning if no data loaded
  output$ui_DIV_warn <- renderUI({
    if (is.null(dataset)) {
      # div(
      #   tags$br(),
      #   span(class = "warn", "No dataset loaded")
      # )
      div(
        h5("No dataset loaded")
      )
    }
  })
  
  # select variable
  observe({
      choices <- colnames(dataset[sapply(dataset, is.numeric)])
      updateSelectInput(session, "si_variable", choices = choices)
  })
  
  # Radio Button
  output$ui_RB_function <- renderUI({
    radioButtons(ns("RB_function"), label = "Function",
                 choices = c("log", "abs", "sqrt"), selected = "log")
  })
  
  # apply button
  output$ui_AB_apply <- renderUI({
    if (is.null(dataset)) {
      shinyjs::disabled(
        actionButton(ns("AB_apply"), label = "Apply function !")
      )
    } else {
      actionButton(ns("AB_apply"), label = "Apply function !")
    }
  })
  
  # ReactiveValue to return
  toReturn <- reactiveValues(result = NULL,   # new values of updated variable
                             variable = NULL, # name of variable being updated
                             trigger = NULL,
                             fun = NULL)
  
  # Apply function on variable
  observeEvent(input$AB_apply, {
      if (input$RB_function == "log") {
        toReturn$result <- log(dataset[, input$si_variable])
      } else if (input$RB_function == "abs") {
        toReturn$result <- abs(dataset[, input$si_variable])
      } else if (input$RB_function == "sqrt") {
        toReturn$result <- sqrt(dataset[, input$si_variable])
      }
    
      toReturn$trigger <- ifelse(is.null(toReturn$trigger), 0, toReturn$trigger) + 1
      toReturn$fun     <- input$RB_function
      toReturn$variable = input$si_variable 
  })
  
  return(toReturn)
}