library(shiny)
library(ggplot2)

load_dataUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(12,
      actionButton(ns("generatePlot"), "Generate plot")
    )
  )
}

load_data <- function(input, output, session) {
  ns <- session$ns
  
  toReturn    <-  reactiveValues(
    variables = NULL,
    variable_name = NULL,
    trigger = 0
  )
  
  # (Re)load button
  observeEvent(input$test, {
    toReturn$trigger        <- toReturn$trigger + 1
  })
  
  observeEvent(input$test, {
    print("Button clicked")
  })
  
  return(toReturn)
}