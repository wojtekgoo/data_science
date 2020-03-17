LDAUI <- function(id) {
  ns <- NS(id)

  div(
        sidebarLayout(
      
          # sidebarPanel
          sidebarPanel(width = 2,
            
            # formula
            h4("Create formula"),
            selectInput(ns("SI_dependentVar"), "Dependent (factor only)", choices = NULL, selected = NULL),
            uiOutput(ns("UI_SI_reference")),
            selectInput(ns("SI_independentVar"), "Explanatory", choices = NULL, selected = NULL, multiple = TRUE),
            
            h4("Interactions"),
            
            conditionalPanel(
              # show interactions if Explanatory vars not empty
              condition = paste0('input[\'', ns('SI_independentVar'), "\'] != \'\'"),
              uiOutput(ns("UI_interactionChoice")),
              uiOutput(ns("UI_interactionTerms"))
            ),
            
            tags$hr(style ="border-top: 1px solid #888888;"),
            
            # split into train and test sets
            h4("Create train set"),
            textInput(ns("TI_trainSet"), "0-100%", value = "70", width = '100px'),
            
            tags$hr(style ="border-top: 1px solid #888888;"),
            
            # resampling methods
            h4("Resampling method"),
            radioButtons(ns("RB_resamplingMethod"), label = "", 
                           choices = c("None" = "none", "LOOCV" = "loocv", "K-fold CV" = "kfold"), selected = "none"),
            
            conditionalPanel(
              condition = paste0('input[\'', ns('RB_resamplingMethod'), "\'] == \'kfold\'"),
              textInput(ns("TI_kfoldNumber"), "# of k", value = "4", width = '100px')
            ),
            
            tags$hr(style ="border-top: 1px solid #888888;"),
            
            # set random seed
            h4("Set random seed"),
              radioButtons(ns("RB_randomSeed"), label = "", 
                           choices = c("Random" = "random", "Not random" = "notrandom"), selected = "random"),
            
            conditionalPanel(
              condition = paste0('input[\'', ns('RB_randomSeed'), "\'] == \'notrandom\'"),
              textInput(ns("TI_seed"), "", value = "1", width = '100px')
            ),
            
            
            # fit model
            tags$hr(style ="border-top: 1px solid #888888;"),
            actionButton(ns("BTN_fitModel"), "Fit model", icon = icon("play"), class = "btn-success"),
            
            # export model to RDS file
            tags$hr(style ="border-top: 1px solid #888888;"),
            downloadButton(ns("BTN_exportModel"), "Export model")
          ),
         
          # mainPanel
          mainPanel(width = 10,
            
            fluidRow(
              # left-side
              column(width = 7,
                  fluidRow(
                    column(width = 12,
                      h4(textOutput(ns("TO_summaryLabel"))),
                      verbatimTextOutput(ns("TO_metrics"))
                    )
                  ),
                  fluidRow(
                    column(width = 6,
                      h4(textOutput(ns("TO_predLabel"))),
                      htmlOutput(ns("TO_predictedValues")),
                      h4(textOutput(ns("TO_predAccuracyLabel"))),
                      verbatimTextOutput(ns("TO_predAccuracy"))
                    )
                  )
              ),
              # right-side
              column(width = 5,
                  fluidRow(
                    column(width = 12,
                      h4(textOutput(ns("TO_confMatrixLabel"))),
                      verbatimTextOutput(ns("TO_confMatrix"))
                    )
                  )

              )
            )
          ) # mainPanel
    ) # sidebarLayout
  ) # div
}


LDA <- function(input, output, session, dataset, id) {
  ns <- session$ns
  req(dataset)
  
  # remove NA's
  dataset = na.omit(dataset)
  
  v <- reactiveValues()
  observe({
    v$depvar = input$SI_dependentVar
    v$expvar = input$SI_independentVar
    v$inter = input$SI_interactions
  })
  
  # remove LOOCV option if dataset too big
  if(nrow(dataset) > 100)
    updateRadioButtons(session, "RB_resamplingMethod", label = "", choices = c("None" = "none", "K-fold CV" = "kfold"), selected = "none")
  
  # enable selection from current dataset variables
  choices <- colnames(dataset)
  updateSelectInput(session, "SI_independentVar", choices = choices, selected = '')

  factorChoices <- colnames(dataset[sapply(dataset, is.factor)]) # create dataset only with factor columns
  updateSelectInput(session, "SI_dependentVar", choices = factorChoices)
  
  
  # Show radio button with interactions choice
  output$UI_interactionChoice <- renderUI({
    radioButtons(
      inputId = ns("RB_interactions"), label = NULL,
      choices = c("None" = "None", "2nd" = "2", "3rd" = "3"), selected = "None",
      inline = TRUE
    )
  })
  
  
  # Show panel with interaction terms
  output$UI_interactionTerms = renderUI({
    choices = NULL
    vars = v$expvar
    
    if (is_empty(input$RB_interactions) || input$RB_interactions == "None") {
      return()
    }  
    
    if(input$RB_interactions != "None") {
        numeric = colnames( dplyr::select_if(dataset[vars], is.numeric) )
          if (length(numeric) > 0) {
            choices <- qterms(numeric, input$RB_interactions)

          ## list of interaction terms to show
            if (length(vars) > 1) {
              choices <- c(choices, iterms(vars, input$RB_interactions))
            }
          }
        
        if (length(choices) == 0) return()
    }
    
    
    selectInput(
      ns("SI_interactions"), label = NULL,
      choices = choices,
      selected = NULL,
      multiple = TRUE,
      size = min(8, length(choices)),
      selectize = FALSE
    )
  })
  
  # let user choose reference level if dependent var is binomial
    output$UI_SI_reference <- renderUI({
      req(v$depvar)
      if( nlevels(dataset[[v$depvar]]) == 2 )
          selectInput(ns("SI_reference"), "Select 'positive' reference", choices = levels(dataset[[v$depvar]]), selected = NULL)
      })
  
  # create model  
  m = eventReactive(input$BTN_fitModel, {
    
    req(v$depvar)
    req(v$expvar)
    req(as.numeric(input$TI_trainSet)/100 > 0 && as.numeric(input$TI_trainSet)/100 < 1)
    
    # remove NA's
    dataset = na.omit(dataset)
    
    if(input$RB_randomSeed == 'notrandom')
      set.seed(input$TI_seed)
    
    # change reference level (ref needs to be the other value than the 'positive' level selected by user)
    if(nlevels(dataset[[v$depvar]]) == 2) {
        dataset[[v$depvar]] = relevel(dataset[[v$depvar]], ref = dplyr::setdiff(dataset[[v$depvar]], input$SI_reference))
    }
    
    # divide observations into train and test splits  
    index <- createDataPartition(dataset[[v$depvar]], p = as.numeric(input$TI_trainSet)/100, list = FALSE)
    train_set = dataset[index, ]
    test_set = dataset[-index, ]
    
    # formula
    if(is_empty(v$inter)) { # do not add interaction terms
        formula = as.formula(paste(v$depvar,'~', paste(v$expvar, collapse = "+")))
    } else { # add interaction terms
        formula = as.formula(paste(v$depvar,'~', paste(v$expvar, collapse = "+"), '+', paste(v$inter, collapse = "+")))
    }
    
    # choose resampling method
    if(input$RB_resamplingMethod == "none") 
      train.control <- trainControl(method = "none")
    else if (input$RB_resamplingMethod == "kfold")
      train.control <- trainControl(method = "cv", number = as.numeric(input$TI_kfoldNumber), savePredictions = TRUE)
    else 
      train.control <- trainControl(method = "LOOCV", savePredictions = TRUE)
    
    # train model
    model = train(formula, 
                  data = train_set, 
                  method = "lda", 
                  trControl = train.control)

      
    output$TO_summaryLabel = renderText({
      "Fitted model summary"
    })
    
    output$TO_predLabel = renderText({
      "Predictions"
    })
    
    # output$TO_predAccuracyLabel = renderText({
    #   "Accuracy"
    # })
    
    output$TO_confMatrixLabel = renderText({
      "Confusion matrix"
    })
    
    output$TO_confMatrix = renderPrint({
        confusionMatrix(isolate( m()$test_set[[v$depvar]] ), m()$pred, mode = "prec_recall", positive = isolate( input$SI_reference ))
    })
    
    output$UI_saveModel = renderUI({
        actionButton(ns('BTN_saveModel'), 'Save model to file')
    })
    
    
    predictions = predict(model, newdata = test_set)

    # return results
    x = list(model = model, pred = predictions, test_set = test_set)
    x
      
  })
  
  # output$TO_predAccuracy = renderPrint({
  #   round(mean(m()$pred == m()$test_set[v$depvar][ , 1])*100,2)
  # })
    
  output$TO_metrics = renderPrint({
    print.train.project( m()$model, depvar = isolate( v$depvar ), expvar = isolate( v$expvar ), inter = isolate(v$inter) )
  })
     
  output$TO_predictedValues = function() {
    df = data.frame("Names" = rownames(m()$test_set), 
                    "Predictions" = m()$pred, 
                    "Actuals" = isolate( m()$test_set[[v$depvar]] ), 
                    stringsAsFactors = FALSE)
    
    df %>% mutate(
      Predictions = cell_spec(Predictions, "html", color = ifelse(Predictions != Actuals, "red", "black"))
    ) %>%
      knitr::kable(format = "html", align = 'cc', escape = FALSE) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
  }
  
  # export model to RDS file
  output$BTN_exportModel = downloadHandler(
    filename = function() { paste0("LDA", "_", Sys.Date(), ".rds") },
    content = function(con) {
      saveRDS(m()$model, file = con)
    }
  )
  
}