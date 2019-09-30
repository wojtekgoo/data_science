LDAUI <- function(id) {
  ns <- NS(id)

  div(
    fluidRow(
      column(width = 12,
        sidebarLayout(
      
          # sidebarPanel
          sidebarPanel(
            
            # formula
            h4("Create formula"),
            fluidRow(
              selectInput(ns("SI_dependentVar"), "Dependent var (factor only)", choices = NULL, selected = NULL),
              selectInput(ns("SI_independentVar"), "Independent vars", choices = NULL, selected = NULL, multiple = TRUE),
              textInput(ns("TI_interactionTerms"), "Add custom term to your model (eg. x1*x2 or x1:x2)")
            ),
            
            tags$hr(style ="border-top: 1px solid #888888;"),
            
            # split into train and test sets
            h4("Allocate observations to the train set"),
            fluidRow(
              textInput(ns("TI_trainSet"), "train set %", width = '100px', value = "70")
            ),
            
            tags$hr(style ="border-top: 1px solid #888888;"),
            
            # resampling methods
            h4("Resampling method"),
            fluidRow(
              radioButtons(ns("RB_resamplingMethod"), label = "", 
                           choices = c("None" = "none", "LOOCV" = "loocv", "K-fold CV" = "kfold"), selected = "none")
            ),
            
            conditionalPanel(
              condition = paste0('input[\'', ns('RB_resamplingMethod'), "\'] == \'kfold\'"),
              textInput(ns("TI_kfoldNumber"), "# of k", width = '100px')
            ),
            
            fluidRow(
              actionButton(ns("BTN_fitModel"), "Fit model")
            )
          ),
         
          # mainPanel
          mainPanel(
            
            fluidRow(
              column(width = 6,
                h4(textOutput(ns("TO_summaryLabel"))),
                verbatimTextOutput(ns("TO_modelSummary")),
                
                verbatimTextOutput(ns("TO_metrics")),
                tableOutput(ns("TO_predictedValues"))
              ),
              column(width = 6,
                h4(textOutput(ns("TO_confusionMatrixLabel"))),
                verbatimTextOutput(ns("TO_confusionMatrix"))
              )
            ),
            
            fluidRow(
              column(width = 6,
                h4(textOutput(ns("TO_predAccuracyLabel"))),
                verbatimTextOutput(ns("TO_predAccuracy"))
              )
            )
            
          ) # mainPanel
        ) # sidebarLayout
      ) # column
    ) # fluidRow
  ) # div
}


LDA <- function(input, output, session, dataset, id) {
  ns <- session$ns
  
  # remove NA's
  dataset = na.omit(dataset)
  
  # enable selection from current dataset variables
  choices <- colnames(dataset)
  updateSelectInput(session, "SI_independentVar", choices = choices, selected = '')

  factorChoices <- colnames(dataset[sapply(dataset, is.factor)]) # create choice only from factor columns
  updateSelectInput(session, "SI_dependentVar", choices = factorChoices)
  
  
  # create model  
  myModel = eventReactive(input$BTN_fitModel, {
      
      index <- createDataPartition(dataset[[input$SI_dependentVar]], p = as.numeric(input$TI_trainSet)/100, list = FALSE)
      train_set = dataset[index, ]
      test_set = dataset[-index, ]
    
    # formula
    if(input$TI_interactionTerms == '') { # do not add interaction terms
        formula = as.formula(paste(input$SI_dependentVar,'~', paste(input$SI_independentVar, collapse = "+")))
    } else { # add interaction terms
        formula = as.formula(paste(input$SI_dependentVar,'~', paste(input$SI_independentVar, collapse = "+"), '+', I(input$TI_interactionTerms)))
    }
    
    # divide observations into train and test splits
    if(input$RB_resamplingMethod == "none") {
      train.control <- trainControl(method = "none")
      model = train(formula, data = train_set, method = "lda", trControl = train.control)
    
    # k-fold CV
    } else if (input$RB_resamplingMethod == "kfold") {
        train.control <- trainControl(method = "cv", number = as.numeric(input$TI_kfoldNumber), savePredictions = TRUE)
        model = train(formula, data = train_set, method = "lda", trControl = train.control)
        
    # LOOCV
    } else { 
        train.control <- trainControl(method = "LOOCV", savePredictions = TRUE)
        model = train(formula, data = train_set, method = "lda", trControl = train.control)
    }
      
    output$TO_summaryLabel = renderText({
      "Fitted model summary"
    })
    
    output$TO_predAccuracyLabel = renderText({
      "Prediction accuracy"
    })
    
    output$TO_confusionMatrixLabel = renderText({
      "Confusion matrix"
    })
    
    predictions = predict(model, newdata = test_set)

    # return results
    x = list(model = model, pred = predictions, test_set = test_set)
    x
      
  })
  

  output$TO_modelSummary = renderPrint({
    summary(myModel()$model)
  })
    
  output$TO_confusionMatrix = renderPrint({
    confusionMatrix(myModel()$pred, myModel()$test_set[input$SI_dependentVar][ , 1])
  })
  
  output$TO_predAccuracy = renderPrint({
    round(mean(myModel()$pred == myModel()$test_set[input$SI_dependentVar][ , 1])*100,2)
  })
    
  output$TO_metrics = renderPrint({
    myModel()$model
  })
     
  output$TO_predictedValues = renderTable({
    data.frame("Names" = rownames(myModel()$test_set), "Predictions" = myModel()$pred, "Actuals" = myModel()$test_set[input$SI_dependentVar][ , 1], stringsAsFactors = FALSE)
  })
}