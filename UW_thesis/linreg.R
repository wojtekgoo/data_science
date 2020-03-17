linregUI <- function(id) {
  ns <- NS(id)

  div(
        sidebarLayout(
      
          # sidebarPanel
          sidebarPanel(width = 2,
            
            # formula
            h4("Create formula"),
            selectInput(ns("SI_dependentVar"), "Dependent", choices = NULL, selected = NULL),
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
              column(width = 8,
                  fluidRow(
                    column(width = 11,
                      h4(textOutput(ns("TO_summaryLabel"))),
                      verbatimTextOutput(ns("TO_modelSummary")),
                    )
                  ),
                  fluidRow(
                    column(width = 6,
                     h4(textOutput(ns("TO_metricsLabel"))),
                      verbatimTextOutput(ns("TO_metrics")),
                      htmlOutput(ns("TO_predictedValues"))
                    )
                  )
              ),
              # right-side
              column(width = 4,
                  fluidRow(
                    column(width = 8,
                      h4(textOutput(ns("TO_varImpLabel"))),
                      htmlOutput(ns("TO_varImp"))
                    )
                  ),
                  fluidRow(
                      h4(textOutput(ns("TO_actVsFitLabel"))),
                      plotOutput(ns("PO_actVsFit")),
                      h4(textOutput(ns("TO_fitVsResLabel"))),
                      plotOutput(ns("PO_fitVsRes"))
                  )
                )
            )
          ) # mainPanel
        ) # sidebarLayout
  ) # div
}


linreg <- function(input, output, session, dataset, id) {
  ns <- session$ns
  req(dataset)
  
  v <- reactiveValues()
  observe({
    v$depvar = input$SI_dependentVar
    v$expvar = input$SI_independentVar
    v$inter = input$SI_interactions
  })
  
  # remove NA's
  dataset = na.omit(dataset)
  
  # remove LOOCV option if dataset too big
  if(nrow(dataset) > 100)
    updateRadioButtons(session, "RB_resamplingMethod", label = "", choices = c("None" = "none", "K-fold CV" = "kfold"), selected = "none")
  
  # enable selection from current dataset variables
  choices = colnames(dataset)
  varnames = colnames(dataset)
  updateSelectInput(session, "SI_dependentVar", choices = choices, selected = '')
  updateSelectInput(session, "SI_independentVar", choices = choices, selected = '')
  
  # output$SI_independentVar <- renderUI({
  #   #req(available(input$reg_rvar))
  #   vars <- colnames(dataset)
  #   ## don't use setdiff, removes names
  #   #if (length(vars) > 0 && input$SI_dependentVar %in% vars) {
  #   #  vars <- vars[-which(vars == input$SI_dependentVar)]
  #   #}
  #   
  #   selectInput(
  #     inputId = "reg_evar", label = "Explanatory variables:", choices = vars,
  #     selected = NULL,
  #     multiple = TRUE, size = min(10, length(vars)), selectize = FALSE
  #   )
  # })
  
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
    
    if( !is_empty(input$RB_interactions) && input$RB_interactions != "None") {
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
  
  
  # create lm model  
  m = eventReactive(input$BTN_fitModel, {
    
    req(v$depvar)
    req(v$expvar)
    req(as.numeric(input$TI_trainSet)/100 > 0 && as.numeric(input$TI_trainSet)/100 < 1)
    
    if(input$RB_randomSeed == 'notrandom') set.seed(input$TI_seed)
    
    # divide observations into train and test splits
    index <- createDataPartition(dataset[[v$depvar]], p = as.numeric(input$TI_trainSet)/100, list = FALSE)
    
    # create some vars to be returned
    train_set = dataset[index, ]
    test_set = dataset[-index, ]
    train_act = train_set[[v$depvar]]
    test_act = test_set[[v$depvar]]
    
    # formula
    if(is_empty(v$inter)) { # do not add interaction terms
        formula = as.formula(paste(v$depvar,'~', paste(v$expvar, collapse = "+")))
    } else { # add interaction terms
        formula = as.formula(paste(v$depvar,'~', paste(v$expvar, collapse = "+"), '+', paste(v$inter, collapse = "+")))
    }

    # choose resampling method
    if (input$RB_resamplingMethod == "none")
      train.control <- trainControl(method = "none")
    else if (input$RB_resamplingMethod == "kfold")
      train.control <- trainControl(method = "cv", number = as.numeric(input$TI_kfoldNumber), savePredictions = TRUE)
    else 
      train.control <- trainControl(method = "LOOCV", savePredictions = TRUE)
      
    # Train model
    model = train(formula, 
                  data = train_set, 
                  method = "lm", 
                  trControl = train.control)
    
    # print labels  
    output$TO_summaryLabel = renderText({
      "Fitted model summary"
    })
    
    output$TO_metricsLabel = renderText({
      "Prediction metrics"
    })
    
    output$TO_varImpLabel = renderText({
      "Variable importance"
    })
    
    output$TO_actVsFitLabel = renderText({
      "Actuals vs Fitted values"
    })
    
    output$TO_fitVsResLabel = renderText({
      "Residuals vs Fitted values"
    })
    
    output$TO_varImpLabel = renderText({
      "Variable importance"
    })
    
    # save model
    output$UI_saveModel = renderUI({
      actionButton(ns("BTN_saveModel"), "Save model to file")
    })
    
    # make prediction
    predictions = predict(model, newdata = test_set)
    
    # return results
    x = list(model = model, 
             pred = predictions, 
             train_set = train_set, 
             test_set = test_set, 
             train_act = train_act,
             test_act = test_act,
             depvar = v$depvar, 
             expvar = v$expvar)
    x
      
  })
  
  # varImp function showing |t value| and scaling to 0-100
  myVarImp = function(s) {
    values <- summary(s)$coef
    varImps <-  abs(values[ !grepl( rownames(values), pattern = 'Intercept' ),
                            grep("value$", colnames(values)), drop = FALSE])
    out <- data.frame(varImps)
    
    if(!is.null(names(varImps))) rownames(out) <- names(varImps)
    
    #scale to 0-100 and sort
    out$scaled = out[, 1] - min(out[, 1], na.rm = T)
    out$scaled = out$scaled / max(out$scaled, na.rm=T) * 100
    out = out[order(-out$scaled), , drop = FALSE]
    out = round( out[order(-out$scaled), , drop = FALSE], 2) # drop=F to preserve row names
    colnames(out) = c("|t|", "scaled")
    out
  }
  
  output$TO_varImp = renderText({
    myVarImp(m()$model) %>%
      knitr::kable("html", align='cc', escape = F) %>%
      kable_styling(
        bootstrap_options = c("hover", "responsive")
      )
  })

  output$TO_modelSummary = renderPrint({
    summary.lm.project(m()$model, depvar = isolate( v$depvar ), expvar = isolate( v$expvar ), inter = isolate( v$inter ))
  })
    
  output$TO_metrics = renderPrint({
    postResample(m()$pred, m()$test_act)
  })
     
  output$TO_predictedValues = renderText({
    df = data.frame("Names" = names(m()$pred), 
               "Predictions" = round( m()$pred, 2), 
               "Actuals" = round( m()$test_act, 2),
               stringsAsFactors = FALSE)
    
    df = df %>%
      dplyr::mutate("+/-" = round(Actuals - Predictions, 2))
    
    df %>%
      knitr::kable("html", align='ccc', escape = F) %>%
      kable_styling(
        bootstrap_options = c("hover", "responsive")
      )
  })
  
  output$PO_actVsFit = renderPlot({
    act = m()$train_act
    fit = fitted.values(m()$model)
    p = ggplot(m()$train_set, aes(x = act, y = fit)) + geom_point()
    p = p + geom_smooth(method = "loess", alpha = 0.2, size = .75, linetype = "dotdash")
    p = p + labs(x = "Actuals", y = "Fitted")
    p
  })
  
  output$PO_fitVsRes = renderPlot({
    res = resid(m()$model)
    fit = fitted.values(m()$model)
    p = ggplot(m()$train_set, aes(x = fit, y = res)) + geom_point()
    p = p + geom_smooth(method = "loess", alpha = 0.2, size = .75, linetype = "dotdash")
    p = p + labs(x = "Fitted", y = "Residuals")
    p
  })
  
  # export model to RDS file
  output$BTN_exportModel = downloadHandler(
    filename = function() { paste0("linreg", "_", Sys.Date(), ".rds") },
    content = function(con) {
      saveRDS(m()$model, file = con)
    }
  )
}