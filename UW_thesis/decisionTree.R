decisionTreeUI <- function(id) {
  ns <- NS(id)
  
  div(
    fluidRow(
      column(width = 12,
        sidebarLayout(
      
          # sidebarPanel
          sidebarPanel(width = 2,
            
            # formula
            h4("Create formula"),
            fluidRow(
              selectInput(ns("SI_dependentVar"), "Dependent", choices = NULL, selected = NULL),
              selectInput(ns("SI_independentVar"), "Explanatory", choices = NULL, selected = NULL, multiple = TRUE)
            ),
            
            tags$hr(style ="border-top: 1px solid #888888;"),
            
            # model control
            h4("Control options"),
            fluidRow(
              textInput(ns("TI_maxDepth"), "Max tree depth", width = '100px', placeholder = "8"),
              textInput(ns("TI_minsplit"), "Minimum split", width = '100px', placeholder = "10"),
              textInput(ns("TI_cp"), "cp", width = '100px', placeholder = "0.001")
            ),
            
            
            tags$hr(style ="border-top: 1px solid #888888;"),
            
            # split into train and test sets
            h4("Create train set"),
            fluidRow(
              textInput(ns("TI_trainSet"), "0-100%", value = "70", width = '100px')
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
              textInput(ns("TI_kfoldNumber"), "# of k", value = "4", width = '100px')
            ),
            
            tags$hr(style ="border-top: 1px solid #888888;"),
            
            # set random seed
            h4("Set random seed"),
            fluidRow(
              radioButtons(ns("RB_randomSeed"), label = "", 
                           choices = c("Random" = "random", "Not random" = "notrandom"), selected = "random")
            ),
            
            conditionalPanel(
              condition = paste0('input[\'', ns('RB_randomSeed'), "\'] == \'notrandom\'"),
              textInput(ns("TI_seed"), "", value = "1", width = '100px')
            ),
            
            # fit model
            tags$hr(style ="border-top: 1px solid #888888;"),
            actionButton(ns("BTN_fitModel"), "Fit model", class = "btn-success"),
            
            # export model to RDS file
            tags$hr(style ="border-top: 1px solid #888888;"),
            downloadButton(ns("BTN_exportModel"), "Export model")
          ),
         
          # mainPanel
          mainPanel(width = 10,
            
            fluidRow(
              column(width = 6,
                h4(textOutput(ns("TO_summaryLabel"))),
                verbatimTextOutput(ns("TO_modelSummary"))
              ),
              
              column(width = 6,
                h4(textOutput(ns("TO_cpLabel"))),
                verbatimTextOutput(ns("TO_cpTable")),
                plotOutput(ns("PO_cpPlot"))
              )
            ),
            
            fluidRow(
              # column(width = 6,
                #downloadButton(ns("BTN_savePlot"), ""),
                plotOutput(ns("PO_rpartPlot"))
            ),
            
            fluidRow(
              column(width = 6,
                htmlOutput(ns("TO_predictedValues"))
              )
            )
            
          ) # mainPanel
        ) # sidebarLayout
      ) # column
    ) # fluidRow
  ) # div
}


decisionTree <- function(input, output, session, dataset, id) {
  ns <- session$ns
  req(dataset)
  
  v <- reactiveValues()
  observe({
    v$depvar = input$SI_dependentVar
    v$expvar = input$SI_independentVar
  })
  
  # printcp function modified not to print model formula
  my_printcp = function (x, digits = getOption("digits") - 2L) 
  {
    if (!inherits(x, "rpart")) 
        stop("'x' must be an \"rpart\" object")
    cat(switch(x$method, anova = "Regression tree:\n", 
        class = "Classification tree:\n", poisson = "\nRates regression tree:\n", 
        exp = "\nSurvival regression tree:\n"))
    frame <- x$frame
    leaves <- frame$var == "<leaf>"
    used <- unique(frame$var[!leaves])
    if (!is.null(used)) {
        cat("Variables actually used in tree construction:\n")
        print(sort(as.character(used)), quote = FALSE)
        cat("\n")
    }
    cat("Root node error: ", format(frame$dev[1L], digits = digits), 
        "/", frame$n[1L], " = ", format(frame$dev[1L]/frame$n[1L], 
            digits = digits), "\n\n", sep = "")
    n <- x$frame$n
    omit <- x$na.action
    if (length(omit)) 
        cat("n=", n[1L], " (", naprint(omit), ")\n\n", 
            sep = "")
    else cat("n=", n[1L], "\n\n")
    print(x$cptable, digits = digits)
    invisible(x$cptable)
  }
  
  
  # remove NA's
  dataset = na.omit(dataset)
  
  # remove LOOCV option if dataset too big
  if(nrow(dataset) > 100)
    updateRadioButtons(session, "RB_resamplingMethod", label = "", choices = c("None" = "none", "K-fold CV" = "kfold"), selected = "none")
  
  # enable selection from current dataset variables
  choices <- colnames(dataset)
  updateSelectInput(session, "SI_independentVar", choices = choices, selected = '')
  updateSelectInput(session, "SI_dependentVar", choices = choices, selected = '')
  
  # create model  
  m = eventReactive(input$BTN_fitModel, {
    
    req(v$depvar)
    req(v$expvar)
    req(as.numeric(input$TI_trainSet)/100 > 0 && as.numeric(input$TI_trainSet)/100 <= 1)

    if(input$RB_randomSeed == 'notrandom') set.seed(input$TI_seed)
    
    # divide observations into train and test splits
    index <- createDataPartition(dataset[[v$depvar]], p = as.numeric(input$TI_trainSet)/100, list = FALSE)
    train_set = dataset[index, ]
    test_set = dataset[-index, ]
    
    # formula
    formula = as.formula(paste(v$depvar,'~', paste(v$expvar, collapse = "+")))
    
    # choose resampling method
    if(input$RB_resamplingMethod == "none")
      resamples <- 0
    else if (input$RB_resamplingMethod == "kfold")
      resamples = as.numeric(input$TI_kfoldNumber)
    else
      resamples = nrow(dataset)
    
    # Train and predict  
    # if y is factor or character, use classification model
    if ( is.factor(dataset[[v$depvar]]) || is.character(dataset[[v$depvar]]) ) {
      type = "class"
      model = rpart(formula, 
                    data = train_set, 
                    method = "class", 
                    model = TRUE,
                    control = rpart.control(
                                            minsplit = ifelse(input$TI_minsplit == "", 10, as.numeric(input$TI_minsplit)),
                                            maxdepth = ifelse(input$TI_maxDepth == "", 5, as.numeric(input$TI_maxDepth)),
                                            cp = ifelse(input$TI_cp == "", 0.001, as.numeric(input$TI_cp)),
                                            xval = resamples
                                            ))
      
      predictions = predict(model, newdata = test_set, type = "class")
    }
    # else assume y is numeric and do regression
    else {
      type = "anova"
      model = rpart(formula, data = train_set, method = "anova", model = TRUE, 
                    control = rpart.control(
                                            minsplit = ifelse(input$TI_minsplit == "", 10, as.numeric(input$TI_minsplit)),
                                            maxdepth = ifelse(input$TI_maxDepth == "", 5, as.numeric(input$TI_maxDepth)),
                                            cp = ifelse(input$TI_cp == "", 0.001, as.numeric(input$TI_cp)),
                                            xval = resamples
                                            ))
      predictions = predict(model, newdata = test_set)
    }
    
    output$TO_summaryLabel = renderText({
      "Fitted model summary"
    })
    
    output$TO_cpLabel = renderText({
      "Complexity parameter"
    })
   

    # return results
    x = list(model = model, pred = predictions, test_set = test_set, type = type)
    x
      
  })
  

  output$TO_modelSummary = renderPrint({
    summary.rpart.project( m()$model, depvar = isolate(v$depvar), expvar = isolate(v$expvar) )
  })
    
  output$TO_cpTable = renderPrint({
    my_printcp( m()$model )
  })
  
  output$PO_cpPlot = renderPlot({
    # plot cp only when cross-validation has been selected
    if( input$RB_resamplingMethod == "kfold" && m()$model$control$xval != 0 ) {
        plotcp(m()$model)
    }
  })
    
  output$TO_predictedValues = function() {
    df = data.frame("Names" = rownames(m()$test_set), 
                    "Predictions" = m()$pred, 
                    "Actuals" = isolate( m()$test_set[[v$depvar]] ), 
                    stringsAsFactors = FALSE)
    
    if(m()$type == "class") {
      df %>% mutate(
          Predictions = cell_spec(Predictions, "html", color = ifelse(Predictions != Actuals, "red", "black"))
        ) %>%
          knitr::kable(format = "html", align = 'cc', escape = FALSE) %>%
          kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
      
    } else {
      
    df = df %>%
      dplyr::mutate("+/-" = round(Actuals - Predictions, 2))
    
    df %>%
      knitr::kable("html", align='ccc', escape = F) %>%
      kable_styling(
        bootstrap_options = c("hover", "responsive")
      )
    }
  }
  
  output$PO_rpartPlot = renderPlot({
    rpart.plot(m()$model)
  })
  
  # export model to RDS file
  output$BTN_savePlot = downloadHandler(
    filename = function() { paste0("dectree_plot_",  Sys.Date(), ".png") },
    content = function(file) {
      png(file)
      rpart.plot(m()$model)
      dev.off()
    }
  )
  
  # export model to RDS file
  output$BTN_exportModel = downloadHandler(
    filename = function() { paste0("dectree", "_", Sys.Date(), ".rds") },
    content = function(con) {
      saveRDS(m()$model, file = con)
    }
  )
}