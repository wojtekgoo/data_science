decisionTreeUI <- function(id) {
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
              selectInput(ns("SI_dependentVar"), "Dependent var", choices = NULL, selected = NULL),
              selectInput(ns("SI_independentVar"), "Independent variables", choices = NULL, selected = NULL, multiple = TRUE)
            ),
            
            tags$hr(style ="border-top: 1px solid #888888;"),
            
            # model control
            h4("Control options"),
            fluidRow(
              textInput(ns("TI_maxDepth"), "Max tree depth", width = '100px', placeholder = "10"),
              textInput(ns("TI_cp"), "cp", width = '100px', placeholder = "0.001")
            ),
            
            
            tags$hr(style ="border-top: 1px solid #888888;"),
            
            # split into train and test sets
            h4("Allocate observations to the train set"),
            fluidRow(
              textInput(ns("TI_trainSet"), "train set %", width = '100px')
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
              column(width = 7,
                h4(textOutput(ns("TO_summaryLabel"))),
                verbatimTextOutput(ns("TO_modelSummary"))
              ),
              
              column(width = 5,
                h4(textOutput(ns("TO_cpLabel"))),
                verbatimTextOutput(ns("TO_cpTable")),
                plotOutput(ns("PO_cpPlot"))
              )
            ),
            
            fluidRow(
              # column(width = 6,
                plotOutput(ns("PO_rpartPlot"))
            ),
            
            fluidRow(
              column(width = 6,
                tableOutput(ns("TO_predictedValues"))
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
  
  # enable selection from current dataset variables
  choices <- colnames(dataset)
  updateSelectInput(session, "SI_independentVar", choices = choices, selected = '')
  updateSelectInput(session, "SI_dependentVar", choices = choices)
  
  # create model  
  myModel = eventReactive(input$BTN_fitModel, {
    # divide observations into train and test splits
    index <- createDataPartition(dataset[[input$SI_dependentVar]], p = as.numeric(input$TI_trainSet)/100, list = FALSE)
    train_set = dataset[index, ]
    test_set = dataset[-index, ]
    
    # formula
    formula = as.formula(paste(input$SI_dependentVar,'~', paste(input$SI_independentVar, collapse = "+")))
    
    # choose resampling method
    if(input$RB_resamplingMethod == "none")
      xval <- 0
    else if (input$RB_resamplingMethod == "kfold")
      xval = as.numeric(input$TI_kfoldNumber)
    else
      xval = nrow(dataset)
    
    # Train and predict  
    # if y is factor or character, use classification model
    if ( is.factor(dataset[[input$SI_dependentVar]]) || is.character(dataset[[input$SI_dependentVar]]) ) {
      model = rpart(formula, data = train_set, method = "class", model = TRUE, 
                    control = rpart.control(maxdepth = ifelse(input$TI_maxDepth == "", 10, as.numeric(input$TI_maxDepth)),
                                            cp = ifelse(input$TI_cp == "", 0.001, as.numeric(input$TI_cp)),
                                            xval = xval
                                            ))
      predictions = predict(model, newdata = test_set, type = "class")
    }
    # else assume y is numeric and do regression
    else {
      model = rpart(formula, data = train_set, method = "anova", model = TRUE, 
                    control = rpart.control(maxdepth = ifelse(input$TI_maxDepth == "", 30, as.numeric(input$TI_maxDepth)),
                                            cp = ifelse(input$TI_cp == "", 0.001, as.numeric(input$TI_cp)),
                                            xval = xval
                                            ))
      predictions = predict.train(model, newdata = test_set)
    }
    
    output$TO_summaryLabel = renderText({
      "Fitted model summary"
    })
    
    output$TO_cpLabel = renderText({
      "Complexity parameter"
    })
   

    # return results
    x = list(model = model, pred = predictions, test_set = test_set)
    x
      
  })
  

  output$TO_modelSummary = renderPrint({
    summary( myModel()$model )
  })
    
  output$TO_cpTable = renderPrint({
    my_printcp(myModel()$model)
  })
  
  output$PO_cpPlot = renderPlot({
    # plot cp only when cross-validation has been selected
    try( plotcp(myModel()$model) )
  })
    
  output$TO_predictedValues = renderTable({
    data.frame("Names" = rownames(myModel()$test_set), "Predictions" = myModel()$pred, "Actuals" = myModel()$test_set[input$SI_dependentVar][ , 1], stringsAsFactors = FALSE)
  })
  
  output$PO_rpartPlot = renderPlot({
   rpart.plot(myModel()$model, fallen.leaves = TRUE)
  })

}