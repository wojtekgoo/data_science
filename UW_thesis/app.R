# Author: Wojciech Gusztyła

source("stats_module.R")
source("linreg.R")  # linear regression model
source("binomialGLM.R") # logistic regression model
source("LDA.R") # Linear Discriminant Analysis
source("decisionTree.R") # Decision tree
source("external.R")

pkg("shiny")
pkg("car")  # for qqPlot and VIF
pkg("ISLR")
pkg("ggplot2")
pkg("shinyjs")
pkg("DT")
pkg("naniar")
pkg("shinyWidgets")
pkg("shinythemes")
pkg("plotly")
pkg("colourpicker")
pkg("ggthemes")
pkg("datasets")
pkg("knitr")
pkg("kableExtra") # for kable
pkg("formattable")
pkg("Hmisc")
pkg("caret")
pkg("caTools") # for ROC curve
pkg("boot")
pkg("GGally") # ggpairs
pkg("shinycssloaders") # for progress indicator
pkg("rpart") # for decision trees
pkg("rpart.plot") # to print decision trees
pkg("e1071")
pkg("tidyverse")
pkg("ROCR") # ROC curve
pkg("glue")
pkg("summarytools")


remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt = quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H = 1.5 * IQR(x, na.rm = na.rm)
  y = x
  y[x < (qnt[1] - H)] = NA
  y[x > (qnt[2] + H)] = NA
  y
}


find_outliers = function(x, na.rm = TRUE) {
  x <- x[!is.na(x)] # remove NA's
  if(is.factor(x)) return("n/a") # factors not applicable
  if(is.character(x)) return("n/a") # characters not applicable
  
  qnt <- quantile(x, probs=c(.25, .75), na.rm = TRUE)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y = x
  low = y[x < (qnt[1] - H)]
  high = y[x > (qnt[2] + H)]
  result = c(low, high)
  if(length(result) > 0) {
    return(paste(result, collapse = " "))   # combine all results into one-element char vector (to avoid vectorization when printing)     
  } else return("no outl.")
}



######## UI ########
# Fluid page needed to have one title across all tabs
ui <- fluidPage(theme = "spacelab.min.css", title = "Dissertation",

    useShinyjs(),
    # TITLE
    fluidRow(
      h3(HTML("<center>Analysis of sample datasets and implementation of ML algorithms in Shiny</center>")),
      h4(HTML("<center>A thesis on 'Data science in business' course at the University of Warsaw</center>")),
      h5(HTML("<center>Author: Wojciech Gusztyla</center>"))
    ),
    
    hr(),
    
    # NAVBAR LAYOUT
    navbarPage(NULL, collapsible = TRUE, 
               
######## DATASET UI ########   
        tabPanel("Dataset",
            sidebarLayout(
                tags$div(id = 'my_label', #style = ".sidebar { height: 90vh; overflow-y: auto; }",
                sidebarPanel(width = 2,
                             
                    # select dataset
                    HTML("<div align='center'><b>Please select the dataset</b></div>"),
                    br(),
                    fluidRow(
                        column(12,
                            # to select all packages from R 'datasets': choices = ls("package:datasets")
                            selectInput("BTN_datasets", label = NULL, choices = c("airquality", "iris", "mtcars", "rock", "quakes", ls("package:ISLR")), 
                            selected = "mtcars")
                        )
                    ),
                    fluidRow(
                        column(12,
                            actionButton("BTN_load", "Load dataset", width = "100%", icon = icon("play"), class = "btn-success")
                        )
                    ),
                    
                    # upload CSV file
                    tags$hr(style ="border-top: 1px solid #888888;"),
                    HTML("<div align='center'><b>Or upload your own CSV file</b></div>"),
                    br(),
                    
                    # select file for upload
                    fileInput("fileName", NULL,
                          multiple = FALSE,
                          accept = c(
                              "text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv")
                          ),
                    
                    checkboxInput("CB_header", "Header", TRUE),
                    checkboxInput("CB_stringsAsFactors", "Strings as factors", FALSE),
                    radioButtons("sep", "Separator",
                                 choices = c(Comma = ",",
                                             Semicolon = ";",
                                             Tab = "\t"),
                                 selected = ","),
                    
                    radioButtons("quote", "Quote",
                      choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"), selected = ""),
                    
                    actionButton("displayFile", "Load file"),
                    
                    # change to NA
                    tags$hr(style ="border-top: 1px solid #888888;"),
                    tags$style(HTML(".my_centered{text-align: center;}")),
                    HTML("<div align='center'><b>Change value to NA</b></div>"),
                    fluidRow(
                      column(width = 12, align="center",
                        uiOutput("UI_selectColumn")
                      )
                    ),
                    fluidRow(
                        column(12,
                            textInput("TI_valueToChangeToNA", "", width = "50%"),
                            actionButton("BTN_changeToNA", "Change")
                        )
                    
                    ),
                    
                    # convert to other type
                    tags$hr(style ="border-top: 1px solid #888888;"),
                    HTML("<div align='center'><b>Convert variable type</b></div>"),
                    br(),
                    fluidRow(
                        div(style="display: block;vertical-align:top; width: 150px;",
                            selectInput("SI_varToConvert", label = "Select variable:", choices = NULL, multiple = TRUE, selected = NULL),
                            selectInput("SI_convertTo", label = "Choose type:", choices = NULL, selected = NULL)
                        )
                    ),
                    
                    # recode factor levels
                    tags$hr(style ="border-top: 1px solid #888888;"),
                    HTML("<div align='center'><b>Recode factor levels</b></div>"),
                    br(),
                    fluidRow(
                        div(style="display: block;vertical-align:top; width: 150px;",
                            selectInput("SI_factorToRecode", label = "Factor variable:", choices = NULL),
                            textInput("TI_recodeFrom", label = "From:"),   
                            textInput("TI_recodeTo", label = "To:")   
                        )
                    ),
                    
                    # delete selected rows button
                    tags$hr(style ="border-top: 1px solid #888888;"),
                    HTML("<div align='center'><b>Delete selected rows</b></div>"),
                    fluidRow(
                      column(width = 12, align="center",
                        uiOutput("UI_selectRow")
                      )
                    ),
                    br(),
                    fluidRow(
                      column(width = 2,
                        actionButton("BTN_deleteRows", "Delete", class = "btn-danger")
                      )
                    ),
                      
                    # remove outliers
                    tags$hr(style ="border-top: 1px solid #888888;"),
                    HTML("<div align='center'><b>Remove outliers</b></div>"),
                    br(),
                    fluidRow(
                        column(12,
                            selectInput("SI_removeOutliers", label = "Choose column:", choices = NULL)
                        )
                    ),
                    
                    
                    # save dataset
                    tags$hr(style ="border-top: 1px solid #888888;"),
                    HTML("<div align='center'><b>Save dataset</b></div>"),
                    br(),
                    fluidRow(
                        column(12,
                            downloadButton("BTN_saveDataset", label = "Save")
                        )
                    )
                    
                ) # sidebarPanel
                ), # div
              
                mainPanel(
                    # row with printed dataset
                    fluidRow(
                        DT::dataTableOutput("dataset")
                    ),
                    
                    # row with outliers and NAs
                    fluidRow(
                      column(width = 4, #style = 'border: 1px solid #888888;',
                        tableOutput("HO_outliersAndMissing")
                      )
                    )
                ) # mainPanel
            )
        ), # DATASET tab
        
######## STATS UI ########
        tabPanel("Stats",
          fluidRow(
            actionButton("BTN_generateStats", "Generate stats"),
            br(),
            br()
          ),
             
          # tabset with all generated plots
          fluidRow(
            column(width = 12,
              tabsetPanel(
                id = "stats_all_tabs"
              )
            )
          )
        ),
        
######## ML UI ########
        tabPanel("ML",
            h5("Select model"),
            fluidRow(
                column(width = 2,
                  selectInput("SI_selectModel", NULL, 
                    choices = c("Linear regression", "Binomial GLM", "Linear Discriminant Analysis", "Decision tree")
                                , selected = "Linear regression" )
                  ),
                
                column(width = 2,
                  actionButton("BTN_generateModel", "Generate model")
                )
            ),
            
            br(),
             
            # tabset with all generated plots
            fluidRow(
                column(
                    width = 12,
                    tabsetPanel(
                        id = "ml_all_tabs"
                    )
                )
            )
        )
    ) # navbarPage
) # fluidPage






######## SERVER ########
server = function(input, output, session) {
    df = reactiveValues(x = NULL) # store loaded dataset to be available across modules and its name
    obs_close <- list()

######## Load dataset ########
    observeEvent(input$BTN_load, {
        print(sprintf("%s loaded", input$BTN_datasets))
        df$x = get(input$BTN_datasets)
        
        # # diplay full file 
        print("df printed")
        output$dataset <- DT::renderDataTable({
            req(df$x)
            DT::datatable(df$x, selection = list(target = 'row+column'))
        })
        
        # enable selection from current dataset variables
        choices <- colnames(df$x)
        updateSelectInput(session, "SI_varToConvert", choices = choices, selected = '')
        
        numericCols = colnames(select_if(df$x, is.numeric))   # select only numeric columns
        updateSelectInput(session, "SI_removeOutliers", choices = numericCols, selected = '')
        
        updateSelectInput(session, "SI_convertTo", choices = c("As factor", "As numeric", "As character"), selected = '')
        
        # fill SI_factorToRecode with existing factor columns
        factorChoices <- colnames(df$x[sapply(df$x, is.factor)])
        updateSelectInput(session, "SI_factorToRecode", choices = factorChoices)
        
        # show missing values label
        output$TO_missingValues = renderText({
          "Missing values"
        })
        
        # print 'Outliers' table label
        output$TO_outliersLabel = renderText({
          "Outliers"
        })
        
    })
        
######## Display header of file uploaded by user ########
    observeEvent(input$fileName, {
        tryCatch(
            {
                df$x <- read.csv(input$fileName$datapath,
                    header = input$CB_header,
                    sep = input$sep,
                    quote = input$quote,
                    stringsAsFactors = input$CB_stringsAsFactors)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        
        print(paste0(input$fileName, " loaded"))
        
        # display uploaded file
        output$dataset <- DT::renderDataTable({
            req(df$x)
            DT::datatable(df$x, selection = list(target = 'row+column'))
        })
        
        # enable selection from current dataset variables
        choices <- colnames(df$x)
        updateSelectInput(session, "SI_varToConvert", choices = choices, selected = '')
        
        numericCols = colnames(select_if(df$x, is.numeric))   # select only numeric columns
        updateSelectInput(session, "SI_removeOutliers", choices = numericCols, selected = '')
        
        updateSelectInput(session, "SI_convertTo", choices = c("As factor", "As numeric", "As character"), selected = '')
        
        # fill SI_factorToRecode with existing factor columns
        factorChoices <- colnames(df$x[sapply(df$x, is.factor)])
        updateSelectInput(session, "SI_factorToRecode", choices = factorChoices)
        
        # show missing values label
        output$TO_missingValues = renderText({
          "Missing values"
        })
        
        # print 'Outliers' table label
        output$TO_outliersLabel = renderText({
          "Outliers"
        })

    })
    
    # change value to NA in selected columns
    observeEvent(input$BTN_changeToNA, {
      if (!is.null(input$dataset_columns_selected)) {
        df$x[ , input$dataset_columns_selected][df$x[ , input$dataset_columns_selected] == input$TI_valueToChangeToNA] = NA
        # if we remove values from factor column, apply factor() to drop unused factor levels
        if( is.factor( df$x[[input$dataset_columns_selected]] ) )
          df$x[ , input$dataset_columns_selected] = factor( df$x[ , input$dataset_columns_selected] )
      }
    })
    
    output$UI_selectColumn = renderText({
      if (is.null(input$dataset_columns_selected))
        paste("(select column on dataset first)")
    })
    
##### Print outliers and missing #####  
    output$HO_outliersAndMissing = function() {
      req(df$x)
      
      # outliers
      outl = sapply(df$x, find_outliers)
      
      # missing values
      # common_na_strings has bug: empty "" value spoils miss_scan_count results
      # recreate common_na_strings without ""
      na_strings = c("NA", "N A", "N/A", "NA ", " NA", "N /A", "N / A", "N / A ", "na", "n a", "n/a", "na ", " na", "n /a", "n / a", " a / a", "n / a ", "NULL", "null", "Not Available", "NOt available")
      na = sapply(df$x, function(y) sum(is.na(y))) # this will count NA's
      miss_scan = miss_scan_count(df$x, na_strings) # this won't count NA's)
      missing = miss_scan$n + na # add NA's manually to miss_scan_count results
      
      df = data.frame("missing" = missing, "outliers" = outl)
      df$missing = ifelse(missing !=0, cell_spec(missing, "html", background = "red", color = "white", align = "center"), missing)
      df %>%
        knitr::kable(format = "html", align = 'cc', escape = FALSE) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
    }
    
#### Remove outliers from selected column #####
    observeEvent(input$SI_removeOutliers, {
        df$x[input$SI_removeOutliers] = remove_outliers(unlist(df$x[input$SI_removeOutliers]))
    })
    
    
##### Convert to other type #####
    observeEvent(input$SI_convertTo, {
        req(df$x)
        cols = input$SI_varToConvert 
        
        # convert to factor
        if(input$SI_convertTo == "As factor") {
            #temp = as.factor( unlist(df$x[input$SI_varToConvert]) )   - other way to do it
            #df$x[input$SI_varToConvert] = temp
            df$x[cols] = lapply(df$x[cols], as.factor)
            
        # convert to numeric    
        } else if (input$SI_convertTo == "As numeric") {
            # if var is a factor, get first its levels and convert them back to numeric
            if( is.factor(unlist(df$x[input$SI_varToConvert])) ) {
                df$x[cols] = lapply(df$x[cols], as.character)
                df$x[cols] = lapply(df$x[cols], as.numeric)
                
            # if var is not factor, do normal conversion
            } else
                df$x[cols] = lapply(df$x[cols], as.numeric)
            
        # convert to character   
        } else {
            df$x[cols] = lapply(df$x[cols], as.character)
        }
        
        # fill selection box with factor variables
        factorChoices <- colnames(df$x[sapply(df$x, is.factor)])
        updateSelectInput(session, "SI_factorToRecode", choices = factorChoices)
        # reset selectInpiut box not to show anything
        updateSelectInput(session, "SI_convertTo", choices = c("As factor", "As numeric", "As character"), selected = '')
        
        # update remove outliers box to show only numeric cols
        numericCols = colnames(select_if(df$x, is.numeric))   # select only numeric columns
        updateSelectInput(session, "SI_removeOutliers", choices = numericCols, selected = '')
    })
    
    observeEvent(input$TI_recodeTo, {
        temp = as.factor(unlist(df$x[input$SI_factorToRecode])) # subsetting df results in class df - change it back to factor
        levels(temp)[levels(temp) == input$TI_recodeFrom] = input$TI_recodeTo # amend factor level
        df$x[input$SI_factorToRecode] = temp
    })
  
    
#### Delete selected rows    
    observeEvent(input$BTN_deleteRows, {
      if (!is.null(input$dataset_rows_selected)) {
        df$x <- df$x[-as.numeric(input$dataset_rows_selected),]
      }
    })
    
    output$UI_selectRow = renderText({
      if (is.null(input$dataset_rows_selected))
        paste("(select row on dataset first)")
    })
    
##### Other #####
    resStat = list() # list of open Stat tabs
    resML = list() # list of open ML tabs
    counterStat = reactiveValues(counterValue = 0) # counter of Stat tabs
    counterML = reactiveValues(counterValue = 0)  # counter of ML tabs
    
    
##### STATS tab #####
    # Add a new tab with modules

    # create tab title and add cross to close
    tabStatTitle = function(id) {
        tags$span(
            "Stats ", id, HTML("&nbsp;"),
            tags$span(
                id = paste0("close", id),
                class = "close",
                HTML("&times;")
            )
        )
    }

    addStatTab = function(id, dset) {
        
        resStat[[paste(id)]] <- callModule(
           module = stats_module,
           dataset = dset,
           id = id
        )
        
        appendTab(
            inputId = "stats_all_tabs",
            tabPanel(
                title = tabStatTitle(id),
                value = id,
                br(),
                stats_moduleUI(id = id)
            ),
            select = TRUE
        )
        
        # call closing function
        obs_close[[paste(id)]] <<- observe({
            shinyjs::onclick(id = paste0("close", id), closeStatTab(id = id))
        })
        
        print("STAT tab created")
 
    }
    
    # close tab
    closeStatTab <- function(id) {
        print(paste0("Closing STAT tab ", id))
        # Remove tab from UI
        removeTab(inputId = "stats_all_tabs", target = paste(id))
        # Remove related element from list
        resStat[[paste(id)]] = NULL
    }
    
    # call addTab after button click
    observeEvent(input$BTN_generateStats, {
        counterStat$counterValue <- counterStat$counterValue + 1
        addStatTab(id = counterStat$counterValue, dset = df$x)
    })
    

##### ML tab #####
    # Add a new tab with modules
    
    tabMLtitle = function(id) {
        tags$span(
            "Model ", id, HTML("&nbsp;"),
            tags$span(
                id = paste0("close", id),
                class = "close",
                HTML("&times;")
            )
        )
    }
    
    addMLTab = function(id, dataset) {
        
        model_type = switch(input$SI_selectModel, 
                            "Linear regression" = "linreg", 
                            "Binomial GLM" = "binomialGLM", 
                            "Linear Discriminant Analysis" = "LDA",
                            "Decision tree" = "decisionTree"
                            )
        model_typeUI = paste0(model_type, "UI")
        
        resML[[paste(id)]] <- callModule(
           module = get(model_type),
           dataset = dataset,
           id = id
        )
        
        appendTab(
            inputId = "ml_all_tabs",
            tabPanel(
                title = tabMLtitle(id),
                value = id,
                br(),
                get(model_typeUI)(id = id)
            ),
            select = TRUE
        )
        
        # call closing function
        obs_close[[paste(id)]] <<- observe({
            shinyjs::onclick(id = paste0("close", id), closeMLTab(id = id))
        })
        
        print("ML tab created")
    }
    
    # close tab
    closeMLTab <- function(id) {
        print(paste0("Closing ML tab ", id))
        # Remove tab from UI
        removeTab(inputId = "ml_all_tabs", target = paste(id))
        # Remove related element from list
        resML[[paste(id)]] = NULL
    }
    
    # call addTab after button click
    observeEvent(input$BTN_generateModel, {
        counterML$counterValue <- counterML$counterValue + 1
        addMLTab(id = counterML$counterValue, dataset = df$x)
    })
    
    output$BTN_saveDataset = downloadHandler(
      filename = function() { paste(input$BTN_datasets, "_", Sys.Date(), ".csv", sep = "") },
      content = function(file) {
          write.csv(df$x, file, row.names = F)
      }
    )
    
} #  end of server function

######## Run the application 
shinyApp(ui = ui, server = server)
