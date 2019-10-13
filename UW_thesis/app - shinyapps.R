# Author: Wojciech Gusztyla

####### PACKAGES #######
library(shiny)
library(ISLR)
library(ggplot2)
library(shinyjs)
library(tidyverse)
library(DT)
library(naniar)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(colourpicker)
library(ggthemes)
library(datasets)
library(car) # for VIF
library(kableExtra) # for kable
library(Hmisc)
library(caret)
library(caTools) # for ROC curve
library(boot)
library(GGally) # ggpairs
library(shinycssloaders) # for progress indicator
library(rpart) # for decision trees
library(rpart.plot) # to print decision trees
library(e1071)
library(broom)


source("stats_module.R")
source("linreg.R")  # linear regression model
source("binomialGLM.R") # logistic regression model
source("LDA.R") # Linear Discriminant Analysis
source("decisionTree.R") # Decision tree

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
ui <- fluidPage(theme = "spacelab.min.css", title = "UW_thesis",

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
                sidebarPanel(width = 3,
                    HTML("<div align='center'><b>Please select the dataset</b></div>"),
                    br(),
                    fluidRow(
                        column(10,
                            # to select all packages from R 'datasets': choices = ls("package:datasets")
                            selectInput("BTN_datasets", label = NULL, choices = c("airquality", "iris", "mtcars", "rock", "quakes", ls("package:ISLR")), 
                            selected = "mtcars")
                        ),
                        column(2, 
                            actionButton("BTN_load", label = "Go!")
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
                    
                    # Input: Select separator
                    radioButtons("sep", "Separator",
                                 choices = c(Comma = ",",
                                             Semicolon = ";",
                                             Tab = "\t"),
                                 selected = ","),
                    
                    # Input: Select quotes
                    radioButtons("quote", "Quote",
                      choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"), selected = ""),
                    
                    # Rownames
                    #textInput("TI_rownames", "Row names column", value = 1, placeholder = "1", width = "100px"),
                    
                    # change to NA
                    tags$hr(style ="border-top: 1px solid #888888;"),
                    HTML("<div align='center'><b>Change value to NA</b></div>"),
                    br(),
                    fluidRow(
                      
                      # tags$form(class="form-horizontal",
                      #   tags$div(class="form-group",
                      #     tags$label(class = "col-sm-3 control-label", `for` = "enterValue", "Enter value:"),
                          column(10,
                            textInput("TI_valueToChangeToNA", "Enter value")
                          ),
                          column(2,
                            actionButton("BTN_changeToNA", "Go!")   
                          )
                      #   ) # form group
                      # ) # form horizontal
                    
                    ),
                    
                    # convert to other type
                    tags$hr(style ="border-top: 1px solid #888888;"),
                    HTML("<div align='center'><b>Convert variable type</b></div>"),
                    br(),
                    fluidRow(
                        column(5,
                            selectInput("SI_varToConvert", label = "Select variable:", choices = NULL, multiple = TRUE)
                        ),
                        column(5,
                            selectInput("SI_convertTo", label = "Choose type:", choices = c("factor", "numeric", "character"))   
                        ),
                        column(2,
                            actionButton("BTN_convert", label = "Go!")   
                        )
                    ),
                    
                    # recode factor levels
                    tags$hr(style ="border-top: 1px solid #888888;"),
                    HTML("<div align='center'><b>Recode factor levels</b></div>"),
                    br(),
                    fluidRow(
                        column(4,
                            selectInput("SI_factorToRecode", label = "Factor variable:", choices = NULL)
                        ),
                        column(3,
                            textInput("TI_recodeFrom", label = "From:")   
                        ),
                        column(3,
                            textInput("TI_recodeTo", label = "To:")   
                        ),
                        column(2,
                            actionButton("BTN_recodeFactor", label = "Go!")   
                        )
                    ),
                    
                    # delete selected rows button
                    tags$hr(style ="border-top: 1px solid #888888;"),
                    HTML("<div align='center'><b>Delete selected rows</b></div>"),
                    br(),
                    fluidRow(
                      column(width = 2,
                        actionButton("BTN_deleteRows", "Delete")
                      )
                    ),
                      
                    # remove outliers
                    tags$hr(style ="border-top: 1px solid #888888;"),
                    HTML("<div align='center'><b>Remove outliers</b></div>"),
                    br(),
                    fluidRow(
                        column(10,
                            selectInput("SI_removeOutliers", label = "Choose column:", choices = NULL)
                        ),
                        
                        column(2,
                            actionButton("BTN_removeOutliers", label = "Go!")   
                        )
                    )
                    
                ) # sidebarPanel
                ), # div
              
                mainPanel(
                    # row with printed dataset
                    fluidRow(
                        DT::dataTableOutput("dataset")
                    ),
                    
                #tags$div(style = 'overflow-x: auto;',    
                    fluidRow(
                      # table with missing values
                      column(width = 2, #style = 'border: 1px solid #888888;',
                        h4(textOutput("TO_missingValues")),
                        tableOutput("showMissing")
                      ),
                      
                      #column(width = 1), # make space between tables
                      
                      # table with potential outliers
                      column(width = 3, #style = 'border: 1px solid #888888;',
                        h4(textOutput("TO_outliersLabel")),
                        tableOutput("TA_outliersTable")
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
    df = reactiveValues(x = NULL) # store name of the loaded dataset to be available across modules
    obs_closeStat <- list()
    obs_closeML <- list()

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
        updateSelectInput(session, "SI_removeOutliers", choices = choices, selected = '')
        
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
        
######## Display file uploaded by user ########
    observeEvent(input$fileName, {
        tryCatch(
            {
                df$x <- read.csv(input$fileName$datapath,
                    header = input$CB_header,
                    sep = input$sep,
                    quote = input$quote,
                    stringsAsFactors = input$CB_stringsAsFactors,
                    row.names = 1,
                    check.names = FALSE)
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
        updateSelectInput(session, "SI_removeOutliers", choices = choices, selected = '')
        
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
      }
    })
    
    #output$showMissing = renderTable({
    output$showMissing = function() {
      req(df$x)
      # common_na_strings has bug: empty "" value spoils miss_scan_count results
      # recreate common_na_strings without ""
      na_strings = c("NA", "N A", "N/A", "NA ", " NA", "N /A", "N / A", "N / A ", "na", "n a", "n/a", "na ", " na", "n /a", "n / a", " a / a", "n / a ", "NULL", "null", "Not Available", "NOt available")
      na = sapply(df$x, function(y) sum(is.na(y))) # this will count NA's
      miss_scan = miss_scan_count(df$x, na_strings) # this won't count NA's
      result = miss_scan
      result$n = miss_scan$n + na # add NA's manually to miss_scan_count results
      
      # pretty print with kable
      result %>% mutate(
        n = ifelse(n !=0, cell_spec(n, "html", background = "red", color = "white", align = "center"), n)
      ) %>%
        knitr::kable(format = "html", escape = FALSE) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
    }
    
##### Print outliers #####  
    #output$TA_outliersTable = renderTable({
    output$TA_outliersTable = function() {
      req(df$x)
      res = sapply(df$x, find_outliers)
      result = data.frame("Variable" = names(res), "outliers" = res)
      
      # pretty print with kable
      result = as.tibble(result)
      result %>% 
        knitr::kable(format = "html", escape = FALSE) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
    }
    
##### Convert to other type #####
    observeEvent(input$BTN_convert, {
    
        cols = input$SI_varToConvert 
        
        # convert to factor
        if(input$SI_convertTo == "factor") {
            #temp = as.factor( unlist(df$x[input$SI_varToConvert]) )   - other way to do it
            #df$x[input$SI_varToConvert] = temp
            df$x[cols] = lapply(df$x[cols], as.factor)
            
        # convert to numeric    
        } else if (input$SI_convertTo == "numeric") {
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

    })
    
    observeEvent(input$BTN_recodeFactor, {
        temp = as.factor(unlist(df$x[input$SI_factorToRecode])) # subsetting df results in class df - change it back to factor
        levels(temp)[levels(temp) == input$TI_recodeFrom] = input$TI_recodeTo # amend factor level
        df$x[input$SI_factorToRecode] = temp
    })
    
#### Remove outliers from selected column #####
    observeEvent(input$BTN_removeOutliers, {
      df$x[input$SI_removeOutliers] = remove_outliers(unlist(df$x[input$SI_removeOutliers]))
    })
    
##### Other #####
    resStat = list() # list of open Stat tabs
    resML = list() # list of open ML tabs
    counterStat = reactiveValues(counterValue = 0) # counter of Stat tabs
    counterML = reactiveValues(counterValue = 0)  # counter of ML tabs
    
    
##### STATS tab #####
    # Add a new tab with modules

    # create tab title and add cross to close
    tabStatTitle = function(idStat) {
        tags$span(
            "Stats ", idStat, HTML("&nbsp;"),
            tags$span(
                id = paste0("closeStat", idStat),
                class = "close",
                HTML("&times;")
            )
        )
    }

    addStatTab = function(idStat, dset) {
        
        resStat[[paste(idStat)]] <- callModule(
           module = stats_module,
           dataset = dset,
           id = idStat
        )
        
        appendTab(
            inputId = "stats_all_tabs",
            tabPanel(
                title = tabStatTitle(idStat),
                value = idStat,
                br(),
                stats_moduleUI(id = idStat)
            ),
            select = TRUE
        )
        
        # call closing function
        obs_closeStat[[paste(idStat)]] <<- observe({
            shinyjs::onclick(id = paste0("closeStat", idStat), closeStatTab(id = idStat))
        })
        
        print("STAT tab created")
 
    }
    
    # close tab
    closeStatTab <- function(idStat) {
        print(paste0("Closing STAT tab ", idStat))
        # Remove tab from UI
        removeTab(inputId = "stats_all_tabs", target = paste(idStat))
        # Remove related element from list
        resStat[[paste(idStat)]] = NULL
    }
    
    # call addTab after button click
    observeEvent(input$BTN_generateStats, {
        counterStat$counterValue <- counterStat$counterValue + 1
        addStatTab(id = counterStat$counterValue, dset = df$x)
    })
    

##### ML tab #####
    # Add a new tab with modules
    
    tabMLtitle = function(idML) {
        tags$span(
            "Model ", idML, HTML("&nbsp;"),
            tags$span(
                id = paste0("closeML", idML),
                class = "close",
                HTML("&times;")
            )
        )
    }
    
    addMLTab = function(idML, dataset) {
        
        model_type = switch(input$SI_selectModel, 
                            "Linear regression" = "linreg", 
                            "Binomial GLM" = "binomialGLM", 
                            "Linear Discriminant Analysis" = "LDA",
                            "Decision tree" = "decisionTree"
                            )
        model_typeUI = paste0(model_type, "UI")
        
        resML[[paste(idML)]] <- callModule(
           module = get(model_type),
           dataset = dataset,
           id = idML
        )
        
        appendTab(
            inputId = "ml_all_tabs",
            tabPanel(
                title = tabMLtitle(idML),
                value = idML,
                br(),
                get(model_typeUI)(id = idML)
            ),
            select = TRUE
        )
        
        # call closing function
        obs_closeML[[paste(idML)]] <<- observe({
            shinyjs::onclick(id = paste0("closeML", idML), closeMLTab(id = idML))
        })
        
        print("ML tab created")
    }
    
    # close tab
    closeMLTab <- function(idML) {
        print(paste0("Closing ML tab ", idML))
        # Remove tab from UI
        removeTab(inputId = "ml_all_tabs", target = paste(idML))
        # Remove related element from list
        resML[[paste(idML)]] = NULL
    }
    
    # call addTab after button click
    observeEvent(input$BTN_generateModel, {
        counterML$counterValue <- counterML$counterValue + 1
        addMLTab(id = counterML$counterValue, dataset = df$x)
    })
    
    
    observeEvent(input$BTN_deleteRows, {
      if (!is.null(input$dataset_rows_selected)) {
        df$x <- df$x[-as.numeric(input$dataset_rows_selected),]
      }
    })
    
} #  end of server function

######## Run the application 
shinyApp(ui = ui, server = server)
