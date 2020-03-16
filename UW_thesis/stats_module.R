## STATs module
source("apply_function.R")
source("apply_scale.R")
source("scatterplot.R")
source("funHistory.R")
source("boxplot.R")
source("histogram.R")
source("ggpairs.R")

stats_moduleUI <- function(id) {
  ns <- NS(id)
  
  #tagList(
    div(
      tabsetPanel(type = "tabs",
          tabPanel("Summary",
              fluidRow(
                column(width = 4,
                  radioButtons(ns("RB_summaryType"), "", choices = c("Hmisc", "head", "summary", "str", "dfSummary", "descr"), selected = "Hmisc", inline = TRUE)       
                )
              ),
          
              fluidRow(
                column(width = 8,
                  verbatimTextOutput(ns("TO_summaryWhole"))
                 ),
                column(width = 4,
                  column(12,
                    h4(textOutput(ns("TO_NormalityTestsLabel"))),
                    htmlOutput(ns("TO_NormalityTests")),    # kable returns HTML so render table with htmlOutput (else it'll print HTML code)
                    br(),
                    h4(textOutput(ns("TO_QQplotLabel"))),
                    selectInput(ns("SI_QQplot"), label = "", choices = NULL),
                    plotOutput(ns("PO_QQplot"))
                  )
                )
              ),
          ),
          
          tabPanel("Plots",
            fluidRow(
              column(width = 4,
                panel(
                  heading = "Module : apply_function",
                  status = "warning", # to apply color to the panel border
                  apply_functionUI(id = ns("mod1"))
                ),
        
                panel(
                  heading = "Module : apply_scale",
                  status = "danger",
                  apply_scaleUI(id = ns("mod2"))
                ),
        
                panel(
                  heading = "Module : funHistory",
                  status = "default",
                  funHistoryUI(id = ns("mod3"))
                )
              ),
      
              column(width = 8,
                panel(
                  heading = "Module : scatterplot",
                  status = "success",
                  scatterplotUI(id = ns("mod4"))
                )
              )
            ),
    
            br(),
    
            fluidRow(
              column(width = 6,
                panel(
                  heading = "Module : boxplot",
                  status = "primary",
                  boxplotUI(id = ns("mod5"))
                )
              ),
      
              column(width = 6,
                panel(
                  heading = "Module : histogram / barplot",
                  status = "info",
                  histogramUI(id = ns("mod6"))
                )
              )
            ),
    
            br(),

            fluidRow(
              column(width = 12,
                panel(
                  heading = "Module : plot matrix",
                  ggPairsUI(id = ns("mod7"))
                )
              )
            )
          ) # tabPanel
      ) # tabset
    ) #div
  #) #taglist
}


stats_module <- function(input, output, session, dataset, id) {
    ns <- session$ns
    req(dataset)

    # ReactiveValue that "belongs" to Application and updates through all modules
    rv <- reactiveValues(df = dataset, variable = NULL, fun_history = NULL)
    choices = colnames( dplyr::select_if(dataset, is.numeric) )
    updateSelectInput(session, "SI_QQplot", choices = choices, selected = '')
    
    # Summary
    output$TO_summaryWhole <- renderPrint({
      req(rv$df)

      switch(input$RB_summaryType,
        head = {
          head(rv$df)
        },

        summary = {
          summary(rv$df)
        },

        str = {
          str(rv$df)
        },

        Hmisc = {
          # html code is printed instead of nice output
          # html = summary(rv$df) %>% kable() %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive", full_width = F)) %>%
          #   scroll_box(width = "100%", height = "100%")
          print(class(rv$df))
          # print character vector without quotes and indices (Hmisc::describe cannot be printed out directly)
          out = capture.output(Hmisc::describe(rv$df))
          paste(noquote(out), collapse = '\n') %>% cat()
        },

        dfSummary = {
          summarytools::dfSummary(rv$df)
        },

        descr = {
          summarytools::descr(rv$df)
        },

        {
          head(rv$df)  # default
        }

      )

    })
    
    # normality tests
    
    output$TO_NormalityTestsLabel = renderText({
      "Normality tests for numeric columns"
    })
    
    # shapiro.wilk function wrapper
    sw = function(x) {
      sw_test = lapply(x, shapiro.test)
      ks_test = lapply(x, function(y) {
        ks.test(y, "pnorm", mean = mean(y), sd = sd(y))
      })
      # Convert a list to a data frame
      out_sw = do.call(rbind.data.frame, sw_test)[1:2]
      out_ks = do.call(rbind.data.frame, ks_test)[1:2]
      out = cbind(out_sw, out_ks)
      
      names(out)[1] = "D"
      names(out)[2] = "p.val"
      names(out)[3] = "W"
      names(out)[4] = "p.val"
      
      return( format(round(out, 3), scientific = FALSE) )
    }
    
    # render output table with normality tests
    output$TO_NormalityTests = renderText({    # render HTML code so kable works nicely
      req(rv$df)
      numeric <- rv$df[sapply(rv$df, is.numeric)] 
      res = sw(numeric)
      # highlight p-values < 0.05 (not normal)
      res[, 2] = ifelse(res[, 2] < 0.05, 
                        cell_spec(res[, 2], background = "red", color = "white", align = "center"), 
                        cell_spec(res[, 2], background = "lightgreen", color = "white", align = "center"))
      res[, 4] = ifelse(res[, 4] < 0.05, 
                        cell_spec(res[, 4], background = "red", color = "white", align = "center"), 
                        cell_spec(res[, 4], background = "lightgreen", color = "white", align = "center"))
      
      res %>% 
        knitr::kable("html", align='cccc', escape = F) %>% 
        kable_styling(
          bootstrap_options = c("striped", "hover", "responsive")
        ) %>%
        add_header_above(c(" ", "Shapiro-Wilk" = 2, "Kolmogorov-Smirnov" = 2))
    })


    # Q-Q plot
    output$TO_QQplotLabel = renderText({
      "Q-Q plot"
    })
    
    output$PO_QQplot = renderPlot({
      if( !is.null(dataset) && !(input$SI_QQplot == "") ) {
        qqPlot(dataset[[input$SI_QQplot]], ylab = input$SI_QQplot)
      }
    })

    
    ##################################
    ## Module 1 : Apply Function  ####
    ##     id call = "mod1"       ####
    ##################################
    {
        # apply log, abs, sqrt to numeric column
        data_mod1 <- callModule(
            module = apply_function, 
            id = "mod1",
            # if I passed reactive(rv$df) then each function log, abs, sqrt will be applied to previous value of variable (log to var, then sqrt to log of var etc.)
            # passing rv$df means that each button click applies function on original variable value
            dataset = rv$df
        )
    }
    
    observeEvent(data_mod1$trigger, {
      req(data_mod1$trigger>0)
      rv$df[, data_mod1$variable] <- data_mod1$result
      rv$fun_history <- c(rv$fun_history, paste0(data_mod1$fun, " ", data_mod1$variable))
    })
    
    ##############################+
    ## Module 2 : Apply Scale  ####
    ##     id call = "mod2"    ###+
    ##############################+
    {
      # Call module scale
      data_mod2 <- callModule(
        module = apply_scale, 
        id = "mod2",
        dataset = rv$df
      )
      
      # When applied function (data_mod2$trigger change) :
      #   - Update rv$variable with module output "variable"
      #   - Update rv$fun_history with "scale"
      observeEvent(data_mod2$trigger, {
        req(data_mod2$trigger>0)
        rv$df[, data_mod2$variable] <- data_mod2$result
        rv$fun_history <- c(rv$fun_history, paste0("scale ", data_mod2$variable))
      })
    }
    
    ####################################+
    ## Module 3 : Functions History  ####
    ##     id call = "mod3"          ###+
    ####################################+
    {
      # Call module funHistory
      callModule(
        module = funHistory,
        id = "mod3",
        histo = reactive(rv$fun_history)
      )
    }
    
    #############################+
    ## Module 4 : Scatterplot ###+
    ##     id call = "mod4"   ###+
    #############################+
    {
      # Call module scatterplot
      callModule(
        module = scatterplot, 
        id = "mod4",
        dataset = reactive(rv$df)
      )
    }
    
    ##############################+
    ## Module 5 : Plot boxplot ###+
    ##     id call = "mod5"    ###+
    ##############################+
    {
      # Call module boxplot
      callModule(
        module = boxplot, 
        id = "mod5",
        dataset = reactive(rv$df)
      )
    }
    
    ################################+
    ## Module 6 : Plot histogram ###+
    ##     id call = "mod6"      ###+
    ################################+
    {
      # Call module histogram
      callModule(
        module = histogram, 
        id = "mod6",
        dataset = reactive(rv$df)
      )
    }
    
    ################################+
    ## Module 7 : Plot ggPairs   ###+
    ##     id call = "mod7"      ###+
    ################################+
    {
      # Call module ggPairs
      callModule(
        module = ggPairs,
        id = "mod7",
        dataset = reactive(rv$df)
      )
    }
}