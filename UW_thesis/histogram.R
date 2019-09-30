histogramUI <- function(id) {
  ns <- NS(id)
  
  div(
    fluidRow(
      column(12,
        uiOutput(ns("ui_PL_histogram"))
      )
    ),
    
    br(),
  
    fluidRow(
      column(4,
        selectInput(ns("xvar"), label = "Choose OX", choices = NULL) 
      ),
      # #595959 is the default greyish fill of ggplot
      # column(4,
      #   colourInput(ns("color"), label = "Color")
      # ),
      column(4,
        selectInput(ns("fill"), label = "Fill", choices = NULL)
      ),
      column(2,
        #radioButtons(ns("yvar"), label = "Choose OY", choices = c("Count", "Density")) 
        uiOutput(ns("UI_yvar"))
      ),
      column(2,
        #radioButtons(ns("mean_line"), label = "Mean line", choices = c("No", "Yes"))
        uiOutput(ns("UI_meanLine"))
      ),
      column(2,
        #radioButtons(ns("position"), label = "Position", choices = c("Stack", "Dodge")) 
        uiOutput(ns("UI_position"))
      ),
      column(4,
        uiOutput(ns("sliderUI"))
      ),
      column(3,
        sliderInput(ns("alpha"), label = "Alpha (transparency)", min = 0.0, max = 1, value = 1, step = 0.1) 
      )
    ),
    
    fluidRow(
      column(12,
        uiOutput(ns("ui_DIV_warn"))
      )
    )
  ) 
}


histogram <- function(input, output, session, id, dataset = NULL, OX = NULL, OY = NULL) {
  
  ns <- session$ns

  # enable selection from current dataset variables
  choices <- colnames(dataset())
  # add None to be able to deselect variable with selectInput
  choicesWithNone = c("None", choices)
  
  updateSelectInput(session, "xvar", choices = choices, selected = '')
  updateSelectInput(session, "fill", choices = choicesWithNone, selected = '')
  
  # due to bug in Shiny (https://github.com/rstudio/shiny/issues/2398) I have to use renderUI instead of updateSliderInput
  bw <- reactiveValues(max = 10, value = 1)
  bwidth = reactiveValues(max = 10, value = 1)
  
  output$sliderUI <- renderUI({
    # sliderInput(ns("binwidth"), "Binwidth", min = 1, max = bw$max, value = bw$value, step = 1)
    sliderInput(ns("binwidth"), "Binwidth", min = 1, max = bwidth$max, value = bw$value, step = 1)
  })
  
  observeEvent(input$xvar, {

    if(input$xvar == "" || input$xvar == "None") {
      bins = 30 
      bw$value = floor(bins/2)
      bw$max = bins
      
      
    # if xvar is not numeric we don't calculate bins
    # input$xvar is a character string, its class is character and logical comparison doesn't do what I want ( is.numeric(dataset()[input$xvar]) ) 
    # First I need to subset dataset() with [[ operator which gives me a vector and then check its class  
    } else if( is.numeric(dataset()[[input$xvar]]) ) {
        
        # one method of calculation
        # bins = nclass.FD(unlist(dataset()[input$xvar]))
        # bw$value = floor(bins/2)
        # bw$max = bins
        
        breaks = pretty(range(unlist(dataset()[input$xvar])), n = nclass.FD(unlist(dataset()[input$xvar])), min.n = 1)
        bwidth$max <- breaks[2]-breaks[1]
        
        
    } else
        bins = NULL


  })
  
  
  output$PL_histogram = renderPlot({

      if( !is.null(dataset()) && !(input$xvar == "") ) {
        
        ### HISTOGRAM ###
        # if xvar is numeric, plot histogram for continuous variable
        if( is.numeric(dataset()[[input$xvar]]) ) {
          
          p = ggplot(dataset(), aes_string(
                x = input$xvar,
                fill = ifelse( !(input$fill == "None" || input$fill == ""), sprintf("factor(%s)", input$fill), "NULL")
              )) 
          
          if(input$yvar == "Density") {
            p = p + geom_histogram(
              aes_string(y="..density.."),
              alpha = input$alpha,
              position = input$position,
              binwidth = input$binwidth
            ) + geom_density(alpha=.2, fill="#FF6666", na.rm = TRUE)
  
          } else {
            p = p + geom_histogram(
              alpha = input$alpha,
              position = input$position,
              binwidth = input$binwidth
            )
          }
          
          
          # add mean line
          if(input$mean_line == "Yes") {
            p = p + geom_vline(aes_string( xintercept = mean(unlist(dataset()[input$xvar]))), linetype = "dashed", size = 1)
          }
          
          p = p + theme_economist() + theme(legend.position = "right") 
          
          if( !(input$fill == "None" || input$fill == "") ) {
            p = p + labs(fill = input$fill)
          } 
          
          
          
        ### BARPLOT ###  
        # if xvar not numeric plot barplot for discrete variable
        } else { 
            p = ggplot(dataset(), aes_string(
              x = input$xvar,
              fill = ifelse( !(input$fill == "None" || input$fill == ""), sprintf("factor(%s)", input$fill), "NULL")
            ))
            
            p = p + geom_bar(alpha = input$alpha, position = input$position)
            
            p = p + theme_economist() + theme(legend.position = "right") 
            
            if( !(input$fill == "None" || input$fill == "") ) {
              p = p + labs(fill = input$fill)
            } 
        } 
        
        
        # print final plot
        p
      }
  })
  
  output$test <- renderText({input$binwidth})
  
  # Display plot if dataset loaded
  output$ui_PL_histogram <- renderUI({
    if ( !is.null(dataset()) ) 
      plotOutput(ns("PL_histogram"))
  })
  
  # Warning if no data loaded
  output$ui_DIV_warn <- renderUI({
    if ( is.null(dataset()) ) {
      tags$div(
        span(class = "warn", "No dataset loaded")
      )
    }
  })
  
  
  # show OY choice only if continuous variable is selected on X axis
  output$UI_yvar <- renderUI({
    if ( is.numeric(dataset()[[input$xvar]]) ) {
      radioButtons(ns("yvar"), label = "Choose OY", choices = c("Count", "Density")) 
    } else {
      shinyjs::disabled(
        radioButtons(ns("yvar"), label = "Choose OY", choices = c("Count", "Density")) 
      )
    }
  })
  
  # show mean line only if continuous variable is selected on X axis
  output$UI_meanLine <- renderUI({
    if ( is.numeric(dataset()[[input$xvar]]) ) {
        radioButtons(ns("mean_line"), label = "Mean line", choices = c("No", "Yes")) 
    } else {
      shinyjs::disabled(
        radioButtons(ns("mean_line"), label = "Mean line", choices = c("No", "Yes")) 
      )
    }
  })
  
  
  # show position choice only when factor/character variable is selected on X axis
  output$UI_position <- renderUI({
    if ( is.null(dataset()) || input$xvar == "" || is.numeric(dataset()[[input$xvar]]) ) {
      shinyjs::disabled(
        radioButtons(ns("position"), label = "Position", choices = c("Stack", "Dodge")) 
      )
    } else {
        radioButtons(ns("position"), label = "Position", choices = c("Stack", "Dodge")) 
    }
  })
  
}