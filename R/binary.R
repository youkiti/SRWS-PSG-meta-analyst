# Binary Data Meta-Analysis Module

# UI Module
binaryUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        width = 12,
        title = "Data Input",
        status = "primary",
        solidHeader = TRUE,
        
        tabsetPanel(
          id = ns("data_input_type"),
          
          # File Upload Tab
          tabPanel(
            "Upload Data",
            fileInput(ns("data_file"), "Choose CSV File",
                     accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
            checkboxInput(ns("header"), "File has header", TRUE),
            
            # Column selection
            selectInput(ns("study_col"), "Study Column", choices = NULL),
            selectInput(ns("e_treat_col"), "Events (Treatment)", choices = NULL),
            selectInput(ns("n_treat_col"), "Total (Treatment)", choices = NULL),
            selectInput(ns("e_ctrl_col"), "Events (Control)", choices = NULL),
            selectInput(ns("n_ctrl_col"), "Total (Control)", choices = NULL)
          ),
          
          # Manual Input Tab
          tabPanel(
            "Manual Input",
            DTOutput(ns("manual_input_table")),
            actionButton(ns("add_row"), "Add Row", icon = icon("plus")),
            actionButton(ns("remove_row"), "Remove Selected", icon = icon("minus"))
          )
        )
      )
    ),
    
    fluidRow(
      box(
        width = 6,
        title = "Analysis Settings",
        status = "info",
        
        selectInput(ns("effect_measure"), "Effect Measure",
                   choices = BINARY_MEASURES),
        
        selectInput(ns("method"), "Analysis Method",
                   choices = META_METHODS),
        
        numericInput(ns("conf_level"), "Confidence Level",
                    value = 0.95, min = 0.8, max = 0.99, step = 0.01),
        
        actionButton(ns("run_analysis"), "Run Analysis",
                    icon = icon("play"), 
                    class = "btn-primary")
      ),
      
      box(
        width = 6,
        title = "Summary Results",
        status = "success",
        DTOutput(ns("results_table"))
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "Forest Plot",
        status = "primary",
        plotOutput(ns("forest_plot"), height = "600px")
      )
    ),
    
    fluidRow(
      box(
        width = 6,
        title = "Funnel Plot",
        status = "info",
        plotOutput(ns("funnel_plot"))
      ),
      
      box(
        width = 6,
        title = "Heterogeneity",
        status = "warning",
        tableOutput(ns("heterogeneity_table"))
      )
    )
  )
}

# Server Module
binary <- function(input, output, session) {
  ns <- session$ns
  
  # Reactive values for storing data and results
  rv <- reactiveValues(
    data = data.frame(
      study = character(),
      e_treat = numeric(),
      n_treat = numeric(),
      e_ctrl = numeric(),
      n_ctrl = numeric(),
      stringsAsFactors = FALSE
    ),
    results = NULL
  )
  
  # File upload handling
  observeEvent(input$data_file, {
    req(input$data_file)
    
    # Read uploaded file
    data <- read.csv(input$data_file$datapath, header = input$header)
    
    # Update column selection choices
    updateSelectInput(session, "study_col", choices = names(data))
    updateSelectInput(session, "e_treat_col", choices = names(data))
    updateSelectInput(session, "n_treat_col", choices = names(data))
    updateSelectInput(session, "e_ctrl_col", choices = names(data))
    updateSelectInput(session, "n_ctrl_col", choices = names(data))
    
    # Store data
    rv$data <- data
  })
  
  # Manual input table
  output$manual_input_table <- renderDT({
    datatable(
      rv$data,
      editable = TRUE,
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 15, 20)
      )
    )
  })
  
  # Add row to manual input
  observeEvent(input$add_row, {
    new_row <- data.frame(
      study = paste("Study", nrow(rv$data) + 1),
      e_treat = NA,
      n_treat = NA,
      e_ctrl = NA,
      n_ctrl = NA,
      stringsAsFactors = FALSE
    )
    rv$data <- rbind(rv$data, new_row)
  })
  
  # Run meta-analysis
  observeEvent(input$run_analysis, {
    req(rv$data)
    
    # Validate data
    validate(
      need(all(c("e_treat", "n_treat", "e_ctrl", "n_ctrl") %in% names(rv$data)),
           "Missing required columns in data")
    )
    
    # Perform meta-analysis using meta package
    tryCatch({
      model <- metabin(
        event.e = rv$data$e_treat,
        n.e = rv$data$n_treat,
        event.c = rv$data$e_ctrl,
        n.c = rv$data$n_ctrl,
        studlab = rv$data$study,
        sm = input$effect_measure,
        method = input$method,
        level = input$conf_level,
        level.comb = input$conf_level
      )
      
      rv$results <- model
      
    }, error = function(e) {
      showNotification(
        paste("Error in meta-analysis:", e$message),
        type = "error"
      )
    })
  })
  
  # Render results
  output$results_table <- renderDT({
    req(rv$results)
    
    summary_data <- data.frame(
      Measure = c("Effect Size", "95% CI", "p-value", "I²", "τ²"),
      Value = c(
        sprintf("%.3f", rv$results$TE.random),
        sprintf("%.3f to %.3f", 
                rv$results$lower.random,
                rv$results$upper.random),
        sprintf("%.4f", rv$results$pval.random),
        sprintf("%.1f%%", rv$results$I2),
        sprintf("%.3f", rv$results$tau2)
      )
    )
    
    datatable(summary_data,
              options = list(dom = 't',
                           pageLength = 5,
                           searching = FALSE,
                           ordering = FALSE),
              rownames = FALSE)
  })
  
  # Render forest plot
  output$forest_plot <- renderPlot({
    req(rv$results)
    forest(rv$results,
           leftlabs = c("Study", "Events", "Total", "Proportion"),
           rightlabs = c("Weight", "Effect (95% CI)"),
           fontsize = 1)
  })
  
  # Render funnel plot
  output$funnel_plot <- renderPlot({
    req(rv$results)
    funnel(rv$results)
  })
  
  # Render heterogeneity table
  output$heterogeneity_table <- renderTable({
    req(rv$results)
    data.frame(
      Statistic = c("Q", "df", "p-value", "I²", "τ²"),
      Value = c(
        sprintf("%.2f", rv$results$Q),
        rv$results$df.Q,
        sprintf("%.4f", rv$results$pval.Q),
        sprintf("%.1f%%", rv$results$I2),
        sprintf("%.3f", rv$results$tau2)
      )
    )
  })
  
  # Return reactive results for use in main server
  return(reactive({ rv$results }))
}
