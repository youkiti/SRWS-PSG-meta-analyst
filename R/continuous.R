# Continuous Data Meta-Analysis Module

# UI Module
continuousUI <- function(id) {
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
            downloadLink(ns("download_sample"), "Download Sample Data"),
            tags$br(), tags$br(),
            checkboxInput(ns("header"), "File has header", TRUE),
            
            # Column selection
            selectInput(ns("study_col"), "Study Column", choices = NULL),
            selectInput(ns("mean_treat_col"), "Mean (Treatment)", choices = NULL),
            selectInput(ns("sd_treat_col"), "SD (Treatment)", choices = NULL),
            selectInput(ns("n_treat_col"), "N (Treatment)", choices = NULL),
            selectInput(ns("mean_ctrl_col"), "Mean (Control)", choices = NULL),
            selectInput(ns("sd_ctrl_col"), "SD (Control)", choices = NULL),
            selectInput(ns("n_ctrl_col"), "N (Control)", choices = NULL)
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
                   choices = CONTINUOUS_MEASURES),
        
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
continuous <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  
  # Reactive values for storing data and results
  rv <- reactiveValues(
    data = data.frame(
      study = character(),
      mean_treat = numeric(),
      sd_treat = numeric(),
      n_treat = numeric(),
      mean_ctrl = numeric(),
      sd_ctrl = numeric(),
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
    updateSelectInput(session, "mean_treat_col", choices = names(data))
    updateSelectInput(session, "sd_treat_col", choices = names(data))
    updateSelectInput(session, "n_treat_col", choices = names(data))
    updateSelectInput(session, "mean_ctrl_col", choices = names(data))
    updateSelectInput(session, "sd_ctrl_col", choices = names(data))
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
      mean_treat = NA,
      sd_treat = NA,
      n_treat = NA,
      mean_ctrl = NA,
      sd_ctrl = NA,
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
      need(all(c("mean_treat", "sd_treat", "n_treat", 
                 "mean_ctrl", "sd_ctrl", "n_ctrl") %in% names(rv$data)),
           "Missing required columns in data")
    )
    
    # Calculate effect sizes and variances
    tryCatch({
      # Calculate mean differences and standard errors
      yi <- rv$data$mean_treat - rv$data$mean_ctrl
      
      # Calculate pooled SD for each study
      n1 <- rv$data$n_treat
      n2 <- rv$data$n_ctrl
      sd1 <- rv$data$sd_treat
      sd2 <- rv$data$sd_ctrl
      
      # Calculate variance for each study
      vi <- (sd1^2/n1 + sd2^2/n2)
      
      # Perform random effects meta-analysis
      model <- rma.uni(yi = yi, vi = vi, method = "REML")
      
      # Calculate prediction interval
      pred <- predict(model, level = input$conf_level, addpred = TRUE)
      
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
      Measure = c("Effect Size", "95% CI", "95% PI", "p-value", "I²", "τ²"),
      Value = c(
        sprintf("%.3f", model$b),
        sprintf("%.3f to %.3f", 
                model$ci.lb,
                model$ci.ub),
        sprintf("%.3f to %.3f",
                pred$pi.lb,
                pred$pi.ub),
        sprintf("%.4f", model$pval),
        sprintf("%.1f%%", model$I2),
        sprintf("%.3f", model$tau2)
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
    # Create forest plot with prediction interval
    forest(model,
           slab = rv$data$study,
           addpred = TRUE,
           header = "Study",
           mlab = "Random Effects Model",
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
        sprintf("%.2f", model$QE),
        model$k - 1,
        sprintf("%.4f", model$QEp),
        sprintf("%.1f%%", model$I2),
        sprintf("%.3f", model$tau2)
      )
    )
  })
  
  # Sample data download handler
  output$download_sample <- downloadHandler(
    filename = function() {
      "continuous_sample.csv"
    },
    content = function(file) {
      file.copy("sample_data/continuous.csv", file)
    }
  )

  # Return reactive results for use in main server
  return(reactive({ rv$results }))
  })
}
