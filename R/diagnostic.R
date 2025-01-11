# Diagnostic Data Meta-Analysis Module

# UI Module
diagnosticUI <- function(id) {
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
            selectInput(ns("tp_col"), "True Positives", choices = NULL),
            selectInput(ns("fp_col"), "False Positives", choices = NULL),
            selectInput(ns("fn_col"), "False Negatives", choices = NULL),
            selectInput(ns("tn_col"), "True Negatives", choices = NULL)
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
                   choices = DIAGNOSTIC_MEASURES),
        
        selectInput(ns("method"), "Analysis Method",
                   choices = c(
                     "Bivariate Random-Effects" = "reml",
                     "HSROC" = "hsroc"
                   )),
        
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
        title = "SROC Plot",
        status = "primary",
        plotOutput(ns("sroc_plot"), height = "600px")
      )
    ),
    
    fluidRow(
      box(
        width = 6,
        title = "Forest Plot (Sensitivity)",
        status = "info",
        plotOutput(ns("sens_forest_plot"))
      ),
      
      box(
        width = 6,
        title = "Forest Plot (Specificity)",
        status = "warning",
        plotOutput(ns("spec_forest_plot"))
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "Heterogeneity Statistics",
        status = "primary",
        DTOutput(ns("heterogeneity_table"))
      )
    )
  )
}

# Server Module
diagnostic <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  
  # Reactive values for storing data and results
  rv <- reactiveValues(
    data = data.frame(
      study = character(),
      tp = numeric(),
      fp = numeric(),
      fn = numeric(),
      tn = numeric(),
      stringsAsFactors = FALSE
    ),
    results = NULL,
    sroc_data = NULL
  )
  
  # File upload handling
  observeEvent(input$data_file, {
    req(input$data_file)
    
    # Read uploaded file
    data <- read.csv(input$data_file$datapath, header = input$header)
    
    # Update column selection choices
    updateSelectInput(session, "study_col", choices = names(data))
    updateSelectInput(session, "tp_col", choices = names(data))
    updateSelectInput(session, "fp_col", choices = names(data))
    updateSelectInput(session, "fn_col", choices = names(data))
    updateSelectInput(session, "tn_col", choices = names(data))
    
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
      tp = NA,
      fp = NA,
      fn = NA,
      tn = NA,
      stringsAsFactors = FALSE
    )
    rv$data <- rbind(rv$data, new_row)
  })
  
  # Run meta-analysis
  observeEvent(input$run_analysis, {
    req(rv$data)
    
    # Validate data
    validate(
      need(all(c("tp", "fp", "fn", "tn") %in% names(rv$data)),
           "Missing required columns in data")
    )
    
    # Calculate sensitivity and specificity
    sens <- rv$data$tp / (rv$data$tp + rv$data$fn)
    spec <- rv$data$tn / (rv$data$tn + rv$data$fp)
    
    if (input$effect_measure == "sens_spec") {
      # Bivariate random-effects meta-analysis
      tryCatch({
        # Using metafor for bivariate model
        dat <- escalc(measure = "PLO",
                     xi = tp, ni = tp + fn,
                     data = rv$data)
        
        model <- rma.mv(yi, vi,
                       random = ~ 1 | study,
                       data = dat,
                       method = input$method)
        
        rv$results <- model
        
        # Calculate SROC curve points
        false_pos_rate <- seq(0, 1, length.out = 100)
        pred_sens <- predict(model, newdata = data.frame(fpr = false_pos_rate))
        
        rv$sroc_data <- data.frame(
          FPR = false_pos_rate,
          Sensitivity = pred_sens$pred
        )
        
      }, error = function(e) {
        showNotification(
          paste("Error in meta-analysis:", e$message),
          type = "error"
        )
      })
    } else {
      # Diagnostic Odds Ratio analysis
      tryCatch({
        dor <- (rv$data$tp * rv$data$tn) / (rv$data$fp * rv$data$fn)
        se_log_dor <- sqrt(1/rv$data$tp + 1/rv$data$fp + 1/rv$data$fn + 1/rv$data$tn)
        
        model <- rma(yi = log(dor),
                    sei = se_log_dor,
                    method = input$method)
        
        rv$results <- model
        
      }, error = function(e) {
        showNotification(
          paste("Error in meta-analysis:", e$message),
          type = "error"
        )
      })
    }
  })
  
  # Render results table
  output$results_table <- renderDT({
    req(rv$results)
    
    if (input$effect_measure == "sens_spec") {
      summary_data <- data.frame(
        Measure = c("Pooled Sensitivity", "Pooled Specificity",
                   "Sensitivity 95% CI", "Specificity 95% CI"),
        Value = c(
          sprintf("%.3f", inv.logit(rv$results$b[1])),
          sprintf("%.3f", inv.logit(rv$results$b[2])),
          sprintf("%.3f to %.3f",
                  inv.logit(rv$results$ci.lb[1]),
                  inv.logit(rv$results$ci.ub[1])),
          sprintf("%.3f to %.3f",
                  inv.logit(rv$results$ci.lb[2]),
                  inv.logit(rv$results$ci.ub[2]))
        )
      )
    } else {
      summary_data <- data.frame(
        Measure = c("Diagnostic Odds Ratio", "95% CI", "p-value"),
        Value = c(
          sprintf("%.3f", exp(rv$results$b)),
          sprintf("%.3f to %.3f",
                  exp(rv$results$ci.lb),
                  exp(rv$results$ci.ub)),
          sprintf("%.4f", rv$results$pval)
        )
      )
    }
    
    datatable(summary_data,
              options = list(dom = 't',
                           pageLength = 5,
                           searching = FALSE,
                           ordering = FALSE),
              rownames = FALSE)
  })
  
  # Render SROC plot
  output$sroc_plot <- renderPlot({
    req(rv$sroc_data)
    
    ggplot(rv$sroc_data, aes(x = FPR, y = Sensitivity)) +
      geom_line() +
      geom_point(data = data.frame(
        FPR = 1 - spec,
        Sensitivity = sens
      ), size = 2) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
      theme_minimal() +
      labs(x = "1 - Specificity",
           y = "Sensitivity",
           title = "Summary ROC Curve")
  })
  
  # Render forest plots
  output$sens_forest_plot <- renderPlot({
    req(rv$results)
    if (input$effect_measure == "sens_spec") {
      forest(metaprop(
        event = tp,
        n = tp + fn,
        studlab = study,
        data = rv$data,
        sm = "PLO"
      ))
    }
  })
  
  output$spec_forest_plot <- renderPlot({
    req(rv$results)
    if (input$effect_measure == "sens_spec") {
      forest(metaprop(
        event = tn,
        n = tn + fp,
        studlab = study,
        data = rv$data,
        sm = "PLO"
      ))
    }
  })
  
  # Render heterogeneity table
  output$heterogeneity_table <- renderDT({
    req(rv$results)
    
    if (input$effect_measure == "sens_spec") {
      het_data <- data.frame(
        Parameter = c("τ² (Sensitivity)", "τ² (Specificity)", "Correlation"),
        Value = c(
          sprintf("%.3f", rv$results$tau2[1]),
          sprintf("%.3f", rv$results$tau2[2]),
          sprintf("%.3f", rv$results$rho)
        )
      )
    } else {
      het_data <- data.frame(
        Parameter = c("τ²", "I²", "H²"),
        Value = c(
          sprintf("%.3f", rv$results$tau2),
          sprintf("%.1f%%", rv$results$I2),
          sprintf("%.2f", rv$results$H2)
        )
      )
    }
    
    datatable(het_data,
              options = list(dom = 't',
                           pageLength = 5,
                           searching = FALSE,
                           ordering = FALSE),
              rownames = FALSE)
  })
  
  # Sample data download handler
  output$download_sample <- downloadHandler(
    filename = function() {
      "diagnostic_sample.csv"
    },
    content = function(file) {
      file.copy("sample_data/diagnostic.csv", file)
    }
  )

  # Return reactive results for use in main server
  return(reactive({ rv$results }))
  })
}
