# Meta-Regression Analysis Module

# UI Module
meta_regUI <- function(id) {
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
            selectInput(ns("effect_col"), "Effect Size", choices = NULL),
            selectInput(ns("se_col"), "Standard Error", choices = NULL),
            
            # Moderator selection
            selectizeInput(ns("moderators"), "Select Moderators",
                         choices = NULL,
                         multiple = TRUE,
                         options = list(
                           placeholder = "Choose moderator variables",
                           plugins = list("remove_button")
                         ))
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
        
        selectInput(ns("method"), "Analysis Method",
                   choices = c(
                     "Mixed-Effects (REML)" = "REML",
                     "Mixed-Effects (ML)" = "ML",
                     "Mixed-Effects (DL)" = "DL"
                   )),
        
        numericInput(ns("conf_level"), "Confidence Level",
                    value = 0.95, min = 0.8, max = 0.99, step = 0.01),
        
        checkboxInput(ns("center_mods"), "Center Continuous Moderators", TRUE),
        
        actionButton(ns("run_analysis"), "Run Analysis",
                    icon = icon("play"), 
                    class = "btn-primary")
      ),
      
      box(
        width = 6,
        title = "Model Summary",
        status = "success",
        verbatimTextOutput(ns("model_summary"))
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "Moderator Effects",
        status = "primary",
        DTOutput(ns("moderator_effects"))
      )
    ),
    
    fluidRow(
      box(
        width = 6,
        title = "Bubble Plot",
        status = "info",
        
        # Moderator selection for bubble plot
        selectInput(ns("bubble_mod"), "Select Moderator",
                   choices = NULL),
        
        plotOutput(ns("bubble_plot"))
      ),
      
      box(
        width = 6,
        title = "Model Diagnostics",
        status = "warning",
        
        tabsetPanel(
          tabPanel("Residuals vs. Fitted",
                  plotOutput(ns("resid_plot"))),
          tabPanel("Q-Q Plot",
                  plotOutput(ns("qq_plot"))),
          tabPanel("Cook's Distance",
                  plotOutput(ns("cook_plot")))
        )
      )
    )
  )
}

# Server Module
meta_reg <- function(input, output, session) {
  ns <- session$ns
  
  # Reactive values for storing data and results
  rv <- reactiveValues(
    data = NULL,
    model = NULL,
    diagnostics = NULL
  )
  
  # File upload handling
  observeEvent(input$data_file, {
    req(input$data_file)
    
    # Read uploaded file
    data <- read.csv(input$data_file$datapath, header = input$header)
    
    # Update column selection choices
    updateSelectInput(session, "study_col", choices = names(data))
    updateSelectInput(session, "effect_col", choices = names(data))
    updateSelectInput(session, "se_col", choices = names(data))
    updateSelectizeInput(session, "moderators", choices = names(data))
    updateSelectInput(session, "bubble_mod", choices = names(data))
    
    # Store data
    rv$data <- data
  })
  
  # Manual input table
  output$manual_input_table <- renderDT({
    if (is.null(rv$data)) {
      rv$data <- data.frame(
        study = character(),
        effect = numeric(),
        se = numeric(),
        mod1 = numeric(),
        mod2 = numeric(),
        stringsAsFactors = FALSE
      )
    }
    
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
    req(rv$data)
    new_row <- data.frame(matrix(NA, nrow = 1, ncol = ncol(rv$data)))
    names(new_row) <- names(rv$data)
    new_row$study <- paste("Study", nrow(rv$data) + 1)
    rv$data <- rbind(rv$data, new_row)
  })
  
  # Run meta-regression
  observeEvent(input$run_analysis, {
    req(rv$data, input$effect_col, input$se_col, length(input$moderators) > 0)
    
    # Prepare formula
    mod_formula <- paste("~", paste(input$moderators, collapse = " + "))
    
    # Center continuous moderators if requested
    if (input$center_mods) {
      for (mod in input$moderators) {
        if (is.numeric(rv$data[[mod]])) {
          rv$data[[mod]] <- scale(rv$data[[mod]], center = TRUE, scale = FALSE)
        }
      }
    }
    
    # Run meta-regression
    tryCatch({
      model <- rma(yi = rv$data[[input$effect_col]],
                  sei = rv$data[[input$se_col]],
                  mods = as.formula(mod_formula),
                  data = rv$data,
                  method = input$method)
      
      rv$model <- model
      
      # Calculate diagnostics
      rv$diagnostics <- list(
        resid = residuals(model),
        fitted = fitted(model),
        cook.d = cooks.distance(model),
        hat = hatvalues(model)
      )
      
    }, error = function(e) {
      showNotification(
        paste("Error in meta-regression:", e$message),
        type = "error"
      )
    })
  })
  
  # Render model summary
  output$model_summary <- renderPrint({
    req(rv$model)
    summary(rv$model)
  })
  
  # Render moderator effects table
  output$moderator_effects <- renderDT({
    req(rv$model)
    
    coef_table <- coef(summary(rv$model))
    effects_df <- data.frame(
      Moderator = rownames(coef_table),
      Estimate = coef_table[, "estimate"],
      SE = coef_table[, "se"],
      Z = coef_table[, "zval"],
      `P-value` = coef_table[, "pval"],
      `Lower CI` = coef_table[, "ci.lb"],
      `Upper CI` = coef_table[, "ci.ub"]
    )
    
    datatable(effects_df,
              options = list(pageLength = 10,
                           searching = FALSE),
              rownames = FALSE) %>%
      formatRound(columns = c("Estimate", "SE", "Z", "P-value", "Lower CI", "Upper CI"),
                 digits = 3)
  })
  
  # Render bubble plot
  output$bubble_plot <- renderPlot({
    req(rv$model, input$bubble_mod)
    
    # Create bubble plot
    weights <- 1/rv$data[[input$se_col]]^2
    sizes <- sqrt(weights/max(weights)) * 20
    
    ggplot(rv$data, aes_string(x = input$bubble_mod,
                              y = input$effect_col)) +
      geom_point(aes(size = sizes), alpha = 0.6) +
      geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
      theme_minimal() +
      labs(x = input$bubble_mod,
           y = "Effect Size",
           size = "Precision") +
      theme(legend.position = "none")
  })
  
  # Render diagnostic plots
  output$resid_plot <- renderPlot({
    req(rv$diagnostics)
    
    ggplot(data.frame(fitted = rv$diagnostics$fitted,
                      resid = rv$diagnostics$resid),
           aes(x = fitted, y = resid)) +
      geom_point() +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_smooth(method = "loess", se = FALSE) +
      theme_minimal() +
      labs(x = "Fitted Values",
           y = "Residuals",
           title = "Residuals vs. Fitted Values")
  })
  
  output$qq_plot <- renderPlot({
    req(rv$diagnostics)
    
    ggplot(data.frame(resid = rv$diagnostics$resid),
           aes(sample = resid)) +
      stat_qq() +
      stat_qq_line() +
      theme_minimal() +
      labs(x = "Theoretical Quantiles",
           y = "Sample Quantiles",
           title = "Normal Q-Q Plot")
  })
  
  output$cook_plot <- renderPlot({
    req(rv$diagnostics)
    
    ggplot(data.frame(index = 1:length(rv$diagnostics$cook.d),
                      cook = rv$diagnostics$cook.d),
           aes(x = index, y = cook)) +
      geom_point() +
      geom_hline(yintercept = 4/length(rv$diagnostics$cook.d),
                 linetype = "dashed", color = "red") +
      theme_minimal() +
      labs(x = "Study Index",
           y = "Cook's Distance",
           title = "Cook's Distance Plot")
  })
  
  # Return reactive results for use in main server
  return(reactive({ rv$model }))
}
