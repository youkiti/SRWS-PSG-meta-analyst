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
            downloadLink(ns("download_sample"), "Download Sample Data"),
            tags$br(), tags$br(),
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
meta_reg <- function(id) {
  moduleServer(id, function(input, output, session) {
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
    
    # Check multicollinearity
    if (length(input$moderators) > 1) {
      mod_data <- rv$data[, input$moderators, drop = FALSE]
      cor_matrix <- cor(mod_data[, sapply(mod_data, is.numeric), drop = FALSE])
      
      # VIF calculation if more than one numeric moderator
      numeric_mods <- names(which(sapply(mod_data, is.numeric)))
      if (length(numeric_mods) > 1) {
        vif_data <- car::vif(lm(as.formula(paste(input$effect_col, mod_formula)), data = rv$data))
        rv$diagnostics$vif <- vif_data
      }
      
      rv$diagnostics$cor_matrix <- cor_matrix
    }
    
    # Run meta-regression
    tryCatch({
      # Basic meta-regression model
      model <- rma(yi = rv$data[[input$effect_col]],
                  sei = rv$data[[input$se_col]],
                  mods = as.formula(mod_formula),
                  data = rv$data,
                  method = input$method,
                  test = "knha")
      
      rv$model <- model
      
      # Run permutation test
      rv$diagnostics$permutation <- try(permutest(model, iter = 1000))
      
      # Multimodel inference if multiple moderators
      if (length(input$moderators) > 1) {
        rv$diagnostics$multimodel <- try(
          dmetar::multimodel.inference(
            TE = input$effect_col,
            seTE = input$se_col,
            data = rv$data,
            predictors = input$moderators,
            method = input$method,
            test = "knha",
            eval.criterion = "AICc"
          )
        )
      }
      
      # Basic diagnostics
      rv$diagnostics$resid <- residuals(model)
      rv$diagnostics$fitted <- fitted(model)
      rv$diagnostics$cook.d <- cooks.distance(model)
      rv$diagnostics$hat <- hatvalues(model)
      rv$diagnostics$influence <- influence(model)
      
      # Additional model information
      rv$diagnostics$i2 <- model$I2
      rv$diagnostics$r2 <- model$R2
      
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
    
    # Basic model summary
    cat("META-REGRESSION ANALYSIS\n")
    cat("=======================\n\n")
    print(summary(rv$model))
    
    # Multicollinearity results
    if (!is.null(rv$diagnostics$vif)) {
      cat("\nVARIANCE INFLATION FACTORS\n")
      cat("=========================\n")
      print(rv$diagnostics$vif)
    }
    
    if (!is.null(rv$diagnostics$cor_matrix)) {
      cat("\nCORRELATION MATRIX OF MODERATORS\n")
      cat("===============================\n")
      print(round(rv$diagnostics$cor_matrix, 3))
    }
    
    # Permutation test results
    if (!is.null(rv$diagnostics$permutation) && !inherits(rv$diagnostics$permutation, "try-error")) {
      cat("\nPERMUTATION TEST RESULTS\n")
      cat("=======================\n")
      print(rv$diagnostics$permutation)
    }
    
    # Multimodel inference results
    if (!is.null(rv$diagnostics$multimodel) && !inherits(rv$diagnostics$multimodel, "try-error")) {
      cat("\nMULTIMODEL INFERENCE RESULTS\n")
      cat("==========================\n")
      print(rv$diagnostics$multimodel)
    }
    
    # Model fit statistics
    cat("\nMODEL FIT STATISTICS\n")
    cat("===================\n")
    cat(sprintf("I² = %.1f%%\n", rv$diagnostics$i2))
    if (!is.null(rv$diagnostics$r2)) {
      cat(sprintf("R² = %.1f%%\n", rv$diagnostics$r2))
    }
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
    
    # Create enhanced bubble plot
    bubble_data <- data.frame(
      x = rv$data[[input$bubble_mod]],
      y = rv$data[[input$effect_col]],
      se = rv$data[[input$se_col]],
      study = rv$data$study
    )
    
    # Calculate prediction interval
    pred <- predict(rv$model, newmods = bubble_data$x)
    
    # Create plot
    ggplot(bubble_data, aes(x = x, y = y)) +
      # Add confidence interval ribbon
      geom_ribbon(data = data.frame(x = bubble_data$x,
                                   fit = pred$pred,
                                   ci.lb = pred$ci.lb,
                                   ci.ub = pred$ci.ub),
                 aes(x = x, y = fit, ymin = ci.lb, ymax = ci.ub),
                 alpha = 0.2) +
      # Add regression line
      geom_line(aes(y = pred$pred), color = "blue", size = 1) +
      # Add points with size based on precision
      geom_point(aes(size = 1/se^2), alpha = 0.6) +
      # Add study labels
      geom_text_repel(aes(label = study), size = 3, box.padding = 0.5) +
      # Customize theme and labels
      theme_minimal() +
      labs(x = input$bubble_mod,
           y = "Effect Size",
           title = "Meta-Regression Bubble Plot",
           subtitle = paste("Moderator:", input$bubble_mod),
           caption = "Size of bubbles represents precision (1/SE²)") +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold"),
            plot.subtitle = element_text(face = "italic"))
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
  
  # Sample data download handler
  output$download_sample <- downloadHandler(
    filename = function() {
      "meta_reg_sample.csv"
    },
    content = function(file) {
      file.copy("sample_data/meta_reg.csv", file)
    }
  )

  # Return reactive results for use in main server
  return(reactive({ rv$model }))
  })
}
