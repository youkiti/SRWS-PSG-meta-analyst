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
            selectInput(ns("effect_col"), "Effect Size Column", choices = NULL),
            selectInput(ns("se_col"), "Standard Error Column", choices = NULL),
            
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
            actionButton(ns("remove_row"), "Remove Selected Row", icon = icon("minus"))
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
        
        # Advanced analysis options
        checkboxGroupInput(ns("advanced_options"), "Advanced Options",
                         choices = c(
                           "Run Permutation Test" = "permutation",
                           "Multimodel Inference" = "multimodel",
                           "Check Multicollinearity" = "multicollinearity"
                         )),
        
        # Interaction terms
        selectizeInput(ns("interaction_terms"), "Add Interaction Terms",
                      choices = NULL,
                      multiple = TRUE,
                      options = list(
                        placeholder = "Select variables for interaction",
                        plugins = list("remove_button")
                      )),
        
        # Number of permutations
        conditionalPanel(
          condition = "input.advanced_options.includes('permutation')",
          ns = ns,
          numericInput(ns("n_permutations"), "Number of Permutations",
                      value = 1000, min = 100, max = 10000, step = 100)
        ),
        
        # Run Analysis Button
        actionButton(ns("run_analysis"), "Run Analysis",
                    icon = icon("play"), 
                    class = "btn-primary")
      ),
      
      # Model Summary and Moderator Effects will be rendered dynamically based on the number of moderators
      uiOutput(ns("dynamic_outputs"))
    ),
    
    fluidRow(
      # Bubble Plots will be rendered dynamically based on the number of moderators
      uiOutput(ns("dynamic_bubble_plots"))
    ),
    
    fluidRow(
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
      model_summary = NULL, # Store model summary
      moderator_effects = NULL, # Store moderator effects
      diagnostics = NULL
    )
    
    # File upload handling
    observeEvent(input$data_file, {
      req(input$data_file)
      
      # Read uploaded file with type checking
      tryCatch({
        data <- read.csv(input$data_file$datapath, 
                         header = input$header,
                         stringsAsFactors = FALSE,
                         na.strings = c("NA", "", ".", "N/A"))
        
        # Convert numeric columns
        numeric_cols <- sapply(data, function(x) {
          all(grepl("^\\s*-?\\d*\\.?\\d+\\s*$", na.omit(x)) | is.na(x))
        })
        
        for(col in names(data)[numeric_cols]) {
          data[[col]] <- as.numeric(data[[col]])
        }
        
        # Check for required numeric columns
        if (any(is.na(data))) {
          showNotification("Warning: Some values could not be converted to numbers.", type = "warning")
        }
        
        # Set default column selections if sample data is detected
        if (all(c("study", "effect", "se") %in% names(data))) {
          updateSelectInput(session, "study_col", selected = "study")
          updateSelectInput(session, "effect_col", selected = "effect")
          updateSelectInput(session, "se_col", selected = "se")
        }
        
        # Update column selection choices
        updateSelectInput(session, "study_col", choices = names(data))
        updateSelectInput(session, "effect_col", choices = names(data)[numeric_cols])
        updateSelectInput(session, "se_col", choices = names(data)[numeric_cols])
        updateSelectizeInput(session, "moderators", choices = names(data))
        
        # Store data
        rv$data <- data
        
      }, error = function(e) {
        showNotification(paste("Error reading file:", e$message), type = "error")
      })
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
      
      # Create a copy of the data for modifications
      analysis_data <- rv$data
      
      # Prepare formula components
      mod_terms <- c()
      
      # Center continuous moderators if requested and build formula
      if (input$center_mods) {
        for (mod in input$moderators) {
          if (is.numeric(analysis_data[[mod]])) {
            mod_name <- paste0(mod, "_centered")
            analysis_data[[mod_name]] <- scale(analysis_data[[mod]], center = TRUE, scale = FALSE)
            mod_terms <- c(mod_terms, mod_name)
          } else {
            mod_terms <- c(mod_terms, mod)
          }
        }
      } else {
        mod_terms <- input$moderators
      }
      
      # Add interaction terms if specified
      if (length(input$interaction_terms) > 0) {
        interaction_pairs <- strsplit(input$interaction_terms, ":")
        for (pair in interaction_pairs) {
          if (length(pair) == 2) {
            # Create interaction term
            term1 <- if(input$center_mods && is.numeric(analysis_data[[pair[1]]])) paste0(pair[1], "_centered") else pair[1]
            term2 <- if(input$center_mods && is.numeric(analysis_data[[pair[2]]])) paste0(pair[2], "_centered") else pair[2]
            mod_terms <- c(mod_terms, paste(term1, term2, sep=":"))
          }
        }
      }
      
      # Prepare final formula
      mod_formula <- paste("~", paste(mod_terms, collapse = " + "))
      
      # Check multicollinearity for multiple moderators
      if (length(mod_terms) > 1) {
        # Extract numeric moderators for correlation matrix
        numeric_mods <- mod_terms[sapply(mod_terms, function(x) {
          is.numeric(analysis_data[[sub("_centered$", "", x)]])
        })]
        
        if (length(numeric_mods) > 0) {
          mod_data <- as.data.frame(sapply(numeric_mods, function(x) analysis_data[[x]]))
          colnames(mod_data) <- numeric_mods
          rv$diagnostics$cor_matrix <- cor(mod_data)
          
          # VIF calculation if more than one numeric moderator
          if (length(numeric_mods) > 1) {
            tryCatch({
              vif_formula <- as.formula(paste(input$effect_col, mod_formula))
              vif_model <- lm(vif_formula, data = analysis_data)
              rv$diagnostics$vif <- car::vif(vif_model)
            }, error = function(e) {
              rv$diagnostics$vif <- NULL
              showNotification(paste("VIF calculation error:", e$message), type = "warning")
            })
          }
        }
      }
      
      # Run meta-regression based on the number of moderators selected
      tryCatch({
        if (length(input$moderators) == 1) {
          # Single moderator: Use metareg from meta package
          
          # Create a meta-analysis object using metagen
          m.gen <- metagen(data = analysis_data, 
                           TE = analysis_data[[input$effect_col]], 
                           seTE = analysis_data[[input$se_col]],
                           studlab = analysis_data$study,
                           method.tau = input$method)
          
          # Perform meta-regression with single moderator
          model <- metareg(m.gen, 
                            formula = as.formula(mod_formula))
          
          rv$model <- model
          rv$model_summary <- summary(model)
          rv$moderator_effects <- coef(summary(model))
          
          # Basic diagnostics
          rv$diagnostics$resid <- as.numeric(residuals(model))
          rv$diagnostics$fitted <- as.numeric(fitted(model))
          rv$diagnostics$cook.d <- as.numeric(cooks.distance(model))
          rv$diagnostics$hat <- as.numeric(hatvalues(model))
          
          # Additional model information
          rv$diagnostics$i2 <- as.numeric(model$I2)
          rv$diagnostics$r2 <- as.numeric(model$R2)
          
        } else {
          # Multiple moderators: Use rma from metafor package
          model <- rma(yi = analysis_data[[input$effect_col]],
                       sei = analysis_data[[input$se_col]],
                       mods = as.formula(mod_formula),
                       data = analysis_data,
                       method = input$method,
                       test = "knha")
          
          rv$model <- model
          rv$model_summary <- summary(model)
          rv$moderator_effects <- coef(summary(model))
          
          # Basic diagnostics
          rv$diagnostics$resid <- as.numeric(residuals(model))
          rv$diagnostics$fitted <- as.numeric(fitted(model))
          rv$diagnostics$cook.d <- as.numeric(cooks.distance(model))
          rv$diagnostics$hat <- as.numeric(hatvalues(model))
          
          # Additional model information
          rv$diagnostics$i2 <- as.numeric(model$I2)
          rv$diagnostics$r2 <- as.numeric(model$R2)
          
          # Run permutation test if requested
          if ("permutation" %in% input$advanced_options) {
            rv$diagnostics$permutation <- tryCatch({
              set.seed(123) # for reproducibility
              perm_result <- permutest(model, iter = input$n_permutations)
              # Store only necessary components
              list(
                test = perm_result$test,
                pval = perm_result$pval,
                statistic = perm_result$statistic,
                df = perm_result$df
              )
            }, error = function(e) {
              showNotification(paste("Permutation test error:", e$message), type = "warning")
              NULL
            })
          }
          
          # Multimodel inference if requested
          if ("multimodel" %in% input$advanced_options) {
            rv$diagnostics$multimodel <- tryCatch({
              # Prepare data for multimodel inference
              data_for_mm <- data.frame(
                TE = as.numeric(analysis_data[[input$effect_col]]),
                seTE = as.numeric(analysis_data[[input$se_col]])
              )
              
              # Add moderators, ensuring numeric conversion where appropriate
              for (mod in unique(sub("_centered$", "", mod_terms))) {
                data_for_mm[[mod]] <- if(is.numeric(analysis_data[[mod]])) {
                  as.numeric(analysis_data[[mod]])
                } else {
                  analysis_data[[mod]]
                }
              }
              
              mm_result <- dmetar::multimodel.inference(
                TE = "TE",
                seTE = "seTE",
                data = data_for_mm,
                predictors = unique(sub("_centered$", "", mod_terms)),
                method = input$method,
                test = "knha",
                eval.criterion = "AICc"
              )
              
              # Store only necessary components
              list(
                coefficients = mm_result$coefficients,
                model_results = mm_result$model.results,
                best_model = mm_result$best.model
              )
            }, error = function(e) {
              showNotification(paste("Multimodel inference error:", e$message), type = "warning")
              NULL
            })
          }
        }
        
        # Store influence analysis results if needed
        rv$diagnostics$influence <- influence(model)
        
      }, error = function(e) {
        showNotification(
          paste("Error in meta-regression:", e$message),
          type = "error"
        )
      })
    })
    
    # Dynamically render Model Summary and Moderator Effects based on the number of moderators
    output$dynamic_outputs <- renderUI({
      if (length(input$moderators) == 1) {
        # Single moderator: Display metareg summary and effects
        tagList(
          box(
            width = 6,
            title = "Model Summary",
            status = "success",
            verbatimTextOutput(ns("model_summary_single"))
          ),
          box(
            width = 6,
            title = "Moderator Effects",
            status = "primary",
            DTOutput(ns("moderator_effects_single"))
          )
        )
      } else {
        # Multiple moderators: Display rma summary and effects
        tagList(
          box(
            width = 6,
            title = "Model Summary",
            status = "success",
            verbatimTextOutput(ns("model_summary_multi"))
          ),
          box(
            width = 6,
            title = "Moderator Effects",
            status = "primary",
            DTOutput(ns("moderator_effects_multi"))
          )
        )
      }
    })
    
    # Render model summary for single moderator
    output$model_summary_single <- renderPrint({
      req(rv$model_summary)
      if(length(input$moderators) == 1){
        print(rv$model_summary)
      }
    })
    
    # Render moderator effects for single moderator
    output$moderator_effects_single <- renderDT({
      req(rv$moderator_effects)
      if(length(input$moderators) == 1){
        
        effects_df <- data.frame(
          Moderator = rownames(rv$moderator_effects),
          Estimate = rv$moderator_effects[, "estimate"],
          SE = rv$moderator_effects[, "se"],
          Z = rv$moderator_effects[, "zval"],
          `P-value` = rv$moderator_effects[, "pval"],
          `Lower CI` = rv$moderator_effects[, "ci.lb"],
          `Upper CI` = rv$moderator_effects[, "ci.ub"]
        )
        
        datatable(effects_df,
                  options = list(pageLength = 10,
                                 searching = FALSE),
                  rownames = FALSE) %>%
          formatRound(columns = c("Estimate", "SE", "Z", "P-value", "Lower CI", "Upper CI"),
                      digits = 3)
      }
    })
    
    # Render model summary for multiple moderators
    output$model_summary_multi <- renderPrint({
      req(rv$model_summary)
      if(length(input$moderators) > 1){
        
        # Create summary data frame
        summary_data <- data.frame(
          Measure = character(),
          Value = character(),
          stringsAsFactors = FALSE
        )
        
        # Add basic model results
        summary_data <- rbind(
          summary_data,
          data.frame(
            Measure = c(
              "Method",
              "Number of Studies",
              "Test of Moderators",
              "Model p-value",
              "τ²",
              "I²",
              "R²"
            ),
            Value = c(
              input$method,
              as.character(rv$model$k),
              sprintf("F(%d, %d) = %.2f", 
                      rv$model_summary$dfs[1], 
                      rv$model_summary$dfs[2], 
                      rv$model_summary$QM),
              sprintf("%.4f", rv$model_summary$QMp),
              sprintf("%.3f", rv$model$tau2),
              sprintf("%.1f%%", rv$diagnostics$i2),
              sprintf("%.1f%%", rv$diagnostics$r2)
            )
          )
        )
        
        # Add VIF if available
        if (!is.null(rv$diagnostics$vif)) {
          vif_data <- data.frame(
            Measure = paste("VIF:", names(rv$diagnostics$vif)),
            Value = sprintf("%.2f", rv$diagnostics$vif)
          )
          summary_data <- rbind(summary_data, vif_data)
        }
        
        # Add permutation test results if available
        if (!is.null(rv$diagnostics$permutation)) {
          perm_data <- data.frame(
            Measure = "Permutation Test p-value",
            Value = sprintf("%.4f", rv$diagnostics$permutation$pval)
          )
          summary_data <- rbind(summary_data, perm_data)
        }
        
        # Create table
        datatable(summary_data,
                  options = list(
                    dom = 't',
                    pageLength = -1,
                    ordering = FALSE,
                    searching = FALSE
                  ),
                  rownames = FALSE) %>%
          formatStyle(
            'Measure',
            fontWeight = 'bold'
          )
      }
    })
    
    # Render moderator effects for multiple moderators
    output$moderator_effects_multi <- renderDT({
      req(rv$moderator_effects)
      
      if(length(input$moderators) > 1){
        effects_df <- data.frame(
          Moderator = rownames(rv$moderator_effects),
          Estimate = rv$moderator_effects[, "estimate"],
          SE = rv$moderator_effects[, "se"],
          Z = rv$moderator_effects[, "zval"],
          `P-value` = rv$moderator_effects[, "pval"],
          `Lower CI` = rv$moderator_effects[, "ci.lb"],
          `Upper CI` = rv$moderator_effects[, "ci.ub"]
        )
        
        datatable(effects_df,
                  options = list(pageLength = 10,
                                 searching = FALSE),
                  rownames = FALSE) %>%
          formatRound(columns = c("Estimate", "SE", "Z", "P-value", "Lower CI", "Upper CI"),
                      digits = 3)
      }
    })
    
    # Dynamically render Bubble Plots based on the number of moderators
    output$dynamic_bubble_plots <- renderUI({
      req(rv$model)
      
      if (length(input$moderators) == 1) {
        # Single moderator: Create a single bubble plot
        box(
          width = 6,
          title = "Bubble Plot",
          status = "info",
          selectInput(ns("bubble_mod"), "Select Moderator", choices = input$moderators),
          plotOutput(ns("bubble_plot_single"))
        )
      } else {
        # Multiple moderators: Create multiple bubble plots, one for each moderator
        lapply(input$moderators, function(mod) {
          box(
            width = 6,
            title = paste("Bubble Plot - Moderator:", mod),
            status = "info",
            plotOutput(ns(paste0("bubble_plot_multi_", mod)))
          )
        })
      }
    })
    
    # Render bubble plot for single moderator
    output$bubble_plot_single <- renderPlot({
      req(rv$model, input$bubble_mod)
      
      if (length(input$moderators) == 1) {
        
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
          # Add study labels (using ggrepel)
          ggrepel::geom_text_repel(aes(label = study), size = 3, box.padding = 0.5) +
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
      }
    })
    
    # Render bubble plots for multiple moderators
    observe({
      req(rv$model)
      if (length(input$moderators) > 1) {
        for (mod in input$moderators) {
          local({
            current_mod <- mod
            output[[paste0("bubble_plot_multi_", current_mod)]] <- renderPlot({
              req(rv$model)
              
              # Prepare data for the bubble plot
              bubble_data <- data.frame(
                x = rv$data[[current_mod]],
                y = rv$data[[input$effect_col]],
                se = rv$data[[input$se_col]],
                study = rv$data$study
              )
              
              # Generate the bubble plot using ggplot2
              ggplot(bubble_data, aes(x = x, y = y)) +
                geom_point(aes(size = 1/se^2), alpha = 0.6) + # Size represents precision
                geom_smooth(method = "lm", aes(weight = 1/se^2), se = TRUE, color = "blue") + # Weighted regression line
                ggrepel::geom_text_repel(aes(label = study), size = 3, box.padding = 0.5) + # Add study labels
                theme_minimal() +
                labs(x = current_mod,
                     y = "Effect Size",
                     title = paste("Bubble Plot - Moderator:", current_mod),
                     caption = "Size of bubbles represents precision (1/SE²)") +
                theme(legend.position = "none",
                      plot.title = element_text(face = "bold"))
            })
          })
        }
      }
    })
    
    # Render diagnostic plots
    output$resid_plot <- renderPlot({
      req(rv$diagnostics$fitted, rv$diagnostics$resid)
      
      plot_data <- data.frame(
        fitted = as.numeric(rv$diagnostics$fitted),
        resid = as.numeric(rv$diagnostics$resid)
      )
      
      ggplot(plot_data, aes(x = fitted, y = resid)) +
        geom_point() +
        geom_hline(yintercept = 0, linetype = "dashed") +
        geom_smooth(method = "loess", se = FALSE) +
        theme_minimal() +
        labs(x = "Fitted Values",
             y = "Residuals",
             title = "Residuals vs. Fitted Values")
    })
    
    output$qq_plot <- renderPlot({
      req(rv$diagnostics$resid)
      
      plot_data <- data.frame(
        resid = as.numeric(rv$diagnostics$resid)
      )
      
      ggplot(plot_data, aes(sample = resid)) +
        stat_qq() +
        stat_qq_line() +
        theme_minimal() +
        labs(x = "Theoretical Quantiles",
             y = "Sample Quantiles",
             title = "Normal Q-Q Plot")
    })
    
    output$cook_plot <- renderPlot({
      req(rv$diagnostics$cook.d)
      
      plot_data <- data.frame(
        index = 1:length(rv$diagnostics$cook.d),
        cook = as.numeric(rv$diagnostics$cook.d)
      )
      
      ggplot(plot_data, aes(x = index, y = cook)) +
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
