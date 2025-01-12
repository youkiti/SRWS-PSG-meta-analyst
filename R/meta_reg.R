#------------------------------
# Shiny Module: meta_regUI
#------------------------------

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
      
      # Model Summary will be rendered dynamically
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

#------------------------------
# Shiny Module: meta_reg (Server)
#------------------------------

meta_reg <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    rv <- reactiveValues(
      data = NULL,
      model = NULL,
      model_summary = NULL,    # Store model summary
      diagnostics = NULL
    )
    
    #---------------------------
    # File upload handling
    #---------------------------
    observeEvent(input$data_file, {
      req(input$data_file)
      
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
          data[[col]] <- as.numeric(data[[col]] )
        }
        
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
        
        rv$data <- data
        
      }, error = function(e) {
        showNotification(paste("Error reading file:", e$message), type = "error")
      })
    })
    
    #---------------------------
    # Manual input table
    #---------------------------
    output$manual_input_table <- renderDT({
      if (is.null(rv$data)) {
        rv$data <- data.frame(
          study  = character(),
          effect = numeric(),
          se     = numeric(),
          mod1   = numeric(),
          mod2   = numeric(),
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
    
    # Remove row (not fully implemented in your code, but let's keep it here)
    observeEvent(input$remove_row, {
      # TODO: implement logic to remove selected row if needed
    })
    
    #---------------------------
    # Run meta-regression
    #---------------------------
    observeEvent(input$run_analysis, {
      req(rv$data, input$effect_col, input$se_col, length(input$moderators) > 0)
      
      # Create a copy
      analysis_data <- rv$data
      
      # Prepare formula components
      mod_terms <- c()
      
      # Center continuous moderators if requested
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
      
      # Add interaction terms
      if (length(input$interaction_terms) > 0) {
        interaction_pairs <- strsplit(input$interaction_terms, ":")
        for (pair in interaction_pairs) {
          if (length(pair) == 2) {
            term1 <- if (input$center_mods && is.numeric(analysis_data[[pair[1]]])) 
              paste0(pair[1], "_centered") else pair[1]
            term2 <- if (input$center_mods && is.numeric(analysis_data[[pair[2]]])) 
              paste0(pair[2], "_centered") else pair[2]
            mod_terms <- c(mod_terms, paste(term1, term2, sep=":"))
          }
        }
      }
      
      # Final formula
      mod_formula <- paste("~", paste(mod_terms, collapse = " + "))
      
      # Check multicollinearity if needed
      if (length(mod_terms) > 1) {
        # Extract numeric moderators
        numeric_mods <- mod_terms[sapply(mod_terms, function(x) {
          is.numeric(analysis_data[[sub("_centered$", "", x)]])
        })]
        
        if (length(numeric_mods) > 0) {
          mod_data <- as.data.frame(sapply(numeric_mods, function(x) analysis_data[[x]]))
          colnames(mod_data) <- numeric_mods
          rv$diagnostics$cor_matrix <- cor(mod_data, use = "pairwise.complete.obs")
          
          if (length(numeric_mods) > 1 && "multicollinearity" %in% input$advanced_options) {
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
      
      #---------------------------
      # Run meta-regression:
      #   Single moderator -> meta::metareg
      #   Multiple moderators -> metafor::rma
      #---------------------------
      tryCatch({
        if (length(input$moderators) == 1) {
          #------------------------------------
          # Single moderator (meta package)
          #------------------------------------
          
          m.gen <- metagen(
            data   = analysis_data, 
            TE     = analysis_data[[input$effect_col]], 
            seTE   = analysis_data[[input$se_col]],
            studlab= analysis_data[[input$study_col]],
            method.tau = input$method
          )
          
          model <- metareg(m.gen, formula = as.formula(mod_formula))
          
          rv$model <- model
          rv$model_summary <- summary(model)
          rv$diagnostics$resid <- as.numeric(residuals(model))
          rv$diagnostics$fitted<- as.numeric(fitted(model))
          rv$diagnostics$cook.d<- as.numeric(cooks.distance(model))
          rv$diagnostics$hat   <- as.numeric(hatvalues(model))
          
          rv$diagnostics$i2 <- as.numeric(model$I2)
          rv$diagnostics$r2 <- as.numeric(model$R2)
          
        } else {
          #------------------------------------
          # Multiple moderators (metafor)
          #------------------------------------
          
          model <- rma(
            yi   = analysis_data[[input$effect_col]],
            sei  = analysis_data[[input$se_col]],
            mods = as.formula(mod_formula),
            data = analysis_data,
            method = input$method,
            test = "knha"
          )
          
          rv$model <- model
          rv$model_summary <- summary(model)
          rv$diagnostics$resid <- as.numeric(residuals(model))
          rv$diagnostics$fitted<- as.numeric(fitted(model))
          rv$diagnostics$cook.d<- as.numeric(cooks.distance(model))
          rv$diagnostics$hat   <- as.numeric(hatvalues(model))
          
          rv$diagnostics$i2 <- as.numeric(model$I2)
          rv$diagnostics$r2 <- as.numeric(model$R2)
          
          # Permutation test
          if ("permutation" %in% input$advanced_options) {
            rv$diagnostics$permutation <- tryCatch({
              set.seed(123)
              perm_result <- permutest(model, iter = input$n_permutations)
              list(
                test      = perm_result$test,
                pval      = perm_result$pval,
                statistic = perm_result$statistic,
                df        = perm_result$df
              )
            }, error = function(e) {
              showNotification(paste("Permutation test error:", e$message), type = "warning")
              NULL
            })
          }
          
          # Multimodel inference
          if ("multimodel" %in% input$advanced_options) {
            rv$diagnostics$multimodel <- tryCatch({
              data_for_mm <- data.frame(
                TE   = as.numeric(analysis_data[[input$effect_col]]),
                seTE = as.numeric(analysis_data[[input$se_col]])
              )
              for (mod in unique(sub("_centered$", "", mod_terms))) {
                data_for_mm[[mod]] <- if (is.numeric(analysis_data[[mod]])) {
                  as.numeric(analysis_data[[mod]])
                } else {
                  analysis_data[[mod]]
                }
              }
              
              mm_result <- dmetar::multimodel.inference(
                TE     = "TE",
                seTE   = "seTE",
                data   = data_for_mm,
                predictors = unique(sub("_centered$", "", mod_terms)),
                method = input$method,
                test   = "knha",
                eval.criterion = "AICc"
              )
              
              list(
                coefficients = mm_result$coefficients,
                model_results= mm_result$model.results,
                best_model   = mm_result$best.model
              )
            }, error = function(e) {
              showNotification(paste("Multimodel inference error:", e$message), type = "warning")
              NULL
            })
          }
        }
        
        rv$diagnostics$influence <- influence(rv$model)
        
      }, error = function(e) {
        showNotification(
          paste("Error in meta-regression:", e$message),
          type = "error"
        )
      })
    })
    
    #---------------------------
    # Dynamically render UI outputs
    #---------------------------
    output$dynamic_outputs <- renderUI({
      if (length(input$moderators) == 1) {
        tagList(
          box(
            width = 12,
            title = "Model Summary",
            status = "success",
            verbatimTextOutput(ns("model_summary_single"))
          )
        )
      } else {
        tagList(
          box(
            width = 12,
            title = "Model Summary",
            status = "success",
            verbatimTextOutput(ns("model_summary_multi"))
          )
        )
      }
    })
    
    #---------------------------
    # Model summary outputs
    #---------------------------
    output$model_summary_single <- renderPrint({
      req(rv$model_summary)
      if (length(input$moderators) == 1) {
        print(rv$model_summary)
      }
    })
    
    output$model_summary_multi <- renderPrint({
      req(rv$model_summary)
      if (length(input$moderators) > 1) {
        
        summary_data <- data.frame(
          Measure = character(),
          Value   = character(),
          stringsAsFactors = FALSE
        )
        
        summary_data <- rbind(
          summary_data,
          data.frame(
            Measure = c("Method", 
                        "Number of Studies",
                        "Test of Moderators",
                        "Model p-value",
                        "τ²",
                        "I²",
                        "R²"),
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
        
        # VIF
        if (!is.null(rv$diagnostics$vif)) {
          vif_data <- data.frame(
            Measure = paste("VIF:", names(rv$diagnostics$vif)),
            Value   = sprintf("%.2f", rv$diagnostics$vif)
          )
          summary_data <- rbind(summary_data, vif_data)
        }
        
        # Permutation test
        if (!is.null(rv$diagnostics$permutation)) {
          perm_data <- data.frame(
            Measure = "Permutation Test p-value",
            Value   = sprintf("%.4f", rv$diagnostics$permutation$pval)
          )
          summary_data <- rbind(summary_data, perm_data)
        }
        
        datatable(summary_data,
                  options = list(dom = 't', pageLength = -1, ordering = FALSE, searching = FALSE),
                  rownames = FALSE) %>%
          formatStyle('Measure', fontWeight = 'bold')
      }
    })
    
    #---------------------------
    # Bubble plots
    #---------------------------
    output$dynamic_bubble_plots <- renderUI({
      req(rv$model)
      if (length(input$moderators) == 1) {
        box(
          width = 6,
          title = "Bubble Plot",
          status = "info",
          selectInput(ns("bubble_mod"), "Select Moderator", choices = input$moderators),
          plotOutput(ns("bubble_plot_single"))
        )
      } else {
        # Multiple bubble plots
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
    
    #---------------------------
    # Single moderator bubble plot
    #---------------------------
    output$bubble_plot_single <- renderPlot({
      req(rv$model, input$bubble_mod)
      if (length(input$moderators) == 1) {
        bubble_data <- data.frame(
          x     = rv$data[[input$bubble_mod]],
          y     = rv$data[[input$effect_col]],
          se    = rv$data[[input$se_col]],
          study = rv$data[[input$study_col]]
        )
        
        pred <- predict(rv$model, newmods = bubble_data$x)
        
        ggplot(bubble_data, aes(x = x, y = y)) +
          geom_ribbon(data = data.frame(
                         x = bubble_data$x,
                         fit = pred$pred,
                         ci.lb = pred$ci.lb,
                         ci.ub = pred$ci.ub),
                      aes(x = x, y = fit, ymin = ci.lb, ymax = ci.ub),
                      alpha = 0.2) +
          geom_line(aes(y = pred$pred), color = "blue", size = 1) +
          geom_point(aes(size = 1/se^2), alpha = 0.6) +
          ggrepel::geom_text_repel(aes(label = study), size = 3, box.padding = 0.5) +
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
    
    #---------------------------
    # Multiple moderators bubble plots
    #---------------------------
    observe({
      req(rv$model)
      if (length(input$moderators) > 1) {
        for (mod in input$moderators) {
          local({
            current_mod <- mod
            output[[paste0("bubble_plot_multi_", current_mod)]] <- renderPlot({
              req(rv$model)
              
              bubble_data <- data.frame(
                x     = rv$data[[current_mod]],
                y     = rv$data[[input$effect_col]],
                se    = rv$data[[input$se_col]],
                study = rv$data[[input$study_col]]
              )
              
              ggplot(bubble_data, aes(x = x, y = y)) +
                geom_point(aes(size = 1/se^2), alpha = 0.6) +
                geom_smooth(method = "lm", aes(weight = 1/se^2), se = TRUE, color = "blue") +
                ggrepel::geom_text_repel(aes(label = study), size = 3, box.padding = 0.5) +
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
    
    #---------------------------
    # Diagnostics
    #---------------------------
    output$resid_plot <- renderPlot({
      req(rv$diagnostics$fitted, rv$diagnostics$resid)
      plot_data <- data.frame(
        fitted = as.numeric(rv$diagnostics$fitted),
        resid  = as.numeric(rv$diagnostics$resid)
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
        cook  = as.numeric(rv$diagnostics$cook.d)
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
    
    #---------------------------
    # Sample data download
    #---------------------------
    output$download_sample <- downloadHandler(
      filename = function() { "meta_reg_sample.csv" },
      content  = function(file) {
        file.copy("sample_data/meta_reg.csv", file)
      }
    )
    
    # Return reactive model
    return(reactive({ rv$model }))
  })
}
