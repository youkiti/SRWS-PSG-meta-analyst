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
            downloadLink(ns("download_sample"), "Download Sample Data"),
            tags$br(), tags$br(),
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
                   choices = c("Risk Ratio" = "RR",
                             "Odds Ratio" = "OR",
                             "Risk Difference" = "RD")),
        
        # Analysis method selection
        uiOutput(ns("method_ui")),
        
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
        plotOutput(ns("forest_plot"), height = "600px"),
        div(style = "text-align: right;",
            downloadButton(ns("download_forest_svg"), "Download SVG"),
            downloadButton(ns("download_forest_pdf"), "Download PDF"))
      )
    ),
    
    fluidRow(
      box(
        width = 6,
        title = "Funnel Plot",
        status = "info",
        plotOutput(ns("funnel_plot")),
        div(style = "text-align: right;",
            downloadButton(ns("download_funnel_svg"), "Download SVG"),
            downloadButton(ns("download_funnel_pdf"), "Download PDF"))
      ),
      
      box(
        width = 6,
        title = "Heterogeneity",
        status = "warning",
        tableOutput(ns("heterogeneity_table"))
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "Subgroup Forest Plot",
        status = "primary",
        selectInput(ns("subgroup_col"), "Subgroup Column", choices = NULL),
        actionButton(ns("create_subgroup"), "Create Subgroup Plot", 
                    icon = icon("chart-bar"), 
                    class = "btn-primary"),
        plotOutput(ns("subgroup_forest_plot"), height = "600px"),
        div(style = "text-align: right;",
            downloadButton(ns("download_subgroup_svg"), "Download SVG"),
            downloadButton(ns("download_subgroup_pdf"), "Download PDF"))
      )
    )
  )
}

# Server Module
binary <- function(id) {
  moduleServer(id, function(input, output, session) {
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
      results = NULL,
      has_zeros = FALSE,
      analysis_data = NULL
    )
    
    # Dynamic method selection UI
    output$method_ui <- renderUI({
      if (rv$has_zeros) {
        selectInput(ns("method"), "Analysis Method",
                   choices = c(
                     "MH without correction" = "mh_no_correction",
                     "Logistic regression" = "logistic"
                   ))
      } else {
        selectInput(ns("method"), "Analysis Method",
                   choices = c(
                     "Fixed Effect" = "FE",
                     "Random Effects (REML)" = "REML"
                   ))
      }
    })
    
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
      updateSelectInput(session, "subgroup_col", choices = c("", names(data)))
      
      # Set default column selections if sample data is detected
      if (all(c("study", "e_treat", "n_treat", "e_ctrl", "n_ctrl") %in% names(data))) {
        updateSelectInput(session, "study_col", selected = "study")
        updateSelectInput(session, "e_treat_col", selected = "e_treat")
        updateSelectInput(session, "n_treat_col", selected = "n_treat")
        updateSelectInput(session, "e_ctrl_col", selected = "e_ctrl")
        updateSelectInput(session, "n_ctrl_col", selected = "n_ctrl")
      }
      
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
      
      # Get selected column names
      selected_cols <- c(
        e_treat = input$e_treat_col,
        n_treat = input$n_treat_col,
        e_ctrl = input$e_ctrl_col,
        n_ctrl = input$n_ctrl_col
      )
      
      # Validate data
      validate(
        need(all(!is.null(selected_cols)), "Please select all required columns"),
        need(all(selected_cols %in% names(rv$data)), "Selected columns not found in data")
      )
      
      # Create data frame with standardized column names
      analysis_data <- data.frame(
        study = rv$data[[input$study_col]],
        e_treat = as.numeric(rv$data[[selected_cols["e_treat"]]]),
        n_treat = as.numeric(rv$data[[selected_cols["n_treat"]]]),
        e_ctrl = as.numeric(rv$data[[selected_cols["e_ctrl"]]]),
        n_ctrl = as.numeric(rv$data[[selected_cols["n_ctrl"]]])
      )
      
      rv$analysis_data <- analysis_data
      
      # Check for zeros in the data
      has_zeros <- any(rv$analysis_data$e_treat == 0 | rv$analysis_data$e_ctrl == 0)
      rv$has_zeros <- has_zeros
      
      # Perform meta-analysis
      tryCatch({
        if (has_zeros && input$method == "logistic") {
          # Prepare data for logistic regression
          data_long <- tidyr::pivot_longer(
            rv$analysis_data,
            cols = c(e_treat, e_ctrl),
            names_to = "group",
            values_to = "events"
          ) %>%
          mutate(
            total = ifelse(group == "e_treat", n_treat, n_ctrl),
            group = factor(group, levels = c("e_ctrl", "e_treat")),
            study = factor(study)
          )
          
          # Fit logistic regression model
          model <- rma.glmm(events ~ group,
                           random = ~ 1 | study,
                           family = binomial(link = "logit"),
                           data = data_long)
          
          rv$results <- list(model = model, type = "logistic")
          
        } else {
          if (has_zeros) {
            # MH model without continuity correction
            model <- metabin(
              event.e = rv$analysis_data$e_treat,
              n.e = rv$analysis_data$n_treat,
              event.c = rv$analysis_data$e_ctrl,
              n.c = rv$analysis_data$n_ctrl,
              method = "MH",
              add = 0,
              allstudies = TRUE
            )
            rv$results <- list(model = model, type = "mh_no_correction")
          } else {
            # Standard meta-analysis using metabin
            model <- metabin(
              event.e = rv$analysis_data$e_treat,
              n.e = rv$analysis_data$n_treat,
              event.c = rv$analysis_data$e_ctrl,
              n.c = rv$analysis_data$n_ctrl,
              studlab = rv$analysis_data$study,
              sm = input$effect_measure,  # Effect measure (RR, OR, etc.)
              method.tau = "REML",  # REML method for τ²
              method.random.ci = "HK",  # Hartung-Knapp adjustment
              prediction = TRUE,  # Add prediction interval
              level = input$conf_level,
              level.predict = input$conf_level,
              method = ifelse(input$method == "FE", "Inverse", "Inverse"),
              fixed = input$method == "FE",
              random = input$method != "FE"
            )
            
            rv$results <- list(
              model = model,
              type = "standard"
            )
          }
        }
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
      
      summary_data <- tryCatch({
        if (rv$results$type == "logistic" && !is.null(rv$results$model)) {
          # Logistic regression results
          model <- rv$results$model
          if (!is.null(model$b) && !is.null(model$ci.lb) && !is.null(model$ci.ub) && !is.null(model$pval) &&
              length(model$b) >= 2 && length(model$ci.lb) >= 2 && length(model$ci.ub) >= 2 && length(model$pval) >= 2) {
            data.frame(
              Measure = c(
                switch(input$effect_measure,
                       "RR" = "Risk Ratio",
                       "OR" = "Odds Ratio",
                       "RD" = "Risk Difference"),
                "95% CI", "p-value"),
              Value = c(
                sprintf("%.3f", exp(as.numeric(model$b[2]))),  # intervention effect
                sprintf("%.3f to %.3f", 
                        exp(as.numeric(model$ci.lb[2])),
                        exp(as.numeric(model$ci.ub[2]))),
                sprintf("%.4f", as.numeric(model$pval[2]))
              )
            )
          } else {
            data.frame(
              Measure = "Status",
              Value = "Model estimation failed: incomplete results"
            )
          }
        } else if (rv$results$type == "mh_no_correction") {
          # MH model results
          model <- rv$results$model
          data.frame(
              Measure = c(
                switch(input$effect_measure,
                       "RR" = "Risk Ratio",
                       "OR" = "Odds Ratio",
                       "RD" = "Risk Difference"),
                "95% CI", "p-value"),
            Value = c(
                sprintf("%.3f", if(input$effect_measure == "RD") model$TE.fixed else exp(model$TE.fixed)),
                sprintf("%.3f to %.3f", 
                        if(input$effect_measure == "RD") model$lower.fixed else exp(model$lower.fixed),
                        if(input$effect_measure == "RD") model$upper.fixed else exp(model$upper.fixed)),
              sprintf("%.4f", model$pval.fixed)
            )
          )
        } else {
          # Standard meta-analysis results
          model <- rv$results$model
          
          # Create summary data frame
          if (input$method == "FE") {
            data.frame(
              Measure = c(
                switch(input$effect_measure,
                       "RR" = "Risk Ratio",
                       "OR" = "Odds Ratio",
                       "RD" = "Risk Difference"),
                "95% CI", "p-value"),
              Value = c(
                sprintf("%.3f", exp(model$TE.fixed)),
                sprintf("%.3f to %.3f", 
                        exp(model$lower.fixed),
                        exp(model$upper.fixed)),
                sprintf("%.4f", model$pval.fixed)
              )
            )
          } else {
            # Random effects results
            data.frame(
              Measure = c(
                switch(input$effect_measure,
                       "RR" = "Risk Ratio",
                       "OR" = "Odds Ratio",
                       "RD" = "Risk Difference"),
                "95% CI", "95% PI", "p-value", "τ²"),
              Value = c(
                sprintf("%.3f", if(input$effect_measure == "RD") model$TE.random else exp(model$TE.random)),
                sprintf("%.3f to %.3f", 
                        if(input$effect_measure == "RD") model$lower.random else exp(model$lower.random),
                        if(input$effect_measure == "RD") model$upper.random else exp(model$upper.random)),
                sprintf("%.3f to %.3f",
                        if(input$effect_measure == "RD") model$lower.predict else exp(model$lower.predict),
                        if(input$effect_measure == "RD") model$upper.predict else exp(model$upper.predict)),
                sprintf("%.4f", model$pval.random),
                sprintf("%.3f", model$tau2)
              )
            )
          }
        }
      }, error = function(e) {
        data.frame(
          Measure = "Status",
          Value = "Error calculating results"
        )
      })
      
      datatable(summary_data,
                options = list(dom = 't',
                             pageLength = 5,
                             searching = FALSE,
                             ordering = FALSE),
                rownames = FALSE)
    })
    
    # Create forest plot
    create_forest_plot <- function() {
      req(rv$results)
      
      model <- rv$results$model
      
      if (rv$results$type == "logistic") {
        forest(model,
          mlab = paste0("Random Effects Logistic Regression (k = ", model$k, ")"),
          slab = rv$analysis_data$study,
          ilab = cbind(
            paste(rv$analysis_data$e_treat, "/", rv$analysis_data$n_treat),
            paste(rv$analysis_data$e_ctrl, "/", rv$analysis_data$n_ctrl)
          ),
          ilab.xpos = c(-10, -6),
          ilab.pos = 2,
          header = TRUE,
          xlim = c(-16, 10),
          atransf = exp,
          refline = 1,
          level = input$conf_level,
          xlab = "Odds Ratio",
          cex = 0.8,
          showweights = TRUE
        )
        
        # Add column headers
        text(c(-10, -6), model$k + 2, 
             c("Treatment\nEvents/Total", "Control\nEvents/Total"),
             cex = 0.8, font = 2)
             
      } else if (rv$results$type == "mh_no_correction") {
        forest(model,
          mlab = paste0("Mantel-Haenszel Model (k = ", model$k, ")"),
          slab = rv$analysis_data$study,
          ilab = cbind(
            paste(rv$analysis_data$e_treat, "/", rv$analysis_data$n_treat),
            paste(rv$analysis_data$e_ctrl, "/", rv$analysis_data$n_ctrl)
          ),
          ilab.xpos = c(-10, -6),
          ilab.pos = 2,
          header = TRUE,
          xlim = c(-16, 10),
          atransf = exp,
          refline = 1,
          level = input$conf_level,
          xlab = "Odds Ratio",
          cex = 0.8,
          showweights = TRUE
        )
        
        # Add column headers
        text(c(-10, -6), model$k + 2,
             c("Treatment\nEvents/Total", "Control\nEvents/Total"),
             cex = 0.8, font = 2)
             
      } else {
        # Forest plot for meta-analysis using meta package
        forest(model,
          layout = "RevMan5",
          prediction = input$method != "FE"
        )
        
        # Add heterogeneity statistics
        if (input$method != "FE") {
          text(-16, -2, pos = 4, cex = 0.8, font = 3, bquote(paste(
            "Heterogeneity: ",
            "Q = ", .(sprintf("%.2f", model$Q)), ", ",
            "df = ", .(model$k - 1), ", ",  # あるいは model$df.Q が定義されていればそちらを使う
            "p ", .(sprintf("%.3f", model$pval.Q)), "; ",
            I^2, " = ", .(sprintf("%.1f", model$I2)), "%, ",
            tau^2, " = ", .(sprintf("%.3f", model$tau2))
          )))  # あるいは model$tau2 が定義されていればそちらを使う
        }
      }
    }
    
    # Create funnel plot
    create_funnel_plot <- function() {
      req(rv$results)
      
      if (rv$results$type == "logistic") {
        # Create an empty plot first
        plot.new()
        plot.window(xlim = c(0, 1), ylim = c(0, 1))
        text(0.5, 0.5, "Funnel plot not applicable\nfor logistic regression model",
             cex = 1.2, col = "darkgray")
      } else {
        # Create funnel plot
        funnel(rv$results$model)
        
        # Add Egger's test
        if (!is.null(rv$results$model)) {
          # Get number of studies
          k <- if (rv$results$type == "standard") {
            rv$results$model$k
          } else if (rv$results$type == "mh_no_correction") {
            length(rv$results$model$TE)
          } else {
            0
          }
          
          if (k >= 3) {  # Minimum studies needed for Egger's test
            tryCatch({
              # For binary outcomes, convert to metagen object first
              if (rv$results$type == "standard") {
                # Create metagen object from binary data
                m.gen <- metagen(TE = rv$results$model$TE,
                               seTE = rv$results$model$seTE,
                               studlab = rv$results$model$studlab,
                               sm = input$effect_measure,
                               fixed = input$method == "FE",
                               random = input$method != "FE",
                               method.tau = "REML",
                               method.random.ci = "HK")
                
                # Perform bias test on metagen object
                bias_test <- metabias(m.gen, 
                                    method.bias = "linreg",
                                    k.min = 3)
                
                # Add test results to plot
                legend("topright",
                      title = "Egger's Test",
                      legend = c(
                        sprintf("p = %.3f", max(0.001, bias_test$p.value))
                      ),
                      bty = "n",
                      cex = 0.8,
                      text.col = "darkblue")
              } else {
                # For non-standard models (e.g., MH without correction),
                # use standard Egger's test as fallback
                bias_test <- metabias(rv$results$model,
                                    method.bias = "linreg")
                
                legend("topright",
                      title = "Egger's Test",
                      legend = c(
                        sprintf("p = %.3f", max(0.001, bias_test$p.value))
                      ),
                      bty = "n",
                      cex = 0.8,
                      text.col = "darkblue")
              }
            }, error = function(e) {
              # More informative error message
              legend("topright",
                    title = "Publication Bias Test",
                    legend = paste("Could not calculate:",
                                 "insufficient data or",
                                 "variance structure"),
                    bty = "n",
                    cex = 0.8,
                    text.col = "darkred")
            })
          } else {
            # Show message when insufficient studies
            legend("topright",
                   title = "Egger's Test",
                   legend = "Not applicable\n(< 3 studies)",
                   bty = "n",
                   cex = 0.8,
                   text.col = "darkred")
          }
        }
      }
    }
    
    # Render plots
    output$forest_plot <- renderPlot({
      create_forest_plot()
    })
    
    output$funnel_plot <- renderPlot({
      create_funnel_plot()
    })
    
    # Download handlers for Forest plot
    output$download_forest_svg <- downloadHandler(
      filename = function() {
        paste0("forest_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".svg")
      },
      content = function(file) {
        svg(file)
        create_forest_plot()
        dev.off()
      }
    )
    
    output$download_forest_pdf <- downloadHandler(
      filename = function() {
        paste0("forest_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
      },
      content = function(file) {
        pdf(file)
        create_forest_plot()
        dev.off()
      }
    )
    
    # Download handlers for Funnel plot
    output$download_funnel_svg <- downloadHandler(
      filename = function() {
        paste0("funnel_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".svg")
      },
      content = function(file) {
        svg(file)
        create_funnel_plot()
        dev.off()
      }
    )
    
    output$download_funnel_pdf <- downloadHandler(
      filename = function() {
        paste0("funnel_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
      },
      content = function(file) {
        pdf(file)
        create_funnel_plot()
        dev.off()
      }
    )
    
    # Render heterogeneity table
    output$heterogeneity_table <- renderTable({
      req(rv$results)
      
      tryCatch({
        if (rv$results$type == "logistic" && !is.null(rv$results$model)) {
          if (!is.null(rv$results$model$tau2) && is.numeric(rv$results$model$tau2)) {
            data.frame(
              Statistic = c("Q statistic", "τ²", "τ", "I²"),
              Value = c(
                sprintf("%.2f", rv$results$model$QE),
                sprintf("%.3f", rv$results$model$tau2),
                sprintf("%.3f", sqrt(rv$results$model$tau2)),
                sprintf("%.1f%%", rv$results$model$I2)
              )
            )
          } else {
            data.frame(
              Statistic = c("Status"),
              Value = c("Heterogeneity calculation failed")
            )
          }
        } else if (rv$results$type == "mh_no_correction") {
          data.frame(
            Statistic = c("Q statistic", "Q p-value", "I²"),
            Value = c(
              sprintf("%.2f", rv$results$model$Q),
              sprintf("%.4f", rv$results$model$pval.Q),
              sprintf("%.1f%%", rv$results$model$I2)
            )
          )
        } else {
          if (input$method == "FE") {
            data.frame(
              Statistic = c("Q statistic", "Q p-value", "I²"),
              Value = c(
                sprintf("%.2f", rv$results$model$Q),
                sprintf("%.4f", rv$results$model$pval.Q),
                sprintf("%.1f%%", rv$results$model$I2)
              )
            )
          } else {
            data.frame(
              Statistic = c("Q statistic", "Q p-value", "τ²", "τ", "I²"),
              Value = c(
                sprintf("%.2f", rv$results$model$Q),
                sprintf("%.4f", rv$results$model$pval.Q),
                sprintf("%.3f", rv$results$model$tau2),
                sprintf("%.3f", sqrt(rv$results$model$tau2)),
                sprintf("%.1f%%", rv$results$model$I2)
              )
            )
          }
        }
      }, error = function(e) {
        data.frame(
          Statistic = c("Status"),
          Value = c("Error calculating heterogeneity statistics")
        )
      })
    })
    
    # Sample data download handler
    output$download_sample <- downloadHandler(
      filename = function() {
        "binary_sample.csv"
      },
      content = function(file) {
        file.copy("sample_data/binary.csv", file)
      }
    )
    
    # Create subgroup forest plot
    create_subgroup_plot <- function() {
      req(rv$results, input$subgroup_col, input$subgroup_col != "")
      
      # Add subgroup to analysis data
      analysis_data <- rv$analysis_data
      analysis_data$subgroup <- rv$data[[input$subgroup_col]]
      
      # Create subgroup model
      subgroup_model <- update(rv$results$model,
                             subgroup = analysis_data$subgroup,
                             tau.common = FALSE)
      
      # Create forest plot with subgroups
      forest(subgroup_model,
             leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c"),
             leftlabs = c("Study", "Events", "Total", "Events", "Total"),
             text.random = ifelse(input$method == "FE", 
                                "Fixed Effect Model",
                                "Random Effects Model (REML + HK)"),
             col.diamond = ifelse(input$method == "FE", "black", "navy"),
             col.predict = "darkred",
             prediction = input$method != "FE",
             label.e = "Treatment",
             label.c = "Control",
             label.left = paste("Favors", "Control"),
             label.right = paste("Favors", "Treatment"),
             subgroup = TRUE,
             test.subgroup = TRUE,
             subgroup.name = input$subgroup_col)
    }
    
    # Render subgroup forest plot
    observeEvent(input$create_subgroup, {
      output$subgroup_forest_plot <- renderPlot({
        create_subgroup_plot()
      })
    })
    
    # Download handlers for Subgroup forest plot
    output$download_subgroup_svg <- downloadHandler(
      filename = function() {
        paste0("subgroup_forest_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".svg")
      },
      content = function(file) {
        svg(file)
        create_subgroup_plot()
        dev.off()
      }
    )
    
    output$download_subgroup_pdf <- downloadHandler(
      filename = function() {
        paste0("subgroup_forest_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
      },
      content = function(file) {
        pdf(file)
        create_subgroup_plot()
        dev.off()
      }
    )
    
    # Return reactive results for use in main server
    return(reactive({ rv$results }))
  })
}
