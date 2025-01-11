# Survival Analysis Module

#' UI function for survival analysis tab
#' @param id Namespace ID for the module
#' @importFrom shiny NS
#' @export
survivalUI <- function(id) {
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
            downloadLink(ns("download_survival_sample"), "Download Sample Data"),
            tags$br(), tags$br(),
            checkboxInput(ns("header"), "File has header", TRUE),
            
            # Column selection
            selectInput(ns("study_col"), "Study Column", choices = NULL),
            selectInput(ns("hr_col"), "Hazard Ratio", choices = NULL),
            selectInput(ns("ci_lb_col"), "CI Lower Bound", choices = NULL),
            selectInput(ns("ci_ub_col"), "CI Upper Bound", choices = NULL),
            
            # Data preparation notes
            tags$div(
              class = "alert alert-info",
              tags$h4("Data Preparation Notes:"),
              tags$ul(
                tags$li("Your CSV file should contain the following columns:"),
                tags$li("study: Study identifier"),
                tags$li("hr: Hazard Ratio"),
                tags$li("ci.lb: Lower bound of 95% confidence interval"),
                tags$li("ci.ub: Upper bound of 95% confidence interval"),
                tags$li("Ensure all numeric values are properly formatted"),
                tags$li("Missing values should be coded as NA"),
                tags$li("Confidence intervals should be on the same scale as hazard ratios"),
                tags$li("All hazard ratios and confidence intervals should be positive numbers")
              )
            )
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
        
        selectInput(ns("model_type"), "Model Type",
                   choices = list(
                     "Fixed Effects" = "fixed",
                     "Random Effects" = "random"
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

#' Server function for survival analysis tab
#' @param id Namespace ID for the module
#' @param input Input objects
#' @param output Output objects
#' @param session Session object
#' @importFrom meta metagen forest funnel
#' @export
survivalServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values for storing data and results
    rv <- reactiveValues(
      data = data.frame(
        study = character(),
        hr = numeric(),
        ci.lb = numeric(),
        ci.ub = numeric(),
        stringsAsFactors = FALSE
      ),
      results = NULL
    )
    
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
        hr = NA,
        ci.lb = NA,
        ci.ub = NA,
        stringsAsFactors = FALSE
      )
      rv$data <- rbind(rv$data, new_row)
    })
    
    # Remove selected row
    observeEvent(input$remove_row, {
      if (!is.null(input$manual_input_table_rows_selected)) {
        rv$data <- rv$data[-input$manual_input_table_rows_selected, ]
      }
    })
    
    # File upload handling
    observeEvent(input$data_file, {
      req(input$data_file)
      
      tryCatch({
        # Read uploaded file
        data <- read.csv(input$data_file$datapath, header = input$header)
        
        # Update column selection choices
        updateSelectInput(session, "study_col", choices = names(data))
        updateSelectInput(session, "hr_col", choices = names(data))
        updateSelectInput(session, "ci_lb_col", choices = names(data))
        updateSelectInput(session, "ci_ub_col", choices = names(data))
        updateSelectInput(session, "subgroup_col", choices = c("", names(data)))
        
        # Set default column selections if sample data is detected
        if (all(c("study", "hr", "ci.lb", "ci.ub") %in% names(data))) {
          updateSelectInput(session, "study_col", selected = "study")
          updateSelectInput(session, "hr_col", selected = "hr")
          updateSelectInput(session, "ci_lb_col", selected = "ci.lb")
          updateSelectInput(session, "ci_ub_col", selected = "ci.ub")
        }
        
        # Store data
        rv$data <- data
      }, error = function(e) {
        showNotification(
          paste("Error in data processing:", e$message),
          type = "error"
        )
      })
    })
    
    # Process data and run analysis
    observeEvent(input$run_analysis, {
      req(rv$data)
      
      tryCatch({
        # Get selected columns
        selected_cols <- c(
          study = input$study_col,
          hr = input$hr_col,
          ci_lb = input$ci_lb_col,
          ci_ub = input$ci_ub_col
        )
        
        # Validate column selection
        validate(
          need(all(!is.null(selected_cols)), "Please select all required columns"),
          need(all(selected_cols %in% names(rv$data)), "Selected columns not found in data")
        )
        
        # Create analysis data frame
        analysis_data <- data.frame(
          study = rv$data[[selected_cols["study"]]],
          hr = as.numeric(rv$data[[selected_cols["hr"]]]),
          ci.lb = as.numeric(rv$data[[selected_cols["ci_lb"]]]),
          ci.ub = as.numeric(rv$data[[selected_cols["ci_ub"]]])
        )
        
        # Validate numeric values
        if (!all(sapply(analysis_data[c("hr", "ci.lb", "ci.ub")], is.numeric))) {
          stop("HR and confidence intervals must be numeric values")
        }
        
        # Validate positive values
        if (any(analysis_data[c("hr", "ci.lb", "ci.ub")] <= 0, na.rm = TRUE)) {
          stop("HR and confidence intervals must be positive values")
        }
        
        # Calculate log hazard ratios and standard errors
        analysis_data$TE <- log(analysis_data$hr)
        analysis_data$seTE <- (log(analysis_data$ci.ub) - log(analysis_data$ci.lb)) / (2 * 1.96)
        
        # Validate standard errors
        if (any(analysis_data$seTE <= 0, na.rm = TRUE)) {
          stop("Invalid confidence intervals detected")
        }
        
        # Fit meta-analysis model using metagen
        rv$results <- metagen(
          TE = analysis_data$TE,
          seTE = analysis_data$seTE,
          studlab = analysis_data$study,
          sm = "HR",
          method.tau = "REML",  # Always use REML
          comb.fixed = input$model_type == "fixed",
          comb.random = input$model_type == "random",
          prediction = TRUE,
          hakn = TRUE,
          level = input$conf_level,
          level.predict = input$conf_level
        )
      }, error = function(e) {
        showNotification(
          paste("Error in analysis:", e$message),
          type = "error"
        )
      })
    })
    
    # Display results
    output$results_table <- renderDT({
      req(rv$results)
      
      summary_data <- tryCatch({
        if (input$model_type == "random") {
          data.frame(
            Measure = c("Hazard Ratio", "95% CI", "95% PI", "p-value", "τ²"),
            Value = c(
              sprintf("%.3f", exp(rv$results$TE.random)),
              sprintf("%.3f to %.3f", 
                      exp(rv$results$lower.random),
                      exp(rv$results$upper.random)),
              sprintf("%.3f to %.3f",
                      exp(rv$results$lower.predict),
                      exp(rv$results$upper.predict)),
              sprintf("%.4f", rv$results$pval.random),
              sprintf("%.3f", rv$results$tau2)
            )
          )
        } else {
          data.frame(
            Measure = c("Hazard Ratio", "95% CI", "p-value"),
            Value = c(
              sprintf("%.3f", exp(rv$results$TE.fixed)),
              sprintf("%.3f to %.3f", 
                      exp(rv$results$lower.fixed),
                      exp(rv$results$upper.fixed)),
              sprintf("%.4f", rv$results$pval.fixed)
            )
          )
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
    
    # Generate forest plot
    output$forest_plot <- renderPlot({
      req(rv$results)
      forest(rv$results,
             leftlabs = c("Study", "HR", "95% CI"),
             label.left = "Favors Treatment",
             label.right = "Favors Control",
             fontsize = 10,
             prediction = input$model_type == "random",
             digits = 3)
    })
    
    # Create funnel plot with Egger's test
    create_funnel_plot <- function() {
      req(rv$results)
      
      # Create funnel plot
      funnel(rv$results,
             xlab = "Log Hazard Ratio",
             ylab = "Standard Error",
             pch = 19)
      
      # Add Egger's test only when there are enough studies
      if (!is.null(rv$results) && rv$results$k >= 10) {
        tryCatch({
          # Perform bias test
          bias_test <- metabias(rv$results, 
                              method.bias = "linreg",
                              k.min = 10)
          
          # Add test results to plot only if test was successful
          legend("topright",
                title = "Egger's Test",
                legend = c(
                  sprintf("p = %.3f", max(0.001, bias_test$p.value))
                ),
                bty = "n",
                cex = 0.8,
                text.col = "darkblue")
        }, error = function(e) {
          # Do nothing if test fails
        })
      }
    }
    
    # Generate funnel plot
    output$funnel_plot <- renderPlot({
      create_funnel_plot()
    })
    
    # Display heterogeneity table
    output$heterogeneity_table <- renderTable({
      req(rv$results)
      
      if (input$model_type == "fixed") {
        data.frame(
          Statistic = c("Q statistic", "Q p-value", "I²"),
          Value = c(
            sprintf("%.2f", rv$results$Q),
            sprintf("%.4f", rv$results$pval.Q),
            sprintf("%.1f%%", rv$results$I2)
          )
        )
      } else {
        data.frame(
          Statistic = c("Q statistic", "Q p-value", "τ²", "τ", "I²"),
          Value = c(
            sprintf("%.2f", rv$results$Q),
            sprintf("%.4f", rv$results$pval.Q),
            sprintf("%.3f", rv$results$tau2),
            sprintf("%.3f", sqrt(rv$results$tau2)),
            sprintf("%.1f%%", rv$results$I2)
          )
        )
      }
    })
    
    # Sample data download handler
    output$download_survival_sample <- downloadHandler(
      filename = function() {
        "survival_sample.csv"
      },
      content = function(file) {
        file.copy(file.path(getwd(), "sample_data", "survival.csv"), file)
      }
    )
    
    # Download handlers
    output$download_results <- downloadHandler(
      filename = function() {
        paste0("survival_meta_analysis_results_", format(Sys.time(), "%Y%m%d"), ".txt")
      },
      content = function(file) {
        req(rv$results)
        sink(file)
        print(rv$results)
        cat("\nPooled Hazard Ratio and 95% CI:\n")
        if (input$model_type == "random") {
          cat("HR:", format(exp(rv$results$TE.random), digits = 3))
          cat("\n95% CI:", format(exp(rv$results$lower.random), digits = 3),
              "-", format(exp(rv$results$upper.random), digits = 3))
          if (rv$results$prediction) {
            cat("\n95% PI:", format(exp(rv$results$lower.predict), digits = 3),
                "-", format(exp(rv$results$lower.predict), digits = 3))
          }
        } else {
          cat("HR:", format(exp(rv$results$TE.fixed), digits = 3))
          cat("\n95% CI:", format(exp(rv$results$lower.fixed), digits = 3),
              "-", format(exp(rv$results$upper.fixed), digits = 3))
        }
        sink()
      }
    )
    
    # Forest plot download handlers
    output$download_forest_pdf <- downloadHandler(
      filename = function() {
        paste0("forest_plot_", format(Sys.time(), "%Y%m%d"), ".pdf")
      },
      content = function(file) {
        req(rv$results)
        pdf(file)
        forest(rv$results,
               leftlabs = c("Study", "HR", "95% CI"),
               label.left = "Favors Treatment",
               label.right = "Favors Control",
               fontsize = 10,
               prediction = input$model_type == "random",
               digits = 3)
        dev.off()
      }
    )
    
    output$download_forest_svg <- downloadHandler(
      filename = function() {
        paste0("forest_plot_", format(Sys.time(), "%Y%m%d"), ".svg")
      },
      content = function(file) {
        req(rv$results)
        svg(file)
        forest(rv$results,
               leftlabs = c("Study", "HR", "95% CI"),
               label.left = "Favors Treatment",
               label.right = "Favors Control",
               fontsize = 10,
               prediction = input$model_type == "random",
               digits = 3)
        dev.off()
      }
    )
    
    # Funnel plot download handlers
    output$download_funnel_pdf <- downloadHandler(
      filename = function() {
        paste0("funnel_plot_", format(Sys.time(), "%Y%m%d"), ".pdf")
      },
      content = function(file) {
        pdf(file)
        create_funnel_plot()
        dev.off()
      }
    )
    
    output$download_funnel_svg <- downloadHandler(
      filename = function() {
        paste0("funnel_plot_", format(Sys.time(), "%Y%m%d"), ".svg")
      },
      content = function(file) {
        svg(file)
        create_funnel_plot()
        dev.off()
      }
    )
    
    # Create subgroup forest plot
    create_subgroup_plot <- function() {
      req(rv$results, input$subgroup_col, input$subgroup_col != "")
      
      # Add subgroup to analysis data
      analysis_data <- data.frame(
        study = rv$data[[input$study_col]],
        hr = as.numeric(rv$data[[input$hr_col]]),
        ci.lb = as.numeric(rv$data[[input$ci_lb_col]]),
        ci.ub = as.numeric(rv$data[[input$ci_ub_col]]),
        subgroup = rv$data[[input$subgroup_col]]
      )
      
      # Calculate log hazard ratios and standard errors
      analysis_data$TE <- log(analysis_data$hr)
      analysis_data$seTE <- (log(analysis_data$ci.ub) - log(analysis_data$ci.lb)) / (2 * 1.96)
      
      # Create subgroup model
      subgroup_model <- update(rv$results,
                             subgroup = analysis_data$subgroup,
                             tau.common = FALSE)
      
      # Create forest plot with subgroups
      forest(subgroup_model,
             leftcols = c("studlab", "TE", "seTE"),
             leftlabs = c("Study", "HR", "SE"),
             text.random = ifelse(input$model_type == "fixed", 
                                "Fixed Effect Model",
                                "Random Effects Model"),
             col.diamond = ifelse(input$model_type == "fixed", "black", "navy"),
             col.predict = "darkred",
             prediction = input$model_type == "random",
             label.left = "Favors Treatment",
             label.right = "Favors Control",
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
        paste0("subgroup_forest_plot_", format(Sys.time(), "%Y%m%d"), ".svg")
      },
      content = function(file) {
        svg(file)
        create_subgroup_plot()
        dev.off()
      }
    )
    
    output$download_subgroup_pdf <- downloadHandler(
      filename = function() {
        paste0("subgroup_forest_plot_", format(Sys.time(), "%Y%m%d"), ".pdf")
      },
      content = function(file) {
        pdf(file)
        create_subgroup_plot()
        dev.off()
      }
    )
  })
}
