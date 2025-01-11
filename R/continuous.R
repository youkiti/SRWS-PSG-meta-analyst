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
            selectInput(ns("n_treat_col"), "Total (Treatment)", choices = NULL),
            selectInput(ns("mean_ctrl_col"), "Mean (Control)", choices = NULL),
            selectInput(ns("sd_ctrl_col"), "SD (Control)", choices = NULL),
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
                   choices = c("Mean Difference" = "MD",
                             "Standardized Mean Difference" = "SMD")),
        
        # Analysis method selection
        selectInput(ns("method"), "Analysis Method",
                   choices = c(
                     "Fixed Effect" = "FE",
                     "Random Effects (REML)" = "REML"
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
      results = NULL,
      analysis_data = NULL
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
      updateSelectInput(session, "subgroup_col", choices = c("", names(data)))
      
      # Set default column selections if sample data is detected
      if (all(c("study", "mean_treat", "sd_treat", "n_treat", "mean_ctrl", "sd_ctrl", "n_ctrl") %in% names(data))) {
        updateSelectInput(session, "study_col", selected = "study")
        updateSelectInput(session, "mean_treat_col", selected = "mean_treat")
        updateSelectInput(session, "sd_treat_col", selected = "sd_treat")
        updateSelectInput(session, "n_treat_col", selected = "n_treat")
        updateSelectInput(session, "mean_ctrl_col", selected = "mean_ctrl")
        updateSelectInput(session, "sd_ctrl_col", selected = "sd_ctrl")
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
      
      # Get selected column names
      selected_cols <- c(
        mean_treat = input$mean_treat_col,
        sd_treat = input$sd_treat_col,
        n_treat = input$n_treat_col,
        mean_ctrl = input$mean_ctrl_col,
        sd_ctrl = input$sd_ctrl_col,
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
        mean_treat = as.numeric(rv$data[[selected_cols["mean_treat"]]]),
        sd_treat = as.numeric(rv$data[[selected_cols["sd_treat"]]]),
        n_treat = as.numeric(rv$data[[selected_cols["n_treat"]]]),
        mean_ctrl = as.numeric(rv$data[[selected_cols["mean_ctrl"]]]),
        sd_ctrl = as.numeric(rv$data[[selected_cols["sd_ctrl"]]]),
        n_ctrl = as.numeric(rv$data[[selected_cols["n_ctrl"]]])
      )
      
      rv$analysis_data <- analysis_data
      
      # Perform meta-analysis
      tryCatch({
        # Standard meta-analysis using metacont
        model <- metacont(
          n.e = rv$analysis_data$n_treat,
          mean.e = rv$analysis_data$mean_treat,
          sd.e = rv$analysis_data$sd_treat,
          n.c = rv$analysis_data$n_ctrl,
          mean.c = rv$analysis_data$mean_ctrl,
          sd.c = rv$analysis_data$sd_ctrl,
          studlab = rv$analysis_data$study,
          sm = input$effect_measure,
          method.tau = "REML",
          method.random.ci = "HK",
          level = input$conf_level,
          fixed = input$method == "FE",
          random = input$method != "FE"
        )
        
        rv$results <- list(
          model = model,
          type = "standard"
        )
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
        # Standard meta-analysis results
        model <- rv$results$model
        
        # Create summary data frame
        if (input$method == "FE") {
          data.frame(
            Measure = c(
              switch(input$effect_measure,
                     "MD" = "Mean Difference",
                     "SMD" = "Standardized Mean Difference"),
              "95% CI", "p-value"),
            Value = c(
              sprintf("%.3f", model$TE.fixed),
              sprintf("%.3f to %.3f", 
                      model$lower.fixed,
                      model$upper.fixed),
              sprintf("%.4f", model$pval.fixed)
            )
          )
        } else {
          # Random effects results
          data.frame(
            Measure = c(
              switch(input$effect_measure,
                     "MD" = "Mean Difference",
                     "SMD" = "Standardized Mean Difference"),
              "95% CI", "95% PI", "p-value", "τ²"),
            Value = c(
              sprintf("%.3f", model$TE.random),
              sprintf("%.3f to %.3f", 
                      model$lower.random,
                      model$upper.random),
              sprintf("%.3f to %.3f",
                      model$lower.predict,
                      model$upper.predict),
              sprintf("%.4f", model$pval.random),
              sprintf("%.3f", model$tau2)
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
    
    # Create forest plot
    create_forest_plot <- function() {
      req(rv$results)
      
      model <- rv$results$model
      
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
    
    # Create funnel plot
    create_funnel_plot <- function() {
      req(rv$results)
      
      # Create funnel plot
      funnel(rv$results$model)
      
      # Add Egger's test
      if (!is.null(rv$results$model)) {
        # Get number of studies
        k <- rv$results$model$k
        
        if (k >= 3) {  # Minimum studies needed for Egger's test
          tryCatch({
            # Perform bias test on metagen object
            bias_test <- metabias(rv$results$model, 
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
        "continuous_sample.csv"
      },
      content = function(file) {
        file.copy("sample_data/continuous.csv", file)
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
             leftcols = c("studlab", "mean.e", "sd.e", "n.e", "mean.c", "sd.c", "n.c"),
             leftlabs = c("Study", "Mean", "SD", "Total", "Mean", "SD", "Total"),
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
