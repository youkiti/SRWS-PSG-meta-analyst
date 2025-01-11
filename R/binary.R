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
                   choices = BINARY_MEASURES),
        
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
      rv$analysis_data <- data.frame(
        study = rv$data[[input$study_col]],
        e_treat = as.numeric(rv$data[[selected_cols["e_treat"]]]),
        n_treat = as.numeric(rv$data[[selected_cols["n_treat"]]]),
        e_ctrl = as.numeric(rv$data[[selected_cols["e_ctrl"]]]),
        n_ctrl = as.numeric(rv$data[[selected_cols["n_ctrl"]]])
      )
      
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
            # Standard random effects model
            model <- rma(measure = "OR",
                        ai = rv$analysis_data$e_treat,
                        bi = rv$analysis_data$n_treat - rv$analysis_data$e_treat,
                        ci = rv$analysis_data$e_ctrl,
                        di = rv$analysis_data$n_ctrl - rv$analysis_data$e_ctrl,
                        method = input$method,
                        level = input$conf_level)
            
            # Calculate prediction interval manually
            k <- length(rv$analysis_data$e_treat)
            pi_factor <- qt(0.975, k-1)
            pi_lb <- model$beta - pi_factor * sqrt(model$tau2 + model$se^2)
            pi_ub <- model$beta + pi_factor * sqrt(model$tau2 + model$se^2)
            
            rv$results <- list(
              model = model, 
              pi = list(lb = pi_lb, ub = pi_ub),
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
      
      if (rv$results$type == "logistic") {
        # Logistic regression results
        model <- rv$results$model
        summary_data <- data.frame(
          Measure = c("Odds Ratio", "95% CI", "p-value"),
          Value = c(
            sprintf("%.3f", exp(model$b[2])),  # intervention effect
            sprintf("%.3f to %.3f", 
                    exp(model$ci.lb[2]),
                    exp(model$ci.ub[2])),
            sprintf("%.4f", model$pval[2])
          )
        )
      } else if (rv$results$type == "mh_no_correction") {
        # MH model results
        model <- rv$results$model
        summary_data <- data.frame(
          Measure = c("Odds Ratio", "95% CI", "p-value"),
          Value = c(
            sprintf("%.3f", exp(model$TE.fixed)),
            sprintf("%.3f to %.3f", 
                    exp(model$lower.fixed),
                    exp(model$upper.fixed)),
            sprintf("%.4f", model$pval.fixed)
          )
        )
      } else {
        # Standard meta-analysis results
        model <- rv$results$model
        
        # Create summary data frame
        if (input$method == "FE") {
          summary_data <- data.frame(
            Measure = c("Odds Ratio", "95% CI", "p-value"),
            Value = c(
              sprintf("%.3f", exp(model$beta)),
              sprintf("%.3f to %.3f", 
                      exp(model$ci.lb),
                      exp(model$ci.ub)),
              sprintf("%.4f", model$pval)
            )
          )
        } else {
          # Use manually calculated prediction interval
          summary_data <- data.frame(
            Measure = c("Odds Ratio", "95% CI", "95% PI", "p-value", "τ²"),
            Value = c(
              sprintf("%.3f", exp(model$beta)),
              sprintf("%.3f to %.3f", 
                      exp(model$ci.lb),
                      exp(model$ci.ub)),
              sprintf("%.3f to %.3f",
                      exp(rv$results$pi$lb),
                      exp(rv$results$pi$ub)),
              sprintf("%.4f", model$pval),
              sprintf("%.3f", model$tau2)
            )
          )
        }
      }
      
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
        # For standard meta-analysis
        if (input$method == "FE") {
          forest(model,
            mlab = paste0("Fixed Effect Model (k = ", model$k, ")"),
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
          
        } else {
          # Random effects model with prediction interval
          forest(model,
            mlab = paste0("Random Effects Model (k = ", model$k, ")"),
            addpred = TRUE,
            predstyle = "bar",
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
        }
        
        # Add column headers
        text(c(-10, -6), model$k + 2,
             c("Treatment\nEvents/Total", "Control\nEvents/Total"),
             cex = 0.8, font = 2)
        
        # Add heterogeneity statistics
        if (input$method != "FE") {
          text(-16, -2, pos = 4, cex = 0.8, font = 3, bquote(paste(
            "Heterogeneity: ",
            "Q = ", .(sprintf("%.2f", model$QE)), ", ",
            "df = ", .(model$k - 1), ", ",
            "p ", .(sprintf("%.3f", model$QEp)), "; ",
            I^2, " = ", .(sprintf("%.1f", model$I2)), "%, ",
            tau^2, " = ", .(sprintf("%.3f", model$tau2))
          )))
        }
      }
    }
    
    # Create funnel plot
    create_funnel_plot <- function() {
      req(rv$results)
      
      if (rv$results$type == "logistic") {
        plot.new()
        text(0.5, 0.5, "Funnel plot not applicable\nfor logistic regression model",
             cex = 1.2, col = "darkgray")
      } else if (rv$results$type == "mh_no_correction") {
        funnel(rv$results$model)
      } else {
        funnel(rv$results$model)
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
      
      if (rv$results$type == "logistic") {
        data.frame(
          Statistic = c("τ² (Random Effects)"),
          Value = sprintf("%.3f", rv$results$model$tau2)
        )
      } else if (rv$results$type == "mh_no_correction") {
        data.frame(
          Statistic = c("Q statistic", "Q p-value"),
          Value = c(
            sprintf("%.2f", rv$results$model$Q),
            sprintf("%.4f", rv$results$model$pval.Q)
          )
        )
      } else {
        if (input$method == "FE") {
          data.frame(
            Statistic = c("Q statistic", "Q p-value"),
            Value = c(
              sprintf("%.2f", rv$results$model$QE),
              sprintf("%.4f", rv$results$model$QEp)
            )
          )
        } else {
          data.frame(
            Statistic = c("τ²", "I²", "H²", "Q statistic", "Q p-value"),
            Value = c(
              sprintf("%.3f", rv$results$model$tau2),
              sprintf("%.1f%%", rv$results$model$I2),
              sprintf("%.2f", rv$results$model$H2),
              sprintf("%.2f", rv$results$model$QE),
              sprintf("%.4f", rv$results$model$QEp)
            )
          )
        }
      }
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
    
    # Return reactive results for use in main server
    return(reactive({ rv$results }))
  })
}
