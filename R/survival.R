# Survival Analysis Module

#' UI function for survival analysis tab
#' @param id Namespace ID for the module
#' @importFrom shiny NS
#' @export
survivalUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
        box(
          title = "Data Input",
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          
          # File upload
          fileInput(ns("file"), "Choose CSV File",
                   accept = c("text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv")),
          
          # Sample data download
          downloadLink(ns("download_survival_sample"), "Download sample data"),
          tags$br(),
          tags$br(),
          
          # Analysis settings
          selectInput(ns("method"), "Analysis Method",
                     choices = c("ML", "REML")),
          
          selectInput(ns("model_type"), "Model Type",
                     choices = list(
                       "Random Effects" = "random",
                       "Fixed Effects" = "fixed"
                     )),
          
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
        )
      )
    ),
    
    fluidRow(
      column(12,
        box(
          title = "Analysis Results",
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          
          # Results output
          verbatimTextOutput(ns("results")),
          
          # Forest plot
          plotOutput(ns("forest_plot")),
          
          # Download section
          tags$div(
            style = "margin-top: 20px;",
            tags$div(
              style = "margin-bottom: 10px;",
              tags$strong("Download Results:"),
              downloadButton(ns("download_results"), "Text")
            ),
            tags$div(
              style = "margin-bottom: 10px;",
              tags$strong("Forest Plot:"),
              downloadButton(ns("download_forest_pdf"), "PDF"),
              downloadButton(ns("download_forest_svg"), "SVG")
            ),
            tags$div(
              style = "margin-bottom: 10px;",
              tags$strong("Funnel Plot:"),
              downloadButton(ns("download_funnel_pdf"), "PDF"),
              downloadButton(ns("download_funnel_svg"), "SVG")
            )
          )
        )
      )
    )
  )
}

#' Server function for survival analysis tab
#' @param id Namespace ID for the module
#' @param input Input objects
#' @param output Output objects
#' @param session Session object
#' @importFrom metafor escalc rma forest
#' @export
survivalServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive value for storing the analysis results
    results <- reactiveVal(NULL)
    
    # Read and process data
    data <- reactive({
      req(input$file)
      
      tryCatch({
        # Read CSV file
        dat <- read.csv(input$file$datapath)
        
        # Validate required columns
        required_cols <- c("study", "hr", "ci.lb", "ci.ub")
        missing_cols <- setdiff(required_cols, names(dat))
        
        if (length(missing_cols) > 0) {
          stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
        }
        
        # Calculate log hazard ratios and standard errors
        dat <- escalc(measure = "HR", 
                     ci.lb = ci.lb, 
                     ci.ub = ci.ub, 
                     data = dat,
                     digits = 4)
        
        dat
      }, error = function(e) {
        showNotification(
          paste("Error in data processing:", e$message),
          type = "error"
        )
        NULL
      })
    })
    
    # Perform meta-analysis
    observe({
      req(data())
      
      tryCatch({
        # Fit meta-analysis model
        res <- rma(yi, vi, data = data(), 
                  method = input$method,
                  test = "knha")
        
        # Calculate prediction interval
        pred <- predict(res, level = 0.95, addpred = TRUE)
        
        # Store results
        results(list(
          model = res,
          pooled = pred
        ))
      }, error = function(e) {
        showNotification(
          paste("Error in analysis:", e$message),
          type = "error"
        )
      })
    })
    
    # Display results
    output$results <- renderPrint({
      req(results())
      
      cat("Meta-Analysis Results:\n\n")
      print(results()$model)
      
      cat("\nPooled Hazard Ratio:\n")
      cat("HR:", format(exp(results()$model$b), digits = 3))
      cat("\n95% CI:", format(exp(results()$model$ci.lb), digits = 3),
          "-", format(exp(results()$model$ci.ub), digits = 3))
      cat("\n95% PI:", format(exp(pred$pi.lb), digits = 3),
          "-", format(exp(pred$pi.ub), digits = 3))
    })
    
    # Generate forest plot
    output$forest_plot <- renderPlot({
      req(results())
      forest(results()$model,
             slab = data()$study,
             refline = 1,
             addpred = TRUE,  # 予測区間を追加
             xlab = "Hazard Ratio",
             leftlabs = c("Study", "HR", "95% CI"),
             leftcols = c("studlab", "effect", "ci"),
             rightcols = c("w.random"),
             rightlabs = c("Weight"),
             label.left = "Favors Treatment",
             label.right = "Favors Control",
             fontsize = 10)
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
        sink(file)
        print(results()$model)
        cat("\nPooled Hazard Ratio and 95% CI:\n")
        cat("HR:", format(results()$pooled$pred, digits = 3),
            "\nCI:", format(results()$pooled$ci.lb, digits = 3),
            "-", format(results()$pooled$ci.ub, digits = 3))
        sink()
      }
    )
    
    # Forest plot download handlers
    output$download_forest_pdf <- downloadHandler(
      filename = function() {
        paste0("forest_plot_", format(Sys.time(), "%Y%m%d"), ".pdf")
      },
      content = function(file) {
        pdf(file)
        forest(results()$model,
               slab = data()$study,
               refline = 1,
               xlab = "Hazard Ratio",
               leftlabs = c("Study", "HR", "95% CI"),
               leftcols = c("studlab", "effect", "ci"),
               rightcols = c("w.random"),
               rightlabs = c("Weight"),
               label.left = "Favors Treatment",
               label.right = "Favors Control",
               fontsize = 10)
        dev.off()
      }
    )
    
    output$download_forest_svg <- downloadHandler(
      filename = function() {
        paste0("forest_plot_", format(Sys.time(), "%Y%m%d"), ".svg")
      },
      content = function(file) {
        svg(file)
        forest(results()$model,
               slab = data()$study,
               refline = 1,
               xlab = "Hazard Ratio",
               leftlabs = c("Study", "HR", "95% CI"),
               leftcols = c("studlab", "effect", "ci"),
               rightcols = c("w.random"),
               rightlabs = c("Weight"),
               label.left = "Favors Treatment",
               label.right = "Favors Control",
               fontsize = 10)
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
        funnel(results()$model,
               xlab = "Log Hazard Ratio",
               ylab = "Standard Error")
        dev.off()
      }
    )
    
    output$download_funnel_svg <- downloadHandler(
      filename = function() {
        paste0("funnel_plot_", format(Sys.time(), "%Y%m%d"), ".svg")
      },
      content = function(file) {
        svg(file)
        funnel(results()$model,
               xlab = "Log Hazard Ratio",
               ylab = "Standard Error")
        dev.off()
      }
    )
  })
}
