server <- function(input, output, session) {
  
  # Reactive values to store analysis results
  results <- reactiveValues(
    binary = NULL,
    continuous = NULL,
    diagnostic = NULL,
    meta_reg = NULL
  )
  
  # Helper function to create forest plots
  create_forest_plot <- function(model, measure) {
    if (is.null(model)) return(NULL)
    
    # Create forest plot using meta package
    forest(model,
           leftlabs = c("Study", "Total", "Effect", "95% CI"),
           rightlabs = c("Weight", "Effect (95% CI)"),
           fontsize = 1,
           label.left = "Favors Control",
           label.right = "Favors Treatment")
  }
  
  # Helper function to create funnel plots
  create_funnel_plot <- function(model) {
    if (is.null(model)) return(NULL)
    
    # Create funnel plot using meta package
    funnel(model,
           xlab = "Effect Size",
           ylab = "Standard Error",
           contour = c(0.9, 0.95, 0.99))
  }
  
  # Initialize modules
  results$binary <- callModule(binary, "binary")
  results$continuous <- callModule(continuous, "continuous")
  results$diagnostic <- callModule(diagnostic, "diagnostic")
  results$meta_reg <- callModule(meta_reg, "meta_reg")
  
  # Download handlers
  output$download_results <- downloadHandler(
    filename = function() {
      paste("meta_analysis_results_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Logic to prepare and write results to CSV
      write.csv(results_data(), file, row.names = FALSE)
    }
  )
  
  # Render plots
  output$forest_plot <- renderPlot({
    # Logic to determine which model to use based on active tab
    model <- switch(input$sidebar_menu,
                   binary = results$binary,
                   continuous = results$continuous,
                   diagnostic = results$diagnostic,
                   meta_reg = results$meta_reg)
    
    create_forest_plot(model, input$effect_measure)
  })
  
  output$funnel_plot <- renderPlot({
    # Logic to determine which model to use based on active tab
    model <- switch(input$sidebar_menu,
                   binary = results$binary,
                   continuous = results$continuous,
                   diagnostic = results$diagnostic,
                   meta_reg = results$meta_reg)
    
    create_funnel_plot(model)
  })
  
  # Render results table
  output$results_table <- renderDT({
    # Logic to create results summary table
    req(results[[input$sidebar_menu]])
    
    summary_data <- data.frame(
      Measure = c("Effect Size", "95% CI", "p-value", "I²", "τ²"),
      Value = c(
        sprintf("%.3f", results[[input$sidebar_menu]]$TE.random),
        sprintf("%.3f to %.3f", 
                results[[input$sidebar_menu]]$lower.random,
                results[[input$sidebar_menu]]$upper.random),
        sprintf("%.4f", results[[input$sidebar_menu]]$pval.random),
        sprintf("%.1f%%", results[[input$sidebar_menu]]$I2),
        sprintf("%.3f", results[[input$sidebar_menu]]$tau2)
      )
    )
    
    datatable(summary_data,
              options = list(dom = 't',
                           pageLength = 5,
                           searching = FALSE,
                           ordering = FALSE),
              rownames = FALSE)
  })
  
  # Session end cleanup
  onSessionEnded(function() {
    # Cleanup code here if needed
  })
}
