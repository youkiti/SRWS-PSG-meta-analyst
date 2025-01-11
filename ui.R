ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = "SRWS-PSG-meta-analyst"
  ),
  
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Binary Data", tabName = "binary", icon = icon("percentage")),
      menuItem("Continuous Data", tabName = "continuous", icon = icon("chart-line")),
      menuItem("Survival Analysis", tabName = "survival", icon = icon("heartbeat")),
      menuItem("Meta-Regression", tabName = "meta_reg", icon = icon("chart-bar")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  # Body
  dashboardBody(
    useShinyjs(),
    
    # Custom CSS
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #ffffff;
        }
        .box {
          box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
          transition: all 0.3s cubic-bezier(.25,.8,.25,1);
        }
        .box:hover {
          box-shadow: 0 14px 28px rgba(0,0,0,0.25), 0 10px 10px rgba(0,0,0,0.22);
        }
      "))
    ),
    
    tabItems(
      # Home tab
      tabItem(
        tabName = "home",
        fluidRow(
          box(
            width = 12,
            title = "Welcome to Shiny Meta-Analysis",
            status = "primary",
            solidHeader = TRUE,
            p("This is a modern implementation of OpenMeta-Analyst, providing tools for various types of meta-analyses:"),
            tags$ul(
              tags$li(strong("Binary Data:"), "For studies with binary outcomes (e.g., event vs. no event)"),
              tags$li(strong("Continuous Data:"), "For studies with continuous outcomes (e.g., mean differences)"),
              tags$li(strong("Meta-Regression:"), "For exploring heterogeneity through covariates")
            ),
            p("Select the appropriate analysis type from the sidebar to begin.")
          )
        ),
        fluidRow(
          box(
            width = 6,
            title = "Quick Start",
            status = "info",
            p("1. Choose your data type from the sidebar"),
            p("2. Upload your data or use the manual input form"),
            p("3. Select your analysis method"),
            p("4. View results and generate plots")
          ),
          box(
            width = 6,
            title = "Features",
            status = "success",
            tags$ul(
              tags$li("Multiple effect measures (OR, RR, MD, etc.)"),
              tags$li("Fixed and random effects models"),
              tags$li("Forest plots and funnel plots"),
              tags$li("Heterogeneity statistics"),
              tags$li("Publication bias assessment"),
              tags$li("Subgroup analyses"),
              tags$li("Meta-regression capabilities")
            )
          )
        )
      ),
      
      # Analysis tabs
      tabItem(tabName = "binary", binaryUI("binary")),
      tabItem(tabName = "continuous", continuousUI("continuous")),
      tabItem(tabName = "survival", survivalUI("survival")),
      tabItem(tabName = "meta_reg", meta_regUI("meta_reg")),
      
      # About tab
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            width = 12,
            title = "About Shiny Meta-Analysis",
            status = "primary",
            p("This is a modern Shiny implementation of OpenMeta-Analyst, originally developed by Byron Wallace and colleagues."),
            p("The original OpenMeta-Analyst software was designed as a cross-platform desktop application for conducting meta-analyses. This web-based version maintains the core functionality while providing a more accessible and modern interface."),
            h4("References"),
            tags$ul(
              tags$li("Wallace, B. C., et al. (2012). OpenMeta-Analyst: Cross-platform software for advanced meta-analysis. Systematic Reviews, 1(1), 1-1."),
              tags$li("Viechtbauer, W. (2010). Conducting meta-analyses in R with the metafor package. Journal of Statistical Software, 36(3), 1-48.")
            ),
            h4("Packages Used"),
            tags$ul(
              tags$li("metafor: For meta-analysis computations"),
              tags$li("meta: Additional meta-analysis functions"),
              tags$li("shiny & shinydashboard: For the web interface"),
              tags$li("ggplot2: For visualization"),
              tags$li("DT: For interactive tables")
            )
          )
        )
      )
    )
  )
)
