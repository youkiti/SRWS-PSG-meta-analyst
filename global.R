# Required packages
required_packages <- c(
  "shiny",
  "shinydashboard",
  "shinyjs",
  "DT",
  "metafor",
  "meta",
  "forestplot",
  "ggplot2",
  "dplyr",
  "tidyr"
)

# Install missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load packages
invisible(lapply(required_packages, library, character.only = TRUE))

# Global variables
META_METHODS <- c(
  "Fixed-effect (Inverse Variance)" = "inverse",
  "Fixed-effect (Mantel-Haenszel)" = "MH",
  "Random-effects (DerSimonian-Laird)" = "DL",
  "Random-effects (REML)" = "REML",
  "Random-effects (Paule-Mandel)" = "PM",
  "Random-effects (GLMM)" = "GLMM"
)

# Effect measures for different data types
BINARY_MEASURES <- c(
  "Odds Ratio" = "OR",
  "Risk Ratio" = "RR",
  "Risk Difference" = "RD"
)

CONTINUOUS_MEASURES <- c(
  "Mean Difference" = "MD",
  "Standardized Mean Difference" = "SMD"
)

DIAGNOSTIC_MEASURES <- c(
  "Sensitivity & Specificity" = "sens_spec",
  "Diagnostic Odds Ratio" = "DOR"
)
