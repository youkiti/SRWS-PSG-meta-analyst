# Install required packages
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

cat("Setup completed. You can now run the app using:\n")
cat("library(shiny)\n")
cat("runApp()\n")
