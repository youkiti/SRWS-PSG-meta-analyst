# Load required packages
source("global.R")

# Load module files
source("R/binary.R")
source("R/continuous.R")
source("R/diagnostic.R")
source("R/meta_reg.R")

# Load UI
source("ui.R")

# Load server
source("server.R")

# Create Shiny app
shinyApp(ui = ui, server = server)
