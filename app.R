# Load required packages and global settings
source("global.R")

# Load module files
source("R/binary.R", local = FALSE)
source("R/continuous.R", local = FALSE)
source("R/diagnostic.R", local = FALSE)
source("R/meta_reg.R", local = FALSE)

# Load UI and server
source("ui.R", local = FALSE)
source("server.R", local = FALSE)

# Create Shiny app
shinyApp(ui = ui, server = server)
