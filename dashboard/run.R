# Global
source("global.R", encoding="utf-8")

# Create user interface
source("ui.R", encoding="utf-8")

# Create server
source("server.R", encoding="utf-8")

# Create a Shiny app object
shinyApp(ui = ui, server = server)
