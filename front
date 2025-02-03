# NOT USED AND CREATED BY AI

# Load necessary libraries
if (!require("leaflet")) install.packages("leaflet", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
if (!require("shiny")) install.packages("shiny", dependencies = TRUE)

# Load libraries
library(leaflet)
library(dplyr)
library(shiny)

# Sample data (replace this with your actual data source)
# Assuming your data has columns: area_name, latitude, longitude, flood_risk_percentage, date
data <- data.frame(
  area_name = c("London", "Cambridge", "Oxford"),
  latitude = c(51.5074, 52.2053, 51.7520),
  longitude = c(-0.1278, 0.1218, -1.2577),
  flood_risk_percentage = c(20, 50, 30),
  date = as.Date(c("2025-02-03", "2025-02-03", "2025-02-03"))
)

# Shiny UI
ui <- fluidPage(
  titlePanel("Flood Risk Map"),
  sidebarLayout(
    sidebarPanel(
      dateInput("date", "Select Date:", value = Sys.Date())
    ),
    mainPanel(
      leafletOutput("floodMap")
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  output$floodMap <- renderLeaflet({
    # Filter data based on selected date
    filtered_data <- filter(data, date == input$date)
    
    # Create leaflet map
    map <- leaflet(filtered_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lat = ~latitude,
        lng = ~longitude,
        popup = ~paste(area_name, ": ", flood_risk_percentage, "% chance of flooding"),
        color = ~ifelse(flood_risk_percentage < 50, 'blue', 'red'),
        fill = TRUE,
        fillColor = ~ifelse(flood_risk_percentage < 50, 'blue', 'red'),
        radius = 5
      )
    map
  })
}

# Run the application
shinyApp(ui, server)
