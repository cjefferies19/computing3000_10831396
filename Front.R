# Load necessary libraries
if (!require("leaflet")) install.packages("leaflet", dependencies = TRUE)
if (!require("dplyr"))   install.packages("dplyr",   dependencies = TRUE)
if (!require("shiny"))   install.packages("shiny",   dependencies = TRUE)

library(leaflet)
library(dplyr)
library(shiny)

# Sample data
data <- data.frame(
  area_name = c("London","Cambridge","Oxford","Manchester","Liverpool",
                "Bristol","Newcastle","Glasgow","Birmingham","Leeds","Plymouth"),
  latitude  = c(51.5074,52.2053,51.7520,53.4808,53.4084,51.4545,54.9784,55.8642,52.4862,53.8008,50.3755),
  longitude = c(-0.1278,0.1218,-1.2577,-2.2426,-2.9916,-2.5879,-1.6178,-4.2518,-1.8904,-1.5491,-4.1427),
  flood_risk_percentage = c(20,50,30,70,40,10,80,60,100,55,100),
  population = c(9000000,150000,150000,530000,490000,450000,1480000,620000,1100000,800000,260000),
  date = as.Date(rep("2025-02-03",11))
)

# Function to adjust flood risk for large populations only, then clamp 5–95%
adjust_flood_risk <- function(risk, pop) {
  if (pop > 1e6) risk <- risk * (1/(pop/1e6))
  pmin(pmax(risk, 5), 95)
}
data$adjusted_flood_risk <- mapply(adjust_flood_risk,
                                   data$flood_risk_percentage,
                                   data$population)

ui <- fluidPage(
  titlePanel("Flood Risk Map with Population Adjustments"),
  
  # Date selector above the map
  fluidRow(
    column(12,
           dateInput("date", "Select Date:", value = as.Date("2025-02-03"))
    )
  ),
  
  # Map
  fluidRow(
    column(12,
           leafletOutput("floodMap", width = "100%", height = "600px")
    )
  ),
  
  # Legend
  fluidRow(
    column(12, align = "center",
           div(style = "font-weight:bold; font-size:16px; margin-bottom:4px;",
               "Flood Risk % (5–95)"),
           div(style = paste(
             "width:50%; max-width:500px; height:20px;",
             "background:linear-gradient(to right, yellow, purple);",
             "border:2px solid black; margin:0 auto;"
           )),
           div(style = "display:flex; justify-content:space-between; width:50%; max-width:500px; margin:0 auto; font-size:14px;",
               div("5%"), div("50%"), div("95%")
           )
    )
  ),
  
  # Info box
  fluidRow(
    column(12, align = "center",
           div(id = "info-box", style = paste(
             "width:50%; max-width:500px; margin:10px auto; padding:8px;",
             "border:2px solid #333; border-radius:8px; background:#f9f9f9;"
           ),
           uiOutput("selectedInfo")
           )
    )
  )
)

server <- function(input, output, session) {
  pal <- colorNumeric(palette = c("yellow","purple"), domain = c(5,95))
  
  observe({
    df <- filter(data, date == input$date)
    
    leafletProxy("floodMap") %>%
      clearMarkers() %>%
      clearControls()
    
    if (nrow(df) == 0) {
      leafletProxy("floodMap") %>%
        addControl(
          html = "<div style='color: red; background-color: white; padding: 8px; border: 2px solid red; border-radius: 5px; font-weight: bold;'>No flood data available for the selected date.</div>",
          position = "topright"
        )
      return()
    }
    
    bounds <- df %>%
      summarize(minLng = min(longitude), minLat = min(latitude),
                maxLng = max(longitude), maxLat = max(latitude))
    
    leafletProxy("floodMap", data = df) %>%
      fitBounds(bounds$minLng, bounds$minLat, bounds$maxLng, bounds$maxLat) %>%
      addCircleMarkers(
        lat         = ~latitude,
        lng         = ~longitude,
        fillColor   = ~pal(adjusted_flood_risk),
        fillOpacity = 0.7,
        color       = "black",
        weight      = 1,
        radius      = 5,
        stroke      = TRUE,
        layerId     = ~area_name,
        label       = ~HTML(paste0(
          "<div style='font-size:14px;'><b>", area_name, "</b><br/>",
          "Orig. Risk: ", flood_risk_percentage, "%<br/>",
          "Adj. Risk: ", round(adjusted_flood_risk,1), "%<br/>",
          "Pop: ", format(population, big.mark=","), "</div>"
        )),
        labelOptions = labelOptions(
          html      = TRUE,
          direction = "auto",
          textsize  = "13px",
          style     = list(
            color          = "black",
            background     = "white",
            border         = "1px solid gray",
            `border-radius`= "4px",
            padding        = "4px"
          ),
          sticky = TRUE
        )
      )
  })
  
  output$floodMap <- renderLeaflet({
    leaflet() %>% addTiles()
  })
  
  output$selectedInfo <- renderUI({
    click <- input$floodMap_marker_click
    if (is.null(click)) {
      return(h4("Selected Location Information", style = "margin:0;"))
    }
    d <- data %>% filter(area_name == click$id)
    div(
      h4(d$area_name, style = "margin:0 0 8px 0;"),
      div(strong("Original Risk: "), paste0(d$flood_risk_percentage, "%")),
      div(strong("Adjusted Risk: "), paste0(round(d$adjusted_flood_risk,1), "%")),
      div(strong("Population: "), format(d$population, big.mark = ","))
    )
  })
}

shinyApp(ui, server)
