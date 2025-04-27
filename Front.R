# Loading libraries
library(shiny)
library(leaflet)
library(dplyr)
library(data.table)
library(ggplot2)
library(DT)

# Calculate flood risk
calculate_flood_risk <- function(temperature, rainfall, wind) {
  risk <- (0.6 * pmin(rainfall, 500) / 500) +
    (0.3 * pmin(wind, 100) / 100) +
    (0.1 * pmin(temperature, 50) / 50)
  round(risk * 100, 1)
}

# Read and prepare data
raw_data <- fread("combined_with_autoencoder_clusters.csv") %>%
  mutate(
    time = as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    flood_risk = calculate_flood_risk(temperature, rainfall, wind),
    month = format(time, "%Y-%m"),
    id = row_number(),
    longitude = longitude + runif(n(), -0.2, 0.2),
    latitude = latitude + runif(n(), -0.15, 0.15)
  ) %>%
  as.data.table()

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #bottom-panel {
        position: fixed;
        bottom: 0;
        left: 0;
        right: 0;
        background: #f8f9fa;
        border-top: 1px solid #ccc;
        padding: 10px;
        z-index: 1000;
      }
      .dataTables_wrapper {
        background: #f8f9fa;
      }
      body {padding-bottom: 130px;} /* Leave space at bottom for the panel */
    "))
  ),
  
  titlePanel("UK Flood Risk Map (by Month)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("month", "Select Month:",
                  choices = sort(unique(raw_data$month))),
      sliderInput("threshold", "Min Flood Risk %:",
                  min = 0, max = 100, value = 30, step = 1)
    ),
    
    mainPanel(
      leafletOutput("floodMap", height = 600),
      div(id = "bottom-panel",
          h4("Selected Flood Risk Details"),
          DTOutput("selectedPoint")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  base_df <- reactive({
    req(input$month)
    raw_data %>%
      filter(month == input$month, flood_risk >= input$threshold) %>%
      mutate(radius_px = 5 + (flood_risk / 100) * 10)
  })
  
  zoom_level <- reactive(input$floodMap_zoom %||% 6) %>%
    debounce(300)
  
  output$floodMap <- renderLeaflet({
    leaflet() %>% addTiles() %>% fitBounds(-10, 50, 2, 56)
  })
  
  observeEvent(
    { base_df(); zoom_level() },
    {
      df <- base_df()
      zoom <- zoom_level()
      proxy <- leafletProxy("floodMap")
      
      proxy %>%
        clearGroup("clustered") %>%
        clearGroup("jittered")
      
      if (zoom <= 8) {
        proxy %>%
          addCircleMarkers(
            data           = df,
            lng            = ~longitude,
            lat            = ~latitude,
            layerId        = ~id,
            group          = "clustered",
            radius         = ~radius_px,
            stroke         = TRUE,
            color          = "black",
            fillColor      = ~colorNumeric("YlOrRd", flood_risk)(flood_risk),
            fillOpacity    = 0.8,
            popup          = ~paste0("Risk: ", flood_risk, "%"),
            clusterOptions = markerClusterOptions(
              spiderfyOnMaxZoom = TRUE,
              maxClusterRadius  = 40
            )
          )
      } else {
        deg_per_px <- 360 / (256 * 2^zoom)
        df2 <- df %>%
          mutate(
            jitter_deg = radius_px * deg_per_px,
            jlon       = longitude + runif(n(), -jitter_deg, jitter_deg),
            jlat       = latitude  + runif(n(), -jitter_deg, jitter_deg)
          )
        
        proxy %>%
          addCircleMarkers(
            data        = df2,
            lng         = ~jlon,
            lat         = ~jlat,
            layerId     = ~id,
            group       = "jittered",
            radius      = ~radius_px,
            stroke      = TRUE,
            color       = "black",
            fillColor   = ~colorNumeric("YlOrRd", flood_risk)(flood_risk),
            fillOpacity = 0.8,
            popup       = ~paste0("Risk: ", flood_risk, "%")
          )
      }
    },
    ignoreInit = FALSE
  )
  
  clicked_id <- reactiveVal(NULL)
  
  observeEvent(input$floodMap_marker_click, {
    click <- input$floodMap_marker_click
    clicked_id(click$id)
  })
  
  output$selectedPoint <- renderDT({
    req(clicked_id())
    df <- base_df()
    selected_row <- df %>%
      filter(id == clicked_id()) %>%
      mutate(
        latitude = round(latitude, 1),
        longitude = round(longitude, 1),
        flood_risk = round(flood_risk, 1),
        temperature = round(temperature, 1),
        rainfall = round(rainfall, 1),
        wind = round(wind, 1)
      ) %>%
      select(time, latitude, longitude, flood_risk, temperature, rainfall, wind)
    
    datatable(
      selected_row,
      options = list(dom = 't', paging = FALSE),
      rownames = FALSE
    )
  })
}

# Run
shinyApp(ui, server)
