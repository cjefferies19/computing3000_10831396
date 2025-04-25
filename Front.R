library(shiny)
library(leaflet)
library(dplyr)
library(data.table)
library(ggplot2)

#–– Flood-risk helper
calculate_flood_risk <- function(temperature, rainfall, wind) {
  risk <- (0.6 * pmin(rainfall, 500) / 500) +
    (0.3 * pmin(wind,     100) / 100) +
    (0.1 * pmin(temperature, 50)  / 50)
  round(risk * 100, 1)
}

#–– 1) Load & preprocess once
raw_data <- fread("combined_with_autoencoder_clusters.csv") %>%
  mutate(
    time = as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    flood_risk = calculate_flood_risk(temperature, rainfall, wind),
    month = format(time, "%Y-%m"),
    id = row_number(),
    longitude = longitude + runif(n(), -0.2, 0.2),
    latitude = latitude + runif(n(), -0.15, 0.15)
  ) %>%
  as.data.table()  # If you still need to work with data.table syntax


#–– Debugging aid: Validate the jittered spread
print("Jittered coordinate bounds:")
print(
  raw_data %>%
    summarise(
      lon_min = min(longitude),
      lon_max = max(longitude),
      lat_min = min(latitude),
      lat_max = max(latitude)
    )
)

#–– Simple UK bounds polygon for validation plot
uk_bbox <- data.frame(
  lon = c(-10, 2, 2, -10, -10),
  lat = c(50, 50, 58, 58, 50)
)

#–– Plot the spread for validation
ggplot() +
  geom_path(data = uk_bbox, aes(lon, lat), size = 0.8) +
  geom_point(
    data = raw_data,
    aes(x = longitude, y = latitude),
    alpha = 0.3,
    size = 0.8
  ) +
  coord_quickmap(xlim = c(-10, 2), ylim = c(50, 58)) +
  labs(
    title = "Jittered Data Points over UK Bounding Box",
    x = "Longitude",
    y = "Latitude"
  )

#–– 2) UI
ui <- fluidPage(
  titlePanel("UK Flood Risk Map (by Month)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("month", "Select Month:",
                  choices = sort(unique(raw_data$month))),
      sliderInput("threshold", "Min Flood Risk %:",
                  min = 0, max = 100, value = 30, step = 1)
    ),
    mainPanel(
      leafletOutput("floodMap", height = 600)
    )
  )
)

#–– 3) Server
server <- function(input, output, session) {
  
  # A) Filtered data by month + threshold
  base_df <- reactive({
    req(input$month)
    raw_data %>%
      filter(month == input$month, flood_risk >= input$threshold) %>%
      mutate(radius_px = 5 + (flood_risk / 100) * 10)
  })
  
  # B) Debounced zoom
  zoom_level <- reactive(input$floodMap_zoom %||% 6) %>%
    debounce(300)
  
  # C) Initial map
  output$floodMap <- renderLeaflet({
    leaflet() %>% addTiles() %>% fitBounds(-10, 50, 2, 56)
  })
  
  # D) Redraw on month/threshold or (debounced) zoom changes
  observeEvent(
    { base_df(); zoom_level() },
    {
      df   <- base_df()
      zoom <- zoom_level()
      proxy <- leafletProxy("floodMap")
      
      proxy %>%
        clearGroup("clustered") %>%
        clearGroup("jittered")
      
      if (zoom <= 8) {
        # Clustered view
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
        # Jittered view on zoom
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
}

shinyApp(ui, server)
