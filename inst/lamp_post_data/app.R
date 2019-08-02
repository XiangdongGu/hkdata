library(shiny)
library(tidyverse)
library(hkdata)
library(DT)
library(leaflet)
library(rgdal)

# for converting the northing and easting into lon
# https://stackoverflow.com/questions/36520915/converting-utms-to-lat-long-in-r
# https://medium.com/@eric_hk/dcca-boundary-map-99edb31b62ca
wgs84 = "+init=epsg:4326"
hk1980 = "+init=epsg:2326"

ConvertCoordinates <- function(easting,northing) {
  out = cbind(easting,northing)
  mask = !is.na(easting)
  sp <-  sp::spTransform(sp::SpatialPoints(list(easting[mask],northing[mask]),
                                           proj4string=sp::CRS(hk1980)),
                         sp::CRS(wgs84))
  out[mask,]=sp@coords
  out
}

id_names <- tribble(
  ~id, ~datum,
  "no2", "Nitrogen Dioxide (ppb)",
  "no", "Nitrogen Monoxide (ppb)",
  "o3", "Ozone (ppb)",
  "co", "Carbon Monoxide (ppm)",
  "radiation", "Ultraviolet Radiation (uW/cm^2)",
  "pm10", "PM10 (ug/m^3)",
  "pm25", "PM2.5 (ug/m^3)",
  "pm1", "PM1 (ug/m^3)",
  "temperature_45", "Temperature at 4.5m above Ground (degree Celsius)",
  "humidity_45","Relative Humidity (%)",
  "temperature_2", "Temperature at 2m above Ground (degree Celsius)",
  "humidity_2", "Relative Humidity at 2m above Ground (%)",
  "pressure", "Atospheric Pressure (hPa)",
  "windspeed", "Wind Speed (m/s)",
  "winddirection", "Wind Direction (bearing in degree)",
  "vehiclecount_e", "Daily Cumulative Number of Vehicles (Eastbound)",
  "vehiclecount_w", "Daily Cumulative Number of Vehicles (Westbound)",
  "vehiclecount_in", "Daily Cumulative Number of Vehicles (In)",
  "vehiclecount_out", "Daily Cumulative Number of Vehicles (Out)",
  "peoplecount", "Daily Cumulative Number of Pedestrians"
)

value_ids <- c("no2", "no", "o3", "co",
               "radiation",
               "pm10", "pm25", "pm1",
               "temperature_45",
               "humidity_45", 
               "temperature_2",
               "humidity_2",
               "pressure", "windspeed")

value_names <- c("Nitrogen Dioxide (ppb)", "Nitrogen Monoxide (ppb)", "Ozone (ppb)", "Carbon Monoxide (ppm)",
                 "Ultraviolet Radiation (uW/cm^2)",
                 "PM10 (ug/m^3)", "PM2.5 (ug/m^3)", "PM1 (ug/m^3)",
                 "Temperature at 4.5m above Ground (degree Celsius)",
                 "Relative Humidity (%)", 
                 "Temperature at 2m above Ground (degree Celsius)",
                 "Relative Humidity at 2m above Ground (%)",
                 "Atospheric Pressure (hPa)", "Wind Speed (m/s)")

##

actionButton1 <- function(inputId, label, icon = NULL, width = NULL, 
                          status = "default", ...) {
  value <- restoreInput(id = inputId, default = NULL)
  cs <- sprintf("btn %s action-button", paste0("btn-", status))
  tags$button(id = inputId, style = if (!is.null(width)) 
    paste0("width: ", validateCssUnit(width), ";"), type = "button", 
    class = cs, `data-val` = value, 
    list(shiny:::validateIcon(icon), label), ...)
}

ui <- fluidPage(
  titlePanel("Real-time city data collected by multi-purpose lamp posts in Kowloon East"),
  # fluidRow(column(
  #   12,
  #   textAreaInput("address", "Enter Address to Parse", cols = 200, rows = 3),
  #   actionButton1("submit", "Submit", status = "primary")
  # )),
  # hr(style = "border: 3px solid green;"),
  hr(),
  radioButtons("datum", "Show the Value of",
               choiceNames = value_names,
               choiceValues = value_ids,
               inline = TRUE),
  hr(),
  h5("Refresh every minute."),
  textOutput("update_date"),
  fluidRow(column(
    6,
    DT::dataTableOutput("lamp_post")
  ),
  column(
    6,
    leafletOutput("map")
  )),
  hr()
)

server <- function(input, output) {
  data <- reactive({
    # refresh every 30 seconds
    invalidateLater(30000)
    
    # get the real-time data, for the current minute
    res <- lamp_posts_data() %>% 
      mutate_at(vars(hk1980_northing, hk1980_easting, value), as.numeric)
    
    lat_lng <- ConvertCoordinates(res$hk1980_easting, res$hk1980_northing)
    res$lng <- lat_lng[,"easting"]
    res$lat <- lat_lng[,"northing"]
    
    res
  })
  
  lamp_posts <- reactive({
    data() %>%
      select(-id, -value) %>% 
      unique() %>% 
      mutate(label = paste0("<b>", fullname_en, "</b><br>\n",
                            update_date, "<br>",
                            "lat: ", lat, "<br>",
                            "lng: ", lng, "<br>"))
  })
  
  output$update_date <- renderText(max(data()$update_date))
  
  # with clickable marker
  # https://www.r-graph-gallery.com/4-tricks-for-working-with-r-leaflet-and-shiny/
  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  output$map <- renderLeaflet({
    data_pol <- data() %>% filter(id == input$datum)
    pal <- colorNumeric(palette = c("green", "red"), domain = data_pol$value)
    
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = lamp_posts(),
                 lng = ~lng, lat = ~lat,
                 layerId = ~name, popup = ~label) %>% 
      # color pallete
      # https://stackoverflow.com/questions/48002096/how-to-change-circle-colours-in-leaflet-based-on-a-variable
      addCircles(lng = data_pol$lng, lat = data_pol$lat,
                 opacity = 0.9, fillOpacity = 0.5,
                 weight = 1, radius = 100, color = pal(data_pol$value))
  })
  
  # store the click
  observeEvent(input$map_marker_click,{
    data_of_click$clickedMarker <- input$map_marker_click
  })
  
  output$lamp_post <- DT::renderDataTable({
    data() %>% 
      filter(name == data_of_click$clickedMarker$id) %>% 
      inner_join(id_names, by = "id") %>% 
      select(datum, value)
  })
}

shinyApp(ui, server)
