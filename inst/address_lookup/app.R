library(shiny)
library(httr)
library(tidyverse)
library(hkdata)
library(DT)

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
  titlePanel(p("Hong Kong Address Lookup", style = "color:green")),
  fluidRow(column(
    12,
    textAreaInput("address", "Enter Address to Parse", cols = 200, rows = 3),
    actionButton1("submit", "Submit", status = "primary")
  )),
  hr(style = "border: 3px solid green;"),
  fluidRow(column(
    6,
    DT::dataTableOutput("info")
  ),
  column(
    6,
    leafletOutput("map")
  ))
)

server <- function(input, output) {
  parsed <- reactive({
    input$submit
    add <- isolate(input$address)
    if (add == "") return(NULL)
    res <- address_lookup(add)
    res
  })
  
  output$info <- DT::renderDataTable({
    md <- parsed()
    if (is.null(md)) return(NULL)
    d <- list(
      `Building Name` = md$EngPremisesAddressBuildingName,
      Address = paste(md$EngPremisesAddressEngStreetBuildingNoFrom,
                      md$EngPremisesAddressEngStreetStreetName),
      Region = md$EngPremisesAddressRegion,
      District = md$EngPremisesAddressEngDistrictDcDistrict,
      Latitude = md$GeospatialInformationLatitude,
      Longitude = md$GeospatialInformationLongitude
    )
    d <- lapply(d, function(x) ifelse(is.null(x), "", x))
    d <- tibble(
      name = names(d),
      value = unlist(d)
    )
    d %>% datatable(
      rownames = FALSE,
      colnames = c("", ""),
      selection = "none",
      options = list(
        pageLength = 9999,
        dom = "",
        ordering = FALSE,
        columnDefs = list(list(className = "dt-right", targets = 1))
      )
    )
  })
  
  output$map <- renderLeaflet({
    md <- parsed()
    if (is.null(md)) return(NULL)
    lat <- as.numeric(md$GeospatialInformationLatitude)
    long <- as.numeric(md$GeospatialInformationLongitude)
    building <- md$EngPremisesAddressBuildingName
    if (is.null(building)) building <- ""
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng = long, lat = lat, popup = building)
  })
}

shinyApp(ui, server)
