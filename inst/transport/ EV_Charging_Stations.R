
library(hkdata)
library(leaflet)

# Adding Map Markers: EV Charging Stations 

mapdata =  transport_EV_charging(22.267,114.236)
mapdata[1:3,]  # the first record



leaflet(data = mapdata) %>% addTiles() %>% 
  addMarkers(~long, ~lat, popup = ~as.character("name-en")) %>%
  setView(lng = 114.183413 , lat = 22.248648, zoom=12)
