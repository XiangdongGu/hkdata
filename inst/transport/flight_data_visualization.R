library(hkdata)
library(dplyr)
library(plotly)

######################################################################################################

#If we are interested in the flight data for some date, say "2019-07-01"
#then we can easily retrieve the relevant data using the wrapper function in the library

#Definr a functuon to extract the flight count data for some day 

Extract_FlightInfo_day <-function(Date){
  
  test1 <- transport_Flight_retrieve(Date,FALSE,FALSE) # departure for passengers
  
  test2 <- transport_Flight_retrieve(Date,TRUE,FALSE) # arrival for passengers
  
  test3 <- transport_Flight_retrieve(Date,FALSE,TRUE) # departure for cargo
  
  test4 <- transport_Flight_retrieve(Date,TRUE,TRUE) # arrival for cargo
  
  
  test1.1 <- test1 %>%
    select(time,terminal,aisle,gate,destination) %>%
    distinct() %>%
    mutate(Time_Hour = substr(time,1,2), Category = "Departure for Passengers") %>%
    group_by(Category, Time_Hour) %>%
    summarize(Flight_Count = n()) 
  
  
  
  test2.1 <- test2 %>%
    select(time,baggage,hall,terminal,stand,origin) %>%
    distinct() %>%
    mutate(Time_Hour = substr(time,1,2), Category = "Arrival for Passengers") %>%
    group_by(Category, Time_Hour) %>%
    summarize(Flight_Count = n())
  
  
  
  test3.1 <- test3 %>%
    select(destination,time) %>%
    distinct() %>%
    mutate(Time_Hour = substr(time,1,2), Category = "Departure for Cargo") %>%
    group_by(Category, Time_Hour) %>%
    summarize(Flight_Count = n())
  
  
  
  test4.1 <- test4 %>%
    select(time,origin) %>%
    distinct() %>%
    mutate(Time_Hour = substr(time,1,2), Category = "Arrival for Cargo") %>%
    group_by(Category, Time_Hour) %>%
    summarize(Flight_Count = n())
  
  
  test5.1 <- rbind(test1.1, test2.1)
  test5.1 <- rbind(test5.1, test3.1)
  test5.1 <- rbind(test5.1, test4.1)
  
  return(test5.1)
}

#Retrive flight count data for some day, say "2019-07-01"

test1 <- Extract_FlightInfo_day("2019-07-01")


#Interactive plot for the frequencies over hours on that day


p5 <- test1 %>%
  plot_ly(x=~Time_Hour, y=~Flight_Count, color = ~Category) %>%
  layout(title = "Summary of Hourly Flight Counts for 2019-07-01",
         xaxis = list(title = "Hours within One Day"),
         yaxis = list(title = "Flight Counts per Hour"))

p5

######################################################################################################

#For some day, say "2019-07-01", if some one is interested in the detailed information about the 
#the destinations of passengers, then we can easily retreive the data using the the wrapper function
#in the library and then visualize the information in the map.



#Get Longitude and Latitude information from another open data
#Please refer to https://openflights.org/data.html

dff <- read.csv("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat",
                header = FALSE)

names(dff) <- c("AriportID","Name","City","Country","IATA","ICAO","Lat","Long","Alt","TZ","DST","Tz","Type","Source")



#Retrieve the data of departure for passengers

test1 <- transport_Flight_retrieve("2019-07-01",FALSE,FALSE)

#save the information of related worldwide airports
test1.1   <- test1 %>%
  select(time,terminal,aisle,gate,destination) %>%
  distinct() %>%
  group_by(destination) %>%
  summarise(cnt = n()) %>%
  left_join(dff,by=c("destination"="IATA")) %>%
  select("destination","Name","City","Lat","Long","cnt")

#save the geographic information about the airlines 
test1.2 <- test1 %>%
  select(time,terminal,aisle,gate,destination) %>%
  distinct() %>%
  left_join(dff,by=c("destination"="IATA")) %>%
  select("destination","Lat","Long") %>%
  mutate(Lat1 = dff[2916,c("Lat")], Long1 = dff[2916,c("Long")])



# airport locations
air <- test1.1
# flights between airports
flights <- test1.2
flights$id <- seq_len(nrow(flights))

# map projection
geo <- list(
  projection = list(
    type = 'orthographic',
    rotation = list(lon = -100, lat = 40, roll = 0)
  ),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  countrycolor = toRGB("gray80")
)

plot_geo(color = I("red")) %>%
  add_markers(
    data = air, x = ~Long, y = ~Lat, text = ~Name,
    size = ~cnt, hoverinfo = "text", alpha = 0.5
  ) %>%
  add_segments(
    data = group_by(flights, id),
    x = ~Long1, xend = ~Long,
    y = ~Lat1, yend = ~Lat,
    alpha = 0.3, size = I(1), hoverinfo = "none"
  ) %>%
  layout(geo = geo, 
         title = "Visualization of the Flight Departure on 2019-07-01",
         showlegend = FALSE)

######################################################################################################

#For some day, say "2019-07-01", if some one is interested in the detailed information about the 
#the flight cargo arrival, then we can easily retreive the data using the the wrapper function
#in the library and then visualize the information in the map.



#Get Longitude and Latitude information from another open data
#Please refer to https://openflights.org/data.html

dff <- read.csv("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat",
                header = FALSE)

names(dff) <- c("AriportID","Name","City","Country","IATA","ICAO","Lat","Long","Alt","TZ","DST","Tz","Type","Source")



#Retrieve the data of arrival of cargos

test2 <- transport_Flight_retrieve("2019-07-01",TRUE,TRUE)

#save the information of related worldwide airports
test2.1   <- test2 %>%
  select(time,origin) %>%
  distinct() %>%
  group_by(origin) %>%
  summarise(cnt = n()) %>%
  left_join(dff,by=c("origin"="IATA")) %>%
  select("origin","Name","City","Lat","Long","cnt")

#save the geographic information about the airlines 
test2.2 <- test2 %>%
  select(time,origin) %>%
  distinct() %>%
  left_join(dff,by=c("origin"="IATA")) %>%
  select("origin","Lat","Long") %>%
  mutate(Lat1 = dff[2916,c("Lat")], Long1 = dff[2916,c("Long")])



# airport locations
air <- test2.1
# flights between airports
flights <- test2.2
flights$id <- seq_len(nrow(flights))

# map projection
geo <- list(
  projection = list(
    type = 'orthographic',
    rotation = list(lon = -100, lat = 40, roll = 0)
  ),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  countrycolor = toRGB("gray80")
)

plot_geo(color = I("green")) %>%
  add_markers(
    data = air, x = ~Long, y = ~Lat, text = ~Name,
    size = ~cnt, hoverinfo = "text", alpha = 0.5
  ) %>%
  add_segments(
    data = group_by(flights, id),
    x = ~Long1, xend = ~Long,
    y = ~Lat1, yend = ~Lat,
    alpha = 0.3, size = I(1), hoverinfo = "none"
  ) %>%
  layout(geo = geo, 
         title = "Visualization of the Flight Cargo Arrival on 2019-07-01",
         showlegend = FALSE)
