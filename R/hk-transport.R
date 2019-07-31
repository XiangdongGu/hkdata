


# (1) Details of HK Electric EV charging stations
# https://data.gov.hk/en-data/dataset/hkelectric-tnd_cs_ci-hkelectric-ev-location/resource/d4aa7487-1eb7-4b95-b552-d0c7b0029415
#
# Given the lat_in and long_in of the place, the function can return the information of the charging stations around the place and
# arranged by the distances

EV_charging_stations <- function(lat_in,long_in) {
  require(dplyr)

  url <- paste("https://api.data.gov.hk/v1/nearest-hke-electric-vehicle-charging-stations?lat=",lat_in,sep="")
  url <- paste(url,"&long=",sep="")
  url <- paste(url,long_in,sep="")
  temp_file <- get_file_json(url)
  temp_file <- temp_file$results

  temp_file <- temp_file %>%
    mutate(lat = unlist(lapply(1:length(test1), function(x){test1$'lat-long'[x][[1]][1]})),
           long = unlist(lapply(1:length(test1), function(x){test1$'lat-long'[x][[1]][2]})),
           distance = sqrt((lat-lat_in)^2+(long-long_in)^2))  %>%
    arrange(distance)


  return(temp_file)

}




# #Example:
# test1<-EV_charging_stations(22,114)






# (2) Arrival Information for Cross Boundary Ferry Service
# https://data.gov.hk/en-data/dataset/hk-md-mardep-crossboundaryferryservices-arrive/resource/c0b4b251-99c5-472d-adc9-bdd1dfd6c6b8
#
# Generate the historical information of the every arrival information, such as the arrival time, origin, operator, terminal, berth
#
# UPDATE FREQUENCY: EVERY 5 MINUTES



#hist_file_versions(url = "https://www.mardep.gov.hk/e_files/en/opendata/arrival_en.csv",start = "2019-07-29")




Arrival_CrossBoundaryFerry <- function(timestamp){
  #timestamp format: "20190729-0915"
  url <- "https://www.mardep.gov.hk/e_files/en/opendata/arrival_en.csv"
  url <- hist_file_url(url,timestamp)
  temp_file <-read.csv(url, sep = "|")

}


# #Example
# test1 <- Arrival_CrossBoundaryFerry("20190729-1004")






# (3) Flight Information (daily, through API)
# https://data.gov.hk/en-data/dataset/aahk-team1-flight-info
#
# Instruction: https://www.hongkongairport.com/iwov-resources/misc/opendata/Flight_Information_DataSpec_en.pdf
# UPDATE FREQUENCY: DAILY (UPDATED TO PREVIOUS CALENDAR DAY)



# https://www.hongkongairport.com/flightinfo-rest/rest/flights/past?date=%3Cdate%3E&lang=en&cargo=false&arrival=false


Arrival_Flight <- function(date){
  #date format:  "YYYY-MM-DD"
  require(tidyr)
  url = paste("https://www.hongkongairport.com/flightinfo-rest/rest/flights/past?date=",date,sep="")
  url = paste(url,"&lang=en&cargo=false&arrival=false",sep="")
  temp_file = get_file_json(url)
  temp_file = crossing(temp_file["date","arrival","cargo"],temp_file[[4]][[1]])

}

test1 <- Arrival_Flight("2019-05-01")

