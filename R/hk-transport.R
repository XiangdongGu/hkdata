# (1) Details of HK Electric EV charging stations
# https://data.gov.hk/en-data/dataset/hkelectric-tnd_cs_ci-hkelectric-ev-location/resource/d4aa7487-1eb7-4b95-b552-d0c7b0029415
#
# Given the lat_in and long_in of the place, the function can return the information of the charging stations around the place and
# arranged by the distances

transport_EV_charging <- function(lat_in,long_in) {
  require(dplyr)
  
  url <- paste("https://api.data.gov.hk/v1/nearest-hke-electric-vehicle-charging-stations?lat=",lat_in,sep="")
  url <- paste(url,"&long=",sep="")
  url <- paste(url,long_in,sep="")
  temp_file <- get_file_json(url)
  temp_file <- temp_file$results
  
  temp_file <- temp_file %>%
    mutate(lat = unlist(lapply(1:nrow(temp_file), function(x){temp_file$"lat-long"[x][[1]][1]})),
           long = unlist(lapply(1:nrow(temp_file), function(x){temp_file$"lat-long"[x][[1]][2]})),
           distance = sqrt((lat-lat_in)^2+(long-long_in)^2))  %>%
    arrange(distance)
  
  
  return(temp_file)
  
}




# #Example:
# test1<-transport_EV_charging(22,114)






# (2) Arrival Information for Cross Boundary Ferry Service
# https://data.gov.hk/en-data/dataset/hk-md-mardep-crossboundaryferryservices-arrive/resource/c0b4b251-99c5-472d-adc9-bdd1dfd6c6b8
#
# Generate the historical information of the every arrival information, such as the arrival time, origin, operator, terminal, berth
#
# UPDATE FREQUENCY: EVERY 5 MINUTES


#Need consider how to show the possible timestamp

#hist_file_versions(url = "https://www.mardep.gov.hk/e_files/en/opendata/arrival_en.csv",start = "2019-07-29")




transport_Arrival_CrossBoundaryFerry <- function(timestamp){
  #timestamp format: "20190729-0915"
  url <- "https://www.mardep.gov.hk/e_files/en/opendata/arrival_en.csv"
  url <- hist_file_url(url,timestamp)
  temp_file <-read.csv(url, sep = "|")
  
}



# #Example
# test1 <- transport_Arrival_CrossBoundaryFerry("20190729-1004")






# (3) Flight Information (daily, through API)
# https://data.gov.hk/en-data/dataset/aahk-team1-flight-info
#
# Instruction: https://www.hongkongairport.com/iwov-resources/misc/opendata/Flight_Information_DataSpec_en.pdf
# UPDATE FREQUENCY: DAILY (UPDATED TO PREVIOUS CALENDAR DAY)



# https://www.hongkongairport.com/flightinfo-rest/rest/flights/past?date=%3Cdate%3E&lang=en&cargo=false&arrival=false



transport_Flight <- function(Date, Arrival, Cargo, Lang = "en") {
  
  # Parameter                   Description                                          Type
  # Date Date of the records.  The data is up-to-date to theprevious calendar day.   String, “YYYY-MM-DD”
  # Arrival                    Arrival or departure                                  Bool (TRUE=arrival, FALSE=departure)
  # Cargo                      Cargo or passenger flight                             Bool (TRUE=cargo, FALSE=passenger)
  # Lang                       Language                                              en, zh_HK or zh_CN
  
  require(tidyverse)
  require(data.table)
  
  url = paste("https://www.hongkongairport.com/flightinfo-rest/rest/flights/past?date=",Date,sep="")
  url = paste(url,"&lang=",sep = "")
  url = paste(url,Lang,sep = "")
  url = paste(url, "&cargo=",sep = "")
  url = paste(url, tolower(as.character(Cargo)), sep = "")
  url = paste(url, "&arrival=", sep = "")
  url = paste(url,tolower(as.character(Arrival)), sep = "")
  temp_file = get_file_json(url)
  
  
  temp1 <- temp_file %>%
    mutate_if(is.list, map, as_data_frame) %>%
    unnest() %>%
    select (-flight)
  
  
  
  if (Cargo){
    
    if (Arrival) {
      temp2 <- temp_file %>%
        mutate_if(is.list, map, as_data_frame) %>%
        unnest() %>%
        select (-origin)
    } else {
      temp2 <- temp_file %>%
        mutate_if(is.list, map, as_data_frame) %>%
        unnest() %>%
        select (-destination)
    }
    
    
    
    temp3 <- lapply(1:nrow(temp_file[[4]][[1]]), function(x){
      full_join(unnest(temp1[x,]),unnest(temp2[x,]),
                by = c("date", "arrival", "cargo","time", "statusCode", "status"))
    })
    
    
  }
  
  
  else{
    
    if (Arrival) {
      temp2 <- temp_file %>%
        mutate_if(is.list, map, as_data_frame) %>%
        unnest() %>%
        select (-origin)
    } else {
      temp2 <- temp_file %>%
        mutate_if(is.list, map, as_data_frame) %>%
        unnest() %>%
        select (-destination)
    }
    
    
    if (Arrival) {
      temp3 <- lapply(1:nrow(temp_file[[4]][[1]]), function(x){
        full_join(unnest(temp1[x,]),unnest(temp2[x,]),
                  by = c("date", "arrival", "cargo", "baggage", "hall", "terminal", "stand", "time","statusCode", "status" ))
      })
    } else {
      temp3 <- lapply(1:nrow(temp_file[[4]][[1]]), function(x){
        full_join(unnest(temp1[x,]),unnest(temp2[x,]),
                  by = c("date", "arrival", "cargo", "terminal", "aisle", "gate", "time", "statusCode", "status"))
      })
    }
    
    
  }
  
  
  
  temp3 <- rbindlist(temp3)
  
  return(temp3)
  
}



# #Example
# 
# test1<-transport_Flight("2019-05-01",FALSE,FALSE)
# 
# test2<-transport_Flight("2019-05-01",TRUE,FALSE)
# 
# test3<-transport_Flight("2019-05-01",FALSE,TRUE)
# 
# test4<-transport_Flight("2019-05-01",TRUE,TRUE)







# (4)Parking vacancy data
# https://data.gov.hk/en-data/dataset/hk-td-tis_5-real-time-parking-vacancy-data
# 
# Instructions: http://static.data.gov.hk/td/parking-vacancy/dataspec/TD_Parking_Vacancy_Data_Specification.pdf



# Two sub_tables:
#   
# (4.1)  Basic Information of Participating Car Parks
# https://data.gov.hk/en-data/dataset/hk-td-tis_5-real-time-parking-vacancy-data/resource/01f937a6-323d-4347-bd2e-e366a8ae3375
# UPDATE FREQUENCY: FROM HOURLY TO REAL-TIME

transport_carpark_basic <- function(timestamp){
  #timestamp format: "20190705-0015"
  url <- "http://resource.data.one.gov.hk/td/carpark/basic_info_all.json"
  url <- hist_file_url(url,timestamp)
  temp_file = get_file_json(url)
  
  temp_file <- temp_file[[1]]
  
}


# #Example  
# test1<-transport_carpark_basic("20190705-0015")





# (4.2) Parking Vacancy Data of Participating Car Parks
# https://data.gov.hk/en-data/dataset/hk-td-tis_5-real-time-parking-vacancy-data/resource/ab3c69cf-72fa-4f0a-92c1-cf97a08d3a79
# UPDATE FREQUENCY: FROM HOURLY TO REAL-TIME


transport_carpark_vacancy <- function(timestamp){
  require(data.table)
  #timestamp format: "20190701-0030"
  url <- "http://resource.data.one.gov.hk/td/carpark/vacancy_all.json"
  url <- hist_file_url(url,timestamp)
  temp_file = get_file_json(url)
  temp_file <-  unnest(temp_file[[1]])
  temp_file  <- lapply(1:nrow(temp_file), function(x){
    unnest(temp_file[x,])
  })
  temp_file <- rbindlist(temp_file)
  
  return(temp_file)
  
  
}

# #Example
# test1 <- transport_carpark_vacancy("20190701-0030")




