
#-----------------------------------------------------------------------------#
# HONG KONG TRANSPORT DATA
#-----------------------------------------------------------------------------#



# (1) Details of HK Electric EV charging stations
# https://data.gov.hk/en-data/dataset/hkelectric-tnd_cs_ci-hkelectric-ev-location/resource/d4aa7487-1eb7-4b95-b552-d0c7b0029415
#
# Given the lat_in and long_in of current place, the function can return the information of the charging stations around the place and
# arranged by the distances in ascending order.


#' Retrieve details of nearby Electric EV charging stations
#'
#' @param lat_in Input the latitude of the current place
#' @param long_in Input the longitude of the current place
#'
#' @export
#'
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


#' Generate the basic url for Arrival Information for Cross Boundary Ferry Service
#'
#' @param Lang Options of Language: "sc": simplified Chinese; "tc": traditional Chinese; "en": English (default)
#'
#' @export
#'
transport_Arrival_CrossBoundaryFerry_url <- function(Lang ="en"){
  if (Lang == "sc"){
    return("https://www.mardep.gov.hk/e_files/sc/opendata/arrival_sc.csv")
  } else if (Lang == "tc"){
    return("https://www.mardep.gov.hk/e_files/hk/opendata/arrival_tc.csv")
  } else {
    return("https://www.mardep.gov.hk/e_files/en/opendata/arrival_en.csv")
  }
}


#' Retrieve the Arrival Information for Cross Boundary Ferry Service given specific data url
#'
#' @param data_url Specific data url
#'
#' @export
#'
transport_Arrival_CrossBoundaryFerry_retrieve <- function(data_url){
  temp_file <-read.csv(data_url, sep = "|")
  return(temp_file)
}


#' Retrieve the Arrival Information for Cross Boundary Ferry Service given specific timestamp and language version
#'
#' @param timestamp Timestamp of the historical file to retrieve. If null then
#'   current, otherwise historical. It should be in format of \%Y\%m\%d-\%H\%M,
#'   e.g. 20180905-1306.
#' @param Lang Options of Language: "sc": simplified Chinese; "tc": traditional Chinese; "en": English (default)     
#'
#' @export
#'
transport_Arrival_CrossBoundaryFerry <- function(timestamp = NULL, Lang = "en"){
  data_url <- data_file_url(transport_Arrival_CrossBoundaryFerry_url(Lang), timestamp)
  temp_file <- transport_Arrival_CrossBoundaryFerry_retrieve(data_url)
  return(temp_file)
}



# #Example for retrieving data for specific timestamp
# test1 <- transport_Arrival_CrossBoundaryFerry("20190729-1004")
# 
# #Example for retrieving all historical versions of data for specified time range
# #One can go to website https://data.gov.hk/en-data/dataset/hk-md-mardep-crossboundaryferryservices-arrive/resource/c0b4b251-99c5-472d-adc9-bdd1dfd6c6b8
# #to check the versions of data within some range.
# #This example can retrieve  all versions of data within specified range
# test2 <- get_file_versions(url = transport_Arrival_CrossBoundaryFerry_url(),
#                            start = "2019-07-31", retrieve_func = transport_Arrival_CrossBoundaryFerry_retrieve)
 




# (3) Flight Information (daily, through API)
# https://data.gov.hk/en-data/dataset/aahk-team1-flight-info
#
# Instruction: https://www.hongkongairport.com/iwov-resources/misc/opendata/Flight_Information_DataSpec_en.pdf
# UPDATE FREQUENCY: DAILY (UPDATED TO PREVIOUS CALENDAR DAY)

# https://www.hongkongairport.com/flightinfo-rest/rest/flights/past?date=%3Cdate%3E&lang=en&cargo=false&arrival=false




#' Generate the Flight Information given the date, arrival or departure, cargo or passenger for different language versions
#'
#' @param Date String, "YYYY-MM-DD" (The data is up-to-date to the previous calendar day.)
#'
#' @param Arrival Arrival or departure (TRUE=arrival, FALSE=departure)
#'
#' @param Cargo Cargo or passenger flight (TRUE=cargo, FALSE=passenger)
#' 
#' @param Lang Options of Language: "zh_HK": traditional Chinese; "zh_CN": simplified Chinese; "en": English (default)
#'
#' @export
#'
transport_Flight_retrieve <- function(Date, Arrival, Cargo, Lang = "en") {
  
  
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


#' Generate the list of Flight Information given the time range, arrival or departure, cargo or passenger for different language versions
#'
#' @param start String, "YYYY-MM-DD"  starting date of interest
#' 
#' @param end String, "YYYY-MM-DD"  ending date of interest
#'
#' @param Arrival Arrival or departure (TRUE=arrival, FALSE=departure)
#'
#' @param Cargo Cargo or passenger flight (TRUE=cargo, FALSE=passenger)
#' 
#' @param Lang Options of Language: "zh_HK": traditional Chinese; "zh_CN": simplified Chinese; "en": English (default)
#'
#' @export
#'
transport_Flight <- function(start, end, Arrival, Cargo, Lang = "en") {
  lapply(seq(as.Date(start), as.Date(end),"days"), function(x){transport_Flight_retrieve(x,Arrival, Cargo, Lang)}  )
}



# #Example for retrieving flight information data for sepecified day
# 
# test1<-transport_Flight_retrieve("2019-07-01",FALSE,FALSE)
# 
# test2<-transport_Flight_retrieve("2019-07-01",TRUE,FALSE,"zh_HK")
# 
# test3<-transport_Flight_retrieve("2019-07-01",FALSE,TRUE,"zh_CN")
# 
# test4<-transport_Flight_retrieve("2019-07-01",TRUE,TRUE)
# 
# #Example for retrieving flight information data for the time range. It returns the list of data frame for each date within the range. 
# test5 <- transport_Flight("2019-07-29","2019-07-31",FALSE,FALSE)
  




# (4)Parking vacancy data
# https://data.gov.hk/en-data/dataset/hk-td-tis_5-real-time-parking-vacancy-data
# 
# Instructions: http://static.data.gov.hk/td/parking-vacancy/dataspec/TD_Parking_Vacancy_Data_Specification.pdf



# Two sub_tables:
#   
# (4.1)  Basic Information of Participating Car Parks
# https://data.gov.hk/en-data/dataset/hk-td-tis_5-real-time-parking-vacancy-data/resource/01f937a6-323d-4347-bd2e-e366a8ae3375
# UPDATE FREQUENCY: FROM HOURLY TO REAL-TIME


#' Generate the basic url for Basic Information of Participating Car Parks
#'
#'
#' @export
#'
transport_carpark_basic_url <- function(){
  return("http://resource.data.one.gov.hk/td/carpark/basic_info_all.json")
}


#' Retrieve the Basic Information of Participating Car Parks given specific data url
#'
#' @param data_url Specific data url
#'
#' @export
#'
transport_carpark_basic_retrieve <- function(data_url){
  temp_file = get_file_json(data_url)
  temp_file = temp_file[[1]]
  return(temp_file)
}



#' Retrieve the Basic Information of Participating Car Parks given specific timestamp 
#'
#' @param timestamp Timestamp of the historical file to retrieve. If null then
#'   current, otherwise historical. It should be in format of \%Y\%m\%d-\%H\%M,
#'   e.g. 20180905-1306.
#' @export
#'
transport_carpark_basic <- function(timestamp = NULL){
  data_url <- data_file_url(transport_carpark_basic_url(), timestamp)
  temp_tile <- transport_carpark_basic_retrieve(data_url)
  return(temp_tile)
}



# #Example to retrieve data of asic Information of Participating Car Parks given timestamp
# test1<-transport_carpark_basic("20190705-0015")
# 
# #Example for retrieving all historical versions of data for specified time range
# #One can go to website https://data.gov.hk/en-data/dataset/hk-td-tis_5-real-time-parking-vacancy-data/resource/01f937a6-323d-4347-bd2e-e366a8ae3375
# #to check the versions of data within some range.
# #This example can retrieve all versions of data within specified range
# test2 <- get_file_versions(url = transport_carpark_basic_url(),
#                            start = "2019-07-31", retrieve_func = transport_carpark_basic_retrieve)




# (4.2) Parking Vacancy Data of Participating Car Parks
# https://data.gov.hk/en-data/dataset/hk-td-tis_5-real-time-parking-vacancy-data/resource/ab3c69cf-72fa-4f0a-92c1-cf97a08d3a79
# UPDATE FREQUENCY: FROM HOURLY TO REAL-TIME



#' Generate the basic url for Parking Vacancy Data of Participating Car Parks
#'
#' @export
#'
transport_carpark_vacancy_url <- function(){
  return("http://resource.data.one.gov.hk/td/carpark/vacancy_all.json")
}


#' Retrieve the Parking Vacancy Data of Participating Car Parks given specific data url
#'
#' @param data_url Specific data url
#'
#' @export
#'
transport_carpark_vacancy_retrieve <- function(data_url){
  require(data.table)
  
  temp_file = get_file_json(data_url)
  temp_file <-  unnest(temp_file[[1]])
  temp_file  <- lapply(1:nrow(temp_file), function(x){
    unnest(temp_file[x,])
  })
  temp_file <- rbindlist(temp_file)
  return(temp_file)
  
}



#' Retrieve the Parking Vacancy Data of Participating Car Parks given specific timestamp 
#'
#' @param timestamp Timestamp of the historical file to retrieve. If null then
#'   current, otherwise historical. It should be in format of \%Y\%m\%d-\%H\%M,
#'   e.g. 20180905-1306.
#' @export
#'
transport_carpark_vacancy <- function(timestamp = NULL){
  data_url <- data_file_url(transport_carpark_vacancy_url(), timestamp)
  temp_tile <- transport_carpark_vacancy_retrieve(data_url)
  return(temp_tile)
}




# #Example
# test1 <- transport_carpark_vacancy("20190701-0030")
# 
# #Example for retrieving all historical versions of data for specified time range
# #One can go to website https://data.gov.hk/en-data/dataset/hk-td-tis_5-real-time-parking-vacancy-data/resource/ab3c69cf-72fa-4f0a-92c1-cf97a08d3a79
# #to check the versions of data within some range.
# #This example can retrieve all versions of data within specified range
# test2 <- get_file_versions(url = transport_carpark_vacancy_url(),
#                            start = "2019-07-31", retrieve_func = transport_carpark_vacancy_retrieve)



