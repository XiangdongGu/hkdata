#-----------------------------------------------------------------------------#
# HONG KONG POLLUTION DATA
#-----------------------------------------------------------------------------#

#' Retrieve Air Quality Health Index (AQHI) at individual general and roadside Air Quality Monitoring stations for the past 24 hours
#' Reference: https://data.gov.hk/en-data/dataset/hk-epd-airteam-past24hr-aqhi-of-individual-air-quality-monitoring-stations
#'
#'
#' @export
#'
aqhi_24hr_url <- function(lang = "en") {
  # TODO: add lang
  "http://www.aqhi.gov.hk/epd/ddata/html/out/24aqhi_Eng.xml"
}

#' Retrieve Air Quality Health Index (AQHI) at individual general and roadside
#' Air Quality Monitoring stations for the past 24 hours Reference:
#' https://data.gov.hk/en-data/dataset/hk-epd-airteam-past24hr-aqhi-of-individual-air-quality-monitoring-stations
#'
#' @param data_url the url to the specific file, e.g. as returned by
#'   hist_file_url() for a given timestamp
#'
#' @export
#' 
aqhi_24hr_retrieve <- function(data_url) {
  require(rvest)
  require(httr)
  require(dplyr)
  
  res <- content(GET(data_url), encoding = 'UTF-8')
  if (identical("NOT FOUND", res$message)) {
    stop("Unable to retrieve information, input timestamp may not be available
         - try to find the available timestamps.")
  }
  # pick out the components
  res_headers <- sapply(c("title", "link", "description", "language", "copyright", "webMaster", "lastBuildDate"),
                        FUN = function(n) {res %>% html_node(xpath = n) %>% html_text()},
                        simplify = FALSE, USE.NAMES = TRUE)
  res_items <- lapply(res %>% html_nodes(xpath = "item"),
                      function(n) {
                        sapply(c("type", "StationName", "DateTime", "aqhi"),
                               function(x) {n %>% html_node(xpath = x) %>% html_text()},
                               simplify = FALSE, USE.NAMES = TRUE) %>%
                          data.frame(stringsAsFactors = FALSE)
                      })
  res_data <- do.call("rbind", res_items)
  
  list(header = res_headers, data = res_data)
}

#' Retrieve Air Quality Health Index (AQHI) at individual general and roadside Air Quality Monitoring stations for the past 24 hours
#' Reference: https://data.gov.hk/en-data/dataset/hk-epd-airteam-past24hr-aqhi-of-individual-air-quality-monitoring-stations
#'
#' @param timestamp if null then current, otherwise historical weather
#' it should be in format of \%Y\%m\%d-\%H\%M, e.g. 20180905-0100
#'
#' @export
#'
aqhi_24hr <- function(timestamp = NULL) {
  aqhi_24hr_retrieve(data_file_url(aqhi_24hr_url(), timestamp))
}

#' Retrieve Past 24-hour Pollutant Concentration of individual Air Quality Monitoring stations
#' Reference: https://data.gov.hk/en-data/dataset/hk-epd-airteam-past24hr-pc-of-individual-air-quality-monitoring-stations
#'
#' @param timestamp if null then current, otherwise historical weather
#' it should be in format of \%Y\%m\%d-\%H\%M, e.g. 20180905-0100
#'
#' @export
#'
pollutant_24hr <- function(timestamp = NULL) {
  require(rvest)
  require(httr)
  require(dplyr)
  url <- "http://www.aqhi.gov.hk/epd/ddata/html/out/24pc_Eng.xml"
  if (!is.null(timestamp)) {
    valid <- strptime(timestamp, "%Y%m%d-%H%M")
    if (is.na(valid)) stop("Invalid timestamp format")
    url <- hist_file_url(url, timestamp)
  }
  res <- content(GET(url), encoding = 'UTF-8')
  if (identical("NOT FOUND", res$message)) {
    stop("Unable to retrieve information, input timestamp may not be available
         - try to find the available timestamps.")
  }
  # pick out the components
  res_headers <- sapply(c("title", "link", "description", "language", "copyright", "webMaster", "lastBuildDate"),
                        FUN = function(n) {res %>% html_node(xpath = n) %>% html_text()},
                        simplify = FALSE, USE.NAMES = TRUE)
  res_items <- lapply(res %>% html_nodes(xpath = "PollutantConcentration"),
                      function(n) {
                        sapply(c("StationName", "DateTime", "NO2", "O3", "SO2", "CO", "PM10", "PM2.5"),
                               function(x) {n %>% html_node(xpath = x) %>% html_text()},
                               simplify = FALSE, USE.NAMES = TRUE) %>%
                          data.frame(stringsAsFactors = FALSE)
                      })
  res_data <- do.call("rbind", res_items)
  
  list(header = res_headers, data = res_data)
}

#' Retrieve Current Air Quality Health Index of individual Air Quality Monitoring stations
#' Reference: https://data.gov.hk/en-data/dataset/hk-epd-airteam-current-aqhi-of-individual-air-quality-monitoring-stations
#'
#' @param timestamp if null then current, otherwise historical weather
#' it should be in format of \%Y\%m\%d-\%H\%M, e.g. 20180905-0100
#'
#' @export
#'
aqhi_current <- function(timestamp = NULL) {
  require(rvest)
  require(httr)
  require(dplyr)
  url <- "http://www.aqhi.gov.hk/epd/ddata/html/out/aqhi_ind_rss_Eng.xml"
  if (!is.null(timestamp)) {
    valid <- strptime(timestamp, "%Y%m%d-%H%M")
    if (is.na(valid)) stop("Invalid timestamp format")
    url <- hist_file_url(url, timestamp)
  }
  res <- content(GET(url), encoding = 'UTF-8')
  if (identical("NOT FOUND", res$message)) {
    stop("Unable to retrieve information, input timestamp may not be available
         - try to find the available timestamps.")
  }
  # pick out the components
  res_headers <- sapply(c(title = "//channel/title",
                          link = "//channel/link",
                          image_title = "//channel/image/title",
                          image_link = "//channel/image/link",
                          image_url = "//channel/image/url",
                          description = "//channel/description",
                          language = "//channel/language",
                          copyright = "//channel/copyright",
                          webMaster = "//channel/webMaster",
                          pubDate = "//channel/pubDate",
                          lastBuildDate = "//channel/lastBuildDate"),
                        FUN = function(n) {res %>% html_node(xpath = n) %>% html_text()},
                        simplify = FALSE, USE.NAMES = TRUE)

  res_items <- lapply(res %>% html_nodes("item"),
                      function(n) {
                        dat <- sapply(c("title", "guid", "link", "pubDate", "description"),
                                      function(x) {n %>% html_node(xpath = x) %>% html_text()},
                                      simplify = FALSE, USE.NAMES = TRUE)
                        loc_qahi <- trimws(strsplit(dat$title, split = ":")[[1]], "both")
                        data.frame(station = loc_qahi[1],
                                   aqhi = loc_qahi[2],
                                   risk = loc_qahi[3],
                                   guid = dat$guid,
                                   link = dat$link,
                                   pubDate = dat$pubDate,
                                   description = dat$description,
                                   stringsAsFactors = FALSE)
                      })
  res_data <- do.call("rbind", res_items)

  list(header = res_headers, data = res_data)
}

#' Retrieve Current Air Quality Health Index Range and Forecast
#' Reference: https://data.gov.hk/en-data/dataset/hk-epd-airteam-current-air-quality-health-index-range-and-forecast
#'
#' @param timestamp if null then current, otherwise historical weather
#' it should be in format of \%Y\%m\%d-\%H\%M, e.g. 20180905-0100
#'
#' @export
#'
aqhi_range_forecast <- function(timestamp = NULL) {
  require(rvest)
  require(httr)
  require(dplyr)
  url <- "http://www.aqhi.gov.hk/epd/ddata/html/out/aqhirss_Eng.xml"
  if (!is.null(timestamp)) {
    valid <- strptime(timestamp, "%Y%m%d-%H%M")
    if (is.na(valid)) stop("Invalid timestamp format")
    url <- hist_file_url(url, timestamp)
  }
  res <- content(GET(url), encoding = 'UTF-8')
  if (identical("NOT FOUND", res$message)) {
    stop("Unable to retrieve information, input timestamp may not be available
         - try to find the available timestamps.")
  }
  # pick out the components
  res_headers <- sapply(c(title = "//channel/title",
                          link = "//channel/link",
                          description = "//channel/description",
                          language = "//channel/language",
                          copyright = "//channel/copyright",
                          webMaster = "//channel/webMaster",
                          lastBuildDate = "//channel/lastBuildDate"),
                        FUN = function(n) {res %>% html_node(xpath = n) %>% html_text()},
                        simplify = FALSE, USE.NAMES = TRUE)

  res_items <- lapply(res %>% html_nodes("item"),
                      function(n) {
                        sapply(c("title", "guid", "link", "description"),
                               function(x) {n %>% html_node(xpath = x) %>% html_text()},
                               simplify = FALSE, USE.NAMES = TRUE)
                      })

  # the first is for now
  res_current <- res_items[[1]]
  type_date <- trimws(strsplit(res_current$title, split = ": ")[[1]], "both")
  res_current$data_desc <- type_date[1]
  res_current$dateTime <- strsplit(type_date[2], split = "[\t\n]")[[1]][1]
  aqhi_dat <- res_current$description %>% read_html() %>% html_nodes("p") %>% html_text() %>%
    strsplit(split = "[:()]") %>%
    lapply(FUN=function(x) {
      dat <- trimws(x, which = "both")
      data.frame(
        station_type = dat[1],
        aqhi = dat[2],
        risk = dat[4],
        stringsAsFactors = FALSE)
      })
  res_current_data <- do.call("rbind", aqhi_dat) %>%
    mutate(
      data_type = "current",
      data_desc = res_current$data_desc,
      dateTime = res_current$dateTime,
      guid = res_current$guid,
      link = res_current$link) %>%
    select(data_type, data_desc, dateTime, station_type, aqhi, risk, guid, link, everything())

  # the second is the forecast
  res_forecast <- res_items[[2]]
  type_date <- trimws(strsplit(res_forecast$title, split = ": ")[[1]], "both")
  res_forecast$data_desc <- type_date[1]
  aqhi_dat <- res_forecast$description %>% read_html() %>% html_nodes("p") %>% html_text()
  res_forecast_data <- do.call(
    "rbind",
    lapply(list(list(data_type = aqhi_dat[1], type_risk = aqhi_dat[2]),
                list(data_type = aqhi_dat[1], type_risk = aqhi_dat[3]),
                list(data_type = aqhi_dat[4], type_risk = aqhi_dat[5]),
                list(data_type = aqhi_dat[4], type_risk = aqhi_dat[6])),
           function(x) {
             tmp <- strsplit(x$type_risk, split = ":")[[1]] %>% trimws(which = "both")
             data.frame(data_type = x$data_type,
                        station_type = tmp[1],
                        risk = tmp[2])
           })
  ) %>%
    mutate(
      data_desc = res_forecast$data_desc,
      dateTime = NA_character_,
      aqhi = NA_character_,
      guid = res_forecast$guid,
      link = res_forecast$link
    ) %>%
    select(data_type, data_desc, dateTime, station_type, aqhi, risk, guid, link, everything())

  #
  list(header = res_headers, data = rbind(res_current_data, res_forecast_data))
}

#' URL for Past hourly record of Air Pollution Index at a given year and month
#' 
#' Reference: https://data.gov.hk/en-data/dataset/hk-epd-airteam-past-record-of-air-pollution-index-en
#' Only available from July 1999 to Decemenber 2013.
#'
#' @param year year of the past record
#' @param month month of the past record
#'
#' @export
#'
past_pollution_index_url <- function(year, month) {
  if(floor(month)!=month || month < 1 || month > 12) stop("Month should be integer from 1 to 12. Got ", month)
  sprintf(
    "http://www.aqhi.gov.hk/api_history/download/hourly/eng/hr%02d%04d.csv",
    month,
    year
  )
}

#' Retrieve Past hourly record of Air Pollution Index at a given year and month
#' 
#' Reference: https://data.gov.hk/en-data/dataset/hk-epd-airteam-past-record-of-air-pollution-index-en
#' Only available from July 1999 to Decemenber 2013.
#'
#' @param year year of the past record
#' @param month month of the past record
#' @param path the directory where the raw file should be save, if NULL it
#' will not be saved
#'
#' @export
#'
past_pollution_index <- function(year, month, path = NULL) {
  require(readr)
  require(tidyr)
  
  url <- past_pollution_index_url(year, month)
  if (is.null(path)) {
    path <- file.path(tempdir(), basename(url))
    on.exit(unlink(path))
  } else {
    path <- file.path(path, basename(url))
  }
  download.file(url, path, quiet = TRUE)
  # since the file contains variable number of rows (ranging from 8 to 10) before the header row,
  # so use grep to find the first row with header, containing "Date"
  r <- read_lines(path, n_max = 15)
  dt <- min(grep("^Date", r))
  
  # not sure if the fields may have something like "4*", so read as characters to be safe
  data <- read_csv(path, skip = dt - 1, col_types = cols(.default = "c"))
  # still some clean up needed, only hour 0 has Date, the Date for other hours are omitted
  data %>% fill(Date)
}

#' URL for Past record of Air Quality Health Index
#' 
#' Reference: https://data.gov.hk/en-data/dataset/hk-epd-airteam-past-record-of-air-quality-health-index-en
#' Only available from Decemenber 2013 onward.
#'
#' @param year year of the past record
#' @param month month of the past record
#'
#' @export
#'
past_aqhi_url <- function(year, month) {
  if(floor(month)!=month || month < 1 || month > 12) stop("Month should be integer from 1 to 12. Got ", month)
  sprintf(
    "http://www.aqhi.gov.hk/epd/ddata/html/history/%04d/%04d%02d_Eng.csv",
    year,
    year,
    month
  )
}

#' Retrieve Past record of Air Quality Health Index
#' 
#' Reference: https://data.gov.hk/en-data/dataset/hk-epd-airteam-past-record-of-air-quality-health-index-en
#' Only available from Decemenber 2013 onward.
#'
#' @param year year of the past record
#' @param month month of the past record
#' @param path the directory where the raw file should be save, if NULL it
#' will not be saved
#'
#' @export
#'
past_aqhi <- function(year, month, path = NULL) {
  require(readr)
  require(tidyr)
  
  url <- past_aqhi_url(year, month)
  if (is.null(path)) {
    path <- file.path(tempdir(), basename(url))
    on.exit(unlink(path))
  } else {
    path <- file.path(path, basename(url))
  }
  download.file(url, path, quiet = TRUE)
  # the files all have the header at line 8, but would still use the dynamic approach to be sure.
  # to use grep to find the first row with header, containing "Date"
  r <- read_lines(path, n_max = 15)
  dt <- min(grep("^Date", r))
  # the fields for aqhi are not all numeric, some are "6*", so keep them as character
  # even "Hour" has "Daily Max", so not numeric
  data <- read_csv(path, skip = dt - 1, col_types = cols(.default = "c"))
  # still some clean up needed, only hour 1 has Date, the Date for other hours are omitted
  data %>% fill(Date)
}

#' URL for Towngas Environmental Performance Data
#' 
#' Reference: https://data.gov.hk/en-data/dataset/towngas-towngas-environment
#'
#' @param fromYear integer, the starting year of the performance data.
#' @param toYear integer, the ending (inclusive) year of the performance data.
#'
#' @export
#'
towngas_performance_data_url <- function(fromYear, toYear = fromYear) {
  if(floor(fromYear) != fromYear) stop("fromYear should be integer. Got ", fromYear)
  if(floor(toYear) != toYear) stop("toYear should be integer. Got ", toYear)
  sprintf(
    "https://appapi.towngas.com/opendata/v1/environment/filter/%04d/%04d",
    fromYear, toYear
  )
}

#' Retrieve Towngas Environmental Performance Data
#' 
#' Reference: https://data.gov.hk/en-data/dataset/towngas-towngas-environment
#'
#' @param fromYear integer, the starting year of the performance data.
#' @param toYear integer, the ending (inclusive) year of the performance data.
#' @param path the directory where the raw file should be save, if NULL it
#' will not be saved
#'
#' @export
#'
towngas_performance_data <- function(fromYear, toYear = fromYear, path = NULL) {
  require(tidyr)
  url <- towngas_performance_data_url(fromYear, toYear)
  data <- get_file_json(url, path)
  # the returned data has two levels of nesting after the default simplification
  # of arrays of dataframe
  unnest(unnest(data))
}

#' Retrieve Real-time city data collected by multi-purpose lamp posts in Kowloon East
#' 
#' Reference: https://data.gov.hk/en-data/dataset/hk-devb-mplp-mplp-sensor-data
#' Note that only the most update value is available.
#'
#'#' @param path the directory where the raw file should be save, if NULL it
#' will not be saved
#' 
#' @export
#'
lamp_posts_data <- function(path = NULL) {
  require(tidyr)
  url <- "https://mplpssl.wisx.io/nodered/getlampposts/"
  data <- get_file_json(url, path)
  # the returned data has one level of nesting after the default simplification
  # of arrays of dataframe
  unnest(data)
}

#' Retrieve Hong Kong Public Holidays Data
#' 
#' Reference: https://data.gov.hk/en-data/dataset/hk-effo-statistic-cal
#' 1823 currently provides data of Hong Kong Public Holidays for 2018-2020.
#'
#'#' @param path the directory where the raw file should be save, if NULL it
#' will not be saved
#' 
#' @export
#'
hk_holidays <- function(path = NULL) {
  require(tidyr)
  url <- "http://www.1823.gov.hk/common/ical/en.json"
  data <- get_file_json(url, path)
  
  vevents <- data$vcalendar$vevent[[1]]
  tmp1 <- vevents %>% mutate(dtstart = sapply(dtstart, function(x) x[[1]]),
                             dtend = sapply(dtend, function(x) x[[1]]))
  # the returned data has one level of nesting after the default simplification
  # of arrays of dataframe
  unnest(data)
}
