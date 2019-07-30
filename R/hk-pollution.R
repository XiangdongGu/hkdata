#-----------------------------------------------------------------------------#
# HONG KONG POLLUTION DATA
#-----------------------------------------------------------------------------#

#' Retrieve Air Quality Health Index (AQHI) at individual general and roadside Air Quality Monitoring stations for the past 24 hours
#' Reference: https://data.gov.hk/en-data/dataset/hk-epd-airteam-past24hr-aqhi-of-individual-air-quality-monitoring-stations
#'
#' @param timestamp if null then current, otherwise historical weather
#' it should be in format of \%Y\%m\%d-\%H\%M, e.g. 20180905-0100
#'
#' @export
#'
aqhi_24hr <- function(timestamp = NULL) {
  require(rvest)
  require(httr)
  require(dplyr)
  url <- "http://www.aqhi.gov.hk/epd/ddata/html/out/24aqhi_Eng.xml"
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
