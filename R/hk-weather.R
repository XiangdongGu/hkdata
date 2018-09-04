#-----------------------------------------------------------------------------#
# HONG KONG WEATHER DATA
#-----------------------------------------------------------------------------#
#' Retrieve current/historical weather information
#'
#' @param timestamp if null then current, otherwise historical weather
#' should be in format of %Y%m%d-%H%M, e.g. 20180905-0100
#'
#' @export
#'
weather_current <- function(timestamp = NULL) {
  require(rvest)
  require(httr)
  require(dplyr)
  url <- "http://rss.weather.gov.hk/rss/CurrentWeather.xml"
  if (!is.null(timestamp)) {
    valid <- strptime(timestamp, "%Y%m%d-%H%M")
    if (is.na(valid)) stop("Invalid timestamp format")
    url <- hist_file_url(url, timestamp)
  }
  res <- content(GET(url), encoding = 'UTF-8')
  pubdate <- res %>% html_node(xpath = "//item//title") %>% html_text()
  pubdate <- gsub("Bulletin updated at", "", pubdate)
  pubdate <- strptime(pubdate, "%H:%M HKT %d/%m/%Y", tz = "Asia/Hong_Kong")
  text <- res %>% html_node(xpath = "//item//description") %>% html_text()
  text <- read_html(text)
  tab <- html_table(text)[[1]]
  if (!is.null(tab)) {
    names(tab) <- c("area", "temperature")
    tab <- tab %>%
      mutate(temperature = as.numeric(gsub("degrees|[[:punct:]]", "", temperature)))
  }
  summy <- text %>% html_node("p") %>% html_text()
  temperature <- gsub(".+Air temperature : ([[:digit:]]+) degrees Celsius.+",
                      "\\1", summy)
  humidity <- gsub(".+Humidity : ([[:digit:]]+) per.+",
                   "\\1", summy)
  list(
    pubdate = pubdate,
    temperature = as.numeric(temperature),
    humidity = as.numeric(humidity),
    temperature_area = tab
  )
}
