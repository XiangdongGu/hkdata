#-----------------------------------------------------------------------------#
# HONG KONG WEATHER DATA
#-----------------------------------------------------------------------------#

#' Retrieve current and historical weather information
#'
#' @param timestamp if null then current, otherwise historical weather
#' it should be in format of \%Y\%m\%d-\%H\%M, e.g. 20180905-0100
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
  weather_symbol <- text %>% html_node("img") %>% html_attr("src")
  summy <- text %>% html_node("p") %>% html_text()
  temperature <- gsub(".+Air temperature : ([[:digit:]]+) degrees Celsius.+",
                      "\\1", summy)
  humidity <- gsub(".+Humidity : ([[:digit:]]+) per.+",
                   "\\1", summy)
  list(
    pubdate = pubdate,
    weather_symbol = weather_symbol,
    temperature = as.numeric(temperature),
    humidity = as.numeric(humidity),
    temperature_area = tab
  )
}

#' Retrieve current and historical 9day weather forecast
#'
#' @param timestamp if null then current, otherwise historical weather
#' it should be in format of \%Y\%m\%d-\%H\%M, e.g. 20180905-0100
#'
#' @export
#'
weather_forecast_9day <- function(timestamp = NULL) {
  require(rvest)
  require(httr)
  require(dplyr)
  require(stringr)
  require(tidyr)
  url <- "http://rss.weather.gov.hk/rss/SeveralDaysWeatherForecast.xml"
  if (!is.null(timestamp)) {
    valid <- strptime(timestamp, "%Y%m%d-%H%M")
    if (is.na(valid)) stop("Invalid timestamp format")
    url <- hist_file_url(url, timestamp)
  }
  res <- content(GET(url), encoding = 'UTF-8')
  if (identical("NOT FOUND", res$message)) {
    stop("Unable to retrieve information, input timestamp may not be available
    - try to use exact hour like 20180101-1500")
  }
  pubdate <- res %>% html_node(xpath = "//item//title") %>% html_text()
  pubdate <- gsub("Bulletin updated at", "", pubdate)
  pubdate <- strptime(pubdate, "%H:%M HKT %d/%b/%Y", tz = "Asia/Hong_Kong")
  text <- res %>% html_node(xpath = "//item//description") %>% html_text()
  # Parse Forecast text
  text <- gsub("Sea surface.+$", "", text) # remove sea surface temp
  text <- strsplit(text, "<p/><p/>")[[1]]
  gen_situation <- gsub("\\t|\\n|<p/>|<p/>|<br/>", "", text[1])
  gen_situation <- gsub(".*General Situation:", "", gen_situation)
  text <- text[-1]
  weather <- lapply(text, function(x) {
    x <- gsub("\\t|\\n|<p/>|<p/>", "", x)
    x <- gsub("<br/>", "\n", x)
    d <- read.table(text = x, sep = ":")
    d$V1 <- tolower(gsub("[[:punct:]]", "", d$V1))
    d$V1 <- gsub("[[:space:]]", "_", d$V1)
    d %>% spread(V1, V2)
  })
  weather <- do.call("rbind", weather)
  p <- "([[:digit:][:space:]]+)-([[:digit:][:space:]]+).+"
  weather <- weather %>% mutate(
    datemonth = as.Date(datemonth, "%d/%m"),
    rh_low = as.numeric(gsub(p, "\\1", rh_range)),
    rh_high = as.numeric(gsub(p, "\\2", rh_range)),
    temp_low = as.numeric(gsub(p, "\\1", temp_range)),
    temp_high = as.numeric(gsub(p, "\\2", temp_range))
  ) %>%
    select(-rh_range, -temp_range)
  list(
    pubdate = pubdate,
    general_situation = gen_situation,
    weather = weather
  )
}

