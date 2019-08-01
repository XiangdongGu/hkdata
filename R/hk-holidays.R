#' Retrieve Hong Kong Public Holidays Data
#' 
#' Reference: https://data.gov.hk/en-data/dataset/hk-effo-statistic-cal
#' 1823 currently provides data of Hong Kong Public Holidays for 2018-2020.
#' 
#' @param lang select the language of the available data, available choices are
#'   "en" for English, "tc" for traditional Chinese, "sc" for simplified Chinese
#'
#' @export
#'
hk_holidays_url <- function(lang = c("en", "tc", "sc")) {
  lang = match.arg(lang)
  switch(lang,
         en = "http://www.1823.gov.hk/common/ical/en.json",
         tc = "http://www.1823.gov.hk/common/ical/tc.json",
         sc = "http://www.1823.gov.hk/common/ical/sc.json"
  )
}

#' Retrieve Hong Kong Public Holidays Data
#' 
#' Reference: https://data.gov.hk/en-data/dataset/hk-effo-statistic-cal
#' 1823 currently provides data of Hong Kong Public Holidays for 2018-2020.
#'
#' @param path the directory where the raw file should be save, if NULL it
#' will not be saved
#' @param lang select the language of the available data, available choices are
#'   "en" for English, "tc" for traditional Chinese, "sc" for simplified Chinese
#' 
#' @export
#'
hk_holidays <- function(path = NULL, lang = c("en", "tc", "sc")) {
  require(dplyr)
  url <- hk_holidays_url(lang)
  data <- get_file_json(url, path)
  
  events <- do.call(
    "rbind",
    lapply(data$vcalendar$vevent,
           function(v) {
             v %>% mutate(dtstart = sapply(dtstart, function(x) x[[1]]),
                          dtend = sapply(dtend, function(x) x[[1]]))
           })
  )
  
  list(header = data$vcalendar[c("prodid", "version", "calscale", "x-wr-timezone", "x-wr-calname", "x-wr-caldesc")],
       data = events)
}
