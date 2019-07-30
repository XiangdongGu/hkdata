#-----------------------------------------------------------------------------#
# HONG KONG HOUSING RELATED DATA
#-----------------------------------------------------------------------------#
#' Address lookup service API
#' 
#' @param address free text for address
#' @param n maxium number of results to return, default to 1
#' 
#' @export
#' 
address_lookup <- function(address, n = 1) {
  url <- sprintf('https://www.als.ogcio.gov.hk/lookup?n=%s&q=%s', n, address)
  url <- URLencode(url)
  res <- httr::GET(url, add_headers(Accept = 'application/json'))
  res <- jsonlite::fromJSON(content(res, "text"), FALSE)
  data <- lapply(res$SuggestedAddress, function(x) {
    x <- x$Address$PremisesAddress
    y <- unlist(x)
    names(y) <- gsub("\\.", "", names(y))
    data.frame(rbind(y), stringsAsFactors = FALSE)
  })
  do.call("bind_rows", data)
}

#' Housing Authority's Public Rental Housing Stock (Quarterly update)
#' 
#' @export
#' 
public_housing_rental <- function() {
  # find relevant files
  require(dplyr)
  search <- list_hist_file(provider='hk-housing', format = "json")
  search <- search %>% filter(`dataset-id` == "hk-housing-emms-emms-housing-stock")
  urls <- search$url
  names(urls) <- search$`resource-name-en`
  data <- lapply(names(urls), function(x) {
    res <- GET(urls[x])
    cat(sprintf("Retrieving %s...\n", x))
    jsonlite::fromJSON(content(res, "text", encoding = "UTF-8"))$records
  })
  do.call("bind_rows", data)
}

#' Location and Profile of Public housing estates (Annually update)
#' 
#' @param lang the language: en, tc, sc
#' 
#' @export
#' 
public_housing_estates <- function(lang = 'en') {
  url <- paste0("https://data.housingauthority.gov.hk/psi/rest/export/prh-estates/?",
                "lang=%s&format=json")
  url <- sprintf(url, lang)
  res <- GET(url)
  jsonlite::fromJSON(content(res, "text"))$data
}

#' Transactions in the Sale of Home Ownership Scheme (HOS) Flats and 
#' Transactions in the Sale of Green Form Subsidised Home Ownership Scheme (GSH) 
#' Flats
#' 
#' @param year the year of data currently only 2018, will adjust when more data
#' available
#' 
#' @export
#' 
public_housing_trans <- function(year = 2018) {
  url1 <- paste0("https://data.housingauthority.gov.hk/psi/rest/export/ssfs-sale-",
                "transactions-018/?lang=en&format=json")
  url2 <- paste0("https://data.housingauthority.gov.hk/psi/rest/export/ssfs-sale-",
                 "transactions-G18/?lang=en&format=json")
  d1 <- jsonlite::fromJSON(content(GET(url1), "text"))$data
  d2 <- jsonlite::fromJSON(content(GET(url2), "text"))$data
  bind_rows(d1, d2)
}

