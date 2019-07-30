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

#' Public housing estates (Annually update)
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

