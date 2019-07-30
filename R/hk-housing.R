#-----------------------------------------------------------------------------#
# HONG KONG HOUSING RELATED DATA
#-----------------------------------------------------------------------------#
#' Address lookup service API
#' 
#' @param address free text for address
#' @param n maxium number of results to return, default to 1
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


