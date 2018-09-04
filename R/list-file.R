#' Historical Archive File list
#'
#' It lists at most max 500 results.
#'
#' @param start start date of a period
#' @param end end date of a period
#' @param category Category ID, see website for a list of categories
#' @param provider Provider ID, see website for a list of provider ID
#' @param format File format
#' @param search keyword search
#' @param order sort order
#' @param skip the first x number of records to omit
#'
#' @examples
#' list_hist_file("2015-01-01", "2018-01-01")
#'
#' list_hist_file("2015-01-01", "2018-01-01", category = "health")
#'
#' @export
#'
list_hist_file <- function(start, end, category = NULL, provider = NULL,
                           format = NULL, search = NULL, order = NULL,
                           skip = NULL) {
  require(httr)
  require(jsonlite)
  api_url <- "https://api.data.gov.hk/v1/historical-archive/list-files"
  start <- format(as.Date(start), "%Y%m%d")
  end <- format(as.Date(end), "%Y%m%d")
  req <- list(
    start = start,
    end = end,
    category = category,
    provider = provider,
    format = format,
    search = search,
    order = order,
    skip = skip
  )
  res <- GET(api_url, query = req)
  fromJSON(content(res, "text", encoding = "UTF-8"))$files
}

#' List all historical files without maximum
#'
#' It continuesly call list_hist_file until no result returned
#'
#' @param ... parameters passed to list_hist_file function
#'
#' @export
#'
list_hist_file_nomax <- function(...) {
  max <- 500
  result <- list()
  i <- 0
  while (TRUE) {
    data <- list_hist_file(..., skip = i * max)
    if (length(data) == 0 || is.null(data) || nrow(data) == 0) break
    result <- rbind(result, data)
    if (nrow(data) < max) break
    i <- i + 1
  }
  result
}

#' Historical Archive File Version
#'
#' @param url url of the file
#' @param start start date
#' @param end end date
#'
#' @export
#'
hist_file_versions <- function(url, start, end) {
  require(httr)
  require(jsonlite)
  api_url <- "https://api.data.gov.hk/v1/historical-archive/list-file-versions"
  start <- format(as.Date(start), "%Y%m%d")
  end <- format(as.Date(end), "%Y%m%d")
  req <- list(
    url = url,
    start = start,
    end = end
  )
  res <- GET(api_url, query = req)
  fromJSON(content(res, "text", encoding = "UTF-8"))
}

#' Historical Archive File URL For A Specific Version
#'
#' @param url url of the historical files
#' @param timestamp timestamp of the historical file to retrieve
#'
#' @export
#'
hist_file_url <- function(url, timestamp) {
  api_url <- "https://api.data.gov.hk/v1/historical-archive/get-file"
  sprintf(
    "%s?url=%s&time=%s",
    api_url,
    URLencode(url, reserved = TRUE),
    URLencode(timestamp, reserved = TRUE)
  )
}
