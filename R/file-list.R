#' Historical Archive File list
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
#' hist_file_list("2015-01-01", "2018-01-01")
#'
#' hist_file_list("2015-01-01", "2018-01-01", category = "health")
#'
#' @export
#'
hist_file_list <- function(start, end, category = NULL, provider = NULL,
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
