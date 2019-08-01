#' Historical Archive File URL For A Specific Version
#'
#' @param url url of the historical files
#' @param timestamp timestamp of the historical file to retrieve. if null then
#'   current, otherwise historical it should be in format of \%Y\%m\%d-\%H\%M,
#'   e.g. 20180905-1306
#'
#' @export
#' 
data_file_url <- function(url, timestamp=NULL) {
  if (!is.null(timestamp)) {
    valid <- strptime(timestamp, "%Y%m%d-%H%M")
    if (is.na(valid)) stop("Invalid timestamp format")
    url <- hist_file_url(url, timestamp)
  }
  url
}

#' Retrieve Versions of Historical Archive Files
#'
#' @param url url of the file
#' @param start start date
#' @param end end date
#' @param retireve_func function that when given a file url (obtained from
#'   timestamp), would retrieve the specified data
#' @param ... extra parameters to pass to retireve_func()
#'
#' @export
#' 
get_file_versions <- function(url, start, end = NULL, retrieve_func, ...) {
  vs <- hist_file_versions(url, start, end)
  lapply(vs$timestamps,
         function(timestamp) {
           retrieve_func(data_file_url(url, timestamp), ...)
           })
}
