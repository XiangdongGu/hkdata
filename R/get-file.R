#-----------------------------------------------------------------------------#
# FUNCTIONS TO GET FILE FOR A GIVEN URL FOR DIFFERENT FORMAT
#-----------------------------------------------------------------------------#

#' Get xlsx format file
#'
#' @param url the url of the file
#' @param path the directory where the file should be downloaded to
#'
#' @export
#'
get_file_xlsx <- function(url, path = ".") {
  path <- file.path(path, basename(url))
  download.file(url, path, quiet = TRUE)
  cat(sprintf("Downloaded to %s\n", path))
}


#' Get csv format file
#'
#' @param url the url of the file
#' @param path the directory where the file should be save, if NULL it
#' will not save
#'
#' @return it returns a data frame read from the csv file
#'
#' @examples
#' data <- get_file_csv("http://www.chp.gov.hk/files/misc/statistics-2017-vss-eng.csv")
#'
#' @export
#'
get_file_csv <- function(url, path = NULL) {
  require(readr)
  if (is.null(path)) {
    path <- file.path(tempdir(), basename(url))
    on.exit(unlink(path))
  } else {
    path <- file.path(path, basename(url))
  }
  download.file(url, path, quiet = TRUE)
  cat(sprintf("Downloaded to %s\n", path))
  data <- read_csv(path)
  data
}

#' Get jpg format file
#'
#' @param url the url of the file
#' @param path the directory to save the file
#' @param view if TRUE the the file is viewed after download
#'
#' @examples
#' get_file_jpg("http://www.toothclub.gov.hk/chi/jpg/psi/P041.jpg")
#'
#' @export
#'
get_file_jpg <- function(url, path = ".", view = TRUE) {
  path <- file.path(path, basename(url))
  download.file(url, path, quiet = TRUE)
  cat(sprintf("Downloaded to %s\n", path))
  if (view) {
    viewer <- getOption("viewer")
    viewer(path)
  }
}


