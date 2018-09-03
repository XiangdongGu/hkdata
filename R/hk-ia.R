#-----------------------------------------------------------------------------#
# RETRIEVE HONG KONG INSURANCE AUTHORITY DATA
#-----------------------------------------------------------------------------#

#' HKIA: Market Statistics - L1 Individual Life Business
#'
#' @param year year of the data
#' @param path path to save the file
#' @param keep whether to keep the file after read
#'
hkia_l1 <- function(year, path = ".", keep = FALSE) {
  require(readxl)
  url <- sprintf(
    "https://www.ia.org.hk/en/infocenter/statistics/files/Table-L1_%s.xls",
    year)
  fpath <- get_file_xlsx(url, path, silent = TRUE)
  if (!keep) on.exit(unlink(fpath))

  # Read the data----------------------------------------------#
  data <- read_excel(fpath)
}

#' HKIA: Individual Insurers' Statistics - L9 Linked Individual Life In-Force Business
#'
#' @param year year of the data
#' @param path path to save the file
#' @param keep whether to keep the file after read
#'
hkia_l9 <- function(year, path = ".", keep = FALSE) {
  require(readxl)
  require(dplyr)
  url <- sprintf(
    "https://www.ia.org.hk/en/infocenter/statistics/files/Table-L9_%s.xls",
    year)
  fpath <- get_file_xlsx(url, path, silent = TRUE)
  if (!keep) on.exit(unlink(fpath))

  # Read the data----------------------------------------------#
  data <- read_excel(fpath, skip = 7)
  unit_row <- as.character(data[1, ])
  unit_row[is.na(unit_row) | unit_row == "NA"] <- ""
  names(data) <- paste0(names(data), unit_row)
  data <- data[-1, ]
  allna <- apply(data, 1, function(x) all(is.na(x)))
  data <- data[!allna, ]
  data <- data %>% mutate_at(vars(names(data[-(1:3)])), funs(as.numeric))
  data
}

#' HKIA: Individual Insurers' Statistics - L13 Total In-Force Business
#'
#' @param year year of the data
#' @param path path to save the file
#' @param keep whether to keep the file after read
#'
hkia_l13 <- function(year, path = ".", keep = FALSE) {
  require(readxl)
  require(dplyr)
  url <- sprintf(
    "https://www.ia.org.hk/en/infocenter/statistics/files/Table-L13_%s.xls",
    year)
  fpath <- get_file_xlsx(url, path, silent = TRUE)
  if (!keep) on.exit(unlink(fpath))

  # Read the data----------------------------------------------#
  data <- read_excel(fpath, skip = 7)
  unit_row <- as.character(data[1, ])
  unit_row[is.na(unit_row) | unit_row == "NA"] <- ""
  names(data) <- paste0(names(data), unit_row)
  data <- data[-1, ]
  allna <- apply(data, 1, function(x) all(is.na(x)))
  data <- data[!allna, ]
  data <- data %>% mutate_at(vars(names(data[-(1:3)])), funs(as.numeric))
  data
}

