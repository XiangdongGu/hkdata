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

#' HKIA: Annual Statistics for Long Term Business - Individual Insurers
#'
#'
#' @param year year of the data
#' @param table the table name L8 - L19
#' @param path path to save the file
#' @param keep whether to keep the file after read
#'
#' @details
#' The list of available tables:\cr
#' L8 Non-Linked Individual Life In-Force Business \cr
#' L9 Linked Individual Life In-Force Business\cr
#' L10 Group Life In-Force Business\cr
#' L11 Retirement Scheme In-Force Business \cr
#' L12 Annuity and Other In-Force Business  \cr
#' L13 Total In-Force Business \cr
#' L14 Non-Linked Individual Life New Business\cr
#' L15 Linked Individual Life New Business \cr
#' L16 Total Individual Life New Business \cr
#' L17 Non-Linked Individual Annuity New Business\cr
#' L18 Linked Individual Annuity New Business\cr
#' L19 Total Individual Annuity New Business \cr
#'
#' @export
#'
hkia_longterm_insurer <- function(year, table, path = ".", keep = FALSE) {
  require(readxl)
  require(dplyr)
  if (!(table %in% paste0("L", 8:19))) stop(
    "table should be in L8 - L19"
  )
  url <- sprintf(
    "https://www.ia.org.hk/en/infocenter/statistics/files/Table-%s_%s.xls",
    table, year)
  fpath <- get_file_xlsx(url, path, silent = TRUE)
  if (!keep) on.exit(unlink(fpath))

  # Read the data----------------------------------------------#
  data <- read_excel(fpath, skip = 7)
  unit_row <- as.character(data[1, ])
  unit_row[is.na(unit_row) | unit_row == "NA"] <- ""
  names(data) <- paste0(names(data), unit_row)
  data <- data[-1, ]
  # All missing rows
  allna <- apply(data, 1, function(x) all(is.na(x)))
  data <- data[!allna, ]
  # All missing columns
  allna2 <- apply(data, 2, function(x) all(is.na(x)))
  data <- data[, !allna2]
  data <- data %>% mutate_at(vars(names(data[-(1:3)])), funs(as.numeric))
  data
}


