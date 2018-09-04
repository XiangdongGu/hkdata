#' Read Hong Kong Life Tables
#'
#' @export
#'
hk_life_table <- function() {
  require(dplyr)
  require(readxl)
  # Find URL for Hong Kong Life tables
  url <- list_hist_file(Sys.Date() - 1, Sys.Date() - 1, search = "Life Tables")
  url <- url %>%
    filter(!grepl("projected|Accompanying", `resource-name-en`, ignore.case = TRUE))
  if (nrow(url) != 1) stop("Multiple files identified")
  if (nrow(url) == 0) stop("No file identified")
  # Download the file to temp folder
  path <- tempdir()
  fpath <- get_file_xlsx(url$url, path, silent = TRUE)
  on.exit(unlink(fpath))
  # Read the file
  sheets <- excel_sheets(fpath)
  p <- ".+(Male|Female)([[:digit:]]+).*"
  m <- grepl(p, sheets)
  cat(sprintf("Exclude sheets %s\n", paste(sheets[!m], collapse = ",")))
  sheets <- sheets[m]
  result <- list()
  for (sheet in sheets) {
    gender <- gsub(p, "\\1", sheet)
    year <- as.numeric(gsub(p, "\\2", sheet))
    data <- read_excel(fpath, sheet, skip = 4)
    data <- suppressWarnings(sapply(data, as.numeric))
    data <- data[
      !apply(data, 1, function(x) all(is.na(x))),
      !apply(data, 2, function(x) all(is.na(x)))
      ]
    data <- as_tibble(data)
    names(data) <- tolower(gsub("[[:punct:]]", "", names(data)))
    names(data)[1:2] <- c("age", "mortality_rate")
    data$gender <- gender
    data$year <- year
    result <- bind_rows(result, data)
  }
  result <- result %>%
    select(year, age, gender, mortality_rate, everything()) %>%
    filter(!is.na(age))
  result
}
