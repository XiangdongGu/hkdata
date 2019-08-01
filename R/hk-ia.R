#-----------------------------------------------------------------------------#
# RETRIEVE HONG KONG INSURANCE AUTHORITY DATA
#-----------------------------------------------------------------------------#

#' HKIA: Market Statistics
#'
#' @param year year of the data
#' @param table the table name L1 - L7
#' @param path path to save the file
#' @param keep whether to keep the file after read
#'
#' @details
#' The list of available tables:\cr
#' L1 Individual Life Business\cr
#' L2 Non-Linked Individual Life In-Force Business \cr
#' L3 Linked Individual Life In-Force Business\cr
#' L4 Individual Life New Business\cr
#' L6 Group Life and Retirement Scheme In-Force Business\cr
#' L7 Annuity and Other In-Force Business/Individual Annuity New Business\cr
#' L8 Non-Linked Individual Life In-Force Business
#'
#' @export
#'
hkia_longterm_market <- function(year, table, path = ".", keep = FALSE) {
  require(readxl)
  require(dplyr)
  require(tidyr)
  url <- sprintf(
    "https://www.ia.org.hk/en/infocenter/statistics/files/Table-%s_%s.xls",
    table, year)
  if (!(table %in% paste0("L", c(1:4, 6:7)))) stop(
    "table should be in L1-L4 or L6-L7"
  )
  fpath <- get_file_xlsx(url, path, silent = TRUE)
  if (!keep) on.exit(unlink(fpath))
  
  # Read/Parse the data----------------------------------------------#
  allna <- function(x) all(is.na(x))
  to_header <- function(x) ifelse(is.na(x), "", x)
  data <- read_excel(fpath, skip = 3)
  data <- data[, !sapply(data, allna)]
  data <- data[!apply(data, 1, allna), ]
  data <- data[!grepl("AIDS", data[[1]]), ]
  start <- which(grepl("Type of Insurance", data[[1]]))
  end <- which(grepl("Total|All Policies\\*", data[[1]]))
  tab_row <- which(grepl("Table", data[[1]]))
  tab_row <- c(1, tab_row)
  names(tab_row) <- c(names(data)[1], data[[1]][tab_row[-1]])
  result <- list()
  for (i in seq_along(start)) {
    s <- start[i]
    e <- end[i]
    d <- data[(s+1):e, ]
    header <- to_header(as.character(data[s, , drop = TRUE]))
    j <- s - 1
    while (j > 0 && is.na(data[[1]][j])) {
      header_extra <- as.character(data[j, , drop = TRUE])
      header_extra <- zoo::na.locf(header_extra, na.rm = FALSE)
      header_extra <- to_header(header_extra)
      header <- paste(header_extra, header, sep = "|")
      j <- j - 1
    }
    if (is.na(data[[1]][s + 1])) {
      header_extra <- to_header(as.character(data[s+1, , drop = TRUE]))
      header <- paste(header, header_extra, sep = "^")
      d <- d[-1, ]
    }
    names(d) <- header
    d <- d[, !sapply(d, allna)]
    d[-1] <- sapply(d[-1], as.numeric)
    d$table <- names(tab_row)[which.max(tab_row[tab_row < s])]
    names(d)[1] <- "business_type"
    d <- d %>% select(table, business_type, everything())
    d <- d %>% gather("key", "value", -business_type, -table)
    expand_key <- strsplit(d$key, "\\|")
    expand_key <- data.frame(do.call("rbind", expand_key), stringsAsFactors = FALSE)
    names(expand_key) <- paste0("type", 1:ncol(expand_key))
    names(expand_key)[ncol(expand_key)] <- "year"
    if (any(grepl("\\^", expand_key$year))) {
      expand_key <- expand_key %>%
        mutate(
          temp = year,
          year = gsub("(.+)\\^(.*)", "\\1", temp),
          unit = gsub("(.+)\\^(.*)", "\\2", temp)) %>%
        select(-temp)
    }
    d <- cbind(d %>% select(-key), expand_key)
    d <- d %>% filter(!is.na(business_type)) # Special handling for L1
    d <- d %>% select(table, starts_with("type"), everything())
    d <- d %>% spread(key = year, value = value)
    result <- bind_rows(result, d)
  }
  result
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

#' Quarterly Release of Provisional Statistics for Long Term Business.
#' Results are cumulative
#' 
#' @param year the year
#' @param quarter the quarter
#' @param path path to save the file
#' @param keep whether to keep the file after read
#' 
#' @details 
#' Currently we only parse sheets for L1s (new direct individual business) and 
#' L3s (inforce direct individual business)
#' 
#' @export
#' 
hkia_long_term_provisional <- function(year, quarter, path = ".", keep = FALSE) {
  require(readxl)
  require(dplyr)
  require(tidyr)
  year <- year %% 100
  url <- sprintf(paste0("https://www.ia.org.hk/en/infocenter/statistics/",
                        "files/%sq%slong.xls"), quarter, year)
  fpath <- get_file_xlsx(url, path, silent = TRUE)
  if (!keep) on.exit(unlink(fpath))
  
  parse_header <- function(sheet, rows, cat1, cat2, cat3) {
    if (is.character(rows)) return(rows)
    rg <- cell_limits(c(min(rows), 3), c(max(rows), NA))
    d <- read_excel(fpath, sheet, rg, col_names = FALSE)
    d <- data.frame(t(d), stringsAsFactors = FALSE)
    f <- function(x) zoo::na.locf(x, na.rm = FALSE)
    g <- function(x) ifelse(is.na(x), "", x)
    d <- d %>% mutate_all(f) %>% mutate_all(g)
    dcats <- lapply(list(cat1, cat2, cat3), function(x) {
      if (is.character(x)) return(rep(x, nrow(d)))
      apply(d[, x, drop = FALSE], 1, paste0, collapse = " ")
    })
    data.frame(cat1 = dcats[[1]], cat2 = dcats[[2]], cat3 = dcats[[3]],
               row = paste0("r", 1:nrow(d)), stringsAsFactors = F)
  }
  
  parse_sheet <- function(sheet, rows, cat1, cat2, cat3, skip = max(rows)) {
    header <- parse_header(sheet, rows, cat1, cat2, cat3)
    data <- read_excel(fpath, sheet, skip = skip, col_names = c(
      "insurer_eng", "insurer_chi", header$row
    ))
    empty <- apply(data[, -1], 1, function(x) all(is.na(x)))
    data <- data[!empty, ]
    data <- data %>% gather(row, value, -insurer_eng, -insurer_chi)
    data <- data %>% left_join(header, by = "row")
    data %>% select(insurer_eng, insurer_chi, cat1, cat2, cat3, value)
  }
  
  # Read and parse sheets-------------------------------------------#
  # L1 - new direct individual business
  l1 <- parse_sheet("Table L1", 7:13, "Business Class", 1:3, 4:7)
  l1a <- parse_sheet("Table L1(a)", 7:13, 1, 2:3, 4:7)
  l1b <- parse_sheet("Table L1(b)", 7:13, 1, 2:3, 4:7)
  l1c <- parse_sheet("Table L1(c)", 7:12, 1, 2:6, "Premium")
  l1d <- parse_sheet("Table L1(d)", 7:13, 1, 2:3, 4:7)
  l1e <- parse_sheet("Table L1(e)", 7:13, 1, 2:3, 4:7)
  l1f <- parse_sheet("Table L1(f)", 7:13, 1, 2:3, 4:7)
  l1g <- parse_sheet("Table L1(g)", 7:13, 1, 4:7, 2:3)
  l1h <- parse_sheet("Table L1(h)", 7:13, 1, 2:3, 4:7)
  L1 <- bind_rows(l1, l1a, l1b, l1c, l1d, l1e, l1f, l1g, l1h)
  # L3 - direct inforce business
  l31 <- parse_sheet("Table L3-1", 8:11, "Business Class", 1, 2:4)
  l32 <- parse_sheet("Table L3-2", 8:11, "Business Class", 1, 2:4)
  L3 <- bind_rows(l31, l32)
  # Process the tables-------------------------------------------------#
  process_vtype <- function(x) {
    y <- gsub("[^[:alnum:][:punct:][:space:]]", "", x)
    y <- gsub("\\(\\)", "", y)
    y <- gsub("\\n", "", y)
    y <- gsub("/", "", y)
    y <- gsub("[[:space:]]+", " ", y)
    y <- gsub("^[[:space:]]+|[[:space:]]+$", "", cat2)
    y <- gsub("(Number of Policies).+", "\\1", y, ignore.case = TRUE)
  }
  process_category <- function(x) {
    y <- strsplit(x, "\\n")
  }
  L1 <- L1 %>%
    mutate(
      cat1 = process_vtype(cat1),
      cat2 = gsub("^[[:space:]]+|[[:space:]]+$", "", cat2),
      cat3 = process_vtype(cat3),
      value = ifelse(value == "-", 0, value),
      value = as.numeric(value)
    ) %>%
    select(insurer_eng, insurer_chi, by_group = cat1, catetory = cat2,
           value_type = cat3, value)
  
  L3 <- L3 %>% 
    mutate(cat2 = gsub(".+\\n(.+)", "\\1", cat2),
           cat3 = process_vtype(cat3),
           value = ifelse(value == "-", 0, value),
           value = as.numeric(value)) %>%
    select(insurer_eng, insurer_chi, business_class = cat2,
           value_type = cat3, value)
}

