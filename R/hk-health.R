#-----------------------------------------------------------------------------#
# HONG KONG HEALTH DATA
#-----------------------------------------------------------------------------#

#' Department of Health: Inpatient Statistics
#'
#' Inpatient Discharges and Deaths in All Hospitals Classified by Disease \cr
#' \cr
#' UPDATE FREQUENCY: AS AND WHEN NECESSARY									
#'
#' @param year year of the data
#' @param path path to save the file
#' @param keep whether to keep the file after read
#'
#' @format A data frame with 10 variables.
#' * `icd_code`: Detailed List No. ICD 10th Revision
#' * `dx_grp`: Disease group
#' * `ip_ha_hosp`: Number of inpatient discharges and deaths - Hospital Authority hospitals
#' * `ip_ci_hosp`: Number of inpatient discharges and deaths - Correctional Institution hospitals
#' * `ip_pvt_hosp`: Number of inpatient discharges and deaths - Private hospitals
#' * `ip_total`: Number of inpatient discharges and deaths - Total
#' * `reg_dealth_male`: Registered deaths in Hong Kong - Male
#' * `reg_dealth_female`: Registered deaths in Hong Kong - Female
#' * `reg_dealth_total`: Registered deaths in Hong Kong - Total
#' * `reg_dealth_unknown`: Registered deaths in Hong Kong - Unknown sex
#'
#' @source <https://data.gov.hk/en-data/dataset/hk-dh-dh_ncddhss-ncdd-dataset-10>
#'
#' @details
#' * Classification of diseases and causes of death is based on the International
#' Statistical Classification of Diseases and Related Health Problems (ICD) 10th
#' Revision from 2001 onwards. Figures from 2001 onwards may not be comparable
#' with figures for previous years which were compiled based on the ICD 9th Revision.
#' * Chinese terms of disease groups are based on those compiled by WHO Collaborating Center for the Classification of Diseases, Beijing as the benchmark.
#' * Refers to discharges and deaths on attendance basis.
#' Patients treated in nursing homes are excluded.
#' * The number of inpatient discharges and deaths for Hospital Authority hospitals
#' may be slightly different from that presented in "Tables on Health Status and
#' Health Services 2012" Table A18 due to different sources of data.
#'
#' @export
#'
inpatient_statistics <- function(year, path = ".", keep = FALSE) {
  # Require
  require(dplyr)
  require(stringr)
  require(readxl)
  # List historical files
  keyword <- "Inpatient Discharges and Deaths in Hospitals and Registered Deaths in Hong Kong by Disease"
  files <- list_hist_file(Sys.Date() - 1, Sys.Date() - 1, search = keyword)
  urls <- files %>%
    mutate(version = str_extract(`resource-name-en`, "([0-9]{4})")) %>%
    select(version, url)
  # Check available years
  available_years <- urls$version
  if (!(year %in% available_years)) {
    available_years <- paste0(available_years, collapse = ", ")
    stop(sprintf("Data only available in year %s", available_years))
  }
  # Get excel file
  url <- urls %>% filter(version == year) %>% pull(url)
  fpath <- get_file_xlsx(url, path, silent = TRUE)
  if (!keep) on.exit(unlink(fpath))
  # Parse data
  data <- read_excel(
    fpath, skip = 7,
    col_names = c(
      "icd_code",
      "null",
      "dx_grp",
      "ip_ha_hosp",
      "ip_ci_hosp",
      "ip_pvt_hosp",
      "ip_total",
      "reg_dealth_male",
      "reg_dealth_female",
      "reg_dealth_total",
      "reg_dealth_unknown"
    )
  )
  # Clean data
  data <- data %>%
    select(-null) %>%
    filter(!is.na(dx_grp)) %>%
    mutate(
      dx_grp = gsub("\r\n", " ", dx_grp),
    ) %>%
    within({ reg_dealth_unknown[is.na(reg_dealth_unknown)] <- 0 })
  # Return
  return(data)
}

#' Department of Health: Number of Notifiable Infectious Diseases by Month Share
#' 
#' Number of notifiable infectious diseases by month \cr
#' \cr
#' UPDATE FREQUENCY: MONTHLY
#' 
#' @param year year of the data
#' @param path path to save the file
#' 
#' @format A data frame with 14 variables.
#' * `disease`: Disease name
#' * `jan`-`dec`: Number of cases in each month
#' * `total`: Total number of cases in the year
#' 
#' @source <https://data.gov.hk/en-data/dataset/hk-dh-chpsebcdd-number-of-notifications-for-notifiable-infectious-diseases>
#' 
#' @export
#' 
notifiable_infectious_diseases <- function(year, path = ".") {
  # Require
  require(dplyr)
  require(stringr)
  # List historical files
  keyword <- "Number of notifiable infectious diseases by month"
  files <- list_hist_file(Sys.Date() - 1, Sys.Date() - 1, search = keyword)
  urls <- files %>%
    mutate(
      version = str_extract(`resource-name-en`, "([0-9]{4})"),
      lang = str_remove_all(str_extract(`resource-name-en`, "\\([a-zA-Z]+\\)"), "\\W")
    ) %>%
    filter(lang == "English") %>% 
    select(version, url)
  # Check available years
  available_years <- urls$version
  if (!(year %in% available_years)) {
    available_years <- paste0(available_years, collapse = ", ")
    stop(sprintf("Data only available in year %s", available_years))
  }
  # Get csv file
  url <- urls %>% filter(version == year) %>% pull(url)
  # Parse data
  data <- get_file_csv(url, path)
  # Clean data
  data <- data %>% 
    rename_all(tolower) %>% 
    filter(!grepl("^Total", disease))
  # Return
  return(data)
}
