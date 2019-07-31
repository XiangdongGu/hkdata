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
  require(dplyr)
  require(stringr)
  require(readxl)
  # List historical files
  keyword <- "Inpatient Discharges and Deaths in Hospitals and Registered Deaths in Hong Kong by Disease"
  files <- list_hist_file(Sys.Date() - 1, Sys.Date() - 1, search = keyword)
  # Check file availability
  if (length(files) == 0) stop("Failed to retrive historical data.")
  # Extract versions and urls
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
  require(dplyr)
  require(stringr)
  # List historical files
  keyword <- "Number of notifiable infectious diseases by month"
  files <- list_hist_file(Sys.Date() - 1, Sys.Date() - 1, search = keyword)
  # Check file availability
  if (length(files) == 0) stop("Failed to retrive historical data.")
  # Extract versions and urls
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
  data <- get_file_csv(url, path, col_types = readr::cols())
  # Clean data
  data <- data %>% 
    rename_all(tolower) %>% 
    filter(!grepl("^Total", disease))
  return(data)
}

#' Department of Health: Flu Express's Figures Data
#' 
#' Influenza surveillance data including sentinel surveillance, laboratory surveillance, influenza-like illness outbreak, hospital surveillance and severe influenza case during influenza season. \c
#' \cr
#' UPDATE FREQUENCY: WEEKLY
#' 
#' @param path path to save the file
#' @param keep whether to keep the file after read
#' 
#' @source <https://data.gov.hk/en-data/dataset/hk-dh-chpsebcddr-flu-express>
#' 
#' @export
#' 
flu_surveillance <- function(path = ".", keep = FALSE) {
  require(dplyr)
  require(stringr)
  require(readxl)
  # List historical files
  keyword <- "Flu Express's figures data"
  files <- list_hist_file(Sys.Date() - 1, Sys.Date() - 1, search = keyword)
  # Check file availability
  if (length(files) == 0) stop("Failed to retrive historical data.")
  # Get excel file
  url <- files$url[1]
  fpath <- get_file_xlsx(url, path, silent = TRUE)
  if (!keep) on.exit(unlink(fpath))
  # Parse data
  data <- suppressWarnings(read_excel(
    fpath, skip = 3,
    col_names = c(
      "year",
      "week",
      "from",
      "to",
      "rate_ili_consult_sentinel_gopc",
      "rate_ili_consult_sentinel_gp",
      "n_inf_pos_lab_surv_a_h1",
      "n_inf_pos_lab_surv_a_h3",
      "n_inf_pos_lab_surv_b",
      "n_inf_pos_lab_surv_c",
      "n_inf_pos_lab_surv_all_subtypes",
      "pct_inf_pos_lab_surv_a_h1",
      "pct_inf_pos_lab_surv_a_h3",
      "pct_inf_pos_lab_surv_b",
      "pct_inf_pos_lab_surv_c",
      "pct_inf_pos_lab_surv_all_subtypes",
      "n_ili_scl_inst",
      "rate_inf_adm_pub_hosp_0_5",
      "rate_inf_adm_pub_hosp_6_11",
      "rate_inf_adm_pub_hosp_12_17",
      "rate_inf_adm_pub_hosp_18_49",
      "rate_inf_adm_pub_hosp_50_64",
      "rate_inf_adm_pub_hosp_65_abv",
      "rate_inf_adm_pub_hosp_all",
      "rate_ili_inf_aed_pub_hosp",
      "pct_fever_kg_ccc",
      "pct_fever_rche",
      "rate_ili_consult_cmp",
      "n_svr_inf_weekly_0_17",
      "n_svr_inf_weekly_18_49",
      "n_svr_inf_weekly_50_64",
      "n_svr_inf_weekly_65_abv"
    )
  ))
}

