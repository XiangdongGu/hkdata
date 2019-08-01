#-----------------------------------------------------------------------------#
# HONG KONG HEALTH DATA
#-----------------------------------------------------------------------------#

# Inpatient Statistics --------------------------------------------------------

#' Get Base URL for "Department of Health: Inpatient Statistics"
#' 
#' Inpatient discharges and deaths in all hospitals classified by disease. \cr
#' \cr
#' UPDATE FREQUENCY: AS AND WHEN NECESSARY		
#' 
#' @param year year of the data (available since 2012)
#' 
#' @export
#' 
inpatient_statistics_url <- function(year) {
  if (!grepl("[0-9]{4}", year)) stop("Invalid year format.")
  url <- paste0(
    "http://www.dh.gov.hk/datagovhk/ncdd/Number%20of%20Inpatient%20Discharges%20and%20Deaths%20in%20Hospitals%20and%20Registered%20Deaths%20in%20Hong%20Kong%20by%20Disease%20", 
    paste0(year, ".xlsx")
  )
  return(url)
}

#' Retrieve Data for "Department of Health: Inpatient Statistics"
#'
#' Inpatient discharges and deaths in all hospitals classified by disease. \cr
#' \cr
#' UPDATE FREQUENCY: AS AND WHEN NECESSARY									
#'
#' @param url base url as returned by `inpatient_statistics_url`
#' @param path path to save the file
#' @param keep whether to keep the file after read
#'
#' @export
#'
inpatient_statistics_retrieve <- function(url, path = NULL, keep = FALSE) {
  require(dplyr)
  require(stringr)
  require(readxl)
  # Parse data
  tryCatch(
    {fpath <- get_file_xlsx(url, path, silent = TRUE)},
    error = function(msg) stop("Unable to retrieve information, input year may not be available.")
  )
  if (!keep) on.exit(unlink(fpath))
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

#' Get Data for "Department of Health: Inpatient Statistics"
#'
#' Inpatient discharges and deaths in all hospitals classified by disease. \cr
#' \cr
#' UPDATE FREQUENCY: AS AND WHEN NECESSARY									
#'
#' @param year year of the data (available since 2012)
#' @param path path to save the file
#' @param keep whether to keep the file after read
#'
#' @format A data frame with 10 variables.\cr
#' * `icd_code`: Detailed List No. ICD 10th Revision\cr
#' * `dx_grp`: Disease group\cr
#' * `ip_ha_hosp`: Number of inpatient discharges and deaths - Hospital Authority hospitals\cr
#' * `ip_ci_hosp`: Number of inpatient discharges and deaths - Correctional Institution hospitals\cr
#' * `ip_pvt_hosp`: Number of inpatient discharges and deaths - Private hospitals\cr
#' * `ip_total`: Number of inpatient discharges and deaths - Total\cr
#' * `reg_dealth_male`: Registered deaths in Hong Kong - Male\cr
#' * `reg_dealth_female`: Registered deaths in Hong Kong - Female\cr
#' * `reg_dealth_total`: Registered deaths in Hong Kong - Total\cr
#' * `reg_dealth_unknown`: Registered deaths in Hong Kong - Unknown sex
#'
#' @source <https://data.gov.hk/en-data/dataset/hk-dh-dh_ncddhss-ncdd-dataset-10>
#'
#' @details
#' * Classification of diseases and causes of death is based on the International\cr
#' Statistical Classification of Diseases and Related Health Problems (ICD) 10th\cr
#' Revision from 2001 onwards. Figures from 2001 onwards may not be comparable\cr
#' with figures for previous years which were compiled based on the ICD 9th Revision.\cr
#' * Chinese terms of disease groups are based on those compiled by WHO Collaborating Center for the Classification of Diseases, Beijing as the benchmark.\cr
#' * Refers to discharges and deaths on attendance basis.\cr
#' Patients treated in nursing homes are excluded.\cr
#' * The number of inpatient discharges and deaths for Hospital Authority hospitals\cr
#' may be slightly different from that presented in "Tables on Health Status and\cr
#' Health Services 2012" Table A18 due to different sources of data.
#'
#' @export
#'
inpatient_statistics <- function(year, path = NULL, keep = FALSE) {
  url <- inpatient_statistics_url(year)
  data <- inpatient_statistics_retrieve(url, path, keep)
  return(data)
}

# Number of Notifiable Infectious Diseases by Month Share ---------------------

#' Get Base URL for "Department of Health: Number of Notifiable Infectious Diseases by Month Share"
#' 
#' Number of notifiable infectious diseases by month. \cr
#' \cr
#' UPDATE FREQUENCY: MONTHLY
#' 
#' @param year year of the data (available since 1997)
#' 
#' @export
#' 
infectious_diseases_url <- function(year) {
  if (!grepl("[0-9]{4}", year)) stop("Invalid year format.")
  url <- sprintf("https://www.chp.gov.hk/files/misc/nid%sen.csv", year)
  return(url)
}

#' Retrieve Data for "Department of Health: Number of Notifiable Infectious Diseases by Month Share"
#' 
#' Number of notifiable infectious diseases by month. \cr
#' \cr
#' UPDATE FREQUENCY: MONTHLY
#' 
#' @param url base url as returned by `infectious_diseases_url`
#' @param path path to save the file
#' 
#' @export
#' 
infectious_diseases_retrieve <- function(url, path = NULL) {
  require(dplyr)
  require(stringr)
  # Parse data
  tryCatch(
    {data <- get_file_csv(url, path, col_types = readr::cols())},
    error = function(msg) stop("Unable to retrieve information, input year may not be available.")
  )
  # Clean data
  data <- data %>% 
    rename_all(tolower) %>% 
    filter(!grepl("^Total", disease)) %>% 
    mutate_all(list(~na_if(., "-"))) %>% 
    mutate_at(vars(jan:total), as.numeric)
  return(data)
}

#' Get Data for "Department of Health: Number of Notifiable Infectious Diseases by Month Share"
#' 
#' Number of notifiable infectious diseases by month. \cr
#' \cr
#' UPDATE FREQUENCY: MONTHLY
#' 
#' @param year year of the data (available since 1997)
#' @param path path to save the file
#' 
#' @format A data frame with 14 variables.\cr
#' * `disease`: Disease name\cr
#' * `jan`-`dec`: Number of cases in each month\cr
#' * `total`: Total number of cases in the year
#' 
#' @source <https://data.gov.hk/en-data/dataset/hk-dh-chpsebcdd-number-of-notifications-for-notifiable-infectious-diseases>
#' 
#' @export
#' 
infectious_diseases <- function(year, path = NULL) {
  url <- infectious_diseases_url(year)
  data <- infectious_diseases_retrieve(url, path)
  return(data)
}

# Flu Express's Figures Data --------------------------------------------------

#' Get Base URL for "Department of Health: Flu Express's Figures Data"
#' 
#' Influenza surveillance data including sentinel surveillance, laboratory surveillance, influenza-like illness outbreak, hospital surveillance and severe influenza case during influenza season. \c
#' \cr
#' UPDATE FREQUENCY: WEEKLY
#' 
#' @export
#' 
flu_surveillance_url <- function() {
  url <- "http://www.chp.gov.hk/files/xls/flux_data.xlsx"
  return(url)
}

#' Retrieve Data for "Department of Health: Flu Express's Figures Data"
#' 
#' Influenza surveillance data including sentinel surveillance, laboratory surveillance, influenza-like illness outbreak, hospital surveillance and severe influenza case during influenza season. \c
#' \cr
#' UPDATE FREQUENCY: WEEKLY
#' 
#' @param url base url as returned by `flu_surveillance_url`
#' @param path path to save the file
#' @param keep whether to keep the file after read
#' 
#' @export
#' 
flu_surveillance_retrieve <- function(url, path = NULL, keep = FALSE) {
  require(dplyr)
  require(stringr)
  require(readxl)
  # Parse data
  fpath <- get_file_xlsx(url, path, silent = TRUE)
  if (!keep) on.exit(unlink(fpath))
  data <- suppressWarnings(read_excel(
    fpath, skip = 3,
    col_names = c(
      "year",
      "week",
      "from",
      "to",
      "rate_ili_gopc",
      "rate_ili_gp",
      "n_inf_lab_surv_a_h1",
      "n_inf_lab_surv_a_h3",
      "n_inf_lab_surv_b",
      "n_inf_lab_surv_c",
      "n_inf_lab_surv_all_subtypes",
      "pct_inf_lab_surv_a_h1",
      "pct_inf_lab_surv_a_h3",
      "pct_inf_lab_surv_b",
      "pct_inf_lab_surv_c",
      "pct_inf_lab_surv_all_subtypes",
      "n_ili_scl_inst",
      "rate_inf_pub_hosp_0_5",
      "rate_inf_pub_hosp_6_11",
      "rate_inf_pub_hosp_12_17",
      "rate_inf_pub_hosp_18_49",
      "rate_inf_pub_hosp_50_64",
      "rate_inf_pub_hosp_65_abv",
      "rate_inf_pub_hosp_all",
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
  # Clean data
  data <- data %>% 
    filter(grepl("^[0-9]{4}$", year) & !is.na(week)) %>% 
    mutate_if(is.character, list(~na_if(., "-")))
  return(data)
}

#' Get Data for "Department of Health: Flu Express's Figures Data"
#' 
#' Influenza surveillance data including sentinel surveillance, laboratory surveillance, influenza-like illness outbreak, hospital surveillance and severe influenza case during influenza season. \c
#' \cr
#' UPDATE FREQUENCY: WEEKLY
#' 
#' @param path path to save the file
#' @param keep whether to keep the file after read
#' 
#' @format A data frame with 32 variables.\cr
#' * `year`: Year\cr
#' * `week`: Week\cr
#' * `from`: From (Date)\cr
#' * `to`: To (Date)\cr
#' * `rate_ili_gopc`: ILI consultation rate (per 1,000 consultations) - Sentinel general out-patient clinics (GOPC)\cr
#' * `rate_ili_gp`: ILI consultation rate (per 1,000 consultations) - Sentinel GP\cr
#' * `n_inf_lab_surv_a_h1`: Laboratory surveillance - No. of positive detections of seasonal influenza viruses - A(H1)\cr
#' * `n_inf_lab_surv_a_h3`: Laboratory surveillance - No. of positive detections of seasonal influenza viruses - A(H3)\cr
#' * `n_inf_lab_surv_b`: Laboratory surveillance - No. of positive detections of seasonal influenza viruses - B\cr
#' * `n_inf_lab_surv_c`: Laboratory surveillance - No. of positive detections of seasonal influenza viruses - C\cr
#' * `n_inf_lab_surv_all_subtypes`: Laboratory surveillance - No. of positive detections of seasonal influenza viruses - All subtypes\cr
#' * `pct_inf_lab_surv_a_h1`: Laboratory surveillance - Positive% for influenza among all respiratory specimens - A(H1)\cr
#' * `pct_inf_lab_surv_a_h3`: Laboratory surveillance - Positive% for influenza among all respiratory specimens - A(H3)\cr
#' * `pct_inf_lab_surv_b`: Laboratory surveillance - Positive% for influenza among all respiratory specimens - B\cr
#' * `pct_inf_lab_surv_c`: Laboratory surveillance - Positive% for influenza among all respiratory specimens - C\cr
#' * `pct_inf_lab_surv_all_subtypes`: Laboratory surveillance - Positive% for influenza among all respiratory specimens - All subtypes\cr
#' * `n_ili_scl_inst`: No. of ILI outbreaks in schools/institutions\cr
#' * `rate_inf_pub_hosp_0_5`: Admission rates in public hospitals with principal diagnosis of influenza (per 10,000 people in the age group) - 0-5 years\cr
#' * `rate_inf_pub_hosp_6_11`: Admission rates in public hospitals with principal diagnosis of influenza (per 10,000 people in the age group) - 6-11 years\cr
#' * `rate_inf_pub_hosp_12_17`: Admission rates in public hospitals with principal diagnosis of influenza (per 10,000 people in the age group) - 12-17 years\cr
#' * `rate_inf_pub_hosp_18_49`: Admission rates in public hospitals with principal diagnosis of influenza (per 10,000 people in the age group) - 18-49 years\cr
#' * `rate_inf_pub_hosp_50_64`: Admission rates in public hospitals with principal diagnosis of influenza (per 10,000 people in the age group) - 50-64 years\cr
#' * `rate_inf_pub_hosp_65_abv`: Admission rates in public hospitals with principal diagnosis of influenza (per 10,000 people in the age group) - 65 years or above\cr
#' * `rate_inf_pub_hosp_all`: Admission rates in public hospitals with principal diagnosis of influenza (per 10,000 people in the age group) - All ages\cr
#' * `rate_ili_inf_aed_pub_hosp`: ILI syndrome group in Accident & Emergency Department (AED) of public hospitals (per 1,000 coded cases)\cr
#' * `pct_fever_kg_ccc`: Percentage of children at sentinel kindergartens/child care centres KG/CCC having fever\cr
#' * `pct_fever_rche`: Percentage of residents at sentinel RCHE having fever\cr
#' * `rate_ili_consult_cmp`: ILI consultation rate at sentinel Chinese medical practitioner (CMP) (per 1,000 consultations)\cr
#' * `n_svr_inf_weekly_0_17`: Weekly number of severe influenza cases by age groups - 0-17 years\cr
#' * `n_svr_inf_weekly_18_49`: Weekly number of severe influenza cases by age groups - 18-49 years\cr
#' * `n_svr_inf_weekly_50_64`: Weekly number of severe influenza cases by age groups - 50-64 years\cr
#' * `n_svr_inf_weekly_65_abv`: Weekly number of severe influenza cases by age groups - 65 years or above
#' 
#' @source <https://data.gov.hk/en-data/dataset/hk-dh-chpsebcddr-flu-express>
#' 
#' @details
#' * For `n_inf_lab_surv_c` and `pct_inf_lab_surv_c`, since week 7 of 2014 (week ending 10 Feb, 2014), \cr
#' the Public Health Laboratory Services Branch has adopted new genetic tests\cr
#' as the primary tests for various respiratory viruses including influenza C.\cr
#' * For all `n_svr_inf_weekly_`, the surveillance system for severe influenza cases among \cr
#' adult patients aged 18 years or above was only activated intermittently during influenza seasons before 2018.\cr
#' * All data of the recent few weeks are provisional figures and subject to further revision.
#' 
#' @export
#' 
flu_surveillance <- function(path = NULL, keep = FALSE) {
  url <- flu_surveillance_url()
  data <- flu_surveillance_retrieve(url, path, keep)
  return(data)
}

# EV Scan's Figures Data ------------------------------------------------------

#' Get Base URL for "Department of Health: EV Scan's Figures Data"
#' 
#' Hand, foot and mouth disease surveillance data including number of EV71 cases, 
#' institutional outbreaks, hospital surveillance and sentinel surveillance. \cr
#' \cr
#' UPDATE FREQUENCY: WEEKLY
#'
#' @export 
#'
ev_surveillance_url <- function() {
  url <- "http://www.chp.gov.hk/files/xls/evscan_data.xlsx"
  return(url)
}

#' Retrieve Data for "Department of Health: EV Scan's Figures Data"
#' 
#' Hand, foot and mouth disease surveillance data including number of EV71 cases, 
#' institutional outbreaks, hospital surveillance and sentinel surveillance. \cr
#' \cr
#' UPDATE FREQUENCY: WEEKLY
#' 
#' @param url base url as returned by `ev_surveillance_url`
#' @param path path to save the file
#' @param keep whether to keep the file after read
#'
#' @export 
#'
ev_surveillance_retrieve <- function(url, path = NULL, keep = FALSE) {
  require(dplyr)
  require(stringr)
  require(readxl)
  # Parse data
  fpath <- get_file_xlsx(url, path, silent = TRUE)
  if (!keep) on.exit(unlink(fpath))
  data <- suppressWarnings(read_excel(
    fpath, skip = 2,
    col_names = c(
      "year",
      "week",
      "from",
      "to",
      "n_ev71",
      "n_hfmd_inst",
      "n_hfmd_hosp_adm",
      "rate_hfmd_aed",
      "prop_hfmd_ccc_kg",
      "rate_hfmd_pvt_med",
      "rate_hfmd_gopc"
    ),
    col_types = c(
      "guess", "numeric", rep("date", 2), rep("numeric", 4), "text", rep("numeric", 2)
    )
  ))
  # Clean data
  data <- data %>% filter(grepl("^[0-9]{4}$", year) & !is.na(week)) 
  return(data)
}

#' Get Data for "Department of Health: EV Scan's Figures Data"
#' 
#' Hand, foot and mouth disease surveillance data including number of EV71 cases, 
#' institutional outbreaks, hospital surveillance and sentinel surveillance. \cr
#' \cr
#' UPDATE FREQUENCY: WEEKLY
#'
#' @param path path to save the file
#' @param keep whether to keep the file after read
#' 
#' @source <https://data.gov.hk/en-data/dataset/hk-dh-chpsebcdde-ev-scan>
#' 
#' @format A data frame with 11 variables.\cr
#' * `year`: Year\cr
#' * `week`: Week\cr
#' * `from`: From (Date)\cr
#' * `to`: To (Date)\cr
#' * `n_ev71`: Number of EV71 cases by week\cr
#' * `n_hfmd_inst`: Number of HFMD institutional outbreaks by week\cr
#' * `n_hfmd_hosp_adm`: Number of hospital admission episodes of HFMD by week\cr
#' * `rate_hfmd_aed`: Accident & Emergency Department surveillance of HFMD syndrome group (per 1000 coded cases)\cr
#' * `prop_hfmd_ccc_kg`: Proportion of child care centres/kindergartens (CCC/KG) with HFMD cases based on HFMD sentinel surveillance at CCC/KG by week\cr
#' * `rate_hfmd_pvt_med`: Consultation rate for HFMD based on HFMD sentinel surveillance among private medical practitioner clinics by week (per 1000 consultations) \cr
#' * `rate_hfmd_gopc`: Consultation rate for HFMD based on HFMD sentinel surveillance among General Out-patient Clinics by week (per 1000 consultations)
#' 
#' @details
#' * Recent data are provisional figures and subject to further revision.
#'
#' @export 
#'
ev_surveillance <- function(path = NULL, keep = FALSE) {
  url <- ev_surveillance_url()
  data <- ev_surveillance_retrieve(url, path, keep)
  return(data)
}

# Accident and Emergency Waiting Time by Hospital -----------------------------

#' Get Base URL for "Hospital Authority: Accident and Emergency Waiting Time by Hospital"
#' 
#' Accident and emergency waiting time by hospital. \cr
#' \cr
#' UPDATE FREQUENCY: EVERY 15 MINUTES
#' 
#' @export
#' 
ae_wait_time_url <- function() {
  url <- "http://www.ha.org.hk/opendata/aed/aedwtdata-en.json"
  return(url)
}

#' Retrieve Data for "Hospital Authority: Accident and Emergency Waiting Time by Hospital"
#' 
#' Accident and emergency waiting time by hospital. \cr
#' \cr
#' UPDATE FREQUENCY: EVERY 15 MINUTES
#' 
#' @param data_url data url as returned by `data_file_url`
#' @param path path to save the file
#' 
#' @export
#' 
ae_wait_time_retrieve <- function(data_url, path = NULL) {
  require(dplyr)
  require(stringr)
  # Parse data
  tryCatch(
    {data <- get_file_json(data_url, path)},
    error = function(msg) stop("Unable to retrieve information, input year may not be available.")
  )
  # Clean data
  data <- data$waitTime %>% rename(hosp_name = hospName, top_wait = topWait)
  return(data)
}

#' Get Data for "Hospital Authority: Accident and Emergency Waiting Time by Hospital"
#' 
#' Accident and emergency waiting time by hospital. \cr
#' \cr
#' UPDATE FREQUENCY: EVERY 15 MINUTES
#' 
#' @param timestamp if null then current, otherwise historical waiting time
#' it should be in format of \%Y\%m\%d-\%H\%M, e.g. 20190101-2315
#' minutes %M only available at 0, 15, 30, 45
#' @param path path to save the file
#' 
#' @source <https://data.gov.hk/en-data/dataset/hospital-hadata-ae-waiting-time>
#' 
#' @export
#' 
ae_wait_time <- function(timestamp = NULL, path = NULL) {
  data_url <- data_file_url(ae_wait_time_url(), timestamp)
  data <- ae_wait_time_retrieve(data_url, path)
  return(data)
}

