#-----------------------------------------------------------------------------#
# RETRIEVE HONG KONG CENSUS DATA (TPU Level)
#-----------------------------------------------------------------------------#

#' HK 2006 Population Census: Domestic Households Number
#'
#' @description 
#' Domestic Households by Monthly Domestic Household Income and Small Tertiary Planning Unit Group, 2006
#' 
#' @details
#' The income group:
#' income_group_1	  < $2,000
#' income_group_2	  $2,000 - $3,999
#' income_group_3	  $4,000 - $5,999
#' income_group_4	  $6,000 - $7,999
#' income_group_5	  $8,000 - $9,999
#' income_group_6	  $10,000 - $14,999
#' income_group_7	  $15,000 - $19,999
#' income_group_8	  $20,000 - $24,999
#' income_group_9	  $25,000 - $29,999
#' income_group_10	$30,000 - $39,999
#' income_group_11	$40,000 - $59,999
#' income_group_12	$60,000+

hk_census_tpu <- function(){
  require(rvest)
  require(dplyr)
  # find URL for data download
  url <- "https://data.gov.hk/en-data/dataset/hk-censtatd-06c_csv_en-2006-population-bycensus-statistical-tables-csv-en"
  page <- read_html(url)

  all_links <- page %>%
    html_nodes(".data-resource-list") %>%
    html_nodes("div") %>%
    html_attr("data-resource-url")
  all_links <- all_links[!is.na(all_links)]

  all_tbls_name <- page %>%
    html_nodes(".data-resource-list") %>%
    html_nodes("div") %>%
    html_attr("data-resource-title-en")
  all_tbls_name <- all_tbls_name[!is.na(all_tbls_name)]

  master <- tibble(tbl_name = all_tbls_name,
                   url = all_links )

  # URL for TPU level data
  by_TPU <- master %>%
    filter(str_detect(tolower(tbl_name),"small tertiary planning unit group"))

  # Download the file to temp folder
  fpath <- get_file_xlsx(by_TPU$url[10], path, silent = TRUE)
  dt <- read_csv(fpath, col_name = FALSE)
  
  # formatting and clean the data
  dt_name <- str_c(dt[1,1], dt[2,1], sep = " / ")
  dt_c <- dt[5:209,1:13]
  names(dt_c) <- c("stpug", sprintf("income_group_%s", seq(1:12)))
}

library(dplyr)
library(rvest)
library(httr)
library(tidyverse)
library(tidytext)








keyword <- by_TPU %>%
  select(tbl_name) %>%
  unnest_tokens(word, tbl_name) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE)




#maybe do all tables, summarize a list by table name in ()






