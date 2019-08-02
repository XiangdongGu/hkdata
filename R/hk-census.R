#-----------------------------------------------------------------------------#
# RETRIEVE HONG KONG CENSUS DATA (STPUG Level)
#-----------------------------------------------------------------------------#

#' HK 2006 Population Census: by Small Tertiary Planning Unit Group (STPUG)
#'
#' @param table_no the number of the table
#' @param keyword the keyword/category of the table
#'
#' @details
#' totally got 11 cencus statistics tables at STPUG level: \cr
#' 1) you can search by 'Table_ID' 
#' No Table_ID Table_Keywords Table_Description \cr
#' 1	A501  Hong Kong Resident Population by Ethnicity and Small Tertiary Planning Unit Group, 2006 (A501) [English] \cr
#' 2	A502	Hong Kong Resident Population Aged 5 and Over by Usual Language and Small Tertiary Planning Unit Group, 2006 (A502) [English] \cr
#' 3	A503	Hong Kong Resident Population by Broad Age Group, Sex and Small Tertiary Planning Unit Group, 2006 (A503) [English] \cr
#' 4	C501	Hong Kong Resident Population by Economic Activity Status and Small Tertiary Planning Unit Group, 2006 (C501) [English] \cr
#' 5	C502	Working Population by Industry (Sector) and Small Tertiary Planning Unit Group, 2006 (C502) [English] \cr
#' 6	C503	Working Population by Occupation (Major Group) and Small Tertiary Planning Unit Group, 2006 (C503) [English] \cr
#' 7	C504	Working Population by Monthly Income from Main Employment and Small Tertiary Planning Unit Group, 2006 (C504) [English] \cr
#' 8	D401	Domestic Households by Household Size and Small Tertiary Planning Unit Group, 2006 (D401) [English] \cr
#' 9	D402	Domestic Households by Household Composition and Small Tertiary Planning Unit Group, 2006 (D402) [English] \cr
#' 10	D403	Domestic Households by Monthly Domestic Household Income and Small Tertiary Planning Unit Group, 2006 (D403) [English] \cr
#' 11	E501	Occupied Quarters (Land) by Type of Quarters and Small Tertiary Planning Unit Group, 2006 (E501) [English] \cr
#' \cr
#' 2) or search by keywords: \cr
#' ethnicity, language, age_group, sex, economic_activity_status \cr
#' industry, occupation, monthly_income \cr
#' household_income, household_size, household_composition, quarter_type

#' @examples  
#' hk_census_stpug(table_id = "D401")
#' hk_census_stpug(keyword = "household_size")
#' hk_census_stpug(keyword = "household_income")
#' hk_census_stpug(keyword = "monthly_income")

#' @export 
#' 

hk_census_stpug <- function(table_id = NULL, keyword = NULL){
  require(rvest)
  require(dplyr)
  require(tidyverse)
  
  # find URL for all census data download
  url <- paste("https://data.gov.hk/en-data/dataset/",
                "hk-censtatd-06c_csv_en-2006-population",
                "-bycensus-statistical-tables-csv-en", sep = "")
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

  master <- tibble(tbl_name = all_tbls_name, url = all_links )

  # URL for TPU level data
  by_tpu <- master %>%
    filter(str_detect(tolower(tbl_name),
                      "small tertiary planning unit group")) %>%
    mutate(tbl_id = substr(tbl_name, nchar(tbl_name) - 14, 
                           nchar(tbl_name) - 11)) %>%
    select(tbl_id, tbl_name, url)

  # Download the file to temp folder
  path <- tempdir()
  fpath <- file.path(path, basename(by_tpu$url))
  by_tpu_dt <- lapply(fpath, read_csv, col_name = FALSE, 
                      col_types = cols(.default = "c"))
  
  # formatting and clean the data
  # 1 - A501
  dt_1 <- by_tpu_dt[[1]][5:209,1:3] %>%
    `colnames<-`(c("stpug", as.character(by_tpu_dt[[1]][3,2:3]))) %>%
    gather("ethnicity", "resident_population_number", - c(stpug)) %>%
    mutate(resident_population_number = 
             suppressWarnings(as.numeric(resident_population_number)))

  # 2 - A502
  dt_2 <- by_tpu_dt[[2]][6:210,1:3] %>%
    `colnames<-`(c("stpug", as.character(by_tpu_dt[[1]][3,2:3]))) %>%
    gather("language", "resident_population_number", - c(stpug)) %>%
    mutate(resident_population_number = 
            suppressWarnings(as.numeric(resident_population_number)))
  
  # 3	- A503
  dt_3 <- by_tpu_dt[[3]][6:620,1:9] %>%
    `colnames<-`(c("stpug", "sex", as.character(by_tpu_dt[[3]][4,3:9]))) %>%
    gather("age_group", "resident_population_number", - c(stpug, sex)) %>%
    mutate(resident_population_number = 
             suppressWarnings(as.numeric(resident_population_number)))
  
  # 4	- C501
  dt_4 <- by_tpu_dt[[4]][5:209,1:9] %>%
    `colnames<-`(c("stpug", as.character(by_tpu_dt[[4]][3,3:9]))) %>%
    gather("economic_activity_status", "resident_population_number", - stpug) %>%
    mutate(resident_population_number = 
             suppressWarnings(as.numeric(resident_population_number)))
  
  # 5	- C502
  dt_5 <- by_tpu_dt[[5]][5:209,1:8] %>%
    `colnames<-`(c("stpug", as.character(by_tpu_dt[[5]][3,3:8]))) %>%
    gather("industry", "working_population_number", - stpug) %>%
    mutate(working_population_number = 
             suppressWarnings(as.numeric(working_population_number)))
  
  # 6	- C503
  dt_6 <- by_tpu_dt[[6]][5:209,1:11] %>%
    `colnames<-`(c("stpug", as.character(by_tpu_dt[[6]][3,2:11]))) %>%
    gather("occupation", "working_population_number", - stpug) %>%
    mutate(working_population_number = 
             suppressWarnings(as.numeric(working_population_number)))
  
  # 7	- C504
  dt_7 <- by_tpu_dt[[7]][5:209,1:12] %>%
    `colnames<-`(c("stpug", as.character(by_tpu_dt[[7]][3,2:12]))) %>%
    gather("monthly_income_from_main_employment", "working_population_number", 
           - stpug) %>%
    mutate(working_population_number = 
             suppressWarnings(as.numeric(working_population_number)))
  
  # 8	- D401
  dt_8 <- by_tpu_dt[[8]][5:209,1:7] %>%
    `colnames<-`(c("stpug", as.character(by_tpu_dt[[8]][3,2:7]))) %>%
    gather("household_size", "domestic_households_number", - stpug) %>%
    mutate(domestic_households_number = 
             suppressWarnings(as.numeric(domestic_households_number)))
  
  # 9	- D402
  dt_9 <- by_tpu_dt[[9]][5:209,1:8] %>%
    `colnames<-`(c("stpug", as.character(by_tpu_dt[[9]][3,2:8]))) %>%
    gather("household_composition", "domestic_households_number", - stpug) %>%
    mutate(domestic_households_number = 
             suppressWarnings(as.numeric(domestic_households_number)))
  
  # 10 - D403
  dt_10 <- by_tpu_dt[[10]][5:209,1:13] %>%
    `colnames<-`(c("stpug", as.character(by_tpu_dt[[10]][3,2:13]))) %>%
    gather("monthly_domestic_household_income", "domestic_households_number", 
           - stpug) %>%
    mutate(domestic_households_number = 
             suppressWarnings(as.numeric(domestic_households_number)))
  
  # 11 - E501
  dt_11 <- by_tpu_dt[[11]][5:209,1:5] %>%
    `colnames<-`(c("stpug", as.character(by_tpu_dt[[11]][3,2:5]))) %>%
    gather("quarter_type", "occupied_quarters", - stpug) %>%
    mutate(occupied_quarters = 
             suppressWarnings(as.numeric(occupied_quarters)))
  
# search list  
dt <- sprintf("dt_%s", seq(1:11))
keywords_list <- sapply(dt, function(x) names(get(x)))
keywords_max <- seq_len(max(sapply(keywords_list, length)))
keywords <- t(sapply(keywords_list, "[", i = keywords_max)) %>% as_tibble()

search_list <- cbind(tbl = names(keywords_list), keywords, by_tpu) %>%
  mutate(keywords = paste(V1,V2,V3,V4, sep = " / ", na.rm = TRUE)) %>%
  select(- starts_with("V")) %>%
  mutate(tbl = as.character(tbl))

# 1 output data
if (!is.null(table_id)){
  table_ids <- c("A501", "A502", "A503", "C501", "C502", "C503", "C504",
                 "D401", "D402", "D403", "E501")
  if (!table_id %in% table_ids) stop("please enter a correct table id")
  
  search_tbl_name <- search_list[search_list$tbl_id == table_id,1]
  search_tbl <- get(search_tbl_name)
}

if (!is.null(keyword)){
  table_cates <- c(
    "ethnicity", "language", "age_group", "sex", "economic_activity_status", 
    "industry", "occupation", "monthly_income",
    "household_income", "household_size", "household_composition", "quarter_type")
  if (!keyword %in% table_cates) stop("cannot find any related tables")
  
  search_tbl_name <- search_list[str_detect(search_list$keywords,keyword),1]
  search_tbl <- get(search_tbl_name)
}

# 2 output charts
cate <- colnames(search_tbl[2])
# pie chart
pie <- search_tbl %>%
  group_by(category = get(colnames(search_tbl[2]))) %>%
  summarise(tot_number = sum(get(colnames(search_tbl[3])), na.rm = TRUE)) %>%
  ggplot(aes(x = "",
             y = tot_number,
             fill = category)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  xlab("") +
  labs(title = paste(cate,"percentage pie chart")) +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  geom_text(aes(label = scales::percent(tot_number / sum(tot_number))),
            position = position_stack(vjust = 0.5))

# density plot
dp <- search_tbl %>%
  ggplot(aes(get(colnames(search_tbl[3])))) +
  facet_wrap(~ get(colnames(search_tbl[2])), scales = "free") +
  # geom_density()
  geom_histogram(bins = 10)

dp1 <- search_tbl %>%
  spread(key = get(colnames(search_tbl[2])), value = get(colnames(search_tbl[3])))
dp2 <- dp1[-1]/rowSums(dp1[-1])
dp3 <- cbind(dp1[1],dp2) %>% gather(key = "category", value = "value", - stpug)
dp <- dp3 %>%
  ggplot(aes(get(colnames(dp3[3])))) +
  facet_wrap(~ get(colnames(dp3[2])), scales = "free") +
  geom_density() +
  scale_x_continuous(labels = percent) +
  labs(title = paste(cate," density chart of each category"), x = NULL) 

return(list(search_tbl,pie,dp))
}


# income analysis 
dt <- hk_census_stpug(table_id = "D403")[[1]]

tmp <- dt %>% 
  spread(key = monthly_domestic_household_income, value = domestic_households_number)
tmp2 <- tmp[-1] / rowSums(tmp[-1]) * 100
tmp3 <- cbind(tmp[1], tmp2[c(1,2,11)])
names(tmp3) <- c("stpug", "low_income_pctg", "median_income_pctg", "high_income_pctg")
tmp3 <- tmp3 %>% 
  mutate(low_income_pctg = ifelse(is.na(low_income_pctg),0,low_income_pctg))

x <- c("low", "median", "high")
y <- sprintf("{point.%s}", c("low_income_pctg", 
                             "median_income_pctg", 
                             "high_income_pctg"))
tltip <- tooltip_table(x, y)

hchart(tmp3, type = "columnrange",
       hcaes(x = stpug, low = low_income_pctg, high = high_income_pctg,
             color = median_income_pctg)) %>% 
  hc_chart(polar = TRUE) %>%
  hc_yAxis( max = 100, min = -10, labels = list(format = "{value} %"),
            showFirstLabel = FALSE) %>% 
  hc_xAxis(
    title = list(text = "income difference between highest and lowest"), 
    gridLineWidth = 1,
    labels = FALSE) 


  
