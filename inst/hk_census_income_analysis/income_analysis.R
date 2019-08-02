
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



