library(hkdata)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(highcharter)
library(viridis)

# library(gganimate)

# flu_data <- flu_surveillance()
# 
# # version 1
# 
# p1 <- flu_data %>% 
#   select(year, week, pct_inf_lab_surv_a_h1:pct_inf_lab_surv_c) %>% 
#   replace(., is.na(.), 0) %>% 
#   gather(key, value, -year, -week) %>% 
#   mutate(
#     year = as.integer(year),
#     value = as.numeric(value)
#   ) %>% 
#   ggplot(aes(x = week, y = value, color = key)) +
#   geom_point() +
#   labs(x = "Week", y = "Percent", color = "Virus") +
#   theme(legend.position = "top") +
#   scale_color_discrete(labels = c("A(H1)", "A(H3)", "B", "C")) +
#   transition_time(year) +
#   labs(title = "Year: {frame_time}") +
#   shadow_mark(alpha = 0.3, size = 0.5)
# 
# # version 2
# 
# p2 <- flu_data %>% 
#   select(year, week, n_inf_lab_surv_a_h1:n_inf_lab_surv_c, pct_inf_lab_surv_a_h1:pct_inf_lab_surv_c) %>% 
#   replace(., is.na(.), 0) %>% 
#   gather(key, value, -year, -week) %>% 
#   mutate(virus = str_extract(key, "(a_h1|a_h3|b|c)$")) %>% 
#   mutate(key = str_extract(key, "^(n|pct)")) %>% 
#   spread(key, value) %>% 
#   mutate(
#     year = as.integer(year),
#     n = as.numeric(n),
#     pct = as.numeric(pct)
#   ) %>% 
#   ggplot(aes(x = week, y = pct, color = virus)) +
#   geom_jitter(aes(size = n), alpha = 0.8) +
#   labs(x = "Week", y = "Percent", color = "Virus") +
#   theme_bw() +
#   theme(legend.position = "top", text = element_text(size = 15)) +
#   scale_x_continuous(breaks = seq(1, 53, 5)) +
#   scale_color_discrete(labels = c("A(H1)", "A(H3)", "B", "C")) +
#   scale_size_continuous(guide = FALSE) +
#   transition_time(year) +
#   labs(title = "Year: {frame_time}") +
#   shadow_mark(alpha = 0.3, size = 0.5)
# 
# animate(p2, width = 780, height = 480, res = 120)

# Surveillence ----------------------------------------------------------------

flu_data <- flu_surveillance()

# adm

flu_data_adm <- flu_data %>%
  select(from, rate_inf_pub_hosp_0_5:rate_inf_pub_hosp_65_abv) %>%
  gather(key, value, -from) %>%
  mutate(from = as.Date(from)) %>%
  mutate(
    key = factor(
      case_when(
        grepl("0_5$", key) ~ "0 - 5",
        grepl("6_11$", key) ~ "6 - 11",
        grepl("12_17$", key) ~ "12 - 17",
        grepl("18_49$", key) ~ "18 - 49",
        grepl("50_64$", key) ~ "50 - 64",
        grepl("65_abv$", key) ~ "65 +",
      ), levels = c("0 - 5", "6 - 11", "12 - 17", "18 - 49", "50 - 64", "65 +"),
      ordered = TRUE
    )
  )

write_csv(flu_data_adm, "inst/health/flu_data_adm.csv")

hchart(flu_data_adm, "line", hcaes(x = from, y = value, group = key)) %>% 
  hc_title(text = "Influenza Admission Rate in Public hospitals") %>% 
  hc_xAxis(title = list(text = "Date")) %>% 
  hc_yAxis(title = list(text = "Admission Rate (per 10,000 ppl)"))

# gp

flu_data_gp <- flu_data %>%
  select(from, rate_ili_gopc:rate_ili_gp, rate_ili_consult_cmp) %>%
  gather(key, value, -from) %>%
  mutate(from = as.Date(from)) %>%
  mutate(
    key = 
      case_when(
        grepl("gopc$", key) ~ "GOPC",
        grepl("gp$", key) ~ "GP",
        grepl("cmp$", key) ~ "CMP"
      )
  )

write_csv(flu_data_gp, "inst/health/flu_data_gp.csv")

hchart(flu_data_gp, "line", hcaes(x = from, y = value, group = key)) %>% 
  hc_title(text = "ILI Consultation at Clinics") %>% 
  hc_xAxis(title = list(text = "Date")) %>% 
  hc_yAxis(title = list(text = "ILI Consultation Rate (per 1,000 ppl)"))

# lab

flu_data_lab <- flu_data %>%
  select(from, n_inf_lab_surv_a_h1:n_inf_lab_surv_c) %>%
  gather(key, value, -from) %>%
  mutate(from = as.Date(from)) %>%
  mutate(
    key = 
      case_when(
        grepl("a_h1$", key) ~ "A(H1)",
        grepl("a_h3$", key) ~ "A(H3)",
        grepl("b$", key) ~ "B",
        grepl("c$", key) ~ "C"
      )
  ) %>% 
  mutate(value = as.numeric(value))

write_csv(flu_data_lab, "inst/health/flu_data_lab.csv")

hchart(flu_data_lab, "line", hcaes(x = from, y = value, group = key)) %>% 
  hc_title(text = "No. of Positive Detections of Seasonal Influenza Viruses") %>% 
  hc_xAxis(title = list(text = "Date")) %>% 
  hc_yAxis(title = list(text = "Count"))

# sevr

flu_data_sevr <- flu_data %>%
  select(from, n_svr_inf_weekly_0_17:n_svr_inf_weekly_65_abv) %>%
  gather(key, value, -from) %>%
  mutate(from = as.Date(from)) %>%
  mutate(
    key = factor(
      case_when(
        grepl("0_17$", key) ~ "0 - 17",
        grepl("18_49$", key) ~ "18 - 49",
        grepl("50_64$", key) ~ "50 - 64",
        grepl("65_abv$", key) ~ "65 +",
      ), levels = c("0 - 17", "18 - 49", "50 - 64", "65 +"),
      ordered = TRUE
    )
  ) %>% 
  replace(., is.na(.), 0)

write_csv(flu_data_sevr, "inst/health/flu_data_sevr.csv")

hchart(flu_data_sevr, "line", hcaes(x = from, y = value, group = key)) %>% 
  hc_title(text = "Number of Severe Influenza Cases") %>% 
  hc_xAxis(title = list(text = "Date")) %>% 
  hc_yAxis(title = list(text = "Count"))

# AE Wait Time ----------------------------------------------------------------

# vs <- hist_file_versions(ae_wait_time_url(), start = "2019-07-21", end = "2019-07-27")
# ae_data <- lapply(
#   vs$timestamps,
#   function(timestamp) {
#     print(timestamp)
#     ae_wait_time(timestamp) %>% mutate(time = timestamp)
#   }
# )
# 
# ae_data <- do.call("rbind", ae_data)
# 
# ae_data <- ae_data %>% 
#   mutate(
#     top_wait_hr = case_when(
#       top_wait == "Around 1 hour" ~ 0.5,
#       top_wait == "Over 1 hour" ~ 1,
#       top_wait == "Over 2 hours" ~ 2,
#       top_wait == "Over 3 hours" ~ 3,
#       top_wait == "Over 4 hours" ~ 4,
#       top_wait == "Over 5 hours" ~ 5,
#       top_wait == "Over 6 hours" ~ 6,
#       top_wait == "Over 7 hours" ~ 7,
#       top_wait == "Over 8 hours" ~ 8
#     )
#   )
# 
# ae_data <- ae_data %>% mutate(timestamp = ymd_hm(time))
# ae_data <- ae_data %>% mutate(datetime = format.Date(timestamp, "%d %H:%M"))

# write_csv(ae_data, "inst/health/ae_data.csv")

fntltp <- JS("function(){
  return this.point.x + ' ' +  this.series.yAxis.categories[this.point.y] + ':<br>' +
  Highcharts.numberFormat(this.point.value, 2);
}")

hchart(ae_data, "heatmap", hcaes(x = datetime, y = hosp_name, value = top_wait_hr)) %>% 
  hc_colorAxis(stops = color_stops(7, rev(inferno(7)))) %>% 
  hc_xAxis(title = list(text = NULL),
           offset = -5, labels = list(rotation = 90, style = list(fontSize = "10px"))) %>% 
  hc_yAxis(title = list(text = ""), reversed = TRUE, offset = -10, tickLength = 0,
           gridLineWidth = 0, minorGridLineWidth = 0,
           labels = list(style = list(fontSize = "10px"))) %>% 
  hc_tooltip(formatter = fntltp) %>%
  hc_title(text = "Accident and Emergency Waiting Time (Hours) by Hospital in July 2019") %>% 
  hc_legend(layout = "vertical", verticalAlign = "top",
            align = "right", valueDecimals = 0) %>% 
  hc_size(width = 1000, height = 400) %>% 
  hc_add_theme(hc_theme_google())
