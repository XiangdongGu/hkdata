#-----------------------------------------------------------------------------#
# HONG KONG INSURANCE PROVISIONAL STATISTICS
#-----------------------------------------------------------------------------#
library(hkdata)
library(tidyverse)
library(ggthemes)

get_yearly <- function(year) {
  data <- hkia_long_term_provisional(year, 4)$L1
  data <- data %>%
    filter(by_group == "Onshore Offshore",
           !grepl("Number of", value_type),
           grepl("Onshore|Offshore", category),
           !grepl("Total", insurer_eng)) %>%
    mutate(category = gsub(".+(Onshore|Offshore)", "\\1", category)) %>%
    group_by(insurer_eng, category) %>%
    summarise(premium = sum(value)) %>%
    ungroup() %>%
    mutate(year = year)
  data
}

data <- list()

for (y in 2014:2018) {
  d <- get_yearly(y)
  data <- bind_rows(data, d)
}

idata <- data %>%
  spread(category, premium) %>%
  mutate(total = Onshore + Offshore,
         pct = Offshore / total)

top5 <- idata %>%
  group_by(insurer_eng) %>%
  summarise(total = sum(total)) %>%
  ungroup() %>%
  arrange(desc(total)) %>%
  filter(row_number() <= 5) %>%
  select(insurer_eng) %>%
  pull("insurer_eng")

idata <- idata %>%
  filter(insurer_eng %in% top5)

anno <- idata %>% filter(year == 2018) %>% pull(pct)
anno_t <- idata %>% filter(year == 2018) %>% pull(insurer_eng)

ggplot(idata, aes(year, pct, group = insurer_eng, color = insurer_eng)) +
  geom_line(linetype = 2) +
  geom_point(color = "steelblue") +
  theme_wsj() + 
  scale_y_continuous(labels = scales::percent) + 
  # annotate("text", x = 2018, y = anno, label = anno_t) +
  ggtitle("Top 5 Insuers' Offshore Business") +
  theme(legend.position = "none",
        title = element_text(size = 12))
  
  
