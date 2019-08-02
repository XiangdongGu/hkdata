
# Plot the 24-hour Air Pollutant Concentration ----------------------------

library(hkdata)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(gganimate)
library(plotly)

# the past 24-hour air pollutant concentration
raw_data <- pollutant_24hr()
pollutant <- raw_data$data %>% 
  mutate_at(vars(-StationName, -DateTime), as.numeric) %>% 
  mutate(DateTime = with_tz(parse_date_time(DateTime, orders = "adbYH:M:S z"),
                            tzone = "Asia/Hong_Kong"))

# now make the animation
plot_one <- function(data, pol) {
  p <- data %>%
    ggplot(aes_string(x = "DateTime", y = pol,
                      group = "StationName", 
                      color = "StationName")) +
    geom_point() +
    geom_line() +
    ggtitle(paste0("Concentration of ", pol, " at some stations")) +
    xlab("Date Time") + ylab(paste0(pol, " ug/m^3"))
  
  p
}

some_stations <- pollutant %>% 
  filter(StationName %in% c("Central/Western", "Mong Kok", "Sha Tin",
                            "Causeway Bay", "Central", "Tuen Mun"))

# can use gganimate to animate it
for(pol in c("NO2", "O3", "SO2", "PM10", "PM2.5")) {
  p <- plot_one(some_stations, pol) + transition_reveal(DateTime)
  anim_save(paste0("pollutant_", pol, ".gif"), animate(p))
}

# also try the plotly for interactive
plot_one(pollutant, "NO2") %>% ggplotly()
plot_one(pollutant, "O3") %>% ggplotly()
plot_one(pollutant, "SO2") %>% ggplotly()
plot_one(pollutant, "PM10") %>% ggplotly()
plot_one(pollutant, "PM2.5") %>% ggplotly()
