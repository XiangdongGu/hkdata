library(shiny)
library(dplyr)
library(readr)
library(argonR)
library(argonDash)
library(highcharter)

ui <- argonDashPage(
  header = argonDashHeader(
    gradient = FALSE,
    color = "primary",
    separator = FALSE,
    separator_color = "secondary",
    tagList(
      h1("Hong Kong Flu Express", 
         style = "color: white; font-family: 'Avenir'; margin-top: -35px;
         text-align:center; font-size: 20px; font-weight: bold;"),
      argonCard(
        hover_lift = TRUE,
        shadow = TRUE,
        shadow_size = NULL,
        hover_shadow = FALSE,
        border_level = 0,
        icon = argonIcon("atom"),
        status = "primary",
        background_color = NULL,
        gradient = FALSE, 
        floating = FALSE,
        highchartOutput("flu_plot_adm")
      ),
      argonCard(
        hover_lift = TRUE,
        shadow = TRUE,
        shadow_size = NULL,
        hover_shadow = FALSE,
        border_level = 0,
        icon = argonIcon("atom"),
        status = "primary",
        background_color = NULL,
        gradient = FALSE, 
        floating = FALSE,
        highchartOutput("flu_plot_gp")
      ),
      argonCard(
        hover_lift = TRUE,
        shadow = TRUE,
        shadow_size = NULL,
        hover_shadow = FALSE,
        border_level = 0,
        icon = argonIcon("atom"),
        status = "primary",
        background_color = NULL,
        gradient = FALSE, 
        floating = FALSE,
        highchartOutput("flu_plot_lab")
      ),
      argonCard(
        hover_lift = TRUE,
        shadow = TRUE,
        shadow_size = NULL,
        hover_shadow = FALSE,
        border_level = 0,
        icon = argonIcon("atom"),
        status = "primary",
        background_color = NULL,
        gradient = FALSE, 
        floating = FALSE,
        highchartOutput("flu_plot_sevr")
      )
    )
  )
)

server <- function(input, output) {
  
  output$flu_plot_adm <- renderHighchart({
    flu_data_adm <- read_csv("flu_data_adm.csv")
    flu_data_adm <- flu_data_adm %>% 
      mutate(key = factor(key, levels = c("0 - 5", "6 - 11", "12 - 17", "18 - 49", "50 - 64", "65 +"), ordered = TRUE))
    hchart(flu_data_adm, "line", hcaes(x = from, y = value, group = key)) %>% 
      hc_title(text = "Influenza Admission Rate at Public hospitals", style = list(fontFamily = "Avenir")) %>% 
      hc_xAxis(title = list(text = "Date"), style = list(fontFamily = "Avenir")) %>% 
      hc_yAxis(title = list(text = "Admission Rate (per 10,000 ppl)"), style = list(fontFamily = "Avenir"))
  })
  
  output$flu_plot_gp <- renderHighchart({
    flu_data_gp <- read_csv("flu_data_gp.csv")
    hchart(flu_data_gp, "line", hcaes(x = from, y = value, group = key)) %>% 
      hc_title(text = "Influenza-like-illness Consultation at Clinics", style = list(fontFamily = "Avenir")) %>% 
      hc_xAxis(title = list(text = "Date"), style = list(fontFamily = "Avenir")) %>% 
      hc_yAxis(title = list(text = "ILI Consultation Rate (per 1,000 ppl)"), style = list(fontFamily = "Avenir"))
  })
  
  output$flu_plot_lab <- renderHighchart({
    flu_data_lab <- read_csv("flu_data_lab.csv")
    hchart(flu_data_lab, "line", hcaes(x = from, y = value, group = key)) %>% 
      hc_title(text = "Number of Positive Detections of Seasonal Influenza Viruses", style = list(fontFamily = "Avenir")) %>% 
      hc_xAxis(title = list(text = "Date"), style = list(fontFamily = "Avenir")) %>% 
      hc_yAxis(title = list(text = "Count"), style = list(fontFamily = "Avenir"))
  })
  
  output$flu_plot_sevr <- renderHighchart({
    flu_data_sevr <- read_csv("flu_data_sevr.csv")
    hchart(flu_data_sevr, "line", hcaes(x = from, y = value, group = key)) %>% 
      hc_title(text = "Number of Severe Influenza Cases", style = list(fontFamily = "Avenir")) %>% 
      hc_xAxis(title = list(text = "Date"), style = list(fontFamily = "Avenir")) %>% 
      hc_yAxis(title = list(text = "Count"), style = list(fontFamily = "Avenir"))
  })
  
}

shinyApp(ui, server)
