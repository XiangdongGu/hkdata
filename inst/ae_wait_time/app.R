library(shiny)
library(dplyr)
library(readr)
library(argonR)
library(argonDash)
library(highcharter)
library(viridis)
library(treemap)

ui <- argonDashPage(
  header = argonDashHeader(
    gradient = FALSE,
    color = "warning",
    separator = FALSE,
    separator_color = "secondary",
    height = 2000,
    tagList(
      h1("Accidenct and Emergency", 
         style = "color: white; font-family: 'Avenir'; margin-top: -35px;
         text-align:center; font-size: 20px; font-weight: bold;"),
      argonCard(
        width = 12,
        hover_lift = TRUE,
        shadow = TRUE,
        shadow_size = NULL,
        hover_shadow = FALSE,
        border_level = 0,
        icon = argonIcon("atom"),
        status = "warning",
        background_color = NULL,
        gradient = FALSE,
        floating = FALSE,
        highchartOutput("ae_plot")
      ),
      argonCard(
        width = 12,
        hover_lift = TRUE,
        shadow = TRUE,
        shadow_size = NULL,
        hover_shadow = FALSE,
        border_level = 0,
        icon = argonIcon("atom"),
        status = "warning",
        background_color = NULL,
        gradient = FALSE,
        floating = FALSE,
        highchartOutput("ae_plot_line")
      )
    )
  )
)

server <- function(input, output) {
  
  output$ae_plot <- renderHighchart({
    ae_data <- read_csv("ae_data.csv")
    fntltp <- JS("function(){
                  return this.point.x + ' ' +  this.series.yAxis.categories[this.point.y] + ':<br>' +
                  Highcharts.numberFormat(this.point.value, 2);
                  }")
    hchart(ae_data, "heatmap", hcaes(x = datetime, y = hosp_name, value = top_wait_hr)) %>% 
      hc_colorAxis(stops = color_stops(9, rev(inferno(9)))) %>% 
      hc_xAxis(title = list(text = NULL), style = list(fontFamily = "Avenir"),
               offset = -5, labels = list(rotation = 90, style = list(fontSize = "10px"))) %>% 
      hc_yAxis(title = list(text = ""), reversed = TRUE, offset = -10, tickLength = 0,
               gridLineWidth = 0, minorGridLineWidth = 0, style = list(fontFamily = "Avenir"),
               labels = list(style = list(fontSize = "10px"))) %>% 
      hc_tooltip(formatter = fntltp) %>%
      hc_title(text = "Accident and Emergency Waiting Time (Hours) by Hospital in July 2019", style = list(fontFamily = "Avenir")) %>% 
      hc_legend(layout = "vertical", verticalAlign = "top",
                align = "right", valueDecimals = 0) 
  })
  
  output$ae_plot_line <- renderHighchart({
    ae_data <- read_csv("ae_data.csv")
    ae_data <- ae_data %>% mutate(top_wait_hr = as.numeric(top_wait_hr))
    tm <- treemap(ae_data, index = "hosp_name",
                  vSize = "top_wait_hr", vColor = "top_wait_hr",
                  type = "value", palette = viridis(9))
    hctreemap(tm) %>% 
      hc_title(text = "Accident and Emergency Waiting Time (Hours) by Hospital in July 2019", style = list(fontFamily = "Avenir"))
  })
  
}

shinyApp(ui, server)
