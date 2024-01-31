# The dataset used contains operational variables of Air Handling Units (AHU) provided by Berkeley Lab
# Available here
library(shiny)
library(dplyr)
library(ggplot2)
library(magrittr)
library(lubridate)


ui <- fluidPage(
  h2("AHU Data Exploration Tool"),
  p(
    "This app demonstrate how to actively interact with data and change some parameters of the visualization."
  ),
  
  
  
  fluidRow(
    column(
      width = 4,
      selectInput(
        inputId = "variable",
        label = "Select a variable:",
        selected = "OA_TEMP",
        choices = c(
          'CHWC_VLV' = 'CHWC_VLV',
          'CHWC_VLV_DM' = 'CHWC_VLV_DM',
          'MA_TEMP' = 'MA_TEMP',
          'OA_CFM' = 'OA_CFM',
          'OA_DMPR' = 'OA_DMPR',
          'OA_DMPR_DM' = 'OA_DMPR_DM',
          'OA_TEMP' = 'OA_TEMP',
          'RA_CFM' = 'RA_CFM',
          'RA_DMPR' = 'RA_DMPR',
          'RA_DMPR_DM' = 'RA_DMPR_DM',
          'RA_TEMP' = 'RA_TEMP',
          'RF_CS' = 'RF_CS',
          'RF_SPD' = 'RF_SPD',
          'RF_SPD_DM' = 'RF_SPD_DM',
          'RF_WAT' = 'RF_WAT',
          'SA_CFM' = 'SA_CFM',
          'SA_SP' = 'SA_SP',
          'SA_SPSPT' = 'SA_SPSPT',
          'SA_TEMP' = 'SA_TEMP',
          'SA_TEMPSPT' = 'SA_TEMPSPT',
          'SF_CS' = 'SF_CS',
          'SF_SPD' = 'SF_SPD',
          'SF_SPD_DM' = 'SF_SPD_DM',
          'SF_WAT' = 'SF_WAT',
          'SYS_CTL' = 'SYS_CTL',
          'ZONE_TEMP_1' = 'ZONE_TEMP_1',
          'ZONE_TEMP_2' = 'ZONE_TEMP_2',
          'ZONE_TEMP_3' = 'ZONE_TEMP_3',
          'ZONE_TEMP_4' = 'ZONE_TEMP_4',
          'ZONE_TEMP_5' = 'ZONE_TEMP_5'
        )
      )
    ),
    column(
      width = 4,
      dateRangeInput(
        inputId = "daterange",
        label = "Date range:",
        start = "2018-01-01",
        end = "2018-06-01"
      )
    ),
    column(
      width = 4,
      sliderInput(
        inputId = "bins",
        label = "Select the number of bins:",
        min = 10,
        max = 100,
        value = 80
      )
    )
  ),
  
  
  column(width = 8,
         style = "padding-left:0px; padding-right:0px;",
         plotOutput("lineplot")),
  column(width = 4,
         style = "padding-left:0px; padding-right:0px;",
         plotOutput("histogram")),
  fluidRow(column(
    width = 12,
    helpText(
      "* The data used refer to a simulated Air Handling Units (AHU) provided by Berkeley Lab, available",
      a('here', href = 'https://faultdetection.lbl.gov/dataset/simulated-dd-ahu-dataset/')
    ),
    HTML(
      '<p style="align: center">© 2024 Copyright: <a href="https://github.com/RobertoChiosa"> Roberto Chiosa</a></p>'
    )
  ))
  
)

server <- function(input, output) {
  # load data
  df <- reactive({
    read.csv("data.csv", dec = '.', sep = ',') %>%
      mutate(
        DateTime = as.POSIXct(Datetime, format = '%Y-%m-%d %H:%M', tz = 'GMT'),
        Date = as.Date(DateTime)
      )
  })
  
  
  # define plot 1 histogram
  output$histogram <- renderPlot({
    df() %>%
      filter(Date >= input$daterange[1] &
               Date <= input$daterange[2]) %>%
      ggplot() +
      geom_histogram(
        aes(y = !!sym(input$variable)),
        color = "white",
        fill = "#337ab7",
        bins = input$bins
      ) +
      scale_x_continuous(expand = c(0, 0))+
      scale_y_continuous(position = "right")+
      theme_linedraw() +
      theme(
        text = element_text(size = 15),
        panel.grid.minor = element_line(size = 0.1, color = "gray")) +
      labs(y = NULL, x = NULL)
  })
  
  # define plot 1 line plot
  output$lineplot <- renderPlot({
    df() %>%
      filter(Date >= input$daterange[1] &
               Date <= input$daterange[2]) %>%
      ggplot() +
      geom_line(
        aes(x = DateTime, y = !!sym(input$variable)),
        color = "#337ab7",
        linewidth = 1
      ) +
      scale_x_datetime(expand = c(0, 0))+
      theme_linedraw() +
      theme(
        text = element_text(size = 15),
        panel.grid.minor = element_line(size = 0.1, color = "gray")) +
      labs(y = input$variable, x = NULL)
  })
  
}

shinyApp(ui = ui, server = server)