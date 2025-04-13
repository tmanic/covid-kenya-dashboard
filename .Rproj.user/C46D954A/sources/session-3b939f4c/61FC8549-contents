

## Shiny Documents
# Load packages
library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(readr)

# Load data
url <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"
covid_data <- read_csv(url)

# Filter for Kenya only
kenya_data <- covid_data %>%
  filter(location == "Kenya") %>%
  select(date, total_cases, new_cases, total_deaths, new_deaths) %>%
  mutate(date = as.Date(date))

# UI
ui <- fluidPage(
  titlePanel("COVID-19 Dashboard: Kenya"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_range", "Select Date Range:",
                     start = min(kenya_data$date),
                     end = max(kenya_data$date)),
      br(),
      helpText("Data from Our World in Data")
    ),
    
    mainPanel(
      fluidRow(
        column(4, h4("Total Cases"), textOutput("total_cases")),
        column(4, h4("Total Deaths"), textOutput("total_deaths"))
      ),
      br(),
      plotlyOutput("trend_plot"),
      br(),
      DTOutput("data_table")
    )
  )
)

# Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    kenya_data %>%
      filter(date >= input$date_range[1], date <= input$date_range[2])
  })
  
  output$total_cases <- renderText({
    max(filtered_data()$total_cases, na.rm = TRUE)
  })
  
  output$total_deaths <- renderText({
    max(filtered_data()$total_deaths, na.rm = TRUE)
  })
  
  output$trend_plot <- renderPlotly({
    plot_ly(data = filtered_data(), x = ~date) %>%
      add_lines(y = ~new_cases, name = "New Cases", line = list(color = 'blue')) %>%
      add_lines(y = ~new_deaths, name = "New Deaths", line = list(color = 'red')) %>%
      layout(title = "Daily COVID-19 Cases & Deaths in Kenya",
             yaxis = list(title = "Count"),
             xaxis = list(title = "Date"))
  })
  
  output$data_table <- renderDT({
    datatable(filtered_data())
  })
}

# Run the app
shinyApp(ui = ui, server = server)

