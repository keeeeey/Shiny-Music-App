library(tidyverse)
library(shiny)
library(shinydashboard)  # 新增
library(DT)

load("E:/上学威龙/【7CPDSR】 Programming with R/Session 4 User Interface; Server/steps/emissions.RData")

# 定义 UI
ui <- dashboardPage(
  
  dashboardHeader(title = "CO2 Emissions Dashboard"),
  
  dashboardSidebar( 
    sidebarMenu(
      menuItem("Plot", tabName = "plot", icon = icon("image", lib = "font-awesome")),
      menuItem("Table", tabName = "table", icon = icon("table", lib = "font-awesome"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "plot",
              sliderInput("years", h4("Years"), 
                          min = 1950, max = 2020, value = c(1960, 2010), step = 5, sep = ""),
              selectInput("country", h4("Country"), 
                          choices = emissions$country %>% unique(),
                          selected = c("United States", "China", "Germany"),
                          multiple = TRUE),
              selectInput("source", "CO2 sources included",
                          choices = emissions$source %>% unique(),
                          selected = c("coal", "gas", "oil"), multiple = TRUE),
              sliderInput("population", h4("Population (millions)"),
                          min = 0, max = 1400, step = 10, value = c(0,1400)),
              plotOutput("plot", height = 300)
      ),
      
      tabItem(tabName = "table",
              DT::dataTableOutput("table_1")
      )
    )
  )
)

# 定义 Server
server <- function(input, output) {
  data <- reactive({                
    emissions %>%                  
      filter(year >= input$years[1], 
             year <= input$years[2],
             country %in% input$country,
             source %in% input$source,
             population > input$population[1],
             population < input$population[2]) 
  })
  
  output$table_1 <- DT::renderDataTable({data()})
  
  output$plot <- renderPlot({
    data() %>% 
      group_by(year, country) %>%
      summarise(total_emissions = sum(emissions, na.rm = TRUE)) %>%  
      ggplot(aes(x = year, y = total_emissions, color = country)) + 
      geom_line() + theme_bw() + ylab("Total emissions")
  }, height = 300)
}

# 运行 App
shinyApp(ui = ui, server = server)

