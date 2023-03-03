
## opening libraries

library(tidyverse)
library(shiny)
library(lubridate)


## loading data

weather <- read_delim("seattle_weather_1948-2017.csv.xls")


## mutating data

some_date <- weather$DATE

weather_mon <- weather %>% 
  mutate("month" = month(as.POSIXlt(some_date, format="%Y/%m/%d")))

weather_year <- weather %>% 
  mutate("year" = year(as.POSIXlt(some_date, format="%Y/%m/%d")))


## extra work space



## problem set work

ui <- fluidPage(
  titlePanel("Seattle Weather Data"),
  tabsetPanel(
    tabPanel("About Information",
               mainPanel(
                 ## information:
                 h4("What is this data?"),
                 p("This dataset describes daily", strong("Seattle weather"), 
                   "from", em("1948 till 2017"), "taking into account the 
                   highest and lowest temperature, in Fahrenheit, for the day,
                   along with information about the amount of precipitation that day
                   and how much, if it did."),
                 h4("Where is the data from?"),
                 p("This dataset was taken from", strong("Kaggle.")),
                 h4("What does the data contain?"),
                 p("The data has", nrow(weather), "observations and", 
                   ncol(weather), "variables.\n Here is a small, random sample
                   of the data:"),
                 ## data:
                 dataTableOutput("sample")
               )
             ),
    tabPanel("Plot", 
             sidebarLayout(
               sidebarPanel(
                 fluidRow(
                   column(12,
                ## general intro:
                          h4("Plot:"),
                          p("You can analyze the daily maximum temperature in Seattle.
                 Select the range of dates you'd like to see. You will see
                 a daily scatterplot and the corresponding trend lines, if you
                 choose.")
                   ),
                ## visual changes:
                   column(6,
                          h5("Visuals:"),
                          checkboxInput("checkbox", label = "Show Trends", value = TRUE),
                          radioButtons("color", "Choose color:",
                                       choices = c("orangered3", "gold3", "green4","blue3",
                                                               "purple3"))
                   ),
                ## data changes:
                   column(6,
                          dateRangeInput("daterange", label = h5("Determine Date Range:"),
                                         start = "1948-01-01",
                                         end = "2017-12-14",
                                         min = "1948-01-01",
                                         max = "2017-12-14",
                                         startview = "decade"),
                          p(em("Select a range of dates from January 1, 1948 to December
                            14, 2017 to learn more."))
                   )
                 )
               ),
               mainPanel(
                 plotOutput("scatterplot"),
                 textOutput("textDateRange")
               )
              )
             ),
    tabPanel("Table",
             sidebarLayout(
               sidebarPanel(
                 fluidRow()
               ),
               mainPanel()
             )
        )
    )
)


server <- function(input, output) {
## summary information:  
  output$sample <- renderDataTable({
    weather %>% 
      sample_n(5)
  })
  
## plot information:
  years_filtered <- reactive({
    weather %>% 
      filter(DATE > input$daterange[1] & DATE <= input$daterange[2])
  })
  output$scatterplot <- renderPlot({
      ggplot(years_filtered(), aes(DATE, TMAX)) +
        geom_point(col = input$color, size = 3) +
      {if(input$checkbox) geom_smooth(col = "gray2")} +
        labs(x = "Date",
             y = "Maximum Temperature (F)",
             title = "Daily Maximum Temperature over the Decades") +
        theme(text = element_text(size = 15))
  })
  output$textDateRange <- renderText({
    paste("This graph now shows the different maximum daily temperature 
          between", input$daterange[1], "and", input$daterange[2],".")
  })
  
## table information:
  
}


shinyApp(ui = ui, server = server)
