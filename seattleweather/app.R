
## opening libraries:

library(tidyverse)
library(shiny)
library(lubridate) ## used to manipulate the dates from the data


## loading data:

weather <- read_delim("seattle_weather_1948-2017.csv.xls")


## mutating data:

some_date <- weather$DATE

weather_mon <- weather %>% 
  mutate("month" = month(as.POSIXlt(some_date, format="%Y/%m/%d")))



## problem set work:

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
                   along with information about whether it rained that day, and 
                   if it did, the amount of precipitation that resulted."),
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
                          p("You can analyze the daily maximum temperature in 
                          Seattle. Select the range of dates you'd like to see. 
                          You will see a daily scatterplot and the corresponding
                          trend lines, if you choose.")
                   ),
                ## visual changes:
                   column(6,
                          h5("Visuals:"),
                          checkboxInput("checkbox", label = "Show Trends", value
                                        = TRUE),
                          radioButtons("color", "Choose color:",
                                       choices = c("orangered3", "gold3", 
                                                   "green4","blue3","purple3"))
                   ),
                ## data changes:
                   column(6,
                          dateRangeInput("daterange", label = h5("Determine Date
                                                                 Range:"),
                                         start = "1948-01-01",
                                         end = "2017-12-14",
                                         min = "1948-01-01",
                                         max = "2017-12-14",
                                         startview = "decade"),
                          p("Select a range of dates from January 1, 1948 to 
                          December 14, 2017 to learn more. Make sure to use a",
                            strong("later date"), "for the first input."),
                          p(em("The format for the date is Year-Month-Date."))
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
                 fluidRow(
                   column(12,
                  ## explanations/information:
                   h4("Table:"),
                   p("You can analyze the different average precipitation 
                     patterns over different seasons:", em("spring, summer,
                     fall"), "and", em("winter."), "Choose which option
                     you'd like below."),
                  uiOutput("seasons")
                   )
                 )
                 ),
               mainPanel(
                 p(strong("Fun Fact:"), "The average amount of precipitation 
                   in this season is", textOutput(outputId = "averagepre", 
                    inline=T),"."),
                 dataTableOutput("datatable")
               )
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
      p <- ggplot(years_filtered(), aes(DATE, TMAX)) +
        geom_point(col = input$color, size = 3) +
      {if(input$checkbox) geom_smooth(col = "gray2")} +
        labs(x = "Date",
             y = "Maximum Temperature (F)",
             title = "Daily Maximum Temperature over the Decades") +
        theme(text = element_text(size = 15))
      if(input$daterange[2] < input$daterange[1]){
        p <- p +
          labs(title = "Note: Please ensure you chose the correct order for 
               your dates.")
      }
      p
  })
  output$textDateRange <- renderText({
    paste("This graph now shows the different maximum daily temperature 
          between", input$daterange[1], "and", input$daterange[2],".")
  })
  
## table information:
  ### radio buttons:
output$seasons <- renderUI({
  radioButtons("seasonscheck", "Choose a season:",
               choices = list("Spring (March, April, May)",
                              "Summer (June, July, August)",
                              "Fall (September, October, November)",
                              "Winter (December, January, February)"))
})
  ### data table:
output$datatable <- renderDataTable({
  if(input$seasonscheck == "Spring (March, April, May)") {
    return(weather_mon %>% 
             filter(month == "3" |
                      month == "4" |
                      month == "5") %>% 
             select(DATE, PRCP))
  }
  if(input$seasonscheck == "Summer (June, July, August)") {
    return(weather_mon %>% 
             filter(month == "6" |
                      month == "7" |
                      month == "8") %>% 
             select(DATE, PRCP))
  }
  if(input$seasonscheck == "Fall (September, October, November)") {
    return(weather_mon %>% 
             filter(month == "9" |
                      month == "10" |
                      month == "11") %>% 
             select(DATE, PRCP))
  }
  if(input$seasonscheck == "Winter (December, January, February)") {
    return(weather_mon %>% 
             filter(month == "12" |
                      month == "1" |
                      month == "2") %>% 
             select(DATE, PRCP))
  }
})    

 ## finding average value for textual output
  ### categorizing the dates:
spring <- weather_mon %>% 
  filter(month == "3" |
           month == "4" |
           month == "5") %>% 
  filter(!is.na(PRCP))
summer <- weather_mon %>% 
  filter(month == "6" |
           month == "7" |
           month == "8") %>% 
  filter(!is.na(PRCP))
fall <- weather_mon %>% 
  filter(month == "9" |
           month == "10" |
           month == "11") %>% 
  filter(!is.na(PRCP))
winter <- weather_mon %>% 
  filter(month == "12" |
           month == "1" |
           month == "2") %>%
  filter(!is.na(PRCP))
  ### computing average precipitation
output$averagepre <- renderText({
  if(input$seasonscheck == "Spring (March, April, May)") {
    return(mean(spring$PRCP))
  }
  if(input$seasonscheck == "Summer (June, July, August)") {
    return(mean(summer$PRCP))
  }
  if(input$seasonscheck == "Fall (September, October, November)") {
    return(mean(fall$PRCP))
  }
  if(input$seasonscheck == "Winter (December, January, February)") {
    return(mean(winter$PRCP)) ## keep getting not numeric
  }
}) 
}


shinyApp(ui = ui, server = server)
