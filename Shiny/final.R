library(shiny)
library(readxl)
library(nlme)
library(tidyverse)
library(ggfortify)
library(lubridate)
library(grid)
library(gridExtra)
library(plotly)
library(devtools)
library(stringr)
library(grid)
library(shinydashboard)
library(benford.analysis)

df <- read_csv("new.csv", col_types = cols(X1 = col_skip()))
sum <- read_csv("sum.csv", col_types = cols(X1 = col_skip()))
data <- sum


######################


ui <- dashboardPage(
  dashboardHeader(title = "Benford's law Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About & Methodology", tabName = "about", icon = icon("info")),
      menuItem("Data Exploration", tabName = "data", icon = icon("database")),
      menuSubItem("Raw data", tabName = "rawdata", icon = icon("database")),
      menuSubItem("Plot raw data", tabName = "plot_rawdata", icon = icon("database")),
      menuItem("Benford Analysis", tabName = "benford_analysis", icon = icon("area-chart")),
      menuItem("Result", tabName = "result", icon = icon("file")),
      menuItem("Evaluation", tabName = "explain", icon = icon("file"))
    )
  ),

  dashboardBody(
    tabItems(
      # About and Methodology Tab
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            title = "About the Project",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,
            print("This project is to use Benford's law to examine the statistics 
of aviation traffic data. Once in every few months, department of transportation 
of United States will release the data of aviation traffic, which is 
                      provided by each airline. I am interested in finding out whether the data are 
'true' or not by Benford's law.Particularly, I selected four variables that are distinctive but also 
correlated with each other. 
                  They are number of available seats, number of passengers, 
                  the distance of the flight and the airtime.")
          )
        ),

        fluidRow(
          box(
            title = "Methodology",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,
            print("Benford's law is a phenomenological law also called the first digit law. 
                      This law states that in the listings, tables of statistics, etc., 
                      the digit 1 tends to occur with probability of 30%, greater than 
the expected of 11% (i.e., one out of nine).
                      Benford’s Law analyses were conducted using package of Benford's Analysis in r.")
          )
         
          
        ),
        fluidRow(
          box(
            title = "References",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,
            collapsed = T,
            print("Benford Analysis for Data Validation and Forensic Analytics
  http://github.com/carloscinelli/benford.analysis"),
            h4(""),
            print("Bureau of Transportation Statistics https://www.bts.gov/")
          )
        )
      ),
      tabItem(
        tabName = "data",
        fluidRow(
          box(selectizeInput("city",
            label = "City Name(s) of Interest",
            choices = unique(df$DEST_CITY_NAME), multiple = T,
            options = list(maxItems = 5, placeholder = "Select a name"),
            selected = "Atlanta, GA"
          )),
          box(selectizeInput("airline",
            label = "Airline Name(s) of Interest",
            choices = unique(df$UNIQUE_CARRIER_NAME), multiple = T,
            options = list(maxItems = 5, placeholder = "Select a name"),
            selected = "Southwest Airlines Co."
          )),
          box(
            title = "Travelling passengers",
            width = 9,
            solidHeader = TRUE,
            status = "primary",
            collapsible = F,
            plotlyOutput("data.main")
          )
        )
      ),
      tabItem(
        tabName = "rawdata",
        fluidRow(
          column(
            4,
            selectInput(
              "UNIQUE_CARRIER_NAME",
              "Unique carrier",
              c(
                "All",
                unique(as.character(sum$UNIQUE_CARRIER_NAME))
              )
            )
          ),
          column(
            4,
            selectInput(
              "YEAR",
              "Year",
              c(
                "All",
                unique(as.character(sum$YEAR))
              )
            )
          )
        ),
        # Create a new row for the table.
        box(
          title = "Raw data", width = NULL, status = "primary",
          div(style = "overflow-x: scroll", DT::dataTableOutput("table"))
        )
      ),
      tabItem(
        tabName = "plot_rawdata",
        h2("Plot of raw data"),
        fluidPage(
          title = "Airline_data",
          plotOutput("plot"),
          hr(),
          fluidRow(
            column(
              3,
              h4("Travelling data"),
              br(),
              checkboxInput("jitter", "Jitter"),
              checkboxInput("smooth", "Smooth")
            ),
            column(4,
              offset = 1,
              selectInput("x", "X", names(sum), selected = "log_passenger", multiple = FALSE),
              selectInput("y", "Y", names(sum), selected = "log_airtime", multiple = FALSE),
              selectInput("color", "Color", c("None", names(sum)))
            )
          )
        )
      ),
      tabItem(
        tabName = "benford_analysis",
        fluidRow(
          box(selectizeInput("type",
            label = "Type of data",
            choices = names(sum)[4:7], multiple = F,
            selected = "sum_seats"
          )),
          box(selectInput("number",
            label = "Number of first digits",
            choices = c("1", "2", "3"), multiple = F,
            selected = "1"
          )),
          box(
            title = "Benford Analysis",
            width = 11,
            solidHeader = TRUE,
            status = "primary",
            collapsible = F,
            plotOutput("trybenford")
          )
        )
      ),
      tabItem(
        tabName = "result",
        h2("Get suspecious data"),
        fluidRow(
          box(selectizeInput("interesting",
            label = "Type of data",
            choices = names(sum)[4:7], multiple = F,
            selected = "sum_seats"
          )),
          box(
            title = "Suspecious data", width = NULL, status = "primary",
            div(style = "overflow-x: scroll", DT::dataTableOutput("suspecious"))
          )
        )
      ),
      tabItem(
        tabName = "explain",
        fluidRow(
          box(
            title = "Possible explaination of suspecious data",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            collapsible = FALSE,
            print("Benford's law successfully detected some suspecious rows in the dataframe. For example,
                  on 2015 August, ABS JETS A.S company logged a total of 35 passengers with 0 airtime.
                  How could it be possible for an airline company to transfer passengers without logging 
                  the flight time? Another example is from ABX Air Inc, which transported total of 0 
                  passengers on March 2015 while loggin 108233 total airtime. I bet this company will go
                  towards bankrupt in very short future. One of the probable explanations for such weird result 
                  is that data are not properly collected. Another possible explanation is that some airlines 
                  only do cargo flight, which results in large amount of airtime but no records of passengers 
                  or seats.")
          )
        )
      )
    )
  )
)

server <-

  shinyServer(function(input, output) {
    output$table <- DT::renderDataTable(DT::datatable({
      if (input$UNIQUE_CARRIER_NAME != "All") {
        data <- data[data$UNIQUE_CARRIER_NAME == input$UNIQUE_CARRIER_NAME, ]
      }
      if (input$YEAR != "All") {
        data <- data[data$YEAR == input$YEAR, ]
      }
      data
    }))



    output$plot <- renderPlot({
      p <- ggplot(sum, aes_string(x = input$x, y = input$y)) + geom_point()

      if (input$color != "None") {
        p <- p + aes_string(color = input$color)
      }

      if (input$jitter) {
        p <- p + geom_jitter()
      }
      if (input$smooth) {
        p <- p + geom_smooth()
      }

      print(p)
    })


    output$data.main <- renderPlotly({
      if (length(input$city) == 0) {
        city_select <- df %>% filter(UNIQUE_CARRIER_NAME %in% input$airline)

        usamap <- borders("usa", colour = "#efede1", fill = "#efede1", bg = "grey15") # create a layer of borders
        states <- map_data("state")
        ggplot(city_select) + usamap + geom_polygon(
          data = states, aes(x = long, y = lat, group = group),
          color = "black", fill = "white"
        ) +
          geom_point(aes(x = origin_lng, y = origin_lat), col = "#970027", size = 0.05) +
          geom_segment(aes(
            x = origin_lng, y = origin_lat, xend = dest_lng,
            yend = dest_lat
          ),
          col = "#b29e7d",
          size = city_select$log_passenger / 50
          ) + coord_fixed(1.3)
      } else {
        city_select <- df %>% filter(DEST_CITY_NAME %in% input$city & UNIQUE_CARRIER_NAME %in% input$airline)

        usamap <- borders("usa", colour = "#efede1", fill = "#efede1", bg = "grey15") # create a layer of borders
        states <- map_data("state")
        ggplot(city_select) + usamap + geom_polygon(
          data = states, aes(x = long, y = lat, group = group),
          color = "black", fill = "white"
        ) +
          geom_point(aes(x = origin_lng, y = origin_lat), col = "#970027", size = 0.05) +
          geom_segment(aes(
            x = origin_lng, y = origin_lat, xend = dest_lng,
            yend = dest_lat
          ),
          col = "#b29e7d",
          size = city_select$log_passenger / 50
          ) + coord_fixed(1.3)
      }
    })

    output$trybenford <- renderPlot({
      # subsum <- sum %>% select(input$type)
      sum_check <- benford(dplyr::pull(sum, input$type), number.of.digits = as.numeric(input$number))
      plot(sum_check)
    })

    output$suspecious <- DT::renderDataTable(DT::datatable({
      sum_check <- benford(dplyr::pull(sum, input$interesting), number.of.digits = as.numeric(input$number))
      passenger_suspect <- getSuspects(bfd = sum_check, data = sum)
      passenger_suspect
    }))
  })

shinyApp(ui, server)
