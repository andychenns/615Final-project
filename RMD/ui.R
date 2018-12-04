####################################################

library(shiny)
library(shinydashboard)
library(readxl)
library(nlme)
library(tidyverse)
library(changepoint)
library(ggfortify)
library(imputeTS)
library(lubridate)
library(padr)
library(grid)
library(gridExtra)
library(plotly)
library(devtools)
library(stringr)



####################################################

dashboardPage(
  dashboardHeader(title = "Aviation statistics analysis"),
  
  dashboardSidebar(
    #Create the sidebar menu tabs
    sidebarMenu(
      menuItem("About & Methodology", tabName = "about", icon = icon("info")),
      menuItem("Data Exploration", tabName = "data", icon = icon("database")),
      menuItem("Benford's Law detection", tabName = "changepoint", icon = icon("area-chart")),
      menuItem("Result", tabName = "result", icon = icon("file")),
      menuItem("Implication(s)", tabName = "imply", icon = icon("upload"))
    )
  ),
  
  #Create the body
  dashboardBody(
    tabItems(
      # About and Methodology Tab
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "About the Project", 
                  width = 12, 
                  solidHeader = TRUE, 
                  status = "primary",
                  collapsible  = TRUE,
                  h3("Objectives"),
                  print("Change points are abrupt variations in time series data. Such abrupt changes may represent transitions that occur between states. Detection of change points is useful in modelling and prediction of time series and is found in application areas such as medical condition monitoring, climate change detection, speech and image analysis, and human activity analysis.  I plan to apply several anomaly detection statistical methods (where R packages for the methods are readily available) to a financial time series. After the detection, I will attempt to search for the events happened that may cause the structual Changes.  ")
                )
              ),
              
              fluidRow(
                box(
                  title = "Methodology", 
                  width = 12, 
                  solidHeader = TRUE, 
                  status = "primary",
                  collapsible = TRUE,
                  print("Changepoint detection is the name given to the problem of estimating the point(s) at which the statistical properties of a time-series observations change. In this project, I focus on the changes in mean and variance. The penalty function is chosen as SIC (Schwarz Information Criterion); and we employ the PELT (Pruned Exact Linear Time) searching algorithm for the change points proposed by Killick et al. (2012). ")
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
                  print("Why Log Returns by Quantivity"),
                  h4(""),
                  print("How to detect breakouts in a time series in R by R statistics"),
                  h4(""),
                  print("changepoint: An R Package for Changepoint Analysis by Rebecca Killick and Idris A. Eckley"),
                  h4(""),
                  print("Killick R, Fearnhead P, Eckley IA (2012). “Optimal Detection of Changepoints with a
                        Linear Computational Cost.” Journal of the American Statistical Association, 107(500),
                        1590-98.")
                  )
                  )
                ),
      
      # Data Exploration Tab
      tabItem(tabName = "data",
              fluidRow(
                box(
                  title = "Controls", 
                  width = 3, 
                  solidHeader = TRUE, 
                  status = "primary",
                  collapsible  = F,
                  selectizeInput("data.index",
                                 label = "Index(s) of Interest",
                                 choices = unique(Agg.tidy$Index),
                                 multiple = T,
                                 options = list(maxItems = 10, placeholder = "select an Index"),
                                 selected = "Brent"
                                 
                  )
                ),
                
                box(
                  title = "Dataset", 
                  width = 9, 
                  solidHeader = TRUE, 
                  status = "primary",
                  collapsible  = F,
                  plotlyOutput("data.main")
                )
              ),
              
              fluidRow(
                box(
                  title = "Summary Statistics", 
                  width = 12, 
                  solidHeader = TRUE, 
                  status = "primary",
                  collapsible  = TRUE,
                  dataTableOutput("summary.stat")
                ),
                box(
                  title = "Correlation Matrix", 
                  width = 12, 
                  solidHeader = TRUE, 
                  status = "primary",
                  collapsible = TRUE,
                  dataTableOutput("data.corr")
                )
              )
      ),
      
      tabItem(tabName = "changepoint",
              fluidRow(
                tabBox(
                  title = "Changepoint Analysis",
                  id = "changepoint.display",
                  height = "500px",
                  width = 12,
                  tabPanel("Brent Oil",plotOutput("Brent.Changepoint")),
                  
                  tabPanel("S&P500",plotOutput("sp500.Changepoint")),
                  
                  tabPanel("Gold",plotOutput("Gold.Changepoint")),
                  
                  tabPanel("VIX",plotOutput("VIX.Changepoint")),
                  
                  tabPanel("USD-EUR RR",plotOutput("usdeur.changepoint"))
                  
                )
              ),
              
              fluidRow(
                box(
                  title = "Changepoint Event", 
                  width = 12, 
                  solidHeader = TRUE, 
                  status = "primary",
                  collapsible = TRUE,
                  dataTableOutput("changepoint.event")
                )
                
                
              )
      ),
      
      tabItem(tabName = "result",
              fluidRow(
                box(
                  title = "Controls", 
                  width = 3, 
                  solidHeader = TRUE, 
                  status = "primary",
                  collapsible  = T,
                  selectizeInput("result.index",
                                 label = "Index(s) of Interest",
                                 choices = unique(Agg.tidy$Index),
                                 multiple = T,
                                 options = list(maxItems = 10, placeholder = "select an Index"),
                                 selected = "Brent"
                  ),
                  checkboxInput("Brent.cp.result",
                                label = "Brent",
                                value = TRUE
                  ),
                  checkboxInput("SP500.cp.result",
                                label = "S&P 500",
                                value = FALSE
                  ),
                  checkboxInput("gold.cp.result",
                                label = "Gold",
                                value = FALSE
                  ),
                  checkboxInput("vix.cp.result",
                                label = "VIX",
                                value = FALSE
                  ),
                  checkboxInput("usdeur.cp.result",
                                label = "USD EUR RR",
                                value = FALSE
                  )
                  
                  
                  
                ),
                
                box(
                  title = "Results", 
                  width = 9, 
                  solidHeader = TRUE, 
                  status = "primary",
                  collapsible  = T,
                  plotOutput("result.main")
                )
              ),
              
              fluidRow(
                box(
                  title = "Observation 1", 
                  width = 12, 
                  solidHeader = TRUE, 
                  status = "primary",
                  collapsible  = TRUE,
                  collapsed = T,
                  h4("Stock Price seems to have more frequent structural Changepoints"),
                  plotOutput("obs.sp500")
                )
              ),
              
              fluidRow(
                box(
                  title = "Observation 2", 
                  width = 12, 
                  solidHeader = TRUE, 
                  status = "primary",
                  collapsible  = TRUE,
                  collapsed = T,
                  h4("High overlapping of changepoints between Brent and Gold"),
                  plotOutput("obs.overlap")
                )
              ),
              
              fluidRow(
                box(
                  title = "Observation 3", 
                  width = 12, 
                  solidHeader = TRUE, 
                  status = "primary",
                  collapsible  = TRUE,
                  collapsed = T,
                  h4("Financial Crisis of 2008 seem to affect all cross indexes"),
                  plotOutput("obs.fc")
                )
              ),
              
              fluidRow(
                box(
                  title = "Observation 4",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  collapsible = T,
                  collapsed = T,
                  h4("Commodies and market seems to detect structural change after the election of Donald Trump"),
                  plotOutput("obs.dt")
                  
                )
                
              )
              
      ),
      
      tabItem(tabName = "imply",
              fluidRow(
                box(
                  title = "upload your own data",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  collapsible = FALSE,
                  print("Please label: "),
                  print("column 1 to \"Date\" in d-m-y format and"),
                  print("column 2 to \"PX_LAST\""),
                  fileInput("User.data", "Choose CSV File",
                            multiple = FALSE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),
                  downloadButton("downloadData", "Download Sample Data"),
                  downloadButton("downloadDate", "Download Changepoint Date"),
                  p("note that in order to download Changepoint dates you would have to uploaded a csv file first.")
                  
                ),
                
                box(
                  title = "Changepoint analysis",
                  width = 12,
                  solidHeader = T,
                  collapsible = T,
                  status = "primary",
                  plotOutput("user.changepoint"),
                  h2(""),
                  h2(textOutput("user.text")),
                  h2(""),
                  dataTableOutput("user.dates")
                  
                )
              )      
      )
              )
    )
)

