###################################################

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

###################################################
server <- function(input, output) {
  
  #Data exploration plot begins
  output$data.main <- renderPlotly({
    if(length(input$data.index)==0){
      
        print("Please select at least one index")
      
    }else {
      
        data_index <- Agg.tidy[Agg.tidy$Index == input$data.index,]
        ggplot(data_index)+
                geom_line(aes(x = as.Date(Date), y = Price, by = Index, color = Index))+
                labs(x = "Date", y ="Price", title = "Time Series of Index(s) Price") 
        
    }})
  
  output$summary.stat <- renderDataTable(summary_statistics,
                                         options = list(dom = "t"))
  
  output$data.corr <- renderDataTable(corr.table, 
                                      options = list(dom = 't'))
    
  
  
  #End of Data exploration plot
  
  #Changepoint Plot begins
  
  output$Brent.Changepoint <- renderPlot({
    grid.draw(rbind(ggplotGrob(Brent.Plot1),ggplotGrob(Brent.Plot2), size = "last"))  
  })
  
  output$sp500.Changepoint <- renderPlot({
    grid.draw(rbind(ggplotGrob(sp500.Plot1),ggplotGrob(sp500.Plot2), size = "last"))
  })
  
  output$Gold.Changepoint <- renderPlot({
    grid.draw(rbind(ggplotGrob(gold.Plot1),ggplotGrob(gold.Plot2), size = "last"))
  })
  
  output$VIX.Changepoint <- renderPlot({
    grid.draw(rbind(ggplotGrob(vix.Plot1),ggplotGrob(vix.Plot2), size = "last"))
  })
  
  output$usdeur.changepoint <- renderPlot({
    grid.draw(rbind(ggplotGrob(usdeur.Plot1),ggplotGrob(usdeur.Plot2), size = "last"))
  })
  
  #End of Changepoint plots
  
  #Changpoint result table begins
  
  output$changepoint.event <- renderDataTable(
    
    if(input$changepoint.display == "Brent Oil"){

        News[which(as.Date(News$Date) %in% as.Date(B.Date)),]
      
    } else if (input$changepoint.display == "S&P500"){
      
        News[which(as.Date(News$Date) %in% as.Date(sp.Date)),]
      
    } else if (input$changepoint.display == "Gold"){
      
        News[which(as.Date(News$Date) %in% as.Date(g.Date)),]
      
    } else if (input$changepoint.display == "VIX"){
      
        News[which(as.Date(News$Date) %in% as.Date(v.Date)),]
      
    } else if (input$changepoint.display == "10Yr T-Bill"){
      
        News[which(as.Date(News$Date) %in% as.Date(b10.Date)),]
      
    } else if (input$changepoint.display == "USD-EUR RR"){
      
        News[which(as.Date(News$Date) %in% as.Date(u.Date)),]
      
    }
  )
  
  #End of Changepoint result table
  
  #Result data exploration begins
  
  output$result.main <- renderPlot({
    if(length(input$result.index)==0){
      
      print("Please select at least one index")
      
    } else {
      data_index <- Agg.tidy[Agg.tidy$Index == input$result.index,]
      result.plot <- ggplot(data_index)+
        geom_line(aes(x = as.Date(Date), y = Price, by = Index, color = Index))+
        labs(x = "Date", y ="Price", title = "Time Series of Index(s) Price")
      if (input$Brent.cp.result){
        result.plot <- result.plot + geom_vline(xintercept = as.Date(B.Date) , color = "red", linetype = "dotdash")
      }
      if (input$SP500.cp.result){
        result.plot <- result.plot + geom_vline(xintercept = as.Date(sp.Date) , color = "Blue", linetype = "dotdash")
      }
      if (input$gold.cp.result){
        result.plot <- result.plot + geom_vline(xintercept = as.Date(g.Date) , color = "Yellow", linetype = "dotdash")
      }
      if (input$vix.cp.result){
        result.plot <- result.plot + geom_vline(xintercept = as.Date(v.Date) , color = "green", linetype = "dotdash")
      }
      if (input$usdeur.cp.result){
        result.plot <- result.plot + geom_vline(xintercept = as.Date(u.Date) , color = "purple", linetype = "dotdash")
      }
      result.plot
    }

    
  })
  
  #End of result data exploration
  
  #Result observation charts begins
  
  output$obs.sp500 <- renderPlot({
    
    sp500.Plot2 + ggtitle("S&P 500 Changepoints")
    
  })
  
  output$obs.fc <- renderPlot({
    
    grid.draw(rbind(ggplotGrob(Brent.fc),ggplotGrob(sp500.fc),ggplotGrob(gold.fc),ggplotGrob(vix.fc),ggplotGrob(usdeur.fc), size = "last"))
    
  })
  
  output$obs.overlap <- renderPlot({
    
    grid.draw(rbind(ggplotGrob(Brent.Plot3),ggplotGrob(gold.Plot3),size = "last"))
    
  })
  
  output$obs.dt <- renderPlot({
    
    grid.draw(rbind(ggplotGrob(Brent.dt),ggplotGrob(gold.dt),ggplotGrob(sp500.dt),size = "last"))
    
  })
  
  #End of result observation chars
  
  #Begin of upload your own data
  output$user.changepoint <- renderPlot({
    req(input$User.data)
    
    user.data <- read.csv(input$User.data$datapath)  
    
    user.data <- user.data %>% 
                 mutate(Date = dmy(as.character(Date))) %>% 
                 mutate(PX_LAST = as.character(PX_LAST)) %>% 
                 mutate(PX_LAST = as.numeric(PX_LAST))
    
    user.data <- clean.function(user.data)
    
    user.Changepoint <- (cpt.meanvar(user.data$log_return, 
                                      method = "PELT", 
                                      test.stat = "Normal"))
    
    user.Date <-user.data$Date[cpts(user.Changepoint)]
    
    User.Plot1 <- ggplot(user.data, aes(as.Date(Date),log_return))+ 
      geom_line()+
      geom_vline(xintercept = as.Date(user.Date), color = "red", linetype = "dotdash", size = 1)+
      ylab("log returns")+
      ggtitle("User Data Changepoints Analysis")+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    
    User.Plot2 <- ggplot(user.data, aes(as.Date(Date), Price))+  geom_line()+
      geom_vline(xintercept = as.Date(user.Date), color = "red", linetype = "dotdash", size = 1)+
      xlab("Date")+ 
      ylab("Price")
    
    grid.draw(rbind(ggplotGrob(User.Plot1),ggplotGrob(User.Plot2), size = "last"))
    
  })
  
  output$user.dates <- renderDataTable({
    req(input$User.data)
    
    user.data <- read.csv(input$User.data$datapath)  
    
    user.data <- user.data %>% 
      mutate(Date = dmy(as.character(Date))) %>% 
      mutate(PX_LAST = as.character(PX_LAST)) %>% 
      mutate(PX_LAST = as.numeric(PX_LAST))
    
    user.data <- clean.function(user.data)
    
    user.Changepoint <- (cpt.meanvar(user.data$log_return, 
                                     method = "PELT", 
                                     test.stat = "Normal"))
    
    user.Date <-user.data$Date[cpts(user.Changepoint)]
    
  
    as.tibble(user.Date)
    
  })
  
  output$user.text <- renderText({
    req(input$User.data)
    
    print("Changepoint Dates:")
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function(){
      paste("Brent_Price", ".csv", sep = "")
    },
    
    content = function(file){
      write.csv(sample.data, file, row.names = TRUE)
    }
    
    
  )
  
  output$downloadDate <- downloadHandler(
    
    filename = function(){
      paste("Userchangepoint_date", ".csv", sep = "")
    },
    
    content = function(file){
      req(input$User.data)
      
      user.data <- read.csv(input$User.data$datapath)  
      
      user.data <- user.data %>% 
        mutate(Date = dmy(as.character(Date))) %>% 
        mutate(PX_LAST = as.character(PX_LAST)) %>% 
        mutate(PX_LAST = as.numeric(PX_LAST))
      
      user.data <- clean.function(user.data)
      
      user.Changepoint <- (cpt.meanvar(user.data$log_return, 
                                       method = "PELT", 
                                       test.stat = "Normal"))
      
      user.Date <-user.data$Date[cpts(user.Changepoint)]
      
      write.csv(user.Date, file, row.names = T) 
    }
    
    
  )
  
  
  #End of upload your own data 
}

