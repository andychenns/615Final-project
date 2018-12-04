#########################################################

library(readxl)
library(nlme)
source("./Scripts/Clean data function shiny.R")
library(shiny)
library(shinydashboard)
library(readxl)
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

#########################################################

#Read in the comparision files + Events
Weekdays <- read_excel("./Misc/Weekdays.xlsx")
News <- read_excel("./Misc/news.xlsx")

#Read in cleaned data
Brent <- read.csv("./Data/Brent.csv")
sp500 <- read.csv("./Data/sp500.csv")
gold <- read.csv("./Data/gold.csv")
vix <- read.csv("./Data/vix.csv")
usdeur <- read.csv("./Data/usdeur.csv")
Agg_price <- read.csv("./Data/Agg_Price.csv")
Agg.tidy <- read.csv("./Data/Agg_tidy.csv")

#Read in summary stats + corr table
summary_statistics <- read.csv("./Misc/summary.csv")
corr.table <- read.csv("./Misc/corr_table.csv")

#User upload own data sample
sample.data <- read.csv("./Sample User Data/Brent oil price data.csv")


########################################################

#Calculate the changepoint dates for each indexes
Brent.Changepoint <- (cpt.meanvar(Brent$log_return, 
                                  method = "PELT", 
                                  test.stat = "Normal"))

  B.Date <-Brent$Date[cpts(Brent.Changepoint)]

SP500.Changepoint <- (cpt.meanvar(sp500$log_return,
                                 method = "PELT", 
                                 test.stat = "Normal"))

  sp.Date <- sp500$Date[cpts(SP500.Changepoint)]

gold.Changepoint <- cpt.meanvar(gold$log_return, 
                                method = "PELT", 
                                test.stat = "Normal")
  g.Date <- gold$Date[cpts(gold.Changepoint)]

vix.Changepoint <- cpt.meanvar(vix$log_return, 
                               method = "PELT", 
                               test.stat = "Normal")

  v.Date <- vix$Date[cpts(vix.Changepoint)]

usdeur.Changepoint <- cpt.meanvar(usdeur$log_return, 
                                  method = "PELT", 
                                  test.stat = "Normal")

  u.Date <- usdeur$Date[cpts(usdeur.Changepoint)] 

########################################################

#Create the plots
Brent.Plot1 <- ggplot(Brent, aes(as.Date(Date),log_return))+ 
  geom_line()+
  geom_vline(xintercept = as.Date(B.Date), color = "red", linetype = "dotdash", size = 1)+
  ylab("log returns")+
  ggtitle("Brent Crude Oil Changepoints Analysis")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

Brent.Plot2 <- ggplot(Brent, aes(as.Date(Date), Price))+  geom_line()+
  geom_vline(xintercept = as.Date(B.Date), color = "red", linetype = "dotdash", size = 1)+
  xlab("Date")+ 
  ylab("Price")

sp500.Plot1 <- ggplot(sp500, aes(as.Date(Date),log_return))+ 
  geom_line()+
  geom_vline(xintercept = as.Date(sp.Date), color = "Blue", linetype = "dotdash", size = 1)+
  ylab("log returns")+
  ggtitle("S&P 500 Changepoint Detection")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

sp500.Plot2 <- ggplot(sp500, aes(as.Date(Date), Price))+  geom_line()+
  geom_vline(xintercept = as.Date(sp.Date), color = "Blue", linetype = "dotdash", size = 1)+
  xlab("Date")+ 
  ylab("Price")

gold.Plot1 <- ggplot(gold, aes(as.Date(Date),log_return))+ 
  geom_line()+
  geom_vline(xintercept = as.Date(g.Date), color = "Yellow", linetype = "dotdash", size = 1)+
  ylab("log returns")+
  ggtitle("Gold Changepoint Detection")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

gold.Plot2 <- ggplot(gold, aes(as.Date(Date), Price))+  geom_line()+
  geom_vline(xintercept = as.Date(g.Date), color = "Yellow", linetype = "dotdash", size = 1)+
  xlab("Date")+ 
  ylab("Price")


vix.Plot1 <- ggplot(vix, aes(as.Date(Date),log_return))+ 
  geom_line()+
  geom_vline(xintercept = as.Date(v.Date), color = "Green", linetype = "dotdash")+
  ylab("log returns")+
  ggtitle("VIX Changepoint Detection")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

vix.Plot2 <- ggplot(vix, aes(as.Date(Date), Price))+  geom_line()+
  geom_vline(xintercept = as.Date(v.Date), color = "Green", linetype = "dotdash", size = 1)+
  xlab("Date")+ 
  ylab("Price")

usdeur.Plot1 <- ggplot(usdeur, aes(as.Date(Date),log_return))+ 
  geom_line()+
  geom_vline(xintercept = as.Date(u.Date), color = "purple", linetype = "dotdash", size = 1)+
  ylab("log returns")+
  ggtitle("USD EUR RR Changepoint Detection")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

usdeur.Plot2 <- ggplot(usdeur, aes(as.Date(Date), Price))+  geom_line()+
  geom_vline(xintercept = as.Date(u.Date), color = "purple", linetype = "dotdash", size = 1)+
  xlab("Date")+ 
  ylab("Price")


###########################################################

#Create the plots for observation financial criss
Brent.fc <- ggplot(Brent, aes(as.Date(Date), Price), color ="red")+
            geom_vline(xintercept = as.Date(B.Date), color = "red", linetype = "dotdash", size = 1)+
            geom_line()+
            geom_rect(aes(xmin=as.Date("2008/08/02"), xmax =as.Date("2009/08/02"), ymin = 0, ymax =Inf), alpha = 0.01)+
            ylab("Brent")+
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank())

sp500.fc <- ggplot(sp500, aes(as.Date(Date), Price), color = "blue")+
            geom_line()+
            geom_vline(xintercept = as.Date(sp.Date), color = "Blue", linetype = "dotdash", size = 1)+
            geom_rect(aes(xmin=as.Date("2008/08/02"), xmax =as.Date("2009/08/02"), ymin = 0, ymax =Inf), alpha = 0.01)+          
            ylab("S&P 500")+
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank())

gold.fc <- ggplot(gold, aes(as.Date(Date), Price), color = "yellow")+
           geom_line()+
           geom_vline(xintercept = as.Date(g.Date), color = "Yellow", linetype = "dotdash", size = 1)+
           ylab("Gold")+
  geom_rect(aes(xmin=as.Date("2008/08/02"), xmax =as.Date("2009/08/02"), ymin = 0, ymax =Inf), alpha = 0.01)+         
  theme(axis.title.x=element_blank(),
                 axis.text.x=element_blank(),
                 axis.ticks.x=element_blank())

vix.fc <- ggplot(vix, aes(as.Date(Date), Price), color = "green")+
          geom_line()+
          ylab("VIX")+
  geom_rect(aes(xmin=as.Date("2008/08/02"), xmax =as.Date("2009/08/02"), ymin = 0, ymax =Inf), alpha = 0.01)+
          geom_vline(xintercept = as.Date(v.Date), color = "Green", linetype = "dotdash", size = 1)+
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank())

usdeur.fc <- ggplot(usdeur, aes(as.Date(Date),Price), color = "purple")+
             geom_vline(xintercept = as.Date(u.Date), color = "purple", linetype = "dotdash", size = 1)+
  geom_rect(aes(xmin=as.Date("2008/08/02"), xmax =as.Date("2009/08/02"), ymin = 0, ymax =Inf), alpha = 0.01)+
             geom_line()+
             ylab("USD EUR")+
             xlab("Date")

#Create plots for overlapping of Brent and Gold

Brent.Plot3 <- ggplot(Brent, aes(as.Date(Date), Price))+  geom_line()+
  geom_vline(xintercept = as.Date(B.Date), color = "red", linetype = "dotdash", size = 1)+
  ylab("Brent")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

gold.Plot3 <- ggplot(gold, aes(as.Date(Date), Price))+  geom_line()+
  geom_vline(xintercept = as.Date(g.Date), color = "Yellow", linetype = "dotdash", size = 1)+
  xlab("Date")+ 
  ylab("Gold")

#Create plot for donald trump election

Brent.dt <- ggplot(Brent, aes(as.Date(Date), Price))+  geom_line()+
  geom_vline(xintercept = as.Date(B.Date), color = "red", linetype = "dotdash", size = 1)+
  geom_rect(aes(xmin=as.Date("2011/11/20"), xmax =as.Date("2011/12/30"), ymin = 0, ymax =Inf), alpha = 0.01)+   
  ylab("Brent")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

gold.dt <- ggplot(gold, aes(as.Date(Date), Price))+  geom_line()+
  geom_vline(xintercept = as.Date(g.Date), color = "Yellow", linetype = "dotdash", size = 1)+
  geom_rect(aes(xmin=as.Date("2011/11/20"), xmax =as.Date("2011/12/30"), ymin = 0, ymax =Inf), alpha = 0.01)+    
  ylab("Gold")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

sp500.dt <- ggplot(sp500, aes(as.Date(Date), Price), color = "blue")+
  geom_line()+
  geom_vline(xintercept = as.Date(sp.Date), color = "Blue", linetype = "dotdash", size = 1)+
  geom_rect(aes(xmin=as.Date("2011/11/20"), xmax =as.Date("2011/12/30"), ymin = 0, ymax =Inf), alpha = 0.01)+           
  ylab("S&P 500")+
  xlab("Date")



