Payment <- read_excel("~/Desktop/MA615/Final group project/Personal project/Payment - September 2018.xls")

library(benford.analysis) # loads package data(corporate.payment) # loads data


#The first code is to look at the high level test of reasonableness.
payment1 <- benford(Payment$Amount, number.of.digits = 1, sign = "positive", discrete = TRUE, round = 3)
#We can take a closer look at the first two digits, which is designed to select audit targets.
payment2 <- benford(Payment$Amount, number.of.digits = 2, sign = "positive", discrete = TRUE, round = 3)

plot(payment1)
plot(payment2)
suspects <- getSuspects(payment1, Payment)
suspects


GDP <- read_csv("~/Desktop/MA678/Midterm project/Data/GDP.csv")
GDP <- GDP[, c(-3, -4)]
GDP <- melt(GDP, id.vars = c("Country Code", "Country Name"), na.rm = TRUE, value.name = "GDP")
GDP$GDP <- as.numeric(GDP$GDP)

gdp_detect<-benford(GDP$GDP, number.of.digits = 2, sign = "positive", discrete = TRUE, round = 3)
plot(gdp_detect)
detect <- getSuspects(gdp_detect,GDP)
detect
