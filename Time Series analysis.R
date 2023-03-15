getwd()
setwd("C:/Users/Bilal/OneDrive/Desktop/Study folder/Data Analytics/Assignment 4") 

library(tidyverse)
library(openxlsx)
library(tidyquant)
library(quantmod)
library(vars)
library(xts)
library(zoo)
library(tseries)
library(Quandl)
library(lubridate)
library(AER)
library(sandwich)
library(lmtest)
library(stargazer)
library(ggplot2)

data <- read.xlsx("FRED.xlsx")


data$GOLDPM%>% na.omit()%>% adf.test(k=0)
data$EURUSD%>% na.omit()%>% adf.test(k=0)
data$VIX%>% na.omit()%>% adf.test(k=0)

#delta-Transformation

TGold <- Delt(data$GOLDPM)
TEurUSD <- Delt(data$EURUSD)

TGold %>% na.omit() %>% adf.test (k=0)
TEurUSD %>% na.omit() %>% adf.test(k=0)

#Regression-model

model1 <- TGold ~ TEurUSD + VIX
lmmodel1 <- lm(model1, data = data)

    ###Test for Autocorrelation and Heteroscadasticity###

coeftest(lmmodel1, vcov= vcovHAC(lmmodel1, type = "HC1")) %>% stargazer(data=lmmodel1, type="html", out = "CT.html")

###Plotting the Graph###

data %>% ggplot(aes(x=data$DATE, y=data$GOLDPM)) + geom_line (color="green")


data2 <- read.xlsx("Ratings.xlsx" , sheet = 2)

###Stationary test###

data2$AAA%>% na.omit()%>% adf.test(k=0)
data2$BBB%>% na.omit()%>% adf.test(k=0)
data2$VIX%>% na.omit()%>% adf.test(k=0)
data2$STOCK_RET%>% na.omit()%>% adf.test(k=0)
data2$UST1Y%>% na.omit()%>% adf.test(k=0)

#Delta2 Transformation

DeltaBBB<- Delt(data2$BBB)
deltaUST1Y <- Delt(data2$UST1Y)

DeltaBBB %>% na.omit() %>% adf.test (k=0)
deltaUST1Y %>% na.omit() %>% adf.test(k=0)

model2a <- AAA ~ VIX + STOCK_RET + UST1Y
lmmodel2a <- lm(model2, data = data2)

model2b <- BBB ~ VIX + STOCK_RET + UST1Y
lmmodel2b <- lm(model2, data = data2)

###Heteroscadasity test###

Test_1 <- coeftest(lmmodel2a, vcov= vcovHAC(lmmodel2a, type = "HC1")) 

Test_2 <- coeftest(lmmodel2b, vcov= vcovHAC(lmmodel2b, type = "HC1"))

stargazer(Test_1,Test_2, lmmodel2a,lmmodel2b,  type="html", out = "CT.html")








