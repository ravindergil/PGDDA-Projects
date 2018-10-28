# Retail-Giant Sales Forecasting #
#--------------------------------#

#-------------------- Business and Problem understanding ----------------#
# "Global Mart" is an online store super giant having worldwide operations. 
# The store caters to 7 different markets and 3 major customer segments.
# Need to find out 2 most profitable (and consistent) segment from these 21.
# Forecast the sales and the demand for these segments for the next 6 months.
# The data currently has the transaction level data. 
# There are 24 attributes related to each such transaction.

library(dplyr)
library(lubridate)
library(ggplot2)
setwd("C:\\Users\\Desktop\\TimeSeries\\Group Case Study")

rm(list = ls())
store <- read.csv("Global Superstore.csv", stringsAsFactors = FALSE)
head(store)
str(store)

################################ Data preparation ###################################
# Check for NA and dupilicate values
sapply(store, function(x) sum(is.na(x)))
# Only  Postal.Code has 41296 NA values

sum(duplicated(store))
# No duplicate data found

# Convert Market and Segment to category
store$Market <- as.factor(store$Market)
store$Segment <- as.factor(store$Segment)

table(store$Market) # 7 marketts
table(store$Segment) # 3 segments
# 21 total categories

# Keeping only essential columns in new df store_new
store_new <- as.data.frame(store)
store_new <- store_new [,-c(1,2,4,5,6,7,9,10,11,12)]
str(store_new)
store_new <- store_new [,-c(4,5,6,7,8)]
colnames(store_new)
store_new <- store_new [,-c(6,8,9)]
str(store_new)

#Convert date to Year-Month for timeseries
store_new$Order.Date <- parse_date_time(x = store_new$Order.Date, 
                                        order = c("d m Y"), 
                                        locale = "eng")

store_new$Order.Date <- format(as.Date(store_new$Order.Date, "%d-%m-%Y"), "%Y-%m")
max(store_new$Order.Date) #2014-12
min(store_new$Order.Date) #2011-01
levels(as.factor(store_new$Order.Date))
# S0 12*4 = 48 data points available across each 21 total categories ()

# Aggregating the data w.r.t. Market, segment and Order.Date
categories_agg <- store_new %>%  group_by(Market, Segment, Order.Date) %>%
  summarise(sum_profit = sum(Profit,na.rm = T), 
            sum_quantity = sum(Quantity, na.rm= T),
            sum_sales =sum(Sales, na.rm= T)
  ) %>% 
  arrange(Market, Segment, Order.Date)
nrow(categories_agg)
count(categories_agg) 
#Canada does not have data for all 48 months


#Add timeseries element for each of the 21 categories
i=0
categories_agg$Month <-0
for (i in 1:nrow(categories_agg)){
  yr= as.numeric( substr(categories_agg$Order.Date[i],4,4)) 
  mon = as.numeric (substr (categories_agg$Order.Date[i],6,7))
  categories_agg$Month[i] <- ((yr-1) *12) +mon
}

# Calculate CV(Coefficient of variation) for Profit
categories_agg_profit <- store_new %>%  group_by(Market, Segment) %>%
  summarise(sum_profit = sum(Profit,na.rm = T), 
            sum_quantity = sum(Quantity, na.rm= T),
            sum_sales =sum(Sales, na.rm= T),
            cv = sd(Profit)/mean(Profit)) %>% arrange(Market, Segment)

categories_agg_profit <- categories_agg_profit[order(categories_agg_profit$sum_profit, decreasing = TRUE),]
View(categories_agg_profit)
ggplot(categories_agg_profit, aes(x = Market, y = sum_profit, fill = cv)) + geom_bar(stat = "identity")

# The 2 most profitable and consistently profitable segments
# APAC-Consumer - 4.2(Cv)
# EU - Consumer - 4.7(cv) 

# Time Series:
#1.APAC - Consumer - Sales --> Top1Sales
#2.APAC - Consumer - Quantity --> Top1Qyt
#3.EU - Consumer - Sales --> Top2Sale
#4.EU - Consumer - Quantity --> Top2Qyt

# Create timeseries for sales and quantity for top 2 categories (based on cv)
# Series 1 - EMEA Corporate - Sales
top_2_categories <- categories_agg %>% filter((Market == "APAC" & Segment == "Consumer") | 
                                                (Market == "EU" & Segment == "Consumer"))
#write.csv(top_2_categories, file = "RetailGiant_Top2.csv")

############################## Data Preparation is Complete here ###############################
################################################################################################
####################################### Model building #########################################
# Total 8 models will be created: 2 models each for the following time series
#1.APAC - Consumer - Sales
#2.APAC - Consumer - Quantity 
#3.EU - Consumer - Sales 
#4.EU - Consumer - Quantity 


#*****************************************************************************************************************
# APAC Consumer Sale Analysis --> Top1Sales
#******************************************************************************************************************
#******************************************************************************************************************
library(forecast)
library(tseries)
require(graphics)

labels <- c("Raw", "Smoothed")
ylab1 <- c("Sales", "Quantity")
xlab1 <- c("Months from Jan 2011")
title <- c("Top 1 (APAC) Sales Analysis", "Top 1 (APAC) Quantity Analysis", 
           "Top 2 (EU) Sales Analysis", "Top 2 (EU) Quantity Analysis")
           
Top1Sales <- top_2_categories %>% filter(Market == "APAC" , Segment == "Consumer")
Top1Sales <- Top1Sales[, c(6,7)]
Top1Sales$Sales <- Top1Sales$sum_sales
Top1Sales <- Top1Sales[,-1]

sum(is.na(Top1Sales)) #Missing value check 
summary(Top1Sales) #Missing value check

timeser.Top1Sale <- ts(Top1Sales[1:42,2])
plot(timeser.Top1Sale, main=title[1], xlab = xlab1, ylab = ylab1[1], col="black", lwd = 2) #Original ts

# Using Hot and Winters method for find optimum alpha value

cols <- c("red", "blue", "green", "black")
alphas <- c(0.02, 0.2, 0.8)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  smoothedseries <- HoltWinters(timeser.Top1Sale, alpha=alphas[i], beta=FALSE, gamma=FALSE)
  lines(fitted(smoothedseries)[,1], col=cols[i], lwd=2)
}
legend("topleft", labels, col=cols, lwd=2)

# alpha = 0.02 appears to be a flat line not capturing trend or seasonality
# alpha value must be any thing between 0.2, 0.8
# Picking value of alpha betwees 0.2 and 0.8
# Final alpha after trial and error
smoothedseries.HW <- HoltWinters(timeser.Top1Sale, alpha=0.3,  beta=FALSE, gamma=FALSE)
lines(fitted(smoothedseries.HW)[,1], col="yellow", lwd=2)

#Smoothing the series - Moving Average Smoothing
w <-1
smoothedseries <- stats::filter(timeser.Top1Sale, filter=rep(1/(w+1),(w+1)), method='convolution', sides=2)
lines(smoothedseries, col='orange', lwd=2) #smoothed ts Moving Average

#Smoothing left end of the time series
diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series
n <- length(timeser.Top1Sale)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

# Comparing results of Moving Average and Holts-Winter methods
plot(timeser.Top1Sale, xlab = xlab1, ylab = ylab1[1], col="black", lwd = 2,
     main = "Top1 (APAC) Sales Analysis - Smoothing Comparision") #Original ts
lines(smoothedseries, col='red', lwd=2) #smoothed time series MA
lines(fitted(smoothedseries.HW)[,1], col='blue', lwd=2) #smoothed ts Holt Winters
legend("topleft", c("Original", "Smoothed-MA", "Smoothed-HW"), col=c("black", "red" , "blue"), lwd=2)

#Between the Holts Winter and Moving Average, the latter shows better smoothing. The Holts Winter shows substantial lag
#So choosing the Moving Average smoothing

################################## Classical Decomposition ##################################
# Time Series Forecasting using classical decomposition


Top1Sale.timevals_in <- Top1Sales$Month[1:42]

smoothedseries.Top1Sale.DF <- as.data.frame(cbind(Top1Sale.timevals_in, smoothedseries))

colnames(smoothedseries.Top1Sale.DF) <- c('Month', 'Sales')

# Try linear regression and linear model with seasonality both
plot(timeser.Top1Sale, main = "Globally Predictable", xlab = xlab1, ylab = ylab1[1], col="black", lwd = 2) #Original ts
lmfit <- lm(Sales ~ Month, data = smoothedseries.Top1Sale.DF)

#lmfit <- lm(Profit ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
# + Month, data = smoothedseries.Top1Sale.DF)

globalpred.Top1Sale <- predict(lmfit, Month = Top1Sale.timevals_in)
lines(Top1Sale.timevals_in, globalpred.Top1Sale, col='blue', lwd=2)
legend("topleft", c("Regression Fit"), col=c("blue"), lwd=2)

#Remove global pred from smoothed ts to get local component of the time series
localpred.Top1Sale <- smoothedseries - globalpred.Top1Sale
plot(localpred.Top1Sale, col='green', type = "l", main = "Locally Predictable", lwd = 2, ylab = c("Sales"))


#Model using ARIMA
acf(localpred.Top1Sale) 
pacf(localpred.Top1Sale)
armafit <- auto.arima(localpred.Top1Sale)
tsdiag(armafit)
armafit #ARIMA (0,0,1)

armapred <- fitted(armafit) 

#Verify if residual ts is white noise
resi.Top1Sale <- localpred.Top1Sale - armapred # arma= local ; local =stationary
plot(resi.Top1Sale, lwd = 2, main = "Residual Series", col = "brown", ylab = "Sales")
adf.test(resi.Top1Sale,alternative = "stationary") 
# From Dickey-Fuller Test: p-value = 0.01847 < 0.05, so reject null hypothesis, ts is stationary
kpss.test(resi.Top1Sale) 
# From KPSS Test: p-value = 0.1 > 0.05 , cannot reject null hypothesis - ts is stationary

#Evaluate the model on outdata 
outdata.Top1Sale <- ts(Top1Sales[43:48,2])
Top1Sale.timevals_out <- c(43:48) #Top1Sales$Month

# Our final model is combination of Global and Auto regression models
# Total forecast

Top1Sales.GlobalPred <- predict(lmfit, data.frame(Month = Top1Sale.timevals_out))

Top1Sales.ARMAPred <- predict(armafit, n.ahead = 6)

Top1Sales.TotalPred <- NULL
for (i in 1:6) {
  Top1Sales.TotalPred[i] <- Top1Sales.GlobalPred[i] + Top1Sales.ARMAPred[[1]][i]
}

#COmpare predictions with forecasted values using MAPE
MAPE_Top1Sales <- accuracy(Top1Sales.TotalPred,outdata.Top1Sale)[5]
MAPE_Top1Sales #29.7 (LOCAL lm) ; #46.8 (LOCAL sin) 

#Comparing the MAPE values, we keep the Simple Linear Regression as the trend function

#Plot the (modelled + forecasted) and original ts for the 48 months
total.timeser.Top1Sales <- ts(Top1Sales$Sales)
Top1Sales.ModelledSeries <- c(ts(globalpred.Top1Sale  + armapred),ts(Top1Sales.TotalPred))
plot(total.timeser.Top1Sales, col = "black", lwd = 2, main = "Original vs Forecast", ylab = "Sales")
lines(Top1Sales.ModelledSeries, col = "red", lwd = 2)
legend("topleft", c("Original", "Forecasted"), col=c("black", "red"), lwd=2)

#Forecast for next 6 months
Top1Sales.timevals_next <- c(43:54)
Top1Sales.NextGlobal <- predict(lmfit,data.frame(Month = Top1Sales.timevals_next))
Top1Sales.NextARMA <- predict(armafit, n.ahead = 12)

i=0
Top1Sales.NextFcast <- NULL
for(i in 1:6){
  Top1Sales.NextFcast[i] <-  Top1Sales.NextGlobal[i+6] + Top1Sales.NextARMA[[1]][i+6]
}

total.timeser.Top1Sales <- ts(Top1Sales$Sales)
Top1Sales.Modelled.Fcast <- c(ts(globalpred.Top1Sale + armapred),ts(Top1Sales.TotalPred), ts(Top1Sales.NextFcast))
plot(total.timeser.Top1Sales, col = "black", lwd = 2, ylab = "Sales",
     main = "Forecast using Classical Decomposition")
lines(Top1Sales.Modelled.Fcast, col = "red", lwd = 2)
legend("topleft", c("Original", "Forecast"), col=c("black", "red", "blue"), lwd=2)
legend("bottomright", c("MAPE = 29.7"))

#**********************************************************************************
####################  Top 1 (APAC) Sales Forecast - ARIMA fit #####################

timeser.Top1Sale.ARIMA <- ts(Top1Sales[1:42,2])
outdata.Top1Sale.ARIMA <- ts(Top1Sales[1:48,2])

autoarima <- auto.arima(timeser.Top1Sale.ARIMA)
autoarima  #ARIMA(0,1,1)
tsdiag(autoarima)
plot(autoarima$x, col="black", lwd = 2, main = "Forecaste using ARIMA Modelling")
lines(fitted(autoarima), col="blue", lwd = 2)
legend("topleft", c("Original", "Forecast"), col=c("black", "blue"), lwd=2)
legend("bottomright", c("MAPE = 27.68"))

#Again, let's check if the residual series is white noise

resi.Top1Sale.ARIMA <- timeser.Top1Sale.ARIMA - fitted(autoarima)

adf.test(resi.Top1Sale.ARIMA,alternative = "stationary")
kpss.test(resi.Top1Sale.ARIMA)

#Also, let's evaluate the model using MAPE
fcast.auto.arima.Top1Sales <- predict(autoarima, n.ahead = 6)

MAPE.Top1Sale.ARIMA <- accuracy(fcast.auto.arima.Top1Sales$pred,outdata.Top1Sale.ARIMA)[5]
MAPE.Top1Sale.ARIMA #27.68

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred_Top1Sales <- c(fitted(autoarima),ts(fcast.auto.arima.Top1Sales$pred))
plot(total.timeser.Top1Sales, col = "black", lwd = 2, ylab = "Sales",
     main = "Original vs Classical Decomposition vs Auto ARIMA")
lines(auto_arima_pred_Top1Sales, col = "blue", lwd = 2) #Auto arima
lines(Top1Sales.Modelled.Fcast, col = "red", lwd = 2) #Classic

legend("topleft", c("Original", "Forecast - Classic Decomposition", "Forecast - Auto-ARIMA"),
       col=c("black", "red", "blue"), lwd=2)
legend("bottomright", c("MAPE = 29.7", "MAPE = 27.68"), col=c("red", "blue"), lwd=2)


##SUMMARY -- TOP1SALES##
#_________________________
#CLASSICAL DECOMPOSITION : MAPE = 29.7
#AUTO - ARIMA : MAPE = 27.68

#Auto Arima is simpler and gives better results, so we prefer this method

#*****************************************************************************************************************
# APAC Consumer Quantity Analysis --> Top1Qty
#******************************************************************************************************************

Top1Qty <- top_2_categories %>% filter(Market == "APAC" , Segment == "Consumer")
Top1Qty  <- Top1Qty[,c(5,7)]
Top1Qty$Quantity <- Top1Qty$sum_quantity
Top1Qty <- Top1Qty[,-1]

sum(is.na(Top1Qty)) #Missing value check 
summary(Top1Qty) #Range 134 - 932

####### Creating Time series for APAC - Consumer - Quantity
################################## Classical Decomposition ##################################
# Time Series Forecasting using classical decomposition


timeser.Top1Qty <- ts(Top1Qty[1:42,2])
plot(timeser.Top1Qty, main=title[2], xlab = xlab1, ylab = ylab1[2], col = "black", lwd = 2)  # Original Plot

# Smoothing the time series:  
# First with Moving Average method then with Holt Winter Method for exponential smoothing

w <- 1
smoothedseries <- stats::filter(timeser.Top1Qty, filter = rep(1/(1*w+1),(1*w+1)), method = "convolution", sides = 2)
lines(smoothedseries, col = "red", lwd = 2)

#Smoothing left end of the time series
diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series
n <- length(timeser.Top1Qty)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

# Plotting the smoothed value
lines(smoothedseries, col="brown", lwd=2)

# Using Hot and Winters method for find optimum alpha value
plot(timeser.Top1Qty, main=title[2], xlab = xlab1, ylab = ylab1[2], col = "black", lwd = 2)  # Original Plot
alphas <- c(0.02,0.1,0.8)
cols <- c("red", "blue", "green", "black", "brown")
lables <- c(paste("alpha = ", alphas), "Original", "MA Series")

for (i in seq(1, length(alphas))){
  smoothedseries.HW <- HoltWinters(timeser.Top1Qty, alpha = alphas[i], beta = FALSE, gamma = FALSE)
  lines(fitted(smoothedseries.HW)[,1], col = cols[i], lwd = 2)
}
legend("topleft", labels, col=cols, lwd=2)

# Picking value of alpha betwees 0.2 and 0.8 
smoothedseries.HW1 <- HoltWinters(timeser.Top1Qty, alpha = 0.5, beta = FALSE, gamma = FALSE)
lines(fitted(smoothedseries.HW1)[,1], col = "yellow", lwd = 2)

# Comparing result of Moving Average and Holts-Winter methods
plot(timeser.Top1Qty, xlab = xlab1, ylab = ylab1[2], col = "black", lwd = 2,
     main = "Top1 (APAC) Quantity Analysis - Smoothing Comparision") # Original Plot
lines(smoothedseries, col="blue", lwd=2)
lines(fitted(smoothedseries.HW1)[,1], col = "red", lwd = 2)
legend("topleft", c("Original", "Smoothed-MA", "Smoothed-HW"), col=c("black", "blue", "red"), lwd=2)

# Moving Average methods fits better as compared to holt-winters method 
# Building a model on the smoothed time series using classical decomposition
# First, let's convert the time series to a dataframe


Top1Qty.timevals_in <- Top1Qty$Month[1:42]
smoothedseries.DF <- as.data.frame(cbind(Top1Qty.timevals_in, smoothedseries))
colnames(smoothedseries.DF) <- c("Month", "Quantity")

#Try linear regression and linear model with seasonality both
plot(timeser.Top1Qty, main = "Globally Predictable", xlab = xlab1, ylab = ylab1[2], col="black", lwd = 2) #Original ts
lmfit <- lm(Quantity ~ Month, data = smoothedseries.DF)

#lmfit <- lm(Profit ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
# + Month, data=smootheddf)

globalpred.Top1Qty <- predict(lmfit, Month = Top1Qty.timevals_in)
lines(Top1Qty.timevals_in, globalpred.Top1Qty, col='blue', lwd=2)
legend("topleft", c("Original Curve","Regression Fit"), col=c("black","blue"), lwd=2)

#Remove global pred from smoothed ts to get local component of the time series
localpred.Top1Qty <- smoothedseries - globalpred.Top1Qty
plot(localpred.Top1Qty, col='green', type = "l", ylab = ylab1[2], main = "Locally Predictable", lwd = 2)

#Model using ARIMA
acf(localpred.Top1Qty) 
pacf(localpred.Top1Qty)
armafit <- auto.arima(localpred.Top1Qty)
tsdiag(armafit)
armafit #ARIMA (2,0,1)

armapred <- fitted(armafit) 

#Verify if residual ts is white noise
resi.Top1Qty <- localpred.Top1Qty - armapred  # arma = local ; local = stationary
plot(resi.Top1Qty, lwd = 2, main = "Residual Series", col = "brown", ylab = ylab1[2])
adf.test(resi.Top1Qty,alternative = "stationary")
# For Dickey-Fuller : p-value = 0.03144 < 0.05
kpss.test(resi.Top1Qty) 
# For KPSS Test : p-value = 0.1 > 0.05 , cannot reject null hypothesis - ts is stationary


#Evaluate the model on outdata 
outdata.Top1Qty <- ts(Top1Qty[43:48,2])
Top1Qty.timevals_out <- c(43:48) #Top1Sales$Month

# Our final model is combination of Global and Auto regression models
# Total forecast

Top1Qty.GlobalPred <- predict(lmfit,data.frame(Month = Top1Qty.timevals_out))

Top1Qty.ARMAPred <- predict(armafit, n.ahead = 6)

Top1Qty.TotalPred <- NULL
for (i in 1:6) {
  Top1Qty.TotalPred[i] <- Top1Qty.GlobalPred[i] + Top1Qty.ARMAPred[[1]][i]
}

#COmpare predictions with forecasted values using MAPE
MAPE_Top1Qty <- accuracy(Top1Qty.TotalPred, outdata.Top1Qty)[5]
MAPE_Top1Qty  #36.9 (LOCAL lm)

#Plot the (modelled + forecasted) and original ts for the 48 months
total.timeser.Top1Qty <- ts(Top1Qty$Quantity)
Top1Qty.ModelledSeries <- c(ts(globalpred.Top1Qty + armapred),ts(Top1Qty.TotalPred))
plot(total.timeser.Top1Qty, col = "black", lwd = 2, ylab = ylab1[2])
lines(Top1Qty.ModelledSeries, col = "red", lwd = 2)
legend("topleft", c("Original", "Forecasted"), col=c("black", "red"), lwd=2)
legend("bottomright", c("MAPE = 36.91"))

#Forecast for next 6 months
Top1Qty.timevals_next <- c(43:54)
Top1Qty.NextGlobal <- predict(lmfit,data.frame(Month =Top1Qty.timevals_next))
Top1Qty.NextARMA <- predict(armafit, n.ahead = 12)

i=0
Top1Qty.NextFcast <- NULL
for(i in 1:6){
  Top1Qty.NextFcast[i] <-  Top1Qty.NextGlobal[i+6] + Top1Qty.NextARMA[[1]][i+6]
}

total.timeser.Top1Qty  <- ts(Top1Qty$Quantity)
Top1Qty.Modelled.Fcast <- c(ts(globalpred.Top1Qty + armapred),ts(Top1Qty.TotalPred), ts(Top1Qty.NextFcast))
plot(total.timeser.Top1Qty, col = "black", lwd = 2, ylab = ylab1[2], main = "Forecast using Classical Decomposition")
lines(Top1Qty.Modelled.Fcast, col = "red", lwd = 2)
legend("topleft", c("Original", "Forecasted"), col=c("black", "red"), lwd=2)
legend("bottomright", c("MAPE = 36.91"))


#**********************************************************************************
#######################  TOP 1 Quantity Forecast - ARIMA fit ######################

timeser.Top1Qty.ARIMA <- ts(Top1Qty[1:42,2])
outdata.Top1Qty.ARIMA <- ts(Top1Qty[1:48,2])

autoarima <- auto.arima(timeser.Top1Qty.ARIMA)
autoarima  #ARIMA(0,1,0)
tsdiag(autoarima)
plot(autoarima$x, col="black", lwd = 2, main = "Forecast using ARIMA Model")
lines(fitted(autoarima), col="blue", lwd = 2)
legend("topleft", c("Original", "Forecasted - ARIMA"), col=c("black", "blue"), lwd=2)
legend("bottomright", c("MAPE = 26.24"))

#Again, let's check if the residual series is white noise

resi.Top1Qty.ARIMA <- timeser.Top1Qty.ARIMA - fitted(autoarima)

adf.test(resi.Top1Qty.ARIMA ,alternative = "stationary")
kpss.test(resi.Top1Qty.ARIMA )

#Also, let's evaluate the model using MAPE
fcast.auto.arima.Top1Qty <- predict(autoarima, n.ahead = 6)

MAPE.Top1Qty.ARIMA <- accuracy(fcast.auto.arima.Top1Qty$pred,outdata.Top1Qty.ARIMA)[5]
MAPE.Top1Qty.ARIMA #26.24

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred_Top1Qty <- c(fitted(autoarima),ts(fcast.auto.arima.Top1Qty$pred))
plot(total.timeser.Top1Qty, col = "black", lwd = 2, ylab = ylab1[2],
     main = "Original vs Classical Decomposition vs Auto ARIMA")
lines(auto_arima_pred_Top1Qty, col = "blue", lwd = 2) #Auto arima
lines(Top1Qty.Modelled.Fcast, col = "red", lwd = 2) #Classic

legend("topleft", c("Original", "Forecast - Classic Decomposition", "Forecast - Auto-ARIMA"),
       col=c("black", "red", "blue"), lwd=2)
legend("bottomright", c("MAPE = 26.24", "MAPE = 36.91"), col=c("blue", "red"), lwd=2)

fcast_auto_arima_Top2Qty_Next <- predict(autoarima, n.ahead = 12)
#We take forecast from n.ahead 7:12 as that
#for months 49:54

##SUMMARY -- TOP1QTY##
#_________________________
#CLASSICAL DECOMPOSITION : MAPE = 36.9
#AUTO - ARIMA : MAPE = 26.24

#Auto Arima is simpler and gives better results, so we prefer this method

#*****************************************************************************************************************
# EU Consumer Sales Analysis --> Top2Sales
#******************************************************************************************************************

Top2Sales <- top_2_categories %>% filter(Market == "EU" , Segment == "Consumer")
Top2Sales <- Top2Sales[, c(6,7)]
Top2Sales$Sales <- Top2Sales$sum_sales
Top2Sales <- Top2Sales[,-1]

sum(is.na(Top2Sales)) #Missing value check 
summary(Top2Sales) #Missing value check

timeser.Top2Sales <- ts(Top2Sales[1:42,2])

plot(timeser.Top2Sales, main=title[3], xlab = xlab1, ylab = ylab1[1], col = "black", lwd = 2)  # Original Plot

# Smoothing the time series:  
# First with Moving Average method then with Holt Winter Method for exponential smoothing
################################## Classical Decomposition ##################################
# Time Series Forecasting using classical decomposition


# holt winters smoothing
cols <- c("red", "blue", "green", "black")
alphas <- c(0.02, 0.2, 0.8)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  smoothedseries <- HoltWinters(timeser.Top2Sales, alpha=alphas[i], beta=FALSE, gamma=FALSE)
  lines(fitted(smoothedseries)[,1], col=cols[i], lwd=2)
}
legend("topleft", labels, col=cols, lwd=2)

# alpha = 0.002 appears to be flat
# alpha value must be any thing between 0.2, 0.8
# Picking value of alpha betwees 0.2 and 0.8
smoothedseries.HW <- HoltWinters(timeser.Top2Sales, alpha=0.5, beta=FALSE, gamma=FALSE) 
lines(fitted(smoothedseries.HW)[,1], col="yellow", lwd=2)                              

#Smoothing the series - Moving Average Smoothing
plot(timeser.Top2Sales, main=title[3], xlab = xlab1, ylab = ylab1[1], col = "black", lwd = 2)  # Original Plot
w <- 2
smoothedseries <- stats::filter(timeser.Top2Sales, filter=rep(1/(1*w+1),(1*w+1)), method='convolution', sides=2)
lines(smoothedseries, col="red", lwd=2)

#Smoothing left end of the time series
diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series
n <- length(timeser.Top2Sales)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

# Plotting the smoothed value
lines(smoothedseries, col = "brown", lwd = 2)

# Comparing result of Moving Average and Holts-Winter methods
plot(timeser.Top2Sales, xlab = xlab1, ylab = ylab1[1], col = "black", lwd = 2,
     main = "Top 2 (EU) Sales Analysis - Smoothing Comparision")  # Original Plot
lines(smoothedseries, col="blue", lwd=2)
lines(fitted(smoothedseries.HW)[,1], col = "red", lwd = 2)
legend("topleft", c("Original", "Smoothed-MA", "Smoothed-HW"), col=c("black", "blue" , "red"), lwd=2)

# Moving Average methods looks better as compared to holt-winters method at alpha = 0.5
# Between the Holts Winter and Moving Average, the latter shows better smoothing. The Holts Winter shows substantial lag
# So choosing the Moving Average smoothing
# Building a model on the smoothed time series using classical decomposition
# First, let's convert the time series to a dataframe

Top2Sales.timevals_in <- Top2Sales$Month[1:42]
smoothedseries.DF <- as.data.frame(cbind(Top2Sales.timevals_in, smoothedseries))
colnames(smoothedseries.DF) <- c("Month", "Sales")

#Try linear regression and linear model with seasonality both
plot(timeser.Top2Sales, xlab = xlab1, ylab = ylab1[1], col = "black", lwd = 2,
     main = "Globally Predicatable")  # Original Plot
lmfit <- lm(Sales ~ Month, data = smoothedseries.DF)

#lmfit <- lm(Profit ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
# + Month, data=smootheddf)

globalpred.Top2Sales <- predict(lmfit, Month = timevals_in)

lines(Top2Sales.timevals_in, globalpred.Top2Sales, col='blue', lwd=2)
legend("topleft", c("Original Curve" ,"Regression Fit"), col = c("black", "blue"), lwd = 2)

#Remove global pred from smoothed ts to get local component of the time series
localpred.Top2Sales <- smoothedseries - globalpred.Top2Sales
plot(localpred.Top2Sales, ylab = "Sales", col='green', type = "l", lwd = 2, main = "Locally Predictable")

#Model using ARIMA
acf(localpred.Top2Sales) 
pacf(localpred.Top2Sales)
armafit <- auto.arima(localpred.Top2Sales)
tsdiag(armafit)
armafit #ARIMA (1,0,2)

armapred <- fitted(armafit) 

#Verify if residual ts is white noise
resi.Top2Sales <- localpred.Top2Sales - armapred  # arma = local ; local = stationary
plot(resi.Top2Sales, ylab = "Sales", main = "Residual Series", col = "brown", lwd = 2)
adf.test(resi.Top2Sales,alternative = "stationary")
# For Dickey-Fuller Test --> p-value = 0.02198 < 0.05 , cannot reject null hypothesis - ts is stationary
kpss.test(resi.Top2Sales) 
# For KPSS Test --> p-value = 0.1 , > 0.05 , cannot reject null hypothesis - ts is stationary
#Residue is indeed white noise

#Evaluate the model on outdata 
outdata.Top2Sales <- ts(Top2Sales[43:48,2])
Top2Sales.timevals_out <- Top2Sales$Month[43:48]

# Our final model is combination of Global and Auto regression models
# Total forecast

Top2Sales.GlobalPred <- predict(lmfit,data.frame(Month = Top2Sales.timevals_out))

Top2Sales.ARMAPred <- predict(armafit, n.ahead = 6)

Top2Sales.TotalPred <- NULL
for (i in 1:6) {
  Top2Sales.TotalPred[i] <- Top2Sales.GlobalPred[i] + Top2Sales.ARMAPred[[1]][i]
}

# Compare predictions with forecasted values using MAPE
MAPE_Top2Sales <- accuracy(Top2Sales.TotalPred,outdata.Top2Sales)[5]
MAPE_Top2Sales  #27.42 (Local lm)

# Plot the (modelled+forecasted) and original ts for the 48 months
total.timeser.Top2Sales <- ts(Top2Sales$Sales)
Top2Sales.ModelledSeries <- c(ts(globalpred.Top2Sales + armapred),ts(Top2Sales.TotalPred))
plot(total.timeser.Top2Sales, col = "black", lwd = 2)
lines(Top2Sales.ModelledSeries, col = "red", lwd = 2)
legend("topleft", c("Original", "Forecasted"), col=c("black", "red"), lwd=2)

# Forecast for next 6 months
Top2Sales.timevals_next <- c(43:54)
Top2Sales.NextGlobal <- predict(lmfit,data.frame(Month = Top2Sales.timevals_next))
Top2Sales.NextARMA <- predict(armafit, n.ahead = 12)

i=0
Top2Sales.NextFcast <- NULL
for(i in 1:6){
  Top2Sales.NextFcast[i] <- Top2Sales.NextGlobal[i+6] + Top2Sales.NextARMA[[1]][i+6]
}

Top2Sales.Modelled.Fcast <- c(ts(globalpred.Top2Sales + armapred),ts(Top2Sales.TotalPred), ts(Top2Sales.NextFcast))
plot(total.timeser.Top2Sales, col = "black", lwd = 2, ylab = "Sales",
     main = "Forecast using Classical Decomposition")
lines(Top2Sales.Modelled.Fcast, col = "red", lwd = 2)
legend("topleft", c("Original", "Forecast"), col=c("black", "red"), lwd=2)
legend("bottomright", c("MAPE = 27.42"))




#**********************************************************************************
#######################  EU Sales Forecast - ARIMA fit ######################

timeser.Top2Sales.ARIMA <- ts(Top2Sales[1:42,2])
outdata.Top2Sales.ARIMA <- ts(Top2Sales[1:48,2])

autoarima <- auto.arima(timeser.Top2Sales.ARIMA)
autoarima  #ARIMA(2,1,0)
tsdiag(autoarima)
plot(autoarima$x, col="black", ylab = "Sales", main = "Forecast using ARIMA Modelling", lwd = 2)
lines(fitted(autoarima), col="blue", lwd = 2)
legend("topleft", c("Original", "Forecast"), col=c("black", "blue"), lwd=2)
legend("bottomright", c("MAPE = 28.9"))

#Again, let's check if the residual series is white noise
resi.Top2Sales.ARIMA <- timeser.Top2Sales - fitted(autoarima)

resi.Top2Sales.ARIMA <- as.vector(resi.Top2Sales.ARIMA)

adf.test(resi.Top2Sales.ARIMA, alternative = "stationary")

kpss.test(resi.Top2Sales.ARIMA)

#Also, let's evaluate the model using MAPE
fcast.auto.arima.Top2Sales <- predict(autoarima, n.ahead = 6)

MAPE.Top2Sales.ARIMA <- accuracy(fcast.auto.arima.Top2Sales$pred, outdata.Top2Sales.ARIMA)[5]
MAPE.Top2Sales.ARIMA #28.9

# Lastly, let's plot the predictions along with original values, to
# get a visual feel of the fit

auto_arima_pred_Top2Sales <- c(fitted(autoarima),ts(fcast.auto.arima.Top2Sales$pred))
plot(total.timeser.Top2Sales, col = "black", lwd = 2, ylab = "Sales",
     main = "Original vs Classical Decomposition vs Auto ARIMA")
lines(auto_arima_pred_Top2Sales, col = "blue", lwd = 2) #Auto arima
lines(Top2Sales.Modelled.Fcast, col = "red", lwd = 2) #Classic

legend("topleft", c("Original", "Forecast - Classic Decomposition", "Forecast - Auto-ARIMA"),
       col=c("black", "red", "blue"), lwd=2)
legend("bottomright", c("MAPE =  27.42", "MAPE = 28.92"), col=c("red", "blue"), lwd=2)

fcast_auto_arima_Top2Qty_Next <- predict(autoarima, n.ahead = 12)
fcast_auto_arima_Top2Qty_Next

# We take forecast from n.ahead 7:12 as that
# for months 49:54

## SUMMARY -- TOP1QTY##
# _________________________
# CLASSICAL DECOMPOSITION : MAPE = 27.42
# AUTO - ARIMA : MAPE = 28.9

# Auto Arima is simpler and gives better results, so we prefer this method

#*****************************************************************************************************************
# EU Consumer Quantity Analysis --> Top2Qty
#******************************************************************************************************************
#******************************************************************************************************************
Top2Qty <- top_2_categories %>% filter(Market == "EU" , Segment == "Consumer")
Top2Qty <- Top2Qty[, c(5,7)]
Top2Qty$Quantity <- Top2Qty$sum_quantity
Top2Qty <- Top2Qty[,-1]

sum(is.na(Top2Qty)) #Missing value check 
summary(Top2Qty) #Range 134 - 932

timeser.Top2Qty <- ts(Top2Qty[1:42,2])

plot(timeser.Top2Qty, main=title[4], xlab = xlab1, ylab = ylab1[2], col = "black", lwd = 2)  # Original Plot

# Smoothing the time series:  
# First with Moving Average method then with Holt Winter Method for exponential smoothing
################################## Classical Decomposition ##################################
# Time Series Forecasting using classical decomposition

cols <- c("red", "blue", "green", "black")
alphas <- c(0.1, 0.25, 0.5)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  smoothedseries <- HoltWinters(timeser.Top2Qty, alpha=alphas[i],beta=FALSE, gamma = FALSE)
  lines(fitted(smoothedseries)[,1], col=cols[i], lwd=2)
}
legend("topleft", labels, col=cols, lwd=2)
smoothedseries <- HoltWinters(timeser.Top2Qty, #alpha=alphas[i],beta=FALSE, 
                              gamma=FALSE)
smoothedseries

#Final alpha and beta (given as optimal values from the Holt Winters function)
smoothedseries.HW <- HoltWinters(timeser.Top2Qty, alpha=0.22,  beta=0.13, gamma=FALSE)
lines(fitted(smoothedseries)[,1], col="orange", lwd=2)

smoothseries <-  as.vector(fitted(smoothedseries)[,2])
s1 <- c(145, 145)
for (i in 1:40){
  s1[i+2] <- fitted(smoothedseries)[i,2]
}
smoothedseries <- s1

Top2Qty.timevals_in <- Top2Qty$Month[1:42]

# Comparing results of Moving Average and Holts-Winter methods
plot(timeser.Top2Qty, xlab = xlab1, ylab = ylab1[2], col="black", lwd = 2,
     main = "Top2 (EU) Quantity Analysis - Smoothing Comparision") #Original ts
lines(smoothedseries, col='blue', lwd=2) #smoothed time series MA
lines(fitted(smoothedseries.HW)[,1], col='red', lwd=2) #smoothed ts Holt Winters
legend("topleft", c("Original", "Smoothed-MA", "Smoothed-HW"), col=c("black", "blue" , "red"), lwd=2)

#Time Series Forecasting using classical decomposition
#Data shows only trend, no seasonality
#ts <- as.vector(fitted(smoothedseries)[,2])
#timeseriesdf <- as.data.frame(cbind(timevals_in, ts))
#colnames(timeseriesdf) <- c('Month', 'Top1Sales')


smoothedseries.DF <- as.data.frame(cbind(Top2Qty.timevals_in, smoothedseries))
colnames(smoothedseries.DF) <- c('Month', 'Quantity')

plot(timeser.Top2Qty, main = "Globally Predictable", xlab = xlab1, ylab = ylab1[2], col="black", lwd = 2) #Original ts
lmfit <- lm(Quantity ~ Month, data = smoothedseries.DF)

#Try linear regression and linear model with seasonality both
#lmfit <- lm(Profit ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
# + Month, data=smootheddf)
globalpred.Top2Qty <- predict(lmfit, Month = Top2Qty.timevals_in)
lines(Top2Qty.timevals_in, globalpred.Top2Qty, col='blue', lwd=2)
legend("topleft", c("Original Curve","Regression Fit"), col=c("black","blue"), lwd=2)

#Remove global pred from smoothed ts to get local component of the time series
localpred.Top2Qty <- smoothedseries - globalpred.Top2Qty
plot(localpred.Top2Qty , col='green', type = "l", main = "Locally Predictable", lwd = 2, ylab = ylab1[2])


#Model using ARIMA
acf(localpred.Top2Qty) 
pacf(localpred.Top2Qty)
armafit <- auto.arima(localpred.Top2Qty)

tsdiag(armafit)
armafit #ARIMA (1,0,0)

armapred <- fitted(armafit) 

#Verify if residual ts is white noise
resi.Top2Qty <- localpred.Top2Qty - armapred # arma= local ; local =stationary
plot(resi.Top2Qty, lwd = 2, main = "Residual Series", col = "brown", ylab = ylab1[2])
adf.test(resi.Top2Qty,alternative = "stationary")
kpss.test(resi.Top2Qty) #p value=0.1 , >0.05 , cannot reject null hypothesis - ts is stationary
#Residue is indeed white noise

#Evaluate the model on outdata 
outdata.Top2Qty <- ts(Top2Qty[43:48,2])
Top2Qty.timevals_out <- c(43:48) #Top2Sales$Month

# Our final model is combination of Global and Auto regression models
# Total forecast

Top2Qty.GlobalPred <- predict(lmfit,data.frame(Month = Top2Qty.timevals_out))

Top2Qty.ARMAPred <- predict(armafit, n.ahead = 6)

Top2Qty.TotalPred <- NULL
for (i in 1:6) {
  Top2Qty.TotalPred[i] <- Top2Qty.GlobalPred[i] + Top2Qty.ARMAPred[[1]][i]
}

#COmpare predictions with forecasted values using MAPE
MAPE_Top2Qty <- accuracy(Top2Qty.TotalPred, outdata.Top2Qty)[5]
MAPE_Top2Qty  #28.7 (LOCAL lm)

#Plot the (modelled+forecasted) and original ts for the 48 months
total.timeser.Top2Qty <- ts(Top2Qty$Quantity)
Top2Qty.ModelledSeries <- c(ts(globalpred.Top2Qty + armapred),ts(Top2Qty.TotalPred))
plot(total.timeser.Top2Qty, col = "black", lwd = 2)
lines(Top2Qty.ModelledSeries, col = "red", lwd = 2)
legend("topleft", c("Original", "Forecasted"), col=c("black", "red"), lwd=2)

#Forecast for next 6 months
Top2Qty.timevals_next <- c(43:54)
Top2Qty.NextGlobal <- predict(lmfit,data.frame(Month = Top2Qty.timevals_next))
Top2Qty.NextARMA <- predict(armafit, n.ahead = 12)

i=0
Top2Qty.NextFcast <- NULL
for(i in 1:6){
  Top2Qty.NextFcast[i] <-  Top2Qty.NextGlobal[i+6] + Top2Qty.NextARMA[[1]][i+6]
}

total.timeser.Top2Qty  <- ts(Top2Qty$Quantity)
Top2Qty.Modelled.Fcast <- c(ts(globalpred.Top2Qty + armapred),ts(Top2Qty.TotalPred), ts(Top2Qty.NextFcast))
plot(total.timeser.Top2Qty, col = "black", lwd = 2, ylab = "Sales",
     main = "Forecast using Classical Decomposition")
lines(Top2Qty.Modelled.Fcast, col = "red", lwd = 2)
legend("topleft", c("Original", "Forecasted"), col=c("black", "red"), lwd=2)
legend("bottomright", c("MAPE = 28.73"))


#**********************************************************************************
#######################  TOP 2 Quantity Forecast - ARIMA fit ######################

timeser.Top2Qty.ARIMA <- ts(Top2Qty[1:42,2])
outdata.Top2Qty.ARIMA <- ts(Top2Qty[1:48,2])

autoarima <- auto.arima(timeser.Top2Qty.ARIMA)
autoarima  #ARIMA(2,1,0)
tsdiag(autoarima)
plot(autoarima$x, col="black", lwd = 2, main = "Forecast using ARIMA Modelling")
lines(fitted(autoarima), col="blue", lwd = 2)
legend("topleft", c("Original", "Forecast"), col=c("black", "blue"), lwd=2)
legend("bottomright", c("MAPE = 30.13"))

#Again, let's check if the residual series is white noise

resi.Top2Qty.ARIMA <- timeser.Top2Qty - fitted(autoarima)

adf.test(resi.Top2Qty.ARIMA, alternative = "stationary")
kpss.test(resi.Top2Qty.ARIMA)

#Also, let's evaluate the model using MAPE
fcast.auto.arima.Top2Qty <- predict(autoarima, n.ahead = 6)

MAPE.Top2Qty.ARIMA <- accuracy(fcast.auto.arima.Top2Qty$pred,outdata.Top2Qty.ARIMA)[5]
MAPE.Top2Qty.ARIMA #30.13

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred_Top2Qty <- c(fitted(autoarima),ts(fcast.auto.arima.Top2Qty$pred))
plot(total.timeser.Top2Qty, col = "black", lwd = 2, ylab = "Sales",
     main = "Original vs Classical Decomposition vs Auto ARIMA")
lines(auto_arima_pred_Top2Qty, col = "blue", lwd = 2) #Auto arima
lines(Top2Qty.Modelled.Fcast, col = "red", lwd = 2) #Classic

legend("topleft", c("Original", "Forecast - Classic Decomposition", "Forecast - Auto ARIMA"),
       col=c("black", "blue", "red"), lwd=2)
legend("bottomright", c("MAPE =  28.73", "MAPE = 30.13"), col=c("red", "blue"), lwd=2)

fcast_auto_arima_Top2Qty_Next <- predict(autoarima, n.ahead = 12) #We take forecast from n.ahead 7:12 as that

#for months 49:54

##SUMMARY -- TOP2QTY##
#_________________________
#CLASSICAL DECOMPOSITION : MAPE = 28.72
#AUTO - ARIMA : MAPE = 30.13

#Classic Decomposition gives better results so we choose it.

########################################################################################################
########################################################################################################

# Conclusion :
#For APAC - Consumer:
#Both market segments i.e. Sales and Quantity, Auto ARIMA method seems to be a better choice to forecast future sales and demand, since it has low MAPE value.
#Whereas
#For EU - Consumer:
#Both market segments i.e. Sales and Quantity, Classical Decomposition comes across as a better choice to forecast future sales and demand, since it has low MAPE value.











