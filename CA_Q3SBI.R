library(quantmod)
library(forecast)
library(xts)
library(pdfetch)

############### fetching the data of SBI stock market from the yahoo finance website ######### 
SBI_DATA <- pdfetch_YAHOO("SBI")

# we can also use below code if we want to run machine learing on specific time period of data 
#SBI_DATA <- pdfetch_YAHOO("SBI",from = '2019-01-01', to = '2019-12-22') 
#it will run analysis on data from date 2019-01-01 to 2019-12-22
head(SBI_DATA)

# in order to use close price as a time series variable we select 4th coloum
Stock_price <- SBI_DATA[,4] #<- specifing 4th coloum

## grapgical visualization
plot(Stock_price,type='l', main='SBI log returns plot')


Stock = diff(log(Stock_price))
plot(Stock,type='l', main='SBI log returns plot')

Stock = Stock[!is.na(Stock)]

######## applying AR and MA
acf( Stock , lag.max=20)  
pacf( Stock , lag.max=20) 

## manual fitting of arima 
manual.fit<-arima(Stock, c(1,1,2))  # fitted model
manual.fit

# prediction for 10 step ahead
manual.fcast <- forecast(manual.fit, h=10) 
manual.fcast
plot(manual.fcast)
opt.fit<-arima(Stock, c(4,1,2))  # fitted model


# prediction for 10 step ahead
opt.fcast <- forecast(opt.fit, h=10) 
plot(opt.fcast)
