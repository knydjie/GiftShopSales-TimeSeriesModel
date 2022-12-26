# UAS Analisis Deret Waktu

# PENGEMBANGAN MODEL DERET WAKTU DENGAN TUJUAN MEMPREDIKSI (FORECASTING) 
# HASIL PENJUALAN BULANAN UNTUK TOKO SUVENIR 
# DI KOTA RESOR PANTAI DI QUEENSLAND, AUSTRALIA

# Kanaya Tabhita Djie (01112180013)

library(TSA)
library(ggplot2)
library(ggfortify)
library(fpp2)
library(sarima)
library(tseries)
library(forecast)

#### STEP 1: Import the Data ####
sales <- scan("https://robjhyndman.com/tsdldata/data/fancy.dat")
options(scipen = 5)
sales.ts <- ts(sales, start = 1987, frequency = 12)
class(sales.ts)

#### STEP 2: Data Exploration ####
summary(sales)
sales.ts

#### STEP 3: Trend analysis ####
# Plotting the time series data
win.graph(width = 4.875, height = 2.5, pointsize = 8)
autoplot(sales.ts) + 
  ggtitle("Hasil Penjualan Toko Suvenir") + 
  xlab("Year") + 
  ylab ("Sales")

# Check Seasonality
win.graph(width = 4.875, height = 2.5, pointsize = 8)
plot(sales.ts, ylab = "Sales", xlab = "Year", log = 'y')
points(y = sales.ts, x = time(sales.ts), pch = as.vector(season(sales.ts)))

# Plotting against the individual seasons
ggseasonplot(sales.ts, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Sales") 

ggsubseriesplot(sales.ts) +
  ylab("Sales") 

# Plotting Sales with lags
win.graph(width = 3, height = 3, pointsize = 8)
plot(y = sales.ts, x = zlag(sales.ts, d = 12), 
     ylab = 'Sales', xlab = 'Sales 12 years ago', log = 'y')

#### STEP 4: Model Specification ####

# Seasonal differencing
win.graph(width = 4.875, height = 2.5)
seas.diff <- diff(sales.ts, lag = 12)
autoplot(seas.diff)

# First difference
seas.non.diff <- diff(sales.ts)
autoplot(seas.non.diff)

# Stationary Test
adf.test(seas.non.diff)

# ACF and PACF
ggAcf(seas.non.diff, lag.max = 84) # tail off
ggPacf(seas.non.diff, lag.max = 84) # cut off

#### STEP 5: Parameter estimation ####

# First Model
(mod1 <- arima(seas.non.diff,order=c(2,1,0), seasonal=c(1,1,0), method = 'ML'))

# Second Model
(mod2 <- arima(seas.non.diff,order=c(2,1,0), seasonal=c(1,1,0), 
              fixed = c(NA, 0, NA), transform.pars = FALSE, method = 'ML'))

#### STEP 6: Model diagnostics ####
# Residual
ggtsdisplay(rstandard(mod1))
checkresiduals(mod1)

# Uji kenormalan
qqnorm(residuals(mod1))
qqline(residuals(mod1), col = 'red', lty = 3)
shapiro.test(residuals(mod1))

#### STEP 7: Forecasting ####
sales.ts %>%
  Arima(order=c(2,1,0), seasonal=c(1,1,0), lambda=0) %>%
  forecast() %>%
  autoplot() +
  ylab("Sales") + xlab("Year")
