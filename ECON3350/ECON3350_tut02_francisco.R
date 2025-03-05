# Tutorial 2: Forecasting Univariate Processes - I

# Answers by Eric Eisenstat
# https://sites.google.com/view/ericeisenstat

# Extra material by Francisco Tavares Garcia
# https://tavaresgarcia.github.io/


# House cleaning
rm(list = ls()) # clear Environment
if (!is.null(dev.list())) dev.off() # clear Plots
cat("\014") # clear Console

# Set working directory. To use the following line, save this script in the same
# folder as the data files.
install.packages("rstudioapi")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load the data in arma.csv
mydata <- read.delim("arma.csv", header = TRUE,  sep = ",")

# Use a for loop to generate ACFs and PACFs for all 8 time series
for (i in 1:8){
  acf(mydata[1 + i], main = colnames(mydata[1 + i]))
  pacf(mydata[1 + i], main = colnames(mydata[1 + i]))
}


### DGPs ###
t <- mydata$t

#### DGP 1 ####
DGP <- mydata$DGP1
# DGP # print
plot(t, DGP, type = "l", main = "DGP1") # plot
mean(DGP) # mean or expected value
var(DGP) # variance
acf(DGP) # ACF
pacf(DGP) # PACF

arima(DGP) # arima
arima(DGP, order = c(1,0,0)) # AR(1)
arima(DGP, order = c(1,0,0), fixed = c(0.75, NA)) # fixed coefficients


#### DGP 2 ####
DGP <- mydata$DGP2
# DGP # print
plot(t, DGP, type = "l", main = "DGP2") # plot
mean(DGP) # mean or expected value
var(DGP) # variance
acf(DGP) # ACF
pacf(DGP) # PACF

arima(DGP) # arima
arima(DGP, order = c(1,0,0)) # AR(1)
arima(DGP, order = c(1,0,0), fixed = c(-0.75, NA)) # fixed coefficients


#### DGP 3 ####
DGP <- mydata$DGP3
# DGP # print
plot(t, DGP, type = "l", main = "DGP3") # plot
mean(DGP) # mean or expected value
var(DGP) # variance
acf(DGP) # ACF
pacf(DGP) # PACF

arima(DGP) # arima
arima(DGP, order = c(1,0,0)) # AR(1)
arima(DGP, order = c(1,0,0), fixed = c(0.95, NA)) # fixed coefficients


#### DGP 4 ####
DGP <- mydata$DGP4
# DGP # print
plot(t, DGP, type = "l", main = "DGP4") # plot
mean(DGP) # mean or expected value
var(DGP) # variance
acf(DGP) # ACF
pacf(DGP) # PACF

arima(DGP) # arima
arima(DGP, order = c(2,0,0)) # AR(2)
arima(DGP, order = c(2,0,0), fixed = c(0.5, 0.25, NA)) # fixed coefficients


#### DGP 5 ####
DGP <- mydata$DGP5
# DGP # print
plot(t, DGP, type = "l", main = "DGP5") # plot
mean(DGP) # mean or expected value
var(DGP) # variance
acf(DGP) # ACF
pacf(DGP) # PACF

arima(DGP) # arima
arima(DGP, order = c(2,0,0)) # AR(2)
arima(DGP, order = c(2,0,0), fixed = c(0.25, -0.5, NA)) # fixed coefficients


#### DGP 6 ####
DGP <- mydata$DGP6
# DGP # print
plot(t, DGP, type = "l", main = "DGP6") # plot
mean(DGP) # mean or expected value
var(DGP) # variance
acf(DGP) # ACF
pacf(DGP) # PACF

arima(DGP) # arima
arima(DGP, order = c(0,0,1)) # MA(1)
arima(DGP, order = c(0,0,1), fixed = c(0.75, NA)) # fixed coefficients


#### DGP 7 ####
DGP <- mydata$DGP7
# DGP # print
plot(t, DGP, type = "l", main = "DGP7") # plot
mean(DGP) # mean or expected value
var(DGP) # variance
acf(DGP) # ACF
pacf(DGP) # PACF

arima(DGP) # arima
arima(DGP, order = c(0,0,2)) # MA(2)
arima(DGP, order = c(0,0,2), fixed = c(0.75, -0.5, NA)) # fixed coefficients


#### DGP 8 ####
DGP <- mydata$DGP8
# DGP # print
plot(t, DGP, type = "l", main = "DGP8") # plot
mean(DGP) # mean or expected value
var(DGP) # variance
acf(DGP) # ACF
pacf(DGP) # PACF

arima(DGP) # arima
arima(DGP, order = c(1,0,1)) # ARMA(1,1)
arima(DGP, order = c(1,0,1), fixed = c(0.75, 0.5, NA)) # fixed coefficients
