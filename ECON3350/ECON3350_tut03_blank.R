# Tutorial 03: Forecasting Univariate Processes - II

# Answers by Eric Eisenstat
# https://sites.google.com/view/ericeisenstat

# Extra material by Francisco Tavares Garcia
# https://tavaresgarcia.github.io/


# House cleaning
rm(list = ls()) # clear Environment
if (!is.null(dev.list())) dev.off() # clear Plots
cat("\014") # clear Console

# Note that "forecast" requires the package "colorspace", which is not (always)
# installed automatically because on CRAN, the source version is newer than the
# binary version. To install "colorspace", it may be necessary to begin
# installation then say "No" to installing from sources.
install.packages("forecast")
library(forecast)


# 1. The file Merck.csv contains daily data of stock prices of Merck & Co., Inc.
# (MRK) during 2001–2013. In what follows, we use y(t) to denote the adjusted
# closing prices (Adj Close in the data) at time t.

# (a) Load the data into R and construct a data set with observations in the
# range 1 January 2011—31 January 2012.


# (b) Construct the following variables:
#   - changes in prices: ∆y(t) = y(t) - y(t-1);
#   - log returns: rt = log(y(t)/y(t-1)).


# (c) Draw time series plots of y(t), ∆y(t) and rt; comment on the stationarity
# of the processes that may have generated these observations.


# (d) Compute and plot the sample ACFs and PACFs of y(t) and ∆y(t). Comment on
# your findings.


# (e) Propose and estimate 25 ARMA(p, q) models for ∆y(t).


# (f) Use the AIC and BIC to reduce the set of ARMA(p, q) models.


# (g) Draw time series plots of the estimated residuals you obtained for the
# ARMA models selected in part (f). Comment on your findings. Run the Ljung-Box
# test (at the α = 5% significance level) to test the white noise hypothesis on
# estimated residuals obtained from each ARMA in the set obtain in part (f) and
# report the test results. Use this information to identify the adequate set of
# specified ARMAs.


# (h) Forecast changes in MRK stock prices in January, 2012. For each ARMA model
# in the adequate set, compare your predicted price changes with real price
# changes in the data. Compare the forecasts you obtained as well as their
# “quality” across ARMA models and comment on the robustness of the generated
# forecasts.


# (i) Forecast MRK prices y(t) (levels this time, instead of changes) using an
# ARMA(2, 1) model only. Compare your predicted prices with real prices in the
# data. Compare the price forecasts obtained in this part with price forecasts
# obtained by transforming the forecasts in part (h). HINT: you will need
# convert predicted prices changes to predicted prices.


# (j) OPTIONAL: Repeat parts (d)–(h) for log returns rt. Note that here you will
# forecast daily returns (y(t) - y(t-1))/y(t-1) in January, 2012. Hint: Recall
# that (y(t) - y(t-1))/y(t-1) ≈ rt.
