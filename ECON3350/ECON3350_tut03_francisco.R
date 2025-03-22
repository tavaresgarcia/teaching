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

## Q1
# (a)
# Set working directory (make sure you edit to your own WD)
# Ex Win: setwd("G:/My Drive/BEcon/TUTOR/ECON3350/03")
# Ex Mac: setwd("/Users/uqdkim7/Dropbox/Teaching/R tutorials/Tutorial03")

# Set working directory. To use the following line, save this script in the same
# folder as the data files.
# install.packages("rstudioapi")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load the data in Merck.csv
mydata <- read.delim("Merck.csv", header = TRUE,  sep = ",")

# creates a list of 0s and 1s, with 1 representing the selected sample
sel_sample <- mydata$Date >= as.Date("2011-01-01") &
  mydata$Date <= as.Date("2012-01-31")

# extracts values only for the selected sample
y <- as.matrix(mydata$Adj_Close[sel_sample])

# (b)
#
# difference and lag operators
Dy <- diff(y)

# log difference
r <- as.matrix(log(y[2:nrow(y)]) - log(lag(y[1:nrow(y) - 1])))

# a shorter version
r <- diff(log(y))

# set column names
colnames(y) <- "Stock Prices of Merck (MRK)"
colnames(Dy) <- "Changes in Stock Prices"
colnames(r) <- "Log Returns" 

# (c)
#
# we use time series plot to detect obvious trends, volatility clustering, etc
# this is typically the first thing to do when processing time series data
sel2011 <- mydata$Date[sel_sample] <= as.Date("2011-12-31") # only 2011
dates = as.Date(mydata$Date[sel_sample]) # saves dates for selected sample

plot(dates[sel2011], y[sel2011], type = "l", xlab = "Time (2011)",
     ylab = colnames(y)) # plot data as it is (levels)

plot(dates[sel2011], Dy[sel2011], type = "l", xlab = "Time (2011)",
     ylab = colnames(Dy)) # plot differences

plot(dates[sel2011], r[sel2011], type = "l", xlab = "Time (2011)",
     ylab = colnames(r)) # plot log differences

# the DGP for y is likely not stationary as its mean appears to vary over time;
# the DGP for Dy seems to have zero mean, but its variance may depend on time
# ignore the variance for now (we will cover volatility later) 

# (d)

acf(y[sel2011], main = colnames(y))
pacf(y[sel2011], main = colnames(y))

# For prices, SACF decays but the SPACF drops to zero after one lag;
# also, the first PAC is near 1, suggesting an AR(1) model of the form
# y[t] = y[t-1] + e[t].

acf(Dy[sel2011], main = colnames(Dy))
pacf(Dy[sel2011], main = colnames(Dy))

# For price changes, SACF drops to zero after lag zero but the SPACF oscillates;
# suggesting an MA(1) model of the form Dy[t] = e[t-1] + e[t]. Still, almost all
# partial autocorrelation is contained between the blue lines.


# (e)
# creates the ic (information criterion) matrix with empty values
ic <- matrix( nrow = 25, ncol = 4 )

# names the columns in the ic matrix
colnames(ic) <- c("p", "q", "aic", "bic")

# generates the 5x5 different combinations of ARMA (p,q)
for (p in 0:4){
  for (q in 0:4){
    # fits an Arima model with the specification (p,q) in the loop
    fit_p_q <- Arima(Dy, order = c(p, 0, q))
    
    # extracts the AIC and BIC and saves it to the ic matrix
    ic[p * 5 + q + 1,] = c(p, q, fit_p_q[["aic"]], fit_p_q[["bic"]])
  }
}

# (f)

ic

# A quick look through the aic/bic table constructed in (g) shows that aic and
# bic wildly disagree! There is no systematic approach to resolving this
# conflicting information; we will look at the top 10 specifications preferred
# by the aic as well as the top 10 preferred by the bic---this is easy to do by
# sorting the ic table.

ic_aic <- ic[order(ic[,3], decreasing = FALSE),][1:10,] # top models in AIC
ic_bic <- ic[order(ic[,4], decreasing = FALSE),][1:10,] # top models in BIC
ic_aic # print top 10
ic_bic # print top 10

# Next, we select the models that make top 10 in both lists; however, this is
# just a "sensible" choice in this particular setting -- there may be other
# reasonable ways of using the info provided by aic/bic to reduce the set; here
# is where judgement / experience / common sense come into play!
adq_set = list(c(1, 0, 1), c(1, 0, 2), c(2, 0, 1), c(3, 0, 0))

# (g)
#
# Once we have reduced the set, perform residual analysis;
# the "forecast" package simplifies this with the command "checkresiduals"
# we don't get carried away with any particular hypothesis test here --
# nothing clearly stands out to suggest a problem with correlated residuals,
# so we proceed with the four models in the set
for (i in 1:length(adq_set)){
  checkresiduals(Arima(Dy[sel2011], order = adq_set[[i]]))
}

# (h)
#
# here, we forecast each model in the set using the "forecast" command;
# again, it is all one easily and quickly with a for loop;
hrz = sum(sel_sample) - sum(sel2011) # hrz receives the horizon of forecast (Jan)
xticks <- c(sum(sel_sample) - 3 * hrz + c(1, 2 * hrz, 3 * hrz)) # position of dates for forecast x-axis
actual_Dy <- as.matrix(Dy[!sel2011]) # extracts the actual values for Jan 2012
fcst_Dy <- vector(mode = "list", length(adq_set)) # creates a list of forecasts

for (i in 1:length(adq_set)){
  # attributes the current model to the variable model_p_q
  model_p_q <- adq_set[[i]]
  
  # generate the forecasted values for the horizon hrz, and the bands 68% and 95%
  fcst_Dy[[i]] <- forecast(Arima(Dy[sel2011], order = model_p_q),
                           h = hrz, level = c(68, 95))
  
  
  # Creates the title for the plot
  title_p_q <- paste("ARMA(", as.character(model_p_q[1]), ", ",
                     as.character(model_p_q[3]), ")", sep = "")
  
  # plots the current data and the forecast line and bands
  plot(fcst_Dy[[i]], include = hrz * 2, ylab = colnames(Dy),
       main = title_p_q, xaxt = "n")
  
  # adds the actual values to the forecast
  lines(sum(sel2011) + 1:hrz, actual_Dy)
  
  # adds the respective dates to the x-axis label
  axis(1, at = xticks, labels = dates[xticks])
}
# when we use the "plot" command with output from the "forecast" command, we
# get a nice depiction of how the data is extrapolated into the future,
# complete with predictive intervals to capture uncertainty;
# we also add the actual outcomes in the forecast period to help us compare
# the forecast performance of each ARMA in the adequate set;
#
# predictive intervals for price changes appear to have a fixed width even as
# the forecast horizon increases (from 1 day to 20 days)
#
# comparison and inference:
# clearly, all four generate very similar forecasts for January 2012; therefore,
# the specification differences between them are not important for our purpose;
# alternatively, our forecast of price changes is ROBUST to minor differences in
# the specification of ARMA models in that we cannot clearly distinguish between
# them with our diagnostic tools.

# (i)
#
# repeat the forecasting steps with the "y" variable using an ARMA(2,1)
actual_y <- as.matrix(y[!sel2011]) # extracts the actual values for Jan 2012

# generate the forecasted values for the horizon hrz, and the bands 68% and 95%
fcst_y_lev = forecast(Arima(y[sel2011], order = c(2, 0, 1)),
                      h = hrz, level = c(68, 95) )

# plots the current data and the forecast line and bands
plot(fcst_y_lev, include = hrz * 2, ylab = colnames(y),
     main = "ARMA(2, 1)", xaxt = "n", ylim = c(26.1, 33.4))

# adds the actual values to the forecast
lines(sum(sel2011) + 1:hrz, actual_y)

# adds the respective dates to the x-axis label
axis(1, at = xticks, labels = dates[xticks])

# now repeat (g) using ARIMA on "y" instead of ARMA on "Dy";
# when we use an ARIMA on "y", it is the same as estimating an ARMA for "Dy",
# but the algorithm will generate forecasts for "y" instead of "Dy"; 
# in particular, it generates forecasts for "Dy", but then cumulatively sums
# them starting with an initial point "y0", which in our case is the last
# observation in the "pre-sample";
y0 <- mydata$Adj_Close[sum(mydata$Date < as.Date("2011-01-01") - 1)]
y_ext = as.matrix(c(y0, y[sel2011]))
fcst_y <- vector(mode = "list", length(adq_set))
for (i in 1:length(adq_set)){
  model_p_q <- adq_set[[i]]
  model_p_q[2] = 1
  fcst_y[[i]] <- forecast(Arima(y_ext, order = model_p_q, include.constant = T),
                          h = hrz, level = c(68, 95) )
  
  title_p_q <- paste("ARIMA(", as.character(model_p_q[1]), ", ",
                     as.character(model_p_q[2]), ", ",
                     as.character(model_p_q[3]), ")", sep = "")
  
  plot(fcst_y[[i]], include = hrz * 2, ylab = colnames(y),
       main = title_p_q, xaxt = "n")
  
  lines(1 + sum(sel2011) + 1:hrz, actual_y)
  
  axis(1, at = 1 + xticks, labels = dates[xticks])
}
# the first thing to note from the results is that, again, all ARIMAs in the
# adequate set generate very similar forecasts;
# the second is to note that the predictive intervals for "y" increase as the
# horizon increases (they are narrower for forecasts in the beginning of the
# forecasting period, and wider towards the end of the forecast period);
# this is a reflection of "y" being possibly generated by a non-stationary
# process --> more uncertainty in the more distant future!
# finally, compare the ARIMA(1,1) to the ARMA(2,1) in "y"; we
# will cover later why in particular these two specifications provide an
# especially interesting comparison but, for now, note that the ARMA(2,1) also
# produces predictive intervals that increase as the horizon increases, but
# relative to the ARIMA(1,1), the intervals indicate that prices should fall in
# January 2012. This is slightly different from what the ARIMA(1,1) produces --
# although the intervals largely overlap, the ARIMA(1,1) clearly puts more
# weight on higher prices in January; when comparing to the actual observations
# in January 2012, it is easy to see that the ARIMA forecasts are better (which
# can be confirmed by formal metrics)

# (j)

acf(r[sel2011], main = colnames(y))
pacf(r[sel2011], main = colnames(y))

ic <- matrix( nrow = 25, ncol = 4 )
colnames(ic) <- c("p", "q", "aic", "bic")
for (p in 0:4){
  for (q in 0:4)  {
    fit_p_q <- Arima(r, order = c(p, 0, q))
    c(p * 5 + q + 1, p, q)
    ic[p * 5 + q + 1,] = c(p, q, fit_p_q[["aic"]], fit_p_q[["bic"]])
  }
}

ic_aic <- ic[order(ic[,3], decreasing = FALSE),][1:10,]
ic_bic <- ic[order(ic[,4], decreasing = FALSE),][1:10,]

adq_set = list(c(1, 0, 1), c(1, 0, 2), c(2, 0, 1), c(3, 0, 0))

for (i in 1:length(adq_set)){
  checkresiduals(Arima(r[sel2011], order = adq_set[[i]]))
}

hrz <- sum(sel_sample) - sum(sel2011)
xticks <- c(sum(sel_sample) - 3 * hrz + c(1, 2 * hrz, 3 * hrz))
actual_r <- as.matrix(r[!sel2011])
fcst_r <- vector(mode = "list", length(adq_set))
for (i in 1:length(adq_set)){
  model_p_q <- adq_set[[i]]
  fcst_r[[i]] <- forecast(Arima(r[sel2011], order = model_p_q),
                          h = hrz, level = c(68, 95))
  
  title_p_q <- paste("ARMA(", as.character(model_p_q[1]), ", ",
                     as.character(model_p_q[3]), ")", sep = "")
  
  plot(fcst_r[[i]], include = hrz * 2, ylab = colnames(r),
       main = title_p_q, xaxt = "n")
  lines(sum(sel2011) + 1:hrz, actual_r)
  axis(1, at = xticks, labels = dates[xticks])
}
