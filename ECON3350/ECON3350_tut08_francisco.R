# Tutorial 08: Cointegration - I

# Answers by Eric Eisenstat
# https://sites.google.com/view/ericeisenstat

# Extra material by Francisco Tavares Garcia
# https://tavaresgarcia.github.io/


# House cleaning
rm(list = ls()) # clear Environment
if (!is.null(dev.list())) dev.off() # clear Plots
cat("\014") # clear Console

# To use the following line, save this file in the same folder as the data files
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# For this tutorial, we need to load a few packages:
# forecast (For Arima and other time series functions),
# dplyr  (to arrange matrices),
# zoo (to manipulate dates),
# aTSA (for ADF and other time series functions).
library(forecast)
library(dplyr)
library(zoo)
library(aTSA)

# create a function to estimate a range of ADF regression specifications
# in levels along with the AICs and BICs
ADF_estimate_lev <- function(y,  p_max = 9)
{
  TT <- length(y)
  ADF_est <- list()
  ic <- matrix(nrow = 3 * (1 + p_max), ncol = 5)
  colnames(ic) <- c("const", "trend", "p", "aic", "bic")
  i <- 0
  for (const in 0:1)
  {
    for (p in 0:p_max)
    {
      i <- i + 1
      try(silent = T, expr =
            {
              ADF_est[[i]] <- Arima(diff(y), xreg = y[-TT],
                                    order = c(p, 0, 0),
                                    include.mean = as.logical(const),
                                    include.drift = F)
              ic[i,] <- c(const, 0, p, ADF_est[[i]]$aic,
                          ADF_est[[i]]$bic)
            })
    }
    
    if (const)
    {
      # only add a specification with trend if there is a
      # constant (i.e., exclude no constant with trend)
      for (p in 0:p_max)
      {
        i <- i + 1
        try(silent = T, expr =
              {
                ADF_est[[i]] <- Arima(diff(y), xreg = y[-TT],
                                      order = c(p, 0, 0),
                                      include.mean = as.logical(const),
                                      include.drift = T)
                ic[i,] <- c(const, 1, p, ADF_est[[i]]$aic,
                            ADF_est[[i]]$bic)
              })
      }
    }
  }
  
  ic_aic <- ic[order(ic[,4]),][1:10,]
  ic_bic <- ic[order(ic[,5]),][1:10,]
  
  return(list(ADF_est = ADF_est, ic = ic,
              ic_aic = ic_aic, ic_bic = ic_bic))
}

# create a function to estimate a range of ADF regression specifications
# in differences along with the AICs and BICs
ADF_estimate_diff <- function(y, p_max = 9)
{
  TT <- length(diff(y))
  ADF_est_diff <- list()
  ic_diff <- matrix(nrow = 3 * (1 + p_max), ncol = 5)
  colnames(ic_diff) <- c("const", "trend", "p", "aic", "bic")
  i <- 0
  for (const in 0:1)
  {
    for (p in 0:p_max)
    {
      i <- i + 1
      try(silent = T, expr =
            {
              ADF_est_diff[[i]] <- Arima(diff(diff(y)),
                                         xreg = diff(y)[-TT],
                                         order = c(p, 0, 0),
                                         include.mean = as.logical(const),
                                         include.drift = F)
              ic_diff[i,] <- c(const, 0, p, ADF_est_diff[[i]]$aic,
                               ADF_est_diff[[i]]$bic)
            })
    }
    
    if (const)
    {
      # only add a specification with trend if there is a
      # constant (i.e., exclude no constant with trend)
      for (p in 0:p_max)
      {
        i <- i + 1
        try(silent = T, expr =
              {
                ADF_est_diff[[i]] <- Arima(diff(diff(y)),
                                           xreg = diff(y)[-TT],
                                           order = c(p, 0, 0),
                                           include.mean = as.logical(const),
                                           include.drift = T)
                ic_diff[i,] <- c(const, 1, p, ADF_est_diff[[i]]$aic,
                                 ADF_est_diff[[i]]$bic)
              })
      }
    }
  }
  
  ic_aic_diff <- ic_diff[order(ic_diff[,4]),][1:10,]
  ic_bic_diff <- ic_diff[order(ic_diff[,5]),][1:10,]
  
  return(list(ADF_est_diff = ADF_est_diff,
              ic_diff = ic_diff,
              ic_aic_diff = ic_aic_diff,
              ic_bic_diff = ic_bic_diff))  
}

# 1.
#
# load the data in term_structure.csv
mydata <- read.delim("term_structure.csv", header = TRUE,  sep = ",")

# store each of the columns in mydata in a different variable
dates <- as.yearmon(mydata$obs, format = "%YM%m") 
i3y <- mydata$I3Y
i5y <- mydata$I5Y
i90d <- mydata$I90D
i180d <- mydata$I180D

### i3y
# Let's visualize the time series
plot(dates, i3y, type = "l") # plot time series
acf(i3y); acf(i3y, plot = F) # ACF of the time series

i3y_ADF_lev <- ADF_estimate_lev(i3y, p_max = 15) # run ADF with 15 lags
i3y_ADF_lev$ic_aic # top 10 AIC
i3y_ADF_lev$ic_bic # top 10 BIC

# AIC: constant + trend and lags 7, 10;
#      constant + no trend and lags 10
# BIC: no constant + no trend and lags 1;
#      constant + no trend and lags 2, 3;
#      constant + trend and lags 2, 3.

i3y_adq_set <- as.matrix(arrange(as.data.frame(
  rbind(i3y_ADF_lev$ic_aic[c(1, 6, 9),],
        i3y_ADF_lev$ic_bic[c(1, 3, 5:7),])),
  const, trend, p))
i3y_adq_set

i3y_adq_idx <- match(data.frame(t(i3y_adq_set[, 1:3])),
                     data.frame(t(i3y_ADF_lev$ic[, 1:3])))


for (i in 1:length(i3y_adq_idx)){
  checkresiduals(i3y_ADF_lev$ADF_est[[i3y_adq_idx[i]]])
}
# We reject white noise residuals at 5% sig level for all models with p < 7.
# Hence, we remove all models except those with p = 7,10 and constant +/- trend.

# This means we should run the ADF test with nlag = 11, but we will include 
# nlag = 15 just to get a bit more info
adf.test(i3y, nlag = 15)

# For specifications with a constant, no trend, all specifications except p = 3,
# the null cannot be rejected at the 5% significance level. For specifications
# with a constant and with a trend, the same conclusion holds for all
# specifications except p = 3,4,7,9. Our concern is the one with  p= 7 since it
# is in our adequate set.

# However, might note that the p-value for p = 7 is 0.475, indicating that if we
# choose 4.75% as the significance level, then we should conclude that the null
# cannot be rejected for any specification in the adequate set. Is there a great
# reason to commit to 5% versus 4.75%? That is a question we would need to
# consider more profoundly in this particular case.

# Overall we might lean towards concluding that i3y is not empirically
# distinguishable from a unit root process, with some ambiguity arising from the
# specification uncertainty that results from the constant with trend and p = 7
# specification rejecting a unit root at the 5% significance level (but not the
# 4.75% level).


## now repeat for the differenced i3y

# Let's visualize the time series
plot(dates[-1], diff(i3y), type = "l") # plot time series
acf(diff(i3y)); acf(diff(i3y), plot = F) # ACF of the time series

i3y_ADF_diff <- ADF_estimate_diff(i3y, p_max = 15)
i3y_ADF_diff$ic_aic_diff
i3y_ADF_diff$ic_bic_diff

# AIC and BIC agree on no const, no trend, p = 4, 5 as well as const, no trend
# with p = 4

i3y_adq_set_diff <- as.matrix(arrange(as.data.frame(
  i3y_ADF_diff$ic_bic_diff[c(1, 3, 5),]),
  const, trend, p))
i3y_adq_set_diff

i3y_adq_idx_diff <- match(data.frame(
  t(i3y_adq_set_diff[, 1:3])),
  data.frame(
    t(i3y_ADF_diff$ic_diff[, 1:3])))

for (i in 1:length(i3y_adq_idx_diff)){
  checkresiduals(
    i3y_ADF_diff$ADF_est_diff[[i3y_adq_idx_diff[i]]])
}
# All residuals look ok.

adf.test(diff(i3y), nlag = 6)

# The null is rejected for all specs. We conclude diff(i3y) is empirically
# distinguishable from I(1). We can infer that the order of integration is not
# greater than one with a high degree of confidence. However, we do not have
# conclusive inference on how distinguishable it is from an I(1).

### i5y
# Let's visualize the time series
plot(dates, i5y, type = "l") # plot time series
acf(i3y); acf(i5y, plot = F) # ACF of the time series

i5y_ADF_lev <- ADF_estimate_lev(i5y, p_max = 15)
i5y_ADF_lev$ic_aic
i5y_ADF_lev$ic_bic

# AIC: only constant + trend and lags 2-7, 9-12;
# BIC: no constant + no trend and lags 0-3;
#      constant + no trend and lags 1-3;
#      constant + trend and lags 1-3.
# AIC and BIC agree on const + trend and lags 2-3, so we include these along
# with p=10 (AIC pref) and no const, no trend, p = 1 (BIC pref)

i5y_adq_set <- as.matrix(arrange(as.data.frame(
  rbind(i5y_ADF_lev$ic_aic[1,],
        i5y_ADF_lev$ic_bic[c(1, 7:8),])),
  const, trend, p))
i5y_adq_set

i5y_adq_idx <- match(data.frame(t(i5y_adq_set[, 1:3])),
                     data.frame(t(i5y_ADF_lev$ic[, 1:3])))

for (i in 1:length(i5y_adq_idx)){
  checkresiduals(i5y_ADF_lev$ADF_est[[i5y_adq_idx[i]]])
}
# We fail to reject white noise residuals at 5% sig level for all
# specifications. Also, all SACFs look similar, so no reason to remove any
# particular spec from the adequate set. We proceed to the ADF test.

# To accommodate all specs in the adequate set, we use nlag = 13
adf.test(i5y, nlag = 13)

# we fail to reject $H_0$ at the 5% significance level for all specifications
# except with a constant, trend and p = 3,9. Again, p = 3 is one of four
# specifications in the adequate set. As with i3y, we might lean towards
# inferring that i5y is not empirically distinguishable from a unit root
# process, but there would be a nontrivial degree of uncertainty in this
# conclusion.

## now repeat for the differenced i5y

# Let's visualize the time series
plot(dates[-1], diff(i5y), type = "l") # plot time series
acf(diff(i3y)); acf(diff(i5y), plot = F) # ACF of the time series

i5y_ADF_diff <- ADF_estimate_diff(i5y, p_max = 15)
i5y_ADF_diff$ic_aic_diff
i5y_ADF_diff$ic_bic_diff

# AIC and BIC agree on no const, no trend, p = 4-6 as well as const, no trend
# with p = 4

i5y_adq_set_diff <- as.matrix(arrange(as.data.frame(
  i5y_ADF_diff$ic_bic_diff[c(2, 5, 7, 10),]),
  const, trend, p))
i5y_adq_set_diff

i5y_adq_idx_diff <- match(data.frame(
  t(i5y_adq_set_diff[, 1:3])),
  data.frame(
    t(i5y_ADF_diff$ic_diff[, 1:3])))

for (i in 1:length(i5y_adq_idx_diff)){
  checkresiduals(i5y_ADF_diff$ADF_est_diff[[i5y_adq_idx_diff[i]]])
}
# All residuals look ok.

adf.test(diff(i5y), nlag = 7)

# The null is rejected for all specs. We conclude diff(i5y) is empirically
# distinguishable from I(1). We can infer that the order of integration is not
# greater than one with a high degree of confidence. However, we do not have
# conclusive inference on how distinguishable it is from an I(1).

### i90d
# Let's visualize the time series
plot(dates, i90d, type = "l") # plot time series
acf(i90d, na.action = na.pass); acf(i90d, plot = F, na.action = na.pass) # ACF

i90d_ADF_lev <- ADF_estimate_lev(i90d, p_max = 15)
i90d_ADF_lev$ic_aic
i90d_ADF_lev$ic_bic

# AIC: constant + trend and lags 3-6, 8;
#      constant + no trend and lags 3-6, 8;
# BIC: no constant + no trend and lags 1, 2, 4;
#      constant + no trend and lags 1-4;
#      constant + trend and lags 1-3.
# They agree on constant + trend with p=3 and const, no trend with p=3, 4. We
# might also include const, no trend p=1 as this is top pref by BIC.

i90d_adq_set <- as.matrix(arrange(as.data.frame(
  rbind(i90d_ADF_lev$ic_aic[c(1, 5, 8),],
        i90d_ADF_lev$ic_bic[1,])),
  const, trend, p))
i90d_adq_set

i90d_adq_idx <- match(data.frame(t(i90d_adq_set[, 1:3])),
                      data.frame(t(i90d_ADF_lev$ic[, 1:3])))

for (i in 1:length(i90d_adq_idx)){
  checkresiduals(i90d_ADF_lev$ADF_est[[i90d_adq_idx[i]]])
}
# We reject white noise residuals at 5% sig level for const, no trend with
# p = 1 (and also see that the SACF has noticeably larger jumps relative to
# other specifications). Hence, p = 1 can be dropped.

adf.test(i90d)

# For all specs, but the last, in the adequate set we reject H0 at 5% sig level
# and conclude that i90d is empirically distinguishable from a unit root process
# Since we infer that i90d is I(0), there is no need to run the test on diffs.

### i180d

# Let's visualize the time series
plot(dates, i180d, type = "l") # plot time series
acf(i180d, na.action = na.pass); acf(i180d, plot = F, na.action = na.pass) # ACF

i180d_ADF_lev <- ADF_estimate_lev(i180d, p_max = 15)
i180d_ADF_lev$ic_aic
i180d_ADF_lev$ic_bic

# AIC: constant + trend and lags 2-6;
#      constant + no trend and lags 2-6;
# BIC: no constant + no trend and lags 1-3;
#      constant + no trend and lags 1-4;
#      constant + trend and lags 1-3.
# They agree on constant + trend with p=2, 3 and const, no trend with p=2-4.
# We might also include p=1 specs with const +/- trend and p=4 with const+trend.

i180d_adq_set <- as.matrix(arrange(as.data.frame(
  rbind(i180d_ADF_lev$ic_aic[c(1:3, 5, 7, 9),],
        i180d_ADF_lev$ic_bic[c(1, 5),])),
  const, trend, p))
i180d_adq_set

i180d_adq_idx <- match(data.frame(t(i180d_adq_set[, 1:3])),
                     data.frame(t(i180d_ADF_lev$ic[, 1:3])))

for (i in 1:length(i180d_adq_idx)){
  checkresiduals(i180d_ADF_lev$ADF_est[[i180d_adq_idx[i]]])
}
# We fail to reject zero-autocorrelation for all specs at the 5% sig level, but
# not that sample autocorrelations appear to stand out for models with p = 1
# (particularly the third lag)

adf.test(i180d)

# Again, we reject H0 at 5% sig level for some specs in the adequate set but not
# others. The ADF results are not sufficiently accurate to ascertain the
# proximity of the DGP to a unit root process. This is true even if we ignore
# specifications with p = 1 (in light of the sample AC observation above)

## now repeat for the differenced i180d

# Let's visualize the time series
plot(dates[-1], diff(i180d), type = "l") # plot time series
acf(diff(i180d), na.action = na.pass)
acf(diff(i180d), plot = F, na.action = na.pass) # ACF of the time series

i180d_ADF_diff <- ADF_estimate_diff(i180d, p_max = 15)
i180d_ADF_diff$ic_aic_diff
i180d_ADF_diff$ic_bic_diff

# Both AIC and BIC agree on smaller lag lengths

i180d_adq_set_diff <- as.matrix(arrange(as.data.frame(
  intersect(as.data.frame(i180d_ADF_diff$ic_aic_diff),
            as.data.frame(i180d_ADF_diff$ic_bic_diff))),
  const, trend, p))
i180d_adq_set_diff

i180d_adq_idx_diff <- match(data.frame(
  t(i180d_adq_set_diff[, 1:3])),
  data.frame(
    t(i180d_ADF_diff$ic_diff[, 1:3])))

for (i in 1:length(i180d_adq_idx_diff)){
  checkresiduals(i180d_ADF_diff$ADF_est_diff[[i180d_adq_idx_diff[i]]])
}
# All residuals look ok.

adf.test(diff(i180d), nlag = 10)

# For lag lengths up to 9, H0 is universally rejected at a very small sig level.
# The process generating diff(i180d) is clearly distinguishable from a unit
# root. However, we cannot say how close i180d is to an I(1) using the ADF
# testing approach.

# 2.
#
# We need to construct a set of adequate ADF specifications for the estimated
# residuals from the regression of i5y on a constant, i3y, i90d, and i180d.
# A regression in R is implemented using the lm function.
eg_reg <- lm(i5y ~ i3y + i90d + i180d, mydata)
eg_res <- eg_reg$residuals

# same approach as in Q1 but with eg_res instead of data
egr_ADF_lev <- ADF_estimate_lev(eg_res, p_max = 15)
egr_ADF_lev$ic_aic
egr_ADF_lev$ic_bic

# We will only consider specifications without a constant or trend since we are
# focusing on residuals.
# AIC: no constant + no trend and lags 0-3, 15;
# BIC: no constant + no trend and lags 0-3;

egr_adq_set <- as.matrix(arrange(as.data.frame(
  egr_ADF_lev$ic_bic[c(1, 2, 4, 7), ]), 
  const, trend, p))
egr_adq_set

egr_adq_idx <- match(data.frame(t(egr_adq_set[, 1:3])),
                     data.frame(t(egr_ADF_lev$ic[, 1:3])))

for (i in 1:length(egr_adq_idx)){
  checkresiduals(egr_ADF_lev$ADF_est[[egr_adq_idx[i]]])
}
# All residuals look OK.

# Now, we can use the function coint.test from the aTSA package to implement the
# E-G test for the specs in the adequate set.
eg_test <- matrix(nrow = 1, ncol = 4)
colnames(eg_test) <- rep("", 4)
rownames(eg_test) <- c("No const, no trend")
for (l in 1:4){
  eg_l <- coint.test(i5y, cbind(i3y, i90d, i180d), nlag = l, output = F)
  eg_test[, l] <- eg_l[1, 3]
  colnames(eg_test)[l] <- paste("Lag", l)
}
eg_test

# For all specs with no const or trend, the unit root in the residuals is
# rejected at low sig level. The best inference we can make is that if the
# residual in the regression i5y on i3y, i90d and i180d is mean-independent,
# then it also does not have a unit root.

# 3.
#
# In Question 1, we concluded that i3y is not empirically distinguishable from
# I(1), while i90d is. For i5y and i180d our inference on their proximity to
# I(1) processes is rather ambiguous.
#
# When we regress i5y on i3y, i90d and i180d, we find that the residuals process
# does not have a unit root if we enforce the restriction that residuals are
# mean-independent. Assuming this restriction is valid, we have the following
# possibilities:
# 1. i3y, i5y, i90d and i180d are all I(0)
# 2. Any three processes are I(1) and cointegrated while a fourth is I(0); for
#    example, we could have i3y, i5y and i90d are cointegrated and i180d is
#    is I(0). The same could hold for any other combinations.
# 3. Any two processes are I(1) and cointegrated while the other two are I(0);
#    for example, we could have i3y and i5y cointegrated while i90d and i180d
#    are both I(0).
# 4. Any two processes are I(1) and cointegrated and the other two processes
#    are also I(1) and cointegrated, but the four processes are not all
#    cointegrated with each other in a single cointegrating relation.
# 5. All four processes are I(1) and cointegrated in a single cointegrating
#    relation.
#
# Which of these five scenarios prevails? It depends on what we assume about
# the integration properties of the processes involved. Our unit root tests
# in Question 1 did not clearly reject a unit root in any of the processes,
# except i90d. If i90d is I(0), then we can rule out scenarios 4 and 5.

# In terms of scenarios 1-3,we can in principle make the unit root assumption
# about any combination of i3y, i5y and i180d, which will determine the
# appropriate interpretation. The important thing to remember is that it is
# ALWAYS an assumption that a unit root exists! Whether or not it's a useful one
# depends on the application.

# 4.
#
## Dependent variable: i3y
eg_reg <- lm( i3y ~ i5y + i90d + i180d, mydata)
eg_res <- eg_reg$residuals

# same approach as in Q1 but with eg_res instead of data
egr_ADF_lev <- ADF_estimate_lev(eg_res, p_max = 15)
egr_ADF_lev$ic_aic
egr_ADF_lev$ic_bic

# AIC: no constant + no trend and lags 0-4, 15;
# BIC: no constant + no trend and lags 0-3;

egr_adq_set <- as.matrix(arrange(as.data.frame(
  egr_ADF_lev$ic_bic[c(1, 2, 4, 7),]),
  const, trend, p))
egr_adq_set

egr_adq_idx <- match(data.frame(t(egr_adq_set[, 1:3])),
                     data.frame(t(egr_ADF_lev$ic[, 1:3])))

for (i in 1:length(egr_adq_idx)){
  checkresiduals(egr_ADF_lev$ADF_est[[egr_adq_idx[i]]])
}
# All residuals look OK.

# Now, we can use the function coint.test from the aTSA package to implement
# the E-G test for the specs in the adequate set.
eg_test <- matrix(nrow = 1, ncol = 4)
colnames(eg_test) <- rep("", 4)
rownames(eg_test) <- c("No const, no trend")
for (l in 1:4){
  eg_l <- coint.test(i3y, cbind(i5y, i90d, i180d), nlag = l, output = F)
  eg_test[, l] <- eg_l[1, 3]
  colnames(eg_test)[l] <- paste("Lag", l)
}
eg_test

# We get the same results as with i5y being the dependent variable.

## Dependent variable: i90d
eg_reg <- lm( i90d ~ i3y + i5y + i180d, mydata)
eg_res <- eg_reg$residuals

# same approach as in Q1 but with eg_res instead of data
egr_ADF_lev <- ADF_estimate_lev(eg_res, p_max = 15)
egr_ADF_lev$ic_aic
egr_ADF_lev$ic_bic

# AIC: no constant + no trend and lags 0-4;
# BIC: no constant + no trend and lags 0-3.
egr_adq_set <- as.matrix(arrange(as.data.frame(
  egr_ADF_lev$ic_bic[c(1, 2, 6, 7),]),
  const, trend, p))
egr_adq_set

egr_adq_idx <- match(data.frame(t(egr_adq_set[, 1:3])),
                     data.frame(t(egr_ADF_lev$ic[, 1:3])))

for (i in 1:length(egr_adq_idx)){
  checkresiduals(egr_ADF_lev$ADF_est[[egr_adq_idx[i]]])
}
# All residuals look OK.

# Now, we can use the function coint.test form the aTSA package to implement
# the E-G test for the specs in the adequate set.
eg_test <- matrix(nrow = 1, ncol = 4)
colnames(eg_test) <- rep("", 4)
rownames(eg_test) <- c("No const, no trend")
for (l in 1:4){
  eg_l <- coint.test(i90d, cbind(i3y, i5y, i180d), nlag = l, output = F)
  eg_test[, l] <- eg_l[1, 3]
  colnames(eg_test)[l] <- paste("Lag", l)
}
eg_test

# We get the same results as with i5y being the dependent variable.

## Dependent variable: i180d
eg_reg <- lm( i180d ~ i3y + i5y + i90d, mydata)
eg_res <- eg_reg$residuals

# same approach as in Q1 but with eg_res instead of data
egr_ADF_lev <- ADF_estimate_lev(eg_res, p_max = 15)
egr_ADF_lev$ic_aic
egr_ADF_lev$ic_bic

# AIC: no constant + no trend and lags 0-3, 5;
# BIC: no constant + no trend and lags 0-3, 5;

egr_adq_set <- as.matrix(arrange(as.data.frame(
  egr_ADF_lev$ic_bic[c(1, 2, 5, 7, 9), ]),
  const, trend, p))
egr_adq_set

egr_adq_idx <- match(data.frame(t(egr_adq_set[, 1:3])),
                     data.frame(t(egr_ADF_lev$ic[, 1:3])))

for (i in 1:length(egr_adq_idx)){
  checkresiduals(egr_ADF_lev$ADF_est[[egr_adq_idx[i]]])
}
# All residuals look OK.

# Now, we can use the function coint.test form the aTSA package to implement
# the E-G test for the specs in the adequate set.
eg_test <- matrix(nrow = 1, ncol = 6)
colnames(eg_test) <- rep("", 6)
rownames(eg_test) <- c("No const, no trend")
for (l in 1:6){
  eg_l <- coint.test(i180d, cbind(i3y, i5y, i90d), nlag = l, output = F)
  eg_test[, l] <- eg_l[1, 3]
  colnames(eg_test)[l] <- paste("Lag", l)
}
eg_test

# We get the same results as with i5y being the dependent variable.

# Changing the dependent variable does not materially affect our inference about
# cointegrating relations involving these four processes.


# 5.
#
# Capital market spread: i5y - i3y
cm_ADF_lev <- ADF_estimate_lev(i5y - i3y, p_max = 15)
cm_ADF_lev$ic_aic
cm_ADF_lev$ic_bic

# AIC: constant + no trend and lags 2-4, 15
#      constant + trend and lags 1-4, 6, 15.
# BIC: no constant + no trend and lags 0-3;
#      constant + no trend and lags 0-2;
#      constant + trend and lags 1-2.

cm_adq_set <- as.matrix(arrange(as.data.frame(
                      cm_ADF_lev$ic_bic[c(2:3, 6:8, 10),]),
                      const, trend, p))
cm_adq_set

cm_adq_idx <- match(data.frame(t(cm_adq_set[, 1:3])),
                     data.frame(t(cm_ADF_lev$ic[, 1:3])))

for (i in 1:length(cm_adq_idx)){
  checkresiduals(cm_ADF_lev$ADF_est[[cm_adq_idx[i]]])
}
# We reject white noise residuals at 5% sig level for all models with p = 0,
# although the SACFs do not look very different from models with larger p.
# We keep this in mind when interpreting the ADF test results.

adf.test(i5y - i3y, nlag = 3)

# The test rejects a unit root for models with p=1, but fails to reject for
# models with p=2.

# Money market spread: i180d - i90d
mm_ADF_lev <- ADF_estimate_lev(i180d - i90d, p_max = 15)
mm_ADF_lev$ic_aic
mm_ADF_lev$ic_bic

# AIC: no constant + no trend and lags 1-3;
#      constant + no trend and lags 1-4;
#      constant + trend and lags 1-3.
# BIC: no constant + no trend and lags 0-4;
#      constant + no trend and lags 1-3;
#      constant + trend and lags 1-2.

mm_adq_set <- as.matrix(arrange(as.data.frame(
                        mm_ADF_lev$ic_bic[c(1:8),]),
                        const, trend, p))
mm_adq_set

mm_adq_idx <- match(data.frame(t(mm_adq_set[, 1:3])),
                    data.frame(t(mm_ADF_lev$ic[, 1:3])))

for (i in 1:length(mm_adq_idx)){
  checkresiduals(mm_ADF_lev$ADF_est[[mm_adq_idx[i]]])
}
# All residuals are OK

adf.test(i180d - i90d)

# The test rejects a unit root for all models in the adequate set. Note that
# some specifications lead to a failure to reject, but they are not in our
# adequate set, so we can ignore them! We can confidently conclude that the
# money market spread is I(0).