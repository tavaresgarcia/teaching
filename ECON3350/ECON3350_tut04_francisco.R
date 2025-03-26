# Tutorial 04: Trends and Cycles

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
# forecast (already installed from tut 3),
# dplyr, zoo (included in default R installation)
# aTSA (needs to be installed)
install.packages("aTSA")

library(forecast)
library(dplyr)
library(zoo)
library(aTSA)

# 1
#
# (a)
#
# load the data in usdata.csv
mydata <- read.delim("usdata.csv", header = TRUE,  sep = ",")

dates <- as.yearqtr(mydata$obs) # stores dates (year and quarter)
y <- mydata$GDP # stores GDP
r <- mydata$FFR # stores interest rate

# plot GDP in levels
plot(dates, y, type = "l", xlab = "Time (Quarters)",
     main = "Log Real US GDP per Capita")

acf(y, plot=T) # acf of GDP

# (b)
#
# Estimate ADF regressions for different values of p plus combinations of
# constant and/or trend. We only consider models with a trend if it also has a
# const. In R, we use implement this with the 'Arima' function from 'forecast'.

TT <- length(y) # stores the length of y to use in xreg
ADF_est <- list() # initializes an empty list of estimated models
ic <- matrix( nrow = 30, ncol = 5 ) # matrix of models and information criteria
colnames(ic) <- c("cons", "trend", "p", "aic", "bic") # ic matrix column names
i <- 0 # index for matrices rows

for (const in 0:1){
  for (p in 0:9){

    i <- i + 1 # increments the index
    
    # estimates the Arima model
    ADF_est[[i]] <- Arima(diff(y), xreg = y[-TT],
                          order = c(p, 0, 0),
                          include.mean = as.logical(const),
                          include.drift = F)
    
    # stores the model and ICs in the matrix
    ic[i,] <- c(const, 0, p, ADF_est[[i]]$aic,
                             ADF_est[[i]]$bic)
  }
  
  if (const){
    # only add a specification with trend if there is a
    # constant (i.e., exclude no constant with trend)
    for (p in 0:9){
      i <- i + 1 # increments the index
      
      # estimates the Arima model
      ADF_est[[i]] <- Arima(diff(y), xreg = y[-TT],
                            order = c(p, 0, 0),
                            include.mean = as.logical(const),
                            include.drift = T)
      
      # stores the model and ICs in the matrix
      ic[i,] <- c(const, 1, p, ADF_est[[i]]$aic,
                               ADF_est[[i]]$bic)
    }
  }
}

ic_aic <- ic[order(ic[,4]),][1:10,] # top 10 models according to AIC
ic_bic <- ic[order(ic[,5]),][1:10,] # top 10 models according to BIC

# does it need to be the top 10?

# The AIC prefers specifications with both a constant and trend as well as lag
# lengths in the range p = 1, ..., 5. The BIC ranking includes specifications
# with both a constant and trend as well as lags p = 1, ..., 3. However, it also
# includes specifications with a constant only and lags p = 1, ..., 2. Putting
# this information together, we select the top five specifications preferred by
# the BIC.

# saves the ic info from top 5 BIC
adq_set <- as.matrix(arrange(as.data.frame(ic_bic[1:5,]),
                   const, trend, p))

# saves the index from each of the 5 models.
adq_idx <- match(data.frame(t(adq_set[, 1:3])),
                 data.frame(t(ic[, 1:3])))

# Check the residuals for specifications in the adequate set.
for (i in 1:length(adq_idx)){
  checkresiduals(ADF_est[[adq_idx[i]]])
}

# As no obvious problems jump out from the residuals analysis, we proceed with
# the adequate set constructed above.

# (c)
#
adf.test(y)

# Since only "Type 2" and "Type 3" specifications are in our adequate set, we
# ignore the output related to "Type 1" specifications. Consequently, for all
# specifications in our adequate set, the null of unit root cannot be rejected.
# We conclude that the process is not empirically distinguishable from an
# integrated process with a drift (and possibly linear growth).

# (d)
#
# repeat for differenced y series
TT <- length(diff(y)) # stores the length of diff(y) to use in xreg
ADF_est_diff <- list() # initializes an empty list of estimated models

# plot GDP change (differences)
plot(dates[-1], diff(y), type = "l", xlab = "Time (Quarters)",
     main = "Log Real US GDP per Capita")

acf(diff(y), plot=T) # acf of GDP
pacf(diff(y))

ic_diff <- matrix( nrow = 30, ncol = 5 ) # matrix of models and information criteria
colnames(ic_diff) <- c("cons", "trend", "p", "aic", "bic") # ic matrix column names
i <- 0 # index for matrices rows
for (const in 0:1){
  for (p in 0:9){
    i <- i + 1 # increments the index
    
    # estimates the Arima model
    ADF_est_diff[[i]] <- Arima(diff(diff(y)),
                               xreg = diff(y)[-TT],
                               order = c(p, 0, 0),
                               include.mean = as.logical(const),
                               include.drift = F)
    
    # stores the model and ICs in the matrix
    ic_diff[i,] <- c(const, 0, p, ADF_est_diff[[i]]$aic,
                                  ADF_est_diff[[i]]$bic)
  }
  
  if (const){
    # only add a specification with trend if there is a
    # constant (i.e., exclude no constant with trend)
    for (p in 0:9){
      i <- i + 1 # increments the index
      
      # estimates the Arima model
      ADF_est_diff[[i]] <- Arima(diff(diff(y)),
                                 xreg = diff(y)[-TT],
                                 order = c(p, 0, 0),
                                 include.mean = as.logical(const),
                                 include.drift = T)
      
      # stores the model and ICs in the matrix
      ic_diff[i,] <- c(const, 1, p, ADF_est_diff[[i]]$aic,
                                    ADF_est_diff[[i]]$bic)
    }
  }
}

ic_aic_diff <- ic_diff[order(ic_diff[,4]),][1:10,] # top 10 models according to AIC
ic_bic_diff <- ic_diff[order(ic_diff[,5]),][1:10,] # top 10 models according to BIC

# Note that in this case, the top five specifications ranked by the AIC are the
# same as the top five ranked by the BIC. Since they agree, we chose these five
# to form the adequate set: p = 0, 1, 2 with constant and no trend along with p
# = 0, 1 with both a constant and trend.

# saves the ic info from top 5 BIC
adq_set_diff <- as.matrix(arrange(as.data.frame(
                      ic_bic_diff[1:5,]), const, trend, p))

# saves the index from each of the 5 models.
adq_idx_diff <- match(data.frame(t(adq_set_diff[, 1:3])),
                      data.frame(t(ic_diff[, 1:3])))

# Check the residuals for specifications in the adequate set.
for (i in 1:length(adq_idx_diff)){
  checkresiduals(ADF_est_diff[[adq_idx_diff[i]]])
}

# As no obvious problems jump out from the residuals analysis, we proceed with
# ADF test using specifications in the adequate set.

adf.test(diff(y))

# A unit root is rejected at very small significance level for all
# specifications. Hence, the differenced process is empirically distinguishable
# from an integrated process.


# (e)
#
# Since {y_t} is not empirically distinguishable from an integrated process, but
# {diff(y_t)} is, we conclude that {yt} is not empirically distinguishable from
# an I(1) process. Remember, however, that we did not find evidence of {y_t}
# being I(1) exactly!

# (f)
#
# We will consider models for p = 0,..,3; q = 0,...,3; d = 0, 1
# and either with or without constant and/or trend terms.
# There are 96 models to estimate altogether. We use the Arima function
# in a nested for loop. There are two caveats to deal with.
# 1. The Arima function with d=1 will only specify an intercept
#    when setting the include.drift = T option. Since we want
#    to include a linear growth term (i.e. "t" as a regressor)
#    in the differenced specification, we need to pass it as
#    an exogenous variable to the Arima function through
#    the "xreg" option. However, with d=1, Arima will difference
#    whatever data we pass this way, so we need to cummulatively
#    sum "t" before passing it.
# 2. Some specifications will be so bad that MLE will run into
#    numerical problems and return an error. We want to ignore
#    these specifications in the least disruptive way possible.
#    The way to do it is to embed the Arima function as an
#    argument to the "try" function with the "silent = T" opt.

TT <- length(y)
ARIMA_est <- list()
ic_arima <- matrix( nrow = (2 * 2 + 2) * 4 ^ 2, ncol = 7 )
colnames(ic_arima) <- c("d", "cons", "trend", "p", "q", "aic", "bic")
i <- 0
for (d in 0:1){
  for (const in 0:1){
    for (p in 0:3){
      for (q in 0:3){
        i <- i + 1
        d1 <- as.logical(d)
        c1 <- as.logical(const)
        
        try(silent = F, expr = {
        ARIMA_est[[i]] <- Arima(y, order = c(p, d, q),
                                include.constant = c1)
        
        ic_arima[i,] <- c(d, const, 0, p, q,
                          ARIMA_est[[i]]$aic,
                          ARIMA_est[[i]]$bic)
        })

        if (const){
          # only add a specification with trend if there is a
          # constant (i.e., exclude no constant with trend)
          i <- i + 1
          
          if (d1){
            x <- c(0,cumsum(1:(TT - 1)))
          }
          else{
            x <- NULL
          }

          try(silent = T, expr = {
          ARIMA_est[[i]] <- Arima(y, order = c(p, d, q),
                                  xreg = x,
                                  include.constant = c1,
                                  include.drift = T)
          
          ic_arima[i,] <- c(d, const, 1, p, q,
                            ARIMA_est[[i]]$aic,
                            ARIMA_est[[i]]$bic)
          })
        }
      }
    }
  }
}

ic_aic_arima <- ic_arima[order(ic_arima[,6]),][1:10,] # top 10 models according to AIC
ic_bic_arima <- ic_arima[order(ic_arima[,7]),][1:10,] # top 10 models according to BIC

# find the intersection of AIC and BIC preferred sets
ic_int_arima <- intersect(as.data.frame(ic_aic_arima),
                          as.data.frame(ic_bic_arima))

# Note that the intersection contains only specifications in levels (d=0); take
# a closer look to see if any specifications in differences (d=1) are worth
# considering.
# Looking at the aic and bic preferred tables it's clear that the AIC heavily
# prefers d=0 specifications, whereas the BIC in fact slightly prefers d=1
# specifications, although the ranking is more balanced.
# ARIMA(1,1,0) and ARIMA(2,1,0) -- both with a constant only -- are in the top 3
# of the BIC ranking.
# Taking into consideration also our inference that that GDP is not empirically
# distinguishable from an I(1) process, we will add ARIMA(1,1,0) and
# ARIMA(2,1,0) to the four in the intersecting set.

adq_set_arima <- as.matrix(arrange(as.data.frame(
                           rbind(ic_int_arima,
                                 ic_bic_arima[c(1, 3),])),
                                      d, const, trend, p))
adq_idx_arima <- match(data.frame(t(adq_set_arima[, 1:5])),
                       data.frame(t(ic_arima[, 1:5])))

# Check the residuals for specifications in the adequate set.
for (i in 1:length(adq_idx_arima)){
  checkresiduals(ARIMA_est[[adq_idx_arima[i]]])
}

# All the residuals look fine so our construction of the adequate set is
# complete.

# 2
#
# plot interest rates in levels
plot(dates, r, type = "l", xlab = "Time (Quarters)",
     main = "Federal Funds Rate")

acf(r, plot = T) # acf of interest rate

TT <- length(r)
ADF_est <- list()
ic <- matrix( nrow = 30, ncol = 5 )
colnames(ic) <- c("cons", "trend", "p", "aic", "bic")
i <- 0
for (const in 0:1){
  for (p in 0:9){
    i <- i + 1
    ADF_est[[i]] <- Arima(diff(r), xreg = r[-TT],
                          order = c(p, 0, 0),
                          include.mean = as.logical(const),
                          include.drift = F)
    ic[i,] <- c(const, 0, p, ADF_est[[i]]$aic,
                ADF_est[[i]]$bic)
  }
  
  if (const){
    # only add a specification with trend if there is a
    # constant (i.e., exclude no constant with trend)
    for (p in 0:9){
      i <- i + 1
      ADF_est[[i]] <- Arima(diff(r), xreg = r[-TT],
                            order = c(p, 0, 0),
                            include.mean = as.logical(const),
                            include.drift = T)
      ic[i,] <- c(const, 1, p, ADF_est[[i]]$aic,
                  ADF_est[[i]]$bic)
    }
  }
}

ic_aic <- ic[order(ic[,4]),][1:10,]
ic_bic <- ic[order(ic[,5]),][1:10,]

# find the intersection of AIC and BIC preferred sets
ic_int <- intersect(as.data.frame(ic_aic),
                    as.data.frame(ic_bic))

# The intersecting set has 3 models: 2 with const and p = 6, 7, and 1 without
# constant and 7 lags; no models have a trend.  We may reasonably set this to be
# the adequate set (although other variations are obviously justifiable as
# well).

adq_set <- as.matrix(arrange(as.data.frame(ic_int),
                             const, trend, p))
adq_idx <- match(data.frame(t(adq_set[, 1:3])),
                 data.frame(t(ic[, 1:3])))

for (i in 1:length(adq_idx)){
  checkresiduals(ADF_est[[adq_idx[i]]])
}

# Residuals look ok, although some ACs exceed 95% conf intervals very slightly
# (and at larger lags). For robustness, we might add a few larger p models into
# the adequate set, such as p = 8, 9.

# Here the default option p <= 5 is too small, so we need to explicitly specify
# longer lag lengths.
adf.test(r, nlag = 10)

# Only "Type 1" and "Type 2" specifications are in our adequate set, so we
# ignore the output related to "Type 3". For all specifications in our adequate
# set, null of unit root cannot be rejected.
# Note however, that there are specifications for which the null is indeed
# rejected, such as constant and no trend with p = 5. This is the 9th ranked
# specification by the BIC and outside the top 10 in terms of AIC.
# We can summarise our inference as follows. The process is not empirically
# distinguishable from a unit root process with a drift,  but there is a small
# element of uncertainty in this conclusion.


# plot interest rates chages (differences)
plot(dates[-1], diff(r), type = "l", xlab = "Time (Quarters)",
     main = "Federal Funds Rate")

acf(diff(r))
pacf(diff(r))

TT <- length(diff(r))
ADF_est_diff <- list()
ic_diff <- matrix( nrow = 30, ncol = 5 )
colnames(ic_diff) <- c("cons", "trend", "p", "aic", "bic")
i <- 0
for (const in 0:1){
  for (p in 0:9){
    i <- i + 1
    ADF_est_diff[[i]] <- Arima(diff(diff(r)),
                               xreg = diff(r)[-TT],
                               order = c(p, 0, 0),
                               include.mean = as.logical(const),
                               include.drift = F)
    ic_diff[i,] <- c(const, 0, p, ADF_est_diff[[i]]$aic,
                     ADF_est_diff[[i]]$bic)
  }
  
  if (const){
    # only add a specification with trend if there is a
    # constant (i.e., exclude no constant with trend)
    for (p in 0:9){
      i <- i + 1
      ADF_est_diff[[i]] <- Arima(diff(diff(r)),
                                 xreg = diff(r)[-TT],
                                 order = c(p, 0, 0),
                                 include.mean = as.logical(const),
                                 include.drift = T)
      ic_diff[i,] <- c(const, 1, p, ADF_est_diff[[i]]$aic,
                       ADF_est_diff[[i]]$bic)
    }
  }
}

ic_aic_diff <- ic_diff[order(ic_diff[,4]),][1:10,]
ic_bic_diff <- ic_diff[order(ic_diff[,5]),][1:10,]

# find the intersection of AIC and BIC preferred sets
ic_int_diff <- intersect(as.data.frame(ic_aic_diff),
                         as.data.frame(ic_bic_diff))

# There are only two specifications in the intersecting set: both with no
# constant or trend and p = 6, 7. Overall, both AIC and BIC favour
# specifications without a trend, and AIC generally favours larger lag lengths,
# whereas the BIC yields a more dispersed ranking.

adq_set_diff <- as.matrix(arrange(
                          as.data.frame(ic_int_diff),
                                       const, trend, p))
adq_idx_diff <- match(data.frame(t(adq_set_diff[, 1:3])),
                      data.frame(t(ic_diff[, 1:3])))

# adq_set_diff <- adq_set_diff[4:6,]
# adq_idx_diff <- adq_idx_diff[4:6]


for (i in 1:length(adq_idx_diff)){
  checkresiduals(ADF_est_diff[[adq_idx_diff[i]]])
}

# As with levels, residuals look ok, but with some indication that longer lag
# lengths should be considered.

adf.test(diff(r), nlag = 10)

# A unit root is rejected at very small significance level for all
# specifications. We infer that FFR is not empirically distinguishable from an
# I(1) process.