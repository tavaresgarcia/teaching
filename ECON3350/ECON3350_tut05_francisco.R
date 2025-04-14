# Tutorial 5: Modelling Volatility - I

# Answers by Eric Eisenstat
# https://sites.google.com/view/ericeisenstat

# Extra material by Francisco Tavares Garcia
# https://tavaresgarcia.github.io/


# House cleaning
rm(list = ls()) # clear Environment
if (!is.null(dev.list())) dev.off() # clear Plots
cat("\014") # clear Console

# For this tutorial, we need to load these packages:
# install.packages("forecast")
# install.packages("dplyr")
install.packages("rugarch")

library(forecast)
library(dplyr)
library(rugarch)

# Set working directory. To use the following line, save this script in the same
# folder as the data files.
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load the data in cwb.csv
mydata <- read.delim("cwb.csv", header = TRUE,  sep = ",")

# sore columns into new variables
date <- as.Date(mydata$date, format = "%m/%d/%Y") # dates
y <- mydata$y # price
r <- diff(log(y)) # price change

# 1
#
# share prices are trending upwards
plot(date, y, type = "l", xlab = "", ylab = "share prices")

# 2
#
# Returns look stable with possible heteroscedasticity. Observe that if returns
# are heteroscedastic then so must be the share prices. However, evidence of
# heteroscedasticity may be less obvious in plots of share prices than in plots
# of returns.
plot(date[-1], r, type = "l", xlab = "", ylab = "returns")

# 3
#
ARMA_est <- list() # initiates list of models
ic_arma <- matrix( nrow = 4 * 4, ncol = 4 ) # initiates matrix
colnames(ic_arma) <- c("p", "q", "aic", "bic") # matrix col names
for (p in 0:3){
  for (q in 0:3){
    i <- p * 4 + q + 1 # matrix row
    ARMA_est[[i]] <- Arima(r, order = c(p, 0, q)) # ARMA estimation
    ic_arma[i,] <- c(p, q, ARMA_est[[i]]$aic, ARMA_est[[i]]$bic) # stores AIC & BIC
  }
}

ic_aic_arma <- ic_arma[order(ic_arma[,3]),][1:10,] # top10 AIC
ic_bic_arma <- ic_arma[order(ic_arma[,4]),][1:10,] # top10 BIC

# find the intersection of AIC and BIC preferred sets
ic_int_arma <- intersect(as.data.frame(ic_aic_arma),
                         as.data.frame(ic_bic_arma)) # intersection

# select MA(1), MA(2), AR(1), AR(2), ARMA(1,1)
adq_set_arma <- as.matrix(arrange(as.data.frame(
                                 rbind(ic_int_arma[c(1:3, 6),],
                                  ic_bic_arma[2,])), p, q)) # adq set of models
adq_idx_arma <- match(data.frame(t(adq_set_arma[, 1:2])),
                           data.frame(t(ic_arma[, 1:2]))) # models indexes

# Check the residuals for specifications in the adequate set.
nmods <- length(adq_idx_arma)
for (i in 1:nmods){
  checkresiduals(ARMA_est[[adq_idx_arma[i]]])
}

# All residuals look OK.

# 4
#
# Generate squared residuals and plot them.
e2_arma <- list()
for (i in 1:nmods){
  e2_arma[[i]] <- resid(ARMA_est[[adq_idx_arma[i]]]) ^ 2 # squared residuals
  
  title_p_q <- paste("ARMA(",
                     as.character(adq_set_arma[i, 1]), ", ",
                     as.character(adq_set_arma[i, 2]), ")",
                     sep = "") # generates the plot title
  plot(date[-1], e2_arma[[i]], type = "l",
                 xlab = "", ylab = "squared resid",
                 main = paste("Plot: ", title_p_q)) # plot sqr resid
  
  acf(e2_arma[[i]], xlab = "", ylab = "",
                 main = paste("SACF: ", title_p_q)) # Sample ACF of sqr resid
}

# squared residuals are clearly autocorrelated

# 5
#
# Breusch-Pagan type (LM) test for presence of heteroscedasticity
bptest <- matrix(nrow = 10 * nmods, ncol = 5) # initiates matrix
colnames(bptest) <- c("p", "q", "j", "LM-stat", "p-value") # matrix col names
for (i in 1:nmods){
  e2_i <- as.vector(e2_arma[[i]]) # squared residuals
  f <- formula(e2_i ~ 1) # regression formula
  for (j in 1:10)  {
    # lag lengths in the auto-regression of squared residuals
    k <- 10 * (i - 1) + j # matrix row
    f <- update.formula(f, paste("~ . + lag(e2_i, n = ", j, ")")); # update formula
    bp_reg_j <- lm(f) # runs linear regression
    LM_j <- length(e2_i) * summary(bp_reg_j)$r.squared # Lagrange Multiplier
    p_val_j <- 1 - pchisq(LM_j, df = j) # p-value from LM
    bptest[k,] <- c(adq_set_arma[i, 1:2], j, LM_j, p_val_j) # updates matrix
  }
}

bptest # print table

# the null of homoscedasticity is uniformly rejected at very small significance
# levels

# 6
#
# we consider ARMA variants with p = 0, ..., 2 and q = 0, ..., 2
# + ARCH/GARCH lags from 0 to 2 (each);
# this is a total of 3 ^ 4 = 81 models altogether -- it will take a few minutes
# to estimate them all; also, some specifications will be so bad that numerical
# optimisation will fail, so use the try() function to avoid interruptions.
ARMA_GARCH_est <- list() # initiates list of ARMA_GARCH models
ic_arma_garch <- matrix( nrow = 3 ^ 4, ncol = 6 ) # matrix of AIC BIC
colnames(ic_arma_garch) <- c("pm", "qm", "ph", "qh", "aic", "bic") # matrix col names
i <- 0 # matrix row indicator
for (pm in 0:2){
  for (qm in 0:2){
    for (ph in 0:2){
      for (qh in 0:2){
        i <- i + 1
        ic_arma_garch[i, 1:4] <- c(pm, qm, ph, qh)
        
        if (ph == 0 && qh == 0){
          # for models with constant variance, the ugarchspec and
          # ugarchfit functions do not work well; instead, the
          # documentation advises to use arfimaspec and arfimafit
          ARMA_GARCH_mod <- arfimaspec(
            mean.model = list(armaOrder = c(pm, qm)))
          
          ARMA_GARCH_est[[i]] <- arfimafit(ARMA_GARCH_mod, r)
          
          ic_arma_garch[i,5:6] <- infocriteria(ARMA_GARCH_est[[i]])[1:2]
        }
        else{
          try(silent = T, expr =
                {
                  ARMA_GARCH_mod <- ugarchspec(
                    mean.model = list(armaOrder = c(pm, qm)),
                    variance.model = list(garchOrder = c(ph, qh)))
                  
                  ARMA_GARCH_est[[i]] <- ugarchfit(ARMA_GARCH_mod, r,
                                                   solver = 'hybrid')
                  
                  ic_arma_garch[i,5:6] <- infocriteria(ARMA_GARCH_est[[i]])[1:2]
                })
        }
      }
    }
  }
}

ic_aic_arma_garch <- ic_arma_garch[order(ic_arma_garch[,5]),][1:40,] # top40 AIC
ic_bic_arma_garch <- ic_arma_garch[order(ic_arma_garch[,6]),][1:40,] # top40 BIC

# find the intersection of AIC and BIC preferred sets
ic_int_arma_garch <- intersect(as.data.frame(ic_aic_arma_garch),
                               as.data.frame(ic_bic_arma_garch)) # intersection

# There are A LOT of models that have comparable AIC and BIC measures!
# We select the first 36 in the intersection set.
adq_set_arma_garch <- as.matrix(arrange(as.data.frame(
                      ic_int_arma_garch[1:36,]), pm, qm, ph, qh)) # adequate set
adq_idx_arma_garch <- match(data.frame(t(adq_set_arma_garch[, 1:4])),
                            data.frame(t(ic_arma_garch[, 1:4]))) # index

# Check the residuals for specifications in the adequate set.
# The standard Ljung-Box test is not well-defined for heteroscedastic models,
# but we can just look at the sacf.
nmods <- length(adq_idx_arma_garch) # number of models
sacf_garch <- matrix(nrow = nmods, ncol = 14) # matrix of white noise ACFs
colnames(sacf_garch) <- c("pm", "qm", "ph", "qh", 1:10)
for (i in 1:nmods){
  sacf_garch[i,1:4] <- adq_set_arma_garch[i,1:4] # stores model spec
  sacf_garch[i,5:14] <-
                  acf(ARMA_GARCH_est[[adq_idx_arma_garch[i]]]@fit$z,
                  lag = 10, plot = F)$acf[2:11] # stores acf values lags 1-10
}

# Autocorrelations appear to be relatively small for all models.

# 7
#
# We have already estimated all the models in the adequate set, so the only
# thing left to do is plot the estimated volatilities. These are stored in
# ARMA_GARCH_est[[j]]$fit$var; unfortunately, the rugarch package does not
# provide standard errors or confidence intervals for estimated volatilities :(
for (i in 1:nmods){
  title_p_q <- paste("ARMA(",
                     as.character(adq_set_arma_garch[i, 1]), ", ",
                     as.character(adq_set_arma_garch[i, 2]),
                     ")-GARCH(",
                     as.character(adq_set_arma_garch[i, 3]), ", ",
                     as.character(adq_set_arma_garch[i, 4]), ")",
                     sep = "") # plot title
  plot(date[-1], ARMA_GARCH_est[[adq_idx_arma_garch[i]]]@fit$var,
                     type = "l", xlab = "", ylab = "volatilities",
                     main = title_p_q) # plot estimated variance
}

# We notice some subtle differences across these specifications, but overall the
# estimated volatilities over the sample period follow a similar pattern.
