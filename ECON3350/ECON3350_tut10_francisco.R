# Tutorial 10: Multivariate Processes - I/II

# Answers by Eric Eisenstat
# https://sites.google.com/view/ericeisenstat

# Extra material by Francisco Tavares Garcia
# https://tavaresgarcia.github.io/


# House cleaning
rm(list = ls()) # clear Environment
if (!is.null(dev.list())) dev.off() # clear Plots
cat("\014") # clear Console

# For this tutorial, we need to load these packages:
# install.packages("zoo")
install.packages("vars")
install.packages("pracma")

library(zoo)
library(vars)
library(pracma)

# Set working directory. To use the following line, save this script in the same
# folder as the data files.
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load the data in money_dem.csv
mydata <- read.delim("money_dem.csv", header = TRUE,  sep = ",")

# create a variables for the vector and dates
date <- as.yearqtr(mydata$DATE)
lrgdp <- log(mydata$RGDP)
price <- mydata$GDP/mydata$RGDP
lrm2 <- log(mydata$M2) - log(price) # log(M2/price)
rs <- mydata$TB3mo

# bind variables into our vector X
x <- cbind(lrgdp, lrm2, rs) 

# plot the three generated samples
plot(date, lrgdp, type = "l", xlab = "", ylab = "Real GDP")
plot(date, lrm2, type = "l", xlab = "", ylab = "Money Supply")
plot(date, rs, type = "l", xlab = "", ylab = "Short-Term Interest Rate")

# 1
#
# (a)
#
# We will consider VARs with p= 1,...,20.
# Note: the VAR command does not allow estimating a VAR(0), so we will not worry
# about this specification.
VAR_est <- list() # list of VARs
ic_var <- matrix(nrow = 20,  ncol = 3) # matrix with information criteria
colnames(ic_var) <- c("p", "aic", "bic") # column names
for (p in 1:20){ # estimation of VARs
  VAR_est[[p]] <- VAR(x, p)
  ic_var[p,] <- c(p, AIC(VAR_est[[p]]), BIC(VAR_est[[p]]))
}

ic_aic_var <- ic_var[order(ic_var[,2]),] # Sorting all by AIC
ic_bic_var <- ic_var[order(ic_var[,3]),] # Sorting all by BIC

# AIC and BIC clearly agree on p = 2, 3, 4 as the top 3; the rest are quite a
# bit inferior, so we will go with these.
adq_set_var <- as.matrix(ic_var[2:4,]) # adding models 2 to 4 to adequate set
adq_idx_var <- c(2:4) # setting index from 2 to 4

# Check the residuals: the vars package provides a few different tests through
# the serial.test function; we will use the LM test by setting type = "BG", but
# other tests are just as valid.
nmods <- length(adq_idx_var)
for (i in 1:nmods){
  p <- adq_idx_var[i]
  print(paste0("Checking VAR(", p, ")"))
  print(serial.test(VAR_est[[p]], type = "BG"))
}

# Autocorrelations appear to be quite high for all three models.
# This is a concern, so we proceed by checking all 20 VAR specifications.
for (p in 1:20){
  print(paste0("Checking VAR(", p, ")"))
  print(serial.test(VAR_est[[p]], type = "BG"))
}

# White noise residuals is rejected for p <= 7, which indicates that we need to
# consider higher lag orders.
# For p >= 8, both AIC and BIC clearly agree that lower is better.
# We will proceed with p = 8, 9, 10 as the adequate set.
adq_set_var <- as.matrix(ic_var[8:10,])
adq_idx_var <- c(8:10)

# (b)
#
# intercept and slope coefficients
nmods <- length(adq_idx_var)
for (i in 1:nmods){
  p <- adq_idx_var[i]
  print(paste0("VAR(", p, ") has ", 3 * (1 + 3 * p), " coefficients."))
}

# (c)
#
# The vars package provides a handy function "roots" to ascertain the stability
# of the estimated VARs.
# Unfortunately, it does not provide confidence intervals for the estimated
# roots.
# Also, be careful not to use the function "stability" for this purpose -- that
# function searches for potential structural breaks.
nmods <- length(adq_idx_var)
for (i in 1:nmods){
  p <- adq_idx_var[i]
  print(paste0("VAR(", p, "): Maximum absolute eigenvalue is ", 
               max(vars::roots(VAR_est[[p]]))))
}

# We have at least one root that is approximately 1 in absolute value -- i.e.,
# close to a unit root. We should be aware that forecasts will be less reliable
# at longer horizons. Do we want to consider imposing a restriction of an exact
# unit root? We will explore this in Tutorial 12!

# (d)
#
# Use the predict function to generate forecasts; plot will then automatically
# produce 95% predictive intervals (forecast uncertainty only, not estimation
# uncertainty).
hrz = 12 # forecast horizon in quarters
VAR_fcst <- list() # list of forecasts
xlim <- c(length(date) - 3 * hrz, length(date) + hrz) # x label limits
ylim <- c(lrgdp[xlim[1]], max(lrgdp) + 0.2) # y label limits
for (i in 1:nmods){
  p <- adq_idx_var[i]
  VAR_fcst[[i]] <- predict(VAR_est[[p]], n.ahead = hrz)
  plot(VAR_fcst[[i]], names = "lrgdp", xlim = xlim, ylim = ylim,
       main = paste("Forecast for Log Real GDP - VAR(", p, ")", sep = ""),
       xlab = "Horizon", ylab = "log(RGDP)", xaxt = "n")
  axis(1, at = c(0, 0.5, 0.95), labels = c(1992, 1998, 2004))
}



# Forecasts for the three VAR specifications look qualitatively similar. Also,
# to note is that the near unit root estimated for each VAR does not seem to be
# of any consequence in terms of forecasts up to 12 quarters ahead -- they are
# still quite accurate (although these confidence intervals DO NOT take into
# account estimate uncertainty!)

# 2
#
# (a)
#
# We fix p = 8 for this question.
orders <- perms(1:3)
vnames <- c("lrgdp", "lrm2", "rs")
for (i in 1:3){
  for (j in 1:3){
    for (k in 1:nrow(orders)){
      title_i_j_k <- paste0("Response of ", vnames[i], " to a shock in ",
                            vnames[j], "; x = (", paste0(vnames[orders[k,]],
                                                         collapse = ", "), ")'")
      
      irf_i_j_k <- irf(VAR(x[,orders[k,]], 8), n.ahead = 40, 
                       response = vnames[i], impulse = vnames[j], 
                       boot = TRUE)
      
      plot(irf_i_j_k, main = title_i_j_k)
      cat("\r", title_i_j_k, "  ", sep = "")
    }
  }
}

# See "solutions" pdf for a possible interpretation.

# (b)
#
# For this part we also fix the order to be (lgdp, lrm2, rs).
# The fevd function only computes decompositions at the estimated values and
# does not provide confidence intervals.
FEVD_est <- fevd(VAR_est[[8]], n.ahead = 40)
plot(FEVD_est, mar = c(2,1,2,1), oma = c(0,1,0,1))

# See "solutions" pdf for a possible interpretation.

# (c)
#
# The function causality in the vars package is very handy to implement Granger
# causality tests
for (i in 1:3){
  ctest_i <- causality(VAR_est[[8]], cause = vnames[i])
  print(ctest_i$Granger)
}

# We confirm that GDP Granger-causes either real money supply or interest rates
# (or both) at the 1% significance level.
# Likewise, interest rates are confirmed to Granger-cause either GDP or money
# supply (or both) at very small significance level.
# However, we DO NOT have sufficient evidence to confirm that money supply DOES
# NOT Granger-cause GDP and interest rates.