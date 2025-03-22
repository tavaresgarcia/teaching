# Tutorial 4: Linear Regression with Multiple Regressors

# Answers by Dong-Hyuk Kim
# https://sites.google.com/site/kimdonghyuk000/home

# Extra material by Francisco Tavares Garcia
# https://tavaresgarcia.github.io/

# install.packages("readr")
# install.packages("dplyr")
# install.packages("estimatr")
# install.packages("psych") 
library(readr)    # package for fast read rectangular data
library(dplyr)    # package for data manipulation
library(estimatr) # package for commonly used estimators with robust SE
library(psych)    # package containing many functions useful for data analysis

### SW E6.1

# House cleaning
rm(list = ls()) # clear Environment
if (!is.null(dev.list())) dev.off() # clear Plots
cat("\014") # clear Console

# Set working directory (make sure you edit to your own WD)
# Ex Win: setwd("G:/My Drive/BEcon/TUTOR/ECON2300/02")
# Ex Mac: setwd("/Users/uqdkim7/Dropbox/Teaching/R tutorials/Tutorial02")

# To use the following line: save this file in the same folder as the data files
# install.packages("rstudioapi")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load birthweight_smoking.csv into a variable
BW <- read_csv("birthweight_smoking.csv")
# attach the data to the R search path
attach(BW)

## (a)
# regression of birthweight on smoker
reg1 <- lm_robust(birthweight ~ smoker, se_type = "stata")
summary(reg1)

# The estimated effect of smoking on birthweight is -253.2 grams.

## (b)
# regression of birthweight on smoker, alcohol and nprevist
reg2 <- lm_robust(birthweight ~ smoker + alcohol + nprevist, se_type = "stata")
summary(reg2)

# (i)
# Smoking may be correlated with both alcohol and the number of prenatal doctor 
# visits. Moreover, both alcohol consumption and the number of doctor visits may 
# have their own independent effects on birth weight.

# (ii)
# The estimate is somewhat smaller: it has fallen to 217.6 grams from 253.2 
# grams, so the regression in (a) may suffer from omitted variable bias.

# (iii)
predict(reg2, newdata = data.frame(smoker = 1, alcohol = 0, nprevist = 8))
# birthweight_hat = 3051.25 - 217.58 x 1 - 30.49 x 0 + 34.07 x 8 = 3106.23

# (vi)
# Multiple R-squared:  0.07285 ,	Adjusted R-squared:  0.07192 
# They are nearly identical because the sample size is very large (n = 3000).
# slide 21 from Lecture 4.
# https://www.econometrics-with-r.org/6.3-mofimr.html
(1-(1-reg2$r.squared)*(reg2$nobs-1)/(reg2$nobs-3-1))


## (c)
# regression of birthweight on smoker, alcohol and nprevist
reg3 <- lm_robust(
  birthweight ~ smoker + alcohol + tripre0 + tripre2 + tripre3,
  se_type = "stata")
summary(reg3)

# (i)
# tripre1 is omitted to avoid perfect multicollinearity. (tripre0 + tripre1 + 
# tripre2 + tripre3 = 1, the value of the "constant" regressor that determines 
# the intercept). The regression would not run, or the software will report 
# results from an arbitrary normalization if tripre0, tripre1, tripre2, tripre3,
# and the constant term all included in the regression.

# Extra: if you want to check what happens:
reg <- lm_robust(
  birthweight ~ smoker + alcohol + tripre0 + tripre1 + tripre2 + tripre3,
  se_type = "stata")
summary(reg)

# (ii)
# Babies born to women who had no prenatal doctor visits (tripre0 = 1) had 
# birth weights that on average were 698.0 grams (~ 1.5 lbs) lower than babies 
# from others who saw a doctor during the first trimester (tripre1 = 1).

# (iii)
# Babies born to women whose first doctor visit was during the second trimester 
# (tripre2 = 1) had birth weights that on average were 100.8 grams (~ 0.2 lbs) 
# lower than babies from others who saw a doctor during the first trimester 
# (tripre1 = 1). Babies born to women whose first doctor visit was during the 
# third trimester (tripre3 = 1) had birth weights that on average were 137 grams 
# (~ 0.3 lbs) lower than babies from others who saw a doctor during the first 
# trimester (tripre1 = 1).

# (iv)
# Based on R^2 values, no. The R^2 for the regression in (c) is 0.046 (4.6%)
# variance in birth weight is explained by the regression model), while the R^2
# for the regression in (b) is 0.073 (7.3%).

detach(BW)

### SW E6.2

# Clean Working Environment
rm(list = ls())

# Load birthweight_smoking.csv into a variable
Growth <- read_csv("Growth.csv") %>%
  filter(country_name != "Malta")
attach(Growth)

# The pipe operator "%>%"
# https://towardsdatascience.com/an-introduction-to-the-pipe-in-r-823090760d64

## (a)
describe(data.frame(growth, tradeshare, yearsschool, oil, rev_coups, 
                    assasinations, rgdp60), fast = T)

## (b)
reg4 <- lm_robust(growth ~ tradeshare + yearsschool + oil + rev_coups +
                    assasinations + rgdp60, se_type = "stata")
summary(reg4)

predict(reg4, newdata = data.frame(tradeshare = mean(tradeshare),
                                   yearsschool = mean(yearsschool),
                                   oil = mean(oil),
                                   rev_coups = mean(rev_coups),
                                   assasinations = mean(assasinations),
                                   rgdp60 = mean(rgdp60)))
# Use the sample averages for the regressors in (a) above. The predicted growth 
# rate at the mean values for all regressors is 1.87.

## (c)
predict(reg4, newdata = data.frame(tradeshare = mean(tradeshare) + sd(tradeshare),
                                   yearsschool = mean(yearsschool),
                                   oil = mean(oil),
                                   rev_coups = mean(rev_coups),
                                   assasinations = mean(assasinations),
                                   rgdp60 = mean(rgdp60)))
# Use the standard deviation in (a) above. The resulting predicted value is
# 2.18.

## (d)
# The variable "oil" takes on the value of 0 for all 64 countries in the sample.
# This would generate perfect multicollinearity, since oil_i + 1 = 1, and hence 
# the variable is a linear combination of one of the regressors, namely the 
# constant (intercept).

detach(Growth)