# Tutorial 3: Linear Regression with a Single Regressor
# Hypothesis Tests and Confidence Intervals 

# Answers by Dong-Hyuk Kim
# https://sites.google.com/site/kimdonghyuk000/home
# Extra material by Francisco Tavares Garcia
# https://tavaresgarcia.github.io/

# install.packages("readr")
# install.packages("dplyr")
# install.packages("estimatr")
install.packages("psych") 
# If you find the warning "ERROR: failed to lock directory '...\R-4.1.3\library'
# for modifying." delete the folder '00LOCK' from '...\R-4.1.3\library\' and try 
# again.

library(readr)    # package for fast read rectangular data
library(dplyr)    # package for data manipulation
library(estimatr) # package for commonly used estimators with robust SE
library(psych)    # package containing many functions useful for data analysis


### SW E5.1
# Clean Working Environment
rm(list = ls())

# Set working directory (make sure you edit to your own WD)
# Ex Win: setwd("G:/My Drive/BEcon/TUTOR/ECON2300/02")
# Ex Mac: setwd("/Users/uqdkim7/Dropbox/Teaching/R tutorials/Tutorial02")

# To use the following line: save this file in the same folder as the data files
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load Earnings_and_Height.csv into a variable
EH <- read_csv("Earnings_and_Height.csv")



## (a)

# Regression of earnings on height
reg1 <- lm_robust(earnings ~ height, data = EH, se_type = "stata")

# Print a summary of the regression
summary(reg1)

# The 95% confidential interval for the slope coefficient is 707.7 +- 1.96 *
# 50.4, or [608.9, 806.5]. This interval does not include B_1 = 0, so the
# estimated slope is significantly different from 0 at the 5% level.
# Alternatively, the t-statistic is 707.7/50.4 +- 14.0, which is greater in
# absolute value than the 5% critical value of 1.96. And finally, the p-value
# for the t-statistic is ~ 0.000, which is smaller than 0.05.

# plot(EH$height, EH$earnings, pch = 19, col = "blue")
# abline(reg1, col = "red", lwd = 3)


## (b)

# Regression of earnings on height for female observations
reg2 <- lm_robust(earnings ~ height, 
                  data = subset(EH, sex != "1:male"), 
                  se_type = "stata")
# Print a summary of the regression
summary(reg2)

# The 95% confidential interval for the slope coefficient is 511.2 +- 1.96 *
# 97.58. This interval does not include B_1(female) = 0, so the estimated slope
# is significantly different from 0 at the 5% level.

## (c)

# Regression of earnings on height for male observations
reg3 <- lm_robust(earnings ~ height, 
                  data = subset(EH, sex == "1:male"), 
                  se_type = "stata")
# Print a summary of the regression
summary(reg3)

# The 95% confidential interval for the slope coefficient is 1307 +- 1.96 *
# 98.86. This interval does not include B_1(male) = 0, so the estimated slope is
# significantly different from 0 at the 5% level.

## (d)

# B_1(male) - B_1(female) = 1307 - 511.2 = 795.8, SE(B_1(male) -  B_1(female)) = 
# sqrt(SE(B_1(male))^2 + SE(B_1(female))^2) = sqrt(98.86^2 + 97.58^2) = 138.9. 
# The 95% confidence interval = 795.8 +- 1.96 * 138.9 = [523.6, 1068]. 
# This interval does not include B_1(male) - B_1(female) = 0, so the estimated 
# difference in the slopes is significantly different from 0 at the 5% level.


### SW E5.2

# Clean Working Environment
rm(list = ls())

# Load Growth.csv into a variable
Growth <- read_csv("Growth.csv")

## (a)

# Regression of growth on tradeshare for "non-Malta" observations
reg4 <- lm_robust(growth ~ tradeshare, 
                  data = subset(Growth, country_name != "Malta"),
                  se_type = "stata")
# Print a summary of the regression
summary(reg4)

# The t-statistic for the slope coefficient is t = 1.68/0.87 = 1.94. 
# The t-statistic is less in absolute value than the 5% and 1% critical values 
# (1.96 and 2.58). Therefore, the null hypothesis is not rejected at the 5% or 
# 1% levels.

## (b)

# The p-value is 0.057. The summary already shows you the p-value, but you can
# access it as a "column" of your regression variable using the code below:
reg4$p.value[2]
reg4$statistic[2]

# Always when T-value is above the critical value, the p-value is below its 
# critical value.

## (c)

# The 99% confidence interval is 1.68 +- 2.58 * 0.87.
1.68 - 2.58 * 0.87
1.68 + 2.58 * 0.87

# Alternatively you can use the coefficients inside your regression variable:
reg4$coefficients[2] - 2.58 * reg4$std.error[2]
reg4$coefficients[2] + 2.58 * reg4$std.error[2]
# Note the small difference when you use more precise not rounded values.

# You can confirm your calculation using the function confint():
confint <- confint(reg4, level = 0.99)
confint
# Note another small difference here as confint() will use ~2.657 as t-crit
# instead of our 2.58 (z-crit). Run the code below to check the value used:
# (confint[2,2] - reg4$coefficients[2])/reg4$std.error[2]

# You can check the t-table below to confirm the value (df = 62, alpha = 0.995)
# https://www.itl.nist.gov/div898/handbook/eda/section3/eda3672.htm


### SW E5.3

# Clean Working Environment
rm(list = ls())

# Load Growth.csv into a variable
BW <- read_csv("birthweight_smoking.csv")
# attach the data to the R search path
attach(BW)

## (a)

# descriptive statistics of birthweight
describe(birthweight)

# apply the function decribe() over different subsets of birthweight values 
# split by different values of smoker variable. 
# i.e. describe all birthweight for smoker = 1, describe ... for smoker  = 0.
tapply(birthweight, smoker, describe)

# The sample average of birthweight is 3382.93. For smoking mothers, it is 
# 3178.83, while for nonsmoking mothers, it is 3432.06.

## (b)

# First, we can conduct the test using R-outputs in (a). The estimated
# difference is X_Smokers - X_nonSmokers = 3178.83 - 3432.06 = -253.23. The
# standard error of the difference is SE(X_Smokers - X_nonSmokers) =
# sqrt(SE(X_Smokers)^2 + SE(X_nonSmokers)^2) = sqrt(11.89^2 + 24.04^2) = 26.82.
# The 95% confidence for the difference is -253.23 +- 1.96 * 26.82 = (-305.80,
# -200.66).

# Second, we can use t.test
t.test(birthweight[smoker == 1], birthweight[smoker == 0])

# Extra code for answers:
# (i)
mean_diff <- mean(subset(BW, smoker == 1)$birthweight) - 
  mean(subset(BW, smoker == 0)$birthweight)
mean_diff
# (ii)
se_1 <- describe(subset(BW, smoker == 1)$birthweight)$se
se_1
se_0 <- describe(subset(BW, smoker == 0)$birthweight)$se
se_0
se_diff <- sqrt(se_1^2 + se_0^2)
se_diff
# (iii)
mean_diff - 1.96 * se_diff
mean_diff + 1.96 * se_diff

# More reference of on:
# Slide 13 from Lecture 3.

## (c)

reg5 <- lm_robust(birthweight ~ smoker, se_type = "stata")
summary(reg5)

# (i) The intercept is the average birthweight for non-smokers (smoker=0). The 
# slope is the difference between average birthweights for smokers (smoker=1) 
# and non-smokers (smoker=0).
# (ii) They are the same.
# (iii) It is the same as the 95% confidence for the difference, 
# i.e., -253.2 1 1.96 W 26.8 = (-305.9, -200.6).

# plot(smoker, birthweight, pch = 19, col = "blue")
# abline(reg5, col = "red", lwd = 3)