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


# E5.1 The file Earnings and Height.csv contains data on earnings, height, and
# other characteristics of a random sample of U.S. workers. See Earnings and
# Height Description.pdf for a detailed description of the data. Carry out the
# following exercises.

# Load Earnings_and_Height.csv into a variable
EH <-

# (a) Run a regression of earnings on height.
# i. Is the estimated slope statistically significant?
#   ii. Construct a 95% confidence interval for the slope coefficient.



# The 95% confidential interval for the slope coefficient is 707.7 +- 1.96 *
# 50.4, or [608.9, 806.5]. This interval does not include B_1 = 0, so the
# estimated slope is significantly different from 0 at the 5% level.
# Alternatively, the t-statistic is 707.7/50.4 +- 14.0, which is greater in
# absolute value than the 5% critical value of 1.96. And finally, the p-value
# for the t-statistic is ~ 0.000, which is smaller than 0.05.


# (b) Repeat (a) for female observations.


# The 95% confidential interval for the slope coefficient is 511.2 +- 1.96 *
# 97.58. This interval does not include B_1(female) = 0, so the estimated slope
# is significantly different from 0 at the 5% level.

# (c) Repeat (a) for male observations.


# The 95% confidential interval for the slope coefficient is 1307 +- 1.96 *
# 98.86. This interval does not include B_1(male) = 0, so the estimated slope is
# significantly different from 0 at the 5% level.

# (d) Test the null hypothesis that the effect of height on earnings is the same
# for men and women.

# B_1(male) - B_1(female) = 1307 - 511.2 = 795.8, SE(B_1(male) -  B_1(female)) = 
# sqrt(SE(B_1(male))^2 + SE(B_1(female))^2) = sqrt(98.86^2 + 97.58^2) = 138.9. 
# The 95% confidence interval = 795.8 +- 1.96 * 138.9 = [523.6, 1068]. 
# This interval does not include B_1(male) - B_1(female) = 0, so the estimated 
# difference in the slopes is significantly different from 0 at the 5% level.


### SW E5.2

# E5.2 Using the dataset Growth.csv, but excluding the data for Malta, run a
# regression of growth on tradeshare.

# Clean Working Environment
rm(list = ls())

# Load Growth.csv into a variable
Growth <- 

# (a) Is the estimated regression slope statistically significant? This is, can
# you reject the null hypothesis H0 : β1 = 0 vs. H1 : β1 ̸= 0 at the 5% or 1%
# significance level?


# The t-statistic for the slope coefficient is t = 1.68/0.87 = 1.94. 
# The t-statistic is less in absolute value than the 5% and 1% critical values 
# (1.96 and 2.58). Therefore, the null hypothesis is not rejected at the 5% or 
# 1% levels.


# (b) What is the p-value associated with the coefficient’s t-statistic?


# Always when T-value is above the critical value, the p-value is below its 
# critical value.

# (c) Construct a 99% confidence interval for β1.

### SW E5.3
  
# E5.3 The data file Birthweight Smoking.csv contains data for a random sample
# of babies in Pennsylvania in 1989. The data include the baby’s birth weight
# together with various characteristics of the mother, including whether she
# smoked during the pregnancy. See Birthweight Smoking Description.pdf for a
# detailed description of the data. You will investigate the relationship
# between birth weight and smoking during pregnancy.

# Clean Working Environment
rm(list = ls())

# Load Growth.csv into a variable
BW <- 
# attach the data to the R search path


# (a) In the sample:
# i. What is the average value of birthweight?
# ii. What is the average value of birthweight for mothers who smoke?
# iii. What is the average value of birthweight for mothers who do not smoke?


# The sample average of birthweight is 3382.93. For smoking mothers, it is 
# 3178.83, while for nonsmoking mothers, it is 3432.06.

# (b) i. Use the data in the sample to estimate the difference in average birth
# weight for smoking and nonsmoking mothers.
# ii. What is the standard error for the estimated difference in (b)i?
# iii. Construct a 95% confidence interval for the difference in the average
# birth weight for smoking and nonsmoking mothers.

# First, we can conduct the test using R-outputs in (a). The estimated
# difference is X_Smokers - X_nonSmokers = 3178.83 - 3432.06 = -253.23. The
# standard error of the difference is SE(X_Smokers - X_nonSmokers) =
# sqrt(SE(X_Smokers)^2 + SE(X_nonSmokers)^2) = sqrt(11.89^2 + 24.04^2) = 26.82.
# The 95% confidence for the difference is -253.23 +- 1.96 * 26.82 = (-305.80,
# -200.66).


# More reference of on:
# Slide 13 from Lecture 3.

# (c) Run a regression of birthweight on the binary variable smoker.
# i. How the estimated slope and intercept are related to your answers in Parts
# (a) and (b)?
# ii. How the SE(^β_1) is related to your answer in (b)ii.
# iii. Construct a 95% confidence interval for the effect of smoking on birth
# weight.



# (i) The intercept is the average birthweight for non-smokers (smoker=0). The 
# slope is the difference between average birthweights for smokers (smoker=1) 
# and non-smokers (smoker=0).
# (ii) They are the same.
# (iii) It is the same as the 95% confidence for the difference, 
# i.e., -253.2 1 1.96 W 26.8 = (-305.9, -200.6).