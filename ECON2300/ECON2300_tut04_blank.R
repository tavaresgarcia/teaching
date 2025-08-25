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
BW <- 
# attach the data to the R search path

  
# E6.1 Use the Birthweight Smoking.csv introduced in E5.3 to answer the
# following questions.
# (a) Regress birthweight on smoker.


# (b) Regress birthweight on smoker, alcohol, and nprevist.

# i. Using the two conditions for omitted variable bias, explain why the
# exclusion of alcohol and nprevist could lead to omitted variable bias in the
# regression estimated in (a)

# ii. Is the estimated effect of smoking on birth weight substantially different
# from the regression that excludes alcohol and nprevist? Does the regression in
# (a) seem to suffer from omitted variable bias?

# iii. Jane smoked during her pregnancy, did not drink alcohol, and had 8
# prenatal care visits. Use the regression to predict the birth weight of Jane’s
# child.

# iv. Compute R^2 and adjusted_R^2. Why are they so similar?

# (c) An alternative way to control for prenatal visits is to use the binary
# variables tripre0 through tripre3. Regress birthweight on smoker, alcohol,
# tripre0, tripre2, and tripre3.

# i. Why is tripre1 excluded from the regression? What would happen if you
# included it in the regression?

# ii. The estimated coefficient on tripre0 is large and negative. What does this
# coefficient measure? Interpret its value.

# iii. Interpret the value of the estimated coefficients on tripre2 and tripre3

# iv. Does the regression in (c) explain a larger fraction of the variance in
# birth weight than the regression in (b)?

### SW E6.2
  
# Clean Working Environment
rm(list = ls())

# Load birthweight_smoking.csv into a variable
Growth <-
  
# E6.2 Using the dataset Growth.csv, but excluding the data for Malta, run a
# regression of growth on tradeshare.

# (a) Construct a table that shows the sample mean, standard deviation, and
# minimum and maximum values for the series growth, tradeshare, yearsschool,
# oil, rev coups, assassinations, and rgdp60.

# (b) Run a regression of growth on tradeshare, yearsschool, oil, rev coups,
# assassinations, and rgdp60. Use the regression to predict the average annual
# growth rate for a country that has average values for all regressors.

# (c) Repeat (b) but now assume that the country’s value for tradeshare is one
# standard deviation above the mean.

# (d) Why is oil omitted from the regression? What would happen if it were
# included?
