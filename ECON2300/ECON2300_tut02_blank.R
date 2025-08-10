# Tutorial 2: Linear Regression with One Regressor

# Answers by Dong-Hyuk Kim
# https://sites.google.com/site/kimdonghyuk000/home

# Extra material by Francisco Tavares Garcia
# https://tavaresgarcia.github.io/

# load non-built-in R packages (you should install them first!)

install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("estimatr")
install.packages("Hmisc")

library(readr)    # package for fast read rectangular data
library(dplyr)    # package for data manipulation
library(ggplot2)  # package for elegant data visualisation
library(estimatr) # package for commonly used estimators with robust SE
library(Hmisc)    # package for statistics functions

# House cleaning
rm(list = ls()) # clear Environment
if (!is.null(dev.list())) dev.off() # clear Plots
cat("\014") # clear Console

### SW E4.1

# Set working directory. To use the following line, save this script in the same
# folder as the data files.
install.packages("rstudioapi")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


## E4.1 The file Growth.csv contains data on average growth rates from 1960
# through 1995 for 65 countries, along with variables that are potentially
# related to growth. A detailed description is given in Growth Description.pdf.
# You will investigate the relationship between growth and trade.

# load csv data
Growth <- 


# (a) Construct a scatterplot of average annual growth rate, growth, on the
# average trade share, tradeshare. Does there appear to be a relationship
# between the variables?


# (b) One country, Malta, has a trade share much larger than the other
# countries. Find Malta on the scatterplot. Does Malta look like an outlier?


# (c) Using all observations, run a regression of growth on tradeshare. What is
# the estimated slope? What is the estimated intercept? Use the regression to
# predict the growth rate for a country with a trade share of 0.5 and with a
# trade equal to 1.0.


# (d) Estimate the same regression, excluding the data from Malta. Answer the same questions in (c).


# (e) Plot the estimated regression functions from (c) and (d). Using the
# scatterplot in (a), explain why the regression function that includes Malta is
# steeper than the regression function that excludes Malta.


# (f) Where is Malta? Why is the Malta trade share so large? Should Malta be
# included or excluded from the analysis?


## E4.2 The file Earnings and Height.csv contains data on earnings, height, and
# other characteristics of a random sample of U.S. workers. See Earnings and
# Height Description.pdf for a detailed description of the data. You will
# investigate the relationship between earnings and height.

# (a) What is the median value of height in the sample?


# (b) i. Estimate average earnings for workers whose height is at most 67 inches.
# ii. Estimate average earnings for workers whose height is greater than 67
# inches.
# iii. On average, do taller workers earn more than shorter workers? How much
# more? What is a 95% confidence interval for the difference in average
# earnings?


# (c) Construct a scatterplot of annual earnings, earnings, on height, height.
# Notice that the points on the plot fall along horizontal lines. (There are
# only 23 distinct values of earnings). Why? (Hint: Carefully read the detailed
# data description.)


# (d) Run a regression of earnings on height.
# i. What is the estimated slope?
# ii. Use the estimated regression to predict earnings for a worker who is 67
# inches tall, for a worker who is 70 inches tall, and for a worker who is 65
# inches tall.


# (e) Suppose heights were measured in centimetres instead of inches. Answer the
# following questions about the earnings on height (in cm) regression.
# i. What is the estimated slope of the regression?
# ii. What is the estimated intercept?
# iii. What is the R2?
# iv. What is the standard error of the regression?


# (f) Run a regression of earnings on height using data for female workers only.
# i. What is the estimated slope? 
# ii. A randomly selected woman is 1 inch taller than the average woman in the
# sample. Would you predict her earnings to be higher or lower than the average
# earnings for women in the sample? By how much?


# (g) Repeat (f) for male workers.

# (h) Do you think that height is uncorrelated with other factors that cause
# earning? That is, do you think that the regression error term, say ui, has a
# conditional mean of zero, given height (Xi)?
#   