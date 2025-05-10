# Tutorial 9: Regression with a Binary Dependent Variable

# Answers by Dong-Hyuk Kim
# https://sites.google.com/site/kimdonghyuk000/home

# Extra material by Francisco Tavares Garcia
# https://tavaresgarcia.github.io/

# install.packages("readr")
# install.packages("dplyr")
# install.packages("estimatr")
# install.packages("texreg") 
# install.packages("car")

library(readr)    # package for fast read rectangular data
library(dplyr)    # package for data manipulation
library(estimatr) # package for commonly used estimators with robust SE
library(texreg)   # package for converting R regression output to LaTeX/HTML tables
library(car)      # package for functions used in "An R Companion to Applied Regression"


### SW E11.2

# Clean Working Environment
rm(list = ls())

# Set working directory (make sure you edit to your own WD)
# Ex Win: setwd("G:/My Drive/BEcon/TUTOR/ECON2300/04")
# Ex Mac: setwd("/Users/uqdkim7/Dropbox/Teaching/R tutorials/Tutorial04")

# To use the following line: 
# save this file in the same directory as the data files
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

smoking <- 
attach(smoking)

lpm1 <- 
lpm2 <- 
  
# fit Probit model
probit <- 

# fit Logit model
logit <- 


# To run texreg, please make sure you have RMarkdown installed in your computer
# and some LaTeX processor such as MiKTeX for Windows or MacTeX for Mac.
# An alternative way to print the output from texreg is to create a blank
# document in overleaf.com and paste the code after the \begin{document} line.

texreg(list(lpm1, lpm2, probit, logit), include.ci = F, caption.above = T,
       custom.model.names = c("LPM (1)", "LPM (2)", "Probit", "Logit"))

# (a) Estimate the probability of smoking for (i) all workers, (ii) workers
# affected by workplace smoking bans, and (iii) workers not affected by
# workplace smoking bans.


# (b) What is the difference in the probability of smoking between workers
# affected by a workplace smoking ban and workers not affected by a workplace
# smoking ban? Use a linear probability model to determine whether this
# difference is statistically significant.


# (c) Estimate a linear probability model with smoker as the dependent variable
# and the following regressors: smkban, female, age, age2, hsdrop, hsgrad,
# colsome, colgrad, black, and hispanic. Compare the estimated effect of a
# smoking ban from this regression with your answer from (b). Suggest a reason,
# based on the substance of this regression, explaining the change in the
# estimated effect of a smoking ban between (b) and (c).


# (d) Test the hypothesis that the coefficient on smkban is zero in the
# population version of the regression in (c) against the alternative that it is
# nonzero, at the 5% significance level.


# (e) Test the hypothesis that the probability of smoking does not depend on the
# level of education in the regression in (c). Does the probability of smoking
# increase or decrease with the level of education?


# (f) Repeat (c)–(e) using a probit model.


# (g) Repeat (c)–(e) using a logit model.


# (h) i. Mr. A is white, non-Hispanic, 20 years old, and a high school dropout.
# Using the probit regression and assuming that Mr. A is not subject to a
# workplace smoking ban, calculate the probability that Mr. A smokes. Carry out
# the calculation again, assuming that he is subject to a workplace smoking ban.
# What is the effect of the smoking ban on the probability of smoking?


#   ii. Repeat (i) for Ms. B, a female, black, 40-year-old college graduate.


# iii. Repeat (i) – (ii) using the linear probability model.


# iv. Repeat (i) – (ii) using the logit model.


# v. Based on your answers to (i) – (iv), do the logit, probit, and linear
# probability models differ? If they so, which results make most sense? Are the
# estimated effects large in a real world sense?

# To calculate the probabilities, take the estimation results from the probit
# model to calculate z_hat = x^T*beta_hat and calculate the cumulative standard
# normal distribution at i.e., Pr(smoke) = phi(z_hat). Do a similar calculation
# for the logit and linear probability models.
#
# The linear probability model assumes that the marginal impact of workplace
# smoking bans on the probability of an individual smoking is not dependent on
# the other characteristics of the individual. On the other hand, the probit and
# logit models' predicted marginal impact of workplace smoking bans on the
# probability of smoking depends on individual characteristics. Therefore, in
# the linear probability model, the marginal impact of workplace smoking bans is
# the same for Mr. A and Mr. B, although their profiles would suggest that Mr. A
# has a higher probability of smoking based on his characteristics. Looking at
# the probit results, the marginal impact of workplace smoking bans on the odds
# of smoking are different for Mr. A and Ms. B, because their different
# characteristics are incorporated into the impact of the laws on the
# probability of smoking. The same is true of the logit model. In this sense the
# probit and logit model are likely more appropriate, and they give very similar
# answers.
#
# Are the impacts of workplace smoking bans "large" in a real-world sense? Most
# people might believe the impacts are large. For example, for people with
# characteristics like Mr. A the reduction on the probability is greater than 6%
# (from the probit and logit models). Applied to a large number of people, this
# translates into a 6% reduction in the number of people smoking.
