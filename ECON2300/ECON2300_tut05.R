# Tutorial 5: Hypothesis Tests and Confidence Interval in Multiple Regression

# Answers by Dong-Hyuk Kim
# https://sites.google.com/site/kimdonghyuk000/home
# Extra material by Francisco Tavares Garcia
# https://tavaresgarcia.github.io/

# install.packages("readr")
# install.packages("dplyr")
# install.packages("estimatr")
install.packages("texreg") 
install.packages("car")

library(readr)    # package for fast read rectangular data
library(dplyr)    # package for data manipulation
library(estimatr) # package for commonly used estimators with robust SE
library(texreg)   # package for converting R regression output to LaTeX/HTML tables
library(car)      # package for functions used in "An R companion to Applied Regression"

### SW E7.1

# House cleaning
rm(list = ls()) # clear Environment
if (!is.null(dev.list())) dev.off() # clear Plots
cat("\014") # clear Console

# Set working directory (make sure you edit to your own WD)
# Ex Win: setwd("G:/My Drive/BEcon/TUTOR/ECON2300/04")
# Ex Mac: setwd("/Users/uqdkim7/Dropbox/Teaching/R tutorials/Tutorial04")

# To use the following line: 
# save this file in the same directory as the data files
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load birthweight_smoking.csv into a variable
BW <- read_csv("birthweight_smoking.csv")
attach(BW)

reg1 <- lm_robust(birthweight ~ smoker, se_type = "stata")
reg2 <- lm_robust(birthweight ~ smoker + alcohol + nprevist, se_type = "stata")
reg3 <- lm_robust(birthweight ~ smoker + alcohol + nprevist + unmarried,
                  se_type = "stata")
reg4 <- lm_robust(birthweight ~ smoker + alcohol + nprevist + unmarried + age + 
                    educ, se_type = "stata")

# To run texreg, please make sure you have RMarkdown installed in your computer
# and some LaTeX processor such as MiKTeX for Windows or MacTeX for Mac.
# An alternative way to print the output from texreg is to create a blank
# document in overleaf.com and paste the code after the \begin{document} line.
texreg(list(reg1, reg2, reg3, reg4), include.ci = F, caption.above = T)

## (a)
summary(reg1)
# From Model 1 the smoker coefficient is -253.23.
summary(reg2)
# From Model 2 the smoker coefficient is -217.58.
summary(reg3)
# From Model 3 the smoker coefficient is -175.38.

## (b)
# From Model 1 the 95% CI is -305.8 to -200.7.
confint(reg1)
# From Model 2 the 95% CI is -268.8 to -166.4.
confint(reg2)
# From Model 3 the 95% CI is -228.0 to -122.8.
confint(reg3)

## (c)
# Yes, it seems so. The coefficient falls by roughly 30% in magnitude when 
# additional regressors are added to Model 1. This change is substantively large 
# and large relative to the standard error in Model 1.

## (d)
# Yes, it seems so. The coefficient falls by roughly 20% in magnitude when 
# unmarried is added as an additional regression. This change is substantively 
# large and large relative to the standard error in Model 2.

## (e)
summary(reg3)

# (i) 
# The 95% CI is -241.40 to -132.87 .
confint(reg3)

# (ii) 
# Yes. The 95% confidence interval does not include zero. Alternatively, the 
# t-statistic is -6.76 which is large in absolute value than the 5% critical 
# value of 1.96.

# (iii)
# Yes. On average, birth weight is 187 grams lower for unmarried mothers.

# (iv)
# As the question suggests, unmarried is a control variable that captures the 
# effects of several factors that differ between married and unmarried mothers 
# such as age, education, income, diet and other health factors, and so forth.

## (f)
summary(reg4)
# We have added on additional regression in the table that includes age and educ 
# (years of education). The coefficient on smoker is very similar to its value 
# in regression Model 3 so we can consider Model 3 robust enough. 
# See Model 4 in Table 1.

detach(BW)

### SW E7.2

# House cleaning
rm(list = ls()) # clear Environment
if (!is.null(dev.list())) dev.off() # clear Plots
cat("\014") # clear Console

EH <- read_csv("Earnings_and_Height.csv")
attach(EH)

lm_robust(earnings ~ height, se_type = "stata")

## (a)
# From Key Concept 6.1, omitted variable bias arises if X is correlated with the
# omitted variable and the omitted variable is a determinant of the dependent
# variable, Y. The mechanism described in the problem explains why height (X)
# and cognitive ability (the omitted variable) are correlated and why cognitive
# ability is a determinant of earning (Y). The mechanism suggests that height
# and cognitive ability are positively correlated and that cognitive ability has
# a positive effect on earnings. Thus, X will be positively correlated with the
# error leading to a positive bias in the estimated coefficient.

## (b)

# detach and remove  EH
detach(EH)
rm(EH)

EH <- read_csv("Earnings_and_Height.csv") %>%
  mutate(lt_hs = as.numeric(educ < 12), 
         hs = as.numeric(educ == 12),
         col = as.numeric(educ >= 16), 
         some_col = 1 - lt_hs - hs - col)
attach(EH)

reg1 <- lm_robust(earnings ~ height, 
                  data = subset(EH, sex != "1:male"), se_type = "stata")
reg2 <- lm_robust(earnings ~ height + lt_hs + hs + some_col, 
                  data = subset(EH, sex != "1:male"), se_type = "stata")
reg3 <- lm_robust(earnings ~ height, 
                  data = subset(EH, sex == "1:male"), se_type = "stata")
reg4 <- lm_robust(earnings ~ height + lt_hs + hs + some_col, 
                  data = subset(EH, sex == "1:male"), se_type = "stata")

# To create Table 2, you need to execute the command below then copy/paste the 
# output LATEXcodes to the R markdown PDF file.
texreg(list(reg1, reg2, reg3, reg4), include.ci = F, caption.above = T)

# (i) 
summary(reg1)
summary(reg2)
# The estimated coefficient on height falls by approximately 75%, from 511 to 
# 135 when the education variables are added as control variables in the 
# regression. This is consistent with positive omitted bias in Model 1.

# (ii) 
# The variable college is perfectly collinear with other education regressors 
# and the constant regressor. 
# To try it yourself uncomment and run the following lines.
# reg <- lm_robust(earnings ~ height + lt_hs + hs + some_col + col,
#                   data = subset(EH, sex != "1:male"), se_type = "stata")
# 
# summary(reg)


# (iii) 
linearHypothesis(reg2, c("lt_hs = 0", "hs = 0", "some_col = 0"), test = c("F"))
# The F-statistic is 578, and the corresponding p-value is ~ 0.00. Therefore, 
# the null hypothesis that the coefficients on the education variables are 
# jointly equal to zero is rejected at the 1% significance level.

# linearHypothesis(reg2, c("some_col = 0"), test = c("F"))
# (-17.652)^2

# (iv) 
# The coefficients measure the effect of education on earnings relative to the
# omitted category, which is college. Thus, the estimated coefficient on the
# "Less than High School" regressor implies that workers with less than a high
# school education on average earn $31,858 less per year than a college
# graduate; a worker with a high school education on average earns $20,418 less
# per year than a college graduate; a worker with a some college on average
# earns $12,649 less per year than a college graduate.

## (c)

# (i)
# The estimated coefficient on height falls by approximately 50%, from 1307 to 
# 745. This is consistent with positive omitted bias in the simple regression, 
# Model 1.

# (ii)
# The same answer as (b).

# (iii)
linearHypothesis(reg4, c("lt_hs = 0", "hs = 0", "some_col = 0"), test = c("F"))

# The F-statistic is 500.9, and the corresponding p-value is ~ 0.00. Therefore,
# the null hypothesis that the coefficients on the education variables are
# jointly equal to zero is rejected at the 1% significance level.

# (iv)
# The coefficients measure the effect of education on earnings relative to the
# omitted category, which is college. Thus, the estimated coefficient on the
# "Less than High School" regressor implies that workers with less than a high
# school education on average earn $31,400 less per year than a college
# graduate; a worker with a high school education on average earns $20,346 less
# per year than a college graduate; a worker with a some college on average
# earns $12,611 less per year than a college graduate.

detach(EH)
