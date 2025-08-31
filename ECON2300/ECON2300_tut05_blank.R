# Tutorial 5: Hypothesis Tests and Confidence Interval in Multiple Regression

# Answers by Dong-Hyuk Kim
# https://sites.google.com/site/kimdonghyuk000/home
# Extra material by Francisco Tavares Garcia
# https://tavaresgarcia.github.io/

# install.packages("readr")
# install.packages("dplyr")
# install.packages("estimatr")
install.packages("car")
install.packages("texreg") 

# Packages to generate PDF report via RMarkDown
install.packages("rmarkdown")
install.packages("tinytex")
tinytex::install_tinytex()

library(readr)    # package for fast read rectangular data
library(dplyr)    # package for data manipulation
library(estimatr) # package for commonly used estimators with robust SE
library(texreg)   # package for converting R regression output to LaTeX/HTML tables
library(tinytex)  # package for generating PDFs through RMarkDown
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


### E7.1 Using the Birthweight Smoking.csv introduced in E5.3 to answer the
# following questions. To begin, run three regressions:

# Load birthweight_smoking.csv into a variable
BW 
# Attach BW


# (1) birthweight on smoker.
# (2) birthweight on smoker, alcohol, and nprevist.
# (3) birthweight on smoker, alcohol, nprevist, and unmarried.




# To run texreg, please make sure you have RMarkdown installed in your computer
# and some LaTeX processor such as MiKTeX for Windows or MacTeX for Mac.
# An alternative way to print the output from texreg is to create a blank
# document in overleaf.com and paste the code after the \begin{document} line.
texreg(list(reg1, reg2, reg3, reg4), include.ci = F, caption.above = T)

# (a) What is the value of the estimated effect of smoking on birth weight in
# each of the regressions?


# (b) Construct a 95% confidence interval for the effect of smoking on birth
# weight, using each of the regressions.


# (c) Does the coefficient on smoker in regression (1) suffer from omitted
# variable bias? Explain.

# Yes, it seems so. The coefficient falls by roughly 30% in magnitude when 
# additional regressors are added to Model 1. This change is substantively large 
# and large relative to the standard error in Model 1.


# (d) Does the coefficient on smoker in regression (2) suffer from omitted
# variable bias? Explain.

# Yes, it seems so. The coefficient falls by roughly 20% in magnitude when 
# unmarried is added as an additional regression. This change is substantively 
# large and large relative to the standard error in Model 2.


# (e) Consider the coefficient on unmarried in regression (3).
# i. Construct a 95% confidence interval for the coefficient.


# ii. Is the coefficient statistically significant? Explain.

# Yes. The 95% confidence interval does not include zero. Alternatively, the 
# t-statistic is -6.76 which is large in absolute value than the 5% critical 
# value of 1.96.

# iii. Is the magnitude of the coefficient large? Explain.

# Yes. On average, birth weight is 187 grams lower for unmarried mothers.

# iv. A family advocacy group notes that the large coefficient suggests that
# public policies that encourage marriage will lead, on average, to healthier
# babies. Do you agree? [Hint: Review the discussion of control variables in
# Section 7.5. Discuss some of the various factors that unmarried may be
# controlling for and how this affects the interpretation of its coefficient.]

# As the question suggests, unmarried is a control variable that captures the 
# effects of several factors that differ between married and unmarried mothers 
# such as age, education, income, diet and other health factors, and so forth.

# (f) Consider the various other control variables in the data set. Which do you
# think should be included in the regression? Using a table like Table 7.1,
# examine the robustness of the confidence interval you constructed in (b). What
# is a reasonable 95% confidence interval for the effect of smoking on birth
# weight?


# We have added on additional regression in the table that includes age and educ 
# (years of education). The coefficient on smoker is very similar to its value 
# in regression Model 3 so we can consider Model 3 robust enough. 
# See Model 4 in Table 1.

detach(BW)


### E7.2 

# House cleaning
rm(list = ls()) # clear Environment

# Load Earnings_and_Height.csv into a variable
EH
# Attach EH


# (a) Suppose that the mechanism described above is correct. Explain how
# this leads to omitted variable bias in the OLS regression of earnings on
# height. Does the bias led the estimated slope to be too large or too small?
# [Hint: Review Equation (6.1) in SW.]


# From Key Concept 6.1, omitted variable bias arises if X is correlated with the
# omitted variable and the omitted variable is a determinant of the dependent
# variable, Y. The mechanism described in the problem explains why height (X)
# and cognitive ability (the omitted variable) are correlated and why cognitive
# ability is a determinant of earning (Y). The mechanism suggests that height
# and cognitive ability are positively correlated and that cognitive ability has
# a positive effect on earnings. Thus, X will be positively correlated with the
# error leading to a positive bias in the estimated coefficient.


# (b) Focusing first on women only, run two regressions: (1) earnings on height,
# and (2) earnings on height, including lt hs, hs, and some col as control
# variables.

# To create Table 2, you need to execute the command below then copy/paste the 
# output LATEXcodes to the R markdown PDF file.
texreg(list(reg1, reg2, reg3, reg4), include.ci = F, caption.above = T)

# i. Compare the estimated coefficients on height in regressions (1) and (2). Is
# there a large change in the coefficient? Has it changed in a way consistent
# with the cognitive ability explanation? Explain.


# The estimated coefficient on height falls by approximately 75%, from 511 to 
# 135 when the education variables are added as control variables in the 
# regression. This is consistent with positive omitted bias in Model 1.

# ii. Regression (2) omits the control variable college. Why?

# The variable college is perfectly collinear with other education regressors 
# and the constant regressor. 
# To try it yourself uncomment and run the following lines.
# reg <- lm_robust(earnings ~ height + lt_hs + hs + some_col + col,
#                   data = subset(EH, sex != "1:male"), se_type = "stata")
# 
# summary(reg)

# iii. Test the joint null hypothesis that the coefficients on the education
# variables are equal to zero.



# The F-statistic is 578, and the corresponding p-value is ~ 0.00. Therefore, 
# the null hypothesis that the coefficients on the education variables are 
# jointly equal to zero is rejected at the 1% significance level.

# linearHypothesis(reg2, c("some_col = 0"), test = c("F"))
# (-17.652)^2

# iv. Discuss the values of the estimated coefficients on lt hs, hs, and some
# col. (Each of the estimated coefficients is negative, and the coefficient on
# lt hs is more negative than the coefficient on hs, which in turn is more
# negative than the coefficient on some col. Why? What do the coefficients
# measure?)

# The coefficients measure the effect of education on earnings relative to the
# omitted category, which is college. Thus, the estimated coefficient on the
# "Less than High School" regressor implies that workers with less than a high
# school education on average earn $31,858 less per year than a college
# graduate; a worker with a high school education on average earns $20,418 less
# per year than a college graduate; a worker with a some college on average
# earns $12,649 less per year than a college graduate.


# (c) Repeat (b), using data for men


# (i)
# The estimated coefficient on height falls by approximately 50%, from 1307 to 
# 745. This is consistent with positive omitted bias in the simple regression, 
# Model 1.

# (ii)
# The same answer as (b).

# (iii)


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
