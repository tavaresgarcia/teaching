# Tutorial 11: Experiments and Quasi-Experiments

# Answers by Dong-Hyuk Kim
# https://sites.google.com/site/kimdonghyuk000/home

# Extra material by Francisco Tavares Garcia
# https://tavaresgarcia.github.io/

# install.packages("readr")
# install.packages("dplyr")
# install.packages("estimatr")
# install.packages("texreg") 
# install.packages("AER")
# install.packages("broom")

library(readr)    # package for fast read rectangular data
library(dplyr)    # package for data manipulation
library(estimatr) # package for commonly used estimators with robust SE
library(texreg)   # package for converting R regression output to LaTeX/HTML tables
library(AER)      # package for functions used in "Applied Econometrics using R"
library(broom)    # package converting statistical analysis objects into Tidy Tibbles

### SW E13.1


# House cleaning
rm(list = ls()) # clear Environment
if (!is.null(dev.list())) dev.off() # clear Plots
cat("\014") # clear Console

# Set working directory
# Ex Win: setwd("G:/My Drive/BEcon/TUTOR/ECON2300/11")
# Ex Mac: setwd("/Users/uqdkim7/Dropbox/Teaching/R tutorials/Tutorial11")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load data
Names <- 
# attach data
attach(Names)


## (a)
reg1  <- 
summary(reg1)
# From (1) in the table, the call-back rate for whites is 0.0965 and the
# call-back rate for blacks is 0.0965-0.032 = 0.0645. The difference is -0.032
# is statistically significant at the 1% level (t-statistic = -4.11). This
# result implies that 9.65% of resumes with white-sounding names generated a
# call back. Only 6.45% of resumes with black-sounding names generated a call
# back. The difference is large in both statistical and real-world sense.

## (b)
reg2  <- 
summary(reg2)
# From (2) in the table, the call-back rate for male blacks 0.0965 - 0.0382 =
# 0.0583, and for female blacks is 0.0965 - 0.0382 + 0.008 = 0.0663. The
# difference is 0.008, which is not significant at the 5% level (t-statistic =
# 0.69).

## (c)
reg3  <- 
summary(reg3)

reg4  <- 
summary(reg4)
# From (3) in the table, the call-back rate for low-quality resumes is 0.0734
# and the call-back rate for high-quality resumes is 0.0734 + 0.0141 = 0.0875.
# The difference is 0.0141, which is not significant at the 5% level, but is at
# the 10% level (p-value = 0.071). From (4) the (high-quality)-(low-quality)
# difference for whites is 0.0229 and for blacks is 0.0229 - 0.0178 = 0.0051;
# the black-white difference is -0.0178 which is not statistically significant
# at the 5% level (t-statistic = -1.14).


# To run texreg, please make sure you have RMarkdown installed in your computer
# and some LaTeX processor such as MiKTeX for Windows or MacTeX for Mac.
# An alternative way to print the output from texreg is to create a blank
# document in overleaf.com and paste the code after the \begin{document} line.
texreg(list(reg1, reg2, reg3, reg4), include.ci = F, caption.above = T, digits = 4,
       caption = "Race and Resume Call-Back Rate",
       custom.model.names = c("(1)", "(2)", "(3)", "(4)"))

##(d)
Tests = lm_robust(cbind(ofjobs, yearsexp, honors, volunteer, military, empholes,
              workinschool, email, computerskills, specialskills, eoe, manager,
              supervisor, secretary, offsupport, salesrep,
              retailsales, req, expreq, comreq, educreq, compreq, orgreq,
              manuf, transcom, bankreal, trade, busservice, othservice,
              missind, chicago, high, female, college, call_back) ~ black,
          se_type = "stata")
summary(Tests)
tidy(Tests)

# Results of a series of t-tests (via linear regressions, see the log-file)
# shows estimated means of other characteristics for black and white sounding
# names. There are only two significant differences in the mean values: the
# call-back rate (the variable of interest) and computer skills (for which
# black-named resumes had a slightly higher fraction than white-named resumes).
# Thus, there is no evidence of non-random assignment.

detach(Names)

### TSLS
# Clean Working Environment
rm(list = ls())

# load data
tsls  <- 
# attach dataset
attach(tsls)


## (a)
reg1  <- 
summary(reg1)
# The estimated model is 
# Y_hat = 1.045(0.115) + 0.303(0.176) X1 - 0.543(0.056) X2, adj_R^2 = 0.495

## (b)
# The exogeneity assumption (E[u|X1, X2] = 0) would be violated. If this were
# the case, OLS would be biased and inconsistent.

## (c)
# Two conditions must hold: (1) Cor(u, Z1) = 0 (exogeneity), and (2) 
# Cor(X2, Z1) != 0 (relevance).

## (d)
reg2  <- 
summary(reg2)
# The estimated model is 
# Y_hat = 1.024(0.137) + 0.831(0.337) X1 - 0.929(0.178) X2, adj_R^2 = 0.254 
# As we have one IV for one endogenous regressor, Beta's are exactly identified.
# Running two OLS can replicate the TSLS estimates. However, this procedure
# tends to underestimate the SE of the IV estimator, which would make
# statistical inference (t-statistics, p-values, and confidence intervals, etc.)
# invalid.

## Extra code to show the TSLS process done manually
ols.1stage = lm_robust(x2 ~ x1 + z1, se_type = "stata")
x2_hat <- predict(ols.1stage, newdata = data.frame(x1 = x1, z1 = z1))
summary(ols.1stage)
cor(x2, z1)

ols.2stage = lm_robust(y ~ x1 + x2_hat, se_type = "stata")
summary(ols.2stage)
summary(reg2)
##

## (f)
ols.1stage = lm_robust(x2 ~ x1 + z1, se_type = "stata")
linearHypothesis(ols.1stage, c("z1 = 0"))

# Run a regression of X2 against (1, X1, Z1). Compute the F-statistic for the
# coefficient on Z1 being 0. The F-statistic = 17.83 > 10 and has essentially 0
# p-value. Thus, we can conclude that Z1 is relevant and sufficiently strong.

## (g)
reg3  <- 
summary(reg3, diagnostics = TRUE)

# We conduct the over identifying restrictions test. The resulting p-value =
# 0.355 is large. Hence, we do not reject the null hypothesis that Z2 is
# exogenous.

texreg(list(reg1, reg2, reg3), include.ci = F, caption.above = T, digits = 4,
       caption = "TSLS",
       custom.model.names = c("(1)", "(2)", "(3)"))

## (h) 
# It is better to use both Z1 and Z2. The two TSLS estimations give similar
# estimates of the two slope coefficients, while the one using both Z1 and Z2
# has smaller SE.

## Extra code to show the TSLS process done manually
# first stage
ols.1stage = lm_robust(x2 ~ x1 + z1 + z2) 
summary(ols.1stage)

# prediction
tsls$x2_hat2 <- predict(ols.1stage, newdata = data.frame(x1 = x1, z1 = z1, z2 = z2))

# second stage
ols.2stage = lm_robust(y ~ x1 + tsls$x2_hat2, se_type = "stata")
summary(ols.2stage)
summary(reg2)

# correlation
cor(x2, tsls$x2_hat2)

detach(tsls)