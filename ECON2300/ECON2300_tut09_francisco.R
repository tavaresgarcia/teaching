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

smoking <- read_csv("Smoking.csv") %>% mutate(age2 = age^2)
attach(smoking)

lpm1 = lm_robust(smoker ~ smkban, se_type = "stata")
lpm2 = lm_robust(smoker ~ smkban + female + age + age2 + hsdrop + hsgrad + 
                   colsome + colgrad + black + hispanic, se_type = "stata")

# fit Probit model
probit <- glm(smoker ~ smkban + female + age + age2 + hsdrop + hsgrad + colsome +
               colgrad + black + hispanic, family = binomial(link = "probit"))

# fit Logit model
logit = glm(smoker ~ smkban + female + age + age2 + hsdrop + hsgrad + colsome +
              colgrad + black + hispanic, family = binomial(link = "logit"))


# To run texreg, please make sure you have RMarkdown installed in your computer
# and some LaTeX processor such as MiKTeX for Windows or MacTeX for Mac.
# An alternative way to print the output from texreg is to create a blank
# document in overleaf.com and paste the code after the \begin{document} line.

texreg(list(lpm1, lpm2, probit, logit), include.ci = F, caption.above = T,
        custom.model.names = c("LPM (1)", "LPM (2)", "Probit", "Logit"))

## (a)
# (i)
# run regression with intercept only
Pa = lm(smoker ~ 1, data = smoking)
summary(Pa)
mean(smoker)

# (ii)
P1 = lm(smoker ~ 1, data = subset(smoking, smkban == 1))
summary(P1)

# (iii)
P0 = lm(smoker ~ 1, data = subset(smoking, smkban == 0))
summary(P0)

## (b)
summary(lpm1)
# From LPM(1), the difference is -0.08 with a standard error of 0.01. The
# resulting t-statistic is -8, so the coefficient is statistically significant.
summary(lpm1)$coefficients[2,1:3]

## (c)
summary(lpm2)
# From LPM(2) the estimated difference is -0.05, smaller than the effect in
# LPM(1). Evidently (1) suffers from omitted variable bias. That is, smkban may
# be correlated with the education/race/gender or with age. For example, workers
# with a college degree are more likely to work in an office with a smoking ban
# than high-school drop-outs, and college graduates are less likely to smoke
# than high-school drop-outs.
summary(lpm2)$coefficients[2,1:3]

## (d)
summary(lpm2)
# The t-statistic is -5, so the coefficient is statistically significant at the
# 1% level.
summary(lpm2)$coefficients[2,3]
linearHypothesis(lpm2, c("smkban = 0"), test = "F")

## (e)
linearHypothesis(lpm2, c("hsdrop=0", "hsgrad=0", "colsome=0", "colgrad=0"),
                 test=c("F"))
# The F-statistic has a p-value of 0.00, so the coefficients are significant.
# The omitted education status is "Masters degree or higher." Thus the
# coefficients show the increase in probability relative to someone with a
# postgraduate degree. For example, the coefficient on colgrad is 0.045, so the
# probability of smoking for a college graduate is 0.04 (4%) higher than for
# someone with a postgraduate degree. Similarly, the coefficient on hsdrop is
# 0.32, so the probability of smoking for a high school drop-out is 0.32 (32%)
# higher than for someone with a postgraduate degree. Because the coefficients
# are all positive and get smaller as educational attainment increases, the
# probability of smoking falls as educational attainment increases.

## (f)
summary(probit)
linearHypothesis(probit, c("hsdrop=0", "hsgrad=0", "colsome=0", "colgrad=0"),
                 test=c("F"))

pnorm(-2.58)

# The estimated effect of the smoking ban in the probit model depends on the
# values of the other variable included in the regression. The estimated effects
# for various values of these regressors are given question (h). The t-statistic
# for the coefficient on smkban is -5.5, very similar to the value for the
# linear probability and logit models. The F-statistic is significant at the 1%
# level, as in the linear probability model.

## (g)
summary(logit)
linearHypothesis(logit, c("hsdrop=0", "hsgrad=0", "colsome=0", "colgrad=0"),
                 test=c("F"))
# The estimated effect of the smoking ban in the logit model depends on the
# values of the other variable included in the regression. The estimated effects
# for various values of these regressors is given question (h). The t-statistic
# for the coefficient on smkban is -5.3, very similar to the value for the
# linear probability and probit models. The F-statistic is significant at the 1%
# level, as in the linear probability model.

## (h)
# (i)
# computation and results
# predict probability of binary responses
probA.probit <- predict(probit, type = "response",
                        newdata = data.frame(smkban = c(0, 1), age = 20,
                                             age2 = 20^2, hsdrop = 1,
                                             hsgrad = 0, colsome = 0, colgrad = 0,
                                             female = 0, black = 0, hispanic = 0))
probA.probit
# pnorm(-0.09010474)
# pnorm(-0.24873438)
# compute difference in response probabilities
diff(probA.probit)

# (ii)
probB.probit <- predict(probit, type = "response",
                        newdata = data.frame(smkban = c(0, 1), age = 40,
                                             age2 = 40^2, hsdrop = 0,
                                             hsgrad = 0, colsome = 0, colgrad = 1,
                                             female = 1, black = 1, hispanic = 0))
probB.probit
# pnorm(-1.063862)
# pnorm(-1.222491)
diff(probB.probit)

# (iii)
# computation and results
probA.lpm <- predict(lpm2,
                     newdata = data.frame(smkban = c(0, 1), age = 20,
                                          age2 = 20^2, hsdrop = 1,
                                          hsgrad = 0, colsome = 0, colgrad = 0,
                                          female = 0, black = 0, hispanic = 0))
probA.lpm
diff(probA.lpm)

probB.lpm <- predict(lpm2,
                     newdata = data.frame(smkban = c(0, 1), age = 40,
                                          age2 = 40^2, hsdrop = 0,
                                          hsgrad = 0, colsome = 0, colgrad = 1,
                                          female = 1, black = 1, hispanic = 0))
probB.lpm
diff(probB.lpm)

# (iv)
# computation and results
probA.logit <- predict(logit, type = "response",
                       newdata = data.frame(smkban = c(0, 1), age = 20,
                                            age2 = 20^2, hsdrop = 1,
                                            hsgrad = 0, colsome = 0, colgrad = 0,
                                            female = 0, black = 0, hispanic = 0))
probA.logit
diff(probA.logit)

probB.logit <- predict(logit, #type = "response",
                       newdata = data.frame(smkban = c(0, 1), age = 40,
                                            age2 = 40^2, hsdrop = 0,
                                            hsgrad = 0, colsome = 0, colgrad = 1,
                                            female = 1, black = 1, hispanic = 0))
probB.logit
diff(probB.logit)
# plogis(-1.811043)
# plogis(-2.073071)

# (v)
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