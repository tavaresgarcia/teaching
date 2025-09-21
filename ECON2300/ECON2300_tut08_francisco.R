# Tutorial 8: Regression with Panel Data

# Answers by Dong-Hyuk Kim
# https://sites.google.com/site/kimdonghyuk000/home

# Extra material by Francisco Tavares Garcia
# https://tavaresgarcia.github.io/

# install.packages("readr")
# install.packages("dplyr")
# install.packages("estimatr")
# install.packages("texreg") 
install.packages("plm")

library(readr)    # package for fast read rectangular data
library(dplyr)    # package for data manipulation
library(estimatr) # package for commonly used estimators with robust SE
library(texreg)   # package for converting R regression to LaTeX/HTML tables
library(plm)      # package for estimating linear panel data models


### SW E10.1

# Clean Working Environment
rm(list = ls())

# Set working directory (make sure you edit to your own WD)
# Ex Win: setwd("G:/My Drive/BEcon/TUTOR/ECON2300/04")
# Ex Mac: setwd("/Users/uqdkim7/Dropbox/Teaching/R tutorials/Tutorial04")

# To use the following line: 
# save this file in the same directory as the data files
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

Guns <- read_csv("Guns.csv") %>%
  mutate(lvio = log(vio), lrob = log(rob), lmur = log(mur))
attach(Guns)

# The solutions for (a)-(c) will reference regression results summarized in
# Table 1.

# fit pooled OLS using cluster and heteroskedasticity robust SE
pols1 = lm_robust(lvio ~ shall, data = Guns, se_type = "stata", 
                  clusters = stateid)

pols2 = lm_robust(lvio ~ shall + incarc_rate + density + avginc +
                    pop + pb1064 + pw1064 + pm1029,
                  data = Guns, se_type = "stata", clusters = stateid)

# fit fixed effects model
fe1 = plm(lvio ~ shall + incarc_rate + density + avginc +
            pop + pb1064 + pw1064 + pm1029,
          data = Guns, model = "within", index = c("stateid"))

# fit fixed effects model with time effects
fe2 = plm(lvio ~ shall + incarc_rate + density + avginc +
            pop + pb1064 + pw1064 + pm1029,
          data = Guns, model = "within", index = c("stateid", "year"),
          effect = "twoway")

# or equivalently use function factor() to include dummies
fe3 = plm(lvio ~ shall + incarc_rate + density + avginc +
            pop + pb1064 + pw1064 + pm1029 + factor(year),
          data = Guns, model = "within", index = c("stateid"))


# To my knowledge, there is no option for plm that can help computing cluster 
# robust SE. Here we compute them using the vcovHC function as follows:

# compute cluster robust SE for FE estimator
SE.fe1 <- sqrt(diag(vcovHC(fe1, type="sss", cluster="group")))
SE.fe2 <- sqrt(diag(vcovHC(fe2, type="sss", cluster="group")))

# To use texreg, all SEs and p-values should be customized.

# extract SE of pooled OLS estimator
SE.pols1 <- pols1$std.error
SE.pols2 <- pols2$std.error

# compute p-values
p.pols1 <- 2*(1 - pnorm(abs(pols1$coefficients/SE.pols1)))
p.pols2 <- 2*(1 - pnorm(abs(pols2$coefficients/SE.pols2)))
p.fe1 <- 2*(1 - pnorm(abs(fe1$coefficients/SE.fe1)))
p.fe2 <- 2*(1 - pnorm(abs(fe2$coefficients/SE.fe2)))

# An alternative way to print the output from texreg is to create a blank
# document in overleaf.com and paste the code after the \begin{document} line.

# generate LaTeX code for Table 1
texreg(list(pols1, pols2, fe1, fe2), include.ci = F, caption.above = T, 
       digits = 3, override.se = list(SE.pols1,SE.pols2,SE.fe1,SE.fe2),
       override.pvalues = list(p.pols1, p.pols2, p.fe1, p.fe2),
       caption = "Violent Crime Rate and Shall-Carry Law",
       custom.model.names = c("(1) Pooled OLS (1)", "(2) Pooled OLS",
                              "(3) Fixed Effects", 
                              "(4) Fixed Effects and Time Effects"))

## (a)
summary(pols2)
# (i) The coefficient is -0.368, which suggests that shall-issue laws reduce
# violent crime by 36%. This is a large effect.
# 
summary(pols1)
# (ii) The coefficient in (1) is -0.443, while in (2) it is -0.368. Both are
# highly statistically significant. Adding the control variables results in a
# small drop in the coefficient.
# 
# (iii) There are several examples. Here are two: Attitudes towards guns and
# crime, and quality of police and other crime-prevention programs.


## (b)
summary(fe1)
SE.fe1
# In (3) the coefficient on shall falls to -0.046, a large reduction in the
# coefficient from (2). Evidently there was important omitted variable bias in
# (2). The estimate is not statistically significantly different from zero.

# Extra material - Doing it manually
manual_fe1 <- lm_robust(lvio ~ shall + incarc_rate + density + avginc + 
                          pop + pb1064 + pw1064 + pm1029 + factor(stateid),
                        data = Guns, se_type = "stata", clusters = stateid)
summary(manual_fe1)

# we need to add cluster = stateid to have the correct standard errors
# e.g. all Texas observations are correlated.

## (c)
# test time effects
summary(fe2)
SE.fe2

pFtest(fe2, fe1)
# ?pFtest
# The coefficient in (4) falls further to -0.028. The coefficient is
# insignificantly different from zero. The time effects are jointly
# statistically significant (p-value ~ 0), so this regression seems better
# specified than (3).

summary(fe3)

# Extra material - Doing it manually
manual_fe2 <- lm_robust(lvio ~ shall + incarc_rate + density + avginc + 
                          pop + pb1064 + pw1064 + pm1029 + factor(stateid) + 
                          factor(year),
                        data = Guns, se_type = "stata", clusters = stateid)
summary(manual_fe2)

# The solutions for (d)-(f) will reference regression results summarized in
# Table 2 and 3.

## Table 2
pols1 = lm_robust(lrob ~ shall, data = Guns, se_type = "stata", 
                  clusters = stateid)
pols2 = lm_robust(lrob ~ shall + incarc_rate + density + avginc +
                    pop + pb1064 + pw1064 + pm1029,
                  data = Guns, se_type = "stata", clusters = stateid)
fe1 = plm(lrob ~ shall + incarc_rate + density + avginc +
            pop + pb1064 + pw1064 + pm1029,
          data = Guns, model = "within", index = c("stateid"))
fe2 = plm(lrob ~ shall + incarc_rate + density + avginc +
            pop + pb1064 + pw1064 + pm1029,
          data = Guns, model = "within", index = c("stateid", "year"),
          effect = "twoway")

SE.pols1 <- pols1$std.error
SE.pols2 <- pols2$std.error

SE.fe1 <- sqrt(diag(vcovHC(fe1, type="sss", cluster="group")))
SE.fe2 <- sqrt(diag(vcovHC(fe2, type="sss", cluster="group")))

p.pols1 <- 2*(1 - pnorm(abs(pols1$coefficients/SE.pols1)))
p.pols2 <- 2*(1 - pnorm(abs(pols2$coefficients/SE.pols2)))
p.fe1 <- 2*(1 - pnorm(abs(fe1$coefficients/SE.fe1)))
p.fe2 <- 2*(1 - pnorm(abs(fe2$coefficients/SE.fe2)))

texreg(list(pols1, pols2, fe1, fe2), include.ci = F, caption.above = T, 
       digits = 3, override.se = list(SE.pols1,SE.pols2,SE.fe1,SE.fe2),
       override.pvalues = list(p.pols1, p.pols2, p.fe1, p.fe2),
       caption = "Robbery Rate and Shall-Carry Law",
       custom.model.names = c("(1) Pooled OLS (1)", "(2) Pooled OLS",
                              "(3) Fixed Effects", 
                              "(4) Fixed Effects and Time Effects"))

## Table 3
pols1 = lm_robust(lmur ~ shall, data = Guns, se_type = "stata", 
                  clusters = stateid)
pols2 = lm_robust(lmur ~ shall + incarc_rate + density + avginc +
                    pop + pb1064 + pw1064 + pm1029,
                  data = Guns, se_type = "stata", clusters = stateid)
fe1 = plm(lmur ~ shall + incarc_rate + density + avginc +
            pop + pb1064 + pw1064 + pm1029,
          data = Guns, model = "within", index = c("stateid", "year"))
fe2 = plm(lmur ~ shall + incarc_rate + density + avginc +
            pop + pb1064 + pw1064 + pm1029,
          data = Guns, model = "within", index = c("stateid", "year"),
          effect = "twoway")

SE.pols1 <- pols1$std.error
SE.pols2 <- pols2$std.error

SE.fe1 <- sqrt(diag(vcovHC(fe1, type="sss", cluster="group")))
SE.fe2 <- sqrt(diag(vcovHC(fe2, type="sss", cluster="group")))

p.pols1 <- 2*(1 - pnorm(abs(pols1$coefficients/SE.pols1)))
p.pols2 <- 2*(1 - pnorm(abs(pols2$coefficients/SE.pols2)))
p.fe1 <- 2*(1 - pnorm(abs(fe1$coefficients/SE.fe1)))
p.fe2 <- 2*(1 - pnorm(abs(fe2$coefficients/SE.fe2)))

texreg(list(pols1, pols2, fe1, fe2), include.ci = F, caption.above = T, 
       digits = 3, override.se = list(SE.pols1,SE.pols2,SE.fe1,SE.fe2),
       override.pvalues = list(p.pols1, p.pols2, p.fe1, p.fe2),
       caption = "Murder Rate and Shall-Carry Law",
       custom.model.names = c("(1) Pooled OLS", "(2) Pooled OLS",
                              "(3) Fixed Effects", 
                              "(4) Fixed Effects and Time Effects"))


## (d)
# Tables 2-3 show the coefficient on shall in the regression specifications
# (1)-(4) using ln (rob) and ln (mur) as dependent variables, respectively. The
# quantitative results are similar to the results using violent crimes: there is
# a large estimated effect of concealed weapons laws in specifications (1) and
# (2). This effect is spurious and is due to omitted variable bias as
# specification (3) and (4) show.

## (e)
# There is potential two-way causality between this year's incarceration rate
# and the number of crimes. Because this year's incarceration rate is much like
# last year's rate, there is a potential two-way causality problem. There are
# similar two-way causality issues relating crime and shall.

## (f)
# The most credible results are given by regression (4). The 95% confidence
# interval for B_Shall is -11.0% to 5.3%. This includes B_Shall = 0. Thus, there
# is no statistically significant evidence that concealed weapons laws have any
# effect on crime rates.