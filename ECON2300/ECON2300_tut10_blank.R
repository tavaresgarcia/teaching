# Tutorial 10: Instrumental Variables Regression

# Answers by Dong-Hyuk Kim
# https://sites.google.com/site/kimdonghyuk000/home
# Extra material by Francisco Tavares Garcia
# https://tavaresgarcia.github.io/

# install.packages("readr")
# install.packages("dplyr")
# install.packages("estimatr")
# install.packages("texreg") 
# install.packages("car")
# install.packages("multcomp")
install.packages("AER")

library(readr)    # package for fast read rectangular data
library(dplyr)    # package for data manipulation
library(estimatr) # package for commonly used estimators with robust SE
library(texreg)   # package for converting R regression output to LaTeX/HTML tables
library(car)      # package for functions used in "An R Companion to Applied Regression"
library(multcomp) # package for simultaneous tests and CIs for general linear hypotheses
library(AER)      # package for functions used in "Applied Econometrics using R"


### SW E12.1

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

# Load dataset
Fertility <- 
# attach dataset
attach(Fertility)


## (a)
OLS <- 
summary(OLS)
# The coefficient is -5.39, which indicates that women with more than 2 children
# work 5.39 fewer weeks per year than women with 2 or fewer children.

## (b)
# Both fertility and weeks worked are choice variables. A woman with a positive
# labour supply regression error (a woman who works more than average) may also
# be a woman who is less likely to have an additional child. This would imply
# that morekids is correlated with the error, so that the OLS estimator of
# Beta_morekids is biased.

## (c)
OLS.first1 <- 
summary(OLS.first1)
# The linear regression of morekids on samesex (a linear probability model)
# yields morekids_hat = 0.346(0.001) + 0.068(0.002) x samesex, so that couples
# with samesex = 1 are 6.8% more likely to have an additional child than couples
# with samesex = 0. The effect is highly significant (t-statistic = 35.2).

## (d)


# samesex is random and is unrelated to any of the other variables in the model
# including the error term in the labour supply equation. Thus, the instrument
# is exogenous. From (c), the first stage F-statistic is large (F = 1238.2 > 10)
# so the instrument is relevant. Together, these imply that samesex is a valid
# instrument.


## (e)
# No, see the answer to (d).

## (f)
TSLS1 <- 
summary(TSLS1)
# See column (2) of Table 1. The estimated value of Beta_morekids = -6.314.

# Extra Material:
OLS.first1 <- lm_robust(morekids ~ samesex, se_type = "stata")
FS_morekids <- predict(OLS.first1, newdata = data.frame(samesex = samesex))
TSLS1_byhand <- lm_robust(weeksm1 ~ FS_morekids, se_type = "stata")
summary(TSLS1_byhand)

## (g)

TSLS2 <- 
summary(TSLS2)

# See column (3) of Table 1. The results do not change in an important way. The
# reason is that samesex is unrelated to agem1, black, hispan, and othrace, so
# that there is no omitted variable bias in IV regression in (f).

# Extra Material:
OLS.first2 = lm_robust(morekids ~ samesex + agem1 + 
                         black + hispan + othrace, se_type = "stata")

FS_morekids2 <- predict(OLS.first2, newdata = data.frame(samesex = samesex, 
                                                         agem1 =  agem1,
                                                         black = black, 
                                                         hispan = hispan,
                                                         othrace = othrace))

TSLS2_byhand <- lm_robust(weeksm1 ~ FS_morekids2 + agem1 + 
                            black + hispan + othrace, se_type = "stata")
summary(TSLS2_byhand)

# comparing the impact of adding new regressors with just morekids.
TSLS2_byhand2 <- lm_robust(weeksm1 ~ FS_morekids + agem1 + black + hispan + 
                          othrace, se_type = "stata")
summary(TSLS2_byhand2)


# To run texreg, please make sure you have RMarkdown installed in your computer
# and some LaTeX processor such as MiKTeX for Windows or MacTeX for Mac.
# An alternative way to print the output from texreg is to create a blank
# document in overleaf.com and paste the code after the \begin{document} line.

# Table 1
texreg(list(OLS, TSLS1, TSLS2), include.ci = F, caption.above = T, digits = 3,
       caption = "Fertility and Labor Supply",
       custom.model.names = c("(1) OLS", "(2) TSLS", "(3) TSLS"))

# Table 2
texreg(list(OLS.first1, OLS.first2), include.ci = F, caption.above = T, 
       digits = 3, caption = "First Stage Estimation of TSLS", 
       include.fstatistic = T, custom.model.names = c("(2) TSLS", "(3) TSLS"))


detach(Fertility)

### SW E12.2
# Clean Working Environment
rm(list = ls())

Movies <- read_csv("Movies.csv") %>%
  mutate(ln_assaults = log(assaults),
         attend = attend_v + attend_m + attend_n)
attach(Movies)

## (a)
# (i) 
reg1 = lm_robust(ln_assaults ~ year2 + year3 + year4 + year5 + year6 + year7 + 
                   year8 + year9 + year10 + month2 + month3 + month4 + month5 + 
                   month6 + month7 + month8 + month9 + month10 + month11 + 
                   month12, se_type = "stata")
summary(reg1)

linearHypothesis(reg1, c("month2=0", "month3=0", "month4=0", "month5=0",
                         "month6=0", "month7=0", "month8=0", "month9=0",
                         "month10=0", "month11=0", "month12=0"), test=c("F"))

# The F-statistic on the 11 monthly indicators is 78.28 with a p-value that is
# essentially 0. Thus, there is strong evidence of seasonality in assaults. (The
# estimates imply that there are more assaults in the summer than in the
# winter.)

# (ii) 
reg2 = lm_robust(attend ~ year2 + year3 + year4 + year5 + year6 + 
                   year7 + year8 + year9 + year10 + 
                   month2 + month3 + month4 + month5 + month6 + month7 + 
                   month8 + month9 + month10 + month11 + month12, 
                 se_type = "stata")
summary(reg2)

linearHypothesis(reg2, c("month2=0", "month3=0", "month4=0", "month5=0",
                         "month6=0", "month7=0", "month8=0", "month9=0",
                         "month10=0", "month11=0", "month12=0"), test=c("F"))

# The F-statistic on the 11 monthly indicators is 58.57 with p-value that is
# essentially 0. Thus, there is strong evidence of seasonality in movie
# attendance. (The estimates imply that attendance is high in the summer.)


## (b)
# (i) 
OLS = lm_robust(ln_assaults ~ attend_v + attend_m + attend_n + 
        year2 + year3 + year4 + year5 + year6 + year7 + year8 + year9 + year10 + 
        month2 + month3 + month4 + month5 + month6 + month7 + 
        month8 + month9 + month10 + month11 + month12 + 
        h_chris + h_newyr + h_easter + h_july4 + h_mem + h_labor + 
        w_maxa + w_maxb + w_maxc+ w_mina + w_minb + w_minc + w_rain + w_snow,
      se_type = "stata")
summary(OLS)

# The results are shown in the column labeled OLS in Table 3. An increase in
# strongly violent movie attendance of one million viewers is predicted to
# reduce assaults by 0.32%. The coefficient is statistically significant at the
# 1% significance level.


# (ii) 
linearHypothesis(OLS, c("attend_v = attend_m"), test=c("F"))

linearHypothesis(OLS, c("attend_v = attend_n"), test=c("F"))

linearHypothesis(OLS, c("attend_v=attend_m", "attend_v=attend_n"), test=c("F"))


# The F-statistic suggests that the coefficients Beta_v, Beta_m, and Beta_n are
# not statistically significantly different from one another.

# (iii) 
confint(glht(OLS, linfct = c("6*attend_v - 2*attend_m - attend_n = 0")))

# The question asks for an estimate and standard error for 
# 6*Beta_v - 2*Beta_m - Beta_n. The OLS estimate for this coefficient is -0.011.
# It shows a decrease in assaults of 1.1%. The 95% confidence interval is -0.020
# to -0.0008 (or -2.0% to -0.08%).

## (c)
# (i) 
TSLS1 = ivreg(ln_assaults ~ attend_v + attend_m + attend_n +
    year2 + year3 + year4 + year5 + year6 + year7 + year8 + year9 + year10 + 
    month2 + month3 + month4 + month5 + month6 + month7 + 
    month8 + month9 + month10 + month11 + month12 +
    h_chris + h_newyr + h_easter + h_july4 + h_mem + h_labor +
    w_maxa + w_maxb + w_maxc+ w_mina + w_minb + w_minc + w_rain + w_snow |
    pr_attend_v + pr_attend_m + pr_attend_n +
    year2 + year3 + year4 + year5 + year6 + year7 + year8 + year9 + year10 + 
    month2 + month3 + month4 + month5 + month6 + month7 +
    month8 + month9 + month10 + month11 + month12 +
    h_chris + h_newyr + h_easter + h_july4 + h_mem + h_labor +
    w_maxa + w_maxb + w_maxc+ w_mina + w_minb + w_minc + w_rain + w_snow)

summary(TSLS1)

# The results are shown in the column labeled IV in Table 3. An increase in
# strongly violent movie attendance of one million viewers is predicted to
# reduce assaults by 0.39%. The coefficient is statistically significant at the
# 1% significance level.

# (ii) 
linearHypothesis(TSLS1, c("attend_v=attend_m"), test=c("F"))

linearHypothesis(TSLS1, c("attend_v=attend_n"), test=c("F"))

linearHypothesis(TSLS1, c("attend_v=attend_m", "attend_v=attend_n"), test=c("F"))

# The F-statistic suggests that the coefficients Beta_v, Beta_m, and Beta_n are
# not statistically significantly different from one another.

# (iii) 
confint(glht(TSLS1, linfct = c("6*attend_v - 2*attend_m - attend_n = 0")))

# The TSLS estimate for this coefficient is -0.013. It shows a decrease in
# assaults of 1.3%. The 95% confidence interval is -0.024 to -0.0016 (or -2.4%
# to -0.16%).

## (d)
# (i) 
TSLS2 = ivreg(ln_assaults ~ attend_v + attend_m + attend_n +
    year2 + year3 + year4 + year5 + year6 + year7 + year8 + year9 + year10 + 
    month2 + month3 + month4 + month5 + month6 + month7 +
    month8 + month9 + month10 + month11 + month12 +
    h_chris + h_newyr + h_easter + h_july4 + h_mem + h_labor +
    w_maxa + w_maxb + w_maxc+ w_mina + w_minb + w_minc + w_rain + w_snow |
    attend_v_f + attend_m_f + attend_n_f + attend_v_b + attend_m_b + attend_n_b +
    year2 + year3 + year4 + year5 + year6 + year7 + year8 + year9 + year10 + 
    month2 + month3 + month4 + month5 + month6 + month7 +
    month8 + month9 + month10 + month11 + month12 +
    h_chris + h_newyr + h_easter + h_july4 + h_mem + h_labor +
    w_maxa + w_maxb + w_maxc+ w_mina + w_minb + w_minc + w_rain + w_snow)

summary(TSLS2)

# The results are shown in the column labeled TSLS in Table 3. An increase in
# strongly violent movie attendance of one million viewers is predicted to
# reduce assaults by 0.32%. The coefficient is not statistically significant at
# the 10% significance level.

# (ii) 
linearHypothesis(TSLS2, c("attend_v=attend_m"))

linearHypothesis(TSLS2, c("attend_v=attend_n"))

linearHypothesis(TSLS2, c("attend_v=attend_m", "attend_v=attend_n"))

# The F-statistic suggests that the coefficients Beta_v, Beta_m, and Beta_n are
# not statistically significantly different from one another.

# (iii) 
confint(glht(TSLS2, linfct = c("6*attend_v - 2*attend_m - attend_n = 0")))

# The TSLS estimate for this coefficient is -0.008. It shows a decrease in
# assaults of 0.8%. The 95% confidence interval is -0.027 to 0.011 (or -2.7% to
# 1.1%).

# The texreg to print the table with the main regressors.
texreg(list(OLS, TSLS1, TSLS2), include.ci = F, caption.above = T, digits = 4,
       caption = "Violent Movie and Violent Behavior",
       custom.model.names = c("(1) OLS", "(2) IV", "(3) TSLS"),
       omit.coef = "(year)|(month)|(h_)|(w_)")

## (e)
TSLS3 = ivreg(ln_assaults ~ attend_v + attend_m + attend_n +
    year2 + year3 + year4 + year5 + year6 + year7 + year8 + year9 + year10 + 
    month2 + month3 + month4 + month5 + month6 + month7 +
    month8 + month9 + month10 + month11 + month12 +
    h_chris + h_newyr + h_easter + h_july4 + h_mem + h_labor +
    w_maxa + w_maxb + w_maxc+ w_mina + w_minb + w_minc + w_rain + w_snow |
    pr_attend_v + pr_attend_m + pr_attend_n +
    attend_v_f + attend_m_f + attend_n_f + attend_v_b + attend_m_b + attend_n_b +
    year2 + year3 + year4 + year5 + year6 + year7 + year8 + year9 + year10 + 
    month2 + month3 + month4 + month5 + month6 + month7 +
    month8 + month9 + month10 + month11 + month12 +
    h_chris + h_newyr + h_easter + h_july4 + h_mem + h_labor +
    w_maxa + w_maxb + w_maxc+ w_mina + w_minb + w_minc + w_rain + w_snow)

summary(TSLS3, diagnostics = T)

# The J-statistic is 9.23, which is distributed X^2_6 under the null hypothesis
# that the additional instruments are exogenous. As the p-value is 0.16, we do
# not reject the null hypothesis that the instruments are valid at the 10%
# level.

## (f)
# Movie attendance appears to reduce assaults, but there is little evidence of a
# differential effect of violent movies. This result is consistent with a
# mechanism in which movies attendance is a substitute for other activities,
# such as drinking, that increase assaults.

detach(Movies)