# Tutorial 6: Nonlinear Regression Functions

# Answers by Dong-Hyuk Kim
# https://sites.google.com/site/kimdonghyuk000/home

# Extra material by Francisco Tavares Garcia
# https://tavaresgarcia.github.io/

# install.packages("readr")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("estimatr")
# install.packages("texreg") 
# install.packages("car")
install.packages("multcomp")

library(readr)    # package for fast read rectangular data
library(dplyr)    # package for data manipulation
library(ggplot2)  # package for elegant data visualisations
library(estimatr) # package for commonly used estimators with robust SE
library(texreg)   # package for converting R regression output to LaTeX/HTML tables
library(car)      # package for functions used in "An R companion to Applied Regression"
library(multcomp) # package for simultaneous tests and CIs for general linear hypotheses

### SW E8.1 Lead is toxic, particularly for young children, and for this
#reason government regulations severely restrict the amount of lead in our
#environment. But this was not always the case. In the early part of the 20th
#century, the underground water pipes in many U.S. cities contained lead, and
#lead from these pipes leached into drinking water. In this exercise you will
#investigate the effect of these lead water pipes on infant mortality using the
#dataset, lead_mortality.csv, which contains data on infant mortality, type of
#water pipes (lead or non-lead), water acidity (pH), and several demographic
#variables for 172 U.S cities in 1900; see also Lead_Mortality_Description.pdf.

# House cleaning
rm(list = ls()) # clear Environment
if (!is.null(dev.list())) dev.off() # clear Plots
cat("\014") # clear Console

# Set working directory (make sure you edit to your own WD)
# Ex Win: setwd("G:/My Drive/BEcon/TUTOR/ECON2300/04")
# Ex Mac: setwd("/Users/uqdkim7/Dropbox/Teaching/R tutorials/Tutorial04")

# To use the following line: save this file in the same folder as the data files
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load lead mortality dataset
LM <- 
# attach the dataset variable

# regressions

# To run texreg, please make sure you have RMarkdown installed in your computer
# and some LaTeX processor such as MiKTeX for Windows or MacTeX for Mac.
# An alternative way to print the output from texreg is to create a blank
# document in overleaf.com and paste the code after the \begin{document} line.
texreg(list(reg1, reg2, reg3), include.ci = F, caption.above = T,
       digits = 3, caption = "Lead and Infant Mortality",
       custom.model.names = c("(1)", "(2)", "(3)"))

screenreg(list(reg1, reg2, reg3), include.ci = F, caption.above = T,
       digits = 3, caption = "Lead and Infant Mortality",
       custom.model.names = c("(1)", "(2)", "(3)"))

## (a) Compute the average infant mortality rate, infrate, for cities with lead
#pipes and for cities with non-lead pipes. Is there a statistically significant
#difference in the average?


# Column (1) of Table 1 shows that the sample mean of infrate for cities with 
# non-lead pipes and cities with lead pipes are 0.381 and 0.403, respectively. 
# The difference in the sample means is 0.022 with a standard error of 0.024. 
# The estimate implies that cities with lead pipes have a higher infant 
# mortality rate (by 0.022 deaths per 100 people in the population), but the 
# standard error is comparatively large (0.024) and so the difference is not 
# statistically significant (t-statistic= 0.91).

## (b) The amount of lead leached from lead pipes depends on the chemistry of
#the water running through the pipes. The more acidic the water (that is, the
#lower its pH), the more lead is leached. Run a regression of infrate on lead,
#ph, and the interaction term lead x ph.

## (i) The regression includes four coefficients (the intercept and the three
# coefficients multiplying the regressors). Explain what each coefficient
# measures.


# The first coefficient is the intercept, which shows the level of infrate when 
# lead = 0 and ph = 0. It dictates the level of the regression line. The second 
# and fourth coefficients measure the effect of lead on the infant mortality 
# rate. Comparing 2 cities, one with lead pipes (lead = 1) and one without lead
# pipes (lead = 0), but the same of ph, the difference in predicted infant 
# mortality rate is:
# (infrate_hat | lead=1) - (infrate_hat | lead=0) = 0.462 - 0.057 x ph
# The third and fourth coefficients measure the effect of ph on the infant 
# mortality rate. Comparing 2 cities, one with a ph = 6 and the other with 
# ph = 5, but the same of lead, the difference in predicted infant mortality 
# rate is:
# (infrate_hat | ph=6) - (infrate_hat | ph=5) = -0.075 - 0.057 x lead

## (ii) Plot the estimated regression function relating infrate to ph for lead=0
# and for lead=1. Describe the differences in the regression functions and
# relate these differences to the coefficients you discussed in (i).

fig8.1 <- 

print(fig8.1)

# The infant mortality rate is higher for cities with lead pipes, but the 
# difference declines as the pH level increases.

## (iii) Does lead have a statistically significant effect on infant mortality?
## Explain.



# The F-statistic for the coefficient on lead and the interaction term is 3.94, 
# which has a p-value of 0.021, so the coefficients are jointly statistically 
# significantly different from zero at the 5% but not the 1% significance level.


## (iv) Does the effect of lead on infrate depend on ph? Is this dependence
# statistically significant?


# alternatively
summary(lm_robust(infrate ~ lead * ph, se_type = "stata"))

# The interaction term has a t-statistic of -2.02, so the coefficient is 
# significant at the 5% but not the 1% significance level.

## (v) What is the average value of ph in the sample? At this pH level, what is
#the estimated effect of lead on infant mortality? What is the standard
#deviation of pH? Suppose that the pH level is one standard deviation lower than
#the average level of pH in the sample: what is the estimated effect of lead on
#infant mortality? What if pH level is one standard deviation higher than the
#average value?


# The mean of pH is 7.32. At this level, the difference in infant mortality 
# rates is (infrate_hat|lead=1,ph=7.32) - (infrate_hat|lead=0,ph=7.32) 
# = 0.462 - 0.057 x 7.32 = 0.045

reg2$coefficients[2] + reg2$coefficients[4] * mean(ph)

# # Extra code to confirm results:
# predict(reg2, newdata = data.frame(
#   lead = 1,
#   ph = mean(ph),
#   lead_ph = mean(ph) * 1
# )) -
#   predict(reg2, newdata = data.frame(
#     lead = 0,
#     ph = mean(ph),
#     lead_ph = mean(ph) * 0
#   ))


sd(ph)
mean(ph) + sd(ph)
mean(ph) - sd(ph)

# The standard deviation of pH is 0.69, so that the mean plus 1 standard
# deviation is 8.01 and the mean minus 1 standard deviation is 6.63.
# The infant mortality rates at the pH levels are:
reg2$coefficients[2] + reg2$coefficients[4] * (mean(ph) + sd(ph))
# (infrate_hat|lead=1,ph=8.01) - (infrate_hat|lead=0,ph=8.01) = 0.462 - 0.057 x 8.01 = 0.005
reg2$coefficients[2] + reg2$coefficients[4] * (mean(ph) - sd(ph))
# (infrate_hat|lead=1,ph=6.63) - (infrate_hat|lead=0,ph=6.63) = 0.462 - 0.057 x 6.63 = 0.084

## Extra code to confirm results:
# predict(reg2, newdata = data.frame(
#   lead = 1,
#   ph = (mean(ph) + sd(ph)),
#   lead_ph = (mean(ph) + sd(ph)) * 1
# )) -
#   predict(reg2, newdata = data.frame(
#     lead = 0,
#     ph = (mean(ph) + sd(ph)),
#     lead_ph = (mean(ph) + sd(ph)) * 0
#   ))
# predict(reg2, newdata = data.frame(
#   lead = 1,
#   ph = (mean(ph) - sd(ph)),
#   lead_ph = (mean(ph) - sd(ph)) * 1
# )) -
#   predict(reg2, newdata = data.frame(
#     lead = 0,
#     ph = (mean(ph) - sd(ph)),
#     lead_ph = (mean(ph) - sd(ph)) * 0
#   ))


## (vi) Construct a 95% confidence interval for the effect of lead on infant
#mortality when pH = 6.5.

# Two possible approaches:

# Easy way - use RStudio confint and glht functions


# Hard way

# Write the regression as
# infrate = B0 + B1lead + B2ph + B3lead x ph + u
# so the effect of lead on infrate is B1 + B3 x ph. Thus, we want to construct a 
# 95% confidence interval (CI) for B1 + 6.5B3. This CI can be easily computed 
# using the functions confint and glht. The resulting CI is [0.028, 0.157]. 
# Equivalently, we can also use method 2 of Section 7.3, add and subtract 6.5*B3 
# lead to the regression to obtain:
# infrate = B0 + (B1 + 6.5B3) x lead + B2 x ph + B3 x (lead x ph - 6.5 x lead) +
# u = B0 + Gamma x lead + B2 x ph + B3lead x (ph - 6.5) + u
# So, the 95% CI for B1 + 6.5B3 is equal to the 95% CI for Gamma, which can be 
# constructed using results presented in column (3) of Table 1.

detach(LM)


### SW E8.2

# House cleaning
rm(list = ls()) # clear Environment
if (!is.null(dev.list())) dev.off() # clear Plots
cat("\014") # clear Console

# To use the following line: save this file in the same folder as the data files
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load cps12.csv
CPS12 <- read_csv("cps12.csv") %>%
  mutate(ln_ahe = log(ahe),
         ln_age = log(age),
         age2 = age*age,
         age3 = age^3,
         fem_bac = female*bachelor,
         fem_age = female*age,
         fem_age2 = female*age2,
         bac_age = bachelor*age,
         bac_age2 = bachelor*age2)
attach(CPS12)

reg1 <- lm_robust(ahe ~ age + female + bachelor, 
                  se_type = "stata")
reg2 <- lm_robust(ln_ahe ~ age + female + bachelor, 
                  se_type = "stata")
reg3 <- lm_robust(ln_ahe ~ ln_age + female + bachelor, 
                  se_type = "stata")
reg4 <- lm_robust(ln_ahe ~ age + age2 + female + bachelor, 
                  se_type = "stata")
reg5 <- lm_robust(ln_ahe ~ age + age2 + female + bachelor + fem_bac, 
                  se_type = "stata")
reg6 <- lm_robust(ln_ahe ~ age + age2 + fem_age + fem_age2 + female + bachelor + 
                    fem_bac, se_type = "stata")
reg7 <- lm_robust(ln_ahe ~ age + age2 + bac_age + bac_age2 + female + bachelor + 
                    fem_bac, se_type = "stata")
reg8 <- lm_robust(ln_ahe ~ age + age2 + fem_age + fem_age2 + bac_age + 
                    bac_age2 + female + bachelor + fem_bac, se_type = "stata")

# To run texreg, please make sure you have RMarkdown installed in your computer
# and MiKTeX for Windows or MacTeX for Mac
texreg(list(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8), include.ci = F,
       caption.above = T, digits = 3, caption = "Earnings and Age, 2012",
       custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)"))

screenreg(list(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8), include.ci = F,
       caption.above = T, digits = 3, caption = "Earnings and Age, 2012",
       custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)"))

## (a)
summary(reg1)

# The regression results for this question are shown in column (1) of Table 2.
# If age increases from 25 to 26, earnings are predicted to increase by $0.51
# per hour. If age increases from 33 to 34, earnings are predicted to increase
# by $0.51 per hour. These values are the same because the regression is a
# linear function relating ahe and age.

## Extra code to confirm results:
# predict(reg1, newdata = data.frame(
#   age = 26,
#   female = mean(female),
#   bachelor = mean(bachelor)
# )) -
#   predict(reg1, newdata = data.frame(
#     age = 25,
#     female = mean(female),
#     bachelor = mean(bachelor)
#   ))
# predict(reg1, newdata = data.frame(
#   age = 34,
#   female = mean(female),
#   bachelor = mean(bachelor)
# )) -
#   predict(reg1, newdata = data.frame(
#     age = 33,
#     female = mean(female),
#     bachelor = mean(bachelor)
#   ))


## (b)
summary(reg2)

# The regression results for this question are shown in column(2) of Table 2. 
# If age increases from 25 to 26, ln(ahe) is predicted to increase by 0.026, so 
# earnings are predicted to increase by 2.6%. If age increases from 33 to 34, 
# ln(ahe) is predicted to increase by 0.026, earnings are predicted to increase 
# by 2.6%. These values, in percentage terms, are the same because the 
# regression is a linear function relating ln(ahe) and age.

## Extra code to confirm results:
# predict(reg2, newdata = data.frame(
#   age = 26,
#   female = mean(female),
#   bachelor = mean(bachelor)
# )) -
#   predict(reg2, newdata = data.frame(
#     age = 25,
#     female = mean(female),
#     bachelor = mean(bachelor)
#   ))
# predict(reg2, newdata = data.frame(
#   age = 34,
#   female = mean(female),
#   bachelor = mean(bachelor)
# )) -
#   predict(reg2, newdata = data.frame(
#     age = 33,
#     female = mean(female),
#     bachelor = mean(bachelor)
#   ))
# 
# exp(predict(reg2, newdata = data.frame(
#   age = 26,
#   female = mean(female),
#   bachelor = mean(bachelor)
# ))) -
#   exp(predict(reg2, newdata = data.frame(
#     age = 25,
#     female = mean(female),
#     bachelor = mean(bachelor)
#   )))
# exp(predict(reg2, newdata = data.frame(
#   age = 34,
#   female = mean(female),
#   bachelor = mean(bachelor)
# ))) -
#   exp(predict(reg2, newdata = data.frame(
#     age = 33,
#     female = mean(female),
#     bachelor = mean(bachelor)
#   )))

# Natural log (ln) is log using Euler's number (e) as base.
# (e) is the constant of proportional growth/proportionality.
# Good video explaining the Euler number (e) - Eddie Woo
# https://youtu.be/pg827uDPFqA


## (c)
summary(reg3)

log(26) - log(25)
log(34) - log(33)
reg3$coefficients[2]*(log(26) - log(25))
reg3$coefficients[2]*(log(34) - log(33))

# The regression results for this question are shown in column (3) of Table 2. 
# If age increases from 25 to 26, then ln(age) has increased by 
# ln(26) - ln(25) = 0.0392 (or 3.92%). The predicted increase in ln(ahe) is
# 0.75 x 0.0392 = 0.029. This means that earnings are predicted to increase 
# by 2.9%. If age increases from 34 to 35, then ln(age) has increased by 
# ln(35) - ln(34) = 0.02985 (or 2.99%). The predicted increase in ln(ahe) is
# 0.75 x 0.0299 = 0.0224. This means that earnings are predicted to increase by 2.2%.

## Extra code to confirm results:
# predict(reg3, newdata = data.frame(
#   ln_age = log(26),
#   female = mean(female),
#   bachelor = mean(bachelor)
# )) -
#   predict(reg3, newdata = data.frame(
#     ln_age = log(25),
#     female = mean(female),
#     bachelor = mean(bachelor)
#   ))
# predict(reg3, newdata = data.frame(
#   ln_age = log(34),
#   female = mean(female),
#   bachelor = mean(bachelor)
# )) -
#   predict(reg3, newdata = data.frame(
#     ln_age = log(33),
#     female = mean(female),
#     bachelor = mean(bachelor)
#   ))
# 
# exp(predict(reg3, newdata = data.frame(
#   ln_age = log(26),
#   female = mean(female),
#   bachelor = mean(bachelor)
# ))) -
#   exp(predict(reg3, newdata = data.frame(
#     ln_age = log(25),
#     female = mean(female),
#     bachelor = mean(bachelor)
#   )))
# exp(predict(reg3, newdata = data.frame(
#   ln_age = log(34),
#   female = mean(female),
#   bachelor = mean(bachelor)
# ))) -
#   exp(predict(reg3, newdata = data.frame(
#     ln_age = log(33),
#     female = mean(female),
#     bachelor = mean(bachelor)
#   )))



## (d)
summary(reg4)

# The regression results for this question are shown in column (4) of Table 2. 
# When age increases from 25 to 26, the predicted change in ln(ahe) is
# (0.104 x 26 - 0.0013 x 26^2) - (0.104 x 25 - 0.0013 x 25^2) = 0.036.
# This means that earnings are predicted to increase by 3.6%. When age increases 
# from 33 to 34, the predicted change in ln(ahe) is
# (0.104 x 34 - 0.0013 x 34^2) - (0.104 x 33 - 0.0013 x 33^2) = 0.015.
# This means that earnings are predicted to increase by 1.5%.

## Extra code to confirm results:
# (reg4$coefficients[2]*(26) + reg4$coefficients[3]*(26^2)) - 
#   (reg4$coefficients[2]*(25) + reg4$coefficients[3]*(25^2))
# (reg4$coefficients[2]*(35) + reg4$coefficients[3]*(35^2)) - 
#   (reg4$coefficients[2]*(34) + reg4$coefficients[3]*(34^2))
# 
# exp(reg4$coefficients[2]*(26) + reg4$coefficients[3]*(26^2)) -
#   exp(reg4$coefficients[2]*(25) + reg4$coefficients[3]*(25^2))
# exp(reg4$coefficients[2]*(34) + reg4$coefficients[3]*(34^2)) -
#   exp(reg4$coefficients[2]*(33) + reg4$coefficients[3]*(33^2))

## (e)
summary(reg2)
summary(reg3)

# The regressions differ in their choice of one of the regressors. The two sets 
# of estimates are overall very similar. They can be compared on the basis of 
# the adj_R^2. The regression in (c) has a (marginally) higher adj_R^2 
# (0.1962 vs. 0.1961), so it is preferred.

## (f)
summary(reg2)
summary(reg4)

# The regression in (d) adds the variable age^2 to regression (b). The 
# coefficient on age^2 is not statistically significant at the 5% level and the 
# estimated coefficient is very close to zero. This suggests that the 
# regression in (b) is preferred to that in (d). However, the regressions are 
# so similar that either may be used.

## (g)
summary(reg3)
summary(reg4)

# The regressions differ in their choice of the regressors (ln(age) in (c) and 
# age and age^2 in (d)). They can be compared on the basis of the adj_R^2. The 
# regression in (d) has a (marginally) higher adj_R^2 (0.1963 vs. 0.1962), 
# so it is preferred.

## (h)
age <- seq(25, 34, by = 1)
ln_ageb <- 1.941 + 0.0255*age
ln_agec <- 0.150 + 0.753*log(age)
ln_aged <- 0.792 + 0.104*age - 0.00133*age^2
datab <- data.frame(ln_age = ln_ageb, age = age, data = "b")
datac <- data.frame(ln_age = ln_agec, age = age, data = "c")
datad <- data.frame(ln_age = ln_aged, age = age, data = "d")
data.bcd <- rbind.data.frame(datab, datac, datad)

fig8.2 <- ggplot(data.bcd, aes(x = age, y = ln_age)) +
  geom_line(aes(col = data), linewidth = 0.8) +
  labs(title = "Figure 2: Regression Lines (2) - (4)",
       x = "Age", y = "log of AHE") +
  theme(axis.title = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, size = 12, family = "serif",
                                  face = "bold"),
        legend.position.inside = c(0.15, 0.85),
        legend.title = element_blank(),
        legend.text = element_text(family = "serif", face = "bold"),
        legend.key = element_rect(color = "transparent"),
        legend.background = element_rect(fill = "lightgrey",
                                         linewidth = 0.8,
                                         linetype="solid")) +
  scale_color_discrete(name = "Model", labels = c(" Regression (2)",
                                                  " Regression (3)",
                                                  " Regression (4)"))
print(fig8.2)

# See Figure 2. The regression functions are very similar. The quadratic 
# regression shows somewhat more curvature than the log-log regression, but the 
# difference is small. The regression functions for a female with a high school 
# diploma will look just like these, but they will be shifted by the amount of 
# the coefficient on the binary regressor female. The regression functions for 
# workers with a bachelor degree will also look just like these, but they would 
# be shifted by the amount of the coefficient on the binary variable bachelor.

## (i)
summary(reg5)

# This regression is shown in column (5) of Table 2. The coefficient on the 
# interaction term female x bachelor shows the "extra effect" of bachelor on 
# ln(ahe) for women relative the effect for men.
# 
# Predicted values of ln(ahe):
# Alexis: 0.104 x 30 - 0.0013 x 30^2 - 0.24 x 1 + 0.40 x 1 + 0.090 x 1 + 0.80 = 3.00
# Jane: 0.104 x 30 - 0.0013 x 30^2 - 0.24 x 1 + 0.40 x 0 + 0.090 x 0 + 0.80 = 2.51
# Bob: 0.104 x 30 - 0.0013 x 30^2 - 0.24 x 0 + 0.40 x 1 + 0.090 x 0 + 0.80 = 3.15
# Jim: 0.104 x 30 - 0.0013 x 30^2 - 0.24 x 0 + 0.40 x 0 + 0.090 x 0 + 0.80 = 2.75
# Difference in ln(ahe): Alexis - Jane = 3.00-2.51 = 0.49.
# Difference in ln(ahe): Bob - Jim = 3.15-2.75 = 0.40
# Notice that the difference in the differences of the predicted effects is 0.49 
# - 0.40 = 0.09, which is the value of the coefficient on the interaction term.

## Extra code to confirm results:
# (Alexis <- predict(reg5, newdata = data.frame(
#   age = 30,
#   age2 = 30^2,
#   female = 1,
#   bachelor = 1,
#   fem_bac = 1
# )))
# 
# (Jane <- predict(reg5, newdata = data.frame(
#   age = 30,
#   age2 = 30^2,
#   female = 1,
#   bachelor = 0,
#   fem_bac = 0
# )))
# 
# (Bob <- predict(reg5, newdata = data.frame(
#   age = 30,
#   age2 = 30^2,
#   female = 0,
#   bachelor = 1,
#   fem_bac = 0
# )))
# 
# (Jim <- predict(reg5, newdata = data.frame(
#   age = 30,
#   age2 = 30^2,
#   female = 0,
#   bachelor = 0,
#   fem_bac = 0
# )))
# 
# Alexis - Jane
# Bob - Jim

## (j)
summary(reg6)
linearHypothesis(reg6, c("fem_age = 0", "fem_age2 = 0"), test = c("F"))

# This regression is shown in column (6) of Table 2, which includes two 
# additional regressors: the interactions of female and the age variables, age 
# and age^2. The F-statistic testing the restriction that the coefficients on 
# these interaction terms is equal to zero is 4.14 with a p-value of 0.016. This 
# implies that there is statistically significant evidence (at the 5% but not 1% 
# level) that there is a different effect of age on ln(ahe) for men and women.

## (k)
summary(reg7)
linearHypothesis(reg7, c("bac_age = 0", "bac_age2 = 0"), test = c("F"))

# This regression is shown in column (7) of Table 2, which includes two 
# additional regressors that are interactions of bachelor and the age variables, 
# age and age2. The F-statistic testing the restriction that the coefficients on 
# these interaction terms is zero is 1.30 with a p-value of 0.273. This implies 
# that there is no statistically significant evidence (at the 10% level) that 
# there is a different effect of age on ln(ahe) for high school and college 
# graduates.

## (l)
# The R code below generates the first row of Table 3 from your answer sheet. As 
# you see here it repeats an identical job four times with different numbers. 
# Whenever you are repeating the same thing over and over again in a programming 
# language, you should assume that there should be a more efficient way.


AGE = 25
FEMALE = 1
BACHELOR = 0
fehigh25 = predict(
  reg8,
  newdata = data.frame(
    age = AGE,
    age2 = AGE ^ 2,
    fem_age = FEMALE * AGE,
    fem_age2 = FEMALE * AGE ^ 2,
    bac_age = BACHELOR * AGE,
    bac_age2 = BACHELOR * AGE ^ 2,
    female = FEMALE,
    bachelor = BACHELOR,
    fem_bac = FEMALE * BACHELOR
  )
)
AGE = 32
FEMALE = 1
BACHELOR = 0
fehigh32 = predict(
  reg8,
  newdata = data.frame(
    age = AGE,
    age2 = AGE ^ 2,
    fem_age = FEMALE * AGE,
    fem_age2 = FEMALE * AGE ^ 2,
    bac_age = BACHELOR * AGE,
    bac_age2 = BACHELOR * AGE ^ 2,
    female = FEMALE,
    bachelor = BACHELOR,
    fem_bac = FEMALE * BACHELOR
  )
)
AGE = 34
FEMALE = 1
BACHELOR = 0
fehigh34 = predict(
  reg8,
  newdata = data.frame(
    age = AGE,
    age2 = AGE ^ 2,
    fem_age = FEMALE * AGE,
    fem_age2 = FEMALE * AGE ^ 2,
    bac_age = BACHELOR * AGE,
    bac_age2 = BACHELOR * AGE ^ 2,
    female = FEMALE,
    bachelor = BACHELOR,
    fem_bac = FEMALE * BACHELOR
  )
)

fehigh25
fehigh32
fehigh34
incr25to32 = (fehigh32 - fehigh25) / (32 - 25) * 100
incr25to32
incr32to34 = (fehigh34 - fehigh32) / (34 - 32) * 100
incr32to34

# The estimated regressions suggest that earnings increase as workers age from 
# 25-34, the range of age studied in this sample. Gender and education are 
# significant predictors of earnings, and there are statistically significant 
# interaction effects between age and gender and between gender and education. 
# Earnings for those with a college education are higher than those with a high 
# school degree, and earnings of the college educated increase more rapidly 
# early in their careers (age 25-34). Earnings for men are higher than those of 
# women, and earnings of men increase more rapidly early in their careers (age 
# 25-34). For all categories of workers (men/women, high school/college) 
# earnings increase more rapidly from age 25-32 than from 32-34. While the 
# percentage increase in women's earning is similar to the percentage increase 
# for men from age 25-32, women's earning tend to stagnate from age 32-34, 
# while men's continues to increase.

detach(CPS12)
