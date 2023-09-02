# Tutorial 7: Assessing Studies Based on Multiple Regression

# Answers by Dong-Hyuk Kim
# https://sites.google.com/site/kimdonghyuk000/home
# Extra material by Francisco Tavares Garcia
# https://tavaresgarcia.github.io/

# install.packages("readr")
# install.packages("dplyr")
# install.packages("estimatr")
# install.packages("texreg")

library(readr)    # package for fast read rectangular data
library(dplyr)    # package for data manipulation
library(estimatr) # package for commonly used estimators with robust SE
library(texreg)   # package for converting R regression output to LaTeX tables


### SW E9.1

# House cleaning
rm(list = ls()) # clear Environment
if (!is.null(dev.list())) dev.off() # clear Plots
cat("\014") # clear Console

# Set working directory (make sure you edit to your own WD)
# Ex Win: setwd("G:/My Drive/BEcon/TUTOR/ECON2300/04")
# Ex Mac: setwd("/Users/uqdkim7/Dropbox/Teaching/R tutorials/Tutorial04")

# To use the following line: save this file in the same folder as the data files
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

CPS12 <- read_csv("cps12.csv") %>%
  mutate(ln_ahe = log(ahe),
         ln_age = log(age),
         age2 = age*age,
         fem_bac = female*bachelor,
         fem_age = female*age,
         fem_age2 = female*age2,
         bac_age = bachelor*age,
         bac_age2 = bachelor*age2)
attach(CPS12)

reg1 = lm_robust(ahe ~ age + female + bachelor, 
                 se_type = "stata")
reg2 = lm_robust(ln_ahe ~ age + female + bachelor, 
                 se_type = "stata")
reg3 = lm_robust(ln_ahe ~ ln_age + female + bachelor, data = CPS12, 
                 se_type = "stata")
reg4 = lm_robust(ln_ahe ~ age + age2 + female + bachelor,
                 se_type = "stata")
reg5 = lm_robust(ln_ahe ~ age + age2 + female + bachelor + fem_bac,
                 data = CPS12, se_type = "stata")
reg6 = lm_robust(ln_ahe ~ age + age2 + fem_age + fem_age2 + female + 
                   bachelor + fem_bac, se_type = "stata")
reg7 = lm_robust(ln_ahe ~ age + age2 + bac_age + bac_age2 + female +
                   bachelor + fem_bac, se_type = "stata")
reg8 = lm_robust(ln_ahe ~ age + age2 + fem_age + fem_age2 + bac_age + bac_age2 + 
                   female + bachelor + fem_bac, se_type = "stata")

# To run texreg, please make sure you have RMarkdown installed in your computer
# and MiKTeX for Windows or MacTeX for Mac
texreg(list(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8),
       include.ci = F, caption.above = T,
       digits = 3, caption = "Earnings and Age, 2012",
       custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)"))


## (a)

# - Omitted variables: There is the potential for omitted variable bias when a 
# variable is excluded from the regression that (i) has an effect on ln(ahe) and 
# (ii) is correlated with a variable that is included in the regression. There 
# are several candidates. The most important is a worker's Ability. Higher 
# ability workers will, on average, have higher earnings and are more likely to 
# go to college. Leaving Ability out of the regression may lead to omitted 
# variable bias, particularly for the estimated effect of education on earnings. 
# Also omitted from the regression is Occupation. Two workers with the same 
# education (a BA for example) may have different occupations (accountant versus 
# 3rd grade teacher) and have different earnings. To the extent that occupation 
# choice is correlated with gender, this will lead to omitted variable bias. 
# Occupation choice could also be correlated with Age. Because the data are a 
# cross section, older workers entered the labour force before younger workers, 
# and their occupation reflects, in part, the state of the labour market when 
# they entered the labour force.
# 
# 
# - Misspecification of the functional form: This was investigated carefully in 
# (a)-(k). There does appear to be a nonlinear effect of Age on earnings, which 
# is adequately captured by the polynomial regression with interaction terms.
# 
# 
# - Errors-in-variables: Age is included in the regression as a "proxy" for 
# experience. Workers with more experience are expected to earn more because 
# their productivity increases with experience. But Age is an imperfect measure 
# of experience. (One worker might start his career at age 22, while another 
# might start at age 25. Or, one worker might take a year off to start a family, 
# while another might not). There is also potential measurement error in AHE as 
# these data are collected by retrospective survey in which workers in March 
# 2013 are asked about their average earnings in 2012.
# 
# 
# - Sample selection: The data are full-time workers only, so there is potential 
# for sample-selection bias.
# 
# 
# - Simultaneous causality: This is unlikely to be a problem. It is unlikely 
# that AHE affects Age or gender.
# 
# 
# - Inconsistency of OLS standard errors: Heteroskedastic robust standard errors 
# were used in the analysis, so that heteroskedasticity is not a concern. The 
# data are collected, at least approximately, using i.i.d. sampling, so that 
# correlation across the errors is unlikely to be a problem.


## (b)

# First, generate the table from the previous tutorial. The results will be
TABLE3 <- matrix(0L, nrow = 4, ncol = 5)
AGEs = c(25,32,34)
i = 1;
for(BACHELOR in 0:1){
  for(FEMALE in c(1,0)){
    for(j in 1:3){
      AGE = AGEs[j]
      TABLE3[i,j] <- predict(reg8, newdata = data.frame(
        age = AGE, age2 = AGE^2,
        fem_age = FEMALE * AGE, fem_age2 = FEMALE * AGE^2,
        bac_age = BACHELOR * AGE, bac_age2 = BACHELOR * AGE^2,
        female = FEMALE, bachelor = BACHELOR, fem_bac = FEMALE * BACHELOR
      )
      )
      if(j >= 2){ TABLE3[i,j+2] <- (TABLE3[i,j] - TABLE3[i,j-1]) / 
        (AGEs[j] - AGEs[j-1]) * 100}
      if(j == 3){i = i + 1}
    }
  }
}
TABLE3

detach(CPS12)

# Clean Working Environment
rm(list = ls())

CPS <- read_csv("cps92_12.csv") %>%
  mutate(cpi = 140.3*(year == 1992) + 229.6*(year == 2012),
         ahe12 = (ahe/cpi)*229.6) %>%
  mutate(ln_ahe12 = log(ahe12),
         ln_age = log(age),
         age2 = age*age,
         fem_bac = female*bachelor,
         fem_age = female*age,
         fem_age2= female*age2,
         bac_age = bachelor*age,
         bac_age2= bachelor*age2) %>%
  filter(year == 1992)
attach(CPS)

# List of Regressions
reg1 = lm_robust(ahe12 ~ age + female + bachelor, 
                 se_type = "stata")
reg2 = lm_robust(ln_ahe12 ~ age + female + bachelor, 
                 se_type = "stata")
reg3 = lm_robust(ln_ahe12 ~ ln_age + female + bachelor, 
                 se_type = "stata")
reg4 = lm_robust(ln_ahe12 ~ age + age2 + female + bachelor,
                 se_type = "stata")
reg5 = lm_robust(ln_ahe12 ~ age + age2 + female + bachelor + fem_bac,
                 se_type = "stata")
reg6 = lm_robust(ln_ahe12 ~ age + age2 + fem_age + fem_age2 + female +
                   bachelor + fem_bac, se_type = "stata")
reg7 = lm_robust(ln_ahe12 ~ age + age2 + bac_age + bac_age2 + female +
                   bachelor + fem_bac, se_type = "stata")
reg8 = lm_robust(ln_ahe12 ~ age + age2 + fem_age + fem_age2 + bac_age + 
                   bac_age2 + female + bachelor + fem_bac, se_type = "stata")

# To run texreg, please make sure you have RMarkdown installed in your computer
# and MiKTeX for Windows or MacTeX for Mac
texreg(list(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8),
       include.ci = F, caption.above = T,
       digits = 3, caption = "Earnings and Age, 1992",
       custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)"))


TABLE4 <- matrix(0L, nrow = 4, ncol = 5)
AGEs = c(25,32,34)
i = 1;
for(BACHELOR in 0:1){
  for(FEMALE in c(1,0)){
    for(j in 1:3){
      AGE = AGEs[j]
      TABLE4[i,j] <- predict(reg8, newdata = data.frame(
        age = AGE, age2 = AGE^2,
        fem_age = FEMALE * AGE, fem_age2 = FEMALE * AGE^2,
        bac_age = BACHELOR * AGE, bac_age2 = BACHELOR * AGE^2,
        female = FEMALE, bachelor = BACHELOR, fem_bac = FEMALE * BACHELOR
      )
      )
      if(j >= 2){ TABLE4[i,j+2] <- (TABLE4[i,j] - TABLE4[i,j-1]) / 
        (AGEs[j] - AGEs[j-1]) * 100}
      if(j == 3){i = i + 1}
    }
  }
}
TABLE4

# We run the same set of regressions using the 1992 data and present estimation
# results in Table 2. It turns out that the two sets of regression results are
# overall very similar to each other.
#
# Based on the 2012 data E8.2 (l) concluded: Earnings for those with a college
# education are higher than those with a high school degree, and earnings of the
# college educated increase more rapidly early in their careers (age 25-34).
# Earnings for men are higher than those of women, and earnings of men increase
# more rapidly early in their careers (age 25-34). For all categories of workers
# (men/women, high school/college) earnings increase more rapidly from age 25-32
# than from 32-34. While the percentage increase in women's earning is similar
# to the percentage increase for men from age 25-32, women's earning tend to
# stagnate from age 32-34, while men's continues to increase.
#
# All of these conclusions continue to hold for the 1992 data (although the
# precise values for the differences change somewhat.)

detach(CPS)


### SW E9.2

# Clean Working Environment
rm(list = ls())

BW <- read_csv("birthweight_smoking.csv") %>%
  mutate(young = as.numeric(age <= 20),
         m_ed1 = as.numeric(educ < 12),
         m_ed2 = as.numeric(educ == 12),
         m_ed3 = (educ > 12)*(educ < 16),
         m_ed4 = as.numeric(educ == 16),
         m_ed5 = as.numeric(educ > 16),
         age2 = age*age,
         smoker_age = smoker*age,
         smoker_young = smoker*young)
attach(BW)

# List of Regressions
reg1 = lm_robust(birthweight ~ smoker + alcohol + nprevist + unmarried,
                 se_type = "stata")
reg2 = lm_robust(birthweight ~ smoker + alcohol + nprevist + unmarried + age + 
                   educ, se_type = "stata")
reg3 = lm_robust(birthweight ~ smoker + alcohol + nprevist + unmarried + age +
                   m_ed2 + m_ed3 + m_ed4 + m_ed5,
                 se_type = "stata")
reg4 = lm_robust(birthweight ~ smoker + alcohol + nprevist + unmarried + m_ed2 +
                   m_ed3 + m_ed4 + m_ed5 + young,
                 se_type = "stata")
reg5 = lm_robust(birthweight ~ smoker + alcohol + nprevist + unmarried + age + 
                   age2, se_type = "stata")
reg6 = lm_robust(birthweight ~ smoker + alcohol + nprevist + unmarried + age +
                   smoker_age, se_type = "stata")
reg7 = lm_robust(birthweight ~ smoker + alcohol + nprevist + unmarried + young +
                   smoker_young, se_type = "stata")

# To run texreg, please make sure you have RMarkdown installed in your computer
# and MiKTeX for Windows or MacTeX for Mac
texreg(list(reg1, reg2, reg3, reg4, reg5, reg6, reg7), include.ci = F, 
       caption.above = T, digits = 2, caption = "Birth Weight and Smoking",
       custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)"))

## (a)

# (i)
summary(reg1)
summary(reg2)
# The table shows various regressions. Regressions (1) and (2) were used in the 
# answers to E7.1. They suggested a 95% confidence interval of the effect of 
# smoking on birth weight that ranged from (roughly) -230 to -120 grams. 
# 
summary(reg3)
# Regression (3) changes the education control and uses binary variables for a 
# high school diploma (12 years of education), some college (12 < years of 
# education < 16), a bachelor's degree (years of education = 16), and graduate 
# work (years of education > 16), and where "years of education < 12" is the 
# omitted category. 
# 
summary(reg4)
# Regression (4) additionally changes age2 to the binary variable Young 
# (Age <= 20). 
# 
summary(reg5)
# Regression (5) drops the education variables (which are not statistically 
# significant in (3) and (4)) and adds age2 to check for nonlinear effect of age 
# on birth weight (which is insignificant). These modifications have little 
# effect on the estimated effect of smoking on birth weight.
# 
summary(reg6)
summary(reg7)
# Regressions (6) and (7) investigate potential interaction effects of smoking 
# and age. Both regressions suggest a significant interaction effect, with the 
# effect of smoking on birth weight is larger (that is, more negative) for older 
# mothers. For example, from (6), the estimated effect of smoking on birth 
# weight is 275.6 - 17.7 x 20 = -78.4 grams for a 20-year old mother, but is 
# 275.6 - 17.7 x 30 = -255.5 grams for a 30-year old mother.

# (ii)
# - Omitted variables: There is the potential for omitted variable bias when a
# variable is excluded from the regression that (i) has an effect on birth
# weight and (ii) is correlated with smoking. There are several candidates.
# First, the dataset does not contain data on race and ethnicity and to the
# extent that these are related to birth weight and smoking, then they are
# potential omitted variables. There are other environmental variables such as
# mother's diet, exercise, and so forth that may affect birth weight and be
# correlated with smoking. These too are potential omitted variables. The size
# and significance of unmarried suggests that it is an important control
# variable, but it is undoubtedly an imperfect control.
#
# - Misspecification of the functional form: The regressions reported above
# suggest that an important nonlinearity arises from the interaction smoking and
# mother's age. Other nonlinearities do not seem to be important.
#
# - Errors-in-variables: All of the variables except birthweight are self -
# reported by the mother and may contain error. For example, some mothers may be
# reticent to respond that they smoked or drank during their pregnancy. How
# these kind of measurement errors affect the OLS estimates depends on the
# specifics of the measurement error, as discussed in Section 9.2.
#
# - Sample selection: The data are a random sample of all babies born in
# Pennsylvania in 1989, and thus there is no sample - selection bias.
#
# - Simultaneous causality: This is a problem to the extent that women who are
# more likely to have low - birth weight children are more likely to stop
# smoking during pregnancy. This would induce a positive correlation between the
# regression error, u, and the binary variable smoker, which would result in an
# upward bias in the OLS coefficient.
#
# - Inconsistency of OLS standard errors: Heteroskedastic robust standard errors
# were used in the analysis, so that heteroskedasticity is not a concern. The
# data are collected using i.i.d. sampling from all babies born in Pennsylvania
# in 1989, so that correlation across the errors is unlikely to be a problem.

## (b)
# To the extent that the OLS regression estimates the causal effect of smoking
# on birth weight, the results will be externally valid for these three
# populations. (Biology is the same in 1989 and 2015, and the same in
# Pennsylvania and Korea.) However, to the extent that the OLS estimate is
# influenced by omitted variable bias associated, for example, with other
# environmental factors (mother's diet, exercise, etc.), then the results may be
# different in these populations because the correlation between smoking and
# these omitted factors may differ.

detach(BW)