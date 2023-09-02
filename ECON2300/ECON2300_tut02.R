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


### SW E4.1

# Set working directory (make sure you edit to your own WD)
# Ex Win: setwd("G:/My Drive/BEcon/TUTOR/ECON2300/02")
# Ex Mac: setwd("/Users/uqdkim7/Dropbox/Teaching/R tutorials/Tutorial02")


# To use the following line: 
# save this file in the same directory as the data files
# install.packages("rstudioapi")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Clean Working Environment
rm(list = ls())

# load csv data
Growth <- read_csv("Growth.csv")

## (a)
# set up figure with data and aes
fig1 <- ggplot(Growth, aes(tradeshare, growth)) +
  # add "point" geom and modify it
  geom_point(alpha = .75, size = 1.5, colour = "cyan4") +
  # add title and axis labels
  labs(title = "Figure 1: Growth Rate and Trade Share",
        x = "Trade Share", y = "Growth Rate") +
  # modify theme characteristics
  theme(axis.title = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, family = "serif",
                                  face = "bold", size = 10))

print(fig1)

# Yes, there appears to be a weak positive relationship.

## (b)
fig2 <- ggplot(Growth, aes(tradeshare, growth)) +
  geom_point(alpha = .75, size = 1.5, colour = "cyan4") +
  labs(title = "Figure 2: Growth Rate and Trade Share",
       x = "Trade Share", y = "Growth Rate") +
  theme(axis.title = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, size = 10, family = "serif",
                                  face = "bold")) +
  # annotate Malta
  annotate("text", family = "serif", size = 3,
           x = Growth$tradeshare[Growth$country_name == "Malta"],
           y = Growth$growth[Growth$country_name == "Malta"] - .5,
           label = "Malta")

print(fig2)

# Malta is the “outlying” observation with a trade share of 2.

## (c)
# regression with robust standard errors
reg1 = lm_robust(growth ~ tradeshare, data = Growth, se_type = "stata")
# present output table
summary(reg1)

# The estimated slope is 2.3064 (tradeshare estimate)
# The estimated intercept is 0.6403 (intercept estimate)

# prediction based on regression model
predict(reg1, newdata = data.frame(tradeshare = c(0.5, 1)))

# Tradeshare of 0.5 => 1.793482 growth
# Tradeshare of 1.0 => 2.946699 growth

## (d)
reg2 = lm_robust(growth ~ tradeshare, 
                 data = subset(Growth, country_name != "Malta"), 
                 se_type = "stata")
summary(reg2)

# The estimated slope is 1.6809 (tradeshare estimate)
# The estimated intercept is 0.9574 (intercept estimate)

# prediction based on regression model
predict(reg2, newdata = data.frame(tradeshare = c(0.5, 1)))

# Tradeshare of 0.5 => 1.797863 growth
# Tradeshare of 1.0 => 2.638315 growth

## (e)
fig3 <- ggplot(Growth, aes(tradeshare, growth)) +
  geom_point(alpha = .5, size = 1.5, colour = "cyan4") +
  labs(title = "Figure 3: Growth Rate and Trade share",
       x = "Trade Share", y = "Growth Rate") +
  theme(axis.title = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, size = 10, family = "serif",
                                  face = "bold")) +
  # annotate Malta
  annotate("text", family = "serif", size = 3,
           x = Growth$tradeshare[Growth$country_name == "Malta"],
           y = Growth$growth[Growth$country_name == "Malta"] - .5,
           label = "Malta") +
  
  # fitted line using all data
  geom_smooth(method = "lm", se = TRUE, size = 0.75, colour = "violetred3") +
  #fitted line using all but Malta data
  geom_smooth(data = subset(Growth, country_name != "Malta"),
              method = "lm", se = FALSE, size = 0.75, colour = "dodgerblue3")

print(fig3)

# The data point of Malta is far away from the cloud of other points with very
# high trade share and growth rate. As the sample size is small, such an outlier
# (influential point) can greatly affect the slope of the regression line. In
# this case, as the growth rate of Malta is higher than most other countries,
# including Malta makes the regression line steeper.


## (f)

# Malta is an island nation in the Mediterranean Sea, south of Sicily. Malta is
# a freight transport site, which explains its large "trade share." Many goods
# coming into Malta (imports into Malta) are immediately transported to other
# countries (as exports from Malta). Thus, Malta's imports and exports are
# unlike the imports and exports of most other countries. Malta should not be
# included in the analysis.

# https://www.google.com/maps/place/Malta/


### SW E4.2

# Clean Working Environment
rm(list = ls())

# load csv data
EH <- read_csv("Earnings_and_Height.csv")


## (a)
describe(EH$height, descript = "height")

# The median value height is 67 inches, represented by the quantile 0.5.


## (b)
describe(EH$earnings[EH$height <= 67], descript = "earnings for height <= 67")

describe(EH$earnings[EH$height > 67], descript = "earnings for height > 67")

# unpair two-Samples t-test
t.test(EH$earnings[EH$height <= 67], EH$earnings[EH$height > 67])

# The estimated average annual earnings for shorter workers is $44,488, is
# $49,988 for taller workers, for a difference of $5,499. The 95% confidence
# interval is $4,706 to $6,293. The difference is large (more than 10% of
# average earnings), precisely estimated and statistically significantly
# different from zero (p-value is essentially zero).


## (c)
fig4 <- ggplot(EH, aes(height, earnings)) +
  geom_point(alpha = .75, size = 1.5, colour = "dodgerblue3") +
  labs(title = "Figure 4: Earnings and Height",
       x = "Height", y = "Earnings") +
  theme(axis.title = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, family = "serif",
                                  face = "bold", size = 10))+
  geom_smooth(method = "lm", se = TRUE, size = 0.75, colour = "violetred3")
print(fig4)

# The data documentation reports that individual earnings were reported in 23
# brackets, and a single average value is reported for earnings in the same
# bracket. Thus, the dataset contains 23 distinct values of earnings.


## (d)
reg3 <- lm_robust(earnings ~ height, data = EH, se_type = "stata")
summary(reg3)

predict(reg3, newdata = data.frame(height = c(65, 67, 70)))

# The estimated slope is 707.7 ($ per year). The estimated earnings are
# Height in inches earnings \ in $ per year
# 65 -> 45,486
# 67 -> 46,901
# 70 -> 49,024


## (e)
# create the conversion variable 1 cm = 0.394 inches
inches_to_cm <- 0.394

# create a variable with heights in cms
Heights <- EH$height / inches_to_cm

# describe the heights
describe(Heights)

# linear regression using heights in cm instead of inches
reg <- lm_robust(earnings ~ Heights, data = EH, se_type = "stata")
summary(reg)

# summary of the regression in inches for comparison
summary(reg3)

# (i) the estimated slope changes from 707.7 to 278.8, as now it is being
# multiplied by height in cm. 
# (ii) the estimated intercept remains the same (512.7) as it assumes height = 0
# (iii) the R^2 is unit free so it remains the same (0.01088).

sqrt(reg$res_var)
sqrt(reg3$res_var)

# (iv) the standard error of the regression remains the same $26,777.

## (f)
reg4 <- lm_robust(earnings ~ height, 
                  data = subset(EH, sex == "0:female"), 
                  se_type = "stata")

summary(reg4)

# (i) The estimated slope is 511.2.
# (ii) A women who is one inch taller than average is predicted to have earnings
# that are $511.2 per year higher than average.


## (g)
reg5 <- lm_robust(earnings ~ height, 
                  data = subset(EH, sex == "1:male"), 
                  se_type = "stata")

summary(reg5)

# (i) The estimated slope is 1307.
# (ii) A man who is one inch taller than average is predicted to have earnings
# that are $1307 per year higher than average.

## (h)

# Height may be correlated with other factors that cause earnings. For example,
# height may be correlated with "strength," and in some occupations, stronger
# workers may by more productive. There are many other potential factors that
# may be correlated with height and cause earnings and you will investigate of
# these in future exercises.
