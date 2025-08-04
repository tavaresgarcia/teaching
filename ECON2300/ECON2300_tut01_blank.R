# Tutorial 1: R and Basic Operations

# Answers by Dong-Hyuk Kim
# https://sites.google.com/site/kimdonghyuk000/home

# Extra material by Francisco Tavares Garcia
# https://tavaresgarcia.github.io/


# House cleaning
rm(list = ls()) # clear Environment
if (!is.null(dev.list())) dev.off() # clear Plots
cat("\014") # clear Console

getwd()
# You will need to set the folder containing consumption.txt as your working
# directory.
setwd("D:/Users/fgraf/My Drive/BEcon/tutor/ECON2300/01")


### 1. The text file consumption.txt contains observations on the weekly family
# consumption expenditure (CONS) and income (INC) for a sample of 10 families.

# (a) Read the data into R.


# (b) Draw a scatter diagram of CONS against INC.


# (c) On checking the data, you find that your assistant has recorded the weekly
# consumption expenditure for Family 8 as $900 instead of $90. Correct this
# error and redraw the scatter diagram.


# (d) Compute the mean, median, maximum and minimum values of INC and CONS.


# (e) Compute the correlation coefficient between CONS and INC. Comment on the
#result.


# (f) Create the following new variables:
# DCONS = 0.5CONS,
# LCONS = log(CONS),
# INC2 = INC2,
# SQRTINC = √INC.


# (g) Delete the variables DCONS and SQRTINC.


# (h) Delete everything.


### 2. At the Famous Fulton Fish Market in New York city, sales of whiting (a
# type of fish) vary from day to day. Over a period of several months, daily
# quantities sold (in pounds) were observed. These data are in the file
# fultonfish.dat. Description of the data is in the file fultonfish.def. 
# Describe the first four columns.

# (a) Use R to open the data file and name the series in the first four columns
# as date, lprice, quan and lquan.


# (b) Compute the sample mean and standard deviation of the quantity sold
# (quan).


# (c) Test the null hypothesis that the mean quantity sold is equal to 7,200
# pounds a day at the 5% level of significance.


# (d) Construct the 95% confidence interval for part (c).


# (e) Plot lprice against lquan and label the variable lprice as “log(Price) of
# whiting per pound” and lquan as “log(Quantity)”. Then, comment on the nature
# of the relationship between these two variables.


# (f) Save this workfile to any folder on any drive.
