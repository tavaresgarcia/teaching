# Tutorial 1: R and Basic Operations

# Answers by Dong-Hyuk Kim
# https://sites.google.com/site/kimdonghyuk000/home

# Extra material by Francisco Tavares Garcia
# https://tavaresgarcia.github.io/


# House cleaning
rm(list = ls()) # clear Environment
if (!is.null(dev.list())) dev.off() # clear Plots
cat("\014") # clear Console

# Set working directory
# Ex Win: setwd("G:/My Drive/BEcon/TUTOR/ECON2300/01")
# Ex Mac: setwd("/Users/uqdkim7/Dropbox/Teaching/R tutorials/Tutorial01")
install.packages("rstudioapi")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# 1(a)
#
# The data is loaded using the R command "read.delim". R has a very large
# community of users and they exchange ideas, develop R and its packages, and
# answer each other's questions. Therefore, it is very easy to find ``how to''
# on the Internet. If you face a problem, there must be other people who already
# have struggled with the same issue. The answer should be there on the
# Internet.
#
# To learn how to load the file, for example, try googling a few key words like
# "R load data txt". One of the links should be:
#
# http://www.sthda.com/english/wiki/reading-data-from-txt-csv-files-r-base-functions
#
# This page provides a detailed explanation on how to open a text datafile and
# suggests using the command below. Once it is executed, point out that the data
# are stored in "mydata" in the "Environment" tab in the top-right window.
# Examining this tab shows that there are 10 observations and 2 variables in
# "mydata".



mydata <- read.delim("consumption.txt", header = TRUE,  sep = "")



# 1(b)
#
# Google a few key words like "R scatter plot" and look for a page that explains
# in detail how to make a scatter diagram. One such page (but necessarily the
# only) is:
#   
#   https://www.statmethods.net/graphs/scatterplot.html
# 
# As the webpage says, there are many ways to make a diagram in R, but the
# following is the simplest one.

attach(mydata)

plot(INC, CONS, main="Consumption Data",
     xlab="Income", ylab="Consumption", pch=19)

# Note that without attaching the data, we need to specify the dataset to which
# the variables belong--i.e., mydata$INC and mydata$CONS.
#
# R has many different ways to plot a diagram, providing a rich set of styling
# options. The internet is the best places to discover more.

# 1(c)
#
# The data are in the form of a matrix whose (8,1) element has the error. To
# visualise this, type "mydata" into the Console.

mydata[8,1] <- 90

# Although you changed the variable in the environment, the attached version
# remains the same. So we need to detach the old and attach the new version.
detach(mydata)
attach(mydata)

plot(INC, CONS, main="Consumption Data",
     xlab="Income", ylab="Consumption", pch=19)

# 1(d)

summary(mydata)

# 1(e)
#
# Income and consumption are strongly correlated.

cor(mydata)

# 1(f)
#
# Either "<-" or "=" can be used to create new variables. After creating the
# four new variables, they will also appear in the "Environment" tab.

DCONS <- 0.5 * CONS
LCONS <- log(CONS)
INC2 = INC^2
SQRTINC = sqrt(INC)

# 1(g)
#
# After executing rm(), check the "Environment" tab.

rm(DCONS, SQRTINC)

# 1(h)
#
# Note that to delete all variables, we can alternatively click on the broom in
# the "Environment" tab.

rm(list = ls())

# 2(a)
#
# The data are in ".dat" format, which is not very standard. If we open it using
# a text editor (e.g., notepad), we find that the file does not have a header
# (variable names), but the fifteen variables are all nicely aligned as if they
# are separated by tabs. What happens if we try to load the data as tab
# delimited by using the sep="tab" option? This results in only one variable
# being created, which is clearly wrong! (using, e.g., the "Environment" tab)
#
# Now, clear the workspace and reload the data but this time specifying the
# delimiter to be space.

fultonfish <- read.delim("fultonfish.dat", header = FALSE,  sep = "")

# There should now be 15 variables, as desired. By clicking on data item in the
# "Environment" tab, we can see the data in a spreadsheet.
#
# To rename the first four column, use the command "colnames". However, there
# are a number of other ways to rename variables. In fact, if we google a few
# relevant key words like "change variable names", a number of different
# approaches are uncovered. Some of them require adding a package. What are the
# advantages and disadvantages of using functions based on custom packages?
#
# Also, the "colnames" command works with the command "c". You can get help on
# an unknown command by typing "?c" into the Console prompt.

colnames(fultonfish)[1:4] <- c("date", "lprice", "quan", "lquan") 

# 2(b)

mean(fultonfish$quan)
sd(fultonfish$quan)

# 2(c)
#
# T-test

t.test(fultonfish$quan, mu = 7200)

# 2(d)
#
# Construct the 95% confidence interval using output in part (c). You get the
# same interval as what the "t.test" command provides.

# 2(e)
#
# Use the "cor" command to help analyse the relationship between price and
# quantity. It is important to consider simultaneity bias when inferring
# relationships between price and quantity data.

attach(fultonfish)
plot(lquan, lprice, 
     main = "Log Price and Log Quantity",
     xlab="log(Quantity)", 
     ylab="log(Price) of whiting per pound", 
     pch=19)

cor(lquan, lprice)

# 2(f)
#
# Save the workspace to a file on disk, both in the current folder and also by
# creating a subfolder. After the workspace is saved, reload it to pick up where
# you left off (i.e., close RStudio, then restart it and load the saved Rdata
# file).

save(list = ls(all = TRUE), file = "tutorial01.RData")