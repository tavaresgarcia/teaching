# Tutorial 01: R and Basic Operations

# Answers by Dong-Hyuk Kim
# https://sites.google.com/site/kimdonghyuk000/home
# Extra material by Francisco Tavares Garcia
# https://tavaresgarcia.github.io/

# Set working directory
# Ex Win: setwd("G:/My Drive/BEcon/TUTOR/ECON2300/11")
# Ex Mac: setwd("/Users/uqdkim7/Dropbox/Teaching/R tutorials/Tutorial11")
install.packages("rstudioapi")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Question 1
# (a)
mydata = read.delim("consumption.txt", header = TRUE, sep = "")

# (b)
attach(mydata)
plot(INC, CONS, 
     main = "Consumption Data", 
     xlab = "Income", 
     ylab = "Consumption", 
     pch = 19, 
     col = "hotpink")

# (c)
mydata[8,1] <- 90
detach(mydata)
attach(mydata)


# (d)
summary(mydata)

# (e)
cor(mydata)

# (f)
DCONS <- 0.5 * CONS
LCONS <- log(CONS)
INC2 <- INC^2
SQRTINC <- sqrt(INC)

# (g)
rm(DCONS, SQRTINC)

# (h)
rm(list = ls())


## Question 2

# (a)
fultonfish <- read.delim("fultonfish.dat", 
                         header = FALSE, sep = "")

colnames(fultonfish)[1:4] <- c("date", "lprice", "quan", "lquan")

# (b)
mean(fultonfish$quan)

attach(fultonfish)
mean(quan)
sd(quan)

# (c)
t.test(fultonfish$quan, mu = 7200)
# We reject the null hypothesis (H0)

# (d)
# [5574.717, 7094.617]

# (e)
plot(lquan, lprice, 
     main = "Log Price and Log Quantity",
     xlab = "log(Quantity)", 
     ylab = "log(Price) of whiting per pound",
     pch = 19, col = "darkgreen")
cor(lquan, lprice)

# (f)
save(list = ls(all = TRUE), file = "tut01.RData")
rm(list = ls())
load("tut01.RData")