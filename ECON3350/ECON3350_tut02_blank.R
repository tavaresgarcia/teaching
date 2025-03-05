# Tutorial 2: Forecasting Univariate Processes - I

# Answers by Eric Eisenstat
# https://sites.google.com/view/ericeisenstat

# Extra material by Francisco Tavares Garcia
# https://tavaresgarcia.github.io/


# House cleaning
rm(list = ls()) # clear Environment
if (!is.null(dev.list())) dev.off() # clear Plots
cat("\014") # clear Console


# 3. The data file arma.csv contains (simulated) data for each DGP in Question 2.
# Import the data into R. Compute, plot, and describe the behaviour of the ACF and
# PACF for each DGP. Discuss the effects of parameter signs. Hint: use the acf and
# pacf commands, respectively.


# Set working directory. To use the following line, save this script in the same
# folder as the data files.
install.packages("rstudioapi")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load the data in arma.csv using read.delim funciton



# DGP1: y(t) = 0.75y(t−1) + ϵ(t)


# DGP2: y(t) = −0.75y(t−1) + ϵ(t)


# DGP3: y(t) = 0.95y(t−1) + ϵ(t)


# DGP4: y(t) = 0.5y(t−1) + 0.25y(t−2) + ϵ(t)


# DGP5: y(t) = 0.25y(t−1) − 0.5y(t−2) + ϵ(t)


# DGP6: y(t) = 0.75ϵ(t−1) + ϵ(t)


# DGP7: y(t) = 0.75ϵ(t−1) − 0.5ϵ(t−2) + ϵ(t)


# DGP8: y(t) = 0.75y(t−1) + 0.5ϵ(t−1) + ϵ(t)

