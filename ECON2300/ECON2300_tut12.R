# Tutorial 12: Prediction with Many Regressors and Big Data

# Answers by Dong-Hyuk Kim
# https://sites.google.com/site/kimdonghyuk000/home
# Extra material by Francisco Tavares Garcia
# https://tavaresgarcia.github.io/

# install.packages("readr")
# install.packages("dplyr")
# install.packages("ggplot2")
install.packages("glmnet")
install.packages("pls")
install.packages("ggpubr")

library(readr)    # package for fast read rectangular data
library(dplyr)    # package for data manipulation
library(ggplot2) # package for elegant data visualisations
library(glmnet) # package for elastic-net regularized generalized linear models
library(pls) # package for partial least squares and principal component regression
library(ggpubr) # pacakge facilitating the creation of beautiful ggplot2-based graphs

### SW E14.1

# Set working directory
# Ex Win: setwd("G:/My Drive/BEcon/TUTOR/ECON2300/11")
# Ex Mac: setwd("/Users/uqdkim7/Dropbox/Teaching/R tutorials/Tutorial11")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Clean Working Environment
rm(list = ls())

# Good video summarizing the difference between predicting and causation.
# Josh Angrist: What's the Difference Between Econometrics and Data Science?
# https://www.youtube.com/watch?v=2EhRT2mOXm8

All <- read_csv("AllSample.csv") %>%
  dplyr::select(-c(x2))
attach(All)

# Original data can be found here:
# https://media.pearsoncmg.com/ph/bp/bp_stock_econometrics_4_cw/content/datapages/stock04_data14.html
# Data description
# https://media.pearsoncmg.com/ph/bp/bp_stock_econometrics_4_cw/content/datapages/data/CASchools_EE141_Description.pdf

InSample <- filter(All, InSample == 1)
OutOfSample <- filter(All, InSample == 0)

x.in <- as.matrix(InSample[,2:(ncol(All)-1)]) # 2nd to one before last column
y.in <- as.matrix(InSample[,ncol(All)]) # last column
n.in <- nrow(x.in)
x.out <- as.matrix(OutOfSample[,2:(ncol(All)-1)]) # 2nd to one before last column
y.out <- as.matrix(OutOfSample[,ncol(All)])# last column
n.out <- nrow(x.out)

## (a)
# Charter_s is a binary variable, so it's square has the same value as the
# original data. Enrollment is duplicate because it is equal to STR * number of
# teachers. As we have both of these information and the interaction term,
# keeping Enrollment is redundant.

## (b)
# It is already done.

## (c)
ols <- lm(y ~.-1, data = dplyr::select(InSample, -c(InSample)))
summary(ols)

# Adjusted R-squared:  0.6644

## (d)
ridge <- glmnet(x.in, y.in, alpha = 0, lambda = 300/n.in, 
                intercept = F, standardize = F)
coef(ridge)[1:12,]

# Notice that the ridge and Lasso objective functions used by the glmnet command
# are Sum[i=1,n] (Yi - b1X1i - ... - bkXki)^2 + nLambdaRidge Sum[j=1,k] bj^2 and
# Sum[i=1,n] (Yi - b1X1i - ... - bkXki)^2 + 2nLambdaLasso  Sum[j=1,k] |bj |,
# respectively. (See
# https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html#intro).

## (e)
lasso <- glmnet(x.in, y.in, alpha = 1, lambda = 100/(2*n.in),
                intercept = F, standardize = F)
lasso.coef <- predict(lasso, type = "coefficients")
lasso.coef

lasso.coef[lasso.coef != 0]
# 86 regressors with coefficient != 0

## (f)
pca <- prcomp(x.in, scale. = F)

screeplot(pca, type = c("barplot", "lines"), npcs = 250)

## (g)
pca.all <- predict(pca, newdata = x.in)
pcareg <- lm(y.in ~ pca.all[, 1:20] - 1)
summary(pcareg)

# Adjusted R-squared:  0.5457

## (h)
set.seed(1)
cv.ridge <- cv.glmnet(x.in, y.in, alpha = 0, nfolds = 10, 
                      intercept = F, standardize = F)
bestlam.ridge <- cv.ridge$lambda.min
bestlam.ridge

cv.lasso <- cv.glmnet(x.in, y.in, alpha = 1, nfolds = 10, 
                      intercept = F, standardize = F)
bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso

cv.pca <- pcr(y ~.-1, data = dplyr::select(InSample, -c(InSample)),
              scale = F, center = F, validation = "CV")
summary(cv.pca)
# minimum RMSPE (Root Mean Square Error of Prediction) when p = 20
validationplot(cv.pca, val.type = "RMSEP")

bestp <- 20

## (i)
ols_pred <- predict(ols, newdata = data.frame(x.out))
sqrt(mean((ols_pred - y.out)^2))

ridge_pred <- predict(ridge, newx = x.out)
sqrt(mean((ridge_pred - y.out)^2))

lasso_pred <- predict(lasso, newx = x.out)
sqrt(mean((lasso_pred - y.out)^2))

pca.out <- prcomp(x.out, scale. = F)
pca.all.out <- predict(pca.out, newdata = data.frame(x.out))
pcareg.out <- lm(y.out ~ pca.all.out[, 1:15] - 1)
pca_pred <- predict(pcareg.out, newdata = data.frame(x.out))
sqrt(mean((pca_pred - y.out)^2))

## (ii)
data_pred <- data.frame(y = y.out, ols = ols_pred, ridge = c(ridge_pred),
                        lasso = c(lasso_pred), pc = pca_pred)

scatter1 <- ggplot(data_pred, aes(x = ols, y = y)) +
  geom_point(alpha = .75, size = 1, color = "cyan4") +
  labs(x = "OLS prediction", y = "Actual") +
  geom_smooth(method = "lm", level = 0.5, color = "violetred3") +
  theme(axis.title = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, family = "serif", face = "bold"))
scatter2 <- ggplot(data_pred, aes(x = ridge, y = y)) +
  geom_point(alpha = .75, size = 1, color = "cyan4") +
  labs(x = "Ridge prediction", y = "Actual") +
  geom_smooth(method = "lm", level = 0.5, color = "violetred3") +
  theme(axis.title = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, family = "serif", face = "bold"))
scatter3 <- ggplot(data_pred, aes(ridge, lasso)) +
  geom_point(alpha = .75, size = 1, color = "cyan4") +
  labs(x = "Ridge prediction", y = "Lasso prediction") +
  geom_smooth(method = "lm", level = 0.5, color = "violetred3") +
  theme(axis.title = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, family = "serif", face = "bold"))
scatter4 <- ggplot(data_pred, aes(ridge, pc)) +
  geom_point(alpha = .75, size = 1, color = "cyan4") +
  labs(x = "Ridge prediction", y = "PC prediction") +
  geom_smooth(method = "lm", level = 0.5, color = "violetred3") +
  theme(axis.title = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, family = "serif", face = "bold"))
figMatrix <- ggarrange(scatter1, scatter2, scatter3, scatter4,
                       ncol = 2, nrow = 2)
annotate_figure(figMatrix, top = text_grob("Scatterplots for OOS Predictions",
                                           face = "bold", size = 12, family = "serif"))

## (j)
best.ridge <- glmnet(x.out, y.out, alpha = 0, lambda = bestlam.ridge,
                     intercept = F, standardize = F)
best.lasso <- glmnet(x.out, y.out, alpha = 1, lambda = bestlam.lasso,
                     intercept = F, standardize = F)

best.ridge_pred <- predict(best.ridge, newx = x.out)
sqrt(mean((best.ridge_pred - y.out)^2))

best.lasso_pred <- predict(best.lasso, newx = x.out)
sqrt(mean((best.lasso_pred - y.out)^2))

pca <- prcomp(x.out, scale. = F)
pca.all <- predict(pca, newdata = x.out)
best.pcareg.out <- lm(y.out ~ pca.all[, 1:bestp] - 1)
best.pca_pred <- predict(best.pcareg.out, newdata = data.frame(x.out))
sqrt(mean((best.pca_pred - y.out)^2))
