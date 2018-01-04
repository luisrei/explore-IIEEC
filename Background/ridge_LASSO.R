#######################################
#
# RIDGE vs LASSO
#
# Author: Luis B. Rei                
# Created: 29/12/2017                 
#                                     
#######################################

# Clear workspace
rm(list=ls())
graphics.off()

require(glmnet)
library(ggbiplot)
library(haven)
library(FactoMineR)
library(factoextra)
library(pcaMethods)
library(car)
library(mvtnorm)
library(clusterGeneration)

data.res <- iris[,1:4]
data.class <- iris[,5]

#####################
# Ridge
#####################
x <- as.matrix(data.res) # Removes class
y <- data.class # Only class

# Fitting the model (Ridge: Alpha = 0)
cv.ridge <- cv.glmnet(x, y, family='multinomial', alpha=0, parallel=TRUE, standardize=TRUE, type.measure='mse')

# Results
dev.new()
dev.new()
plot(cv.ridge)
dev.new()
plot(cv.ridge$glmnet.fit, xvar="lambda", label=TRUE)
cv.ridge$lambda.min
cv.ridge$lambda.1se
c1 <- coef(cv.ridge, s=cv.ridge$lambda.1se, exact=T)

#####################
# LASSO
#####################
x <- as.matrix(data.res) # Removes class
y <- data.class # Only class

# Fitting the model (Lasso: Alpha = 1)
cv.lasso <- cv.glmnet(x, y, family='multinomial', alpha=1, parallel=TRUE, standardize=T, type.measure='mse')

# Results
dev.new()
plot(cv.lasso)
dev.new()
plot(cv.lasso$glmnet.fit, xvar="lambda", label=TRUE)
cv.lasso$lambda.min
cv.lasso$lambda.1se
c2 <- coef(cv.lasso, s=cv.lasso$lambda.1se, exact=T)

