#######################################
#
# Compare different clustering algorithms
#
# Author: Luis B. Rei                
# Created: 26/12/2017                 
#                                     
#######################################

# Clear workspace
rm(list=ls())
graphics.off()

library(clValid)
library(data.table)

# Iris data set
df = scale(iris[, -5])

####################
# Internal measures
####################

# Compute clValid
clmethods <- c("hierarchical","kmeans","pam")
intern <- clValid(df, nClust = 2:5, 
                  clMethods = clmethods, validation = "internal")
# Summary
summary(intern)

####################
# Stability measures
####################
stab <- clValid(df, nClust = 2:5, clMethods = clmethods,
                validation = "stability")

# Display only optimal Scores
summary(stab)