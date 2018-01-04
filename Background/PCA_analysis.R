#######################################
#
# Gathering PCA information
#
# Author: Luis B. Rei                
# Created: 26/12/2017                 
#                                     
#######################################

# Clear workspace
rm(list=ls())
graphics.off()

library("ggbiplot")
library("caret")
library("FactoMineR")
library("factoextra")

##################################
# Logaritmic PCA
##################################

# Load simple dataset
data(iris)

log.iris <- log(iris[, 1:4])
iris.species <- iris[, 5]

# PCA
iris.pca <- prcomp(log.iris,
                 center = TRUE,
                 scale. = TRUE) 

print(iris.pca)

# Plot eigen values
dev.new()
dev.new()
fviz_eig(iris.pca, addlabels = TRUE)

# Summary method
summary(iris.pca)

g <- ggbiplot(iris.pca, obs.scale = 1, var.scale = 1, 
        groups = iris.species, ellipse = TRUE, 
            circle = TRUE) + 
        scale_color_discrete(name = '') + 
        theme(legend.direction = 'horizontal', 
            legend.position = 'top')

dev.new()
print(g)

######################
# Box-Cox PCA
######################

# Load simple dataset
data(iris)

cox = preProcess(iris[,1:4], 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))
PC = predict(cox, iris[,1:4])

# Principal Components
print(cox$rotation)

# Summary method
summary(cox)