#######################################
#
# kmeans clustering
#
# Author: Luis B. Rei                
# Created: 26/12/2017                 
#                                     
#######################################

# Clear workspace
rm(list=ls())
graphics.off()

library(ggplot2)
library(colorspace)


# Load simple dataset
data(iris)
iris2 <- iris[,-5]
species_labels <- iris[,5]
species_col <- rev(rainbow_hcl(3))[as.numeric(species_labels)]


#######################
# SPLOM
#######################
dev.new()
dev.new()
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

pairs(iris2, col = species_col,
      lower.panel = NULL,
      cex.labels=2, pch=19, cex = 1.2)

# Add a legend
par(xpd = TRUE)
legend(x = 0.05, y = 0.4, cex = 2,
       legend = as.character(levels(species_labels)),
       fill = unique(species_col))
par(xpd = NA)

#######################
# Parallel coordinates
#######################
dev.new()
par(las = 1, mar = c(4.5, 3, 3, 2) + 0.1, cex = .8)
MASS::parcoord(iris2, col = species_col, var.label = TRUE, lwd = 2)

# Add Title
title("Parallel coordinates plot of the Iris data")

par(xpd = TRUE)
legend(x = 1.75, y = -.25, cex = 1,
       legend = as.character(levels(species_labels)),
       fill = unique(species_col), horiz = TRUE)


#######################
# Cluster
#######################
set.seed(20)
irisCluster <- kmeans(iris[, 3:4], 3, nstart = 10)
irisCluster

#########################
# Pseudo-confusion matrix
#########################
table(irisCluster$cluster, iris$Species)

# See missclassification errors
not.error <- unclass(iris$Species) == irisCluster$cluster
Classification <- not.error

#See errors
dev.new()
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) +
    geom_point(size = 2.5, alpha = 1, aes(shape = Classification))

dev.new()
boxplot(iris2[,4] ~ irisCluster$cluster,
        xlab="Cluster", ylab='Width [cm]',
        main='Petal Width by Cluster')
dev.new()
boxplot(iris2[,3] ~ irisCluster$cluster,
        xlab="Cluster", ylab='Width [cm]',
        main='Petal length by Cluster')
