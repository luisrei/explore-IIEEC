#######################################
#
# Optimal number of clusters
#
# Author: Luis B. Rei                
# Created: 26/12/2017                 
#                                     
#######################################

# Clear workspace
rm(list=ls())
graphics.off()

library(factoextra)
library(NbClust)

# Iris data set
data(iris)
df <- iris[, -5]

#####################################
# ELBOW METHOD
#####################################
dev.new()
dev.new()
fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

#####################################
# SILHOUETTE METHOD
#####################################
dev.new()
fviz_nbclust(df, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

#####################################
# GAP STATISTICS
#####################################
set.seed(123)
dev.new()
fviz_nbclust(df, kmeans,  method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")


#####################################
# APPLY   MAJORITY RULE
#####################################
dev.new()
nb <- NbClust(df, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans")

fviz_nbclust(nb)



