#######################################
#
# HClust
#
# Author: Luis B. Rei                
# Created: 26/12/2017                 
#                                     
#######################################

# Clear workspace
rm(list=ls())
graphics.off()

library(foreign)
library(haven)
library(ggbiplot)
library(FactoMineR)
library(fpc)
library(cluster)

# Load TCCGA data
db = file.choose()
data = read_sav(db)

data <- na.omit(data) # listwise deletion of missing
data.class <- data[,11]

#######################################
# PCA
#######################################
data.pca <- prcomp(data[,2:10],
                   center = TRUE,
                   scale. = TRUE) 

print(data.pca)

# Plot eigen values
dev.new()
fviz_eig(data.pca, addlabels = TRUE)

# Summary method
summary(data.pca)



#######################################
# HClust
#######################################

# Ideal number of clusters
wss <- (nrow(data)-1)*sum(apply(data[,2:10],2,var))
for (i in 2:10) wss[i] <- sum(eclust(data[,2:10], "hclust",
                                  k=i, graph = FALSE)$withinss)
dev.new()
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

fit <- eclust(data[,2:10], "hclust", k = 2, graph = FALSE)
clust_stats<-cluster.stats(d=dist(data[,2:10]), t(data[,11]), fit$cluster)
# Corrected Rand index
clust_stats$corrected.rand
# Meila's VI
clust_stats$vi

table(t(data[,11]), fit$cluster)

dev.new()
fviz_dend(hclust(dist(data[,2:10])), k = 2, k_colors = c('red','blue'),  
          as.ggplot = TRUE, show_labels = FALSE)



