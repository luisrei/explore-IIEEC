#######################################
#
# CDPCA
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
library(FactoMineR)
library(fpc)
library(cluster)
library(factoextra)

par(bg = "white")

# Load TCCGA data
db = file.choose()
data = read_sav(db)

data <- na.omit(data) # listwise deletion of missing
data.res <- data[,2:10]
data.class <- data[,11]

#######################################
# PCA
#######################################
data.pca <- prcomp(data.res,
                   center = TRUE,
                   scale. = TRUE) 

print(data.pca)

# Plot eigen values
dev.new()
fviz_eig(data.pca, addlabels = TRUE)

# Summary method
summary(data.pca)



#######################################
# CDPCA
#######################################

# Ideal number of clusters
wss <- (nrow(data)-1)*sum(apply(data.res,2,var))
for (i in 2:10) wss[i] <- sum(kmeans(data.res,
                                     centers=i)$withinss)
dev.new()
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# Group variables
dev.new()
pf <- PCA(data.res,graph = TRUE)
fit1 <- kmeans(pf$var$coord,2)
fit1

# Group individuals
pf <- PCA(data.res,graph = FALSE)
fit2 <- kmeans(pf$ind$coord,2)
fit2

clust_stats <- cluster.stats(d=dist(data.res), t(data.class), fit2$cluster)
# Corrected Rand index
clust_stats$corrected.rand
# Meila's VI
clust_stats$vi

# Pseudo-confusion
table(t(data.class),fit2$cluster)

dev.new()
clusplot(data.res,fit2$cluster, color=TRUE, shade=FALSE, 
         labels=1, lines=0, plotchar=TRUE, cex=0.7, col.p=fit2$cluster)





