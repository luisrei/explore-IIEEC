#######################################
#
# Cluster validation methods
#
# Author: Luis B. Rei                
# Created: 26/12/2017                 
#                                     
#######################################

# Clear workspace
rm(list=ls())
graphics.off()

library(fpc)
library(clValid)
library(FactoMineR)
library(factoextra)

###################
# Internal Validation
##################

# Excluding the column "Species" at position 5
df <- iris[, -5]
# Standardize
df <- scale(df)

# K-means clustering
km.res <- eclust(df, "kmeans", k = 3, nstart = 5, graph = FALSE)

# Visualize k-means clusters
dev.new()
dev.new()
fviz_cluster(km.res, frame.type = "norm", geom = "point", stand = FALSE) + theme_minimal()

# Hierarchical clustering
hc.res <- eclust(df, "hclust", k = 3, hc_metric = "euclidean", 
                 hc_method = "ward.D2", graph = FALSE)

# Visualize dendrograms
dev.new()
fviz_dend(hc.res, show_labels = FALSE) + theme_minimal()

# visualize silhouette
dev.new()
fviz_silhouette(km.res) + theme_minimal()

# Silhouette information
silinfo <- km.res$silinfo
names(silinfo)

# Silhouette widths of each observation
head(silinfo$widths[, 1:3], 10)

# Average silhouette width of each cluster
silinfo$clus.avg.widths

# The total average (mean of all individual silhouette widths)
silinfo$avg.width

# The size of each clusters
km.res$size

# Silhouette width of observation
sil <- km.res$silinfo$widths[, 1:3]

# Objects with negative silhouette
neg_sil_index <- which(sil[, 'sil_width'] < 0)
sil[neg_sil_index, , drop = FALSE]

# Statistics for k-means clustering
km_stats <- cluster.stats(dist(df),  km.res$cluster)
# Dun index
km_stats$dunn

###################
# External Validation
##################
table(iris$Species, km.res$cluster)

# Compute cluster stats
species <- as.numeric(iris$Species)
clust_stats <- cluster.stats(d = dist(df), 
                             species, km.res$cluster)
# Corrected Rand index
clust_stats$corrected.rand

# Meila's VI
clust_stats$vi

################################
# SAME FOR PAM AND HIERARCHICAL
# Agreement between species and pam clusters
pam.res <- eclust(df, "pam", k = 3, graph = FALSE)
table(iris$Species, pam.res$cluster)
clust_stats <- cluster.stats(d = dist(df), 
              species, pam.res$cluster)
# Dunn
clust_stats$dunn
# Corrected Rand index
clust_stats$corrected.rand
# Meila's VI
clust_stats$vi
# Agreement between species and HC clusters
hc.res <- eclust(df, "hclust", k = 3, graph = FALSE)
table(iris$Species, hc.res$cluster)
clust_stats <- cluster.stats(d = dist(df), 
              species, hc.res$cluster)
# Dunn
clust_stats$dunn
# Corrected Rand index
clust_stats$corrected.rand
# Meila's VI
clust_stats$vi


