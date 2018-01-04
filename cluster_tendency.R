#######################################
#
# Assessing clustering tendency
#
# Author: Luis B. Rei                
# Created: 26/12/2017                 
#                                     
#######################################

# Clear workspace
rm(list=ls())
graphics.off()

library("factoextra")
library("ggplot2")

par(bg = "white")

# Iris data set
data(iris)
df <- iris[, -5]

# Random data generated from the iris data set
random_df <- apply(df, 2, 
                   function(x){runif(length(x), min(x), (max(x)))})
random_df <- as.data.frame(random_df)

# Standardize the data sets
df <- iris.scaled <- scale(df)
random_df <- scale(random_df)


#####################################
# VISUAL INSPECTION OF DATA
#####################################
# Plot faithful data set
dev.new()
fviz_pca_ind(prcomp(df), title = "Iris data", 
             habillage = iris$Species,  palette = "jco",
             geom = "point", ggtheme = theme_classic(base_size = 12, base_family = ""),
             legend = "bottom")

# Plot the random df
dev.new()
fviz_pca_ind(prcomp(random_df), title = "Random data", 
             geom = "point", ggtheme = theme_classic())


#####################################
# CLUSTER DATA
#####################################
# K-means on iris dataset
dev.new()
km.res1 <- kmeans(df, 3,nstart = 5)
fviz_cluster(km.res1, data = df,
             frame.type = "norm", geom = "point", stand = FALSE) + theme_minimal()

# K-means on the random dataset
km.res2 <- kmeans(random_df, 3,nstart = 5)
dev.new()
fviz_cluster(km.res2, data = random_df,
             frame.type = "norm", geom = "point", stand = FALSE) + theme_minimal()

# Hierarchical clustering on the random dataset
dev.new()
fviz_dend(hclust(dist(random_df)), k = 3, show_labels = FALSE)


#####################################
# HOPKINS
#####################################
# Compute Hopkins statistic for iris dataset
res <- get_clust_tendency(df, n = nrow(df)-1, graph = FALSE)
hop1 <- 1-res$hopkins_stat

# Compute Hopkins statistic for a random dataset
res2 <- get_clust_tendency(random_df, n = nrow(random_df)-1, graph = FALSE)
hop2 <- 1-res2$hopkins_stat

########################################
# VISUAL ASSESSMENT OF CLUSTER TENDENCY
########################################
dev.new()
fviz_dist(dist(df), show_labels = FALSE)+
  labs(title = "Iris data")

dev.new()
fviz_dist(dist(random_df), show_labels = FALSE)+
  labs(title = "Random data")

