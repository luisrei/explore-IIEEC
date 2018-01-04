#######################################
#
# High-dimensional clustering methods test
#
# Author: Luis B. Rei                
# Created: 29/12/2017                 
#                                     
#######################################

# Clear workspace
rm(list=ls())
graphics.off()

require(microbenchmark)
library(subspace)
library(haven)

##############################
# SYNTHENTIC DATA
##############################
set.seed(12)
data(subspace_dataset)

#Load the true clustering for this dataset
path_to_clustering <- paste(path.package("subspace"),"/extdata/subspace_dataset.true",sep="")
clust0 <- clustering_from_file(file_path=path_to_clustering)
p <- ggvis::prop(property="shape",x="cross")
#plot(clust0,subspace_dataset,props=p)

clust1 <- ProClus(subspace_dataset,k=10,d=2.5)
#plot(clust1,subspace_dataset)

clust2 <- CLIQUE(subspace_dataset, xi = 40, tau = 0.07)
#plot(clust2,subspace_dataset)

clust3 <- SubClu(subspace_dataset,epsilon=1,minSupport=5)
#plot(clust3,subspace_dataset)

clust4 <- FIRES(subspace_dataset)
#plot(clust4,subspace_dataset)

clust5 <- P3C(subspace_dataset,PoissonThreshold=3)
#plot(clust5,subspace_dataset)


##############################
# BREAST CANCER DATA
##############################

# Load TCCGA data
db = file.choose()
data = read_sav(db)

data <- na.omit(data) # listwise deletion of missing
data.res <- data[,2:10]
data.class <- data[,11]

clust6 <- ProClus(data.res,k=2,d=2.5)
#plot(clust6,data.res)

clust7 <- CLIQUE(data.res, xi = 40, tau = 0.5)
#plot(clust7,data.res)

clust8 <- SubClu(data.res,epsilon=1,minSupport=5)
#plot(clust8,data.res)

clust9 <- FIRES(data.res)
#plot(clust9,data.res)

clust10 <- P3C(data.res,PoissonThreshold=3)
#plot(clust10,data.res)

#######################
# BENCHMARKS
#######################
res1 <- microbenchmark(ProClus(subspace_dataset,k=10,d=2.5), CLIQUE(subspace_dataset, xi = 40, tau = 0.07), SubClu(subspace_dataset,epsilon=1,minSupport=5), FIRES(subspace_dataset), P3C(subspace_dataset,PoissonThreshold=3), times=25)

res2 <- microbenchmark(ProClus(data.res,k=2,d=2.5), CLIQUE(data.res, xi = 40, tau = 0.07), SubClu(data.res,epsilon=1,minSupport=5), FIRES(data.res), P3C(data.res,PoissonThreshold=3), times=25)


