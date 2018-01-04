#######################################
#
# Trying out different PCA methods
#
# Author: Luis B. Rei                
# Created: 26/12/2017                 
#                                     
#######################################

# Clear workspace
rm(list=ls())
graphics.off()

library(pcaMethods)

# Load data
data(metaboliteData)
data(metaboliteDataComplete)

# Center data
md <- prep(metaboliteData, scale="none", center=TRUE)
mdC <- prep(metaboliteDataComplete, scale="none", center=TRUE)

# Run SVD pca, PPCA, BPCA, SVDimpute, nipalsPCA and NLPCA
resPCA <- pca(mdC, method="svd", center=FALSE, nPcs=5)
resPPCA <- pca(md, method="ppca", center=FALSE, nPcs=5)
resBPCA <- pca(md, method="bpca", center=FALSE, nPcs=5)
resSVDI <- pca(md, method="svdImpute", center=FALSE, nPcs=5)
resNipals <- pca(md, method="nipals", center=FALSE, nPcs=5)
resNLPCA <- pca(md, method="nlpca", center=FALSE, nPcs=5, maxSteps=300)

##
resPCA
resPPCA
resBPCA
resSVDI
resNipals
resNLPCA

# Print loadings 
loadings(resPCA)
loadings(resPPCA)
loadings(resBPCA)
loadings(resSVDI)
loadings(resNipals)
loadings(resNLPCA)

# Biplot everything!!!
dev.new()
biplot(resPCA, cex = 0.6, main="PCA")
dev.new()
biplot(resPPCA, cex = 0.6, main="PPCA")
dev.new()
biplot(resBPCA, cex = 0.6, main="BPCA")
dev.new()
biplot(resSVDI, cex = 0.6, main="SDVI")
dev.new()
biplot(resNipals, cex = 0.6, main="Nipals")


