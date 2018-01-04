#######################################
#
# Sparse PCA methods
#
# Author: Luis B. Rei                
# Created: 26/12/2017                 
#                                     
#######################################

# Clear workspace
rm(list=ls())
graphics.off()

library(MASS)
library(nsprcomp)
set.seed(1)

data <- Boston

# Regular PCA, with the tolerance set to return five PCs
p1 <- prcomp(data, tol = 0.35, scale. = TRUE)
p1

# Sparse PCA with different cardinalities per component. The number of components
# is derived from the length of vector k.
nspc1 <- nsprcomp(data, ncomp = 5, k=20, scale = TRUE)
nspc1

# Non-negative sparse PCA. Note that the principal axes
# naturally have a high degree of orthogonality, because each component
# maximizes the additional variance not already explained.
nspc2 <- nsprcomp(Boston, ncomp = 5, k=20, nneg = TRUE, scale = TRUE)
nspc2

# Sparse cumulative PCA with five components and 20 non-zero loadings.
# The orthonormality penalty is set to a value which avoids co-linear principal
# axes. Note that the non-zero loadings are not distributed uniformly over
# the components.
nscumcomp1 <- nscumcomp(data, ncomp = 5, k = 20, gamma = 1e3,scale = TRUE)
nscumcomp1

# Non-negative sparse cumulative PCA
nscumcomp2 <- nscumcomp(data, ncomp = 5, nneg = TRUE, k = 20, gamma = 1e3, scale = TRUE)
nscumcomp2

# Biplot everything!!!
dev.new()
dev.new()
biplot(p1)
dev.new()
biplot(nspc1)
dev.new()
biplot(nspc2)
dev.new()
biplot(nscumcomp1)
dev.new()
biplot(nscumcomp2)












