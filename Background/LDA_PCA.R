#################################################
#
# Comparing supervised and unsupervised methods.
#
# Author: Luis B. Rei                
# Created: 26/12/2017                 
#                                     
#################################################


# Clean workspace
rm(list=ls())
graphics.off()

require(MASS)
require(ggplot2)
require(scales)
require(gridExtra)

# Load data
data(iris)


##############################
# Principal Component Analysis
###############################
pca <- prcomp(iris[,-5],
              center = TRUE,
              scale. = TRUE) 

# Print linear combination coefficients
print(pca$rotation)

# Between group variance
prop.pca = pca$sdev^2/sum(pca$sdev^2)

ppca <- predict(object = pca,
                newdata = iris)

summary(pca)

###############################
# Reduced-rank Discriminant Analysis
###############################
lda <- lda(Species ~ ., 
           iris, 
           prior = c(1,1,1)/3)

# Print linear combination coefficients
print(lda$scaling)

# Between group variance
prop.lda = lda$svd^2/sum(lda$svd^2)

plda <- predict(object = lda,
                newdata = iris)

summary(lda)

# Confusion matrix
table(plda$class, iris[,5])

###############################
# Plot them together
###############################
dataset = data.frame(species = iris[,"Species"],
                     pca = pca$x, lda = plda$x)

p1 <- ggplot(dataset) + geom_point(aes(lda.LD1, lda.LD2, colour = species, shape = species), size = 2.5) + 
        labs(x = paste("LD1 (", percent(prop.lda[1]), ")", sep=""),
             y = paste("LD2 (", percent(prop.lda[2]), ")", sep="")) +
        ggtitle("Reduced-rank Discriminant Analysis")

p2 <- ggplot(dataset) + geom_point(aes(pca.PC1, pca.PC2, colour = species, shape = species), size = 2.5) +
        labs(x = paste("PC1 (", percent(prop.pca[1]), ")", sep=""),
             y = paste("PC2 (", percent(prop.pca[2]), ")", sep="")) +
      ggtitle("Principal Component Analysis")

dev.new()
dev.new()
grid.arrange(p1, p2)