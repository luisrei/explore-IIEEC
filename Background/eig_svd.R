#######################################
#
# eigendecomposition vs svd
#
# Author: Luis B. Rei                
# Created: 28/12/2017                 
#                                     
#######################################

# Clear workspace
rm(list=ls())
graphics.off()

library(ggbiplot)
library(haven)
library(FactoMineR)
library(factoextra)
library(pcaMethods)
library(car)
library(mvtnorm)
library(clusterGeneration)

#######################################
# Wrapper function to perform decompositions
#######################################
PC<-function(X,method="eigen",scaled=T,graph=F,center=F,rm.na=T,print.results=T){
  if (any(is.na(X))){
    tmp<-X
    if(rm.na==T){X<-na.omit(data.frame(X));X<-as.matrix(X)}
    else{X[is.na(X)] = matrix(apply(X, 2, mean, na.rm = TRUE),
                              ncol = ncol(X), nrow = nrow(X), byrow = TRUE)[is.na(X)]}}
  else{tmp<-X}
  if(method=="eigen"){
    if(scaled==1){X1=cor(X);X2=scale(X)}
    else{X1=cov(X);X2=scale(X,scale=F)}
    total.var<-sum(diag(cov(X2)))
    values<-eigen(X1)$values;vectors<-eigen(X1)$vectors;sdev=sqrt(values)}
  
  if(method=="svd"){
    if(sum(scaled,center)>1){X2<-scale(X)}else{
      if(scaled==1){X2=scale(X,center=F)}else{
        if(center==1){X2=scale(X,scale=F)}else{X2=X}}}
    total.var<-sum(diag(cov(X2)))
    var<-nrow(X2)-1
    vectors<-svd(X2)$v;sdev=svd(X2)$d/sqrt(var);values<-sdev*sdev}
  
  prop.var<-rep(NA,ncol(X));cum.var<-rep(NA,ncol(X));scores<-X2%*%vectors
  namex<-as.character(1:ncol(X));scorenames<-rep(NA,ncol(X))
  for(i in 1:(ncol(X))){
    scorenames[i]<-do.call(paste,c("PC",as.list(namex[i]),sep=""))}
  colnames(scores)<-scorenames
  rownames(vectors)<-colnames(X);colnames(vectors)<-scorenames
  for(i in 1:ncol(X)){prop.var[i]<-var(scores[,i])/total.var}
  for(i in 1:ncol(X)){cum.var[i]<-sum(prop.var[1:i])}
  
  importance<-t(matrix(c(sdev,prop.var,cum.var),ncol=3))
  importance<-as.table(importance)
  colnames(importance)<-scorenames
  rownames(importance)<-c("Standard Deviation","Proportion of Variance","Cumulative
                          Proportion")
  z<-list(values=values,vectors=vectors,scores=scores,importance=importance
          ,sdev=sdev)
  if(graph==1){
    biplot(scores[,1:2],vectors[,1:2], main="Biplot of Data",xlab=do.call
           (paste,c("PC1 (",as.list(round(z$importance[2,1]*100,2)),"%)",sep=""))
           ,ylab=do.call(paste,c("PC2
                                 (",as.list(round(z$importance[2,2]*100,2)),"%)",sep="")), cex=0.7)
    windows()
    screeplot(z,type='l',main='Screeplot of Components')
    abline(1,0,col='red',lty=2)}
  
  if(print.results==T){
    if(method=="eigen"){print("PCA Analysis Using Spectral Decomposition")}
    if(method=="svd"){print("PCA Analyis Using Singular Value Decomposition")}
    if (any(is.na(tmp))){
      if(rm.na==T){print("Warning:One or more rows of data were omitted from
                         analysis")}
      if(rm.na==F){print("Warning: Mean of the variable was used for Missing
                         values")}}
    print(importance)}
  z<-list(values=values,vectors=vectors,scores=scores,importance=importance
          ,sdev=sdev)
}



#######################################
# Examine different PCAs
#######################################

sim<-function(n.var=5,n.obs=1000,n.reps=1000){
  results<-data.frame(matrix(numeric(0),n.reps,3))
  colnames(results)<-c("Values","Vectors","Scores")
  for(h in 1:n.reps){
    print(h)
    #Function to create data
    rPCA<-function(n.var,n.obs){
      mu<-rnorm(n.var);R<-rcorrmatrix(n.var,alphad=1)
      X<-rmvnorm(n=n.obs,mean=mu,sigma=R)
      return(X)}
    data<-rPCA(n.var=n.var,n.obs=n.obs)
    
    ##################################
    #Create Objects from PCA analysis#
    ##################################
    ##Analysis using metrics calculated from the function eigen
    pc1<-PC(data,scale=T,rm.na=T,print.results=F)
    #Analysis using the R function prcomp
    pc2<-prcomp(data,center=TRUE,scale=TRUE)
    #Analysis using the R function princomp
    pc3<-princomp(data,cor=TRUE)
    #Analysis using the function PCA from the package FactoMinR
    pc4<-PCA(data, scale.unit=TRUE, ncp=ncol(data), graph=F)
    #Analyis using the function pca from the package pcaMethods
    pc5<-pca(data,method="svd",scale="uv",nPcs=ncol(data))
    
    ############################################
    #Compare Eigenvalues from different methods#
    ############################################
    eigenvalues<-data.frame(pc1$values,pc2$sdev^2,pc3$sdev^2,pc4$eig$eigenvalue,
                            pc5@sDev^2)
    colnames(eigenvalues)<-c("PC","prcomp","princomp","PCA","pca")
    rownames(eigenvalues)<-colnames(data)
    valuediff<-matrix(numeric(0),nrow(eigenvalues),ncol(eigenvalues))
    for(i in 1:ncol(data)){
      for(j in 1:ncol(eigenvalues)){
        valuediff[i,j]<-eigenvalues[i,j]-rowMeans(eigenvalues)[i]}}
    results$Values[h]<-mean(valuediff) #Should be close to Zero
    
    #############################################
    #Compare Eigenvectors from different methods#
    #############################################
    eigenvectors<-list(PC=pc1$vectors,prcomp=pc2$rotation,princomp=pc3$loadings
                       ,PCA=pc4$svd$V,pca=pc5@loadings)
    ev<- array(0, dim=c(5,ncol(data),ncol(data)))
    ev[1,,]<-eigenvectors$PC;ev[2,,]<-eigenvectors$prcomp
    ev[3,,]<-eigenvectors$princomp;ev[4,,]<-eigenvectors$PCA
    ev[5,,]<-eigenvectors$pca
    
    vectordiff<-array(0, dim=c(5,ncol(data),ncol(data)))
    for(i in 1:5){
      for(j in 1:ncol(data)){
        for(k in 1:ncol(data)){
          vectordiff[i,j,k]<-abs(ev[i,j,k])-mean(abs(ev[,j,k]))}}}
    results$Vectors[h]<-mean(vectordiff) #Should be close to Zero
    
    #######################################
    #Compare Scores from different methods#
    #######################################
    scores<-list(PC=pc1$scores,prcomp=pc2$x,princomp=pc3$scores,PCA=pc4$ind$coord
                 ,pca=pc5@scores)
    sc<-array(0,dim=c(5,nrow(data),ncol(data)))
    sc[1,,]<-scores$PC;sc[2,,]<-scores$prcomp;sc[3,,]<-scores$princomp
    sc[4,,]<-scores$PCA;sc[5,,]<-scores$pca
    scorediff<-array(0,dim=c(5,nrow(data),ncol(data)))
    for(i in 1:5){
      for(j in 1:nrow(data)){
        for(k in 1:ncol(data)){
          scorediff[i,j,k]<-abs(sc[i,j,k])-mean(abs(sc[,j,k]))}}}
    results$Scores[h]<-mean(scorediff)
  }
  return(results)
}


#######################################
# Examine different PCAs
#######################################

sim_eigsvd<-function(n.var=5,n.obs=1000,n.reps=1000){
  results<-data.frame(matrix(numeric(0),n.reps,3))
  colnames(results)<-c("Values","Vectors","Scores")
  for(h in 1:n.reps){
    #Function to create data
    rPCA<-function(n.var,n.obs){
      mu<-rnorm(n.var);R<-rcorrmatrix(n.var,alphad=1)
      X<-rmvnorm(n=n.obs,mean=mu,sigma=R)
      return(X)}
    data<-rPCA(n.var=n.var,n.obs=n.obs)
    
    ##################################
    #Create Objects from PCA analysis#
    ##################################
    ##Analysis using metrics calculated from the function eigen
    pc1<-PC(data,scale=T,rm.na=T,print.results=F)
    #Analysis using the R function prcomp
    pc2<-PC(data,method="svd",center=T,scale=T,rm.na=T,print.results=F)

    ############################################
    #Compare Eigenvalues from different methods#
    ############################################
    eigenvalues<-data.frame(pc1$values,pc2$values)
    colnames(eigenvalues)<-c("eig","svd")
    rownames(eigenvalues)<-colnames(data)
    valuediff<-matrix(numeric(0),nrow(eigenvalues),ncol(eigenvalues))
    for(i in 1:ncol(data)){
      for(j in 1:ncol(eigenvalues)){
        valuediff[i,j]<-eigenvalues[i,j]-rowMeans(eigenvalues)[i]}}
    results$Values[h]<-mean(valuediff) #Should be close to Zero
  
    #############################################
    #Compare Eigenvectors from different methods#
    #############################################
    eigenvectors<-list(eig=pc1$vectors,svd=pc2$vectors)
    ev<- array(0, dim=c(2,ncol(data),ncol(data)))
    ev[1,,]<-eigenvectors$eig;ev[2,,]<-eigenvectors$svd
    
    vectordiff<-array(0, dim=c(2,ncol(data),ncol(data)))
    for(i in 1:2){
      for(j in 1:ncol(data)){
        for(k in 1:ncol(data)){
          vectordiff[i,j,k]<-abs(ev[i,j,k])-mean(abs(ev[,j,k]))}}}
    results$Vectors[h]<-mean(vectordiff) #Should be close to Zero
  
    #######################################
    #Compare Scores from different methods#
    #######################################
    scores<-list(eig=pc1$scores,svd=pc2$scores)
    sc<-array(0,dim=c(2,nrow(data),ncol(data)))
    sc[1,,]<-scores$eig;sc[2,,]<-scores$svd;
    scorediff<-array(0,dim=c(2,nrow(data),ncol(data)))
    for(i in 1:2){
      for(j in 1:nrow(data)){
        for(k in 1:ncol(data)){
          scorediff[i,j,k]<-abs(sc[i,j,k])-mean(abs(sc[,j,k]))}}}
    results$Scores[h]<-mean(scorediff)
  }
  return(results)
}



#######################################
# MAIN
#######################################
data.res <- Boston

dev.new()
dev.new()
pc_data1<-PC(data.res,method="eigen",scaled=T,graph=T,rm.na=T,print.results=T)

dev.new()
pc_data2<-PC(data.res,method="svd",scaled=T,center = T,graph=T,rm.na=T,print.results=T)

res <- sim_eigsvd(4, 300, 50)



