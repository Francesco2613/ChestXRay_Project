### ChestXRay #################################################

###----------------------------------- Graphical Labels Analysis --------------------------
count <- vector(mode='numeric', length=14)
for(j in 2:15){
  for(i in 1:189116){
    count[j-1] <- count[j-1] + labels[i,j]
  }
}
rm(i,j)
count

quartz(); par(mar=c(8,13,5,5))
lab <- colnames(labels)
barplot(count,names=lab[-1], col=rainbow(14), horiz=T, las=1)


###------------------------------ Principal Component Analyisis --------------------------

#pc.embeddings <- princomp(embeddings[,2:931], scores=T)
#pc.embeddings
#summary(pc.embeddings)

# proportion of variances explained by each PC
proportion <- pc.embeddings$sd^2/sum(pc.embeddings$sd^2)
n.components <- length(which( proportion <= 0.8 ))
# Graphical Representation
quartz()
plot(cumsum(proportion), type='b', axes=F, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='black')
abline(h=0.87, lty=2, col='red')
box()
axis(2,at=0:10/10,labels=0:10/10)
### Simulation
#PCA_embeddings(embeddings,0.8,"training_densenet121") ### It works!!!

n.components <- length(which( cumsum(proportion) < 0.87 ))
n.components


###-------------------------------------- Gaussianity ------------------------------------

### We use the first 30 components (scores) of PCA:
embeddings30 <- pc.embeddings$scores[,1:30]
embeddings30 <- data.frame(embeddings30)

### We introduce a new function!!!------------------------------
for(i in 1:3){                                              #  |
  CheXgaussian(embeddings30[,i])                            #  |
  title(paste('Component',i), col.main='red', line = 0.5)   #  |
}; rm(i)                                                    #  |
                                                            #  |
graphics.off()                                              #  |
#---------------------------------------------------------------

### Now we consider the squared Mahalanobis distances and we
### test if they are a sample from a chi-square distribution
M <- colMeans(embeddings30)
S <- cov(embeddings30)
#shapiro.test(M)
#CheXgaussian(M)
#d2 <- matrix(mahalanobis(embeddings30, M, S))
#quartz()
#plot(d2, pch=ifelse(d2<150,1,19), col=ifelse(d2<150,'cyan','purple'))
#quartz()
#hist(d2, prob=T)
#lines(0:max(d2), dchisq(0:max(d2),50), col='blue', lty=2)
#d2.class <- cut(d2, qchisq((0:8)/8, df = 50))
#d2.freq  <- table(d2.class)
#qqplot(qchisq(seq(0.5/30, 1 - 0.5/30 ,by = 1/30), df = 4), d2,  main='QQplot di d2')
#chisq.test(x = d2.freq, p = rep(1/8, 8), simulate.p.value = T)
### ???

### We resort to BoxCox transformation
library(car)
min <- abs(min(embeddings30))
Embeddings30 <- embeddings30 + min + 0.1
lambda <- powerTransform(Embeddings30)
lambda
for(i in 1:50){
  Embeddings30[,i] <- bcPower(Embeddings30[,i],  lambda$lambda[i]) 
}

### We extract just a small sample and we study the Shapiro p-value
library(dplyr)
Embeddings30 <- sample_n(Embeddings30,1800)

P <- vector('numeric',50)
for(i in 1:50){
   P[i] <- shapiro.test(Embeddings30[,i])$p
}; rm(i)
anomalies <- which(P<.05)
# E.g. shapiro.test(Embeddings30[,1])

###--------------------------------- Uncertainties ---------------------------------------

### We turn every 'u' into 0 or 1:
U_Binary(labels)

### Which label is the more uncertain?
count <- vector(mode='numeric', length=14)
for(j in 2:15){
  for(i in 1:189116){
    if(labels[i,j]!=1 && labels[i,j]!=0){
    count[j-1] <- count[j-1] + 1
    }
  }
}
rm(i,j)

quartz(); par(mar=c(8,13,5,5))
lab <- colnames(labels)
barplot(count,names=lab[-1], col=rainbow(14), horiz=T, las=1, main = "#Uncertainties")

##----------------------------------------------
## Enforce the relationships between pathologies
##----------------------------------------------
rm(list=ls())

load("data/training_densenet121.RData")
rm(embeddings)

corr_labels <- labels

# Matrix of relationships
rel <- rbind(c('Cardiomegaly', 'Enlarged.Cardiomediastinum'),
             c('Edema', 'Lung.Opacity'),
             c('Consolidation', 'Lung.Opacity'),
             c('Pneumonia', 'Lung.Opacity'),
             c('Lung.Lesion', 'Lung.Opacity'),
             c('Atelectasis', 'Lung.Opacity'),
             c('Pneumonia', 'Consolidation')
)  

# Enforce the relationships
for(i in 1:nrow(rel)) {
  corr_labels[[rel[i,2]]] [ which(labels[[rel[i,1]]] == 1) ] = 1
}

rm(i, rel)

## Count of changes
colSums(labels != corr_labels)

save(list = c('corr_labels'), file = paste('corr_labels', ".RData", sep=""))

rm(list=ls())


###_____________________________________________________________________
###--------------------------- KERNEL PCA ------------------------------
###_____________________________________________________________________
library(kernlab)
library(dplyr)
emb <- sample_n(embeddings[,-1],10000,replace=F)

kpca <- kpca(~.,data=emb,kernel="rbfdot",
                kpar=list(sigma=1),features=2)
plot(rotated(kpca))



