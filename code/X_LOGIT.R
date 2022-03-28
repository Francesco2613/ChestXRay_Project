###___________________________________________________________________###
###--------------------------- LOGISTIC REGRESSION -------------------###
###___________________________________________________________________###
library(MASS)
library(dplyr)
library(glmnet)

load("/Users/Francesco/Documents/ChestXray/XRAYLAB/training_densenet121.RData")
rm(labels)
load("/Users/Francesco/Documents/ChestXray/XRAYLAB/corr_labels.RData")
load("/Users/Francesco/Documents/ChestXray/XRAYLAB/U_Binary.RData")


# [1] "No.Finding"                 "Enlarged.Cardiomediastinum"
# [3] "Cardiomegaly"               "Lung.Opacity"              
# [5] "Lung.Lesion"                "Edema"                     
# [7] "Consolidation"              "Pneumonia"                 
# [9] "Atelectasis"                "Pneumothorax"              
# [11] "Pleural.Effusion"           "Pleural.Other"             
# [13] "Fracture"                   "Support.Devices"           

labels <- labels1[,-1]
labels <- labels0[,-1]
labels <- corr_labels[,-1]

# Uncertaninties Treatment for 'corr_labels'  ------------------------------------
is_uncertain <- function(entry) {
  return(entry > 0 & entry < 1)
}
imp_prob <- 0.6 ### ???
#is_uncertain(corr_labels[,4])
uncert <- is_uncertain(labels[,3])
labels[uncert,3] <- rbinom(sum(uncert), 1, imp_prob) # rbinom(n, size, prob)

# ---------------------------------------------------------------------------------


# (As before) we select the pathology and esxtraxt the dataset:

pathol <- labels[,3]  
#embeddings30 <- data.frame(pc.embeddings$scores[,1:30]) 
#embed <- data.frame(embeddings30, group=as.factor(pathol))
# or 
embeddings930 <- embeddings[,-1]
embed <- data.frame(embeddings930, group=as.factor(pathol))

#set.seed(13)
nsample <- 15000
data <- sample_n(embed,nsample)
train <- sample(1:nsample,nsample*2/3)

# Logistic Regression ----------------------------
fit1 <- glm(group~. ,data=data[train,], family='binomial')
summary(fit1)
prediction <- ifelse(predict(fit1,data[-train,]) > .5, 1,0)
error.rate <- length(which(prediction != data[-train,]$group))/length(data[-train,]$group)
error.rate

# or we can use...

# Lasso Logistic Regression --------------------------------
# Dumy code categorical predictor variables
x <- model.matrix(group~., data=data[train,])[,-1]
# Convert the outcome (class) to a numerical variable
y <- data[train,]$group
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
plot(cv.lasso)
cv.lasso$lambda.min
# Fit the final model on the training data
fit2 <- glmnet(x, y, alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.min)
fit2$df # gives us the number of features used by the model!!!
# Make predictions on the test data
x.test <- model.matrix(group~., data=data[-train,])[,-1]
probabilities <- fit2 %>% predict(newx = x.test)
prediction <- ifelse(probabilities > 0.5, 1,0)
error.rate <- length(which(prediction != data[-train,]$group))/length(data[-train,]$group)
error.rate



### Now we use a DECISION TREE: ----------------------------------------------------
# E.g. Support.Devices YES or NO, Lung.Opacity, ecc...
index <- which(labels[['Lung.Opacity']]==1) # REM. before we have to random analyze the label (see row 29)
length(index)
pathol <- labels[index,6]  
embeddings30 <- data.frame(pc.embeddings$scores[,1:30]) 
embeddings30 <- data.frame(embeddings30[index,], group=as.factor(pathol)) 

nsample <- 30000
data <- sample_n(embeddings30,nsample)
train <- sample(1:nsample,nsample*2/3)
#set.seed(26)
fit2 <- glm(group~. ,data=data[train,], family='binomial')
summary(fit2)
prediction <- ifelse(predict(fit2,data[-train,-31]) > .5, 1,0)
error.rate <- length(which(prediction != data[-train,31]))/10000
error.rate



# We remove the 'useless' components (non optimal way...) ------------------------
#erase <- as.numeric(which(coef(summary(fit1))[,"Pr(>|z|)"] >.05) -1) # '-1' because erase[2]=Comp1 ecc..
#fit2 <- glm(group~. ,data=data[train,-erase], family='binomial') 
#summary(fit2)
## We compute and compare the error rate:
#pred <- rbind(predict(fit1, data.frame(data[-train,-31]), type='response'),
#              predict(fit2, data.frame(data[-train,-31]), type='response'))
#pred <- ifelse(pred > .5, 1,0)
#error <-c(error.1=length(which(pred[1,] != data[-train,31]))/4000,
#          error.2=length(which(pred[2,] != data[-train,31]))/4000 )
#error

# We remove the features (optimal way) -------------------------------------------
backwards <- step(fit1,trace=0)
formula(backwards)
# or
nothing <- glm(group~1,data=data[train,],family='binomial')
forwards <- step(nothing,
                 scope=list(lower=formula(nothing),upper=formula(fit1)),
                 direction="forward",trace=0)
formula(forwards)

fit2 <- glm(formula(backwards),data=data[train,],family='binomial')
summary(fit2)
c(fit1$aic,fit2$aic)

