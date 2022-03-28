#######################
#### RANDOM FOREST ####
#######################


library(MASS)
library(randomForest)
library(dplyr)

load("/Users/Francesco/Documents/ChestXray/XRAYLAB/training_densenet121PCA.RData")
load("/Users/Francesco/Documents/ChestXray/XRAYLAB/corr_labels.RData")


#_________________ We select the pathology we want to focus on ____________

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
# Uncertaninties Treatment for 'corr_labels'  -------------------------------
is_uncertain <- function(entry) {
  return(entry > 0 & entry < 1)
}
imp_prob <- 0.6 ### ???
#is_uncertain(corr_labels[,4])
uncert <- is_uncertain(labels[,5])
labels[uncert,5] <- rbinom(sum(uncert), 1, imp_prob) # rbinom(n, size, prob)
# ---------------------------------------------------------------------------

pathol <- labels[,5]  
rbind('1'=length(which(pathol==1)),'0'=length(which(pathol==0)))#/length(pathol)


embeddings30 <- data.frame(pc.embeddings$scores[,1:30]) 
embeddings30 <- data.frame(embeddings30, group=as.factor(pathol)) 

# When Class Imbalances -------------
library(caret)
data <- downSample(x=embeddings30[,-31],y=embeddings30$group)
colnames(data)[31] <- 'group'
train <- sample(1:length(data[,1]),length(data[,1])*2/3)
#------------------------------------

#set.seed(13)
nsample <- 30000
data <- sample_n(embeddings30,nsample)
train <- sample(1:nsample,nsample*2/3)

#set.seed(26)
rf.data <- randomForest(group~., data=data, subset=train, ntree=400)
rf.data
### OOB error VS test.error
rf.data$err[400,1] # (OOB Error; ntree=500)
test.error <- length( which( as.numeric(data[-train,31]) != 
                               as.numeric(predict(rf.data,data[-train,]))) )/length(train)
test.error


### Now we use a DECISION TREE: ----------------------------------------------------
# E.g. Support.Devices YES or NO, Lung.Opacity, ecc...
index <- which(labels[['Lung.Opacity']]==1) # REM. before we have to random analyze the label (see row 29)
length(index)
pathol <- labels[index,5]  
embeddings30 <- data.frame(pc.embeddings$scores[,1:30]) 
embeddings30 <- data.frame(embeddings30[index,], group=as.factor(pathol)) 

nsample <- 30000
data <- sample_n(embeddings30,nsample)
train <- sample(1:nsample,nsample*2/3)

#set.seed(26)
rf.data <- randomForest(group~., data=data, subset=train, ntree=400)
rf.data
### OOB error VS test.error
rf.data$err[400,] # (OOB Error; ntree=500)
test.error <- length( which( as.numeric(data[-train,31]) != 
                               as.numeric(predict(rf.data,data[-train,]))) )/length(train)
test.error

#------------------------- Graphics ----------------------------
library(randomForestExplainer)

min_depth <- min_depth_distribution(rf.data) ### We considered Lung.Opacity, U=1
save(min_depth, file = "min_depth_lungopa.rda")
head(min_depth,n=20)
plot_min_depth_distribution(min_depth, mean_sample = "relevant_trees", k = 30)

importance_frame <- measure_importance(rf.data)
save(importance_frame, file = "importance_frame_lungopa.rda")
plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes")
#plot_multi_way_importance(importance_frame, x_measure = "mean_min_depth", y_measure = "accuracy_decrease", size_measure = "p_value")


