#############
#### KNN ####
#############

library(class)
library(MASS)
library(dplyr)

#_________________________metti Data al posto di percorso cartella_________________________

#_________________ We select the pathology we want to focus on ____________

# [1] "No.Finding"                 "Enlarged.Cardiomediastinum"
# [3] "Cardiomegaly"               "Lung.Opacity"              
# [5] "Lung.Lesion"                "Edema"                     
# [7] "Consolidation"              "Pneumonia"                 
# [9] "Atelectasis"                "Pneumothorax"              
# [11] "Pleural.Effusion"           "Pleural.Other"             
# [13] "Fracture"                   "Support.Devices"           

### We look for the uncertainties
lab <- labels[,-1]
is_uncertain <- function(entry) {
  return(entry > 0 & entry < 1)
}
# We select the pathology
pathol <- lab[,6]
uncert <- is_uncertain(pathol)
sum(uncert)

embed <- data.frame(embeddings[,-1], group=as.factor(pathol))

nsample <- 1002
data <- sample_n(embed[-uncert,],nsample)
train <- sample(1:nsample,nsample*2/3)

emb.knn.cv <- knn.cv(train = data[,-931], cl = data[,931], k = 5, prob = F)
emb.knn.cv
err <- table(emb.knn.cv==data[,931])[1]/length(data[,931])



erroriK <- NULL

for (k in c(50,55,60,65,70)) {
    
    emb.knn.cv <- knn.cv(train = data[,-931], cl = data[,931], k = k, prob = F)
    #emb.knn.cv
    
    err <- table(emb.knn.cv==data[,931])[1]/length(data[,931])
    
    erroriK <- cbind(erroriK, err)
    
    #k <- which.min(erroriK)
    #err_diseases <- cbind(err_diseases, erroriK[k])
    #opt_num_k <- cbind(opt_num_k, k)
    cat(k,'')
}

erroriK
which.min(erroriK)
60 # This is the best k for Lung.Opacity!!!

trControl <- trainControl(method  = "cv", number  = 5)
fit <- train(group ~ .,
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:30),
             trControl  = trControl,
             metric     = "Accuracy",
             data       = data)
