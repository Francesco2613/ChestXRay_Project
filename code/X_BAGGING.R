library(dplyr)

set.seed(123)

rm(list=ls())
dev.off()

## Function to check uncertainty (useful if we change the definition of uncertainty)
is_uncertain <- function(entry) {
  return(entry > 0 & entry < 1)
}


# [1] "Path"                       "No.Finding"                 "Enlarged.Cardiomediastinum" "Cardiomegaly"              
# [5] "Lung.Opacity"               "Lung.Lesion"                "Edema"                      "Consolidation"             
# [9] "Pneumonia"                  "Atelectasis"                "Pneumothorax"               "Pleural.Effusion"          
# [13] "Pleural.Other"              "Fracture"                   "Support.Devices"

pathol <- 'Lung.Opacity'

## Validation set
# load('data/labels_validation_densenet121.RData')
# load('data/pca30_validation_densenet121.RData')
# 
# validation_set <- data.frame(embeddings30, group=labels[[pathol]])
# validation_set <- validation_set[!is_uncertain(validation_set$group),]
# rm(embeddings30, labels)


## Training set
load("/Users/Francesco/Documents/ChestXray/XRAYLAB/training_densenet121PCA.RData")
load("/Users/Francesco/Documents/ChestXray/XRAYLAB/corr_labels.RData")
embeddings30 <- data.frame(pc.embeddings$scores[,1:30]) 


training_set <- data.frame(embeddings30, group=corr_labels[[pathol]])
#rm(embeddings30, labels)

# Use part of the training_set for validation: overfitting!
val_idx <-  sample(1:nrow(training_set), 10000)
validation_set <- training_set[val_idx, ]
training_set <- training_set[-val_idx, ] # Remove validation rows from training set
validation_set <- validation_set[!is_uncertain(validation_set$group),]
rm(val_idx)


## Undersample (with replacement) the training set
undersample <- function(training_set, size, imp_prob) {
  data <- sample_n(training_set, size, replace=TRUE)
  
  # Imputation of uncertain values
  uncert <- is_uncertain(data$group)
  data[uncert, ]$group <- rbinom(sum(uncert), 1, imp_prob) # rbinom(n, size, prob)
  
  return(data)
}


N <- 1 # Number of bagging iterations
size <- 20000 # Size of each subsampled training set
imp_prob <- .65 # Prob of u -> 1 

coeffs <- rep(0, ncol(training_set)) # Correct size (one for the intercept)

# Bagging iterations
# At each iteration:
#  1) subsample (with replacement) a new training set
#  2) Assign uncertainty to 1 with prob 'imp_prob', 0 otherwise
#  3) fit logit
#  4) save regression coefficients for model averaging
for (i in 1:N) {
  us_data <- undersample(training_set, size, imp_prob)
  fit <- glm(group ~ ., data=us_data, family='binomial')
  #fit <- glm(group ~ ., data=us_data, family=binomial(link = 'probit'))
  coeffs <- coeffs + fit$coefficients
}

rm(i, us_data)

coeffs <- coeffs/N  # model averaging
coeffs


# Assign the averaged coefficients
fit$coefficients <- coeffs
fit$aic

## -------------
## PREDICTION
## -------------

preds <- predict(fit, validation_set, type='response')

#cbind(preds, test$group)
preds <- ifelse(preds > .5, 1, 0)
#cbind(preds, test$group)

error <- sum(preds != validation_set$group) / length(preds)
error

