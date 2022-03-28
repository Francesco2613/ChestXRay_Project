#------------------------ ROC and AUC -------------------------
library(pROC)
#library(ROCR) #???

# If we have run a RF:
prediction <- predict(rf.data,data[-train,])



roc_obj <- roc(as.numeric(data[-train,]$group), as.numeric(prediction))
auc(roc_obj)
plot(roc_obj, print.auc=F)
lines(roc_obj, col="red", type='b')
text(0.3, 0.2, labels=sprintf("AUC_logit_lasso_0.3: %0.3f", auc(roc_obj)), col="red")

dev.off()
