PCA_embeddings <- function(data,filename) {
  
  pc.embeddings <- princomp(data[,-1], scores=T)
  
  # proportion of variances explained by each PC:
  #proportion <- pc.embeddings$sd^2/sum(pc.embeddings$sd^2)
  # we look for the values which respect the threshold:
  #n.components <- length(which( cumsum(proportion) < thresh ))
  
  print(sprintf("Given threshold = %s, we consider %s components", thresh, n.components))
  save(pc.embeddings, file = paste(filename, "PCA", ".RData", sep=""))

}