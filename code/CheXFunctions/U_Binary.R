U_Binary <- function(labels){
  r <- dim(labels)[1]
  c <- dim(labels)[2]
  labels0 <- labels
  labels1 <- labels
  for(i in 2:c){
    for(j in 1:r){
      if(labels[,i][j]!=1 && labels[,i][j]!=0) {labels0[,i][j]=0; labels1[,i][j]=1} 
      }
  }; rm(i,j)
  
  save(list = c("labels0", "labels1"), file = "U_Binary.RData")
}