CheXgaussian <- function(vector){
  
  lim <- max(abs(vector))
  lim <- round(lim, digits=0)
  x <- seq((-lim),lim,by=0.001)
  quartz(width = 10)
  par(mfrow=c(1,2))
  hist(vector, prob=T)
  lines(x, dnorm(x, mean(vector),sd(vector)), col='blue', lty=2)
  qqnorm(vector)
  qqline(vector)
  
}