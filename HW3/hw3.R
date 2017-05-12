set.seed(88)

library(magrittr)
library(scales)

# We have data from two classes. The first class has five samples {(-1,-1), (2,0), (2,1), (0,1), (0.5, 1.5)}.
# The second class has four samples {(3.5,2.5), (3,4), (5,2), (5.5,3)}.
matrix1 <- t(matrix(c(-1,-1,1,
             2,0,1,
             2,1,1,
             0,1,1,
             0.5,1.5,1,
             3.5,2.5,2,
             3,4,2,
             5,2,2,
             5.5,3,2),
             nrow=3))

matrix1[,1] <- matrix1[,1] %>% scale(center=TRUE) %>% rescale(to=c(0,1))
matrix1[,2] <- matrix1[,2] %>% scale(center=TRUE) %>% rescale(to=c(0,1))
matrix1[,3] <- matrix1[,3] %>% rescale(to=c(-1,1))

matrix1 <- cbind(rep(1,nrow(matrix1)),matrix1)

w <- as.matrix(runif(n=3,min=0,max=1))

roundAwayFromZero <- function(x){
  if(x >= 0){
    return(1)
  }else{
    return(-1)
  }
}

loopCounter = 0

repeat{
  classifications <- sapply(t(w) %*% t(matrix1[,1:3]),roundAwayFromZero)
  errs <- sum(abs(classifications - matrix1[,4]))
  if(errs == 0){
    break
  }
  for(i in 1:nrow(matrix1)){
    pred <- t(w) %*% matrix1[i,1:3] %>% roundAwayFromZero()
    if(pred != matrix1[i,4]){
      if(pred == -1){
        w <- w + matrix1[i,1:3]
      }else{
        w <- w - matrix1[i,1:3]
      }
    }
  }
  loopCounter = loopCounter + 1
}

plot(x=matrix1[,2],y=matrix1[,3],pch=3 + matrix1[,4],col=3 + matrix1[,4])
abline(b=(-(w[1]/w[3])/(w[1]/w[2])),a=-w[1]/w[3])

