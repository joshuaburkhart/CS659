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
w_init <- w

plot(x=matrix1[,2],y=matrix1[,3],pch=3 + matrix1[,4],col=3 + matrix1[,4],xlim=c(-1,2),ylim=c(-1,2))
abline(b=(-(w[1]/w[3])/(w[1]/w[2])),a=-w[1]/w[3],col="black")


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

abline(b=(-(w[1]/w[3])/(w[1]/w[2])),a=-w[1]/w[3],col="green")

print(paste("initial weights: ",w_init,sep=""))
print(paste("final weights: ",w,sep=""))
print(paste("iterations: ",loopCounter,sep=""))

# problem 2

main3_1.m <- function(){
  
  library(e1071)
  
  pima_train_df <- read.table("pima_train.txt")
  pima_test_df <- read.table("pima_test.txt")
  
  model <- e1071::svm(kernel="linear",x=pima_train_df[,1:8],y=pima_train_df[,9])
  
  round01 <- function(x){
    if(x >= 0.5){
      return(1)
    }else{
      return(0)
    }
  }
  
  train_pred <- predict(model,pima_train_df[,1:8])
  train_errs <- sum(abs(sapply(train_pred,round01) - pima_train_df[,9]))
  train_mean_misclass <- train_errs / nrow(pima_train_df)
  
  test_pred <- predict(model,pima_test_df[,1:8])
  test_errs <- sum(abs(sapply(test_pred,round01) - pima_test_df[,9]))
  test_mean_misclass <- test_errs / nrow(pima_test_df)
  
  confusionMatrix <- function(y,y_hat){
    true_neg = 0
    true_pos = 0
    false_neg = 0
    false_pos = 0
    for(i in 1:length(y)){
      if(y[i] == 0 && y_hat[i] == 0){
        true_neg = true_neg + 1
      }
      else if(y[i] == 0 && y_hat[i] == 1){
        false_pos = false_pos + 1
      }
      else if(y[i] == 1 && y_hat[i] == 1){
        true_pos = true_pos + 1
      }
      else if(y[i] == 1 && y_hat[i] == 0){
        false_neg = false_neg + 1
      }else{
        print(paste("error! y[",i,"]: ",y[i],", y_hat[",i,"]: ",y_hat[i],sep=""))
      }
    }
    return(matrix(c(true_neg,false_pos,false_neg,true_pos),nrow=2))
  }
  
  conf_train <- confusionMatrix(pima_train_df[,9],sapply(train_pred,round01))
  conf_test <- confusionMatrix(pima_test_df[,9],sapply(test_pred,round01))
  
  print(paste("mean misclassification training error: ",train_mean_misclass,sep=""))
  print(paste("mean misclassification test error: ",test_mean_misclass,sep=""))
  print(paste("training confusion matrix: ",conf_train,sep=""))
  print(paste("test confusion matrix: ",conf_test,sep=""))
  
}

main3_1.m()

