library(magrittr)
library(dplyr)

# Problem 1.1. Exploratory data analysis.
# Examine the dataset housing.txt using Matlab. Answer the following questions.
###############################################################################

housing_df <- read.table("housing.txt") 

# How many binary attributes are in the data set? List the attributes.

summary(housing_df)

# one. column 4 (no columns are labelled)

# Calculate and report correlations in between the first 13 attributes (columns) and the
# target attribute (column 14). What are the attribute names with the highest positive and
# negative correlations to the target attribute?

cor(housing_df) %>% .[,14] %>% sort() %>% rev()

# (no columns are labelled) highest positive: column 6, highest negative: column 13

# Note that the correlation is a linear measure of similarity. Examine scatter plots for
# attributes and the target attribute by writing your own function. Which scatter plot looks
# the most linear, and which looks the most nonlinear? Plot these scatter plots and briefly
# (in 1-2 sentences) explain your choice.

plot_stuff <- function(){
  plot(x=housing_df[,1],y=housing_df[,14])
  plot(x=housing_df[,2],y=housing_df[,14])
  plot(x=housing_df[,3],y=housing_df[,14])
  plot(x=housing_df[,4],y=housing_df[,14])
  plot(x=housing_df[,5],y=housing_df[,14])
  plot(x=housing_df[,6],y=housing_df[,14])
  plot(x=housing_df[,7],y=housing_df[,14])
  plot(x=housing_df[,8],y=housing_df[,14])
  plot(x=housing_df[,9],y=housing_df[,14])
  plot(x=housing_df[,10],y=housing_df[,14])
  plot(x=housing_df[,11],y=housing_df[,14])
  plot(x=housing_df[,12],y=housing_df[,14])
  plot(x=housing_df[,13],y=housing_df[,14])
  plot(x=housing_df[,14],y=housing_df[,14])
}

# most linear: column 7, most nonlinear: column 4
# Column 4 appears bimodal (0 or 1) and column 7 appears
# to be a fairly clear, horizontal line.

# Calculate all correlations between the 14 columns (using the corrcoef function).
# Which two attributes have the largest mutual correlation in the dataset?

# Columns 9 and 10 have a mutual correlation of 0.910228189

# Problem 1.2. Linear regression.
# Our goal is to predict the median value of housing based on the values of 13 attributes.
# For your convenience the data has been divided into two datasets: (1) a training dataset
# housing_train.txt you should use in the learning phase, and (2) a testing dataset
# housing_test.txt to be used for testing.
# Assume that we choose a linear regression model to predict the target attribute. Using
# Matlab:
########################################################################################

# Write a function LR_solve that takes X and y components of the data (X is a matrix
# of inputs where rows correspond to examples) and returns a vector of coefficients w with
# the minimal mean square fit. (Hint: you can use backslash operator ’/’ to do the least
# squares regression directly; check Matlab’s help).

LR_solve <- function(X,y){
  w <- solve(t(X) %*% X) %*% (t(X) %*% y)
  return(w)
}

# Write a function LR_predict that takes input components of the test data (X)
# and a fixed set of weights (w), and computes vector of linear predictions y.

LR_predict <- function(X,w){
  y <- X %*% w
  return(y)
}

# Write and submit the program main3_2.m that loads the train and test set, learns
# the weights for the training set, and computes the mean squared error of your predictor
# on both the training and testing data set.

LR_mse <- function(actual,predicted){
  errors <- actual - predicted
  sqerrors <- errors^2
  meansqerrors <- mean(sqerrors)
  return(meansqerrors)
}

main3_2.m <- function(){
  housing_train_df <- read.table("housing_train.txt")
  w <- LR_solve(as.matrix(housing_train_df[,1:13]),as.matrix(housing_train_df[,14]))
  
  housing_test_df <- read.table("housing_test.txt")
  y <- LR_predict(as.matrix(housing_test_df[,1:13]),w)
  
  # MSE
  y0 <- LR_predict(as.matrix(housing_train_df[,1:13]),w)
  
  mse0 <- LR_mse(as.matrix(housing_train_df[,14]),y0)
  mse1 <- LR_mse(as.matrix(housing_test_df[,14]),y)
}

# in your report please list the resulting weights, and both mean square errors.
# Compare the errors for the training and testing set. Which one is better?

# mse0 (train): 24.47588, mse1 (test): 24.29224
# It's somewhat surprising that the test mse came out slightly lower than the
# training mse. We'd expect the training data to show less error. 

# Problem 1.3. Online gradient descent
# The linear regression can be also learned using the gradient descent method.
##############################################################################

# Implement an online gradient descent procedure for finding the regression coefficients
# w. Your program should:

online_gd <- function(X,y,rate_divisor,error_report_intv){
  X <- cbind(rep(1,nrow(X)),X) # add X0 columns for intercept term
  
# start with zero weights (all weights set to 0 at the beginning);
  w <- as.matrix(rep(0,ncol(X)))
  w_temp <- w
  
# update weights using the annealed learning rate 2/t, where t denotes the t-th update
# step. Thus, for the first data point the learning rate is 2, for the second it is 2/2 = 1,
# for the 3-rd is 2/3 and so on;
  alpha <- function(t){
    rate <- 2/t
    return(rate/rate_divisor)
  }
# repeat the update procedure for 1000 steps reusing the examples in the training data
# if neccessary (hint: the index of the i-th example in the training set can be obtained
# by (i mod n) operation);
  plot(NULL,xlim=range(c(0,1000/error_report_intv)),ylim=range(c(0,6000)),ylab = "MSE",xlab="iteration/50")
  
  predicted_cache <- list()
  actual_cache <- list()
  for(t_idx in 0:999)
  {
    i <- (t_idx %% nrow(X)) + 1 # training sample idx
    a <- alpha(t_idx + 1)
    h <- (t(w) %*% X[i,])
    e <- y[i] - h
    predicted_cache <- cbind(predicted_cache,h)
    actual_cache <- cbind(actual_cache,y[i])
    if(i %% error_report_intv == 0){
      #print(paste("predicted_cache:",predicted_cache,"actual_cache:",actual_cache))
      interval_mse <- LR_mse(as.numeric(actual_cache),as.numeric(predicted_cache))
      print(paste("interval mse:",interval_mse,sep=" "))
      points(y=interval_mse,x=t_idx/error_report_intv)
      #predicted_cache <- list()
      #actual_cache <- list()
    }
    for(j in 1:length(w)){ # feature idx
      w_temp[j] <- w[j] + a * e * X[i,j]
    }
    w <- w_temp # simultaneous update!
    #print(w)
    #Sys.sleep(0.1)
    if(t_idx > 1){
      #exit()
    }
  }
# return the final set of weights.
  return(w)
}
  
online_gd(as.matrix(housing_train_df[,1:13]),as.matrix(housing_train_df[,14]),100000,5)

# Write a program main3_3.m that runs the gradient procedure on the data and at the
# end prints the mean test and train errors. Your program should normalize the data
# before running the method. Run it and report the results. Give the mean errors for
# both the training and test set. Is the result better or worse than the one obtained by solving
# the regression problem exactly?

main3_3.m <- function(){
  # train
  norm_housing_train_df <- housing_train_df %>% scale()
  w <- online_gd(as.matrix(norm_housing_train_df[,1:13]),as.matrix(norm_housing_train_df[,14]),1,10)
 
  # assess training
  train_predicted <- t(w) %*% t(cbind(rep(1,nrow(norm_housing_train_df)),as.matrix(norm_housing_train_df[,1:13])))
  train_actual <- t(as.matrix(norm_housing_train_df[,14]))
  print(paste("mean squared train errors:",LR_mse(train_actual,train_predicted),sep=" "))
  
  # test
  norm_housing_test_df <- housing_test_df %>% scale()
  test_predicted <- t(w) %*% t(cbind(rep(1,nrow(norm_housing_test_df)),as.matrix(norm_housing_test_df[,1:13])))
  test_actual <- t(as.matrix(norm_housing_test_df[,14]))
  print(paste("mean squared test errors:",LR_mse(test_actual,test_predicted),sep=" "))
}

# Run the gradient descent on un-normalized dataset. What happened?

# weights blow up and there's no convergence without a low alpha

# Modify main3_3.m from part b, such that it lets you to progressively observe changes
# in the mean train and test errors. Use functions init_progress_graph and
# add_to_progress_graph provided. The init_progress_graph initializes the graph structure
# and add_to_progress_graph lets you add new data entries on-fly to the graph. Using the
# two functions plot the mean squared errors for the training and test set for every 50
# iteration steps. Submit the program and include the graph in the report.

# Experiment with the gradient descent procedure. Try to use: fixed learning rate (say
# 0.05, 0.01), or different number of update steps (say 500 and 3000). You many want to
# change the learning rate schedule as well. Try for example 2 / n . Report your results
# and any interesting behaviors you observe.

# Problem 1.4. Regression with polynomials.



# Problem 2.1. Data analysis
# Problem 2.2. Logistic regression
# Problem 2.3. Generative classification model