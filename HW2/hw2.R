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

# Write a function extendx that takes an input x and returns an expanded x that
# includes all linear and degree two polynomials.

extendx <- function(x){
  z <- x
  for(col in 1:ncol(x)){
    z <- cbind(z,x[,col]^2)
  }
  return(z)
}

# What happened to the binary attribute after the transformation?

extended_housing_train_mtx <- scale(extendx(as.matrix(housing_train_df[,1:13])))
extended_housing_test_mtx <- scale(extendx(as.matrix(housing_test_df[,1:13])))

# the binary attribute was just duplicated... which makes sense as 1^2 = 1 and 0^2 = 0

# Write and submit a Matlab program main3_4.m that computes the regression
# coefficients for the extended input and both train and test errors for the result.

main3_4.m <- function(){
  # train
  w <- online_gd(extended_housing_train_mtx,as.matrix(norm_housing_train_df[,14]),2,10)
  
  # assess training
  xtrain_predicted <- t(w) %*% t(cbind(rep(1,nrow(extended_housing_train_mtx)),extended_housing_train_mtx))
  xtrain_actual <- t(as.matrix(norm_housing_train_df[,14]))
  print(paste("mean squared x train errors:",LR_mse(xtrain_actual,xtrain_predicted),sep=" "))
  
  # test
  xtest_predicted <- t(w) %*% t(cbind(rep(1,nrow(extended_housing_test_mtx)),extended_housing_test_mtx))
  xtest_actual <- t(as.matrix(norm_housing_test_df[,14]))
  print(paste("mean squared x test errors:",LR_mse(xtest_actual,xtest_predicted),sep=" "))
}

# Report both errors in your report and compare them with the results in part 2. What
# do you see? Which method would you use for the prediction? Why? Please do not turn in
# the weights for this part in your report.

# we get improved performance but have to tune the alpha down a bit.. dividing by 2

# Problem 2.1. Data analysis
# The dataset we use in this problem set is very simple and consists of a two-dimensional
# input and a class label. The data are available on the course web page and are divided
# into two files - one used for training (classification_train.txt), the other for testing
# (classification_test.txt). The data in files are in rows such that first two columns
# represent inputs and the last (third) column the class label (0 or 1).

# Since inputs are only two dimensional we can easily visualize the data for the two
# classes in a 2D plot.

# Write a program that plots the input data points in classification_train.txt such that
# the plot distinguishes between data points with different class labels (use different 
# color and symbol for a point, e.g. ’x’ or ’o’ ).

classification_train_df <- read.table("classification_train.txt")
plot(x=classification_train_df$V1,y=classification_train_df$V2,pch=2 + classification_train_df$V3,col=2 + 2 * classification_train_df$V3)

classification_test_df <- read.table("classification_test.txt")
plot(x=classification_test_df$V1,y=classification_test_df$V2,pch=2 + classification_test_df$V3,col=2 + 2 * classification_test_df$V3)


# Include the plot in your report. Is it possible to separate the two classes perfectly
# with a linear decision boundary?

# No, it does not appear the two classes are linearly separable.

# Problem 2.2. Logistic regression

# We are interested in building a classifier based on the logistic regression model and
# the gradient optimization methods.

# (a) During the class you were given the expression for the gradient of the logistic
# regression model. Use the loglikelihood setup from the lecture to derive the expression.

# Show clearly the steps of the derivation. Please remember that the ’default’ gradient
# takes into account all datapoints in the training set.

# (b) Write and submit a gradient procedure GLR.m for updating the parameters of the
# logistic regression model. Your gradient procedure should:

GLR.m <- function(X,y,rate_divisor,error_report_intv,k){
  X <- cbind(rep(1,nrow(X)),X) # add X0 columns for intercept term
  
  # – start from unit weights (all weights set to 1 at the beginning);
  w <- as.matrix(rep(1,ncol(X)))
  w_temp <- w
  
  # – use the annealed learning rate 2 / k ;
  alpha <- function(k){
    rate <- 2/k
    return(rate/rate_divisor)
  }
  # repeat the update procedure for 1000 steps reusing the examples in the training data
  # if neccessary (hint: the index of the i-th example in the training set can be obtained
  # by (i mod n) operation);
  plot(NULL,xlim=range(c(0,k/error_report_intv)),ylim=range(c(0,1)),ylab = "% error",xlab=paste("iteration/",error_report_intv,sep=""))
  
  predicted_cache <- list()
  actual_cache <- list()
  # – executes for K steps where K is the parameter of the procedures.
  for(k_idx in 0:(k - 1))
  {
    i <- (k_idx %% nrow(X)) + 1 # training sample idx
    a <- alpha(k_idx + 1)
    z <- (t(w) %*% X[i,])
    g <- round(1/(1 + exp(-z)))
    e <- y[i] - g
    predicted_cache <- cbind(predicted_cache,g)
    actual_cache <- cbind(actual_cache,y[i])
    if(i %% error_report_intv == 0){
      #print(paste("predicted_cache:",predicted_cache,"actual_cache:",actual_cache))
      pct_err <- sum(abs(round(class_train_predicted) - class_train_actual)) / length(predicted_cache)
      print(paste("pct_err:",pct_err,sep=" "))
      points(y=pct_err,x=k_idx/error_report_intv)
      #predicted_cache <- list()
      #actual_cache <- list()
    }
    for(j in 1:length(w)){ # feature idx
      w_temp[j] <- w[j] + a * e * X[i,j]
    }
    w <- w_temp # simultaneous update!
    #print(w)
    #Sys.sleep(0.1)
    if(k_idx > 1){
      #exit()
    }
  }
  # return the final set of weights.
  return(w)
}
  
# (c) Write and submit a program main2.m that runs the GLR function for 500 steps and
# after the training computes mean misclassification errors for both the training and
# test set. In your report include, the resulting weights, and misclassification errors.

to_binary <- function(non_binary){
  if(non_binary > 0){
    return(1)
  }
  return(0)
}

main2.m <- function(){
  # train
  w <- GLR.m(as.matrix(classification_train_df[,1:2]),as.matrix(classification_train_df[,3]),1,10,500)
  
  # assess training
  class_train_predicted <- t(w) %*% t(cbind(rep(1,nrow(classification_train_df)),as.matrix(classification_train_df[,1:2])))
  class_train_actual <- t(as.matrix(classification_train_df[,3]))
  print(paste("mean squared class train errors:",LR_mse(class_train_actual,class_train_predicted),sep=" "))
  num_err <- sum(abs(sapply(class_train_predicted,to_binary) - class_train_actual))
  tot <- nrow(classification_train_df)
  pct_err <- (num_err/tot) * 100
  print(paste("number class train errors:",num_err,
              " of ", tot,
              " = ", pct_err, "%",sep=" "))
  
  # test
  class_test_predicted <- t(w) %*% t(cbind(rep(1,nrow(classification_test_df)),as.matrix(classification_test_df[,1:2])))
  class_test_actual <- t(as.matrix(classification_test_df[,3]))
  print(paste("mean squared class test errors:",LR_mse(class_test_actual,class_test_predicted),sep=" "))
  num_err <- sum(abs(sapply(class_test_predicted,to_binary) - class_test_actual))
  tot <- nrow(classification_test_df)
  pct_err <- (num_err/tot) * 100
  print(paste("number class test errors:",num_err,
              " of ", tot,
              " = ", pct_err, "%",sep=" "))
}

# (d) Update the main2.m with plot functions that let you observe the progress of the
# errors after every 50 update steps. Use functions defined earlier for this purpose.
# Include the resulting graph in your report.

# (see above)

# (e) Experiment with the GLR function by:
# (I) changing the number of steps K and
# (II) trying different learning rates.
# In particular, try some constant learning rates and 1/ k learning rate schedule. 
# Report the results and graph from your experiments and explanations of behaviors
# you have observed.

main3.m <- function(){
  # train
  w <- GLR.m(as.matrix(classification_train_df[,1:2]),as.matrix(classification_train_df[,3]),2,100,10000)
  
  # assess training
  class_train_predicted <- t(w) %*% t(cbind(rep(1,nrow(classification_train_df)),as.matrix(classification_train_df[,1:2])))
  class_train_actual <- t(as.matrix(classification_train_df[,3]))
  print(paste("mean squared class train errors:",LR_mse(class_train_actual,class_train_predicted),sep=" "))
  num_err <- sum(abs(sapply(class_train_predicted,to_binary) - class_train_actual))
  tot <- nrow(classification_train_df)
  pct_err <- (num_err/tot) * 100
  print(paste("number class train errors:",num_err,
              " of ", tot,
              " = ", pct_err, "%",sep=" "))
  
  # test
  class_test_predicted <- t(w) %*% t(cbind(rep(1,nrow(classification_test_df)),as.matrix(classification_test_df[,1:2])))
  class_test_actual <- t(as.matrix(classification_test_df[,3]))
  print(paste("mean squared class test errors:",LR_mse(class_test_actual,class_test_predicted),sep=" "))
  num_err <- sum(abs(sapply(class_test_predicted,to_binary) - class_test_actual))
  tot <- nrow(classification_test_df)
  pct_err <- (num_err/tot) * 100
  print(paste("number class test errors:",num_err,
              " of ", tot,
              " = ", pct_err, "%",sep=" "))
}

# Problem 2.3. Generative classification model

# An alernative approach is to learn a generative model with class-conditional densities
# and use the parameters of such a model to do the prediction.

# Assume that an input x for each class (c = 0 or 1) follows a multivariate normal
# distribution. That is,
# <eqn>

# Further assume that the prior probability of a class is represented by a Bernoulli
# distribution.

# Parameters of the generative model can be computed from the training data using density
# estimation techniques, such as maximum likelihood estimation. Once this is ccomplished,
# we can use the estimates to make class predictions for new inputs.

# Let <eqn> represent parameter estimates. To predict the class we use
# discriminant functions based on the posterior probability of a class given the input
# and model parameters. This can be computed via Bayes rule:
# <eqn>

#Assume we want to use a generative model in which the two class conditional densities
#share the same covariance matrix Σ . That is:
# <eqn>
# Provide the following answers:

# (a) Give the formula for computing ML estimates of means of class conditional
# densities?

# (b) How would you go about computing the estimate of the covariance matrix Σ ?
# Note that the estimate of Σ must combine both class 0 and class 1 examples.

# (c) How would you estimate the prior of class
# <eqn>

# (d) Implement function Max_Likelihood that computes the estimates of model
# parameters using the training set.

Max_Likelihood <- function(X,y){
  N = nrow(X)
  num0 = sum(sapply(y, function(x) x == 0))
  num1 = sum(sapply(y, function(x) x == 1))
  prior0 = num0/N
  prior1 = num1/N
  mean0_x = 0
  mean1_x = 0
  cov01 = matrix(c(0, 0, 0, 0), byrow = TRUE, 2, 2)
  
  for(i in 1:N){
    if(y[i] == 0){
      mean0_x = mean0_x + X[i,]
    }
    else { # y[i] == 0
      mean1_x = mean1_x + X[i,]
    }
  }
  mean0 = mean0_x / num0
  mean1 = mean1_x / num1
  
  for(n in 1:N){
    if(y[n] == 0){
      cov01 = cov01 + (X[i,] - mean0) %*% t(X[i,] - mean0)
    }
    else { # y[i] = 0
      cov01 = cov01 + (X[i,] - mean1) %*% t(X[i,] - mean1)
    }
  }
  cov01 = cov01 / N
  return(list("prior0"=prior0,"prior1"=prior1,"mean0"=mean0,"mean1"=mean1,"cov01"=cov01))
}

# (e) Implement the function Predict_class that chooses the class using the discriminant
# functions based on class posteriors.

Predict_class <- function(X,params){
  w_func <- function(cov01,mean0,mean1){
    return(solve(cov01) %*% (mean0 - mean1))
  }
  
  w0_func <- function(cov01,mean0,mean1,prior0,prior1){
    return(as.numeric(
      -1/2 * t(mean0) %*% solve(cov01) %*% mean0
      +1/2 * t(mean1) %*% solve(cov01) %*% mean1
      + log(prior0/prior1)))
  }
  
  w <- w_func(params[["cov01"]],
              params[["mean0"]],
              params[["mean1"]])
  
  w0 <- w0_func(params[["cov01"]],
                params[["mean0"]],
                params[["mean1"]],
                params[["prior0"]],
                params[["prior1"]])
  
  predictions <- t(w) %*% t(X) + w0
  
  return(predictions)
}

# (f) Write and submit a program main4.m that learns the generative model and then uses
# it to compute the predictions. The program should compute mean misclassification errors
# for both training and testing datasets.

to_class0_binary <- function(non_binary){
  if(non_binary > 0){
    return(0)
  }
  return(1)
}

main4.m <- function(){
  params <- Max_Likelihood(as.matrix(classification_train_df[,1:2]),as.matrix(classification_train_df[,3]))
  predictions <- Predict_class(as.matrix(classification_train_df[,1:2]),params)

  # assess training
  class_train_predicted <- Predict_class(as.matrix(classification_train_df[,1:2]),params)
  class_train_actual <- t(as.matrix(classification_train_df[,3]))
  print(paste("mean squared class train errors:",LR_mse(class_train_actual,class_train_predicted),sep=" "))
  num_err <- sum(abs(sapply(class_train_predicted,to_class0_binary) - class_train_actual))
  tot <- nrow(classification_train_df)
  pct_err <- (num_err/tot) * 100
  print(paste("number class train errors:",num_err,
              " of ", tot,
              " = ", pct_err, "%",sep=" "))
  
  # test
  class_test_predicted <- Predict_class(as.matrix(classification_test_df[,1:2]),params)
  class_test_actual <- t(as.matrix(classification_test_df[,3]))
  print(paste("mean squared class test errors:",LR_mse(class_test_actual,class_test_predicted),sep=" "))
  num_err <- sum(abs(sapply(class_test_predicted,to_class0_binary) - class_test_actual))
  tot <- nrow(classification_test_df)
  pct_err <- (num_err/tot) * 100
  print(paste("number class test errors:",num_err,
              " of ", tot,
              " = ", pct_err, "%",sep=" "))  
}

# (g) Report the results (parameters of the generative model), and errors. Compare
# them to the results obtained in problem 2.