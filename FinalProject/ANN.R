set.seed(88)

ETA = 0.001

# input layer
NUM_FEATURES <- 2
INPUT_WIDTH <- NUM_FEATURES + 1 # (bias)

# hidden layer
HIDDEN_LAYER_WIDTH <- 5
ANN_WIDTH <- HIDDEN_LAYER_WIDTH + 1 # (bias)

# output layer
NUM_CLASSES <- 4
OUTPUT_WIDTH <- NUM_CLASSES

# ----------------               -                -----------
# num features + 1 (input axons) 6 (hidden_axons) num classes
# ----------------               -                -----------

# input layer
input_activation <<- matrix(data = 1,
                           nrow = INPUT_WIDTH,
                           ncol = 1)
input_axon_w <<- matrix(data = runif(n = HIDDEN_LAYER_WIDTH * INPUT_WIDTH),
                       nrow = INPUT_WIDTH,
                       ncol = HIDDEN_LAYER_WIDTH)
# hidden layer
hidden_gradients <<- matrix(data = 1,
                           nrow = ANN_WIDTH,
                           ncol = 1)
hidden_activation <<- matrix(data = 1,
                            nrow = ANN_WIDTH,
                            ncol = 1)
hidden_axon_w <<- matrix(data = runif(n = ANN_WIDTH * OUTPUT_WIDTH),
                        nrow = ANN_WIDTH,
                        ncol = OUTPUT_WIDTH)
# output layer
output_gradients <<- matrix(data = 1,
                           nrow = OUTPUT_WIDTH,
                           ncol = 1)
output_activation <<- matrix(data = 1,
                            nrow = OUTPUT_WIDTH,
                            ncol = 1)

activation_function <- function(x){
  ex <- exp(x)
  return(ex / (1 + ex))
}

activation_function_deriv <- function(x){
  ex <- exp(x)
  return(ex / (1 + ex) * (1 - (ex / (1 + ex))))
}

forward_prop <- function(training_sample_features){
  # input layer
  input_activation <<- c(1,training_sample_features)
  #print(input_activation)
  for(hidden_idx in 1:HIDDEN_LAYER_WIDTH){
    hidden_activation[hidden_idx] <<-
      activation_function(t(input_activation) %*%
                            input_axon_w[,hidden_idx])
  }
  #print(hidden_activation)
  
  #output layer
  for(output_idx in 1:OUTPUT_WIDTH){
    output_activation[output_idx] <<-
      activation_function(t(hidden_activation) %*%
                            hidden_axon_w[,output_idx])
  }
}

encode_class <- function(sample_class){
  ann_class_encoding <- numeric(NUM_CLASSES)
  ann_class_encoding[sample_class] <- 1
  return(ann_class_encoding)
}

calc_err <- function(sample_class){
  ann_class_encoding <- encode_class(sample_class)
  return(activation_function(output_activation) - ann_class_encoding)
}

backward_prop <- function(sample_class){
  output_gradients <<- calc_err(sample_class)
  
  for(hidden_idx in 1:ANN_WIDTH){
    hidden_gradients[hidden_idx] <<-
      hidden_axon_w[hidden_idx,] %*%
      output_gradients
  }
  
  for(input_idx in 1:INPUT_WIDTH){
    input_axon_w[input_idx,] <<-
      input_axon_w[input_idx,] -
      ETA *
      hidden_gradients[1:HIDDEN_LAYER_WIDTH] *
      activation_function_deriv(input_activation[input_idx])
  }
  
  for(hidden_idx in 1:ANN_WIDTH){
    hidden_axon_w[hidden_idx,] <<-
      hidden_axon_w[hidden_idx,] -
      ETA *
      output_gradients *
      activation_function_deriv(hidden_activation[hidden_idx])
  }
}

train <- function(training_samples_features, training_samples_classes){
  for(x in 1:1000){
  for(i in 1:nrow(training_samples_features)){
    forward_prop(training_samples_features[i,])
    backward_prop(training_samples_classes[i])
  }
  }
}

predict <- function(test_sample_features){
  forward_prop(test_sample_features)
  prediction <- output_activation
  #print(output_activation)
  return(which(prediction == max(prediction)))
}

predict_n <- function(test_samples_features){
  ps <- c()
  for(i in 1:nrow(test_samples_features)){
    ps <- c(ps,predict(test_samples_features[i,]))
  }
  return(ps)
}

samples_f <- matrix(
  c(1.2,100,
    1.2,2000,
    1.5,300,
    1,300,
    1,1000,
    1.5,1500,
    2,2000,
    2,3000,
    1.5,1000,
    2,3000,
    2.5,7000,
    2.5,2500,
    2.5,1000,
    3,3000,
    3,2000),
  nrow = 15,
  ncol = 2,
  byrow = TRUE)

samples_c <- matrix(
  c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3),
  nrow = 15,
  ncol = 1,
  byrow = TRUE
)

norm_samples_f <- scale(samples_f)
train(norm_samples_f, samples_c)
plot(norm_samples_f,col=samples_c,pch=samples_c)
predict_n(norm_samples_f)

rand_points <<- matrix(data = runif(n = 2 * 100),
                        nrow = 100,
                        ncol = 2)

pred_classes <- predict_n(scale(rand_points))

plot(scale(rand_points),col=pred_classes,pch=pred_classes)
