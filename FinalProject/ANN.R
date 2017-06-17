
NUM_FEATURES <- 2
NUM_CLASSES <- 4
ANN_WIDTH <- max(NUM_FEATURES + 1,NUM_CLASSES + 1)
ANN_DEPTH <- 3

neuron_topology <- array(data = 1,dim=c(ANN_WIDTH,ANN_WIDTH,ANN_DEPTH))
neuron_output <- matrix(data = 1, nrow=ANN_WIDTH, ncol=ANN_DEPTH)

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
  input <- c(1,training_sample_features)
    for(k in 1:NUM_CLASSES){ #neuron row
      neuron_output[k,1] <<- activation_function(t(input) %*% neuron_topology[,k,1])
    }
  
  # other layers
  for(i in 2:ANN_DEPTH){
    prev_layer <- c(1,neuron_output[,i - 1])
      for(k in 1:NUM_CLASSES){ #neuron row
        neuron_output[k,i] <<- activation_function(t(prev_layer) %*% neuron_topology[,k,i])
    }
  }
}

# training_sample_class is a vector like [0, 0, 1, 0]
backward_prop <- function(training_sample_class){
  out_layer <- neuron_output[,ANN_DEPTH]
  mse <- sum(out_layer - training_sample_class)^2 / NUM_CLASSES
  
  print(paste("mse: ",mse,sep=""))
  
  # output layer
  for(j in 1:NUM_CLASSES){
    neuron_topology[j,,ANN_DEPTH] <<- (neuron_output[,ANN_DEPTH] - training_sample_class) * activation_function_deriv(mse)
  }
  
  # other layers
  for(i in (ANN_DEPTH - 1):1){
    prev_layer <- neuron_output[,i + 1]
    for(j in NUM_CLASSES:2){
      neuron_topology[j,i] <<- neuron_topology[j,i] *
        activation_function_deriv(sum(prev_layer))
    }
  }
}

train <- function(training_samples_features, training_samples_classes){
  for(itr in 1:100){
  for(i in 1:nrow(training_samples_features)){
    forward_prop(training_samples_features[i,])
    
    ann_class_encoding <- numeric(NUM_CLASSES)
    training_sample_class <- training_samples_classes[i]
    ann_class_encoding[training_sample_class] = 1
    print(ann_class_encoding)
    backward_prop(ann_class_encoding)
  }
  }
}

predict <- function(test_samples_features){
  for(i in 1:nrow(test_samples_features)){
    forward_prop(test_samples_features[i,])
    prediction <- neuron_output[,ANN_DEPTH]
    print(which(prediction == max(prediction)))
  }
}

samples_f <- matrix(
           c(1,2000,
             3,1000,
             2,4000,
             2,3500,
             3,1500),
           nrow = 5,
           ncol = 2,
           byrow = TRUE)

samples_c <- matrix(
  c(1,3,1,1,3),
  nrow = 5,
  ncol = 1,
  byrow = TRUE
)

norm_samples_f <- scale(samples_f)

train(norm_samples_f, samples_c)

predict(scale(samples_f))
