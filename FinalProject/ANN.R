
NUM_CLASSES <- 4
ANN_WIDTH <- NUM_CLASSES
ANN_DEPTH <- 3

neuron_topology <- matrix(data = 1,nrow = ANN_WIDTH, ncol = ANN_DEPTH)
neuron_output <- neuron_topology

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
  for(j in 2:ANN_WIDTH){
    neuron_output[j,1] <- neuron_topology[j,1] *
      activation_function(sum(training_sample_features) + 1)
  }
  
  # other layers
  for(i in 2:ANN_DEPTH){
    prev_layer <- neuron_topology[,i - 1]
    for(j in 2:ANN_WIDTH){
      neuron_output[j,i] <- neuron_topology[j,i] *
        activation_function(sum(prev_layer))
    }
  }
}

# training_sample_class is a vector like [0, 0, 1, 0]
backward_prop <- function(training_sample_class){
  out_layer <- neuron_topology[,ncol(neuron_topology)]
  sqerror <- sum(out_layer - training_sample_class)^2
  
  # output layer
  for(j in ANN_WIDTH:2){
    neuron_topology[j,1] <- (neuron_output[j,1] - training_sample_class[j]) *
      activation_function_deriv(sqerror)
  }
  
  # other layers
  for(i in (ANN_DEPTH - 1):1){
    prev_layer <- neuron_output[,i + 1]
    for(j in ANN_WIDTH:2){
      neuron_topology[j,i] <- neuron_topology[j,i] *
        activation_function_deriv(sum(prev_layer))
    }
  }
}

train <- function(training_samples_features, training_samples_classes){
  for(training_sample_features in 1:training_samples_features){
    
  }
}

predict <- function(test_samples_features){
  for(test_sample_features in 1:test_samples_features){
    prediction <- forward_prop(test_sample_features)
    print(which(prediction == max(prediction)))
  }
}