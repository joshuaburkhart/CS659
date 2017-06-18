set.seed(88)

NUM_FEATURES <- 2
NUM_CLASSES <- 4
ANN_WIDTH <- 5
ANN_DEPTH <- 3
ETA <- 0.1

#
# ------------   -   -   -   -----------
# num features x 5 x 5 x 5 x num classes
# ------------   -   -   -   -----------
#

input_weights <- array(data = runif(n=ANN_WIDTH*NUM_FEATURES),
                        dim = c(ANN_WIDTH,NUM_FEATURES))
input_activation <- matrix(data = 1,
                           nrow=NUM_FEATURES,
                           ncol=1)

output_gradients <- array(data = runif(n=ANN_WIDTH*NUM_CLASSES),
                        dim = c(ANN_WIDTH,NUM_CLASSES))
output_activation <- matrix(data = 1,
                           nrow=NUM_CLASSES,
                           ncol=1)

axon_weights <- array(data = runif(n=ANN_WIDTH*ANN_WIDTH*ANN_DEPTH),
                      dim = c(ANN_WIDTH,ANN_WIDTH,ANN_DEPTH))
neuron_gradients <- matrix(data = 1,
                        nrow=ANN_WIDTH,
                        ncol=ANN_DEPTH)
neuron_activation <- matrix(data = 1,
                        nrow=NUM_WIDTH,
                        ncol=ANN_DEPTH)

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
    dot_prod <<- t(input) %*% input_weights[k,(1:(NUM_FEATURES + 1)),1]
    neuron_activation[k,1] <<-
      activation_function(dot_prod)
  }
  
  # other layers
  for(i in 2:ANN_DEPTH){
    prev_layer <- c(1,neuron_activation[,i - 1])
    for(k in 1:NUM_CLASSES){ #neuron row
      neuron_activation[k,i] <<-
        activation_function(t(prev_layer) %*% axon_weights[k,,i])
    }
  }
}

error_prop <- function(err){
  # output layer
  for(j in 1:(NUM_CLASSES + 1)){
    neuron_gradients[j,ANN_DEPTH] <<-
      axon_weights[j,,ANN_DEPTH] %*% c(err,1) #rep(mse,(NUM_CLASSES + 1))
  }
  
  # other layers
  for(i in (ANN_DEPTH - 1):1){
    for(j in 1:NUM_CLASSES){
      neuron_gradients[j,i] <<-
        axon_weights[,j,i] %*%
        rep(neuron_gradients[j,i + 1],(NUM_CLASSES + 1)) %*%
        activation_function_deriv(neuron_activation[j,i])
    }
    neuron_gradients[NUM_CLASSES + 1,i] <<-
      axon_weights[,NUM_CLASSES + 1,i] %*%
      rep(neuron_gradients[NUM_CLASSES + 1,i + 1],(NUM_CLASSES + 1)) %*%
      activation_function_deriv(1)
  }
}

weight_update <- function(){
  # all layers
  for(i in 1:(ANN_DEPTH - 1)){
    for(k in 1:NUM_CLASSES){ #neuron row
      axon_weights[k,,i] <<- axon_weights[k,,i] + 
        ETA *
        neuron_gradients[k,i + 1] *
        neuron_activation[k,i + 1]
    }
    axon_weights[NUM_CLASSES + 1,,i] <<- axon_weights[NUM_CLASSES + 1,,i] + 
      ETA *
      neuron_gradients[NUM_CLASSES + 1,i + 1]
  }
}

# training_sample_class is a vector like [0, 0, 1, 0]
backward_prop <- function(training_sample_class){
  out_layer <- neuron_activation[,ANN_DEPTH]
  err <- (out_layer - training_sample_class)^2
  print(paste("err: ",err,sep=""))
  #mse <- sum(out_layer - training_sample_class)^2 / NUM_CLASSES
  #print(paste("mse: ",mse,sep=""))
  
  error_prop(err) #mse)
  
  weight_update()
}

train <- function(training_samples_features, training_samples_classes){
  for(i in 1:nrow(training_samples_features)){
    forward_prop(training_samples_features[i,])
    
    ann_class_encoding <- numeric(NUM_CLASSES)
    training_sample_class <- training_samples_classes[i]
    ann_class_encoding[training_sample_class] = 1
    print(ann_class_encoding)
    backward_prop(ann_class_encoding)
  }
}

predict <- function(test_samples_features){
  for(i in 1:nrow(test_samples_features)){
    forward_prop(test_samples_features[i,])
    prediction <- neuron_activation[,ANN_DEPTH]
    print(which(prediction == min(prediction)))
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

#forward_prop(scale(samples_f[1,]))
