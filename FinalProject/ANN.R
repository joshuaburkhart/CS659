set.seed(88)

NUM_FEATURES <- 2
INPUT_WIDTH <- NUM_FEATURES + 1 # (bias)
NUM_CLASSES <- 4
OUTPUT_WIDTH <- NUM_CLASSES
NEURON_LAYER_WIDTH <- 5
ANN_WIDTH <- NEURON_LAYER_WIDTH + 1 # (bias)
ANN_DEPTH <- 3
ETA <- 1

#
# ----------------   -   -   -   -----------
# num features + 1 x 6 x 6 x 6 x num classes
# ----------------   -   -   -   -----------
#

input_weights <- array(data = runif(n=ANN_WIDTH*INPUT_WIDTH),
                        dim = c(ANN_WIDTH,INPUT_WIDTH))

input_activation <- matrix(data = 1,
                           nrow = INPUT_WIDTH,
                           ncol = 1)

output_weights <- array(data = runif(n=ANN_WIDTH*OUTPUT_WIDTH),
                        dim = c(ANN_WIDTH,OUTPUT_WIDTH))
output_gradients <- matrix(data = 1,
                            nrow = ANN_WIDTH,
                            ncol = 1)
output_activation <- matrix(data = 1,
                        nrow = OUTPUT_WIDTH,
                        ncol = 1)

axon_weights <- array(data = runif(n=ANN_WIDTH*ANN_WIDTH*ANN_DEPTH),
                      dim = c(ANN_WIDTH,ANN_WIDTH,ANN_DEPTH))
neuron_gradients <- matrix(data = 1,
                        nrow=ANN_WIDTH,
                        ncol=ANN_DEPTH)
neuron_activation <- matrix(data = 1,
                        nrow=ANN_WIDTH,
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
  input_activation <<- c(1,training_sample_features)
  for(row in 1:NEURON_LAYER_WIDTH){
    neuron_activation[row,1] <<-
      activation_function(input_activation %*%
                            input_weights[row,])
  }
  
  # hidden layers
  for(layer in 2:ANN_DEPTH){
    prev_layer <- neuron_activation[,layer - 1]
    for(row in 1:NEURON_LAYER_WIDTH){
      neuron_activation[row,layer] <<-
        activation_function(prev_layer %*%
                              axon_weights[row,,layer])
    }
  }
  
  #output layer
  for(row in 1:OUTPUT_WIDTH){
    output_activation[row] <<-
      activation_function(neuron_activation[,ANN_DEPTH] %*%
                            output_weights[,row])
  }
}

error_prop <- function(sqerr){
  
  # output gradients
  for(row in 1:ANN_WIDTH){
    output_gradients[row] <<-
    t(output_weights[row,]) %*%
      (sqerr *
      activation_function_deriv(output_activation))
  }
  
  # final hidden layer
  for(row in 1:ANN_WIDTH){
    neuron_gradients[row,ANN_DEPTH] <<-
      t(axon_weights[,row,ANN_DEPTH]) %*%
      (output_gradients *
      activation_function_deriv(neuron_activation[,ANN_DEPTH]))
  }
  
  # hidden layers
  for(layer in (ANN_DEPTH - 1):1){
    for(row in 1:ANN_WIDTH){
      neuron_gradients[row,layer] <<-
        t(axon_weights[,row,layer]) %*%
        (neuron_gradients[,layer + 1] *
        activation_function_deriv(neuron_activation[,layer]))
    }
  }
}

weight_update <- function(sqerr){
  
  # input layer
  for(row in 1:INPUT_WIDTH){
    input_weights[,row] <<-
      input_weights[,row] +
      ETA *
      rep(input_activation[row],ANN_WIDTH) *
      neuron_gradients[,1]
  }
  
  # hidden layers
  for(layer in 1:(ANN_DEPTH - 1)){
    for(row in 1:ANN_WIDTH){
      axon_weights[row,,layer] <<-
        axon_weights[row,,layer] + 
        ETA *
        rep(neuron_activation[row,layer],ANN_WIDTH) *
        neuron_gradients[,layer + 1]
    }
  }
  
  # final hidden layer
  for(row in 1:ANN_WIDTH){
    axon_weights[row,,ANN_DEPTH] <<-
      axon_weights[row,,ANN_DEPTH] +
      ETA *
      rep(neuron_activation[row,ANN_DEPTH],ANN_WIDTH) *
      output_gradients
  }
  
  # output layer
  for(row in 1:OUTPUT_WIDTH){
    output_weights[,row] <<-
      output_weights[,row] +
      ETA *
      sqerr[row]
  }
}

encode_class <- function(sample_class){
  ann_class_encoding <- numeric(NUM_CLASSES)
  ann_class_encoding[sample_class] <- 1
  return(ann_class_encoding)
}

calc_err <- function(sample_class){
  ann_class_encoding <- encode_class(sample_class)
  return(output_activation - ann_class_encoding)
}

backward_prop <- function(sample_class){
  er <- calc_err(sample_class)
  error_prop(er)
  weight_update(er)
}

train <- function(training_samples_features, training_samples_classes){
  for(i in 1:nrow(training_samples_features)){
    forward_prop(training_samples_features[i,])
    backward_prop(training_samples_classes[i])
  }
}

predict <- function(test_samples_features){
  for(i in 1:nrow(test_samples_features)){
    forward_prop(test_samples_features[i,])
    prediction <- output_activation
    print(which(prediction == max(prediction)))
  }
}

samples_f <- matrix(
  c(1,2000,
    3,1000,
    2,4000,
    2,4000,
    2,4000,
    2,4000,
    2,4000,
    2,4000,
    2,3500,
    3,1500),
  nrow = 10,
  ncol = 2,
  byrow = TRUE)

samples_c <- matrix(
  c(1,3,2,2,2,2,2,2,1,3),
  nrow = 10,
  ncol = 1,
  byrow = TRUE
)

norm_samples_f <- scale(samples_f)

train(norm_samples_f, samples_c)

predict(scale(samples_f))

#forward_prop(scale(samples_f[1,]))
#backward_prop(samples_c[1])
