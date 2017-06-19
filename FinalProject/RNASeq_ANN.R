set.seed(88)

library(scales)

ETA <<- 0.1

# input layer
NUM_FEATURES <<- 2
INPUT_WIDTH <<- NUM_FEATURES + 1 # (bias)

# hidden1 layer
HIDDEN_LAYER_WIDTH <<- 15
ANN_WIDTH <<- HIDDEN_LAYER_WIDTH + 1 # (bias)

# output layer
NUM_CLASSES <<- 4
OUTPUT_WIDTH <<- NUM_CLASSES

# ----------------               -                -----------
# num features + 1 (input axons) 6 (hidden1_axons) num classes
# ----------------               -                -----------

# input layer
input_activation <<- matrix(data = 1,
                            nrow = INPUT_WIDTH,
                            ncol = 1)
input_axon_w <<- matrix(data = runif(n = HIDDEN_LAYER_WIDTH * INPUT_WIDTH),
                        nrow = INPUT_WIDTH,
                        ncol = HIDDEN_LAYER_WIDTH)
# hidden layer 0
hidden0_deltas <<- matrix(data = 1,
                          nrow = HIDDEN_LAYER_WIDTH,
                          ncol = 1)
hidden0_activation <<- matrix(data = 1,
                              nrow = ANN_WIDTH,
                              ncol = 1)
hidden0_axon_w <<- matrix(data = runif(n = ANN_WIDTH * HIDDEN_LAYER_WIDTH),
                          nrow = ANN_WIDTH,
                          ncol = HIDDEN_LAYER_WIDTH)

# hidden layer 1
hidden1_deltas <<- matrix(data = 1,
                         nrow = HIDDEN_LAYER_WIDTH,
                         ncol = 1)
hidden1_activation <<- matrix(data = 1,
                             nrow = ANN_WIDTH,
                             ncol = 1)
hidden1_axon_w <<- matrix(data = runif(n = ANN_WIDTH * OUTPUT_WIDTH),
                         nrow = ANN_WIDTH,
                         ncol = OUTPUT_WIDTH)
# output layer
output_deltas <<- matrix(data = 1,
                         nrow = OUTPUT_WIDTH,
                         ncol = 1)
output_activation <<- matrix(data = 1,
                             nrow = OUTPUT_WIDTH,
                             ncol = 1)

activation_function <- function(x){
  1/(1 + exp(-(x)))
}

forward_prop <- function(training_sample_features){
  # input layer
  input_activation <<- c(training_sample_features,1)
  
  hidden0_activation <<-
    c(activation_function(t(input_axon_w) %*% input_activation),1)
  
  hidden1_activation <<-
    c(activation_function(t(hidden0_axon_w) %*% hidden0_activation),1)
  
  output_activation <<-
    activation_function(t(hidden1_axon_w) %*% hidden1_activation)
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
  output_deltas <<- calc_err(sample_class)
  
  hidden1_deltas <<-
    hidden1_axon_w %*%
    output_deltas *
    hidden1_activation *
    (1 - hidden1_activation)
  
  hidden0_deltas <<-
    hidden0_axon_w %*%
    hidden1_deltas[1:HIDDEN_LAYER_WIDTH] *
    hidden0_activation *
    (1 - hidden0_activation)
  
  input_axon_w <<-
    input_axon_w -
    ETA *
    input_activation %*%
    t(hidden0_deltas[1:HIDDEN_LAYER_WIDTH])
  
  hidden0_axon_w <<-
    hidden0_axon_w -
    ETA *
    hidden0_activation %*%
    t(hidden1_deltas[1:HIDDEN_LAYER_WIDTH])
  
  hidden1_axon_w <<-
    hidden1_axon_w -
    ETA *
    hidden1_activation %*%
    t(output_deltas)
  
  return(sqrt(sum((output_activation - encode_class(sample_class)) ^ 2)))
  #return(sum(output_deltas))
}

train <- function(training_samples_features, training_samples_classes){
  error_sum <- c()
  for(x in 1:1000){
    for(i in 1:nrow(training_samples_features)){
      forward_prop(training_samples_features[i,])
      error_sum <- c(error_sum,backward_prop(training_samples_classes[i]))
    }
  }
  plot(error_sum)
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

load("~/SoftwareProjects/CellFusionAnalysis/src/PrognosticPredictor/rna_seq/top2.rda")

samples_f <- matrix(
  c(0,0,
    0,.2,
    .3,.3,
    .3,.3,
    .3,.1,
    .6,.15,
    .6,.2,
    .6,.3,
    .6,.1,
    .6,.35,
    .8,.8,
    .8,.25,
    .8,.1,
    .8,.3,
    .8,.2,
    .8,.4),
  nrow = 16,
  ncol = 2,
  byrow = TRUE)

samples_c <- matrix(
  c(2,2,2,2,1,1,1,1,1,1,3,3,3,3,3,3),
  nrow = 16,
  ncol = 1,
  byrow = TRUE
)

#samples_f <- as.matrix(top2[,2:3])
#samples_c <- as.matrix(top2$tumor_stage)

norm_samples_f <- scales::rescale(samples_f, to = c(0,1))
train(norm_samples_f, samples_c)

plot(norm_samples_f,col=samples_c,pch=samples_c)

pred_train <- predict_n(norm_samples_f)
plot(norm_samples_f,col=pred_train,pch=pred_train)


rand_points <<- matrix(data = runif(n = 2 * 100),
                       nrow = 100,
                       ncol = 2)
norm_rand_points <- scales::rescale(rand_points, to = c(0,1))
pred_classes <- predict_n(norm_rand_points)
plot(norm_rand_points,col=pred_classes,pch=pred_classes)
