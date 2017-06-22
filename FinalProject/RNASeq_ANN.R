set.seed(88)

library(scales)

NUM_TRAIN_IT <<- 2500
ETA <<- 1/NUM_TRAIN_IT
RAND_POINTS <<- 500 # displays decision boundary

# input layer
NUM_FEATURES <<- 9
INPUT_WIDTH <<- NUM_FEATURES + 1 # (bias)

# hidden layers
HIDDEN_LAYER_DEPTH <<- 3
HIDDEN_CORE_DEPTH <<- HIDDEN_LAYER_DEPTH - 1 # special dims for final hidden layer
HIDDEN_LAYER_WIDTH <<- 30
ANN_WIDTH <<- HIDDEN_LAYER_WIDTH + 1 # (bias)

# output layer
NUM_CLASSES <<- 4
OUTPUT_WIDTH <<- NUM_CLASSES

# input layer
input_activation <<- matrix(data = 1,
                            nrow = INPUT_WIDTH,
                            ncol = 1)
input_axon_w <<- matrix(data = runif(n = HIDDEN_LAYER_WIDTH * INPUT_WIDTH, min = -1, max = 1),
                        nrow = INPUT_WIDTH,
                        ncol = HIDDEN_LAYER_WIDTH)
# hidden core layers
hidden_core_deltas <<- matrix(data = 1,
                              nrow = HIDDEN_LAYER_WIDTH,
                              ncol = HIDDEN_CORE_DEPTH)
hidden_core_activation <<- matrix(data = 1,
                                  nrow = ANN_WIDTH,
                                  ncol = HIDDEN_CORE_DEPTH)
hidden_core_axon_w <<- array(data = runif(n = ANN_WIDTH * HIDDEN_LAYER_WIDTH * HIDDEN_CORE_DEPTH, min = -1, max = 1),
                             dim = c(ANN_WIDTH,
                                     HIDDEN_LAYER_WIDTH,
                                     HIDDEN_CORE_DEPTH))

# hidden final layer
hidden_final_deltas <<- matrix(data = 1,
                               nrow = HIDDEN_LAYER_WIDTH,
                               ncol = 1)
hidden_final_activation <<- matrix(data = 1,
                                   nrow = ANN_WIDTH,
                                   ncol = 1)
hidden_final_axon_w <<- matrix(data = runif(n = ANN_WIDTH * OUTPUT_WIDTH, min = 1, max = 1),
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
  
  hidden_core_activation[,1] <<-
    c(activation_function(t(input_axon_w) %*% input_activation),1)
  
  if(HIDDEN_CORE_DEPTH > 1){
    for(core_layer in 2:HIDDEN_CORE_DEPTH){
      hidden_core_activation[,core_layer] <<-
        c(activation_function(t(hidden_core_axon_w[,,core_layer - 1]) %*%
                                hidden_core_activation[,core_layer - 1]),1)
    }
  }
  
  hidden_final_activation <<-
    c(activation_function(t(hidden_core_axon_w[,,HIDDEN_CORE_DEPTH]) %*%
                            hidden_core_activation[,HIDDEN_CORE_DEPTH]),1)
  
  output_activation <<-
    activation_function(t(hidden_final_axon_w) %*% hidden_final_activation)
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
  
  hidden_final_deltas <<-        
    hidden_final_axon_w[1:HIDDEN_LAYER_WIDTH,] %*%      
    output_deltas *             
    (hidden_final_activation[1:HIDDEN_LAYER_WIDTH] *    
       (1 - hidden_final_activation[1:HIDDEN_LAYER_WIDTH]))
  
  hidden_core_deltas[,HIDDEN_CORE_DEPTH] <<-         
    hidden_core_axon_w[1:HIDDEN_LAYER_WIDTH,,HIDDEN_CORE_DEPTH] %*%     
    hidden_final_deltas *                            
    (hidden_core_activation[1:HIDDEN_LAYER_WIDTH,HIDDEN_CORE_DEPTH] *     
       (1 - hidden_core_activation[1:HIDDEN_LAYER_WIDTH,HIDDEN_CORE_DEPTH]))  
  
  if(HIDDEN_CORE_DEPTH > 1){
    for(core_layer in (HIDDEN_CORE_DEPTH - 1):1){
      hidden_core_deltas[,core_layer] <<-
        hidden_core_axon_w[1:HIDDEN_LAYER_WIDTH,,core_layer] %*%
        hidden_core_deltas[,core_layer + 1] *
        (hidden_core_activation[1:HIDDEN_LAYER_WIDTH,core_layer] *
           (1 - hidden_core_activation[1:HIDDEN_LAYER_WIDTH,core_layer]))
    }
  }
  
  input_axon_w <<-
    input_axon_w -
    ETA *
    input_activation %*%
    t(hidden_core_deltas[1:HIDDEN_LAYER_WIDTH,1])
  
  if(HIDDEN_CORE_DEPTH > 1){
    for(core_layer in 1:(HIDDEN_CORE_DEPTH - 1)){
      hidden_core_axon_w[,,core_layer] <<-
        hidden_core_axon_w[,,core_layer] -
        ETA *
        hidden_core_activation[,core_layer] %*%
        t(hidden_core_deltas[1:HIDDEN_LAYER_WIDTH,core_layer + 1])
    }
  }
  
  hidden_core_axon_w[,,HIDDEN_CORE_DEPTH] <<-     
    hidden_core_axon_w[,,HIDDEN_CORE_DEPTH] -     
    ETA *                                           
    hidden_core_activation[,HIDDEN_CORE_DEPTH] %*%
    t(hidden_final_deltas[1:HIDDEN_LAYER_WIDTH])
  
  hidden_final_axon_w <<-
    hidden_final_axon_w -
    ETA *
    hidden_final_activation %*%
    t(output_deltas)
  
  return(sqrt(sum((output_activation - encode_class(sample_class)) ^ 2)))
}

train <- function(training_samples_features, training_samples_classes){
  error_sum <- c()
  batch_errors <- c()
  mean_error_sum <- c()
  for(x in 1:NUM_TRAIN_IT){
     for(i in 1:nrow(training_samples_features)){
      #i <- ceiling(runif(n=1,min=0,max=nrow(training_samples_features)))
      forward_prop(training_samples_features[i,])
      error <- backward_prop(training_samples_classes[i])
      error_sum <- c(error_sum,error)
      batch_errors <- c(batch_errors,error)
     }
    mbe <- mean(batch_errors)
    print(paste("MBE: ",mbe,sep=""))
    zeta <- (mbe / (2 * 3^(1/2)))
    print(paste("ZETA: ",zeta,sep=""))
    ETA <<- zeta#/nrow(training_samples_features) #zeta # diagonal of 2u cube (norm max)
    print(paste("ETA: ",ETA,sep=""))
    mean_error_sum <- c(mean_error_sum,rep(mean(batch_errors),nrow(training_samples_features)))
    batch_errors <- c() #clear batch errors
  }
  plot(error_sum, col = "black",main="Individual (Black) & Mean (Red) Errors")#,col=seq(1:nrow(training_samples_features)),pch = seq(1:nrow(training_samples_features)))
  points(mean_error_sum, col="red")
}

predict <- function(test_sample_features){
  forward_prop(test_sample_features)
  prediction <- output_activation
  return(which(prediction == max(prediction)))
}

predict_n <- function(test_samples_features){
  ps <- c()
  for(i in 1:nrow(test_samples_features)){
    ps <- c(ps,predict(test_samples_features[i,]))
  }
  return(ps)
}

load("~/SoftwareProjects/CellFusionAnalysis/src/PrognosticPredictor/rna_seq/top10.rda")

samples_f <- matrix(
  c(.1,.6,1,1,1,1,1,1,1,
    .1,.7,1,1,1,1,1,1,1,
    .1,.8,1,1,1,1,1,1,1,
    .1,.9,1,1,1,1,1,1,1,
    0,0,1,1,1,1,1,1,1,
    0,.2,1,1,1,1,1,1,1,
    .3,.3,1,1,1,1,1,1,1,
    .3,.3,1,1,1,1,1,1,1,
    .3,.1,1,1,1,1,1,1,1,
    .6,.15,1,1,1,1,1,1,1,
    .6,.2,1,1,1,1,1,1,1,
    .6,.3,1,1,1,1,1,1,1,
    .6,.1,1,1,1,1,1,1,1,
    .6,.35,1,1,1,1,1,1,1,
    .7,.6,1,1,1,1,1,1,1,
    .7,.7,1,1,1,1,1,1,1,
    .7,.8,1,1,1,1,1,1,1,
    .7,.9,1,1,1,1,1,1,1,
    .8,.8,1,1,1,1,1,1,1,
    .8,.25,1,1,1,1,1,1,1,
    .8,.1,1,1,1,1,1,1,1,
    .8,.3,1,1,1,1,1,1,1,
    .8,.2,1,1,1,1,1,1,1,
    .8,.4,1,1,1,1,1,1,1),
  nrow = 24,
  ncol = NUM_FEATURES,
  byrow = TRUE)

samples_c <- matrix(
  c(1,1,1,1,2,2,2,2,1,1,1,1,1,1,2,2,2,2,3,3,3,3,3,3),
  nrow = 24,
  ncol = 1,
  byrow = TRUE
)

rand_row_order <- sample(105,replace = FALSE)

samples_f <- as.matrix(top2[rand_row_order,2:10])
samples_c <- as.matrix(top2[rand_row_order,"tumor_stage"])

norm_samples_f <- scales::rescale(samples_f, to = c(0,1))
train(norm_samples_f, samples_c)

plot(norm_samples_f[,1:2],col=samples_c,pch=samples_c,main="True Labels")

pred_train <- predict_n(norm_samples_f)
plot(norm_samples_f[,1:2],col=pred_train,pch=pred_train,main="Predicted Labels")


rand_points <<- matrix(data = runif(n = NUM_FEATURES * RAND_POINTS),
                       nrow = RAND_POINTS,
                       ncol = NUM_FEATURES)
norm_rand_points <- scales::rescale(rand_points, to = c(0,1))
pred_classes <- predict_n(norm_rand_points)
plot(norm_rand_points[,1:2],col=pred_classes,pch=pred_classes,main="Decision Boundary")


training_misclass_rate <- sum(samples_c != pred_train) / length(pred_train)
print(training_misclass_rate)