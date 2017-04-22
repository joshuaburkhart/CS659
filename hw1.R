library(magrittr)
library(ggplot2)

poisson_probability2 <- function(x){
  lambda = 2
  return((exp(1)^-lambda * lambda^x)/factorial(x))
}

poisson_probability6 <- function(x){
  lambda = 6
  return((exp(1)^-lambda * lambda^x)/factorial(x))
}

poisson_probabilityML <- function(df){
  n = nrow(df)
  sum = sum(df[,1])# assuming x is in column 1
  return(sum/n)
}

gamma_function <- function(a){
  return(factorial(a - 1))
}

gamma_distribution12 <- function(lambda){
  a = 1
  b = 2
  return((1/(b^a * gamma_function(a))) * lambda^(a - 1) * exp(1)^(-lambda/b))
}

gamma_distribution35 <- function(lambda){
  a = 3
  b = 5
  return((1/(b^a * gamma_function(a))) * lambda^(a - 1) * exp(1)^(-lambda/b))
}

poisson_posterior_density12 <- function(x){
  lambda = ML = 5.24
  a = 1
  b = 2
  return((1/(b^a * gamma_function(x + 1)) * gamma_function(a)) * lambda^(x + a - 1) * exp(1)^(-lambda - lambda/b))
}

poisson_posterior_density35 <- function(x){
  lambda = ML = 5.24
  a = 3
  b = 5
  return((1/(b^a * gamma_function(x + 1)) * gamma_function(a)) * lambda^(x + a - 1) * exp(1)^(-lambda - lambda/b))
}

#(e) plot the probability function for Poisson distributions with parameters λ = 2 and λ = 6. 
#    Note that the Poisson model is defined over nonnegative integers only.

data.frame(x = 0) %>%
  ggplot(mapping = aes(x = x)) +
  stat_function(fun = poisson_probability2,
                           mapping = aes(color = "poisson_probability2")) +
  stat_function(fun = poisson_probability6,
                           mapping = aes(color = "poisson_probability6")) +
  scale_x_continuous(limits = c(0, 15)) +
  scale_color_manual(values = c("blue", "orange"),
                     labels = c("λ = 2", "λ = 6")) + 
  ggtitle("Poisson Distributions") +
  theme(plot.title = element_text(hjust = 0.5))

#(f) Assume the data in ’poisson.txt’ that represent the number of incoming phone calls received
#    over a fixed period of time. Compute and report the ML estimate of the parameter λ .

poisson_df <- read.table("poisson.txt")
ML = poisson_probabilityML(poisson_df) # comes out to 5.24

plot(x = seq(1,25),
     y = poisson_df[,1],
     xlab = "index",
     ylab = "value",
     title(paste("'poisson.txt' ML = ",ML,sep="")))
abline(h = ML, col = "red")

#(g) Assume the prior on λ is given by Gamma(a, b). Plot the Gamma distribution for the following
#    set of parameters (a = 1, b = 2) and (a = 3, b = 5).

data.frame(x = 0) %>%
  ggplot(mapping = aes(x = x)) +
  stat_function(fun = gamma_distribution12,
                mapping = aes(color = "gamma_distribution12")) +
  stat_function(fun = gamma_distribution35,
                mapping = aes(color = "gamma_distribution35")) +
  scale_x_continuous(limits = c(0, 50)) +
  scale_color_manual(values = c("green", "red"),
                     labels = c("a = 1, b = 2", "a = 3, b = 5")) + 
  ggtitle("Gamma Distributions") +
  theme(plot.title = element_text(hjust = 0.5))

#(h) Plot the posterior density for λ after observing samples in ’poission.txt’ and using priors 
#    in part (g). What changes in the distribution do you observe?

data.frame(x = 0) %>%
  ggplot(mapping = aes(x = x)) +
  stat_function(fun = poisson_posterior_density12,
                mapping = aes(color = "poisson_posterior_density12")) +
  stat_function(fun = poisson_posterior_density35,
                mapping = aes(color = "poisson_posterior_density35")) +
  scale_x_continuous(limits = c(0, 20)) +
  scale_color_manual(values = c("purple", "pink"),
                     labels = c("a = 1, b = 2", "a = 3, b = 5")) + 
  ggtitle("Poisson Posterior Density") +
  theme(plot.title = element_text(hjust = 0.5))

