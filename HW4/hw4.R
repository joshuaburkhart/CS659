set.seed(88)

# Problem 2. Clustering experiments

library(magrittr)
library(dplyr)

clust_data_m <- read.csv("clean_clustering_data.txt",
                          header = FALSE,
                          sep=" ") %>%
  as.matrix()

## Part a.

sum_of_sums <- c()

for(i in 1:30){
  clusters <- kmeans(clust_data_m, centers = 2)
  print(paste("cluster size:", clusters$size, sep = " "))
  sum_of_sums <- c(sum_of_sums,clusters$tot.withinss)
}

print(paste("best clustering was ",
            which(sum_of_sums == min(sum_of_sums)),
            "with a sum of sums = ",
            min(sum_of_sums)))

## Part b.

sum_of_sums <- c()

row_sums <- rowSums(clust_data_m)

for(i in 1:30){
  min_center <- clust_data_m[which(row_sums < quantile(row_sums)[3]) %>% sample(1),]
  max_center <- clust_data_m[which(row_sums > quantile(row_sums)[3]) %>% sample(1),]
  
  new_centers <- matrix(c(min_center,max_center), nrow = 55,ncol = 2) %>%
    t()
  clusters <- kmeans(clust_data_m,
                     centers = new_centers)
  print(paste("cluster size:", clusters$size, sep = " "))
  sum_of_sums <- c(sum_of_sums,clusters$tot.withinss)
}

print(paste("best clustering was ",
            which(sum_of_sums == min(sum_of_sums)),
            "with a sum of sums = ",
            min(sum_of_sums)))

default_mins <- c()
num_default_mins <- c()

for(j in 1:100){
  sum_of_sums <- c()
  
  for(i in 1:30){
    clusters <- kmeans(clust_data_m, centers = 2)
    print(paste("cluster size:", clusters$size, sep = " "))
    sum_of_sums <- c(sum_of_sums,clusters$tot.withinss)
  }
  
  default_mins <- c(default_mins,min(sum_of_sums))
  num_default_mins <- c(num_default_mins,length(which(sum_of_sums == min(sum_of_sums))))
  
  print(paste("best clustering was ",
              which(sum_of_sums == min(sum_of_sums)),
              "with a sum of sums = ",
              min(sum_of_sums)))
}

plot(default_mins)
hist(num_default_mins,
     xlab = "Best clusterings Found In 30 Runs",
     main = "Default Seeds Finding Best Clustering")

proposed_mins <- c()
num_proposed_mins <- c()

for(j in 1:100){
  sum_of_sums <- c()
  
  row_sums <- rowSums(clust_data_m)
  
  for(i in 1:30){
    min_center <- clust_data_m[which(row_sums < quantile(row_sums)[3]) %>% sample(1),]
    max_center <- clust_data_m[which(row_sums > quantile(row_sums)[3]) %>% sample(1),]
    
    new_centers <- matrix(c(min_center,max_center), nrow = 55,ncol = 2) %>%
      t()
    clusters <- kmeans(clust_data_m,
                       centers = new_centers)
    print(paste("cluster size:", clusters$size, sep = " "))
    sum_of_sums <- c(sum_of_sums,clusters$tot.withinss)
  }
  
  proposed_mins <- c(proposed_mins,min(sum_of_sums))
  num_proposed_mins <- c(num_proposed_mins,length(which(sum_of_sums == min(sum_of_sums))))
  
  print(paste("best clustering was ",
              which(sum_of_sums == min(sum_of_sums)),
              "with a sum of sums = ",
              min(sum_of_sums)))
}

plot(proposed_mins)
hist(num_proposed_mins,
     xlab = "Best clusterings Found In 30 Runs",
     main = "Proposed Seeds Finding Best Clustering")
