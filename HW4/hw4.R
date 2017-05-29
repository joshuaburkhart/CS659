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