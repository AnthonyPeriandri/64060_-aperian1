library(cluster)
library(factoextra)
library(proxy)
library(mclust)
library(dplyr)

cereals <- read.csv("Cereals.csv", stringsAsFactors = TRUE)
cereals_clean <- na.omit(cereals)
##show dataa
print(cereals_clean)
cereals_num <- cereals_clean[sapply(cereals_clean, is.numeric)]
##normalize dataset
cereals_scaled <- scale(cereals_num)
##agnes comparison
agnes_single <- agnes(cereals_scaled, method = "single")
agnes_complete <- agnes(cereals_scaled, method = "complete")
agnes_average <- agnes(cereals_scaled, method = "average")
agnes_ward <- agnes(cereals_scaled, method = "ward")
##show agnes comparison graphically
plot(agnes_single, main = "single linkage")
plot(agnes_complete, main = "complete linkage")
plot(agnes_average, main = "average linkage")
plot(agnes_ward, main = "ward's method")
fviz_nbclust(cereals_scaled, FUN = hcut, method = "silhouette", k.max = 20)
## choose k at elbow whcih is 2
k <- 2
clusters <- cutree(agnes_ward, k = k)
table(clusters)
set.seed(1)
##create data partitions
idx <- sample(1:nrow(cereals_scaled), size = 0.5 * nrow(cereals_scaled))
partitionA <- cereals_scaled[idx, ]
partitionB <- cereals_scaled[-idx, ]
agnes_A <- agnes(partitionA, method = "ward")
clusters_A <- cutree(agnes_A, k = k)
centroids_A <- aggregate(partitionA, by = list(cluster = clusters_A), FUN = mean)
centroids_matrix <- as.matrix(centroids_A[, -1])
dist_Bcentroids <- proxy::dist(partitionB, centroids_matrix)
clusters_B_1 <- apply(as.matrix(dist_Bcentroids), 1, which.min)
agnes_B <- agnes(partitionB, method = "ward")
clusters_B_2 <- cutree(agnes_B, k = k)
ari <- adjustedRandIndex(clusters_B_1, clusters_B_2)
##clsuters are showing to be unstable, increased numbers of clusters will be
## able to have more accurate data, but assingment asks for one "healthy"
## and one "unhealthy" group
print(paste("adjusted rand Index for cluster stability:", round(ari, 3)))
if (ari > 0.7) {
  cat("Clusters are stable.\n")
} else {
  cat("Clusters may not be stable.\n")
}
##show the avg data between both clusters
totaldata <- aggregate(cereals_num, by = list(cluster = clusters), mean)
print(totaldata)
##show both cluster groupings
cereals_clean$Cluster <- clusters
for (i in 1:k) {
  cat(paste0("Cluster ", i, ":\n"))
  print(cereals_clean[cereals_clean$Cluster == i, "name"])
  cat("\n")
}

##healthy cereal cluster 1(subjective data, i believe more vitamins and fiber outweighs 
##calorie and sugar quantities)
print(cereals_clean[cereals_clean$Cluster == 1, "name"])