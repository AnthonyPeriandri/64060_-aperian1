library(dplyr)
library(ggplot2)
library(cluster)
dataset <- read.csv("Pharmaceuticals.csv")
show(dataset)
cluster_data <- dataset[, 1:9]
cluster_data <- cluster_data[, sapply(cluster_data, is.numeric)]
normalized_data <- scale(cluster_data)
## Euclidean distance works well with normalized numeric values
euclidean_calc <- dist(normalized_data, method = "euclidean")
print(euclidean_calc)
heatmap(as.matrix(euclidean_calc), symm = TRUE,
        col = colorRampPalette(c("brown", "orange"))(100),
        main = "Euclidean Distance")
## using angle method to find best k 
set.seed(349)
optimal_cluster <- sapply(1:10, function(k){
  kmeans(normalized_data, centers = k, nstart = 25)$tot.withinss
})
plot(1:10, optimal_cluster, type = "b", main = "Angle Method", xlab = "Number of clusters", ylab = "within clusters")
## using silhouette method to determine best k because angle in angle method was unclear
silhouette_method <- sapply(2:6, function(k) {
  pam(normalized_data, k = k)$silinfo$avg.width
})
plot(2:6, silhouette_method, type = "b", xlab = "k", ylab = "avg silhouette", main = "Silhouette method for best k")
## best k is 3 
set.seed(349)
result <- kmeans(normalized_data, centers = 3, nstart = 25)
dataset$cluster <- as.factor(result$cluster)
datasummary <- dataset %>%
  group_by(cluster) %>%
  summarise(across(1:9, mean, .names = "mean_{.col}"))
print(datasummary)
cat("cluster vs median recommendation\n")
print(table(dataset$cluster, dataset$Median_Recommendation))
cat("cluster vs location\n")
print(table(dataset$cluster, dataset$Location))
cat("cluster vs stock exchange\n")
print(table(dataset$cluster, dataset$Exchange))
