# Load necessary libraries
library(ggplot2)
library(datasets)
library(purrr)

# Load the Iris dataset
data(iris)

# Display the first few rows of the dataset
head(iris)

# Summary of the dataset
summary(iris)

# Structure of the dataset
str(iris)

# Select only the numeric columns for clustering
iris_data <- iris[, 1:4]

# Function to calculate total within-cluster sum of squares for different k
wcss <- function(k) {
  kmeans(iris_data, k, nstart = 10)$tot.withinss
}

# Compute and plot WCSS for k = 1 to k = 10
k.values <- 1:10
wcss_values <- map_dbl(k.values, wcss)

plot(k.values, wcss_values, 
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# Apply K-Means with the optimal number of clusters
set.seed(42)
kmeans_result <- kmeans(iris_data, centers = 3, nstart = 25)

# Add the cluster results to the original dataset
iris$Cluster <- as.factor(kmeans_result$cluster)

# Plot clusters
ggplot(iris, aes(Petal.Length, Petal.Width, color = Cluster)) +
  geom_point(size = 3) +
  labs(title = "K-Means Clustering of Iris Data", 
       x = "Petal Length", 
       y = "Petal Width") +
  theme_minimal()
