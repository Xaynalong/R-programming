#Clustering
rm(list = ls()) #remove all the variable in the session
set.seed(31538312) # XXXXXXXX = your student ID



#WARMUP

data(iris)
View(iris)

# Select the relevant columns
iris_data <- iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]

# Scale the data
scaled_iris_data <- scale(iris_data)

# Perform k-means clustering with k=3
iris_clusters <- kmeans(scaled_iris_data, centers = 3, nstart = 20)

# View the cluster assignments
as.data.frame(iris_clusters$cluster)



# Load the cluster package
library(cluster)

# Create a scatterplot matrix that shows the clusters
clusplot(scaled_iris_data, iris_clusters$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)


library(ggplot2)

# Add cluster assignments as a column in the iris dataset
iris_clustered <- iris
iris_clustered$cluster <- iris_clusters$cluster

# Create a customized visualization of the clusters
ggplot(iris_clustered, aes(x=Petal.Length, y=Petal.Width, color=factor(cluster))) +
  geom_point() +
  labs(title = "Iris Clustering Results", x = "Petal Length", y = "Petal Width", color = "Cluster")

install.packages("factoextra")
library(factoextra)

# Create a ggplot-based visualization of the clusters
fviz_cluster(iris_clusters, data = scaled_iris_data, geom = "point", palette = "jco", frame.type = "norm")



#Hiearchal clustering
# Standardize the data
iris_std <- scale(iris[,1:4])

# Calculate the distance matrix using Euclidean distance
dist_matrix <- dist(iris_std)

# Perform hierarchical clustering using complete linkage
hc <- hclust(dist_matrix, method = "complete")

# Plot the dendrogram
plot(hc, hang = -1, labels = iris$Species)



#
# Standardize the data
iris_std <- scale(iris[,1:4])

# Calculate the distance matrix using Euclidean distance
dist_matrix <- dist(iris_std)

# Perform hierarchical clustering using complete linkage
hc <- hclust(dist_matrix, method = "complete")

# Cut the dendrogram into 3 clusters
clusters <- cutree(hc, k = 3)

# Create a table that shows the species for each cluster
table(Species = iris$Species, Cluster = clusters)


#
library(dendextend)
# Standardize the data
iris_std <- scale(iris[,1:4])

# Calculate the distance matrix using Euclidean distance
dist_matrix <- dist(iris_std)

# Perform hierarchical clustering using complete linkage
hc <- hclust(dist_matrix, method = "complete")

# Create dendrogram object with labels
dend <- as.dendrogram(hc)
labels <- iris$Species
labels <- labels[order.dendrogram(dend)]
labels(dend) <- labels

# Get the total number of groups
num_groups <- length(unique(labels))

# Print the total number of groups
cat("Total number of groups:", num_groups)

# Cut the dendrogram into 3 clusters
clusters <- cutree(hc, k = 3)

# Create a table that shows the species for each cluster
table(Species = iris$Species, Cluster = clusters)

#---------------------------------------------------------------------------------------

#Assignment

#Social factor
library(cluster) 
set.seed(31538312) # XXXXXXXX = your student ID

unemployment <- read.csv("unemployment.csv")
View(unemployment)

inflation <- read.csv("Inflation.csv")
View(inflation)

vaccination <- read.csv("vaccinations.csv")
View(vaccination)

death_rate <- read.csv("death_rate.csv")
View(death_rate)

gov_effectiveness <- read.csv("gov_effectiveness.csv")
View(gov_effectiveness)

life_expectancy <- read.csv("life_expectancy.csv")


clustering <- function(data, column, country_column){
  
  set.seed(31538312)
  
  scaled_data <- scale(column)
  dist_matrix <- dist(scaled_data)
  
  hc <- hclust(dist_matrix, method = "ave")
  
  i_silhouette_score <- function(k){ 
    km <- kmeans(scaled_data, k, nstart = 50) 
    ss <- silhouette(km$cluster, dist_matrix)
    mean(ss[, 3]) 
  }
  
  k <- 10:20
  avg_sil <- sapply(k, i_silhouette_score)
  max_index <- which(avg_sil == max(avg_sil[order(avg_sil)]))
  max_clusters <- k[max_index]
  
  plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores') 
  
  clusters <- cutree(hc, k = max_clusters)
  result <- table(Country = country_column, Cluster = clusters)
  result_df <- as.data.frame.matrix(result)
  kable(result_df)
 
}

clustering(unemployment, unemployment[,6:7], unemployment$LOCATION)
clustering(inflation, inflation[,6:7], inflation$LOCATION)
clustering(death_rate, death_rate[,2:7], death_rate$Country)
clustering(gov_effectiveness, gov_effectiveness[,3:14], gov_effectiveness$Country.Territory)
clustering(life_expectancy, life_expectancy[,6:7], life_expectancy$LOCATION)












