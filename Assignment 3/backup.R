#setting up the project
rm(list = ls()) 
library(slam) 
library(tm) 
library(SnowballC)  
library(cluster) 
library(proxy)

#Text Analysis*****************************************************************************

#Get file path to folder "text" where the collection of documents are located 
textfolder = file.path(".", "text") #folder and directory
textfolder
print(dir(textfolder)) 


docs = Corpus(DirSource((textfolder))) 
print(summary(docs)) 
#View(docs) #check the doc and its code


#Tokenization

toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x)) # Hyphen to space, ref Williams 
docs <- tm_map(docs, toSpace, "-") #find dash and convert to space, also works other stuff, but it will be easier to remove manually
docs <- tm_map(docs, toSpace, "“") #find quotation mark and remove it
docs <- tm_map(docs, removeNumbers) 
docs <- tm_map(docs, removePunctuation) 
docs <- tm_map(docs, content_transformer(tolower)) 
docs <- tm_map(docs, toSpace, "’s") # Remove possessive form "'s"
docs <- tm_map(docs, toSpace, "also") # Remove possessive form "'s"
docs <- tm_map(docs, toSpace, "just") # Remove possessive form "'s"


#Filter words # Remove stop words and white space 
docs <- tm_map(docs, removeWords, stopwords("english")) 
docs <- tm_map(docs, stripWhitespace)  


# Stem 
docs <- tm_map(docs, stemDocument, language = "english") 

#Create document term matrix 
dtm <- DocumentTermMatrix(docs) 
dtm

View(dtm)


#Remove sparse terms (unique term that cant identify similarity)
dim(dtm) 
inspect(dtm) #check non sparse, maximal
dtms <- removeSparseTerms(dtm, 0.55) #22 terms total
dim(dtms)

dtms = as.matrix(dtms) 
dtms #display the matrix

dim(dtms)

View(dtms)

write.csv(dtms, "dtms_A3.csv") 

read.csv("dtms_A3.csv")


#Clustering
distmatrix = dist(scale(dtms)) 

dim(dtms)

# Hierarchical clustering
hc <- hclust(distmatrix, method = "ave")

# Function to calculate silhouette score for a given number of clusters (k)
i_silhouette_score <- function(k) {
  km <- kmeans(dtms, k, nstart = 22)
  ss <- silhouette(km$cluster, distmatrix)
  mean(ss)
}


# Evaluate silhouette scores for different numbers of clusters
set.seed(31538312)
k <- 1:15
avg_sil <- sapply(k, i_silhouette_score)
table_data <- data.frame(Number_of_Clusters = k, Silhouette_Score = avg_sil)
table_data

# Plot silhouette scores vs number of clusters
plot(k, avg_sil, type = "b", xlab = "Number of clusters", ylab = "Average Silhouette Scores",
     main = "Silhouette Score vs Number of Clusters")

#Plot clustering

fit = hclust(distmatrix, method = "ward.D") #ward.D is eucledian distance , try Cosine distance in the future
plot(fit, main = "Documents Dendogram", xlab = "", sub = "",hang = -1) 

plot(fit, main = "Documents Dendogram 8 clusters", xlab = "", sub = "",hang = -1) 
hfit= cutree(fit, k = 10) #cutting dendogram 
rect.hclust(fit, k = 10, border = "red") #cutting dendogram


#Network*******************************************************************************
library(igraph)
library(igraphdata)

#Network Summary function
network_summary <- function(g){
  cat("\n#Vertices Count\n") 
  print(vcount(g))
  cat("\n#Edge Count\n") 
  print(ecount(g))
  cat("\n#Diameter\n") 
  print(diameter(g))
  cat("\n#Average Path Length\n") 
  print(average.path.length(g))
  cat("\n#Clique Size\n") 
  print(table(sapply(cliques(g),length)))
  cat("\n#Density\n") 
  print(graph.density(g))
  cat("\n#Clustering coefficient\n") 
  print(transitivity(g))
  
}


#Top 5 Vertex Measure function
top5_measure <- function(dataframe){
  top5_values <- function(column) {
    sorted_column <- column[order(column, decreasing = TRUE)]
    top5 <- head(sorted_column, 5)
    top5_indices <- match(top5, column)
    return(data.frame(Value = top5, Row_Index = top5_indices))
  }
  
  # Apply the top5_values function to each column of the news_table
  top5_table <- lapply(dataframe, top5_values)
  
  # Combine the top 5 values for each column into a single data frame
  top5_table <- do.call(rbind, top5_table)
  
  # Print the top 5 table
  print(top5_table)
  
}

#setting up document and token matrix
# start with original document-term matrix 
dtmsx = as.matrix(dtms) 
# convert to binary matrix 
dtmsx = as.matrix((dtmsx > 0) + 0) 
dtmsx

#Network of documents-----------------------------------------------------------------

#multiply binary matrix by its transpose 
ByNewsMatrix = dtmsx %*% t(dtmsx) 
# make leading diagonal zero 
diag(ByNewsMatrix) = 0
ByNewsMatrix

ByNewsMatrix_df <- as.data.frame(ByNewsMatrix)
# Save the data frame as a CSV file
write.csv(ByNewsMatrix_df, "ByNewsMatrix.csv")
View(read.csv("ByNewsMatrix.csv"))


# Assign colors to nodes based on document types
node_colors <- c("lightblue", "lightblue", "lightblue", "lightblue", 
                            "violet", "violet","violet","violet", 
                            "green", "green", "green", "green", 
                            "orange", "orange", "orange", "orange")
                            

ByNews = graph_from_adjacency_matrix(ByNewsMatrix, mode = "undirected", weighted = TRUE) 
ByNews = simplify(ByNews)

bet <- closeness(ByNews) 
V(ByNews)$size <- bet*1000

dev.off()
plot(ByNews, asp = 0, vertex.color = node_colors, layout = layout.fruchterman.reingold, edge.width = E(ByNews)$weight/4,  
     edge.arrow.mode = 1, edge.arrow.size = 0.3, main = "Document Networks") 
legend(x = -1.1, y = -0.8, legend = c("Esport", "Basketball", "Movie", "Food"), 
       pch = 19, col = c("violet", "lightblue", "orange", "green"), bty = "n", cex = 1, pt.cex = 2)


#fast greedy clustering
cfb= cluster_fast_greedy(ByNews)
plot(cfb, asp = 0, ByNews,  vertex.color = node_colors, layout = layout.fruchterman.reingold, edge.width = E(ByNews)$weight/6,  
     edge.arrow.mode = 1, edge.arrow.size = 0.3, edge.color = "brown", main = "Fast greedy Document Network") 

legend(x = -1.1, y = -0.8, legend = c("Cluster1", "Cluster2"), 
       pch = 19, col = c("violet", "orange"), bty = "n", cex = 1, pt.cex = 2)

network_summary(ByNews)


#Vertex measure
degree_table <- format(degree(ByNews), digits = 2)
betweenness_table <- format(betweenness(ByNews), digit = 2)
closeness_table <- format(closeness(ByNews), digit = 2)
e <- evcent(ByNews)
eigenvector_table <- format(e$vector, digit = 2)

news_table <- cbind(degree_table, betweenness_table, closeness_table, eigenvector_table)
colnames(news_table) <- c("Degree", "Betweenness", "Closeness", "Eigenvector")
news_table

news_tabledf = as.data.frame(news_table)
View(news_tabledf)

top5_measure(news_tabledf)


#Network of words(tokens)--------------------------------------------------------------------
# multiply transpose binary matrix by binary matrix 
ByTokenMatrix = t(dtmsx) %*% dtmsx
# make leading diagonal zero
diag(ByTokenMatrix) = 0

ByTokenMatrix

#create graph object, improve this graph
ByToken = graph_from_adjacency_matrix(ByTokenMatrix, mode = "undirected", weighted = TRUE)
ByToken = simplify(ByToken)


bet2 <- closeness(ByToken) 
V(ByToken)$size <- bet2*1500


plot(ByToken, asp = 0, layout = layout.fruchterman.reingold, edge.width = E(ByNews)$weight/6, 
     edge.arrow.mode = 1, edge.arrow.size = 0.3, main = "Token Networks")

cfb2 = cluster_fast_greedy(ByToken)
plot(cfb2, asp = 0, ByToken,  vertex.color = node_colors, layout = layout.fruchterman.reingold, edge.width = E(ByNews)$weight/6,  
     edge.arrow.mode = 1, edge.arrow.size = 0.3, edge.color = "brown", main = "Fast greedy Token Network") 

legend(x = -1.1, y = -0.8, legend = c("Cluster1", "Cluster2"), 
       pch = 19, col = c("violet", "orange"), bty = "n", cex = 1, pt.cex = 2)

#network summary
network_summary(ByToken)

#Vertex Measure
degree_table2 <- format(degree(ByToken), digits = 2)
betweenness_table2 <- format(betweenness(ByToken), digit = 2)
closeness_table2 <- format(closeness(ByToken), digit = 2)
e2 <- evcent(ByToken)
eigenvector_table2 <- format(e2$vector, digit = 2)

token_table <- cbind(degree_table2, betweenness_table2, closeness_table2, eigenvector_table2)
colnames(token_table) <- c("Degree", "Betweenness", "Closeness", "Eigenvector")
token_table

token_tabledf <- as.data.frame(token_table)
top5_measure(token_tabledf)


#Bipartite (two mode) Network------------------------------------------------------------------------------

# start with document term matrix dtms 
dtmsa = as.data.frame(dtms) # clone dtms

dtmsa$NEWS = rownames(dtmsa) # add row names 
dtmsb = data.frame() 

for (i in 1:nrow(dtmsa)){ 
  for (j in 1:(ncol(dtmsa)-1)){ 
    touse = cbind(dtmsa[i,j], dtmsa[i,ncol(dtmsa)], colnames(dtmsa[j])) 
    dtmsb = rbind(dtmsb, touse ) 
  } 
} # close loops 

dtmsb <- dtmsb[, c("V2", "V3", "V1")]
dtmsb

colnames(dtmsb) = c("news", "token", "weight") 

dtmsb

dtmsc = dtmsb[dtmsb$weight != 0,] # delete 0 weights
dtmsc

# create graph object and declare bipartite improve the look of this code

g <- graph.data.frame(dtmsc, directed=FALSE) 
bipartite.mapping(g) 

E(g)$weight


V(g)$type <- bipartite_mapping(g)$type 
V(g)$color <- ifelse(V(g)$type, "lightblue", "salmon") 
V(g)$shape <- ifelse(V(g)$type, "circle", "square") 
E(g)$color <- "lightgray" 
  

plot(g, asp = 0, layout = layout.circle, edge.width = E(g)$weight, 
     edge.arrow.mode = 1, edge.arrow.size = 0.3, vertex.size = 8, main = "Two Node Networks")


ceb= cluster_edge_betweenness(g)
layout <-  layout.fruchterman.reingold
plot(ceb, asp = 0, g, layout = layout, edge.width = E(g)$weight, 
     edge.arrow.mode = 1, edge.arrow.size = 0.3, vertex.size =5 ,main = "Two Node Networks")


degree_table3 <- format(degree(g), digits = 2)
betweenness_table3 <- format(betweenness(g), digit = 2)
closeness_table3 <- format(closeness(g), digit = 2)
e3 <- evcent(g)
eigenvector_table3 <- format(e3$vector, digit = 2)

g <- cbind(degree_table3, betweenness_table3, closeness_table3, eigenvector_table3)
colnames(g) <- c("Degree", "Betweenness", "Closeness", "Eigenvector")
g



