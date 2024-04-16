#libarilies
rm(list = ls()) 
library(slam) 
library(tm) 
library(SnowballC)  
library(cluster) 


#Text Analysis*****************************************************************************

#Get file path to folder "text" where the collection of documents are located 
textfolder = file.path(".", "Mongkhoune_Xaynalong_31538312_A3_text") #folder and directory
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

#Remove sparse terms (unique term that cant identify similarity)
dim(dtm) 
inspect(dtm) #check non sparse, maximal
dtms <- removeSparseTerms(dtm, 0.55) #22 terms total

dtms = as.matrix(dtms) 
View(dtms) #display the matrix
dim(dtms)

#Clustering------------------------------------------------------------------------
distmatrix = dist(scale(dtms)) 
View(dtms)

# Hierarchical clustering, using Euclidean distance

#Plot clustering

fit = hclust(distmatrix, method = "ward.D") #ward.D is eucledian distance

topics = c("basketball", "basketball", "basketball", "basketball", "esport", "esport", "esport", "esport",
           "food", "food", "food", "food", "movie", "movie", "movie", "movie")

groups = cutree(fit, k = 4)
table(GroupNames = topics, Clusters = groups)

#Accuracy
cluster_accuracy = (1 + 2 + 4 + 3)/16
print(cluster_accuracy)

plot(fit, main = "Documents Dendogram", xlab = "", sub = "",hang = -1) 

plot(fit, main = "4 Clusters of Documents Dendogram ", xlab = "", sub = "",hang = -1) 
hfit= cutree(fit, k = 4) #cutting dendogram 
rect.hclust(fit, k = 4, border = "blue") #cutting dendogram

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
  cat("\n#Density\n") 
  print(graph.density(g))
  cat("\n#Clustering coefficient\n") 
  print(transitivity(g))
  
}

#Verex Measure function
vertex_summary <- function(table, title){
  
  degree_table <- format(degree(table), digits = 2)
  betweenness_table <- format(betweenness(table), digit = 2)
  closeness_table <- format(closeness(table), digit = 2)
  e <- evcent(table)
  eigenvector_table <- format(e$vector, digit = 2)
  
  vertex_table <- cbind(degree_table, betweenness_table, closeness_table, eigenvector_table)
  colnames(vertex_table) <- c("Degree", "Betweenness", "Closeness", "Eigenvector")
  print(title)
  print(vertex_table)
  View(vertex_table)
  
  
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
View(ByNewsMatrix)

# Assign colors to nodes based on document types
node_colors <- c("lightblue", "lightblue", "lightblue", "lightblue", 
                  "violet", "violet","violet","violet", 
                  "green", "green", "green", "green", 
                  "orange", "orange", "orange", "orange")


ByNews = graph_from_adjacency_matrix(ByNewsMatrix, mode = "undirected", weighted = TRUE) 
ByNews = simplify(ByNews)

bet <- closeness(ByNews) 
V(ByNews)$size <- bet*1000 #size of node

#ploting document network
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

network_summary(ByNews) #summary
vertex_summary(ByNews, "#Document Vertex Measure") #document vertex measure


#Network of words(tokens)--------------------------------------------------------------------
# multiply transpose binary matrix by binary matrix 
ByTokenMatrix = t(dtmsx) %*% dtmsx
# make leading diagonal zero
diag(ByTokenMatrix) = 0
View(ByTokenMatrix)

#create graph object, improve this graph
ByToken = graph_from_adjacency_matrix(ByTokenMatrix, mode = "undirected", weighted = TRUE)
ByToken = simplify(ByToken)


bet2 <- closeness(ByToken) 
V(ByToken)$size <- bet2*1500 #size of node


plot(ByToken, asp = 0, layout = layout.fruchterman.reingold, edge.width = E(ByNews)$weight/6, 
     edge.arrow.mode = 1, edge.arrow.size = 0.3, main = "Token Networks")

cfb2 = cluster_fast_greedy(ByToken)
plot(cfb2, asp = 0, ByToken,  vertex.color = node_colors, layout = layout.fruchterman.reingold, edge.width = E(ByNews)$weight/6,  
     edge.arrow.mode = 1, edge.arrow.size = 0.3, edge.color = "brown", main = "Fast greedy Token Network") 

legend(x = -1.1, y = -0.8, legend = c("Cluster1", "Cluster2"), 
       pch = 19, col = c("violet", "orange"), bty = "n", cex = 1, pt.cex = 2)

network_summary(ByToken) 
vertex_summary(ByToken, "#Token Vertex Measure") #token vertex measure summary


#Bipartite (two mode) Network------------------------------------------------------------------------------

# Document term matrix for two mode network
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
View(dtmsc)
write.csv(dtmsc, "dtmc_A3.csv") 

# Setting up plotting the two mode network
two_mode <- graph.data.frame(dtmsc, directed=FALSE) 
bipartite.mapping(two_mode) 

deg <- degree(two_mode)  
V(two_mode)$size <- deg/2 #size of node

V(two_mode)$type <- bipartite_mapping(two_mode)$type 
V(two_mode)$color <- ifelse(V(two_mode)$type, "lightblue", "salmon") 
V(two_mode)$shape <- ifelse(V(two_mode)$type, "circle", "square") 
E(two_mode)$color <- "lightgray" 
  

#plot two mode network  
plot(two_mode, asp = 0, layout = layout.circle, edge.width = E(two_mode)$weight, 
     edge.arrow.mode = 1, edge.arrow.size = 0.3, main = "Two Node Networks")

# Plot the two mode network fast greedy clustering
cfb3 = cluster_fast_greedy(two_mode)
plot(cfb3, asp = 0, two_mode, layout = layout.circle, edge.width = E(two_mode)$weight, 
     edge.arrow.mode = 1, edge.arrow.size = 0.3,
     main = "Fast Greedy Two Node Networks")

# Count the total number of communities
num_communities <- length(unique(cfb3$membership))
num_communities

network_summary(two_mode) # two mode network summary
vertex_summary(two_mode, "#Two mode vertex") #two mode network summary



