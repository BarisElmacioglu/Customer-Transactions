

#SÜLEYMAN BARIS ELMACIOGLU - 1805546

#importing dataset
library(readr)
dataset <- read_csv("CustomerTransactions.csv")
View(dataset)

#dataset <- CustomerTransactions[3:34]
#View(dataset)


#check the dataset
attributes(dataset)
head(dataset)
summary(dataset)


#Elbow Method - Finding the optimal number of cluster

set.seed(6)
wcss <- vector()
for(i in 1:10) wcss[i] <- sum(kmeans(dataset[,3:34], i)$withinss)
plot(1:10, wcss, type = "b", main = paste("Cluster of Customers"), xlab = "Number of Clusters", ylab = "WCSS")


#################################################################
##  K-Means-Partional Clustering                               ##
#################################################################

# Kmeans Clustering model fitting
model <- kmeans(dataset[,3:34], 4, iter.max = 300, nstart = 10) # k = 4
summary(model)
#Model attributes 
attributes(model) 

# Clusters:
model$cluster

# Cluster size:
model$size

# Assign cluster number to the original data:
dataset<-cbind(dataset,Cluster=model$cluster)
View(dataset)
model$withinss
#

#Visualizing the Clusters
library(cluster)
clusplot(dataset[,3:34],
         model$cluster,
         lines=0,
         shade=TRUE,
         color = TRUE,
         labels = 2,
         plotchar=FALSE,
         span = TRUE,
         main = paste("Cluster of Customers"))
#warnings()
# Show customers and their corresponding clusters:
table(dataset[,2], model$cluster)



#################################################################
##  Hierarchical clustering                                    ##
#################################################################

#Find Hierarchical clustering using Euclidean distance and wards method in matrix.
d <- dist(dataset[,3:34], method = "euclidean") 
H_Model <- hclust(d, method="ward.D")

# display dendogram
plot(H_Model) 

# cut tree into 4 clusters
groups <- cutree(H_Model, k=4)

# show groups
table(dataset[,3:34],groups)

# draw dendogram with red borders around the 4 clusters
rect.hclust(H_Model, k=4, border="red") 


