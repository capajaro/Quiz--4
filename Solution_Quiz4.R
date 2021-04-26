### Quiz 4 - Solution ###

#1. Load the data
Class = read.csv("Coords.csv", header=TRUE)

#7. Compute distances
distance = dist(Class, method = "manhattan")

#8. Hierarchical clustering
clusterIntensity = hclust(distance, method="ward")

#9. Plot the dendrogram
plot(clusterIntensity)

#10. Select 5 clusters
rect.hclust(clusterIntensity, k = 5, border = "red")

#15. Let's use KMC. Specify number of clusters
k = 5

#16. Run k-means
set.seed(1)
KMC = kmeans(Class[,2:3], centers = k, iter.max = 1000)
str(KMC)

#17. Extract clusters
Class$Cluster = KMC$cluster
Centers = as.data.frame(KMC$centers)
Class$Lat_Center = 0L
Class$Long_Center = 0L
Class$DistCenter = 0L
Tag = 1:nrow(Class)

Class = cbind(Tag,Class[,1:(ncol(Class))])
# Assign Centers
for (irow in 1:nrow(Class)){
  Clust = Class$Cluster[irow]
  Class$Lat_Center[irow] = Centers$Lat[Clust]
  Class$Long_Center[irow] = Centers$Lon[Clust]
  
  Class$DistCenter[irow]=round((((Class$Lat_Center[irow]-Class$Lat[irow])*6371)^2+((Class$Long_Center[irow]-Class$Long[irow])*6371)^2)^0.5,2)
  
}

write.csv(Class, file = "Case_2_Quiz.csv", row.names = FALSE)
