### Case 10 - Solution ###

#1. Load the data
flower = read.csv("case10.csv", header=FALSE)

#2. Review the structure of the data
str(flower)

#3. Change the data type to matrix
flowerMatrix = as.matrix(flower)

#4. Review the structure of the data
str(flowerMatrix)

#5. Turn matrix into a vector
flowerVector = as.vector(flowerMatrix)

#6. Review the structure of the data
str(flowerVector)

#7. Compute distances
distance = dist(flowerVector, method = "euclidean")

#8. Hierarchical clustering
clusterIntensity = hclust(distance, method="ward")

#9. Plot the dendrogram
plot(clusterIntensity)

#10. Select 3 clusters
rect.hclust(clusterIntensity, k = 3, border = "red")

#11. Separate the data into the 3 clusters selected
flowerClusters = cutree(clusterIntensity, k = 3)
flowerClusters

#12. Find mean intensity values
tapply(flowerVector, flowerClusters, mean)

#13. Plot the image and the clusters
dim(flowerClusters) = c(50,50)
image(flowerClusters, axes = FALSE)

#14. Original image
image(flowerMatrix,axes=FALSE)
image(flowerMatrix,axes=FALSE,col=grey(seq(0,1,length=256)))



#15. Let's use KMC. Specify number of clusters
k = 3

#16. Run k-means
set.seed(1)
KMC = kmeans(flowerVector, centers = k, iter.max = 1000)
str(KMC)

#17. Extract clusters
KMCClusters = KMC$cluster
KMC$centers[1]
KMC$centers[2]
KMC$centers[3]

#18. Plot the image with the clusters
dim(KMCClusters) = c(nrow(flowerMatrix), ncol(flowerMatrix))

image(KMCClusters, axes = FALSE, col=rainbow(k))
image(KMCClusters, axes = FALSE)

