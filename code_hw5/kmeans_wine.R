#############
## K-means ##
#############

#remove the class label
wine <- read.table("wine.txt",sep=",")
data <- wine[,2:14]

#K-means algorithm
##random initialize the centroid of clusters
rand <- sample(1:178,3)
m <- length(data[,1])
class <- vector()
for (i in 1:m){
  centroid1 <- sum((data[i,]-data[rand[1],])^2)
  centroid2 <- sum((data[i,]-data[rand[2],])^2)
  centroid3 <- sum((data[i,]-data[rand[3],])^2)
  minDist <- min(centroid1,centroid2,centroid3)
  if(minDist==centroid1)
    class <- c(class,1)
  else if(minDist==centroid2)
    class <- c(class,2)
  else
    class <- c(class,3)
}
change = TRUE
while(change){
  data1 <- cbind(data,class)
  means1 <- colMeans(data1[which(data1$class=='1'),1:13])
  means2 <- colMeans(data1[which(data1$class=='2'),1:13])
  means3 <- colMeans(data1[which(data1$class=='3'),1:13])
  class <- vector()
  for (i in 1:m){
    centroid1 <- sum((data[i,]-means1)^2)
    centroid2 <- sum((data[i,]-means2)^2)
    centroid3 <- sum((data[i,]-means3)^2)
    minDist <- min(centroid1,centroid2,centroid3)
    if(minDist==centroid1)
      class <- c(class,1)
    else if(minDist==centroid2)
      class <- c(class,2)
    else
      class <- c(class,3)
  }
  data2 <- cbind(data,class)
  updateMeans1 <- colMeans(data2[which(data2$class=='1'),1:13])
  updateMeans2 <- colMeans(data2[which(data2$class=='2'),1:13])
  updateMeans3 <- colMeans(data2[which(data2$class=='3'),1:13])
  if ((updateMeans1-means1==0)&&(updateMeans2-means2==0)&&(updateMeans3-means3==0))
    change = FALSE
}

#evaluated the performance by Euclidean Distance
testdata1 <- data2[which(data2$class=='1'),1:13]
m1 <- length(testdata1)
testmean1 <- c(rep(updateMeans1,m1))
EuclideanDistance1 <- sqrt(sum((testdata1-testmean1)^2))
testdata2 <- data2[which(data2$class=='2'),1:13]
m2 <- length(testdata2)
testmean2 <- c(rep(updateMeans1,m2))
EuclideanDistance2 <- sqrt(sum((testdata2-testmean2)^2))
testdata3 <- data2[which(data2$class=='3'),1:13]
m3 <- length(testdata3)
testmean3 <- c(rep(updateMeans3,m3))
EuclideanDistance3 <- sqrt(sum((testdata3-testmean3)^2))
EuclideanDistanceResult <- c(EuclideanDistance1,EuclideanDistance2,EuclideanDistance3)

##plot the data
plot(10:15, 0.5:5.5, type = "n", main='Wine data', ylab='Malic acid', xlab='Alcohol')  
points(data2[which(data2$class=='1'),1], data2[which(data2$class=='1'),2], col = "blue")
points(data2[which(data2$class=='2'),1], data2[which(data2$class=='2'),2], col = "red")
points(data2[which(data2$class=='3'),1], data2[which(data2$class=='3'),2], col = "green")
points(updateMeans1[1], updateMeans1[2], col = "black", pch = 8)
points(updateMeans2[1], updateMeans2[2], col = "black", pch = 8)
points(updateMeans3[1], updateMeans3[2], col = "black", pch = 8)

#measure the error by original class label
confusionMatrix <- matrix(0,3,3)
confusionMatrix <- confusionMatrix + table(class,wine[,1])
