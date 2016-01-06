#############
## K-means ##
#############

#remove the class label
iris <- read.table("iris.txt",sep=",")
data <- iris[,1:4]

#K-means algorithm
##random initialize the centroid of clusters
rand <- sample(1:150,3)
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
  means1 <- colMeans(data1[which(data1$class=='1'),1:4])
  means2 <- colMeans(data1[which(data1$class=='2'),1:4])
  means3 <- colMeans(data1[which(data1$class=='3'),1:4])
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
  updateMeans1 <- colMeans(data2[which(data2$class=='1'),1:4])
  updateMeans2 <- colMeans(data2[which(data2$class=='2'),1:4])
  updateMeans3 <- colMeans(data2[which(data2$class=='3'),1:4])
  if ((updateMeans1-means1==0)&&(updateMeans2-means2==0)&&(updateMeans3-means3==0))
    change = FALSE
}

#evaluated the performance by Euclidean Distance
testdata1 <- data2[which(data2$class=='1'),1:4]
m1 <- length(testdata1)
testmean1 <- c(rep(updateMeans1,m1))
EuclideanDistance1 <- sqrt(sum((testdata1-testmean1)^2))
testdata2 <- data2[which(data2$class=='2'),1:4]
m2 <- length(testdata2)
testmean2 <- c(rep(updateMeans1,m2))
EuclideanDistance2 <- sqrt(sum((testdata2-testmean2)^2))
testdata3 <- data2[which(data2$class=='3'),1:4]
m3 <- length(testdata3)
testmean3 <- c(rep(updateMeans3,m3))
EuclideanDistance3 <- sqrt(sum((testdata3-testmean3)^2))
EuclideanDistanceResult <- c(EuclideanDistance1,EuclideanDistance2,EuclideanDistance3)

##plot the data
plot(1:5, 4:8, type = "n", main='Iris data', ylab='spetal width', xlab='spetal length')  
points(data2[which(data2$class=='1'),2], data2[which(data2$class=='1'),1], col = "blue")
points(data2[which(data2$class=='2'),2], data2[which(data2$class=='2'),1], col = "red")
points(data2[which(data2$class=='3'),2], data2[which(data2$class=='3'),1], col = "green")
points(updateMeans1[2], updateMeans1[1], col = "black", pch = 8)
points(updateMeans2[2], updateMeans2[1], col = "black", pch = 8)
points(updateMeans3[2], updateMeans3[1], col = "black", pch = 8)

#measure the error by original class label
confusionMatrix <- matrix(0,3,3)
confusionMatrix <- confusionMatrix + table(class,iris[,5])

