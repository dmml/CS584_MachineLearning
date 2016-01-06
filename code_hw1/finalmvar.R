data <- read.table("data.txt", sep = ",")
xc1 <- (data$V3+data$V4)/2
xc2 <- (data$V5+data$V6)/2 
xr1 <-  (data$V4-data$V3)/2
xr2 <-  (data$V6-data$V5)/2
yc <- (data$V1+data$V2)/2
yr <- (data$V2-data$V1)/2
n <- nrow(data)
a1 <- xc1+xr1
a2 <- xc2+xr2
y <- yc+yr
A <- cbind(c(2*n,sum(a1),sum(a2)),c(sum(a1),sum(a1^2),sum(a1*a2)),c(sum(a2),sum(a1*a2),sum(a2^2)))
b <- c(sum(y),sum(y*a1),sum(y*a2))
beta <- solve(A)%*%b




#cut the data sets into 10 folds
folds <- cut(seq(1, nrow(data)), breaks=10, labels=FALSE)


#cross-validation
for (i in 1:10){
  indexes <- which(folds==i, arr.ind=TRUE)
  ztestMtrx <- dataMatrix[indexes, ]
  ztrainMtrx <- dataMatrix[-indexes, ]
  ytestMtrx <- matrix(data[indexes,][,3], 250, 1)
  ytrainMtrx <-matrix(data[-indexes,][,3], 2250, 1)
  theta <- solve(t(ztrainMtrx) %*% ztrainMtrx) %*% t(ztrainMtrx) %*% ytrainMtrx
  m <- length(ztrainMtrx[,1])
  yhatTrain <- t(theta) %*% t(ztrainMtrx)
  trainingMSE <- sum((t(yhatTrain)-ytrainMtrx) ^ 2)/m
  trainingRSE <- sum((t(yhatTrain)-ytrainMtrx)^2/ytrainMtrx^2)/m
  
  p <- length(ztestMtrx[,1])
  yTestingHat <- t(theta) %*% t(ztestMtrx)
  testingMSE <- sum((t(yTestingHat)-ytestMtrx) ^ 2)/p
  testingRSE <- sum((t(yTestingHat)-ytestMtrx)^2/ytestMtrx^2)/p
  
  if (i == 1){
    v1 <- c(trainingMSE, testingMSE)
  }else{
    v1 <- c(v1, c(trainingMSE, testingMSE))
  }
  
  indexes <- which(folds==i, arr.ind=TRUE)
  ztestMtrx <- dataMatrix[indexes, ]
  ztrainMtrx <- dataMatrix[-indexes, ]
  ytestMtrx <- matrix(data[indexes,][,3], 250, 1)
  ytrainMtrx <-matrix(data[-indexes,][,3], 2250, 1)
  finalTheta <- gradientDescent(ztrainMtrx, ytrainMtrx, 1000)
  
  #compute training and testing error
  m <- length(ztrainMtrx[,1]) 
  yhatTrain <- t(finalTheta) %*% t(ztrainMtrx)
  trainingMSE <- sum((t(yhatTrain)-ytrainMtrx) ^ 2)/m
  
  
  p <- length(ztestMtrx[,1])
  yTestingHat <- t(finalTheta) %*% t(ztestMtrx)
  testingMSE <- sum((t(yTestingHat)-ytestMtrx) ^ 2)/p
  
  
  if (i == 1){
    v2 <- c(trainingMSE,  testingMSE)
  }else{
    v2 <- c(v2, c(trainingMSE, testingMSE))
  }
}
#output the MSE result
result <- matrix(v, 10, 2, byrow = T)
colnames(result) <- c("trainingMSE", "testingMSE")
gdresult <- matrix(v2, 10, 2, byrow = T)
colnames(gdresult) <- c("trainingMSE",  "testingMSE")
