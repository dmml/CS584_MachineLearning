##SVM hard margin##


#generate data
set.seed(500)
data1 <- cbind(rnorm(150, 20, 1), rnorm(150, 30, 1), matrix(-1, 150, 1))
data2 <- cbind(rnorm(150, 30, 1), rnorm(150, 40, 1), matrix(1, 150, 1))
sepdata <- rbind(data1, data2)
data3 <- cbind(rnorm(150, 20, 2), rnorm(150, 30, 2), matrix(-1, 150, 1))
data4 <- cbind(rnorm(150, 23, 2), rnorm(150, 33, 2), matrix(1, 150, 1))
unsepdata <- rbind(data3, data4)


#SVM of soft margins
traini <- rbind(data1,data2)
trainiX <- traini[,-3]
trainiY <- as.matrix(traini[,3])

trainiUnsep <- rbind(data3,data4)
trainiUnsepX <- trainiUnsep[,-3]
trainiUnsepY <- as.matrix(trainiUnsep[,3])

H = (trainiY %*% t(trainiY)) * (trainiX %*% t(trainiX))
c = matrix(rep(-1, nrow(trainiX)))
A = t(trainiY)
b = 0
r = 0
l = matrix(rep(0, nrow(trainiX)))
u = matrix(rep(0.005, nrow(trainiX)))

require(kernlab)
linearLDResult = ipop(c, H, A, b, l, u, r)
alpha = matrix(primal(linearLDResult))

w = rep(0, ncol(trainiX))
for(i in 1:nrow(trainiX))
{
  w = w + alpha[i] * trainiY[i] * trainiX[i, ]
}

sv = traini[alpha > 0.0001, ] #Support vectors
w0 = (1/nrow(sv)) * sum(as.matrix(sv[, 3]) - sv[, -3] %*% w)

#plotting
plot(15:45, 15:45, type = "n", main='seperate data', ylab='y', xlab='x')  
points(data1[,1], data1[,2], col = "blue")
points(data2[,1], data2[,2], col = "red")
points(matrix(sv[sv[,3]==1,],ncol = 3)[,1], matrix(sv[sv[,3]==1,],ncol = 3)[,2], pch = 19, col = "red")
points(matrix(sv[sv[,3]==-1,],ncol = 3)[,1], matrix(sv[sv[,3]==-1,],ncol = 3)[,2], pch = 19, col = "blue")
curve(-w[1]/w[2] * x - w0/w[2], add = TRUE, col = "green")  
curve(-w[1]/w[2] * x - (w0+1)/w[2], add = TRUE, col = "red")  
curve(-w[1]/w[2] * x - (w0-1)/w[2], add = TRUE, col = "blue")  

confusionMatrix <- matrix(0, 2, 2)
for (j in 1:nrow(traini))
{
  temp <- trainiX[j,] %*% as.matrix(w) + w0
  if (temp > 0)
  {
    if (j <= 150)
    {
      confusionMatrix[2,1] <- confusionMatrix[2,1] + 1
    }else
    {
      confusionMatrix[2,2] <- confusionMatrix[2,2] + 1
    }
  }else{
    if (j <= 150)
    {
      confusionMatrix[1,1] <- confusionMatrix[1,1] + 1
    }else
    {
      confusionMatrix[1,2] <- confusionMatrix[1,2] + 1
    }
  }
}

H = (trainiUnsepY %*% t(trainiUnsepY)) * (trainiUnsepX %*% t(trainiUnsepX))
c = matrix(rep(-1, nrow(trainiUnsepX)))
A = t(trainiUnsepY)
b = 0
r = 0
l = matrix(rep(0, nrow(trainiUnsepX)))
u = matrix(rep(0.005, nrow(trainiUnsepX)))

require(kernlab)
result = ipop(c, H, A, b, l, u, r)
alpha = matrix(primal(result))

w = rep(0, ncol(trainiUnsepX))
for(i in 1:nrow(trainiUnsepX))
{
  w = w + alpha[i] * trainiUnsepY[i] * trainiUnsepX[i, ]
}

sv = trainiUnsep[alpha > 0.0001, ] #Support vectors
w0 = (1/nrow(sv)) * sum(as.matrix(sv[, 3]) - sv[, -3] %*% w)

#plotting
plot(15:45, 15:45, type = "n", main='Unseparate data', ylab='y', xlab='x')  
points(data3[,1], data3[,2], col = "blue")
points(data4[,1], data4[,2], col = "red")
points(matrix(sv[sv[,3]==1,],ncol = 3)[,1], matrix(sv[sv[,3]==1,],ncol = 3)[,2], pch = 19, col = "red")
points(matrix(sv[sv[,3]==-1,],ncol = 3)[,1], matrix(sv[sv[,3]==-1,],ncol = 3)[,2], pch = 19, col = "blue")
curve(-w[1]/w[2] * x - w0/w[2], add = TRUE, col = "green")  
curve(-w[1]/w[2] * x - (w0+1)/w[2], add = TRUE, col = "red")  
curve(-w[1]/w[2] * x - (w0-1)/w[2], add = TRUE, col = "blue") 

confusionMatrixUnsep <- matrix(0, 2, 2)

for (j in 1:nrow(trainiUnsep))
{
  temp <- trainiUnsepX[j,] %*% as.matrix(w) + w0
  if (temp > 0)
  {
    if (j <= 150)
    {
      confusionMatrixUnsep[2,1] <- confusionMatrixUnsep[2,1] + 1
    }else
    {
      confusionMatrixUnsep[2,2] <- confusionMatrixUnsep[2,2] + 1
    }
  }else{
    if (j <= 150)
    {
      confusionMatrixUnsep[1,1] <- confusionMatrixUnsep[1,1] + 1
    }else
    {
      confusionMatrixUnsep[1,2] <- confusionMatrixUnsep[1,2] + 1
    }
  }
}

confusionMatrix
confusionMatrixUnsep
