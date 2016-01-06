##SVM Gaussian kenerl##


#generate data
set.seed(500)
data1 <- cbind(rnorm(150, 20, 1), rnorm(150, 30, 1), matrix(-1, 150, 1))
data2 <- cbind(rnorm(150, 30, 1), rnorm(150, 40, 1), matrix(1, 150, 1))
sepdata <- rbind(data1, data2)
data3 <- cbind(rnorm(150, 20, 2), rnorm(150, 30, 2), matrix(-1, 150, 1))
data4 <- cbind(rnorm(150, 23, 2), rnorm(150, 33, 2), matrix(1, 150, 1))
unsepdata <- rbind(data3, data4)

#SVM with Gaussian Kernel
traini <- rbind(data1,data2)
trainiX <- traini[,-3]
trainiY <- as.matrix(traini[,3])
trainiUnsep <- rbind(data3,data4)
trainiUnsepX <- trainiUnsep[,-3]
trainiUnsepY <- as.matrix(trainiUnsep[,3])


require(KRLS)
H = (trainiY %*% t(trainiY)) * (gausskernel(X=trainiX, sigma=2))
c = matrix(rep(-1, nrow(trainiX)))
A = t(trainiY)
b = 0
r = 0
l = matrix(rep(0, nrow(trainiX)))
u = matrix(rep(1000000, nrow(trainiX)))

require(kernlab)
result1 = ipop(c, H, A, b, l, u, r)
alpha = matrix(primal(result1))

w = rep(0, ncol(trainiX))
for(i in 1:nrow(trainiX))
{
  w = w + alpha[i] * trainiY[i] * trainiX[i, ]
}

sv = traini[alpha > 0.1, ] #Support vectors
w0 = 0 
for(m in 1:nrow(sv))
{
  wtx = 0
  for(n in 1:nrow(trainiX))
  {
    wtx = wtx + + alpha[n] * trainiY[n] * gausskernel(X = rbind(trainiX[n, ], sv[m, -3]), sigma=2)[1,2]
  }
  w0 = w0 + sv[m, 3] - wtx
}
w0 = (1/nrow(sv)) * w0

#plot
plot(15:45, 15:45, type = "n", main='Seperate data', ylab='y', xlab='x')  
points(data1[,1], data1[,2], col = "blue")
points(data2[,1], data2[,2], col = "red")
points(matrix(sv[sv[,3]==1,],ncol = 3)[,1], matrix(sv[sv[,3]==1,],ncol = 3)[,2], pch = 19, col = "red")
points(matrix(sv[sv[,3]==-1,],ncol = 3)[,1], matrix(sv[sv[,3]==-1,],ncol = 3)[,2], pch = 19, col = "black")

confusionMatrix <- matrix(0, 2, 2)
for (j in 1:nrow(traini))
{
  temp <- 0
  for (t in 1:nrow(trainiX))
  {
    temp = temp + + alpha[t]*trainiY[t] * gausskernel(X = rbind(trainiX[t, ], trainiX[j, ]), sigma=2)[1,2]
  }
  temp <- temp + w0
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

H = (trainiUnsepY %*% t(trainiUnsepY)) * (gausskernel(X=trainiUnsepX, sigma=2))
c = matrix(rep(-1, nrow(trainiUnsepX)))
A = t(trainiUnsepY)
b = 0
r = 0
l = matrix(rep(0, nrow(trainiUnsepX)))
u = matrix(rep(100000, nrow(trainiUnsepX)))

require(kernlab)
result = ipop(c, H, A, b, l, u, r)
alpha = matrix(primal(result))

w = rep(0, ncol(trainiUnsepX))
for(i in 1:nrow(trainiUnsepX))
{
  w = w + alpha[i] * trainiUnsepY[i] * trainiUnsepX[i, ]
}

sv = trainiUnsep[alpha > 10000, ] #Support vectors
w0 = 0 
for(m in 1:nrow(sv))
{
  wtx = 0
  for(n in 1:nrow(trainiUnsepX))
  {
    wtx = wtx + + alpha[n] * trainiUnsepY[n] * gausskernel(X = rbind(trainiUnsepX[n, ], sv[m, -3]), sigma=6)[1,2]
  }
  w0 = w0 + sv[m, 3] - wtx
}
w0 = (1/nrow(sv)) * w0

#Show the plots
plot(15:45, 15:45, type = "n", main='Unseperate data', ylab='y', xlab='x')  
points(data3[,1], data3[,2], col = "blue")
points(data4[,1], data4[,2], col = "red")
points(matrix(sv[sv[,3]==1,],ncol = 3)[,1], matrix(sv[sv[,3]==1,],ncol = 3)[,2], pch = 19, col = "red")
points(matrix(sv[sv[,3]==-1,],ncol = 3)[,1], matrix(sv[sv[,3]==-1,],ncol = 3)[,2], pch = 19, col = "blue")

confusionMatrixUnsep <- matrix(0, 2, 2)
for (j in 1:nrow(trainiUnsep))
{
  temp <- 0
  for (t in 1:nrow(trainiUnsepX))
  {
    temp = temp + + alpha[t]*trainiUnsepY[t] * gausskernel(X = rbind(trainiUnsepX[t, ], trainiUnsepX[j, ]), sigma=6)[1,2]
  }
  temp <- temp + w0
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
