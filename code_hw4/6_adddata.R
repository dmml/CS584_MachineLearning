##Added data##

#Generate data
set.seed(100)
data1 <- cbind(rnorm(150, 20, 2), rnorm(150, 30, 2), matrix(-1, 150, 1))
data2 <- cbind(rnorm(50, 23, 1), rnorm(50, 33, 1), matrix(1, 50, 1))

#SVM with hard margins
train <- rbind(data1,data2)
trainX <- train[,-3]
trainY <- as.matrix(train[,3])

H = (trainY %*% t(trainY)) * (trainX %*% t(trainX))
c = matrix(rep(-1, nrow(trainX)))
A = t(trainY)
b = 0
r = 0
l = matrix(rep(0, nrow(trainX)))
u = matrix(rep(100, nrow(trainX)))

require(kernlab)
result = ipop(c, H, A, b, l, u, r)
alpha = matrix(primal(result))

w = rep(0, ncol(trainX))
for(i in 1:nrow(trainX))
{
  w = w + alpha[i] * trainY[i] * trainX[i, ]
}

sv = train[alpha > 0.01, ] #Support vectors
w0 = (1/nrow(sv)) * sum(as.matrix(sv[, 3]) - sv[, -3] %*% w)

#plot
plot(15:45, 15:45, type = "n", main='Orginal data', ylab='y', xlab='x')  
points(data1[,1], data1[,2], col = "blue")
points(data2[,1], data2[,2], col = "red")
points(matrix(sv[sv[,3]==1,],ncol = 3)[,1], matrix(sv[sv[,3]==1,],ncol = 3)[,2], pch = 19, col = "red")
points(matrix(sv[sv[,3]==-1,],ncol = 3)[,1], matrix(sv[sv[,3]==-1,],ncol = 3)[,2], pch = 19, col = "blue")
curve(-w[1]/w[2] * x - w0/w[2], add = TRUE, col = "green")  
curve(-w[1]/w[2] * x - (w0+1)/w[2], add = TRUE, col = "red")  
curve(-w[1]/w[2] * x - (w0-1)/w[2], add = TRUE, col = "blue")  

#add data point
data2 <- rbind(data2, data2, data2)
train <- rbind(data1,data2)
trainX <- train[,-3]
trainY <- as.matrix(train[,3])


H = (trainY %*% t(trainY)) * (trainX %*% t(trainX))
c = matrix(rep(-1, nrow(trainX)))
A = t(trainY)
b = 0
r = 0
l = matrix(rep(0, nrow(trainX)))
u = matrix(rep(100, nrow(trainX)))

require(kernlab)
result = ipop(c, H, A, b, l, u, r)
alpha = matrix(primal(result))

w = rep(0, ncol(trainX))
for(i in 1:nrow(trainX))
{
  w = w + alpha[i] * trainY[i] * trainX[i, ]
}

sv = train[alpha > 50, ] #Support vectors
w0 = (1/nrow(sv)) * sum(as.matrix(sv[, 3]) - sv[, -3] %*% w)

#Show the plots
plot(15:45, 15:45, type = "n", main='Added data', ylab='y', xlab='x')  
points(data1[,1], data1[,2], col = "blue")
points(data2[,1], data2[,2], col = "red")
points(matrix(sv[sv[,3]==1,],ncol = 3)[,1], matrix(sv[sv[,3]==1,],ncol = 3)[,2], pch = 19, col = "red")
points(matrix(sv[sv[,3]==-1,],ncol = 3)[,1], matrix(sv[sv[,3]==-1,],ncol = 3)[,2], pch = 19, col = "blue")
curve(-w[1]/w[2] * x - w0/w[2], add = TRUE, col = "green")  
curve(-w[1]/w[2] * x - (w0+1)/w[2], add = TRUE, col = "red")  
curve(-w[1]/w[2] * x - (w0-1)/w[2], add = TRUE, col = "blue")  