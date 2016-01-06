## svm ##
#########

#generate data
set.seed(500)
data1 <- cbind(rnorm(150, 20, 1), rnorm(150, 30, 1), matrix(-1, 150, 1))
data2 <- cbind(rnorm(150, 30, 1), rnorm(150, 40, 1), matrix(1, 150, 1))
sepdata <- rbind(data1, data2)
plot(15:45, 15:45, type = "n", main='seperate data', ylab='y', xlab='x')
points(data1[,1], data1[,2], col = "red")
points(data2[,1], data2[,2], col = "blue")

data3 <- cbind(rnorm(150, 20, 2), rnorm(150, 30, 2), matrix(-1, 150, 1))
data4 <- cbind(rnorm(150, 23, 2), rnorm(150, 33, 2), matrix(1, 150, 1))
unsepdata <- rbind(data3, data4)
plot(15:45, 15:45, type = "n", main='unseprate data', ylab='y', xlab='x')
points(data3[,1], data3[,2], col = "red")
points(data4[,1], data4[,2], col = "blue")