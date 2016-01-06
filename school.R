###################################################
# linear regression model of interval-valued data #
###################################################

data <- read.table("data.txt", sep = ",")

#compute the interval value data for estimating the parameter Beta
xc1 <- (data$V3+data$V4)/2
xc2 <- (data$V5+data$V6)/2 
xr1 <-  (data$V4-data$V3)/2
xr2 <-  (data$V6-data$V5)/2
yc <- (data$V1+data$V2)/2
yr <- (data$V2-data$V1)/2
a1 <- xc1+xr1
a2 <- xc2+xr2
y <- yc+yr
n <- nrow(data)

#construct the Matrix A and vector b for computing the Beta
A <- cbind(c(2*n,sum(a1),sum(a2)),c(sum(a1),sum(a1^2),sum(a1*a2)),c(sum(a2),sum(a1*a2),sum(a2^2)))
b <- c(sum(y),sum(y*a1),sum(y*a2))

#compute parameter
beta <- solve(A)%*%b

#construct the testing data
testC <- cbind(rep(1,n),xc1,xc2)
testR <- cbind(rep(1,n),xr1,xr2)

#predict the values of Y
yhatC <- testC%*%beta
yhatR <- testR%*%beta
yhatUpbound <- yhatC + yhatR
yhatLowbound <- yhatC - yhatR

#compute RMSE of the boundaries
RMSElowbound <- sqrt(sum((data$V1-yhatLowbound)^2)/n)
RMSEupbound <- sqrt(sum((data$V2-yhatUpbound)^2)/n)

#compute the correlation coefficient
correlationUp <- cov(data$V2,yhatUpbound)
stdUp <- sd(data$V2)*sd(yhatUpbound)
rUp <- correlationUp/stdUp
correlationLow <- cov(data$V1,yhatLowbound)
stdLow <- sd(data$V1)*sd(yhatLowbound)
rLow <- correlationLow/stdLow

#output the RMSE result
v <- c(RMSElowbound,RMSEupbound,rUp,rLow)
result <- matrix(v, 1, 4, byrow = T)
colnames(result) <- c("RMSELowerbound", "RMSEUpperbound","rUpbound","rLowbound")

#plot the data
plot(c(0, 200), c(50, 100), type = "n", xlab = 'data features',ylab='prediction',,main='A high school interval-valued data')
rect(data$V5,data$V1,data$V6,data$V2)
rect(data$V3,data$V1,data$V4,data$V2)

