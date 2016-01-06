###################################################
# linear regression model of interval-valued data #
###################################################


#tranformation of the data into interval-valued data
data <- read.table("auto.txt")
data1 <- data[1:10,]
attach(data1)
e1 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[11:20,]
attach(data1)
e2 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[21:30,]
attach(data1)
e3 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[31:40,]
attach(data1)
e4 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[41:50,]
attach(data1)
e5 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[51:60,]
attach(data1)
e6 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[61:70,]
attach(data1)
e7 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[71:80,]
attach(data1)
e8 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[81:90,]
attach(data1)
e9 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[91:100,]
attach(data1)
e10 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[101:110,]
attach(data1)
e11 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[111:120,]
attach(data1)
e12 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[121:130,]
attach(data1)
e13 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[131:140,]
attach(data1)
e14 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[141:150,]
attach(data1)
e15 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[151:160,]
attach(data1)
e16 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[161:170,]
attach(data1)
e17 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[171:180,]
attach(data1)
e18 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[181:190,]
attach(data1)
e19 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[191:200,]
attach(data1)
e20 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[201:210,]
attach(data1)
e21 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[211:220,]
attach(data1)
e22 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[221:230,]
attach(data1)
e23 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[231:240,]
attach(data1)
e24 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[241:250,]
attach(data1)
e25 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[251:260,]
attach(data1)
e26 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[261:270,]
attach(data1)
e27 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[271:280,]
attach(data1)
e28 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[281:290,]
attach(data1)
e29 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[291:300,]
attach(data1)
e30 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[301:310,]
attach(data1)
e31 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[311:320,]
attach(data1)
e32 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[321:330,]
attach(data1)
e33 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[331:340,]
attach(data1)
e34 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[341:350,]
attach(data1)
e35 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[351:360,]
attach(data1)
e36 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[361:370,]
attach(data1)
e37 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[371:380,]
attach(data1)
e38 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[381:390,]
attach(data1)
e39 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data1 <- data[391:398,]
attach(data1)
e40 <- c(min(V1),max(V1),min(V3),max(V3),min(V5),max(V5),min(V6),max(V6))
detach(data1)
data0 <- rbind(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15,e16,e17,e18,e19,e20,e21,e22,e23,e24,e25,e26,e27,e28,e29,e30,e31,e32,e33,e34,e35,e36,e37,e38,e39,e40)
data0 <- as.data.frame(data0)

#compute the interval value data for estimating the parameter Beta
xc1 <- (data0$V3+data0$V4)/2
xc2 <- (data0$V5+data0$V6)/2
xc3 <- (data0$V7+data0$V8)/2
xr1 <-  (data0$V4-data0$V3)/2
xr2 <-  (data0$V6-data0$V5)/2
xr3 <- (data0$V8-data0$V7)/2
yc <- (data0$V1+data0$V2)/2
yr <- (data0$V2-data0$V1)/2
a1 <- xc1+xr1
a2 <- xc2+xr2
a3 <- xc3+xr3
y <- yc+yr
n <- nrow(data0)

#construct the Matrix A and vector b for computing the Beta
A <- cbind(c(2*n,sum(a1),sum(a2),sum(a3)),c(sum(a1),sum(a1^2),sum(a1*a2),sum(a1*a3)),c(sum(a2),sum(a1*a2),sum(a2^2),sum(a2*a3)),c(sum(a3),sum(a1*a3),sum(a2*a3),sum(a3^2)))
b <- c(sum(y),sum(y*a1),sum(y*a2),sum(y*a3))

#compute parameter Beta
beta <- solve(A)%*%b

#construct the testing data
testC <- cbind(rep(1,n),xc1,xc2,xc3)
testR <- cbind(rep(1,n),xr1,xr2,xr3)

#predict the values of Y
yhatC <- testC%*%beta
yhatR <- testR%*%beta
yhatUpbound <- yhatC + yhatR
yhatLowbound <- yhatC - yhatR

#compute the boundaries of RMSE
RMSElowbound <- sqrt(sum((data0$V1-yhatLowbound)^2)/n)
RMSEupbound <- sqrt(sum((data0$V2-yhatUpbound)^2)/n)

#compute the correlation coefficient
correlationUp <- cov(data0$V2,yhatUpbound)
stdUp <- sd(data0$V2)*sd(yhatUpbound)
rUp <- correlationUp/stdUp
correlationLow <- cov(data0$V1,yhatLowbound)
stdLow <- sd(data0$V1)*sd(yhatLowbound)
rLow <- correlationLow/stdLow

#output the RMSE result
v <- c(RMSElowbound,RMSEupbound,rUp,rLow)
result <- matrix(v, 1, 4, byrow = T)
colnames(result) <- c("RMSELowerbound", "RMSEUpperbound","rUpbound","rLowbound")

#plot the data
plot(c(0, 5000), c(0, 50), type = "n", xlab = 'data features',ylab='prediction',,main='Auto interval-valued data')
rect(data0$V5,data0$V1,data0$V6,data0$V2)
rect(data0$V3,data0$V1,data0$V4,data0$V2)
rect(data0$V7,data0$V1,data0$V8,data0$V2)
