#############
##    EM   ##
#############

#remove the class label
iris <- read.table("iris.txt",sep=",")
data <- iris[,1:4]

#guess the parameters of given data
probabilityOfGuess <- function(data,mu,sigma){
  gaussian <- (exp(-0.5 * t(as.matrix(data - mu)) %*% ginv(sigma) %*% as.matrix(data - mu)))//((2*pi)^2 * sqrt(det(sigma)))
  gaussian
}

#assume a Gaussian mixture model
k <- 3
mu <- c(1,1,1,1)
sigma <- diag(1,4,4)
alpha <- 1/k

alphaNew <- vector()
muNew <- vector()
change = TRUE
while(change){
  
  for (i in 1:150){
    alphaSum <- alphaSum + alpha*probabilityOfGuess(data,mu[i],sigma[i])
  }
  
  probabilityOfGivenX <- alpha*probabilityOfGuess(data,mu[i],sigma[i])/sum
  
  m <- length(data[,1])
  alphaNew <- alphaNew + probabilityOfGivenX
  alphaNew <- alphaNew/m
  
  for (i in 1:150){
    muSum <- muSum + data[i,]*probabilityOfGivenX
  }
  muNew <- muNew + muSum/m*alphaNew
  
  for (i in 1:150){
    sigmaSum <- sigmaSum + probabilityOfGivenX*(data[i,] - muNew)) %*% t(as.matrix((data[i,] - muNew)))
  }
  sigmaNew <- sigmaSum/(matrix(m) * alphaNew, 4, 4))
  
  #stop when reach the threshold
  if ((det(sigmaNew - sigma)) >= 0)
    change = FALSE
  }
  
#evaluate the performance by Mahalanobis Distance
testdata <- data[1,]
MahalanobisDistance <- sqrt(t(as.matrix(testdata-muNew)) %*% ginv(sigmaNew) %*% as.matrix(testdata-muNew))

}



  

