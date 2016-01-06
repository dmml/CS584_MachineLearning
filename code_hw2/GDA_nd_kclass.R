require(cvTools) ##load cross validation package
require(MASS) ##load LDA package

wine <- read.table('wine.txt',sep=',')
data1 <- as.data.frame(cbind(wine[,1],scale(wine[,-1])))
featureNum <- ncol(data1[,-1])

##10 folds cross-validation 
k <- 10 #number of folds
folds <- cvFolds(nrow(data1), K = k, type = "interleaved")
confusionMatrixDefault <- matrix(0,3,3) # confusion matrix computing by R package 
confusionMatrix <- matrix(0,3,3) # confusion matrix computing by implemented function
m1 <- nrow(data1[which(data1$V1=='1'),]) #number of class 1 examples
m2 <- nrow(data1[which(data1$V1=='2'),])#number of class 2 examples
m3 <- nrow(data1[which(data1$V1=='3'),])#number of class 3 examples
m <- nrow(data1) #number of total examples
prior <- c(m1/m,m2/m,m3/m) #prior probability

for(i in 1:k){
  testdata  <- subset(data1, folds$which == i)
  traindata <- subset(data1, folds$which != i)
  mu1 <- colMeans(traindata[which(traindata$V1=='1'),2:14])
  mu2 <- colMeans(traindata[which(traindata$V1=='2'),2:14])
  mu3 <- colMeans(traindata[which(traindata$V1=='3'),2:14])
  sigma <- cov(traindata[,2:14])
  beta12 <- ginv(sigma)%*%(mu1-mu2)
  beta13 <- ginv(sigma)%*%(mu1-mu3)
  beta23 <- ginv(sigma)%*%(mu2-mu3)
  constant12 <- log((prior[1])/(prior[2]))
  constant13 <- log((prior[1])/(prior[3]))
  constant23 <- log((prior[2])/(prior[3]))
  gdaData <- as.matrix(testdata[,2:14])
  yEstimate <- vector()
  for (i in 1:nrow(gdaData)){
    dx12 <- t(beta12)%*%(gdaData[i,]-((mu1+mu2)/2)) - constant12
    dx13 <- t(beta13)%*%(gdaData[i,]-((mu1+mu3)/2)) - constant13
    dx23 <- t(beta23)%*%(gdaData[i,]-((mu2+mu3)/2)) - constant23
    if (dx12>0 && dx13>0)
      yEstimate <- c(yEstimate,1)
    else if(dx12<0 && dx23>0)
      yEstimate <- c(yEstimate,2)
    else
      yEstimate <- c(yEstimate,3)
  }
  
  ## readymade R function LDA
  model <- lda(V1 ~ ., data = traindata, prior = c(m1,m2,m3)/m) 
  pred <- predict(model, testdata)
  yhat <- apply(pred$posterior,1,which.max)
  
  #compute confusion matrix
  confusionMatrixDefault <- confusionMatrixDefault + table(yhat,testdata[,1])
  confusionMatrix <- confusionMatrix + table(yEstimate,testdata[,1])
  
}

#print confusion matrix
confusionMatrixDefault
confusionMatrix

##evaluate performance of algorithm
precision <- as.matrix(c(confusionMatrix[1,1]/sum(confusionMatrix[1,]),confusionMatrix[2,2]/sum(confusionMatrix[2,]),confusionMatrix[3,3]/sum(confusionMatrix[3,])))
rownames(precision) <- c("class1","class2","class3")
recall <- as.matrix(c(confusionMatrix[1,1]/sum(confusionMatrix[,1]),confusionMatrix[2,2]/sum(confusionMatrix[,2]),confusionMatrix[3,3]/sum(confusionMatrix[,3])))
rownames(recall) <- c("class1","class2","class3")
accuracy <- (confusionMatrix[3,3]+confusionMatrix[2,2] + confusionMatrix[1,1])/sum(confusionMatrix)
Fmeasure <- 2*precision*recall/(precision+recall)
precisionDefault <- as.matrix(c(confusionMatrixDefault[1,1]/sum(confusionMatrixDefault[1,]),confusionMatrixDefault[2,2]/sum(confusionMatrixDefault[2,]),confusionMatrixDefault[3,3]/sum(confusionMatrixDefault[3,])))
recallDefault <- as.matrix(c(confusionMatrixDefault[1,1]/sum(confusionMatrixDefault[,1]),confusionMatrixDefault[2,2]/sum(confusionMatrixDefault[,2]),confusionMatrixDefault[3,3]/sum(confusionMatrixDefault[,3])))
accuracyDefault <- (confusionMatrixDefault[3,3]+confusionMatrixDefault[2,2] + confusionMatrixDefault[1,1])/sum(confusionMatrixDefault)
FmeasureDefault <- 2*precisionDefault*recallDefault/(precisionDefault+recallDefault)


#statistics for model
accuracy <- cbind(accuracy,accuracyDefault)
colnames(accuracy) <- c("implement","default")
precision <- cbind(precision,precisionDefault)
colnames(precision) <- c("implement","default")
recall <- cbind(recall,recallDefault)
colnames(recall) <- c("implement","default")
Fmeasure <- cbind(Fmeasure,FmeasureDefault)
colnames(Fmeasure) <- c("implement","default")

#print stats
accuracy
precision
recall
Fmeasure
