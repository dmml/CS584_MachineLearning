require(cvTools) ##load cross validation package
require(MASS) ##load LDA package
require(ROCR) ##load ROCR package

wine <- read.table('wine.txt',sep=',')
data1 <- wine[1:130,1:2]

## GDA 1D function 
gda <- function(x,prior){
  g1 <- -0.5*(x-mu1)^2/var+log(prior[1])
  g2 <- -0.5*(x-mu2)^2/var+log(prior[2])
  dx <- g1-g2
}

##10 folds cross-validation 
k <- 10 #number of folds
folds <- cvFolds(nrow(data1), K = k, type = "interleaved")
confusionMatrixDefault <- matrix(0,2,2) # confusion matrix computing by R package 
confusionMatrix <- matrix(0,2,2) # confusion matrix computing by implemented function
m1 <- nrow(data1[which(data1$V1=='1'),]) #number of class 1 examples
m2 <- nrow(data1[which(data1$V1=='2'),]) #number of class 2 examples
m <- nrow(data1) #number of total examples
prior <- c(m1/m,m2/m) #prior probability

for(i in 1:k){
  testdata  <- subset(data1, folds$which == i)
  traindata <- subset(data1, folds$which != i)  
  mu1 <- mean(traindata[which(traindata$V1=='1'),]$V2)
  mu2 <- mean(traindata[which(traindata$V1=='2'),]$V2)
  var <- var(traindata$V2)
  dx <- gda(testdata[,2],prior)
  yEstimate <- vector()
  for (i in 1:length(dx)){
    if(dx[i]>0)
      yEstimate <- c(yEstimate,1)
    else
      yEstimate <- c(yEstimate,2)
  }
  
  ## R package LDA
  model <- lda(V1 ~ ., data = traindata, prior = c(m1,m2)/m) 
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
precision <- as.matrix(c(confusionMatrix[1,1]/(confusionMatrix[1,1] + confusionMatrix[1,2]),confusionMatrix[2,2]/(confusionMatrix[2,1] + confusionMatrix[2,2])))
rownames(precision) <- c("class1","class2")
recall <- as.matrix(c(confusionMatrix[1,1]/(confusionMatrix[1,1] + confusionMatrix[2,1]),confusionMatrix[2,2]/(confusionMatrix[2,2] + confusionMatrix[1,2])))
rownames(recall) <- c("class1","class2")
accuracy <- (confusionMatrix[2,2] + confusionMatrix[1,1])/(confusionMatrix[2,2] + confusionMatrix[1,1]+ confusionMatrix[1,2] + confusionMatrix[2,1])
Fmeasure <- 2*precision*recall/(precision+recall)
precisionDefault <- as.matrix(c(confusionMatrixDefault[1,1]/(confusionMatrixDefault[1,1] + confusionMatrixDefault[1,2]),confusionMatrixDefault[2,2]/(confusionMatrixDefault[2,1] + confusionMatrixDefault[2,2])))
recallDefault <- as.matrix(c(confusionMatrixDefault[1,1]/(confusionMatrixDefault[1,1] + confusionMatrixDefault[2,1]),confusionMatrixDefault[2,2]/(confusionMatrixDefault[2,2] + confusionMatrixDefault[1,2])))
accuracyDefault <- (confusionMatrixDefault[2,2] + confusionMatrixDefault[1,1])/(confusionMatrixDefault[2,2] + confusionMatrixDefault[1,1]+ confusionMatrixDefault[1,2] + confusionMatrixDefault[2,1])
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

