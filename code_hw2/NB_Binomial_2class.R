rm(list = ls())
require(cvTools)
require(MASS)
require(e1071)

#transform the datasets into binomial form
wine <- read.table('wine.txt', sep=',')
data <- wine [1:130,]
data$V2 <- cut(data$V2, breaks = c(0,11, 13,Inf), labels = c(1:3))
data$V3 <- cut(data$V3, breaks = c(0,1, 3, 5, Inf), labels = c(1:4))
data$V4 <- cut(data$V4, breaks = c(0,3, Inf), labels = c(1:2))
data$V5 <- cut(data$V5, breaks = c(0,10,15,20, Inf), labels = c(1:4))
data$V6 <- cut(data$V6, breaks = c(0,75,85,95, Inf), labels = c(1:4))
data$V7 <- cut(data$V7, breaks = c(0,1,2, Inf), labels = c(1:3))
data$V8 <- cut(data$V8, breaks = c(0,1,2, Inf), labels = c(1:3))
data$V9 <- cut(data$V9, breaks = c(0,0.15,0.25, Inf), labels = c(1:3))
data$V10 <- cut(data$V10, breaks = c(0,1.5,2.5, Inf), labels = c(1:3))
data$V11 <- cut(data$V11, breaks = c(0,11,12,13, Inf), labels = c(1:4))
data$V12 <- cut(data$V12, breaks = c(0,0.8,1,1.5, Inf), labels = c(1:4))
data$V13 <- cut(data$V13, breaks = c(0,2,3, Inf), labels = c(1:3))
data$V14 <- cut(data$V14, breaks = c(0,500, 800, 1100, 1400, Inf), labels = c(1:5))
attach(data)
data <- as.data.frame(cbind(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14))
detach(data)


#10 folds cross-validation
k <- 10
folds <- cvFolds(nrow(data), K = k, type = "interleaved")
confusionMatrixDefault <- matrix(0,2,2) # confusion matrix computing by R package 
confusionMatrix <- matrix(0,2,2) #computed by implementing functions
m1 <- nrow(data[which(data$V1=='1'),])
m2 <- nrow(data[which(data$V1=='2'),])
m <- nrow(data)
prior <- c(m1/m,m2/m)

for (i in 1:k){
  testdata  <- subset(data, folds$which == i)
  traindata <- subset(data, folds$which != i)
  probabilitySumOfClass1<-0
  probabilitySumOfClass2<-0
  mTrain<-nrow(traindata)
  for (j in 1:mTrain){
    
      if (traindata[j,1]==1){
        for (k in 2:14){
        probabilitySumOfClass1<-probabilitySumOfClass1+traindata[j,k]
        }
      }else{
        for (k in 2:14){
        probabilitySumOfClass2<-probabilitySumOfClass2+traindata[j,k]
        }
      }

  }
  
  #parameter alpha
  alphaClass1<-colMeans(traindata[which(traindata$V1=='1'),2:14])
  alphaClass1<-nrow(traindata[which(traindata$V1=='1'),])/probabilitySumOfClass1*alphaClass1
  alphaClass2<-colMeans(traindata[which(traindata$V1=='2'),2:14])
  alphaClass2<-nrow(traindata[which(traindata$V1=='2'),])/probabilitySumOfClass2*alphaClass2
  
  mTest <- nrow(testdata) #testdata number
  yEstimate<-vector() #predict class label
  for (i1 in 1:mTest){
    g1 <-1
    g2 <-1
    temp <-0  
    for (j1 in 2:14){
      temp<-temp+testdata[i1,j1]
    }
    for (j2 in 2:14){
      g1<-g1*choose(temp,testdata[i1,j2])*(alphaClass1[j2-1]^testdata[i1,j2])*((1-alphaClass1[j2-1])^(temp-testdata[i1,j2]))
      g2<-g2*choose(temp,testdata[i1,j2])*(alphaClass2[j2-1]^testdata[i1,j2])*((1-alphaClass2[j2-1])^(temp-testdata[i1,j2]))
    }
    dx<-g1-g2
    if (dx>0){
      yEstimate <- c(yEstimate,1)
    }else{
      yEstimate <- c(yEstimate,2)
    }  
    
  }
  
  #Navie Bayes readymade R function
  model <- naiveBayes(as.factor(V1) ~ ., data = traindata)
  yhat <- predict(model, testdata[,-1])
  
  #compute confusion matrix
  confusionMatrixDefault <- confusionMatrixDefault + table(yhat,testdata[,1]) 
  confusionMatrix <- confusionMatrix + table(yEstimate,testdata[,1])  
}  

#print confusion matrix of default and implemented
confusionMatrixDefault
confusionMatrix

##evaluate performance of algorithm
precision <- as.matrix(c(confusionMatrix[1,1]/(confusionMatrix[1,1] + confusionMatrix[1,2]),confusionMatrix[2,2]/(confusionMatrix[2,1] + confusionMatrix[2,2])))
rownames(precision) <- c("class1","class2")
recall <- as.matrix(c(confusionMatrix[1,1]/(confusionMatrix[1,1] + confusionMatrix[2,1]),confusionMatrix[2,2]/(confusionMatrix[2,2] + confusionMatrix[1,2])))
rownames(recall) <- c("class1","class2")
accuracy <- (confusionMatrix[2,2] + confusionMatrix[1,1])/(confusionMatrix[2,2] + confusionMatrix[1,1]+ confusionMatrix[1,2] + confusionMatrix[2,1])
Fmeasure <- 2*precision*recall/(precision+recall)

#compute stats
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


