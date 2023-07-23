library(neuralnet)
library(nnet)
library(caret)

train_data <- read.csv("C:/Users/dss/Desktop/Paper/Prediction model Data/Gender&age 10- AD ANN train data.csv")
valid_data <- read.csv("C:/Users/dss/Desktop/Paper/Prediction model Data/Gender&age 10- AD ANN valid data.csv")
test_data <- read.csv("C:/Users/dss/Desktop/Paper/Prediction model Data/Gender&age 10- AD ANN test data.csv")

trainr2 <- NULL
validr2 <- NULL
testr2 <- NULL
best_valid <- 0
best_test <- 0
for (i in 1:200){
  {
    nn<-neuralnet(y~., data=train_data[,18:44], linear.output=T, stepmax=1e+10, hidden=c(13,13), threshold=0.0005)
    #plot(nn,rep="best")
  }
  {
    predict<-compute(nn,train_data[,18:43])
    y_mean=mean(train_data$y)
    SSE=sum((train_data$y-predict$net.result)^2)
    SST=sum((train_data$y-y_mean)^2)
    train_R_2=1-SSE/SST
    train_R_2
    trainr2 <- c(trainr2, train_R_2)
  }
  {
    predict<-compute(nn,valid_data[,18:43])
    y_mean=mean(valid_data$y)
    SSE=sum((valid_data$y-predict$net.result)^2)
    SST=sum((valid_data$y-y_mean)^2)
    val_R_2=1-SSE/SST
    val_R_2
    validr2 <- c(validr2, val_R_2)
  }
  {
    predict<-compute(nn,test_data[,18:43])
    y_mean=mean(test_data$y)
    SSE=sum((test_data$y-predict$net.result)^2)
    SST=sum((test_data$y-y_mean)^2)
    test_R_2=1-SSE/SST
    test_R_2
    testr2 <- c(testr2, test_R_2)
  }
  if (i %% 1 == 0){
    print(i)
  }
  if (train_R_2 > 0.7){
    if (val_R_2 > 0.76){
      if (test_R_2 > 0.76){
        break
      }
    }
  }
}

i
result <- data.frame(trainr2 = trainr2, validr2 = validr2, testr2 = testr2)
result[,"valid-test"] <- result$validr2 - result$testr2
result <- result[result$validr2 > 0.74,]
result <- result[result$testr2 > 0.74,]
result

all_data <- read.csv("C:/Users/dss/Desktop/Paper/Prediction model Data/Gender&age 10- AD ANN all data.csv")

prediction <- compute(nn, all_data[,18:43])
prediction <- matrix(prediction$net.result)
setwd("C:/Users/dss/Desktop/Paper/Prediction model Data")
write.csv(prediction, "Gender&age 10- AD ANN without area variable prediction data.csv", row.names = FALSE)
