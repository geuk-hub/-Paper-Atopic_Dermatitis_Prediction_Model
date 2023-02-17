library(neuralnet)
library(nnet)
library(caret)

data <- read.csv("C:/Users/dss/Desktop/Paper/Atopic Data/Administrative district Data.csv")
data <- data[-which(data[,'Area'] == 36),]

normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

data[,"MW0.5"] <- data$M0.5_rate + data$W0.5_rate
data[,"MW4"] <- data$M4_rate + data$W4_rate

data <- data[,c(1:2,49:50,43:44,48)]

column <- c("Area", "DT", "MW0.5", "MW4", "x5", "z1", "y")
names(data) <- column

data[,c(5:6)] <- as.data.frame(lapply(data[,5:6], normalize))
{
  n <- length(data[,1])
  idx <- 1:n
  train_idx <- sample(idx, n*0.6)
  idx <- setdiff(idx, train_idx)
  validate_idx <- sample(idx, n*0.2)
  test_idx <- setdiff(idx, validate_idx)
  
  train_data <- data[train_idx,]
  val_data <- data[validate_idx,]
  test_data <- data[test_idx,]
  
  nn<-neuralnet(y~., data=train_data[,3:7], linear.output=T , hidden=c(4,4), threshold=0.00001)
  plot(nn,rep="best")
}
{
  predict<-compute(nn,train_data[,3:6])
  y_mean=mean(train_data$y)
  SSE=sum((train_data$y-predict$net.result)^2)
  SST=sum((train_data$y-y_mean)^2)
  R_2=1-SSE/SST
  R_2 
}
{
  predict<-compute(nn,val_data[,3:6])
  y_mean=mean(val_data$y)
  SSE=sum((val_data$y-predict$net.result)^2)
  SST=sum((val_data$y-y_mean)^2)
  R_2=1-SSE/SST
  R_2
}
{
  predict<-compute(nn,test_data[,3:6])
  y_mean=mean(test_data$y)
  SSE=sum((test_data$y-predict$net.result)^2)
  SST=sum((test_data$y-y_mean)^2)
  R_2=1-SSE/SST
  R_2
}
package_version(R.version)

setwd("C:/Users/dss/Desktop/Paper/Prediction model Data")
write.csv(train_data, file="Administrative district ANN model train data.csv", row.names = FALSE)
write.csv(val_data, file="Administrative district ANN model valid data.csv", row.names = FALSE)
write.csv(test_data, file="Administrative district ANN model test data.csv", row.names = FALSE)

prediction <- compute(nn, data[,3:6])
prediction <- matrix(prediction$net.result)
write.csv(prediction, "Administrative district ANN model prediction data.csv", row.names = FALSE)
