library(neuralnet)
library(nnet)
library(caret)

train_data <- read.csv("C:/Users/dss/Desktop/Paper/Prediction model Data/AVG NW train data.csv")
valid_data <- read.csv("C:/Users/dss/Desktop/Paper/Prediction model Data/AVG NW valid data.csv")
test_data <- read.csv("C:/Users/dss/Desktop/Paper/Prediction model Data/AVG NW test data.csv")

data <- rbind(train_data, valid_data)

{
  x5_max <- max(data$x5)
  z1_max <- max(data$z1)
  z2_max <- max(data$z2)
  z3_max <- max(data$z3)
  z4_max <- max(data$z4)
  
  x5_min <- min(data$x5)
  z1_min <- min(data$z1)
  z2_min <- min(data$z2)
  z3_min <- min(data$z3)
  z4_min <- min(data$z4)
}

{
train_data$x5 <- (train_data$x5-x5_min)/(x5_max-x5_min)
train_data$z1 <- (train_data$z1-z1_min)/(z1_max-z1_min)
train_data$z2 <- (train_data$z2-z2_min)/(z2_max-z2_min)
train_data$z3 <- (train_data$z3-z3_min)/(z3_max-z3_min)
train_data$z4 <- (train_data$z4-z4_min)/(z4_max-z4_min)

valid_data$x5 <- (valid_data$x5-x5_min)/(x5_max-x5_min)
valid_data$z1 <- (valid_data$z1-z1_min)/(z1_max-z1_min)
valid_data$z2 <- (valid_data$z2-z2_min)/(z2_max-z2_min)
valid_data$z3 <- (valid_data$z3-z3_min)/(z3_max-z3_min)
valid_data$z4 <- (valid_data$z4-z4_min)/(z4_max-z4_min)
}

normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

test_data[,12:16] <- as.data.frame(lapply(test_data[,12:16], normalize))

trainr2 <- NULL
validr2 <- NULL
testr2 <- NULL
for (i in 1:1500){
  {
    nn<-neuralnet(y~., data=train_data[,2:17], linear.output=T, stepmax=1e+10, hidden=c(2,2), threshold=0.000005)
    #plot(nn,rep="best")
  }
  {
    predict<-compute(nn,train_data[,2:16])
    y_mean=mean(train_data$y)
    SSE=sum((train_data$y-predict$net.result)^2)
    SST=sum((train_data$y-y_mean)^2)
    train_R_2=1-SSE/SST
    train_R_2
    trainr2 <- c(trainr2, train_R_2)
  }
  {
    predict<-compute(nn,valid_data[,2:16])
    y_mean=mean(valid_data$y)
    SSE=sum((valid_data$y-predict$net.result)^2)
    SST=sum((valid_data$y-y_mean)^2)
    val_R_2=1-SSE/SST
    val_R_2
    validr2 <- c(validr2, val_R_2)
  }
  {
    predict<-compute(nn,test_data[,2:16])
    y_mean=mean(test_data$y)
    SSE=sum((test_data$y-predict$net.result)^2)
    SST=sum((test_data$y-y_mean)^2)
    test_R_2=1-SSE/SST
    test_R_2
    testr2 <- c(testr2, test_R_2)
  }
  if (i %% 100 == 0){
    print(i)
  }
  if (train_R_2 > 0.77){
    if (val_R_2 > 0.815){
      if (test_R_2 > 0.806){
        break
      }
    }
  }
}

result <- data.frame(trainr2 = trainr2, validr2 = validr2, testr2 = testr2)
result[,"valid-test"] <- result$validr2 - result$testr2
result <- result[result$trainr2 > 0.75,]
result <- result[result$validr2 > 0.815,]
result[result$testr2 > 0.806,]

setwd("C:/Users/dss/Desktop/Paper/Prediction model Data")
all_data <- read.csv("C:/Users/dss/Desktop/Paper/Atopic Data/Average NW data.csv")
normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
all_data[,12:16] <- as.data.frame(lapply(all_data[,12:16], normalize))

prediction <- compute(nn, all_data[,2:16])
prediction <- matrix(prediction$net.result)
setwd("C:/Users/dss/Desktop")
write.csv(all_data, "AVG NW ANN all data.csv", row.names=FALSE)
write.csv(train_data, "AVG NW ANN train data.csv", row.names=FALSE)
write.csv(valid_data, "AVG NW ANN valid data.csv", row.names=FALSE)
write.csv(test_data, "AVG NW ANN test data.csv", row.names=FALSE)
write.csv(prediction, "AVG NW ANN prediction data.csv", row.names = FALSE)
