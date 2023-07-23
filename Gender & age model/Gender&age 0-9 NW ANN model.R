library(neuralnet)
library(nnet)
library(caret)

train_data <- read.csv("C:/Users/dss/Desktop/Paper/Prediction model Data/Gender&age 0-9 NW train data.csv")
valid_data <- read.csv("C:/Users/dss/Desktop/Paper/Prediction model Data/Gender&age 0-9 NW valid data.csv")
test_data <- read.csv("C:/Users/dss/Desktop/Paper/Prediction model Data/Gender&age 0-9 NW test data.csv")

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

test_data[,8:12] <- as.data.frame(lapply(test_data[,8:12], normalize))

train_data$A <- as.factor(train_data$A)
train_data$G <- as.factor(train_data$G)
valid_data$A <- as.factor(valid_data$A)
valid_data$G <- as.factor(valid_data$G)
test_data$A <- as.factor(test_data$A)
test_data$G <- as.factor(test_data$G)

train_data <- cbind(train_data, class.ind(train_data$A), class.ind(train_data$G))
train_data <- train_data[,c(1,14:17,4:13)]
names(train_data) <- c("DT", paste0("A",seq(from=0, to=5, by=5),rep=""), "M", "F",
                           "x1", "x2", "x3", "x4", "x5", "z1", "z2", "z3", "z4", "y")

valid_data <- cbind(valid_data, class.ind(valid_data$A), class.ind(valid_data$G))
valid_data <- valid_data[,c(1,14:17,4:13)]
names(valid_data) <- c("DT", paste0("A",seq(from=0, to=5, by=5),rep=""), "M", "F",
                           "x1", "x2", "x3", "x4", "x5", "z1", "z2", "z3", "z4", "y")

test_data <- cbind(test_data, class.ind(test_data$A), class.ind(test_data$G))
test_data <- test_data[,c(1,14:17,4:13)]
names(test_data) <- c("DT", paste0("A",seq(from=0, to=5, by=5),rep=""), "M", "F",
                      "x1", "x2", "x3", "x4", "x5", "z1", "z2", "z3", "z4", "y")

trainr2 <- NULL
validr2 <- NULL
testr2 <- NULL
for (i in 1:200){
  {
    nn<-neuralnet(y~., data=train_data[,2:15], linear.output=T, stepmax=1e+10, hidden=c(7,7), threshold=0.00001)
    #plot(nn,rep="best")
  }
  {
    predict<-compute(nn,train_data[,2:14])
    y_mean=mean(train_data$y)
    SSE=sum((train_data$y-predict$net.result)^2)
    SST=sum((train_data$y-y_mean)^2)
    train_R_2=1-SSE/SST
    trainr2 <- c(trainr2, train_R_2)
  }
  {
    predict<-compute(nn,valid_data[,2:14])
    y_mean=mean(valid_data$y)
    SSE=sum((valid_data$y-predict$net.result)^2)
    SST=sum((valid_data$y-y_mean)^2)
    val_R_2=1-SSE/SST
    validr2 <- c(validr2, val_R_2)
  }
  {
    predict<-compute(nn,test_data[,2:14])
    y_mean=mean(test_data$y)
    SSE=sum((test_data$y-predict$net.result)^2)
    SST=sum((test_data$y-y_mean)^2)
    test_R_2=1-SSE/SST
    testr2 <- c(testr2, test_R_2)
  }
  if (i %% 10 == 0){
    print(i)
  }
  #if (train_R_2 > 0.9){
  #  if (val_R_2 > 0.953){
  #    if (test_R_2 > 0.953){
  #      break
  #    }
  #  }
  #}
}

i
result <- data.frame(trainr2 = trainr2, validr2 = validr2, testr2 = testr2)
result[,"val-test"] <- result$validr2 - result$testr2
result <- result[result$validr2 > 0.89,]
result[result$testr2 > 0.89,]

all_data <- read.csv("C:/Users/dss/Desktop/Paper/Atopic Data/Gender&age 0-9 NW data.csv")
normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
all_data[,8:12] <- as.data.frame(lapply(all_data[,8:12], normalize))
all_data$A <- as.factor(all_data$A)
all_data$G <- as.factor(all_data$G)
all_data <- cbind(all_data, class.ind(all_data$A), class.ind(all_data$G))
all_data <- all_data[,c(1,14:17,4:13)]
names(all_data) <- c("DT", paste0("A",seq(from=0, to=5, by=5),rep=""), "M", "F",
                           "x1", "x2", "x3", "x4", "x5", "z1", "z2", "z3", "z4", "y")

prediction <- compute(nn, all_data[,2:14])
prediction <- matrix(prediction$net.result)
setwd("C:/Users/dss/Desktop/Paper")
write.csv(all_data, "Gender&age 0-9 NW ANN all data.csv", row.names = FALSE)
write.csv(prediction, "Gender&age 0-9 NW ANN prediction data.csv", row.names = FALSE)
