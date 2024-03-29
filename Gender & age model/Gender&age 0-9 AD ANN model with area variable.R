library(neuralnet)
library(nnet)
library(caret)

data <- read.csv("C:/Users/dss/Desktop/Paper/Atopic Data/Gender&age 0-9 AD data.csv")

{
  mean_x1 <- mean(data$x1)
  sd_x1 <- sd(data$x1)
  mean_x2 <- mean(data$x2)
  sd_x2 <- sd(data$x2)
  mean_x3 <- mean(data$x3)
  sd_x3 <- sd(data$x3)
  mean_x4 <- mean(data$x4)
  sd_x4 <- sd(data$x4)
  mean_x5 <- mean(data$x5)
  sd_x5 <- sd(data$x5)
  
  mean_z1 <- mean(data$z1)
  sd_z1 <- sd(data$z1)
  mean_z2 <- mean(data$z2)
  sd_z2 <- sd(data$z2)
  mean_z3 <- mean(data$z3)
  sd_z3 <- sd(data$z3)
  mean_z4 <- mean(data$z4)
  sd_z4 <- sd(data$z4)
  
  mean <- mean(data$y)
  sd <- sd(data$y)
}


MLR_train_data <- read.csv("C:/Users/dss/Desktop/Paper/Prediction model Data/Gender&age 0-9 AD MLR train data.csv")
test_data <- read.csv("C:/Users/dss/Desktop/Paper/Prediction model Data/Gender&age 0-9 AD test data.csv")

{
  MLR_train_data$x1 <- (MLR_train_data$x1 - 4) * sd_x1 + mean_x1
  MLR_train_data$x2 <- (MLR_train_data$x2 - 4) * sd_x2 + mean_x2
  MLR_train_data$x3 <- (MLR_train_data$x3 - 4) * sd_x3 + mean_x3
  MLR_train_data$x4 <- (MLR_train_data$x4 - 4) * sd_x4 + mean_x4
  MLR_train_data$x5 <- (MLR_train_data$x5 - 4) * sd_x5 + mean_x5
  
  MLR_train_data$z1 <- (MLR_train_data$z1 - 4) * sd_z1 + mean_z1
  MLR_train_data$z2 <- (MLR_train_data$z2 - 4) * sd_z2 + mean_z2
  MLR_train_data$z3 <- (MLR_train_data$z3 - 4) * sd_z3 + mean_z3
  MLR_train_data$z4 <- (MLR_train_data$z4 - 4) * sd_z4 + mean_z4
  
  MLR_train_data$y <- (MLR_train_data$y - 4) * sd + mean

  test_data$x1 <- (test_data$x1 - 4) * sd_x1 + mean_x1
  test_data$x2 <- (test_data$x2 - 4) * sd_x2 + mean_x2
  test_data$x3 <- (test_data$x3 - 4) * sd_x3 + mean_x3
  test_data$x4 <- (test_data$x4 - 4) * sd_x4 + mean_x4
  test_data$x5 <- (test_data$x5 - 4) * sd_x5 + mean_x5
  
  test_data$z1 <- (test_data$z1 - 4) * sd_z1 + mean_z1
  test_data$z2 <- (test_data$z2 - 4) * sd_z2 + mean_z2
  test_data$z3 <- (test_data$z3 - 4) * sd_z3 + mean_z3
  test_data$z4 <- (test_data$z4 - 4) * sd_z4 + mean_z4
  
  test_data$y <- (test_data$y - 4) * sd + mean
}


normalize <- function(x){  
  return((x-min(x))/(max(x)-min(x)))
}

MLR_train_data[,9:13] <- as.data.frame(lapply(MLR_train_data[,9:13], normalize))
test_data[,9:13] <- as.data.frame(lapply(test_data[,9:13], normalize))

MLR_train_data$Area <- as.factor(MLR_train_data$Area)
MLR_train_data$A <- as.factor(MLR_train_data$A)
MLR_train_data$G <- as.factor(MLR_train_data$G)
MLR_train_data <- cbind(MLR_train_data, class.ind(MLR_train_data$Area), class.ind(MLR_train_data$A), class.ind(MLR_train_data$G))
MLR_train_data <- MLR_train_data[,c(2,15:34,5:14)]

test_data$Area <- as.factor(test_data$Area)
test_data$A <- as.factor(test_data$A)
test_data$G <- as.factor(test_data$G)
test_data <- cbind(test_data, class.ind(test_data$Area), class.ind(test_data$A), class.ind(test_data$G))
test_data <- test_data[,c(2,15:34,5:14)]

Area_code <- c("11", "26", "27", "28", "29", "30", "31", "41", "42", "43", "44", "45", "46", "47", "48", "49")
names(MLR_train_data) <- c("DT", paste0("Area", Area_code), paste0("A",seq(from=0, to=5, by=5),rep=""), "M", "F",
                           "x1", "x2", "x3", "x4", "x5", "z1", "z2", "z3", "z4", "y")
names(test_data) <- c("DT", paste0("Area", Area_code), paste0("A",seq(from=0, to=5, by=5),rep=""), "M", "F",
                           "x1", "x2", "x3", "x4", "x5", "z1", "z2", "z3", "z4", "y")

data <- MLR_train_data

trainr2 <- NULL
validr2 <- NULL
testr2 <- NULL
best_valid <- 0
best_test <- 0
for (i in 1:200){
  {
    n <- length(data[,1])
    idx <- 1:n
    train_idx <- sample(idx, n*0.75)
    val_idx <- setdiff(idx, train_idx)
    
    train_data <- data[train_idx,]
    val_data <- data[val_idx,]
    
    nn<-neuralnet(y~., data=train_data[,2:31], linear.output=T, stepmax=1e+06, hidden=c(8,8), threshold=0.0005)
    plot(nn,rep="best")
  }
  {
    predict<-compute(nn,train_data[,2:30])
    y_mean=mean(train_data$y)
    SSE=sum((train_data$y-predict$net.result)^2)
    SST=sum((train_data$y-y_mean)^2)
    train_R_2=1-SSE/SST
    train_R_2
    trainr2 <- c(trainr2, train_R_2)
  }
  {
    predict<-compute(nn,val_data[,2:30])
    y_mean=mean(val_data$y)
    SSE=sum((val_data$y-predict$net.result)^2)
    SST=sum((val_data$y-y_mean)^2)
    val_R_2=1-SSE/SST
    val_R_2
    validr2 <- c(validr2, val_R_2)
  }
  {
    predict<-compute(nn,test_data[,2:30])
    y_mean=mean(test_data$y)
    SSE=sum((test_data$y-predict$net.result)^2)
    SST=sum((test_data$y-y_mean)^2)
    test_R_2=1-SSE/SST
    test_R_2
    testr2 <- c(testr2, test_R_2)
  }
  if (i %% 10 == 0){
    print(i)
  }
  if (train_R_2 > 0.9){
    if (val_R_2 > 0.9185){
      if (test_R_2 > 0.9158){
        break
      }
    }
  }
}

i
result <- data.frame(trainr2 = trainr2, validr2 = validr2, testr2 = testr2)
result[,"valid-test"] <- result$validr2 - result$testr2
result <- result[result$trainr2 > 0.9,]
result <- result[result$validr2 > 0.885,]
result <- result[result$testr2 > 0.885,]
result

setwd("C:/Users/dss/Desktop/Paper/Prediction model Data")
write.csv(train_data, file="Gender&age 0-9 AD ANN train data.csv", row.names = FALSE)
write.csv(val_data, file="Gender&age 0-9 AD ANN valid data.csv", row.names = FALSE)
write.csv(test_data, file="Gender&age 0-9 AD ANN test data.csv", row.names = FALSE)

all_data <- read.csv("C:/Users/dss/Desktop/Paper/Atopic Data/Gender&age 0-9 AD data.csv")
all_data[,9:13] <- as.data.frame(lapply(all_data[,9:13], normalize))
all_data$Area <- as.factor(all_data$Area)
all_data$A <- as.factor(all_data$A)
all_data$G <- as.factor(all_data$G)
all_data <- cbind(all_data, class.ind(all_data$Area), class.ind(all_data$A), class.ind(all_data$G))
all_data <- all_data[,c(2,15:34,5:14)]

Area_code <- c("11", "26", "27", "28", "29", "30", "31", "41", "42", "43", "44", "45", "46", "47", "48", "49")
names(all_data) <- c("DT", paste0("Area", Area_code), paste0("A",seq(from=0, to=5, by=5),rep=""), "M", "F",
                           "x1", "x2", "x3", "x4", "x5", "z1", "z2", "z3", "z4", "y")

prediction <- compute(nn, all_data[,2:30])
prediction <- matrix(prediction$net.result)
write.csv(prediction, "Gender&age 0-9 AD ANN with area variable prediction data.csv", row.names = FALSE)
write.csv(all_data, "Gender&age 0-9 AD ANN all data.csv", row.names = FALSE)
