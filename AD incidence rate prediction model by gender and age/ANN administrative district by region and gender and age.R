library(neuralnet)
library(nnet)
library(caret)

data <- read.csv("C:/Users/dss/Desktop/Paper/Atopic Data/Administrative district gender and age Data.csv")
column <- c("Area", "DT", "M", "A", "x5", "z1", "y")
names(data) <- column

data$Area <- as.factor(data$Area)
data$M <- as.factor(data$M)
data$A <- as.factor(data$A)

normalize <- function(x){
  return((x-min(x))/(max(x)-min(x))) 
}

data <- data[,c(2,1,3:7)]
data[,5:6] <- as.data.frame(lapply(data[,5:6], normalize))
data <- cbind(data, class.ind(data$Area), class.ind(data$M), class.ind(data$A))
data <- data[,c(1,8:42,5:7)]

Area_code <- c("11", "26", "27", "28", "29", "30", "31", "41", "42", "43", "44", "45", "46", "47", "48", "49")
Age <- paste0("A",seq(from=0, to=80, by=5),rep="")
names(data) <- c("DT", paste0("Area", Area_code), "M", "F", Age, "x5", "z1", "y")
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
  
  nn<-neuralnet(y~., data=train_data[,2:39], linear.output = T , hidden=c(4,4), threshold=0.01)
  plot(nn,rep="best")
}
{
  predict<-compute(nn,train_data[,2:38])
  y_mean=mean(train_data$y)
  SSE=sum((train_data$y-predict$net.result)^2)
  SST=sum((train_data$y-y_mean)^2)
  train_R_2=1-SSE/SST
  train_R_2
}
{
  predict<-compute(nn,val_data[,2:38])
  y_mean=mean(val_data$y)
  SSE=sum((val_data$y-predict$net.result)^2)
  SST=sum((val_data$y-y_mean)^2)
  val_R_2=1-SSE/SST
  val_R_2
}
{
  predict<-compute(nn,test_data[,2:38])
  y_mean=mean(test_data$y)
  SSE=sum((test_data$y-predict$net.result)^2)
  SST=sum((test_data$y-y_mean)^2)
  test_R_2=1-SSE/SST
  test_R_2
}

setwd("C:/Users/dss/Desktop/Paper/Prediction model Data")
write.csv(train_data, file="Administrative district by region and gender and age ANN train data.csv", row.names = FALSE)
write.csv(val_data, file="Administrative district by region and gender and age ANN valid data.csv", row.names = FALSE)
write.csv(test_data, file="Administrative district by region and gender and age ANN test data.csv", row.names = FALSE)

prediction <- compute(nn, data)$net.result
write.csv(prediction, "Administrative district by region and gender and age ANN prediction data.csv", row.names = FALSE)
