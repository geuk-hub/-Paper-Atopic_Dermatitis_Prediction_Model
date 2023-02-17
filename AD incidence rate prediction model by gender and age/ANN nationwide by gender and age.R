library(nnet)
library(neuralnet)
library(caret)

data <- read.csv("C:/Users/dss/Desktop/Paper/Atopic Data/Nationwide gender and age Data.csv")
column <- c("DT", "A", "M", "x1", "x2", "x3", "x4", "x5", "z1", "z2", "z3", "z4", "y")
names(data) <- column

data <- data[,c(1:3,8:9,13)]

data$A <- as.factor(data$A)
data$M <- as.factor(data$M)

normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

data[,4:5] <- as.data.frame(lapply(data[,4:5], normalize))

data <- cbind(data, class.ind(data$A), class.ind(data$M))

data <- data[,c(1,7:25,4:6)]
names(data) <- c("DT", paste0("A",seq(from=0, to=80, by=5),rep=""), "M", "F", "x5", "z1", "y")
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

nn<-neuralnet(y~., data=train_data[,2:23], linear.output = T , hidden=c(10,10), threshold=0.0001)
plot(nn,rep="best")
}
{
predict<-compute(nn,train_data[,2:22])
y_mean=mean(train_data$y)
SSE=sum((train_data$y-predict$net.result)^2)
SST=sum((train_data$y-y_mean)^2)
train_R_2=1-SSE/SST
train_R_2
}
{
predict<-compute(nn,val_data[,2:22])
y_mean=mean(val_data$y)
SSE=sum((val_data$y-predict$net.result)^2)
SST=sum((val_data$y-y_mean)^2)
val_R_2=1-SSE/SST
val_R_2
}
{
predict<-compute(nn,test_data[,2:22])
y_mean=mean(test_data$y)
SSE=sum((test_data$y-predict$net.result)^2)
SST=sum((test_data$y-y_mean)^2)
test_R_2=1-SSE/SST
test_R_2
}
plot(predict$net.result, test_data$y)

setwd("C:/Users/dss/Desktop/Paper/Prediction model Data")
write.csv(train_data, file="Nationwide by gender and age ANN train data.csv", row.names = FALSE)
write.csv(val_data, file="Nationwide by gender and age ANN valid data.csv", row.names = FALSE)
write.csv(test_data, file="Nationwide by gender and age ANN test data.csv", row.names = FALSE)

prediction <- compute(nn, data)$net.result
write.csv(prediction, "Nationwide by gender and age ANN prediction data.csv", row.names = FALSE)
