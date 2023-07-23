data_path <- c("C:/Users/dss/Desktop/Paper/Prediction model Data")
train_data <- read.csv(paste0(data_path, "/Gender&age 0-9 AD MLR train data.csv"))
test_data <- read.csv(paste0(data_path, "/Gender&age 0-9 AD test data.csv"))

train_data$A <- as.factor(train_data$A)
train_data$G <- as.factor(train_data$G)
test_data$A <- as.factor(test_data$A)
test_data$G <- as.factor(test_data$G)

model_AD <- lm(y~G+A+G:A+x1+z1+z2+z3+z4, train_data)
summary(model_AD)
anova(model_AD)

predict <- predict(model_AD, test_data)
y_mean = mean(test_data$y)
SSE = sum((test_data$y-predict)^2)
SST = sum((test_data$y-y_mean)^2)
R_2 = 1-SSE/SST
R_2


data <- read.csv("C:/Users/dss/Desktop/Paper/Atopic Data/Gender&age 0-9 AD data.csv")

data$A <- as.factor(data$A)
data$G <- as.factor(data$G)

mean <- mean(data$y)
sd <- sd(data$y)

normalize <- function(x){
  return(scale(x)+4)
}
data[,5:14] <- as.data.frame(lapply(data[,5:14], normalize))

setwd("C:/Users/dss/Desktop/Paper/Prediction model Data")
prediction <- matrix(predict(model_AD, data))
prediction <- (prediction - 4)*sd + mean
prediction[prediction<0] <- 0
write.csv(prediction, "Gender&age 0-9 AD MLR prediction data without area variable.csv", row.names = FALSE)


###

data_path <- c("C:/Users/dss/Desktop/Paper/Prediction model Data")
train_data <- read.csv(paste0(data_path, "/Gender&age 10- AD MLR train data.csv"))
test_data <- read.csv(paste0(data_path, "/Gender&age 10- AD test data.csv"))

train_data$A <- as.factor(train_data$A)
train_data$G <- as.factor(train_data$G)
test_data$A <- as.factor(test_data$A)
test_data$G <- as.factor(test_data$G)

model_AD <- lm(y~G+A+G:A+z1+z2+z3+z4, train_data)
summary(model_AD)
anova(model_AD)

predict <- predict(model_AD, test_data)
y_mean = mean(test_data$y)
SSE = sum((test_data$y-predict)^2)
SST = sum((test_data$y-y_mean)^2)
R_2 = 1-SSE/SST
R_2

data <- read.csv("C:/Users/dss/Desktop/Paper/Atopic Data/Gender&age 10- AD data.csv")

data$A <- as.factor(data$A)
data$G <- as.factor(data$G)

mean <- mean(data$y)
sd <- sd(data$y)

normalize <- function(x){
  return(scale(x)+4)
}
data[,5:14] <- as.data.frame(lapply(data[,5:14], normalize))

setwd("C:/Users/dss/Desktop/Paper/Prediction model Data")
prediction <- matrix(predict(model_AD, data))
prediction <- (prediction - 4)*sd + mean
prediction[prediction<0] <- 0
write.csv(prediction, "Gender&age 10- AD MLR prediction data without area variable.csv", row.names = FALSE)
