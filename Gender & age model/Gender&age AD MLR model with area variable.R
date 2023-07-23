data <- read.csv("C:/Users/dss/Desktop/Paper/Atopic Data/Gender&age 0-9 AD data.csv")

data$Area <- as.factor(data$Area)
data$A <- as.factor(data$A)
data$G <- as.factor(data$G)

normalize <- function(x){
  return(scale(x)+4)
}

mean <- mean(data$y)
sd <- sd(data$y)

data[,5:14] <- as.data.frame(lapply(data[,5:14], normalize))

re <- lm(y~Area+G+A+G:A+x1+z1+z2+z3+z4, data)
summary(re)
anova(re)

train_index <- sample(1:length(data[,1]), length(data[,1])*0.8)
train_data <- data[train_index,]
test_data <- data[-train_index,]
model_AD <- lm(y~Area+G+A+G:A+x1+z1+z2+z3+z4, train_data)
summary(model_AD)
anova(model_AD)

predict <- predict(model_AD, test_data)
y_mean = mean(test_data$y)
SSE = sum((test_data$y-predict)^2)
SST = sum((test_data$y-y_mean)^2)
R_2 = 1-SSE/SST
R_2

setwd("C:/Users/dss/Desktop/Paper/Prediction model Data")
write.csv(train_data, file="Gender&age 0-9 AD MLR train data.csv", row.names = FALSE)
write.csv(test_data, file="Gender&age 0-9 AD test data.csv", row.names = FALSE)

prediction <- matrix(predict(model_AD, data))
prediction <- (prediction - 4)*sd + mean
prediction[prediction<0] <- 0
write.csv(prediction, "Gender&age 0-9 AD MLR prediction data with area variable.csv", row.names = FALSE)


###

data <- read.csv("C:/Users/dss/Desktop/Paper/Atopic Data/Gender&age 10- AD data.csv")

data$Area <- as.factor(data$Area)
data$A <- as.factor(data$A)
data$G <- as.factor(data$G)

normalize <- function(x){
  return(scale(x)+4)
}

mean <- mean(data$y)
sd <- sd(data$y)

data[,5:14] <- as.data.frame(lapply(data[,5:14], normalize))

re <- lm(y~G+A+G:A+z1+z2+z3+z4, data)
summary(re)
anova(re)

train_index <- sample(1:length(data[,1]), length(data[,1])*0.8)
train_data <- data[train_index,]
test_data <- data[-train_index,]
model_AD <- lm(y~Area+G+A+G:A+z1+z2+z3+z4, train_data)
summary(model_AD)
anova(model_AD)

predict <- predict(model_AD, test_data)
y_mean = mean(test_data$y)
SSE = sum((test_data$y-predict)^2)
SST = sum((test_data$y-y_mean)^2)
R_2 = 1-SSE/SST
R_2

setwd("C:/Users/dss/Desktop/Paper/Prediction model Data")
write.csv(train_data, file="Gender&age 10- AD MLR train data.csv", row.names = FALSE)
write.csv(test_data, file="Gender&age 10- AD test data.csv", row.names = FALSE)

prediction <- matrix(predict(model_AD, data))
prediction <- (prediction - 4)*sd + mean
prediction[prediction<0] <- 0
write.csv(prediction, "Gender&age 10- AD MLR prediction data with area variable.csv", row.names = FALSE)
