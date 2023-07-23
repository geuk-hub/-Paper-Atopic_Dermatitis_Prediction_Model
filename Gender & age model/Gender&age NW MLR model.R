data <- read.csv("C:/Users/dss/Desktop/Paper/Atopic Data/Gender&age 0-9 NW data.csv")

data$A <- as.factor(data$A)
data$G <- as.factor(data$G)

re <- lm(y~G+A+G:A+x1+z1+z2+z3+z4, data)
summary(re)
anova(re)

{
  mean <- mean(data$y)
  sd <- sd(data$y)
  
  x1_mean <- mean(data$x1)
  x1_sd <- sd(data$x1)
  
  z1_mean <- mean(data$z1)
  z2_mean <- mean(data$z2)
  z3_mean <- mean(data$z3)
  z4_mean <- mean(data$z4)
  z1_sd <- sd(data$z1)
  z2_sd <- sd(data$z2)
  z3_sd <- sd(data$z3)
  z4_sd <- sd(data$z4)
}

train_data <- read.csv("C:/Users/dss/Desktop/Paper/Prediction model Data/Gender&age 0-9 NW train data.csv")
valid_data <- read.csv("C:/Users/dss/Desktop/Paper/Prediction model Data/Gender&age 0-9 NW valid data.csv")
test_data <- read.csv("C:/Users/dss/Desktop/Paper/Prediction model Data/Gender&age 0-9 NW test data.csv")

train_data <- rbind(train_data, valid_data)

train_data$A <- as.factor(train_data$A)
train_data$G <- as.factor(train_data$G)
test_data$A <- as.factor(test_data$A)
test_data$G <- as.factor(test_data$G)

{
  train_data$x1 <- (train_data$x1 - x1_mean) / x1_sd + 4
  train_data$z1 <- (train_data$z1 - z1_mean) / z1_sd + 4
  train_data$z2 <- (train_data$z2 - z2_mean) / z2_sd + 4
  train_data$z3 <- (train_data$z3 - z3_mean) / z3_sd + 4
  train_data$z4 <- (train_data$z4 - z4_mean) / z4_sd + 4
  train_data$y <- (train_data$y - mean) / sd + 4
  
  test_data$x1 <- (test_data$x1 - x1_mean) / x1_sd + 4
  test_data$z1 <- (test_data$z1 - z1_mean) / z1_sd + 4
  test_data$z2 <- (test_data$z2 - z2_mean) / z2_sd + 4
  test_data$z3 <- (test_data$z3 - z3_mean) / z3_sd + 4
  test_data$z4 <- (test_data$z4 - z4_mean) / z4_sd + 4
  test_data$y <- (test_data$y - mean) / sd + 4
}

model_NW <- lm(y~G+A+G:A+x1+z1+z2+z3+z4, train_data)
summary(model_NW)
anova(model_NW)

predict <- predict(model_NW, test_data)
y_mean = mean(test_data$y)
SSE = sum((test_data$y-predict)^2)
SST = sum((test_data$y-y_mean)^2)
R_2 = 1-SSE/SST
R_2

normalize <- function(x){
  return(scale(x)+4)
}
data[,4:13] <- as.data.frame(lapply(data[,4:13], normalize))
prediction <- matrix(predict(model_NW, data))
prediction <- (prediction - 4)*sd+ mean
setwd("C:/Users/dss/Desktop/Paper/Prediction model Data")
write.csv(prediction, "Gender&age 0-9 NW MLR prediction data.csv", row.names = FALSE)

###

data <- read.csv("C:/Users/dss/Desktop/Paper/Atopic Data/Gender&age 10- NW data.csv")

data$A <- as.factor(data$A)
data$G <- as.factor(data$G)

normalize <- function(x){
  return(scale(x)+4)
}

mean <- mean(data$y)
sd <- sd(data$y)

data[,4:13] <- as.data.frame(lapply(data[,4:13], normalize))

re <- lm(y~G+A+G:A+z1+z2+z3+z4, data)
summary(re)
anova(re)

train_index <- sample(1:length(data[,1]), length(data[,1])*0.8)
train_data <- data[train_index,]
test_data <- data[-train_index,]
model_NW <- lm(y~G+A+G:A+z1+z2+z3+z4, train_data)
summary(model_NW)
anova(model_NW)

predict <- predict(model_NW, test_data)
y_mean = mean(test_data$y)
SSE = sum((test_data$y-predict)^2)
SST = sum((test_data$y-y_mean)^2)
R_2 = 1-SSE/SST
R_2

setwd("C:/Users/dss/Desktop/Paper/Prediction model Data")
write.csv(train_data, file="Gender&age 10- NW MLR train data.csv", row.names = FALSE)
write.csv(test_data, file="Gender&age 10- NW test data.csv", row.names = FALSE)

prediction <- matrix(predict(model_NW, data))
prediction <- (prediction - 4)*sd + mean
write.csv(prediction, "Gender&age 10- NW MLR prediction data.csv", row.names = FALSE)
