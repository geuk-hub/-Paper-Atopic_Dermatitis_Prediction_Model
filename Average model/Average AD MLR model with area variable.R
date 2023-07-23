data <- read.csv("C:/Users/dss/Desktop/Paper/Atopic Data/Average AD data.csv")

normalize <- function(x){
  return(scale(x)+4)
}

mean <- mean(data$y)
sd <- sd(data$y)

x1_sd <- sd(data$x1)
x2_sd <- sd(data$x2)
x3_sd <- sd(data$x3)
x4_sd <- sd(data$x4)
x5_sd <- sd(data$x5)

data[,3:18] <- as.data.frame(lapply(data[,c(3:18)], normalize))

data$Area <- as.factor(data$Area)

re <- lm(y~Area+(MW20+MW60)*M+x1+z1+z2+z3+z4, data)
summary(re)
anova(re)

train_index <- sample(1:length(data[,1]), length(data[,1])*0.8)
train_data <- data[train_index,]
test_data <- data[-train_index,]
model <- lm(y~Area+(MW20+MW60)*M+x1+z1+z2+z3+z4, train_data)
summary(model)
anova(model)

predict <- predict(model, test_data)
y_mean = mean(test_data$y)
SSE = sum((test_data$y-predict)^2)
SST = sum((test_data$y-y_mean)^2)
R_2 = 1-SSE/SST
R_2

#####
setwd("C:/Users/dss/Desktop/Paper/Prediction model Data")
write.csv(train_data, file="AVG AD MLR train data.csv", row.names = FALSE)
write.csv(test_data, file="AVG AD test data.csv", row.names = FALSE)

prediction <- matrix(predict(model, data))
prediction <- (prediction - 4)*sd + mean
write.csv(prediction, "AVG AD MLR with area prediction data.csv", row.names = FALSE)
