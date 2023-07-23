data <- read.csv("C:/Users/dss/Desktop/Paper/Atopic Data/Average AD data.csv")
mean <- mean(data$y)
sd <- sd(data$y)
normalize <- function(x){
  return(scale(x)+4)
}
data[,3:18] <- as.data.frame(lapply(data[,c(3:18)], normalize))

train_data <- read.csv("C:/Users/dss/Desktop/Paper/Prediction model Data/AVG AD MLR train data.csv")
test_data <- read.csv("C:/Users/dss/Desktop/Paper/Prediction model Data/AVG AD test data.csv")

model <- lm(y~(MW20+MW60)*M+x1+z1+z2+z3+z4, train_data)
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

prediction <- matrix(predict(model, data))
prediction <- (prediction - 4)*sd + mean
write.csv(prediction, "AVG AD MLR without area prediction data.csv", row.names = FALSE)
