library(car)

data <- read.csv("C:/Users/dss/Desktop/Paper/Atopic Data/Regression_rate.csv")

data[,"MW0"] <- data$M0_rate + data$W0_rate
data[,"MW0.5"] <- data$M0.5_rate + data$W0.5_rate
data[,"MW3.5"] <- data$M3.5_rate + data$W3.5_rate
data[,"MW4"] <- data$M4_rate + data$W4_rate
data[,"MW8"] <- data$M8_rate + data$W8_rate

data <- data[,c(1:5,11,12,20:22,28,29,37,49:53,40,42:43,48)]

column <- c("DT", "M", "W", "M0", "M0.5", "M3.5", "M4", "M8", "W0", "W0.5", "W3.5", "W4", "W8", "MW0", "MW0.5", "MW3.5", "MW4", "MW8", "x3", "x5", "z1", "y")
names(data) <- column

normalize <- function(x){
  return(scale(x)+4)
}

mean <- mean(data$y)
sd <- sd(data$y)

data <- cbind(data$DT, as.data.frame(lapply(data[,c(15,17,20:22)], normalize)))
names(data)[1] <- "DT"

# data$y <- (data$y-4)*sd+mean

re <- lm(y~MW0.5+MW4+x5+z1, data)
summary(re)
vif(re)
anova(re)

train_index <- sample(1:length(data[,1]), length(data[,1])*0.8)
train_data <- data[train_index,]
test_data <- data[-train_index,]
model <- lm(y~MW0.5+MW4+x5+z1, train_data)
summary(model)
vif(model)
anova(model)

predict <- predict(model, test_data)
y_mean = mean(test_data$y)
SSE = sum((test_data$y-predict)^2)
SST = sum((test_data$y-y_mean)^2)
R_2 = 1-SSE/SST
R_2

#####
setwd("C:/Users/dss/Desktop/Paper/Prediction model Data")
write.csv(train_data, file="nationwide MLR model train data.csv", row.names = FALSE)
write.csv(test_data, file="nationwide MLR model test data.csv", row.names = FALSE)

prediction <- matrix(predict(model, data))
write.csv(prediction, "nationwide MLR model prediction data.csv", row.names = FALSE)
