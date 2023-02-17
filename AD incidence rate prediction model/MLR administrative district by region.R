library(car)

data <- read.csv("C:/Users/dss/Desktop/Paper/Atopic Data/Administrative district Data.csv")

data[,"MW0.5"] <- data$M0.5_rate + data$W0.5_rate
data[,"MW4"] <- data$M4_rate + data$W4_rate

data <- data[,c(1:2,49:50,43:44,48)]

column <- c("Area", "DT", "MW0.5", "MW4", "x5", "z1", "y")
names(data) <- column

normalize <- function(x){
  return(scale(x)+4)
}

mean <- mean(data$y)
sd <- sd(data$y)

data[,3:7] <- as.data.frame(lapply(data[,3:7], normalize))
data <- data[-which(data[,'Area'] == 36),]

data$Area <- as.factor(data$Area)

# data$y <- (data$y-4)*sd+mean

re <- lm(y~Area+MW0.5+MW4+x5+z1, data)
summary(re)
vif(re)
anova(re)

train_index <- sample(1:length(data[,1]), length(data[,1])*0.8)
train_data <- data[train_index,]
test_data <- data[-train_index,]
model <- lm(y~Area+MW0.5+MW4+x5+z1, train_data)
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
write.csv(train_data, file="Administrative district model by region MLR train data.csv", row.names = FALSE)
write.csv(test_data, file="Administrative district model by region MLR test data.csv", row.names = FALSE)

prediction <- matrix(predict(model, data))
write.csv(prediction, "Administrative district model by region MLR prediction data.csv", row.names = FALSE)
