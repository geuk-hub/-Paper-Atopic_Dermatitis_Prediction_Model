library(car)

data <- read.csv("C:/Users/dss/Desktop/Paper/Atopic Data/Regression_rate.csv")

data[,"MW0"] <- data$M0_rate + data$W0_rate
data[,"MW6"] <- data$M6_rate + data$W6_rate
data[,"MW6.5"] <- data$M6.5_rate + data$W6.5_rate

data <- data[,c(1,4,7,36,49:51,38:45,47:48)]


column <- c("DT", "M0", "M1.5", "W7.5", "MW0", "MW6", "MW6.5", "x1", "x2", "x3", "x4", "x5", "z1", "z2", "z3", "z4", "y")
names(data) <- column

data$M0 <- scale(data$M0) + 4
data$M1.5 <- scale(data$M1.5) + 4
data$W7.5 <- scale(data$W7.5) + 4
data$MW0 <- scale(data$MW0) + 4
data$MW6 <- scale(data$MW6) + 4
data$MW6.5 <- scale(data$MW6.5) + 4
data$x1 <- scale(data$x1) + 4
data$x2 <- scale(data$x2) + 4
data$x3 <- scale(data$x3) + 4
data$x4 <- scale(data$x4) + 4
data$x5 <- scale(data$x5) + 4
data$z1 <- scale(data$z1) + 4
data$z2 <- scale(data$z2) + 4
data$z3 <- scale(data$z3) + 4
data$z4 <- scale(data$z4) + 4
data$y <- scale(data$y) + 4

mean <- mean(data$y)
sd <- sd(data$y)
data$y <- (data$y-mean)/sd + 4

data$y <- (data$y-4)*sd+mean

re <- lm(y~MW0+x3+z2, data)
summary(re)
vif(re)
anova(re)

train_index <- sample(1:length(data[,1]), 42)
train_data <- data[train_index,]
test_data <- data[-train_index,]
model <- lm(y~MW0+x5+z1, train_data)
summary(model)
vif(model)
anova(model)

predict <- predict(model, test_data)
y_mean = mean(test_data$y)
SSE = sum((test_data$y-predict)^2)
SST = sum((test_data$y-y_mean)^2)
R_2 = 1-SSE/SST
R_2

setwd("C:/Users/dss/Desktop/Paper/Atopic Data")
write.csv(train_data, file="train data_rate.csv", row.names = FALSE)
write.csv(test_data, file="test data_rate.csv", row.names = FALSE)

prediction <- matrix(predict(model, data))
write.csv(prediction, "prediction_rate.csv", row.names = FALSE)
