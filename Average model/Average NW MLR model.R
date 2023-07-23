data <- read.csv("C:/Users/dss/Desktop/Paper/Atopic Data/Average NW data.csv")

{
mean <- mean(data$y)
sd <- sd(data$y)

M_mean <- mean(data$M)
MW20_mean <- mean(data$MW20)
MW60_mean <- mean(data$MW60)

M_sd <- sd(data$M)
MW20_sd <- sd(data$MW20)
MW60_sd <- sd(data$MW60)

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

train_data <- read.csv("C:/Users/dss/Desktop/Paper/Prediction model Data/AVG NW train data.csv")
valid_data <- read.csv("C:/Users/dss/Desktop/Paper/Prediction model Data/AVG NW valid data.csv")
test_data <- read.csv("C:/Users/dss/Desktop/Paper/Prediction model Data/AVG NW test data.csv")

train_data <- rbind(train_data, valid_data)
{
train_data$M <- (train_data$M - M_mean) / M_sd + 4
train_data$MW20 <- (train_data$MW20 - MW20_mean) / MW20_sd + 4
train_data$MW60 <- (train_data$MW60 - MW60_mean) / MW60_sd + 4
train_data$x1 <- (train_data$x1 - x1_mean) / x1_sd + 4
train_data$z1 <- (train_data$z1 - z1_mean) / z1_sd + 4
train_data$z2 <- (train_data$z2 - z2_mean) / z2_sd + 4
train_data$z3 <- (train_data$z3 - z3_mean) / z3_sd + 4
train_data$z4 <- (train_data$z4 - z4_mean) / z4_sd + 4
train_data$y <- (train_data$y - mean) / sd + 4

test_data$M <- (test_data$M - M_mean) / M_sd + 4
test_data$MW20 <- (test_data$MW20 - MW20_mean) / MW20_sd + 4
test_data$MW60 <- (test_data$MW60 - MW60_mean) / MW60_sd + 4
test_data$x1 <- (test_data$x1 - x1_mean) / x1_sd + 4
test_data$z1 <- (test_data$z1 - z1_mean) / z1_sd + 4
test_data$z2 <- (test_data$z2 - z2_mean) / z2_sd + 4
test_data$z3 <- (test_data$z3 - z3_mean) / z3_sd + 4
test_data$z4 <- (test_data$z4 - z4_mean) / z4_sd + 4
test_data$y <- (test_data$y - mean) / sd + 4
}

##
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

normalize <- function(x){
  return(scale(x)+4)
}
data[,2:17] <- as.data.frame(lapply(data[,2:17], normalize))
prediction <- matrix(predict(model, data))
prediction <- (prediction - 4)*sd + mean
write.csv(prediction, "AVG NW MLR prediction data.csv", row.names = FALSE)
