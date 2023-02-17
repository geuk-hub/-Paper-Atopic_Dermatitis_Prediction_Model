library(car)

src_A_dir <- c("C:/Users/dss/Desktop/Paper/Atopic Data")
data <- read.csv(paste0(src_A_dir, "/Administrative district gender and age Data.csv"))
column <- c("Area", "DT", "M", "A", "x5", "z1", "y")
names(data) <- column

data$Area <- as.factor(data$Area)
data$M <- as.factor(data$M)
data$A <- as.factor(data$A)

mean <- mean(data$y)
sd <- sd(data$y)
mean
sd

normalize <- function(x){
  return(scale(x)+4)
}

data <- cbind(data[,1:4], as.data.frame(lapply(data[,5:7], normalize)))

re <- lm(y~Area+M+A+x5+z1+M:A+x5:A+z1:A+Area:A, data)
summary(re)
vif(re)
anova(re)

train_index <- sample(1:length(data[,1]), length(data[,1])*0.8)
train_data <- data[train_index,]
test_data <- data[-train_index,]
model <- lm(y~Area+M+A+x5+z1+M:A+A:x5+A:z1+Area:A, train_data)
options(max.print=3000)
summary(model)
vif(model)
anova(model)

predict <- predict(model, test_data)
y_mean = mean(test_data$y)
SSE = sum((test_data$y-predict)^2)
SST = sum((test_data$y-y_mean)^2)
R_2 = 1-SSE/SST
R_2

setwd("C:/Users/dss/Desktop/Paper/Prediction model Data")
write.csv(train_data, file="Administrative district model by region and gender and age MLR train data.csv", row.names = FALSE)
write.csv(test_data, file="Administrative district model by region and gender and age MLR test data.csv", row.names = FALSE)

prediction <- matrix(predict(model, data))
write.csv(prediction, "Administrative district model by region and gender and age MLR prediction data.csv", row.names = FALSE)

prediction <- (prediction - 4)*sd + mean
which(prediction < 0)
prediction[28578]
