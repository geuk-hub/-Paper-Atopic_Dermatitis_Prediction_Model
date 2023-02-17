src_A_dir <- c("C:/Users/dss/Desktop/Paper/Atopic Data")
src_M_dir <- c("C:/Users/dss/Desktop/Paper/Meteorological Data")
src_P_dir <- c("C:/Users/dss/Desktop/Paper/Pollutant Data")

Atopic <- read.csv(paste0(src_A_dir, "/All_month_Data.csv"))
Meteorological <- read.csv(paste0(src_M_dir, "/All_month_M_Data.csv"))
Pollutant <- read.csv(paste0(src_P_dir, "/All_month_P_Data.csv"))

Atopic <- Atopic[,c(1:3,6)]

Pollutant <- Pollutant[which(Pollutant[,"Date"] == 20130101):which(Pollutant[,"Date"] == 20171201),]
Pollutant <- Pollutant[,c(1,4,11,18,25,32)]

for (i in 1:length(Pollutant[,1])){
  for (j in 1:length(Atopic[,1])){
    if (Pollutant[i,1] == Atopic[j,1]){
      Atopic[j,c(5:9)] <- Pollutant[i,c(2:6)]
    }
  }
}

Meteorological <- Meteorological[which(Meteorological[,"Date"] == 20130101):which(Meteorological[,"Date"] == 20171201),]
Meteorological <- Meteorological[,c(1,4,8,12,20)]

for (i in 1:length(Meteorological[,1])){
  for (j in 1:length(Atopic[,1])){
    if (Meteorological[i,1] == Atopic[j,1]){
      Atopic[j,c(10:13)] <- Meteorological[i,c(2:5)]
    }
  }
}

Atopic <- Atopic[,c(1:3,5:13,4)]

setwd(src_A_dir)
write.csv(Atopic, "Regression.csv", row.names = FALSE)

################################################################################################

library(car)

src_A_dir <- c("C:/Users/dss/Desktop/Paper/Atopic Data")
data <- read.csv(paste0(src_A_dir, "/Nationwide gender and age Data.csv"))
column <- c("DT", "A", "M", "x1", "x2", "x3", "x4", "x5", "z1", "z2", "z3", "z4", "y")
names(data) <- column

data$A <- as.factor(data$A)
data$M <- as.factor(data$M)

mean <- mean(data$y)
sd <- sd(data$y)

normalize <- function(x){
  return(scale(x)+4)
}

data <- cbind(data[,1:3], as.data.frame(lapply(data[,4:13], normalize)))

re <- lm(y~A+M+x5+z1+A:x5+A:z1+A:M, data)
summary(re)
vif(re)
anova(re)

train_index <- sample(1:length(data[,1]), length(data[,1])*0.8)
train_data <- data[train_index,]
test_data <- data[-train_index,]
model <- lm(y~M+A+x5+z1+M:A+A:x5+A:z1+A:M, train_data)
summary(model)
vif(model)
anova(model)

predict <- predict(model, test_data)
y_mean = mean(test_data$y)
SSE = sum((test_data$y-predict)^2)
SST = sum((test_data$y-y_mean)^2)
R_2 = 1-SSE/SST
R_2

shapiro.test(train_data$y)

plot(model)
plot(predict,test_data$y)

setwd("C:/Users/dss/Desktop/Paper/Prediction model Data")
write.csv(train_data, file="Nationwide model by gender and age MLR train data.csv", row.names = FALSE)
write.csv(test_data, file="Nationwide model by gender and age MLR test data.csv", row.names = FALSE)

predict(model, data)
prediction <- matrix(predict(model, data))
write.csv(prediction, "Nationwide model by gender and age MLR prediction.csv", row.names = FALSE)

