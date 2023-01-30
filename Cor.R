src_A_dir <- c("C:/Users/dss/Desktop/Paper/Data/Atopic Data")
src_M_dir <- c("C:/Users/dss/Desktop/Paper/Meteorological Data")
src_P_dir <- c("C:/Users/dss/Desktop/Paper/Pollutant Data")

Atopic <- read.csv(paste0(src_A_dir, "/All_month_Data.csv"))
Meteorological <- read.csv(paste0(src_M_dir, "/All_month_M_Data.csv"))
Pollutant <- read.csv(paste0(src_P_dir, "/All_month_P_Data.csv"))

Atopic <- Atopic[,c(1,4)]

Meteorological <- Meteorological[which(Meteorological[,"Date"] == 20130101):which(Meteorological[,"Date"] == 20171201),]
Meteorological <- Meteorological[,c(4,8,12,20)]

Pollutant <- Pollutant[which(Pollutant[,"Date"] == 20130101):which(Pollutant[,"Date"] == 20171201),]
Pollutant <- Pollutant[,c(4,11,18,25,32)]

data <- cbind(Atopic, Pollutant, Meteorological)
data <- data[,c(1,3:11,2)]

# 전국단위 대기오염물질, 기상요인, 아토피피부염 발병률간의 상관관계 분석.

cor(data[,c(2:11)])


# O3를 제외한 대기오염물질과 발병률이 음의 상관관계를 가지고 있으므로 추가적인 분석 필요. 
# 년도별 지역별로 끊어서 상관관계 분석.

Atopic <- read.csv(paste0(src_A_dir, "/Area_month_Data.csv"))
Atopic <- Atopic[order(Atopic$DT),]
Atopic <- aggregate(Atopic[,5:6], by=list(Atopic$Area, Atopic$DT), FUN = sum)
names(Atopic) <- c("Area", "DT", "OUT_CNT", "P_CNT")
Atopic[,'incidence_rate'] <-Atopic$OUT_CNT / Atopic$P_CNT

Pollutant <- read.csv(paste0(src_P_dir, "/Area_month_P_Data.csv"))
Pollutant <- Pollutant[order(Pollutant$Area),]
Pollutant <- Pollutant[order(Pollutant$Date),]
Pollutant <- Pollutant[which(Pollutant[,"Date"] == 20130101)[1]:which(Pollutant[,"Date"] == 20171201)[length(which(Pollutant[,"Date"] == 20171201))],]
Pollutant <- Pollutant[,c(1,2,5,12,19,26,33)]
rownames(Pollutant) <- 1:length(Pollutant[,1])

Meteorological <- read.csv(paste0(src_M_dir, "/Area_month_M_Data.csv"))
Meteorological <- Meteorological[order(Meteorological$Area),]
Meteorological <- Meteorological[order(Meteorological$Date),]
Meteorological <- Meteorological[which(Meteorological[,"Date"] == 20130101)[1]:which(Meteorological[,"Date"] == 20171201)[length(which(Meteorological[,"Date"] == 20171201))],]
Meteorological <- Meteorological[,c(1,2,5,32,15,10)]
rownames(Meteorological) <- 1:length(Meteorological[,1])


for (i in 1:length(Pollutant[,1])){
  for (j in 1:length(Atopic[,1])){
    if (Pollutant[i,1] == Atopic[j,1]){
      if (Pollutant[i,2] == Atopic[j,2]){
        Atopic[j,c(6:10)] <- Pollutant[i,c(3:7)]
      }
    }
  }
}

for (i in 1:length(Meteorological[,1])){
  for (j in 1:length(Atopic[,1])){
    if (Meteorological[i,1] == Atopic[j,1]){
      if (Meteorological[i,2] == Atopic[j,2]){
        Atopic[j,c(11:14)] <- Meteorological[i,c(3:6)]
      }
    }
  }
}


Atopic$DT <- substr(Atopic$DT, 1,4)
Atopic <- na.omit(Atopic)
rownames(Atopic) <- 1:length(Atopic[,1])

index <- 1
for (i in 1:length(Atopic[,1])){
  if (i == length(Atopic[,1])){
    index <- c(index, i)
    break
  }
  if (Atopic[i,'DT'] == Atopic[i+1,'DT']){
  }
  else{
    index <- c(index, i)
  }
}

index
data_2 <- Atopic[,c(6:14,5)]

cor(data_2[1:192,])
cor(data_2[193:384,])
cor(data_2[385:576,])
cor(data_2[577:768,])
cor(data_2[769:960,])


# 년도별 상관관계 분석.

data_2013 <- data[which(data[,'Date'] == '2013-01-01'):which(data[,'Date'] == '2013-12-01'),]
cor(data_2013[,c(2:length(data_2013))])

data_2014 <- data[which(data[,'Date'] == '2014-01-01'):which(data[,'Date'] == '2014-12-01'),]
cor(data_2014[,c(2:length(data_2014))])

data_2015 <- data[which(data[,'Date'] == '2015-01-01'):which(data[,'Date'] == '2015-12-01'),]
cor(data_2015[,c(2:length(data_2015))])

data_2016 <- data[which(data[,'Date'] == '2016-01-01'):which(data[,'Date'] == '2016-12-01'),]
cor(data_2016[,c(2:length(data_2016))])

data_2017 <- data[which(data[,'Date'] == '2017-01-01'):which(data[,'Date'] == '2017-12-01'),]
cor(data_2017[,c(2:length(data_2017))])


# 지역별 상관관계 분석.

Atopic <- read.csv(paste0(src_A_dir, "/49_month.csv"))
Meteorological <- read.csv(paste0(src_M_dir, "/49_month.csv"))
Pollutant <- read.csv(paste0(src_P_dir, "/49_month.csv"))

Atopic <- Atopic[,c(1,4)]

Meteorological <- Meteorological[which(Meteorological[,"Date"] == 20130101):which(Meteorological[,"Date"] == 20171201),]
Meteorological <- Meteorological[,c(5,9,13,21)]

Pollutant <- Pollutant[which(Pollutant[,"Date"] == 20130101):which(Pollutant[,"Date"] == 20171201),]
Pollutant <- Pollutant[,c(5,12,19,26,33)]

data <- cbind(Atopic, Pollutant, Meteorological)
data <- data[,c(3:11,2)]
cor(data[,])

# 세종특별자치시

Atopic <- read.csv(paste0(src_A_dir, "/36_month.csv"))
Meteorological <- read.csv(paste0(src_M_dir, "/36_month.csv"))
Pollutant <- read.csv(paste0(src_P_dir, "/36_month.csv"))

Atopic <- Atopic[which(Atopic[,"Date"] == "2016-01-01"):which(Atopic[,"Date"] == "2017-12-01"),c(1,4)]

Pollutant <- Pollutant[1:which(Pollutant[,"Date"] == 20171201),]
Pollutant <- Pollutant[,c(5,12,19,26,33,40)]

data <- cbind(Atopic, Pollutant)
data <- data[,c(1,3:8,2)]
cor(data[,c(2:6,8)])
