#전국단위 인구비율 상관관계 분석.


src_dir <- c("C:/Users/dss/Desktop/Paper/Atopic Data")

#성별 비율 상관관계 분석.
data <- read.csv(paste0(src_dir, "/All_month_Data.csv"))
data <- data[,-2]
data <- data[order(data$SEX_TYPE),]
data <- data[order(data$DT),]
rownames(data) <- 1:length(data[,1])

P_CNT <- aggregate(data[,3:4], by=list(data$DT, data$SEX_TYPE), FUN=sum)
incidence_rate <- aggregate(data[,3:4], by=list(data$DT), FUN=sum)
P_CNT <- P_CNT[order(P_CNT$Group.1),]

index <- 1
check <- 1
for(i in 1:(length(P_CNT[,1])/2)){
  incidence_rate[index,'M'] <- P_CNT[check,'P_CNT']
  incidence_rate[index,'W'] <- P_CNT[check+1,'P_CNT']
  index <- index + 1
  check <- check + 2
}

incidence_rate[,'incidence_rate'] <- incidence_rate$OUT_CNT / incidence_rate$P_CNT
incidence_rate[,'M_rate'] <- incidence_rate$M / incidence_rate$P_CNT
incidence_rate[,'W_rate'] <- incidence_rate$W / incidence_rate$P_CNT
cor(incidence_rate[,c(7:8,6)])

# 연령대별 비율 상관관계 분석.

data <- read.csv(paste0(src_dir, "/All_month_Data.csv"))
data <- data[order(data$SEX_TYPE),]
rownames(data) <- 1:length(data[,1])
data_2 <- aggregate(data[,4:5], by=list(data$DT), FUN=sum)
names(data_2) <- c("DT", "OUT_CNT", "P_CNT")

i <- 1
j <- 1
for (k in 1:(length(data[,1])/34)){
  if (data[j,'DT'] == data_2[i,'DT']){
    data_2[i,'M0'] <- data[j,'P_CNT']
    data_2[i,'M0.5'] <- data[j+1,'P_CNT']
    data_2[i,'M1'] <- data[j+2,'P_CNT']
    data_2[i,'M1.5'] <- data[j+3,'P_CNT']
    data_2[i,'M2'] <- data[j+4,'P_CNT']
    data_2[i,'M2.5'] <- data[j+5,'P_CNT']
    data_2[i,'M3'] <- data[j+6,'P_CNT']
    data_2[i,'M3.5'] <- data[j+7,'P_CNT']
    data_2[i,'M4'] <- data[j+8,'P_CNT']
    data_2[i,'M4.5'] <- data[j+9,'P_CNT']
    data_2[i,'M5'] <- data[j+10,'P_CNT']
    data_2[i,'M5.5'] <- data[j+11,'P_CNT']
    data_2[i,'M6'] <- data[j+12,'P_CNT']
    data_2[i,'M6.5'] <- data[j+13,'P_CNT']
    data_2[i,'M7'] <- data[j+14,'P_CNT']
    data_2[i,'M7.5'] <- data[j+15,'P_CNT']
    data_2[i,'M8'] <- data[j+16,'P_CNT']
    i <- i + 1
    j <- j + 17
  }
}

i <- 1
for (k in 1:(length(data[,1])/34)){
  if (data[j,'DT'] == data_2[i,'DT']){
    data_2[i,'W0'] <- data[j,'P_CNT']
    data_2[i,'W0.5'] <- data[j+1,'P_CNT']
    data_2[i,'W1'] <- data[j+2,'P_CNT']
    data_2[i,'W1.5'] <- data[j+3,'P_CNT']
    data_2[i,'W2'] <- data[j+4,'P_CNT']
    data_2[i,'W2.5'] <- data[j+5,'P_CNT']
    data_2[i,'W3'] <- data[j+6,'P_CNT']
    data_2[i,'W3.5'] <- data[j+7,'P_CNT']
    data_2[i,'W4'] <- data[j+8,'P_CNT']
    data_2[i,'W4.5'] <- data[j+9,'P_CNT']
    data_2[i,'W5'] <- data[j+10,'P_CNT']
    data_2[i,'W5.5'] <- data[j+11,'P_CNT']
    data_2[i,'W6'] <- data[j+12,'P_CNT']
    data_2[i,'W6.5'] <- data[j+13,'P_CNT']
    data_2[i,'W7'] <- data[j+14,'P_CNT']
    data_2[i,'W7.5'] <- data[j+15,'P_CNT']
    data_2[i,'W8'] <- data[j+16,'P_CNT']
    i <- i + 1
    j <- j + 17
  }
}

data_2[,'incidence_rate'] <- data_2$OUT_CNT / data_2$P_CNT
data_2[,'M0_rate'] <- data_2$M0 / data_2$P_CNT
data_2[,'M0.5_rate'] <- data_2$M0.5 / data_2$P_CNT
data_2[,'M1_rate'] <- data_2$M1 / data_2$P_CNT
data_2[,'M1.5_rate'] <- data_2$M1.5 / data_2$P_CNT
data_2[,'M2_rate'] <- data_2$M2 / data_2$P_CNT
data_2[,'M2.5_rate'] <- data_2$M2.5 / data_2$P_CNT
data_2[,'M3_rate'] <- data_2$M3 / data_2$P_CNT
data_2[,'M3.5_rate'] <- data_2$M3.5 / data_2$P_CNT
data_2[,'M4_rate'] <- data_2$M4 / data_2$P_CNT
data_2[,'M4.5_rate'] <- data_2$M4.5 / data_2$P_CNT
data_2[,'M5_rate'] <- data_2$M5 / data_2$P_CNT
data_2[,'M5.5_rate'] <- data_2$M5.5 / data_2$P_CNT
data_2[,'M6_rate'] <- data_2$M6 / data_2$P_CNT
data_2[,'M6.5_rate'] <- data_2$M6.5 / data_2$P_CNT
data_2[,'M7_rate'] <- data_2$M7 / data_2$P_CNT
data_2[,'M7.5_rate'] <- data_2$M7.5 / data_2$P_CNT
data_2[,'M8_rate'] <- data_2$M8 / data_2$P_CNT

data_2[,'W0_rate'] <- data_2$W0 / data_2$P_CNT
data_2[,'W0.5_rate'] <- data_2$W0.5 / data_2$P_CNT
data_2[,'W1_rate'] <- data_2$W1 / data_2$P_CNT
data_2[,'W1.5_rate'] <- data_2$W1.5 / data_2$P_CNT
data_2[,'W2_rate'] <- data_2$W2 / data_2$P_CNT
data_2[,'W2.5_rate'] <- data_2$W2.5 / data_2$P_CNT
data_2[,'W3_rate'] <- data_2$W3 / data_2$P_CNT
data_2[,'W3.5_rate'] <- data_2$W3.5 / data_2$P_CNT
data_2[,'W4_rate'] <- data_2$W4 / data_2$P_CNT
data_2[,'W4.5_rate'] <- data_2$W4.5 / data_2$P_CNT
data_2[,'W5_rate'] <- data_2$W5 / data_2$P_CNT
data_2[,'W5.5_rate'] <- data_2$W5.5 / data_2$P_CNT
data_2[,'W6_rate'] <- data_2$W6 / data_2$P_CNT
data_2[,'W6.5_rate'] <- data_2$W6.5 / data_2$P_CNT
data_2[,'W7_rate'] <- data_2$W7 / data_2$P_CNT
data_2[,'W7.5_rate'] <- data_2$W7.5 / data_2$P_CNT
data_2[,'W8_rate'] <- data_2$W8 / data_2$P_CNT

cor(data_2[,c(39:72,38)])


##################################################################################

# 지역단위 인구비율 상관관계 분석.

src_dir <- c("C:/Users/dss/Desktop/Paper/Atopic Data")

#성별 비율 상관관계 분석.
data <- read.csv(paste0(src_dir, "/Area_month_Data.csv"))
data <- data[,-3]
data <- data[order(data$SEX_TYPE),]
data <- data[order(data$DT),]
rownames(data) <- 1:length(data[,1])

P_CNT <- aggregate(data[,4:5], by=list(data$Area, data$DT, data$SEX_TYPE), FUN=sum)
names(P_CNT) <- c("Area", "DT", "SEX_TYPE", "OUT_CNT", "P_CNT")
P_CNT <- P_CNT[order(P_CNT$SEX_TYPE),]
P_CNT <- P_CNT[order(P_CNT$DT),]
P_CNT <- P_CNT[order(P_CNT$Area),]

incidence_rate <- aggregate(data[,4:5], by=list(data$Area, data$DT), FUN=sum)
names(incidence_rate) <- c("Area", "DT", "OUT_CNT", "P_CNT")
incidence_rate <- incidence_rate[order(incidence_rate$DT),]
incidence_rate <- incidence_rate[order(incidence_rate$Area),]

index <- 1
check <- 1
for(i in 1:(length(P_CNT[,1])/2)){
  incidence_rate[index,'M'] <- P_CNT[check,'P_CNT']
  incidence_rate[index,'W'] <- P_CNT[check+1,'P_CNT']
  index <- index + 1
  check <- check + 2
}

incidence_rate[,'incidence_rate'] <- incidence_rate$OUT_CNT / incidence_rate$P_CNT
incidence_rate[,'M_rate'] <- incidence_rate$M / incidence_rate$P_CNT
incidence_rate[,'W_rate'] <- incidence_rate$W / incidence_rate$P_CNT
cor(incidence_rate[,c(8:9,7)])


# 연령대별 비율 상관관계 분석.

data <- read.csv(paste0(src_dir, "/Area_month_Data.csv"))
data <- data[order(data$SEX_TYPE),]
rownames(data) <- 1:length(data[,1])
data_2 <- aggregate(data[,5:6], by=list(data$Area, data$DT), FUN=sum)
names(data_2) <- c("Area", "DT", "OUT_CNT", "P_CNT")
data_2 <- data_2[order(data_2$Area),]
rownames(data_2) <- 1:length(data_2[,1])

i <- 1
j <- 1
for (k in 1:(length(data[,1])/34)){
  if (data[j,'DT'] == data_2[i,'DT']){
    data_2[i,'M0'] <- data[j,'P_CNT']
    data_2[i,'M0.5'] <- data[j+1,'P_CNT']
    data_2[i,'M1'] <- data[j+2,'P_CNT']
    data_2[i,'M1.5'] <- data[j+3,'P_CNT']
    data_2[i,'M2'] <- data[j+4,'P_CNT']
    data_2[i,'M2.5'] <- data[j+5,'P_CNT']
    data_2[i,'M3'] <- data[j+6,'P_CNT']
    data_2[i,'M3.5'] <- data[j+7,'P_CNT']
    data_2[i,'M4'] <- data[j+8,'P_CNT']
    data_2[i,'M4.5'] <- data[j+9,'P_CNT']
    data_2[i,'M5'] <- data[j+10,'P_CNT']
    data_2[i,'M5.5'] <- data[j+11,'P_CNT']
    data_2[i,'M6'] <- data[j+12,'P_CNT']
    data_2[i,'M6.5'] <- data[j+13,'P_CNT']
    data_2[i,'M7'] <- data[j+14,'P_CNT']
    data_2[i,'M7.5'] <- data[j+15,'P_CNT']
    data_2[i,'M8'] <- data[j+16,'P_CNT']
    i <- i + 1
    j <- j + 17
  }
}

i <- 1
for (k in 1:(length(data[,1])/34)){
  if (data[j,'DT'] == data_2[i,'DT']){
    data_2[i,'W0'] <- data[j,'P_CNT']
    data_2[i,'W0.5'] <- data[j+1,'P_CNT']
    data_2[i,'W1'] <- data[j+2,'P_CNT']
    data_2[i,'W1.5'] <- data[j+3,'P_CNT']
    data_2[i,'W2'] <- data[j+4,'P_CNT']
    data_2[i,'W2.5'] <- data[j+5,'P_CNT']
    data_2[i,'W3'] <- data[j+6,'P_CNT']
    data_2[i,'W3.5'] <- data[j+7,'P_CNT']
    data_2[i,'W4'] <- data[j+8,'P_CNT']
    data_2[i,'W4.5'] <- data[j+9,'P_CNT']
    data_2[i,'W5'] <- data[j+10,'P_CNT']
    data_2[i,'W5.5'] <- data[j+11,'P_CNT']
    data_2[i,'W6'] <- data[j+12,'P_CNT']
    data_2[i,'W6.5'] <- data[j+13,'P_CNT']
    data_2[i,'W7'] <- data[j+14,'P_CNT']
    data_2[i,'W7.5'] <- data[j+15,'P_CNT']
    data_2[i,'W8'] <- data[j+16,'P_CNT']
    i <- i + 1
    j <- j + 17
  }
}

data_2[,'incidence_rate'] <- data_2$OUT_CNT / data_2$P_CNT
data_2[,'M0_rate'] <- data_2$M0 / data_2$P_CNT
data_2[,'M0.5_rate'] <- data_2$M0.5 / data_2$P_CNT
data_2[,'M1_rate'] <- data_2$M1 / data_2$P_CNT
data_2[,'M1.5_rate'] <- data_2$M1.5 / data_2$P_CNT
data_2[,'M2_rate'] <- data_2$M2 / data_2$P_CNT
data_2[,'M2.5_rate'] <- data_2$M2.5 / data_2$P_CNT
data_2[,'M3_rate'] <- data_2$M3 / data_2$P_CNT
data_2[,'M3.5_rate'] <- data_2$M3.5 / data_2$P_CNT
data_2[,'M4_rate'] <- data_2$M4 / data_2$P_CNT
data_2[,'M4.5_rate'] <- data_2$M4.5 / data_2$P_CNT
data_2[,'M5_rate'] <- data_2$M5 / data_2$P_CNT
data_2[,'M5.5_rate'] <- data_2$M5.5 / data_2$P_CNT
data_2[,'M6_rate'] <- data_2$M6 / data_2$P_CNT
data_2[,'M6.5_rate'] <- data_2$M6.5 / data_2$P_CNT
data_2[,'M7_rate'] <- data_2$M7 / data_2$P_CNT
data_2[,'M7.5_rate'] <- data_2$M7.5 / data_2$P_CNT
data_2[,'M8_rate'] <- data_2$M8 / data_2$P_CNT

data_2[,'W0_rate'] <- data_2$W0 / data_2$P_CNT
data_2[,'W0.5_rate'] <- data_2$W0.5 / data_2$P_CNT
data_2[,'W1_rate'] <- data_2$W1 / data_2$P_CNT
data_2[,'W1.5_rate'] <- data_2$W1.5 / data_2$P_CNT
data_2[,'W2_rate'] <- data_2$W2 / data_2$P_CNT
data_2[,'W2.5_rate'] <- data_2$W2.5 / data_2$P_CNT
data_2[,'W3_rate'] <- data_2$W3 / data_2$P_CNT
data_2[,'W3.5_rate'] <- data_2$W3.5 / data_2$P_CNT
data_2[,'W4_rate'] <- data_2$W4 / data_2$P_CNT
data_2[,'W4.5_rate'] <- data_2$W4.5 / data_2$P_CNT
data_2[,'W5_rate'] <- data_2$W5 / data_2$P_CNT
data_2[,'W5.5_rate'] <- data_2$W5.5 / data_2$P_CNT
data_2[,'W6_rate'] <- data_2$W6 / data_2$P_CNT
data_2[,'W6.5_rate'] <- data_2$W6.5 / data_2$P_CNT
data_2[,'W7_rate'] <- data_2$W7 / data_2$P_CNT
data_2[,'W7.5_rate'] <- data_2$W7.5 / data_2$P_CNT
data_2[,'W8_rate'] <- data_2$W8 / data_2$P_CNT

cor(data_2[,c(40:73,39)][25:35])
