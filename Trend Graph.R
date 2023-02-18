src_M_dir <- c("C:/Users/dss/Desktop/Paper/Meteorological Data")
src_P_dir <- c("C:/Users/dss/Desktop/Paper/Pollutant Data")

Meteorological <- read.csv(paste0(src_M_dir, "/All_month_M_Data.csv"))
Pollutant <- read.csv(paste0(src_P_dir, "/All_month_P_Data.csv"))

Meteorological <- Meteorological[which(Meteorological[,"Date"] == 20130101):which(Meteorological[,"Date"] == 20171201),]
Meteorological <- Meteorological[,c(1,4,8,12,16,20)]

Pollutant <- Pollutant[which(Pollutant[,"Date"] == 20130101):which(Pollutant[,"Date"] == 20171201),]
Pollutant <- Pollutant[,c(1,4,11,18,25,32)]

Meteorological$MS_Date <- as.Date(Meteorological$MS_Date)
Pollutant$MS_Date <- as.Date(Pollutant$MS_Date)

setwd(src_M_dir)
write.csv(Meteorological, file = "Meteorological.csv", row.names = FALSE)
setwd(src_P_dir)
write.csv(Pollutant, file = "Pollutant.csv", row.names = FALSE)

library(ggplot2)
library(gridExtra)

## expression(paste("degree (",degree,")"))

Temperature <- ggplot(Meteorological, aes(x=MS_Date, y=Temperature)) + geom_line(color="blue") + geom_point(size=2, colour="blue") + theme_bw() + theme(axis.title.x=element_blank()) + scale_x_date(date_labels= '20%y', date_breaks= '1 year')
Humidity <- ggplot(Meteorological, aes(x=MS_Date, y=Humidity)) + geom_line(color="red") + geom_point(size=2, colour="red") + theme_bw() + theme(axis.title.x=element_blank()) + scale_x_date(date_labels= '20%y', date_breaks= '1 year')
Wind_Speed <- ggplot(Meteorological, aes(x=MS_Date, y=Wind_Speed)) + geom_line(color="#569b4a") + geom_point(size=2, colour="#569b4a") + theme_bw() + theme(axis.title.x=element_blank()) + scale_x_date(date_labels= '20%y', date_breaks= '1 year')
Atmospheric_Pressure <- ggplot(Meteorological, aes(x=MS_Date, y=Atmospheric_Pressure)) + geom_line(color="#edae49") + geom_point(size=2, colour="#edae49") + theme_bw() + theme(axis.title.x=element_blank()) + scale_x_date(date_labels= '20%y', date_breaks= '1 year')
Precipitation <- ggplot(Meteorological, aes(x=MS_Date, y=Precipitation)) + geom_line(color="gray45") + geom_point(size=2, colour="gray45") + xlab("Year") + theme_bw() + scale_x_date(date_labels= '20%y', date_breaks= '1 year')
grid.arrange(Temperature, Humidity, Wind_Speed, Precipitation, nrow=4, ncol=1)

SO2 <- ggplot(Pollutant, aes(x=MS_Date, y=SO2)) + geom_line(color="blue") + geom_point(size=2, colour="blue") + theme_bw() + theme(axis.title.x=element_blank()) + scale_x_date(date_labels= '20%y', date_breaks= '1 year')
CO <- ggplot(Pollutant, aes(x=MS_Date, y=CO)) + geom_line(color="red") + geom_point(size=2, colour="red") + theme_bw() + theme(axis.title.x=element_blank()) + scale_x_date(date_labels= '20%y', date_breaks= '1 year')
O3 <- ggplot(Pollutant, aes(x=MS_Date, y=O3)) + geom_line(color="#569b4a") + geom_point(size=2, colour="#569b4a") + theme_bw() + theme(axis.title.x=element_blank()) + scale_x_date(date_labels= '20%y', date_breaks= '1 year')
NO2 <- ggplot(Pollutant, aes(x=MS_Date, y=NO2)) + geom_line(color="#edae49") + geom_point(size=2, colour="#edae49") + theme_bw() + theme(axis.title.x=element_blank()) + scale_x_date(date_labels= '20%y', date_breaks= '1 year')
PM10 <- ggplot(Pollutant, aes(x=MS_Date, y=PM10)) + geom_line(color="gray45") + geom_point(size=2, colour="gray45") + xlab("Year") + theme_bw() + scale_x_date(date_labels= '20%y', date_breaks= '1 year')
grid.arrange(SO2, CO, O3, NO2, PM10, nrow=5, ncol=1)
