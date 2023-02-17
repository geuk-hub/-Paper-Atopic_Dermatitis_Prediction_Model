src_dir <- c("C:/Users/dss/Desktop/Paper/Atopic Data")

data <- read.csv(paste0(src_dir, "/All_day_Data.csv"))
data$Date <- paste0(substr(data$Date,1,4), substr(data$Date,6,7))
data$Date <- paste0(substr(data$Date,1,4), "-", substr(data$Date,5,6), "-01")
data$Date <- as.Date(data$Date)
month_data <- aggregate(data$Out_cnt, by=list(data$Date, data$Population), FUN = sum)
column <- c("Date", "Population", "Out_cnt")
names(month_data) <- column
month_data <- month_data[,c(1,3,2)]
Incidence_rate <- month_data$Out_cnt / month_data$Population
month_data <- cbind(month_data, Incidence_rate)
month_data <- month_data[order(month_data$Date),]
setwd("C:/Users/dss/Desktop/Paper/Atopic Data")
write.csv(month_data, "All_month_Data.csv", row.names = FALSE)
