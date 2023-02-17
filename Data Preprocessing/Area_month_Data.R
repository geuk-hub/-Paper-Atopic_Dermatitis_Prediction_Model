src_dir <- c("C:/Users/dss/Desktop/Paper/Atopic Data")
src_file <- list.files(src_dir)

area_code <- c(11, 26, 27, 28, 29, 30, 31, 36, 41, 42, 43, 44, 45, 46, 47, 48, 49)

for (i in (1:length(area_code))){
  data <- read.csv(paste0(src_dir, "/", src_file[i]))
  data$Date <- paste0(substr(data$Date,1,4), substr(data$Date,6,7))
  data$Date <- paste0(substr(data$Date,1,4), "-", substr(data$Date,5,6), "-01")
  data$Date <- as.Date(data$Date)
  month_data <- aggregate(data$OUT_CNT, by=list(data$Date, data$Population), FUN = sum)
  column <- c("Date", "Population", "Out_cnt")
  names(month_data) <- column
  month_data <- month_data[,c(1,3,2)]
  Incidence_rate <- month_data$Out_cnt / month_data$Population
  month_data <- cbind(month_data, Incidence_rate)
  month_data <- month_data[order(month_data$Date),]
  setwd("C:/Users/dss/Desktop/Paper/Atopic Data")
  write.csv(month_data, paste0(area_code[i], "_month.csv"), row.names = FALSE)
}