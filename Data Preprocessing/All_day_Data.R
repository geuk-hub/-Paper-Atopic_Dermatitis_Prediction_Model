src_dir <- c("C:/Users/dss/Desktop/Paper/Atopic Data")
src_file <- list.files(src_dir)

area_code <- c(11, 26, 27, 28, 29, 30, 31, 36, 41, 42, 43, 44, 45, 46, 47, 48, 49)

data <- NULL
for (i in (1:length(area_code))){
  new <- read.csv(paste0(src_dir, "/", area_code[i],"_day.csv"))
  data <- rbind(data, new)
}

data <- aggregate(data[,2:3], by=list(data$Date), FUN = sum)
column <- c("Date", "Out_cnt", "Population")
names(data) <- column
Incidence_rate <- data$Out_cnt / data$Population
data <- cbind(data, Incidence_rate)

setwd("C:/Users/dss/Desktop/Paper/Atopic Data")
write.csv(data, "All_day_Data.csv", row.names = FALSE)
