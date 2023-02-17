src_dir <- c("C:/Users/dss/Desktop/Paper/Atopic_Area_Age_Data")
src_p <- c("C:/Users/dss/Desktop/Paper/Population Data/Population_Area_Data_80")
src_file <- list.files(src_dir)
src_p_file <- list.files(src_p)

area_code <- c(11, 26, 27, 28, 29, 30, 31, 36, 41, 42, 43, 44, 45, 46, 47, 48, 49)
column <- c('Date', 'OUT_CNT')

for (i in (1:length(src_file))){
  data <- read.csv(paste0(src_dir, "/", src_file[i]))
  data <- aggregate(data[,'OUT_CNT'], by = list(data$DT), FUN = sum)
  names(data) <- column
  population <- read.csv(paste0(src_p, "/", src_p_file[i]))
  setwd("C:/Users/dss/Desktop/Paper/Atopic Data")
  write.csv(data, paste0(area_code[i], "_day.csv"), row.names = FALSE)
}
