src_dir <- c("C:/Users/dss/Desktop/Paper/Atopic Data")
src_file <- list.files(src_dir)

src_p_dir <- c("C:/Users/dss/Desktop/Paper/Population Data/Population_Area_Data")
src_p_file <- list.files(src_p_dir)

area_code <- c(11, 26, 27, 28, 29, 30, 31, 36, 41, 42, 43, 44, 45, 46, 47, 48, 49)

for (i in (1:length(area_code))){
  data <- read.csv(paste0(src_dir, "/", area_code[i], "_day.csv"))
  data$DT <- paste0(substr(data$DT,1,4), substr(data$DT,6,7))
  data$DT <- paste0(substr(data$DT,1,4), "-", substr(data$DT,5,6), "-01")
  data$DT <- as.Date(data$DT)
  month_data <- aggregate(data$OUT_CNT, by=list(data$Area, data$DT, data$AGE, data$SEX_TYPE), FUN = sum)
  column <- c("Area", "DT", "AGE", "SEX_TYPE", "OUT_CNT")
  names(month_data) <- column
  month_data <- month_data[order(month_data$SEX_TYPE),]
  month_data <- month_data[order(month_data$DT),]
  
  Population <- read.csv(paste0(src_p_dir, "/Population_", area_code[i], ".csv"))
  
  P_CNT <- NULL
  for (j in 1:length(Population[,1])){
    P_CNT <- c(P_CNT, Population[j,c(6:22)])
    P_CNT <- c(P_CNT, Population[j,c(24:40)])
  }
  P_CNT <- as.numeric(P_CNT)
  month_data <- cbind(month_data, P_CNT)
  Incidence_rate <- month_data$OUT_CNT / month_data$P_CNT
  month_data <- cbind(month_data, Incidence_rate)
  month_data <- month_data[order(month_data$SEX_TYPE),]
  month_data <- month_data[order(month_data$AGE),]
  month_data <- month_data[order(month_data$DT),]
  
  setwd("C:/Users/dss/Desktop/Paper/Atopic Data")
  write.csv(month_data, paste0(area_code[i], "_month.csv"), row.names = FALSE)
}

sum(unique(data$P_CNT))/5
