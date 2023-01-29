src_dir <- c("C:/Users/dss/Desktop/Paper/Data/Atopic_All_Data")
src_file <- list.files(src_dir)

src_p_dir <- c("C:/Users/dss/Desktop/Paper/Population Data/Population_Area_Data")
src_p_file <- list.files(src_p_dir)

area_code <- c(11, 26, 27, 28, 29, 30, 31, 36, 41, 42, 43, 44, 45, 46, 47, 48, 49) 
check <- 0
data <- NULL
index <- 1 

for (i in 1:length(src_file)){
  new <- read.csv(paste0(src_dir, "/", src_file[i]))
  data <- rbind(data, new)
  check <- check + 1
  if (check == 5){
    data$DT <- paste0(substr(data$DT,1,4), "-", substr(data$DT,5,6), "-", substr(data$DT,7,8))
    data$DT <- as.Date(data$DT)
    data <- data[order(data$SEX_TYPE),]
    data <- data[order(data$DT),]
    data[,'Area'] <- area_code[index]
    data <- data[,c(6,1:5)]
    
    Population <- read.csv(paste0(src_p_dir, "/", src_p_file[index]))
    
    P_CNT <- NULL
    for (j in 1:length(data[,1])){
      for (k in 1:length(Population[,1])){
        if (substr(data[j,2],1,4) == Population[k,1]){
          if (as.numeric(substr(data[j,2],6,7)) == Population[k,2]){
            if (data[j,4] == 0 & data[j,5] == 1){
              P_CNT <- c(P_CNT, Population[k,c(6:22)])
              P_CNT <- c(P_CNT, Population[k,c(24:40)])
            }
          }
        } 
      }
    }
    
    P_CNT <- as.numeric(P_CNT)
    
    data <- cbind(data, P_CNT)
    Incidence_rate <- data$OUT_CNT / data$P_CNT
    data <- cbind(data, Incidence_rate)
    data <- data[order(data$SEX_TYPE),]
    data <- data[order(data$AGE),]
    data <- data[order(data$DT),]
    
    setwd("C:/Users/dss/Desktop/Paper/Atopic Data")    
    write.csv(data, paste0(area_code[index], "_day.csv"), row.names = FALSE)    
    data <- NULL
    check <- 0
    index <- index + 1
  }
}

