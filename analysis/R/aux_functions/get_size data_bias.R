### Description
# Arranges raw data (morpho_bias_buckley - measurements for the bias analysis) for R analysis (morpho_R)

### Arguments
# none, but requires files in folder morpho_bias_buckley

get_size_data_bias <- function(){
  
  files.names <- list.files(path = "data/raw_data/morpho_bias_buckley/", pattern="*.csv")
  check.col2 <- c()
  check.col4 <- c()
  
  for (k in files.names){ # k <- files.names[15]
    raw.data <- read.csv(paste("data/raw_data/morpho_bias_buckley/",k, sep=""),header = TRUE, stringsAsFactors=FALSE)
    rows <- which(!is.na(str_extract(raw.data[,1],pattern=c("P1R")))) # rows with actual measurements
    data <- data.frame(area=raw.data[rows,2], diam.max=raw.data[rows,4])
    check.col2 <- c(check.col2, raw.data[1,2])
    check.col4 <- c(check.col4, raw.data[1,4])
    write.csv(data, file = paste("data/raw_data/morpho_R/",k,sep=""),row.names=FALSE)
    rm(rows)
    rm(data)
  }
  #print(unique(check.col2)) # checks if used correct column (Area)
  #print(unique(check.col4)) # checks if used correct column (Diameter)
  
}



