### Description
# Gets size data from Buckley Colletion and bias measurements for bias size analysis
# Creates folder "data/bias_size_analysis/"
# Buckley Collection: subsets measurements of size-distrib-forams used for bias analysis (10 samples)
# bias_measurements: arranges raw data from bias_measurements folder for the bias size analysis

### Arguments
# buckley_measurmts: morphometric dataset from size-distrib-forams project (morpho_all_ssp.csv)
# resamp_morpho_df: re-samples information creates by fucntion get_resamples_info.R
# overwrite : TRUE or FALSE (if the output file already exist, do you want to re-run the function and overwrite it?)

get_size_data <- function(buckley_measurmts, resamp_morpho_df, overwrite){
  
  if(!file.exists("data/bias_size_analysis/") | overwrite == TRUE){
    
    if (!file.exists("data/bias_size_analysis")){
      dir.create("data/bias_size_analysis")
    }
    
    ### Getting size data from Buckley Collection
    
    for (j in unique(resamp_morpho_df$sample)){ # j = resamp_morpho_df$sample[3]
      for (i in unique(buckley_measurmts$species)){ # i = unique(buckley_measurmts$species)[11]
        rows <- intersect(which(buckley_measurmts["sample"]==j), which(buckley_measurmts["species"]==i))
        if (length(rows) != 0){
          data <- data.frame(area=buckley_measurmts[rows,"ind_area"], diam.max=buckley_measurmts[rows,"ind_diam"])
          write.csv(data, file = paste("data/bias_size_analysis/",i,j,"B.csv",sep=""),row.names=FALSE)
          rm(data)
        }
        rm(rows)
      }
    }
    
    ### Getting size data from bias_measurements
    
    files.names <- list.files(path = "data/bias_measurements/", pattern="*.csv")
    check.col2 <- c()
    check.col4 <- c()
    
    for (k in files.names){ # k <- files.names[15]
      # Read file
      raw.data <- read.csv(paste("data/bias_measurements/",k, sep=""),header = TRUE, stringsAsFactors=FALSE)
      # Extract measurement data from file (area and diameter)
      rows <- which(!is.na(str_extract(raw.data[,1],pattern=c("P1R")))) # rows with actual measurements
      data <- data.frame(area=raw.data[rows,2], diam.max=raw.data[rows,4])
      
      # Check column names of raw data
      check.col2 <- c(check.col2, raw.data[1,2])
      check.col4 <- c(check.col4, raw.data[1,4])
      
      write.csv(data, file = paste("data/bias_size_analysis/",k,sep=""),row.names=FALSE)
      rm(rows)
      rm(data)
    }
    
    # Chech column names 
    # print(unique(check.col2)) # checks if used correct column (Area)
    # print(unique(check.col4)) # checks if used correct column (Diameter)
    
    
  } # if (overwrite)   
}

