### Description
# Transforming LGM counts into relative data
# creates: counts_relative_lgm.csv

### Usage
# get_abund_relat_glm(forcens_df, overwrite = TRUE/FALSE)

### Arguments
# overwrite : TRUE re-run anyway, FALSE do not re-run if file already exist
# (reads "data/raw_data_counts/counts_lgm.csv")

get_abund_relat_lgm <- function(overwrite){ 
  
  if(file.exists("data/counts_relative_lgm.csv") && overwrite == FALSE){
    
    raw_counts <- read.csv("data/counts_relative_lgm.csv", header = TRUE)
    return(raw_counts)  
    
    
    
  }else{
    
    raw_counts <- read.csv("data/raw_data_counts/counts_lgm.csv", header = TRUE, stringsAsFactors = FALSE)
    raw_counts[is.na(raw_counts)] <- 0
    
    for(i in c(5,10,20,25,27,31,44,46,55,66)){ # i = 55
      
      raw_counts <- raw_counts[-37,] # removing old "total_counts"
      
      # Marina re-samples (OBD Collection)
      A_total_counts <- sum(raw_counts[, sprintf("X%sA",i)])
      raw_counts[sprintf("X%sA",i)] <- raw_counts[sprintf("X%sA",i)]/A_total_counts
      # ForCenS neighbours
      C_total_counts <- sum(raw_counts[, sprintf("X%sC",i)])
      raw_counts[sprintf("X%sC",i)] <- raw_counts[sprintf("X%sC",i)]/C_total_counts
      # LGM neighbours
      L_total_counts <- sum(raw_counts[, sprintf("X%sL",i)])
      raw_counts[sprintf("X%sL",i)] <- raw_counts[sprintf("X%sL",i)]/L_total_counts

    }
    
    if(sum(colSums(raw_counts[,2:31])) != ncol(raw_counts) - 1){
      print("problem: check column sum")
    }
    
    write.csv(raw_counts, file = "data/counts_relative_lgm.csv",row.names=FALSE)
    
    return(raw_counts)  
    
  } # else
}
