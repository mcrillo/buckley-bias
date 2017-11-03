### Description
# 
# creates: 

### Usage
# get_abund_relat(forcens_df)

### Arguments
# forcens_df


get_abund_relat <- function(forcens_df, overwrite){ 
  
  if(file.exists("data/counts_relative.csv") && overwrite == FALSE){
    
    raw_counts <- read.csv("data/counts_relative.csv", header = TRUE)
    return(raw_counts)  
    
  }else{
  
      raw_counts <- read.csv("data/raw_data/counts_raw_R.csv", header = TRUE, stringsAsFactors = FALSE)
    
      colnames(raw_counts) <- c("species","5A","5B","5C","10A","10B","10C","20A","20B","20C","25A","25B","25C","27A","27B","27C",	
                                "31A","31B","31C","44A","44B","44C","46A","46B","46C","55A","55B","55C","66A","66B","66C")
      
      
      from = which(colnames(forcens_df)=="Dentigloborotalia_anfracta")
      to = which(colnames(forcens_df)=="unidentified")
      
      for(i in forcens_df$bias_buckley_resample){
        forcens_dft <- data.frame(t(forcens_df[which(forcens_df$bias_buckley_resample == i),from:to])) # transposing matrix, including only species columns
        raw_counts[sprintf("%sC",i)] <- lapply(raw_counts[sprintf("%sC",i)], 
                                               function(x) forcens_dft[match(raw_counts$species, rownames(forcens_dft)),])
        
        # Adding Forcens "total_counts" values
        if (any(!is.na(forcens_df[forcens_df$bias_buckley_resample == i,c("Count_min","Count")]))){
          raw_counts [which(raw_counts[,"species"]=="total_counts"), sprintf("%sC",i)] <- max(forcens_df[forcens_df$bias_buckley_resample == i,c("Count_min","Count")],na.rm=TRUE)
        }
        
        # Transforming columns A and B also into relative data
        A_total_counts <- raw_counts[which(raw_counts[,"species"] == "total_counts"), sprintf("%sA",i)]
        raw_counts[sprintf("%sA",i)] <- raw_counts[sprintf("%sA",i)]/A_total_counts
        raw_counts[which(raw_counts[,"species"] == "total_counts"), sprintf("%sA",i)] <- A_total_counts
        
        B_total_counts <- raw_counts[which(raw_counts[,"species"] == "total_counts"), sprintf("%sB",i)]
        raw_counts[sprintf("%sB",i)] <- raw_counts[sprintf("%sB",i)]/B_total_counts
        raw_counts[which(raw_counts[,"species"] == "total_counts"), sprintf("%sB",i)] <- B_total_counts
    
      }
      
      write.csv(raw_counts, file = "data/counts_relative.csv",row.names=FALSE)
      
      return(raw_counts)  
      
  } # else
}
