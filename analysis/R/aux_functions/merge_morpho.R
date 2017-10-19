### Description
# Merges all CSV files from morpho_R into one data.frame

### Arguments
# none, but requires files in folder "morpho_R"

merge_morpho <- function(){

  all.bias.size <- data.frame(area=double() , diam.max=double(), species=character(), sspname=character(), sample=integer(), dataset=character(), datasetAB=character())
  s <- c("A", "B")
  dataset <- c("resample", "buckley")
  
  # removing files of species that were present only in one dataset (A or B)
  remove <- c("dehiscens20B.csv","dehiscens25B.csv","dehiscens31B.csv","dehiscens46B.csv","elongatus20A.csv",       
              "elongatus25A.csv","elongatus27A.csv","elongatus31A.csv","elongatus46A.csv","menardii25B.csv",        
              "menardii31B.csv","obliquiloculata31B.csv","pachyderma27B.csv","pachyderma44B.csv","truncatulinoides31B.csv")
  
  
  for (i in 1:2){
    
    files.names <- list.files(path = "data/raw_data/morpho_R", pattern=paste(s[i],".csv", sep=""))
    files.names <- files.names[-which(files.names %in% remove)]
    
    for (k in files.names){ # k <- files.names[15]
      data <- read.csv(paste("data/raw_data/morpho_R/",k, sep=""), header = TRUE, stringsAsFactors=FALSE)
      
      data <- cbind(data, 
                    species = gsub( "[1-9].*$", "", k ),
                    sample = rep(as.numeric(str_extract(k, "[[:digit:]]+")), length(data[,1])), 
                    dataset = dataset[i],
                    datasetAB = s[i])
      
      all.bias.size <- rbind(all.bias.size, data)
      
      rm(data)
    }
  }
  
  all.bias.size <- cbind(all.bias.size, area_log = log(all.bias.size$area))
  write.csv(all.bias.size, file = "data/morpho_bias-analysis.csv",row.names=FALSE)  
  return(all.bias.size)
  
}
  
  