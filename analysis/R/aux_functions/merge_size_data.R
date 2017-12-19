### Description
# Merges all CSV files from bias_size_analysis into one data.frame

### Arguments
# overwrite : TRUE or FALSE (if the output file already exist, do you want to re-run the function and overwrite it?)

merge_size_data <- function(overwrite){


  if(!file.exists("data/bias_size_analysis.csv") | overwrite == TRUE){
      
      all.bias.size <- data.frame(area=double(), diam.max=double(), area_max=double(),  area_log=double(), area_log_max=double(), total_ind=integer(),
                                    species=character(), sspname=character(), sample=integer(), dataset=character(), datasetAB=character())
      s <- c("A", "B")
      dataset <- c("Re-sampling", "Buckley")
      
      ssp.names <- c("G.calida","G.conglobatus","G.crassaformis","S.dehiscens","N.dutertrei","G.falconensis",
                     "G.glutinata","T.humilis","G.inflata","G.menardii","P.obliquiloculata","N.pachyderma",
                     "G.ruber","G.rubescens","G.sacculifer","G.scitula","G.siphonifera","G.tenellus",
                     "G.truncatulinoides","G.tumida","O.universa")
      
      ssp.grep <- c("calida","conglobatus","crassaformis","dehiscens","dutertrei","falconensis",
                    "glutinata","humilis","inflata","menardii","obliquiloculata","pachyderma",
                    "ruber","rubescens","sacculifer","scitula","siphonifera","tenellus",
                    "truncatulinoides","tumida", "universa") 
      
      # removing files of species that were present only in one dataset (A or B)
      remove <- c("dehiscens20B.csv","dehiscens25B.csv","dehiscens31B.csv","dehiscens46B.csv","elongatus20A.csv",       
                  "elongatus25A.csv","elongatus27A.csv","elongatus31A.csv","elongatus46A.csv","menardii25B.csv",        
                  "menardii31B.csv","obliquiloculata31B.csv","pachyderma27B.csv","pachyderma44B.csv","truncatulinoides31B.csv")
    
      
      for (i in 1:2){ # i = 2
        
        files.names <- list.files(path = "data/bias_size_analysis/", pattern=paste(s[i],".csv", sep=""))
        files.names <- files.names[-which(files.names %in% remove)]
        
        for (k in files.names){ # k <- files.names[3]

          data <- read.csv(paste("data/bias_size_analysis/",k, sep=""), header = TRUE, stringsAsFactors=FALSE)
          
          data <- cbind(data, 
                        area_max = max(data$area),
                        area_sqrt = sqrt(data$area),
                        area_sqrt_max = sqrt(max(data$area)),
                        area_log = log(data$area),
                        area_log_max = log(max(data$area)),
                        total_ind = length(data[,1]),
                        species = gsub( "[1-9].*$", "", k ),
                        sspname = ssp.names[grep(gsub( "[1-9].*$", "", k ),ssp.grep)],
                        sample = as.numeric(str_extract(k, "[[:digit:]]+")), 
                        dataset = dataset[i],
                        datasetAB = s[i]
                        )
          
          all.bias.size <- rbind(all.bias.size, data)
          
          rm(data)
        }
      }
      
      write.csv(all.bias.size, file = "data/bias_size_analysis.csv",row.names=FALSE)  
      return(all.bias.size)
      
  }else{ # if (overwrite)   
    all.bias.size <-read.csv("data/bias_size_analysis.csv", header = TRUE, stringsAsFactors=FALSE)
    return(all.bias.size)
  }  
  
}
  
  