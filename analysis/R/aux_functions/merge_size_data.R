### Description
# Merges all CSV files from morpho_R into one data.frame

### Arguments
# none, but requires files in folder "morpho_R"

merge_size_data <- function(){

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

  
  for (i in 1:2){ # i = 1
    
    files.names <- list.files(path = "data/raw_data/morpho_R", pattern=paste(s[i],".csv", sep=""))
    files.names <- files.names[-which(files.names %in% remove)]
    
    for (k in files.names){ # k <- files.names[15]
      data <- read.csv(paste("data/raw_data/morpho_R/",k, sep=""), header = TRUE, stringsAsFactors=FALSE)
      
      data <- cbind(data, 
                    area_max = max(data$area),
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
  
  write.csv(all.bias.size, file = "data/morpho_bias-analysis.csv",row.names=FALSE)  
  return(all.bias.size)
  
}
  
  