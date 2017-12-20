### Description
# Merges all CSV files from bias_size_analysis into one data.frame

### Arguments
# overwrite : TRUE or FALSE (if the output file already exist, do you want to re-run the function and overwrite it?)

merge_size_data <- function(overwrite){

  if(file.exists("data/bias_size_analysis.csv") && overwrite == FALSE){
    
    all.bias.size <-read.csv("data/bias_size_analysis.csv", header = TRUE, stringsAsFactors=FALSE)
    return(all.bias.size)
    
  }else{  
      
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
      
      # merging elongatus into ruber (Buckley did not differentiate between them)
      
      data_elong18 <- read.csv("data/bias_size_analysis/elongatus18A.csv", header = TRUE, stringsAsFactors=FALSE)
      data_ruber18 <- read.csv("data/bias_size_analysis/ruber18A.csv", header = TRUE, stringsAsFactors=FALSE)
      # making sure I do not add elongatus twice to ruber dataset:
      ruber18_original_length <- 41
      elong18_original_length <- 29
      data_ruber18 <- rbind(data_ruber18, data_elong18)
      if(length(data_ruber18[,1]) == (ruber18_original_length + elong18_original_length)){ 
        # TRUE if it is first time elongatus is added to ruber, FALSE if it has already been added
        write.csv(data_ruber18, "data/bias_size_analysis/ruber18A.csv",row.names=FALSE) }
      
      # same thing as for sample 18
      data_elong23 <- read.csv("data/bias_size_analysis/elongatus23A.csv", header = TRUE, stringsAsFactors=FALSE)
      data_ruber23 <- read.csv("data/bias_size_analysis/ruber23A.csv", header = TRUE, stringsAsFactors=FALSE)
      ruber23_original_length <- 62
      elong23_original_length <- 32
      data_ruber23 <- rbind(data_ruber23, data_elong23)
      if(length(data_ruber23[,1])== (ruber23_original_length + elong23_original_length) ){ 
        write.csv(data_ruber23, "data/bias_size_analysis/ruber23A.csv",row.names=FALSE) }
      
      
      data_elong25 <- read.csv("data/bias_size_analysis/elongatus25A.csv", header = TRUE, stringsAsFactors=FALSE)
      data_ruber25 <- read.csv("data/bias_size_analysis/ruber25A.csv", header = TRUE, stringsAsFactors=FALSE)
      ruber25_original_length <- 96
      elong25_original_length <- 98
      data_ruber25 <- rbind(data_ruber25, data_elong25)
      if(length(data_ruber25[,1]) == (ruber25_original_length + elong25_original_length)){ 
        write.csv(data_ruber25, "data/bias_size_analysis/ruber25A.csv",row.names=FALSE)}
      
      data_elong29 <- read.csv("data/bias_size_analysis/elongatus29A.csv", header = TRUE, stringsAsFactors=FALSE)
      data_ruber29 <- read.csv("data/bias_size_analysis/ruber29A.csv", header = TRUE, stringsAsFactors=FALSE)
      ruber29_original_length <- 87
      elong29_original_length <- 36
      data_ruber29 <- rbind(data_ruber29, data_elong29)
      if(length(data_ruber29[,1]) == (ruber29_original_length + elong29_original_length)){ 
        write.csv(data_ruber29, "data/bias_size_analysis/ruber29A.csv",row.names=FALSE)}
      
      data_elong44 <- read.csv("data/bias_size_analysis/elongatus44A.csv", header = TRUE, stringsAsFactors=FALSE)
      data_ruber44 <- read.csv("data/bias_size_analysis/ruber44A.csv", header = TRUE, stringsAsFactors=FALSE)
      ruber44_original_length <- 87
      elong44_original_length <- 24
      data_ruber44 <- rbind(data_ruber44, data_elong44)
      if(length(data_ruber44[,1]) == (ruber44_original_length + elong44_original_length)){ 
        write.csv(data_ruber44, "data/bias_size_analysis/ruber44A.csv",row.names=FALSE)}
      
      
      # removing files of species that were present only in one dataset (A or B)
      # OBS1: new sample number (see README file in raw_data_measurmts folder)
      remove <- c("dehiscens25A.csv","dehiscens42A.csv", "siphonifera23B.csv",
                  "menardii23B.csv","menardii29B.csv","obliquiloculata29B.csv",
                  "pachyderma25B.csv","pachyderma42B.csv","truncatulinoides29B.csv",
                  "elongatus18A.csv","elongatus23A.csv","elongatus25A.csv","elongatus29A.csv","elongatus44A.csv")

      
      
      for (i in 1:2){ # for each dataset (A or B) # i = 2
        
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
      
  } # if (overwrite)   

}
  
  