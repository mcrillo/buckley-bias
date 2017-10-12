### Description
# 
# requires: info_resample_morpho.csv
#           all_species_morpho.csv (Morphometrics)
# creates:  ssp_sample_csv folder
#           bias_morpho_individuals.csv
#           bias_morpho_populations.csv

### Usage
# 

### Arguments
# morpho_df
# resamp_morpho_df
# overwrite : TRUE or FALSE (if the output file already exist, do you want to re0run the function and overwrite it?)

# A = my assemblage (Resamples)
# B = Buckley's assemblage


##### Samples measured for Bias analysis (collected in March/2017) ###############################
files.names <- list.files(path = "Bias_analysis/data_morpho/raw_data_merged", pattern="*.csv")
check.col2 <- c()
check.col4 <- c()

for (k in files.names){ # k <- files.names[15]
  raw.data <- read.csv(paste("data/morpho_bias_merged/",k, sep=""),header = TRUE, stringsAsFactors=FALSE)
  rows <- which(!is.na(str_extract(raw.data[,1],pattern=c("P1R"))))
  data <- data.frame(area=raw.data[rows,2], diam.max=raw.data[rows,4])
  check.col2 <- c(check.col2, raw.data[1,2])
  check.col4 <- c(check.col4, raw.data[1,4])
  write.csv(data, file = paste("data/morphometrics/",k,sep=""),row.names=FALSE)
  rm(rows)
  rm(data)
}
unique(check.col2)
unique(check.col4)
######################################################################################################


##### Binding all samples measurements in one file all_bias_size.csv #################################
all.bias.size <- data.frame(area=double() , diam.max=double(), species=character(), sspname=character(), sample=integer(), dataset=character(), datasetAB=character())
s <- c("A", "B")
dataset <- c("Resample", "Buckley")

for (i in 1:2){
  
      files.names <- list.files(path = "Bias_analysis/data_morpho/ssp_sample_csv", pattern=paste(s[i],".csv", sep=""))
      
      for (k in files.names){ # k <- files.names[15]
        data <- read.csv(paste("Bias_analysis/data_morpho/ssp_sample_csv/",k, sep=""), header = TRUE, stringsAsFactors=FALSE)
       
        data <- cbind(data, 
                    species = gsub( "[1-9].*$", "", k ),
                    sspname = rep(ssp.names[grep(gsub( "[1-9].*$", "", k ),ssp.grep)], length(data[,1])), 
                    sample = rep(as.numeric(str_extract(k, "[[:digit:]]+")), length(data[,1])), 
                    dataset = dataset[i],
                    datasetAB = s[i])
        
        all.bias.size <- rbind(all.bias.size, data)
        
        rm(data)
      }
}
all.bias.size <- cbind(all.bias.size, area_log = log(all.bias.size$area))




######################################################################################################
##### Summary statistics of all_bias_size.csv ########################################################
sumstats <- data.frame(species = character(), sample = integer(), datasetAB = character(), total_ind = integer(), area_max = numeric(), area_log_max = numeric() )

for (j in unique(all.bias.size$sample)){ # j = resamples[5]
  data_sample <- all.bias.size[which(all.bias.size$sample == j),]
  
  for (i in unique(data_sample$species)){ # i = unique(data_sample$species)[1]
    data_species <- data_sample[which(data_sample$species == i),]
    
    for (k in unique(data_species$datasetAB)){ # k = unique(data_species$datasetAB)[1]
      data_final <- data_species[which(data_species$datasetAB == k),]
      sumstats_temp <- data.frame(species = i,
                    sspname = unique(data_final$sspname),            
                    sample = j, 
                    datasetAB = k,
                    total_ind = length(data_final[,1]),
                    
                    area_min = quantile(data_final$area, probs=0), area_log_min = quantile(data_final$area_log, probs=0),
                    
                    area_25q = quantile(data_final$area, probs=0.25), area_log_25q = quantile(data_final$area_log, probs=0.25),
                    
                    area_mean = mean(data_final$area), area_log_mean = mean(data_final$area_log),
                    
                    area_median = median(data_final$area), area_log_median = median(data_final$area_log),
                    
                    area_75q = quantile(data_final$area, probs=0.75), area_log_75q = quantile(data_final$area_log, probs=0.75),

                    area_95q = quantile(data_final$area, probs=0.95), area_log_95q = quantile(data_final$area_log, probs=0.95),
                    
                    area_max = max(data_final$area), area_log_max = max(data_final$area_log)
                    )
      sumstats <- rbind(sumstats, sumstats_temp)
    }
  }
}

write.csv(sumstats, file = "Bias_analysis/data_morpho/bias_morpho_populations.csv",row.names=FALSE)
###################################################################################################



###################################################################################################
##### Adding max size to all_bias_size.csv ########################################################
all.bias.size <- cbind(all.bias.size,data.frame(total_ind = NA, area_max = NA, area_log_max = NA))
  
for (i in 1:length(sumstats[,1])){ # i = 1
    rows <- which(all.bias.size$sample == sumstats$sample[i] & 
            all.bias.size$species== sumstats$species[i] & 
            all.bias.size$datasetAB== sumstats$datasetAB[i])
    all.bias.size[rows,"total_ind"] = rep(sumstats$total_ind[i], length(rows))
    all.bias.size[rows,"area_max"] = rep(sumstats$area_max[i], length(rows))
    all.bias.size[rows,"area_log_max"] = rep(sumstats$area_log_max[i], length(rows))
}

write.csv(all.bias.size, file = "Bias_analysis/data_morpho/bias_morpho_individuals.csv",row.names=FALSE)
##################################################################################################




