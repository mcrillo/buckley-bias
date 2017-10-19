## Created 04 April 2017
## Marina Costa Rillo
##  
## Reads: info_resample_morpho.csv
##        all_species_morpho.csv (Morphometrics)
##
## Creates: ssp_sample_csv folder
##          bias_morpho_individuals.csv
##          bias_morpho_populations.csv
##

# A = my assemblage (Resamples)
# B = Buckley's assemblage

rm(list=ls())
library(stringr)

setwd("/Users/marinacostarillo/Google Drive/DOUTORADO/R_data")

bias.info <- read.csv("Bias_analysis/data_morpho/info_resample_morpho.csv", header = TRUE, stringsAsFactors=FALSE)
morpho.data <- read.csv("Morphometrics/all_species_morpho.csv", header = TRUE, stringsAsFactors=FALSE)

resamples <- c(5,10,20,25,27,31,44,46,55,66)

ssp.names <- c("G.calida","G.conglobatus","G.crassaformis","S.dehiscens","N.dutertrei","G.falconensis",
               "G.glutinata","T.humilis","G.inflata","G.menardii","P.obliquiloculata","N.pachyderma",
               "G.ruber","G.rubescens","G.sacculifer","G.scitula","G.siphonifera","G.tenellus",
               "G.truncatulinoides","G.tumida","O.universa")
  
ssp.grep <- c("calida","conglobatus","crassaformis","dehiscens","dutertrei","falconensis",
              "glutinata","humilis","inflata","menardii","obliquiloculata","pachyderma",
              "ruber","rubescens","sacculifer","scitula","siphonifera","tenellus",
              "truncatulinoides","tumida", "universa") 

##### Saving all ZFs numbers used in the bias analysis ###############################################
zf.bias <- bias.info$ZF.PF.no. # plus merged slides!!
merged.slides <- c(6234,7782,6235,6236,6266,7777,7778,7779,6270,6271,5751,5752,5757,5758,7783,7784,7785,5982,5983,5986,5986,
                   5987,7772,7773,7774,7770,7775,7776,6316,7069,6723,6724,6725,6726,6727,6728,6729,6730,6406,6407,6423,6424,
                   6690,6691,6010,6013,6057,6019,6020,6021,6022,6023,6024,6040,6041,6011,6012,6018,6058) 
                   # from duplicates_explained.rtf file 
buckley.slides <- c(zf.bias,merged.slides)
buckley.slides <- unique(buckley.slides)
buckley.slides <- sort(buckley.slides)
write.csv(buckley.slides, file = "Bias_analysis/ZFs_used_bias.csv",row.names=FALSE)
######################################################################################################

##### Samples used in morphometrics (collected in 2016) ##############################################
for (j in resamples){ # j = resamples[1]
  print(paste("SAMPLE",j))
  for (i in unique(morpho.data$species)){ # i = unique(morpho.data$species)[11]
    rows <- intersect(which(morpho.data["sample"]==j), which(morpho.data["species"]==i))
    if (length(rows) != 0){
      print(i)
      print(paste("ZF",unique(morpho.data[rows,"ZF.PF.no."]))) # check point
      data <- data.frame(area=morpho.data[rows,"area"], diam.max=morpho.data[rows,"diameter"])
      write.csv(data, file = paste("Bias_analysis/data_morpho/ssp_sample_csv/",i,j,"B.csv",sep=""),row.names=FALSE)
      rm(data)
    }
    rm(rows)
  }
}
######################################################################################################

##### Samples measured for Bias analysis (collected in March/2017) ###############################
files.names <- list.files(path = "Bias_analysis/data_morpho/raw_data_merged", pattern="*.csv")
check.col2 <- c()
check.col4 <- c()

for (k in files.names){ # k <- files.names[15]
  raw.data <- read.csv(paste("Bias_analysis/data_morpho/raw_data_merged/",k, sep=""),header = TRUE, stringsAsFactors=FALSE)
  rows <- which(!is.na(str_extract(raw.data[,1],pattern=c("P1R"))))
  data <- data.frame(area=raw.data[rows,2], diam.max=raw.data[rows,4])
  check.col2 <- c(check.col2, raw.data[1,2])
  check.col4 <- c(check.col4, raw.data[1,4])
  write.csv(data, file = paste("Bias_analysis/data_morpho/ssp_sample_csv/",k,sep=""),row.names=FALSE)
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




