### Description
# summary statistics of morpho_bias-analysis.csv

### Arguments
# size_ind_df: bias_size_analysis.csv (individual measurements)
# overwrite: TRUE or FALSE (if the output file already exist, do you want to re-run the function and overwrite it?)


get_size_pop_data <- function(size_ind_df, overwrite){
  
    if(file.exists("data/bias_size_pop.csv") && overwrite == FALSE){
      
      sumstats <- read.csv("data/bias_size_pop.csv", header = TRUE, stringsAsFactors=FALSE)
      return(sumstats)
      
    }else{
          sumstats <- data.frame(species = character(), sample = integer(), datasetAB = character(), total_ind = integer(), area_max = numeric(), area_log_max = numeric() )
          
          for (j in unique(size_ind_df$sample)){ # j = resamples[5]
            data_sample <- size_ind_df[which(size_ind_df$sample == j),]
            
            for (i in unique(data_sample$species)){ # i = unique(data_sample$species)[1]
              data_species <- data_sample[which(data_sample$species == i),]
              
              for (k in unique(data_species$datasetAB)){ # k = unique(data_species$datasetAB)[1]
                data_final <- data_species[which(data_species$datasetAB == k),]
                sumstats_temp <- data.frame(species = i,
                              sspname = unique(data_final$sspname),            
                              sample = j, 
                              datasetAB = k,
                              total_ind = length(data_final[,1]),
                              
                              area_min = quantile(data_final$area, probs=0), 
                              area_log_min = quantile(data_final$area_log, probs=0),
                              area_sqrt_min = quantile(data_final$area_sqrt, probs=0),
                              
                              area_25q = quantile(data_final$area, probs=0.25), 
                              area_log_25q = quantile(data_final$area_log, probs=0.25),
                              area_sqrt_25q = quantile(data_final$area_sqrt, probs=0.25),
                              
                              area_mean = mean(data_final$area), 
                              area_log_mean = mean(data_final$area_log),
                              area_sqrt_mean = mean(data_final$area_sqrt),
                              
                              area_median = median(data_final$area), 
                              area_log_median = median(data_final$area_log),
                              area_sqrt_median = median(data_final$area_sqrt),
                              
                              area_75q = quantile(data_final$area, probs=0.75), 
                              area_log_75q = quantile(data_final$area_log, probs=0.75),
                              area_sqrt_75q = quantile(data_final$area_sqrt, probs=0.75),
                              
                              area_95q = quantile(data_final$area, probs=0.95), 
                              area_log_95q = quantile(data_final$area_log, probs=0.95),
                              area_sqrt_95q = quantile(data_final$area_sqrt, probs=0.95),
                              
                              area_max = max(data_final$area), 
                              area_log_max = max(data_final$area_log),
                              area_sqrt_max = max(data_final$area_sqrt)
                              
                              )
                sumstats <- rbind(sumstats, sumstats_temp)
              }
            }
          }
          
          write.csv(sumstats, file = "data/bias_size_pop.csv",row.names=FALSE)
          return(sumstats)
      
    }# else
}   
      
      



