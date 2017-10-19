

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




