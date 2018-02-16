### Description
# Statistical tests of individual size measuremens 

### Arguments
# size_ind_df

# subseting data for size-distrib-forams species
data <- morpho_df[which(morpho_df$species %in% species_names), ]
data_pop <- morpho_stats[which(morpho_stats$species %in% species_names), ]


# individual - data
sample_size <- data.frame(species = character(), resample = numeric(), buckley = numeric())
for( i in unique(data$species) ) { # i <- unique(data$species)[10]
  
  subset <- data[which(data$species == i), ]
  dfA <- subset[which(subset$datasetAB == "A"), ] # Resample
  dfB <- subset[which(subset$datasetAB == "B"), ] # Buckley
  
  sample_size <- rbind(sample_size, data.frame(species = i, resample = length(dfA[,1]), buckley = length(dfB[,1])))
  write.csv(sample_size, "output/bias-size-distrib-forams/sample_size.csv", row.names = FALSE)
  
  kstest <- ks.test(x=dfA$area_log, y=dfB$area_log, alternative = c("two.sided"),exact = NULL)
  capture.output(kstest,file=paste("output/bias-size-distrib-forams/kstest/",i, "_kstest.txt", sep=""))
}



test_morpho_ind <- function(size_ind_df){}

      ### Checking normality of the data - Shapiro Wilk normality test
      # H0: sample x came from a normally distributed population
      # p-value < 0.05 : reject H0 -> x not normally distributed
      
      dfA <- size_ind_df_subset[which(size_ind_df_subset$datasetAB == "A"), ] # Resample
      dfB <- size_ind_df_subset[which(size_ind_df_subset$datasetAB == "B"), ] # Buckley
      
      # with(dfA, tapply(area_log, species, shapiro.test))
      shap <- shapiro.test(dfA$area_log)
      shapiro.test(dfA$area)
      shapiro.test(dfB$area_log)
      shapiro.test(dfB$area)
      
      ###### All samples together
      kstest <- ks.test(x=dfA$area_log, y=dfB$area_log, alternative = c("two.sided"),exact = NULL)
      # H0: x and y were drawn from the same continuous distribution
      # p-value < 0.05 : reject H0 = x and y come from different distributions
      
      capture.output(kstest,file=paste("output/size_kstest_indiv/",i, ".txt", sep=""))
      
      
      ###### Per sample - see in which sample(s) they differ
      ks.same <- data.frame(species = character(), sample = integer(), p = numeric(),stringsAsFactors=FALSE)
      ks.diff <- data.frame(species = character(), sample = integer(), p = numeric(),stringsAsFactors=FALSE)
      
      size_ind_df_subset <- size_ind_df[which(size_ind_df$species == i), ]
      
      if(any(duplicated(size_ind_df_subset$area_log))){
            print(paste(i, "ties"))
            duplicates <- c()
            dupli <- size_ind_df_subset[which(duplicated(size_ind_df_subset$area_log)),"area_log"]
              for (i in 1:length(dupli)){
                duplicates <- c(duplicates, which(size_ind_df_subset$area_log==dupli[i]))
              }
              print(size_ind_df_subset[duplicates,])
            }
            
            dfA <- size_ind_df_subset[which(size_ind_df_subset$datasetAB == "A"), ] # Resample
            dfB <- size_ind_df_subset[which(size_ind_df_subset$datasetAB == "B"), ] # Buckley
            
            all(unique(dfA$sample) == unique(dfB$sample))
            ks.sample <- data.frame(sample=double(),D=double(), p=double())
            n=1
            for (j in unique(dfA$sample)){ # j=20
              dfAj <- dfA[which(dfA$sample == j), ]
              dfBj <- dfB[which(dfB$sample == j), ]
              test <- ks.test(x=dfBj$area_log, y=dfAj$area_log, alternative = c("two.sided"),exact = NULL)
              ks.sample[n,] <- c(j, as.numeric(test$statistic),test$p.value)
              n = n+1
              rm(dfAj,dfBj,test)
            }
            
            if(any(ks.sample$p>0.05)){
              same.sample <- ks.sample[which(ks.sample$p>0.05),"sample"]
              same.p <- ks.sample[which(ks.sample$p>0.05),"p"]
              same.ssp <- rep(i, length(same.p))
              ks.same <- rbind(ks.same, data.frame(same.ssp, same.sample, same.p))
              
            }
            if(any(ks.sample$p<0.05)){
              diff.sample <- ks.sample[which(ks.sample$p<0.05),"sample"]
              diff.p <- ks.sample[which(ks.sample$p<0.05),"p"]
              diff.ssp <- rep(i, length(diff.p))
              ks.diff <- rbind(ks.diff, data.frame(diff.ssp, diff.sample, diff.p))
            }
      
      ### Plotting per sample
      dfAj <- dfA[which(dfA$sample == 46), ]
      dfBj <- dfB[which(dfB$sample == 46), ]
      ecdf1 <- ecdf(dfAj$area_log)
      ecdf2 <- ecdf(dfBj$area_log)
      
      print(ecdf2)
      plot(ecdf2, ylab="Fn(x)", verticals = TRUE, col.01line = "gray70", do.points=TRUE, pch = 19, col='blue')
      plot(ecdf1, ylab="Fn(x)", verticals = TRUE, col.01line = "gray70", do.points=TRUE, pch = 19, col='red', add=TRUE)
      
      capture.output(kstest,file=paste("output/size_kstest_indiv/",i, ".txt", sep=""))
      
      