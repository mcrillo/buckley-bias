### Description
# Statistical tests of individual size measuremens 

### Arguments
# morpho_df


test_morpho_ind <- function(morpho_df){}
      
      # for( i in unique(morpho_df$species) ) # i <- unique(morpho_df$species)[10]
      
      # One species, two Datasets:
      morpho_df_subset <- morpho_df[which(morpho_df$species == i), ]
      
      ggplot(morpho_df_subset,aes(x=area_log, fill = dataset)) + 
        geom_histogram(binwidth = 0.1, alpha=0.5, position="identity") +
        scale_fill_manual(values = c("red","blue")) +
        labs(x="Log(area)",y="Number of individuals", title=unique(morpho_df_subset$sspname)) +
        theme(plot.title = element_text(face = "bold.italic", size = 14),
              legend.position = c(11,20))
      
      ### Checking normality of the data - Shapiro Wilk normality test
      # H0: sample x came from a normally distributed population
      # p-value < 0.05 : reject H0 -> x not normally distributed
      
      dfA <- morpho_df_subset[which(morpho_df_subset$datasetAB == "A"), ] # Resample
      dfB <- morpho_df_subset[which(morpho_df_subset$datasetAB == "B"), ] # Buckley
      
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
      
      morpho_df_subset <- morpho_df[which(morpho_df$species == i), ]
      
      if(any(duplicated(morpho_df_subset$area_log))){
            print(paste(i, "ties"))
            duplicates <- c()
            dupli <- morpho_df_subset[which(duplicated(morpho_df_subset$area_log)),"area_log"]
              for (i in 1:length(dupli)){
                duplicates <- c(duplicates, which(morpho_df_subset$area_log==dupli[i]))
              }
              print(morpho_df_subset[duplicates,])
            }
            
            dfA <- morpho_df_subset[which(morpho_df_subset$datasetAB == "A"), ] # Resample
            dfB <- morpho_df_subset[which(morpho_df_subset$datasetAB == "B"), ] # Buckley
            
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
      
      