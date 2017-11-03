### Description
# Size bias analysis for size-distrib-forams project

### Arguments
# morpho_df


get_bias_size-distrib_project <- function(morpho_df,morpho_stats){}
    
    source("R/aux_functions/species_names.R")

    # excludign dehiscens: too few samples
    species_names <- species_names[-which(species_names == "dehiscens")]
    
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
    
    # population - data_stats
    resample <- data_pop[which(data_pop$datasetAB %in% c("A")),]
    buckley <- data_pop[which(data_pop$datasetAB %in% c("B")),]
    
    plot(buckley$area_log_mean ~ resample$area_log_mean)
    intercept <- 0
    fit <- lm(I(buckley$area_log_mean - intercept) ~ 0 + resample$area_log_mean)
    summary(fit)
    abline(intercept, coef(fit), col="red")
    abline(0,1)
    abline(0,1.046432)
    
    plot(buckley$area_log_median ~ resample$area_log_median)
    intercept <- 0
    fit <- lm(I(buckley$area_log_median - intercept) ~ 0 + resample$area_log_median)
    summary(fit)
    abline(intercept, coef(fit), col="red")
    abline(0,1)
    abline(0,1.047292)
    
    plot(buckley$area_log_95q ~ resample$area_log_95q)
    intercept <- 0
    fit <- lm(I(buckley$area_log_95q - intercept) ~ 0 + resample$area_log_95q)
    summary(fit)
    abline(intercept, coef(fit), col="red")
    abline(0,1)
    abline(0,1.032684)
    
    plot(buckley$area_log_max ~ resample$area_log_max, xlim=c(0,14), ylim=c(0,14))
    intercept <- 0
    fit <- lm(I(buckley$area_log_max - intercept) ~ 0 + resample$area_log_max)
    summary(fit)
    abline(intercept, coef(fit), col="red")
    abline(0,1)
    abline(0,1.030606)
    
    
    
    rmax <- morpho_statsAB$area_log_maxB - morpho_statsAB$area_log_maxA 
    rmean <- morpho_statsAB$area_log_meanB - morpho_statsAB$area_log_meanA 
    rmedian <- morpho_statsAB$area_log_medianB - morpho_statsAB$area_log_medianA 
    r95q <- morpho_statsAB$area_log_95qB - morpho_statsAB$area_log_95qA 
    r75q <- morpho_statsAB$area_log_75qB - morpho_statsAB$area_log_75qA 
    
    residuals <- data.frame(rmean, rmedian, r75q, r95q, rmax)
    
    apply(residuals, 2, var)
    apply(residuals, 2, mean)
    
    res <- melt(residuals)
    str(res) 
    
    
    
    
    