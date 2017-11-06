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
    
    # Mean
    plot(buckley$area_log_mean ~ resample$area_log_mean)
    intercept <- 0
    fit_mean <- lm(I(buckley$area_log_mean - intercept) ~ 0 + resample$area_log_mean)
    summary(fit_mean)
    abline(intercept, coef(fit_mean), col="red")
    abline(0,1)
    abline(0,1.046432)
    sum(fit_mean$residuals^2) # 8.909305
    res_mean <- buckley$area_log_mean - resample$area_log_mean # = y - x, for model y = f(x) = x, i.e. buckley = resample
    sum(res_mean^2) # 22.80695
    
    # Median
    plot(buckley$area_log_median ~ resample$area_log_median)
    intercept <- 0
    fit_median <- lm(I(buckley$area_log_median - intercept) ~ 0 + resample$area_log_median)
    summary(fit_median)
    abline(intercept, coef(fit_median), col="red")
    abline(0,1)
    abline(0,1.047292)
    sum(fit_median$residuals^2) # 11.36876
    res_median <- buckley$area_log_median - resample$area_log_median # = y - x, for model y = f(x) = x, i.e. buckley = resample
    sum(res_median^2) # 25.8009
    
    # 75th percentile
    plot(buckley$area_log_75q ~ resample$area_log_75q)
    intercept <- 0
    fit_75q <- lm(I(buckley$area_log_75q - intercept) ~ 0 + resample$area_log_75q)
    summary(fit_75q)
    abline(intercept, coef(fit_75q), col="red")
    abline(0,1)
    abline(0,1.032684)
    sum(fit_75q$residuals^2) # 8.783661
    res_75q <- buckley$area_log_75q - resample$area_log_75q # = y - x, for model y = f(x) = x, i.e. buckley = resample
    sum(res_75q^2) # 18.88098
    
    # 95th percentile
    plot(buckley$area_log_95q ~ resample$area_log_95q)
    intercept <- 0
    fit_95q <- lm(I(buckley$area_log_95q - intercept) ~ 0 + resample$area_log_95q)
    summary(fit_95q)
    abline(intercept, coef(fit_95q), col="red")
    abline(0,1)
    abline(0,1.032684)
    sum(fit_95q$residuals^2) # 9.691728
    res_95q <- buckley$area_log_95q - resample$area_log_95q # = y - x, for model y = f(x) = x, i.e. buckley = resample
    sum(res_95q^2) # 17.30539
    
    # Maximum
    plot(buckley$area_log_max ~ resample$area_log_max) #, xlim=c(0,14), ylim=c(0,14))
    intercept <- 0
    fit_max <- lm(I(buckley$area_log_max - intercept) ~ 0 + resample$area_log_max)
    summary(fit_max)
    abline(intercept, coef(fit_max), col="red")
    abline(0,1)
    abline(0,1.030606)
    sum(fit_max$residuals^2) # 12.67864
    res_max <- buckley$area_log_max - resample$area_log_max # = y - x, for model y = f(x) = x, i.e. buckley = resample
    sum(res_max^2) # 19.49306
    
    
    
    stats <- c("mean", "median (50th per.)", "75th percentile", "95th percentile", "maximum")
    ss <- c(sum(res_mean), sum(res_median), sum(res_75q), sum(res_95q), sum(res_max))
    rss <- c(sum(res_mean^2), sum(res_median^2), sum(res_75q^2), sum(res_95q^2),sum(res_max^2))
    mse <- rss/length(buckley$area_log_mean)
    rss_df <- data.frame(stats, ss, rss, mse)
    names(rss_df) <- c("Value of distribution", "Residual sum","Residual sum of squares", "Mean squared error")
    write.csv(rss_df, "output/bias-size-distrib-forams/res_sum_sq.csv", row.names = FALSE)
    
    residuals <- data.frame(res_mean, res_median, res_75q, res_95q, res_max)
    apply(residuals, 2, var)
    apply(residuals, 2, mean)
    
    res <- melt(residuals)
    head(res) 
    
    ### Violin plot of the residuals
    violin <- ggplot(data = res, aes(x=variable, y=value)) + geom_violin(aes(fill=variable), position=position_dodge(width=0.8)) +
                labs(y = "Residuals", x = element_blank()) + ylim (-3,3) +
                scale_x_discrete(labels=c("Mean", "Median", "75%q", "95%q", "Maximum")) + 
                scale_fill_manual(values=c("#7c96a0","#5e5749","#a48e8e","#5d5f6d","#717f70")) +
                theme(axis.text.y = element_text(size=14, color = "black", face="bold"), 
                      axis.title.y = element_text(size=16, color = "black", face="bold"), 
                      axis.text.x = element_text(size=16, color = "black", face="bold"), 
                      axis.ticks.x = element_blank(), legend.position="none") 
    pdf(file = sprintf("output/bias-size-distrib-forams/residuals_violin.pdf", i), width=6, height=6, paper = "special")
      print(violin)
    dev.off()
    
    
    ### Histogram residual distributions for each metric
    hist_res <- ggplot(res, aes(x=value)) + geom_histogram(binwidth = 0.05) + 
                facet_grid(variable ~ .) + # horiz.: facet_grid(. ~ species)
                labs(x = "Residuals (model f(x) = x)", y = "Count") +
                theme( strip.text = element_text(size = 18, face="italic"),
                       axis.text=element_text(size=18),
                       axis.title=element_text(size=18) )
    pdf(file = sprintf("output/bias-size-distrib-forams/residuals_histogram.pdf", i), width=5, height=10, paper = "special")
      print(hist_res)
    dev.off()

    
    ### QQ - plot
    i <- unique(morpho_df$species)[5]
    morpho_df_subset <- morpho_df[which(morpho_df$species == i), ]
    dfA <- morpho_df_subset[which(morpho_df_subset$datasetAB == "A"), ] # Resample
    dfB <- morpho_df_subset[which(morpho_df_subset$datasetAB == "B"), ] # Buckley
    length(dfA[,1])
    length(dfB[,1])
    larger <- dfB$area_log
    smaller <- dfA$area_log
    # Here I interpolated the larger set (that is, estimated 30 quantiles from 50 points); you can do the same to "interpolate" the smaller set but I don't think that helps at all, since the extra 'information' is really just a function of your quantile interpolation function.
    plot(sort(smaller),sort(quantile(larger,probs=ppoints(smaller))), xlim=c(10,15),ylim=c(10,15))
    abline(0,1)