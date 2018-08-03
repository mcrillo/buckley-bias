### Description
# Size distributions boxplots by SAMPLE - Buckley vs. Resamples datasets 
# ONLY PLOTS if folder "size_boxplot_sample" DOES NOT exists

### Arguments
# size_ind_df: bias_size_analysis.csv (individual measurements)
# overwrite : TRUE or FALSE (if the output file already exist, do you want to re-run the function and overwrite it?)


boxplot_size_sample <-function(data, overwrite){
  
      if (!file.exists("output/size_boxplot_sample") | overwrite == TRUE){
        
        if (!file.exists("output/size_boxplot_sample")){
            dir.create("output/size_boxplot_sample") }
        
            dwidth <- 0.8 # space between pair-boxplots
        
            for(i in unique(data$sample)){ # i=25
              
                data.subset <- data[which(data$sample == i), ]
                
                b <- ggplot(data.subset, aes(x=sspname, y=area_sqrt, fill=dataset)) + geom_boxplot(position = position_dodge(width=dwidth))
                
                pdf(file = paste("output/size_boxplot_sample/",i, "_sizebx.pdf", sep=""), paper = "a4r", width = 20, height = 8) # default: inches
                print(b + labs(title = paste("Sample",i,sep=" "), y = "Square-root(area)", x = element_blank()) +
                        geom_text(data = data.subset, aes(x = sspname, y = area_sqrt_max+0.1, label = total_ind), size = 6, color="red",position = position_dodge(width=dwidth)) +
                        theme(axis.text.y=element_text(size=14), 
                              axis.text.x=element_text(size=11, face="italic", colour="black"), axis.ticks.x=element_blank(),
                              axis.title=element_text(size=14),
                              plot.title = element_text(size = 16, face="bold")) )
                dev.off()
              
            }# for
      } # if
}
