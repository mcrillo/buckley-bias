### Description
# Size distributions boxplots by SPECIES - Buckley vs. Resamples datasets
# ONLY PLOTS if folder "size_boxplot_species" DOES NOT exists

### Arguments
# morpho_df


boxplot_size_species <-function(data){
  
      if (!file.exists("output/size_boxplot_species")){
        
            dir.create("output/size_boxplot_species")
              
            dwidth <- 0.8 # space between pair-boxplots
            
            for(i in unique(data$species)){ # i = c("menardii")
              
                data.subset <- data[which(data$species == i), ]
                data.subset$sample <- as.character(data.subset$sample)
                
                b <- ggplot(data.subset, aes(x=sample, y=area_log, fill=dataset)) + geom_boxplot(position = position_dodge(width=dwidth))
                
                pdf(file = paste("output/size_boxplot_species/",i, "_sizebx.pdf", sep=""), paper = "a4r", width = 15, height = 8) # default: inches
                print(b + labs(title = unique(data.subset$sspname), y = "Log(area)", x = "Sample ID number") +
                        geom_text(data = data.subset, aes(label = total_ind, y = area_log_max+0.1), size = 6, color="red", position = position_dodge(width=dwidth)) +
                        theme(axis.text.y=element_text(size=14), 
                              axis.text.x=element_text(size=14), axis.ticks.x=element_blank(),
                              axis.title=element_text(size=16),
                              plot.title = element_text(size = 18, face="bold.italic")) )
                dev.off()
            } # for
    } # if
}
