### Description
# Histograms (per specier) of shell size of 2 datasets (bias re-sampling and Buckley Collection)

### Arguments
# size_ind_df
# overwrite : TRUE or FALSE (if the output file already exist, do you want to re-run the function and overwrite it?)

plot_size_histograms <- function(size_ind_df, overwrite){
  
  if (!file.exists("output/size_histograms") | overwrite == TRUE){
    
    if (!file.exists("output/size_histograms")){
      dir.create("output/size_histograms")}
    
    
    for( i in unique(size_ind_df$species) ){ # i <- unique(size_ind_df$species)[10]
      # One species, two Datasets:
      size_ind_df_subset <- size_ind_df[which(size_ind_df$species == i), ]

      p <-  ggplot(size_ind_df_subset,aes(x=area_sqrt, fill = dataset)) + 
        geom_histogram(binwidth = 10, alpha=0.5, position="identity") +
        scale_fill_manual(values = c("red","blue")) +
        labs(x="Square-root of shell area",y="Number of individuals", title=unique(size_ind_df_subset$sspname)) +
        theme(plot.title = element_text(face = "bold.italic", size = 14))
      
      pdf(file = paste("output/size_histograms/", i, ".pdf", sep = ""), width=12, height=6, paper = "special")
        print(p)
      dev.off()
    } # for species
    
    
  } # if
}