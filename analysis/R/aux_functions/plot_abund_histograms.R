### Description
# Histograms (per sample) of relative abundances of 3 datasets (Boas, Buckley, ForCenS)

### Arguments
# assemb_relat_df
# resamples_df


plot_abund_histograms <- function(assemb_relat_df,resamples_df){

      if (!file.exists("output/abund_histograms")){
        dir.create("output/abund_histograms")
      }

      for (i in resamples_df$sample){ # i=25
          # Subsetting for sample i
          data <- data.frame(assemb_relat_df$species, assemb_relat_df[,c(sprintf("%sC", i), sprintf("%sA", i),sprintf("%sB", i))])
          colnames(data) <- c("species", "ForCenS", "Bias", "Buckley")
          data <- data[-which(data[,"species"] %in% c("unidentified","total_counts")),]

          data <- data[-which(rowSums(data[,c("ForCenS", "Bias", "Buckley")], na.rm=TRUE)==0),]
          
          # melt (reshape Rpackage): transforms data into one column for ggplot
          data <- melt(data, id.vars=1) 
          names(data) <- c("species", "dataset", "relabund")
          palette <- c("#000000", "#009E73", "#D55E00")
          
          pdf(file = sprintf("output/abund_histograms/ssp_abund_sample%s.pdf", i), width=9.22, height=7.43, paper = "special")
          print(ggplot(data, aes(x=reorder(species,relabund), y=relabund, fill=factor(dataset, levels = rev(levels(dataset))))) + 
                  geom_bar(stat="identity",position="dodge", width=0.75) + 
                  labs(x="Species",y="Relative Abundance (%)", title=sprintf("Sample %s", i)) +
                  scale_fill_manual('Dataset',values=palette) + theme_light() +
                  coord_flip()
          )
          dev.off()
          rm(data)
      }

}