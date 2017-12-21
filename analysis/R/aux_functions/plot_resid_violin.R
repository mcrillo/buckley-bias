### Description
# violin plot of the residuals of model buckley = resample

### Arguments
# res : melt(residuals[[i]]) (residuals: list, output of get_size_pop_residuals.R)
# overwrite : TRUE or FALSE (if the output file already exist, do you want to re-run the function and overwrite it?)

plot_resid_violin <- function(res, tranfs, overwrite){
  
  if (!file.exists(paste("output/resid_",transf,"_violin.pdf", sep = "")) | overwrite == TRUE){
    
    violin <- ggplot(data = res, aes(x=variable, y=value)) + 
      geom_violin(position=position_dodge(width=.3)) + # aes(fill=variable) , position=position_dodge(width=10)) + 
      # geom_boxplot(outlier.shape=NA) + #avoid plotting outliers twice
      geom_jitter(position=position_jitter(width=.3, height=0)) +
      labs(y = "Residuals", x = element_blank()) + # ylim (-3,3) +
      scale_x_discrete(labels=c("Min","25%q","Mean", "Median", "75%q", "95%q", "Maximum")) + 
      # scale_fill_manual(values=c("#5d5f6d","#717f70","#7c96a0","#5e5749","#a48e8e","#5d5f6d","#717f70")) +
      theme(axis.text.y = element_text(size=14, color = "black", face="bold"), 
            axis.title.y = element_text(size=16, color = "black", face="bold"), 
            axis.text.x = element_text(size=16, color = "black", face="bold"), 
            axis.ticks.x = element_blank(), legend.position="none") 
    
    pdf(file = paste("output/resid_",transf,"_violin.pdf", sep = ""), width=10, height=6, paper = "special")
      print(violin)
    dev.off()

  } # if
}