### Description
# Histogram of the residual distributions for each metric (model buckley = resample)

### Arguments
# res : melt(residuals[[i]]) (residuals: list, output of get_size_pop_residuals.R)
# overwrite : TRUE or FALSE (if the output file already exist, do you want to re-run the function and overwrite it?)

plot_resid_histogram <- function(res, transf, overwrite){
  
  if (!file.exists(paste("output/resid_",transf,"_histogram.pdf", sep = "")) | overwrite == TRUE){
    
    if(transf == "log") bin = 0.1
    if(transf == "sqrt") bin = 20
    
    hist_res <- ggplot(res, aes(x=value)) + geom_histogram(binwidth = bin) + 
      facet_grid(variable ~ .) + # horiz.: facet_grid(. ~ species)
      labs(x = "Residuals (model f(x) = x)", y = "Count") +
      theme( strip.text = element_text(size = 18, face="italic"),
             axis.text=element_text(size=18),
             axis.title=element_text(size=18) )
    pdf(file = paste("output/resid_",transf,"_histogram.pdf", sep = ""), width=5, height=12, paper = "special")
      print(hist_res)
    dev.off()
    
  } # if
}



