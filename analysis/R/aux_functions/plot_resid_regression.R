### Description
# regression of the residuals of model buckley = resample

### Arguments
# res : melt(residuals[[i]]) (residuals: list, output of get_size_pop_residuals.R)
# overwrite : TRUE or FALSE (if the output file already exist, do you want to re-run the function and overwrite it?)

plot_resid_regression <- function(regress, tranfs, name, overwrite){
  # name="95q"
  if (!file.exists(paste("output/resid_",transf,"_regress",name,".pdf", sep = "")) | overwrite == TRUE){
    
    # ordering sspnames by max area_log_95q, for colours gradient
    regress$sspname <- as.factor(regress$sspname)
    regress$sspname <- factor(regress$sspname, 
                      levels = c("G.menardii","G.tumida","G.sacculifer","G.conglobatus",  
                                  "G.truncatulinoides","O.universa","G.siphonifera","P.obliquiloculata", 
                                  "G.crassaformis","N.dutertrei","G.scitula","G.inflata","G.ruber",           
                                  "G.calida","G.glutinata","N.pachyderma","G.tenellus","G.falconensis",    
                                  "T.humilis","G.rubescens"))

    
    plot95q <- ggplot(regress, aes(y = B, x = A) ) + 
      geom_point(aes(colour = factor(sspname)), size = 3)  +  xlim(10, 14) + ylim(10, 14) +   
      geom_abline(intercept = 0, slope = 1, linetype = "dotted", lwd=1) + 
      annotate("text", label = "1:1",fontface =2, x =13.3, y = 13, size = 7, colour = "black") +
      labs(y = "Buckley Collection (log(size) 95%ile)", x = "Resamples (log(size) 95%ile)") +
      scale_color_viridis(discrete=TRUE, name= c("Species (by size)"), option="plasma", 
                          labels = c("G. menardii *","G. tumida","T. sacculifer *","G. conglobatus *",  
                                     "G. truncatulinoides *","O. universa","G. siphonifera *","P. obliquiloculata *", 
                                     "G. crassaformis","N.dutertrei *","G. scitula","G. inflata *","G. ruber *",           
                                     "G. calida","G. glutinata","N. pachyderma","G. tenellus","G. falconensis",    
                                     "T. humilis","G. rubescens")) +
      guides(col = guide_legend(ncol = 1), fill = guide_legend(title = "Species")) +
      theme(axis.text=element_text(size=18), #aspect.ratio = 1,
            axis.title=element_text(size=18),
            legend.text = element_text(size=14,face="italic"), 
            legend.title = element_text(size=14,face="bold"))

    pdf(file = paste("output/resid_",transf,"_regress",name,".pdf", sep = ""), width=8, height=6, paper = "special")
      print(plot95q)
    dev.off()

    return(plot95q)
    
    }else{
      print("Returning NULL object. If you want to return the plot object, set overwrite to TRUE")
      return()
  }
}  
