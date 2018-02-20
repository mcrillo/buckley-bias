### Description
# violin plot of the residuals of model buckley = resample

### Arguments
# res : melt(resid_list[[i]]) (resid_list: list, output of get_size_pop_residuals.R)
# stats : resid_list$stats (resid_list: list, output of get_size_pop_residuals.R)
# tranfs : log or sqrt (tranformation of shell area)
# overwrite : TRUE or FALSE (if the output file already exist, do you want to re-run the function and overwrite it?)

plot_resid_violin <- function(res, stats, transf, overwrite){
  
  if (!file.exists(paste("output/resid_",transf,"_violin.pdf", sep = "")) | overwrite == TRUE){
    # stats <- resid_list$stats
    res <- res[!apply(is.na(res) | res == "", 1, all),]
    
    # removing area_min and area_25q
    res <- res[which(res$variable %!in% c(paste("area_",transf,"_min",sep=""),paste("area_",transf,"_25q",sep=""))),]
    mse <- stats[which(stats[,"rownames"] %!in% c("area_min","area_25q")),c("rownames", paste("mse_",transf,sep=""))]
    
    if(transf == "log") { bin = 0.09 ; ymin = -1.5; ymax = 2.6 ; ylabel = -1.25 ; rd = 2}
    if(transf == "sqrt") { bin = 20 ; ymin = -200; ymax = 710 ; ylabel = -175 ; rd = 0}
    
    ### Black and White
    violin_bw <- ggplot(data = res, aes(x=variable, y=value))+ 
      geom_violin(trim = T, fill=alpha("white", 0.3), color="grey60") + 
      geom_dotplot(binaxis="y", stackdir = "center", dotsize=0.5, binwidth = bin) +
      geom_abline(intercept = 0, slope = 0, linetype = 2) + 
      labs(y = "Residuals", x = element_blank()) + ylim(ymin,ymax) +
      scale_x_discrete(labels=c("Mean", "Median", "75%ile", "95%ile", "Max")) + 
      scale_fill_viridis(discrete=TRUE, name= c("Species"), option="plasma") + 
      theme(axis.text = element_text(size=18, color = "black"),
            axis.line = element_line(size = 1, colour = "black"),
            axis.title.y = element_text(size=18, color = "black"), 
            legend.position="none") +
      geom_label(data=data.frame(), aes(x=1:5, y=ylabel, label=round(mse[,2],rd)), colour = "black", size=6.5)

    pdf(file = paste("output/resid_",transf,"_violin_bw.pdf", sep = ""), width=7, height=6, paper = "special")
      print(violin_bw)
    dev.off()
    
    
    ### Coloured by species
    # This was suprinsigly difficult due to a bug in geom_dotplot:
    
    # Reported bug:
    # https://stackoverflow.com/questions/31557973/how-to-use-ggplot2s-geom-dotplot-with-both-fill-and-group
    # https://github.com/tidyverse/ggplot2/issues/1359
    # https://github.com/tidyverse/ggplot2/pull/1096
    # https://github.com/tidyverse/ggplot2/issues/1745
    
    # In the end I mixed two solutions:
    # (1) created a color column in the data set 
    # (2) made sure data set was ordered first by the factor (x), then by the variable (y)
    
    df <- res[with(res,order(factor(variable), value)),]
    # creating species' colors, ordered by max area_log_95q
    colors <- data.frame(
      sspname = c("G.menardii","G.tumida","G.sacculifer","G.conglobatus",  
                  "G.truncatulinoides","O.universa","G.siphonifera","P.obliquiloculata", 
                  "G.crassaformis","N.dutertrei","G.scitula","G.inflata","G.ruber",           
                  "G.calida","G.glutinata","N.pachyderma","G.tenellus","G.falconensis",    
                  "T.humilis","G.rubescens"),
      plasma = plasma(n=20), stringsAsFactors=FALSE)
    
    # adding color column to data set
    df$plasma <- c()
    for(i in 1: length(df$sspname)){
      df$plasma[i] <-  colors[which(colors$sspname == df$sspname[i]), "plasma"]
    }
    
    # re-ordering just in case (so that colours are correctly assigned to species)
    # df <- df[with(df,order(factor(variable), value)),]

    # ordering sspnames by max area_log_95q, for colours gradient
    df$sspname <- as.factor(df$sspname)
    df$sspname <- factor(df$sspname, 
                  levels = c("G.menardii","G.tumida","G.sacculifer","G.conglobatus",  
                              "G.truncatulinoides","O.universa","G.siphonifera","P.obliquiloculata", 
                              "G.crassaformis","N.dutertrei","G.scitula","G.inflata","G.ruber",           
                              "G.calida","G.glutinata","N.pachyderma","G.tenellus","G.falconensis",    
                              "T.humilis","G.rubescens"))
    df <- df[with(df,order(factor(variable),value, sspname)),]
    
    # ordering colors to be in gradient lines within each violin plot
    # for this, I had to create a bin variable, 
    # so that all values within a bin (=line in violoin plot) are the same
    # and then when I order it by (1) variable, (2) value (now bins) and (3) sspname, it actually gets ordered by sspname instead of value (since all values now within a bin are the same)
    # PS: this ordering order ((1) variable, (2) value) is KEY to the dotplot to be correct, see comments above
    df$bins <- round(df$value, 1)
    df <- arrange(df, factor(variable), bins, desc(sspname))
    
    
    violin <- ggplot(data = df, aes(x=variable, y=bins))+ 
      geom_violin(trim = T, fill=alpha("grey60", 0.3), color="white") + 
      geom_dotplot(color = df$plasma, fill = df$plasma, binaxis = "y", stackdir = "center", 
                   dotsize=0.6, binwidth = bin) + 
      # other ways to dotplot: stackgroups=TRUE, binpositions="all") +
      geom_abline(intercept = 0, slope = 0, linetype = "dotted", lwd=1) + 
      labs(y = "Residuals", x = element_blank()) + ylim(ymin,ymax) +
      scale_x_discrete(labels=c("Mean", "Median", "75%ile", "95%ile", "Max")) + 
      theme(axis.text = element_text(size=18, color = "black"), 
            axis.title.y = element_text(size=18, color = "black"), 
            axis.line = element_line(colour = "black"),
            legend.position="none") +
      geom_label(data=data.frame(), aes(x=1:5, y=ylabel, label=round(mse[,2],rd)), colour = "black", size=6.5)
    
    
    pdf(file = paste("output/resid_",transf,"_violin.pdf", sep = ""), width=7, height=6, paper = "special")
      print(violin)
    dev.off()
    
    return(violin)
    
  }else{
    print("Returning NULL object. If you want to return the plot object, set overwrite to TRUE")
    return()
  } # if
}   
  