### Description
# Plots Chao similarity index of pair-wise comparison of the three datasets (Re-sampling, lgm, ForCenS)
# ONLY PLOTS if folder "assemb_similarity" DOES NOT exists

### Arguments
# lgm_simil_list
# cores10
# overwrite : TRUE or FALSE (if the output file already exist, do you want to re0run the function and overwrite it?)

plot_assemb_simil_lgm <- function(lgm_simil_list, cores10, overwrite){ # list, string
  
  if (!file.exists("output/assemb_simil_lgm")| overwrite == TRUE){
    
    dir.create("output/assemb_simil_lgm")
    
    simchaoAC <- lgm_simil_list$resamples_forcens 
    simchaoAL <- lgm_simil_list$resamples_lgm 
    simchaoLC <- lgm_simil_list$lgm_forcens
    
    names(simchaoAC) <- c("Estimate", "se", "lci", "uci", "sample", "CqN")
    names(simchaoAL) <- c("Estimate", "se", "lci", "uci", "sample", "CqN")
    names(simchaoLC) <- c("Estimate", "se", "lci", "uci", "sample", "CqN")
    
    simchaoAC[,"CqN"] <- as.factor(rep(c(0:2), length(simchaoAC[,"CqN"])/3))
    simchaoAL[,"CqN"] <- as.factor(rep(c(0:2), length(simchaoAL[,"CqN"])/3))
    simchaoLC[,"CqN"] <- as.factor(rep(c(0:2), length(simchaoLC[,"CqN"])/3))
    
    simchaoAC[,"sample"] <- as.factor(simchaoAC[,"sample"])
    simchaoAL[,"sample"] <- as.factor(simchaoAL[,"sample"])
    simchaoLC[,"sample"] <- as.factor(simchaoLC[,"sample"])
    
    
    ### PLOTS 
    
    pd <- position_dodge(0.3) # space between points
    sd <- 4 # size of point
    
    
    # Resamples X ForCenS 
    sAC <- ggplot(simchaoAC, aes(y=Estimate, x=CqN, colour=sample)) + 
      geom_errorbar(aes(ymin=lci, ymax=uci), width=.1,position=pd) +
      # geom_line(aes(colour = sample, group = sample),position=pd) +
      geom_point(position=pd,size=sd) + ylim(0,1) +
      guides(col = guide_legend(ncol = 1)) + 
      ggtitle("Similarity between Resampling and ForCenS") +
      scale_color_manual(values=cores10, name= c("Sample")) +
      labs(y = "Similarity Chao_q2", x = "Order q") +
      theme(axis.text=element_text(size=22, colour = "black"), axis.title=element_text(size=22),
            legend.text = element_text(size = 18, face="bold"), 
            legend.title = element_text(size=18), # legend.position = c(.5, .25),
            panel.background = element_rect(colour = "black", size=1),
            plot.title = element_text(size = 22, lineheight=.8, face="bold",hjust = 0.5)) +
      annotate("text", x = c(1,2,3), y=0.05, size = 6,  
               label = c("Sørensen index \n (richness)", "Horn index \n (relative abundance)", "Morisita-Horn index \n (abundant species)"))
    
    pdf(file = "output/assemb_similarity/resampling_forcens.pdf", width=11, height=8, paper = "special")
    print(sAC)
    dev.off()
    
    
    
    
    # lgm X ForCenS  
    sLC <- ggplot(simchaoLC, aes(y=Estimate, x=CqN, colour=sample)) + 
      geom_errorbar(aes(ymin=lci, ymax=uci), width=.1,position=pd) +
      #geom_line(aes(colour = sample, group = sample),position=pd) +
      geom_point(position=pd,size=sd) + ylim(0,1) +
      guides(col = guide_legend(ncol = 1)) + 
      scale_color_manual(values=cores10[-1], name= c("Sample")) +
      ggtitle("Similarity between lgm and ForCenS") +
      labs(y = "Similarity Chao_q2", x = "Order q") +
      theme(axis.text=element_text(size=22, colour = "black"), axis.title=element_text(size=22),
            legend.text = element_text(size = 18, face="bold"), 
            legend.title = element_text(size=18), # legend.position = c(.5, .2),
            panel.background = element_rect(colour = "black", size=1),
            plot.title = element_text(size = 22, lineheight=.8, face="bold",hjust = 0.5)) +
      annotate("text", x = c(1,2,3), y=0.05, size = 6,  
               label = c("Sørensen index \n (richness)", "Horn index \n (relative abundance)", "Morisita-Horn index \n (abundant species)"))
    
    pdf(file = "output/assemb_similarity/lgm_forcens.pdf", width=11, height=8, paper = "special")
    print(sLC)
    dev.off()
    
    
    # lgm X Resamples  
    sAL <- ggplot(simchaoAL, aes(y=Estimate, x=CqN, colour=sample)) + 
      geom_errorbar(aes(ymin=lci, ymax=uci), width=.1,position=pd) +
      # geom_line(aes(colour = sample, group = sample),position=pd) +
      geom_point(position=pd,size=sd) + ylim(0,1) +
      guides(col = guide_legend(ncol = 1)) + 
      scale_color_manual(values=cores10[-1], name= c("Sample")) +
      ggtitle("Similarity between lgm and Resampling") +
      labs(y = "Similarity Chao_q2", x = "Order q") +
      theme(axis.text=element_text(size=22, colour = "black"), axis.title=element_text(size=22),
            legend.text = element_text(size = 18, face="bold"), 
            legend.title = element_text(size=18), # legend.position = c(.5, .125),
            panel.background = element_rect(colour = "black", size=1),
            plot.title = element_text(size = 22, lineheight=.8, face="bold",hjust = 0.5)) +
      annotate("text", x = c(1,2,3), y=0.05, size = 6,  
               label = c("Sørensen index \n (richness)", "Horn index \n (relative abundance)", "Morisita-Horn index \n (abundant species)"))
    
    pdf(file = "output/assemb_similarity/lgm_resampling.pdf", width=11, height=8, paper = "special")
    print(sAL)
    dev.off()
  } # if
}




