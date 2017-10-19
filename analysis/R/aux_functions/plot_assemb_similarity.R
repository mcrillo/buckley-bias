### Description
# Plots Chao similarity index of pair-wise comparison of the three datasets (Re-sampling, Buckley, ForCenS)
# ONLY PLOTS if folder "assemb_similarity" DOES NOT exists

### Arguments
# assemb_sim_list
# cores10

plot_assemb_similarity <- function(assemb_sim_list, cores10){ # list, string
        
        if (!file.exists("output/assemb_similarity")){
          
            dir.create("output/assemb_similarity")
      
            simchaoAC <- assemb_sim_list$resamples_forcens 
            simchaoAB <- assemb_sim_list$resamples_buckley 
            simchaoBC <- assemb_sim_list$buckley_forcens
            
            names(simchaoAC) <- c("Estimate", "se", "lci", "uci", "sample", "CqN")
            names(simchaoAB) <- c("Estimate", "se", "lci", "uci", "sample", "CqN")
            names(simchaoBC) <- c("Estimate", "se", "lci", "uci", "sample", "CqN")
            
            simchaoAC[,"CqN"] <- as.factor(rep(c(0:2), length(simchaoAC[,"CqN"])/3))
            simchaoAB[,"CqN"] <- as.factor(rep(c(0:2), length(simchaoAB[,"CqN"])/3))
            simchaoBC[,"CqN"] <- as.factor(rep(c(0:2), length(simchaoBC[,"CqN"])/3))
            
            simchaoAC[,"sample"] <- as.factor(simchaoAC[,"sample"])
            simchaoAB[,"sample"] <- as.factor(simchaoAB[,"sample"])
            simchaoBC[,"sample"] <- as.factor(simchaoBC[,"sample"])
            
        
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
    
              
              
            
          # Buckley X ForCenS  
              sBC <- ggplot(simchaoBC, aes(y=Estimate, x=CqN, colour=sample)) + 
                geom_errorbar(aes(ymin=lci, ymax=uci), width=.1,position=pd) +
                #geom_line(aes(colour = sample, group = sample),position=pd) +
                geom_point(position=pd,size=sd) + ylim(0,1) +
                guides(col = guide_legend(ncol = 1)) + 
                scale_color_manual(values=cores10[-1], name= c("Sample")) +
                ggtitle("Similarity between Buckley and ForCenS") +
                labs(y = "Similarity Chao_q2", x = "Order q") +
                theme(axis.text=element_text(size=22, colour = "black"), axis.title=element_text(size=22),
                      legend.text = element_text(size = 18, face="bold"), 
                      legend.title = element_text(size=18), # legend.position = c(.5, .2),
                      panel.background = element_rect(colour = "black", size=1),
                      plot.title = element_text(size = 22, lineheight=.8, face="bold",hjust = 0.5)) +
                annotate("text", x = c(1,2,3), y=0.05, size = 6,  
                         label = c("Sørensen index \n (richness)", "Horn index \n (relative abundance)", "Morisita-Horn index \n (abundant species)"))
              
              pdf(file = "output/assemb_similarity/buckley_forcens.pdf", width=11, height=8, paper = "special")
                print(sBC)
              dev.off()
            
            
          # Buckley X Resamples  
              sAB <- ggplot(simchaoAB, aes(y=Estimate, x=CqN, colour=sample)) + 
                geom_errorbar(aes(ymin=lci, ymax=uci), width=.1,position=pd) +
                # geom_line(aes(colour = sample, group = sample),position=pd) +
                geom_point(position=pd,size=sd) + ylim(0,1) +
                guides(col = guide_legend(ncol = 1)) + 
                scale_color_manual(values=cores10[-1], name= c("Sample")) +
                ggtitle("Similarity between Buckley and Resampling") +
                labs(y = "Similarity Chao_q2", x = "Order q") +
                theme(axis.text=element_text(size=22, colour = "black"), axis.title=element_text(size=22),
                      legend.text = element_text(size = 18, face="bold"), 
                      legend.title = element_text(size=18), # legend.position = c(.5, .125),
                      panel.background = element_rect(colour = "black", size=1),
                      plot.title = element_text(size = 22, lineheight=.8, face="bold",hjust = 0.5)) +
                annotate("text", x = c(1,2,3), y=0.05, size = 6,  
                         label = c("Sørensen index \n (richness)", "Horn index \n (relative abundance)", "Morisita-Horn index \n (abundant species)"))
              
              pdf(file = "output/assemb_similarity/buckley_resampling.pdf", width=11, height=8, paper = "special")
                print(sAB)
              dev.off()
  } # if
}




