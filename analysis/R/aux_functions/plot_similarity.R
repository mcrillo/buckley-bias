### Description
# Plots Chao similarity index of pair-wise comparison of the three datasets (Re-sampling, lgm, ForCenS)
# ONLY PLOTS if folder "assemb_similarity" DOES NOT exists

### Arguments
# overwrite : TRUE or FALSE (if the output file already exist, do you want to re0run the function and overwrite it?)

plot_similarity <- function(overwrite){ # list, string
  
  if (!file.exists("output/similarity.pdf")| overwrite == TRUE){
    
    simil <- read.csv("output/similarity.csv", header = TRUE, stringsAsFactors = FALSE)
    names(simil) <- c("Estimate", "se", "lci", "uci", "sample", "CqN", "lat", "long", "timing")
    simil[,"CqN"] <- as.factor(rep(c(0:2), length(simil[,"CqN"])/3))
    simil[,"sample"] <- as.factor(simil[,"sample"])
    sim_data <- simil[which(simil[,"CqN"] == 1),] # Horn index (relative abundance)
    sim_data$abs_lat <- round(abs(sim_data$lat))
    sim_data <- sim_data[order(sim_data$abs_lat),]
    sim_data$abs_lat <- round(abs(sim_data$lat))
    sim_data$abs_lat <- as.factor(sim_data$abs_lat)
    
    s <- ggplot(sim_data, aes(x=abs_lat, y=Estimate, shape=timing, colour=timing)) +
      geom_point(size=4)  + ylim(0,1) +
      geom_errorbar(aes(ymin=lci, ymax=uci), width=.3) +
      labs(y = "Assemblage similarity (Horn index)", x = "Historical samples (absolute Latitude)") +
      theme(axis.text=element_text(size=18, colour = "black"), 
          axis.title=element_text(size=18, colour = "black"),
          #axis.text.x=element_blank(),
          #axis.ticks.x=element_blank(),
          axis.line = element_line(colour = "black"),
          legend.text = element_text(size=18, colour = "black"), 
          legend.title = element_blank(),
          legend.position = c(0.85, 0.15),
          legend.background = element_rect(linetype="solid", colour ="black")) +
      scale_color_manual(values=c("#a6611a", "#018571")) +
      scale_shape_manual(values=c(19, 15)) +
      #scale_y_continuous(breaks = seq(0,1,0.25),limits=c(0,1), expand = c(0.02, 0)) +                         
      scale_x_discrete(labels=c(expression("1"*degree),
                                expression("8"*degree),
                                expression("16"*degree),
                                expression("20"*degree),
                                expression("21"*degree),
                                expression("24"*degree),
                                expression("27"*degree),
                                expression("40"*degree),
                                expression("50"*degree),
                                expression("85"*degree)))
      
    pdf(file = "output/similarity.pdf", width=8, height=6, paper = "special")
      print(s)
    dev.off()
    
  
  } # if
}
