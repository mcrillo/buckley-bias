
###
### Other indexes plots
###
similarity <- read.csv("similarity.csv", header = TRUE)

sim_data0 <- similarity[grep("c02", similarity$index),] # Sorensen index (richness)
sim_data1 <- similarity[grep("c12", similarity$index),] # Horn index (relative abundance, abundant)
sim_data_cs <- similarity[grep("chao_sorensen", similarity$index),]
sim_data_cj <- similarity[grep("chao_jaccard", similarity$index),]

# Absolute Latitude 
sim_data0$abs_lat <- round(abs(sim_data0$lat))
sim_data0 <- sim_data0[order(sim_data0$abs_lat),]
sim_data0$abs_lat <- round(abs(sim_data0$lat))
sim_data0$abs_lat <- as.factor(sim_data0$abs_lat)

sim_data1$abs_lat <- round(abs(sim_data1$lat))
sim_data1 <- sim_data1[order(sim_data1$abs_lat),]
sim_data1$abs_lat <- round(abs(sim_data1$lat))
sim_data1$abs_lat <- as.factor(sim_data1$abs_lat)

sim_data_cs$abs_lat <- round(abs(sim_data_cs$lat))
sim_data_cs <- sim_data_cs[order(sim_data_cs$abs_lat),]
sim_data_cs$abs_lat <- round(abs(sim_data_cs$lat))
sim_data_cs$abs_lat <- as.factor(sim_data_cs$abs_lat)

sim_data_cj$abs_lat <- round(abs(sim_data_cj$lat))
sim_data_cj <- sim_data_cj[order(sim_data_cj$abs_lat),]
sim_data_cj$abs_lat <- round(abs(sim_data_cj$lat))
sim_data_cj$abs_lat <- as.factor(sim_data_cj$abs_lat)


# Plots

sorensen <- ggplot(sim_data0, aes(x=abs_lat, y=Estimate, shape=comparison, colour=comparison, group = comparison)) +
  geom_errorbar(position=position_dodge(width=0.5), aes(ymin=lci, ymax=uci), width=.2) +
  geom_point(position=position_dodge(width=0.5), size=4)  + ylim(0,1) +
  labs(y = "Sorensen index", x = "Samples (by absolute Latitude)") +
  theme_bw() + 
  theme(axis.text=element_text(size=18, colour = "black"), 
        axis.title=element_text(size=22, colour = "black"),
        legend.text = element_text(size=18, colour = "black"), 
        legend.title = element_blank(),
        legend.position = c(0.25, 0.15),
        legend.background = element_rect(linetype="solid",size = 0.4,  colour ="black")) +
  scale_color_manual(values=c("#a6611a", "#018571", "#999999")) +
  scale_shape_manual(values=c(19, 15, 17)) +
  #scale_y_continuous(breaks = seq(0,1,0.25),limits=c(0,1), expand = c(0.02, 0)) +                         
  scale_x_discrete(labels=c(expression("1"*degree),
                            expression("8"*degree),
                            expression("16"*degree),
                            expression("20"*degree),
                            expression("21"*degree),
                            expression("24"*degree),
                            expression("27"*degree),
                            expression("40"*degree),
                            expression("50"*degree)))

pdf(file = "fig_sorensen.pdf", width=9, height=6, paper = "special")
print(sorensen)
dev.off()


horn <- ggplot(sim_data1, aes(x=abs_lat, y=Estimate, shape=comparison, colour=comparison)) +
  geom_errorbar(position=position_dodge(width=0.5), aes(ymin=lci, ymax=uci), width=.2) +
  geom_point(position=position_dodge(width=0.5), size=4)  + ylim(0,1) +
  labs(y = "Horn index", x = "Samples (by absolute Latitude)") +
  theme_bw() + 
  theme(axis.text=element_text(size=18, colour = "black"), 
        axis.title=element_text(size=22, colour = "black"),
        legend.text = element_text(size=18, colour = "black"), 
        legend.title = element_blank(),
        legend.position = c(0.25, 0.15),
        legend.background = element_rect(linetype="solid",size = 0.4, colour ="black")) +
  scale_color_manual(values=c("#a6611a", "#018571", "#999999")) +
  scale_shape_manual(values=c(19, 15, 17)) +
  #scale_y_continuous(breaks = seq(0,1,0.25),limits=c(0,1), expand = c(0.02, 0)) +                         
  scale_x_discrete(labels=c(expression("1"*degree),
                            expression("8"*degree),
                            expression("16"*degree),
                            expression("20"*degree),
                            expression("21"*degree),
                            expression("24"*degree),
                            expression("27"*degree),
                            expression("40"*degree),
                            expression("50"*degree)))

pdf(file = "fig_horn.pdf", width=9, height=6, paper = "special")
print(horn)
dev.off()


chao_sorensen <- ggplot(sim_data_cs, aes(x=abs_lat, y=Estimate, shape=comparison, colour=comparison, group = comparison)) +
  geom_errorbar(position=position_dodge(width=0.5), aes(ymin=lci, ymax=uci), width=.2) +
  geom_point(position=position_dodge(width=0.5), size=4)  + ylim(0,max(sim_data_cs$uci)) +
  labs(y = "Sorensen index", x = "Samples (by absolute Latitude)") +
  theme_bw() + 
  theme(axis.text=element_text(size=18, colour = "black"), 
        axis.title=element_text(size=22, colour = "black"),
        legend.text = element_text(size=18, colour = "black"), 
        legend.title = element_blank(),
        legend.position = c(0.25, 0.15),
        legend.background = element_rect(linetype="solid",size = 0.4,  colour ="black")) +
  scale_color_manual(values=c("#a6611a", "#018571", "#999999")) +
  scale_shape_manual(values=c(19, 15, 17)) +
  #scale_y_continuous(breaks = seq(0,1,0.25),limits=c(0,1), expand = c(0.02, 0)) +                         
  scale_x_discrete(labels=c(expression("1"*degree),
                            expression("8"*degree),
                            expression("16"*degree),
                            expression("20"*degree),
                            expression("21"*degree),
                            expression("24"*degree),
                            expression("27"*degree),
                            expression("40"*degree),
                            expression("50"*degree)))


pdf(file = "fig_chao_sorensen.pdf", width=9, height=6, paper = "special")
print(chao_sorensen)
dev.off()


chao_jaccard <- ggplot(sim_data_cj, aes(x=abs_lat, y=Estimate, shape=comparison, colour=comparison, group = comparison)) +
  geom_errorbar(position=position_dodge(width=0.5), aes(ymin=lci, ymax=uci), width=.2) +
  geom_point(position=position_dodge(width=0.5), size=4)  + ylim(0,max(sim_data_cs$uci)) +
  labs(y = "Sorensen index", x = "Samples (by absolute Latitude)") +
  theme_bw() + 
  theme(axis.text=element_text(size=18, colour = "black"), 
        axis.title=element_text(size=22, colour = "black"),
        legend.text = element_text(size=18, colour = "black"), 
        legend.title = element_blank(),
        legend.position = c(0.25, 0.15),
        legend.background = element_rect(linetype="solid",size = 0.4,  colour ="black")) +
  scale_color_manual(values=c("#a6611a", "#018571", "#999999")) +
  scale_shape_manual(values=c(19, 15, 17)) +
  #scale_y_continuous(breaks = seq(0,1,0.25),limits=c(0,1), expand = c(0.02, 0)) +                         
  scale_x_discrete(labels=c(expression("1"*degree),
                            expression("8"*degree),
                            expression("16"*degree),
                            expression("20"*degree),
                            expression("21"*degree),
                            expression("24"*degree),
                            expression("27"*degree),
                            expression("40"*degree),
                            expression("50"*degree)))


pdf(file = "fig_chao_jaccard.pdf", width=9, height=6, paper = "special")
print(chao_jaccard)
dev.off()


