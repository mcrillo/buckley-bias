# Created by Marina Costa Rillo in 05/July/2018 
rm(list=ls())

setwd("/Users/marinacostarillo/Google Drive/PhD/Projects")
setwd("./buckley-bias/short_paper")

# Libraries
library(reshape)
library(geosphere)
library(SpadeR)
library(ggplot2)

# Auxiliary functions
find_neighbours <- function(point, findin, distance) { # vector, data.frame, numeric
  ## point: vector of two numbers (longitude, latitude) 
  ## findin: a matrix of 2 columns (first one is longitude, second is latitude) 
  ## distance: if 0 finds nearest neighbour, if a positive value (in meters) finds neighbours within a circle with the value radius
  
  ## Returns a data.frame: 
  ## "row" = the row number of the neighbour in data.frame
  ## "distance" = the distance between the points
  
  dist_data <- apply(findin, 1, function(x) distCosine(point, x)) #Matrix
  
  if(distance>0) { # find neighbours within radius of distance
    neighb <- data.frame(row_findin = which(dist_data<=distance), distance = dist_data[which(dist_data<=distance)])
    if(length(neighb[,1])==0) distance = 0 
  }  
  
  if(distance==0) { # find nearest neighbour
    neighb <- data.frame(row_findin = which.min(dist_data), distance = min(dist_data))
  }
  
  return(neighb)
  
}


###
### Data
###

historical <- read.csv("historical_data.csv", header = TRUE)

if (!file.exists("ForCenS_subset.csv") | !file.exists("LGM_subset.csv")){
  # Subsetting data to nearest neighbours
  holocene_full <- read.csv("ForCenS_woa.csv", header = TRUE)
  lgm_full <- read.csv("LGM_MARGO_renamed.csv", header = TRUE)

  lgm <- data.frame()
  holocene <- data.frame()

  for (i in 1 : nrow(historical)){
    point <- as.numeric(historical[i,c("Long","Lat")])

    lgm_neighb <- find_neighbours(point, findin = lgm_full[,c("Long","Lat")], distance = 0)
    lgm <- rbind(lgm, cbind(sample = historical$sample[i], lgm_neighb, lgm_full[lgm_neighb$row_findin,]))
    
    holocene_neighb <- find_neighbours(point, findin = holocene_full[,c("Longitude","Latitude")], distance = 0)
    holocene <- rbind(holocene, cbind(sample = historical$sample[i], holocene_neighb, holocene_full[holocene_neighb$row_findin,]))
  }

  write.csv(holocene, "ForCenS_subset.csv", row.names = F)
  write.csv(lgm, "LGM_subset.csv", row.names = F)

}else{
  holocene <- read.csv("ForCenS_subset.csv", header = TRUE)
  lgm <- read.csv("LGM_subset.csv", header = TRUE)
}

# Distance (in km) between historical sample and LGM and ForCenS nearest sample
data.frame(sample=historical$sample, 
           lat=historical$Lat,
           sed_cm_ky=lgm$sedimentation.rate..cm.ky., 
           lgm=lgm$distance/1000,
           holocene=holocene$distance/1000, 
           dist_diff = (holocene$distance - lgm$distance)/1000)

distances <- read.csv("distances_datasets.csv", header = T)

###
### Organizating data for analysis
###

# Calculating relative abundance
holocene_species <- (holocene[25:69]/rowSums(holocene[25:69], na.rm = T))*100; rowSums(holocene_species, na.rm=T)
lgm_species <- (lgm[20:57]/rowSums(lgm[20:57], na.rm = T))*100; rowSums(lgm_species, na.rm=T)
historical_species <- (historical[13:46]/rowSums(historical[13:46], na.rm = T))*100; rowSums(historical_species, na.rm=T)

# Removing species that did not occur
holocene_species <- holocene_species[,-which(as.numeric(colSums(holocene_species, na.rm = T))==0)]
lgm_species <- lgm_species[,-which(as.numeric(colSums(lgm_species, na.rm = T))==0)]
historical_species <- historical_species[,-which(as.numeric(colSums(historical_species, na.rm = T))==0)]

# Solving columns with more than one species

# (1) Globorotalia_menardii_._Globorotalia_tumida
#holocene_species[,unique(c(grep(c("menardii"),colnames(holocene_species)), grep(c("tumida"),colnames(holocene_species))))]
#lgm_species[,unique(c(grep(c("menardii"),colnames(lgm_species)), grep(c("tumida"),colnames(lgm_species))))]
#historical_species[,unique(c(grep(c("menardii"),colnames(historical_species)), grep(c("tumida"),colnames(historical_species))))]
holocene_species[which(!is.na(holocene_species$Globorotalia_menardii_._Globorotalia_tumida)), c("Globorotalia_menardii","Globorotalia_tumida")] <- rep(holocene_species[which(!is.na(holocene_species$Globorotalia_menardii_._Globorotalia_tumida)), c("Globorotalia_menardii_._Globorotalia_tumida")]/2,2)
lgm_species[which(!is.na(lgm_species$Globorotalia_menardii_._Globorotalia_tumida)), c("Globorotalia_menardii","Globorotalia_tumida")] <- rep(lgm_species[which(!is.na(lgm_species$Globorotalia_menardii_._Globorotalia_tumida)), c("Globorotalia_menardii_._Globorotalia_tumida")]/2,2)
holocene_species <- holocene_species[,-which(colnames(holocene_species)=="Globorotalia_menardii_._Globorotalia_tumida")]
lgm_species <- lgm_species[,-which(colnames(lgm_species)=="Globorotalia_menardii_._Globorotalia_tumida")]

# (2) Turborotalita_humilis_._Berggrenia_pumilio
#holocene_species[,unique(c(grep(c("humilis"),colnames(holocene_species)), grep(c("pumilio"),colnames(holocene_species))))]
#lgm_species[,unique(c(grep(c("humilis"),colnames(lgm_species)), grep(c("pumilio"),colnames(lgm_species))))]
#historical_species[,unique(c(grep(c("humilis"),colnames(historical_species)), grep(c("pumilio"),colnames(historical_species))))]
lgm_species[, "Turborotalita_humilis"] <- rowSums(data.frame(lgm_species[, "Turborotalita_humilis"],lgm_species[, "Turborotalita_humilis_._Berggrenia_pumilio"]), na.rm = T)
lgm_species <- lgm_species[,-which(colnames(lgm_species)=="Turborotalita_humilis_._Berggrenia_pumilio")]


# (3) unidentified
holocene_species <- holocene_species[,-which(colnames(holocene_species)=="unidentified")]
lgm_species <- lgm_species[,-which(colnames(lgm_species)=="unidentified")]



# Filling missing species in each data set
#sort(names(holocene_species))
#sort(names(lgm_species))
#sort(names(historical_species))

holocene_species$Dentigloborotalia_anfracta <- 0
historical_species$Dentigloborotalia_anfracta <- 0

lgm_species$Globigerinella_adamsi <- 0
historical_species$Globigerinella_adamsi <- 0

holocene_species$Globorotalia_theyeri <- 0
lgm_species$Globorotalia_theyeri <- 0

holocene_species$Globorotalia_ungulata <- 0
lgm_species$Globorotalia_ungulata <- 0

lgm_species$Hastigerinella_digitata <- 0
historical_species$Hastigerinella_digitata <- 0

# data.frame(sort(names(holocene_species)), sort(names(lgm_species)), sort(names(historical_species)))

# Transforming NA into 0
holocene_species[is.na(holocene_species)] <- 0
lgm_species[is.na(lgm_species)] <- 0
historical_species[is.na(historical_species)] <- 0


# Merging historical data with holocene and LGM
holocene_species$sample <- paste(historical$sample,"_holocene", sep="")
lgm_species$sample <- paste(historical$sample,"_lgm", sep="")
historical_species$sample <- paste(historical$sample,"_hist", sep="")


# Spade R data: species (in rows) by community (in columns) matrix
historical_holocene <- rbind(historical_species,holocene_species)
historical_holocene <- melt(historical_holocene, id = "sample")
historical_holocene <- cast(historical_holocene, variable~sample)
historical_holocene <- historical_holocene[,-grep(names(historical_holocene), pattern = "A66")] # SimilarityPair can only be calculated for assemblages with more than ONE species
row.names(historical_holocene) <- historical_holocene$variable
historical_holocene <- historical_holocene[,-which(names(historical_holocene)=="variable")]

historical_lgm <- rbind(historical_species,lgm_species)
historical_lgm <- melt(historical_lgm, id = "sample")
historical_lgm <- cast(historical_lgm, variable~sample)
historical_lgm <- historical_lgm[,-grep(names(historical_lgm), pattern = "A66")] # SimilarityPair can only be calculated for assemblages with more than ONE species
row.names(historical_lgm) <- historical_lgm$variable
historical_lgm <- historical_lgm[,-which(names(historical_lgm)=="variable")]


holocene_lgm <- rbind(holocene_species,lgm_species)
holocene_lgm <- melt(holocene_lgm, id = "sample")
holocene_lgm <- cast(holocene_lgm, variable~sample)
holocene_lgm <- holocene_lgm[,-grep(names(holocene_lgm), pattern = "A66")] # SimilarityPair can only be calculated for assemblages with more than ONE species
row.names(holocene_lgm) <- holocene_lgm$variable
holocene_lgm <- holocene_lgm[,-which(names(holocene_lgm)=="variable")]



###
### Analysis
###

if (!file.exists("similarity.csv")){
  
  sim_samples <- paste(historical$sample[which(historical$sample!="A66")],"_", sep="") # SimilarityPair can only be calculated for assemblages with more than ONE species
  sim_holo <- data.frame()
  sim_lgm <- data.frame()
  sim_null <- data.frame()
  
  for (i in sim_samples){ # i = sim_samples[1]
    
    sim_holo_data <- historical_holocene[,grep(names(historical_holocene), pattern = i)]
    sim_holo_i <- SimilarityPair(as.matrix(sim_holo_data), datatype = c("abundance"))
    sim_holo <- rbind(sim_holo, 
                      cbind(rbind(c02 = sim_holo_i$estimated_richness[1,],
                                  c12 = sim_holo_i$estimated_relative[1,],
                                  c22 = sim_holo_i$estimated_relative[2,],
                                  chao_jaccard  = sim_holo_i$estimated_relative[4,],
                                  chao_sorensen = sim_holo_i$estimated_relative[5,]),
                            sample = rep(i, 5)))
    
    sim_lgm_data <- historical_lgm[,grep(names(historical_lgm), pattern = i)]
    sim_lgm_i <- SimilarityPair(as.matrix(sim_lgm_data), datatype = c("abundance"))
    sim_lgm <- rbind(sim_lgm, 
                     cbind(rbind(c02 = sim_lgm_i$estimated_richness[1,],
                                 c12 = sim_lgm_i$estimated_relative[1,],
                                 c22 = sim_lgm_i$estimated_relative[2,],
                                 chao_jaccard  = sim_lgm_i$estimated_relative[4,],
                                 chao_sorensen = sim_lgm_i$estimated_relative[5,]),
                           sample = rep(i, 5)))
    
    sim_null_data <- holocene_lgm[,grep(names(holocene_lgm), pattern = i)]
    sim_null_i <- SimilarityPair(as.matrix(sim_null_data), datatype = c("abundance"))
    sim_null <- rbind(sim_null, 
                      cbind(rbind(c02 = sim_null_i$estimated_richness[1,],
                                  c12 = sim_null_i$estimated_relative[1,],
                                  c22 = sim_null_i$estimated_relative[2,],
                                  chao_jaccard  = sim_null_i$estimated_relative[4,],
                                  chao_sorensen = sim_null_i$estimated_relative[5,]),
                            sample = rep(i, 5)))
    
    rm(sim_holo_data)
    rm(sim_holo_i)
    rm(sim_lgm_data)
    rm(sim_lgm_i)
    rm(sim_null_data)
    rm(sim_null_i)
    
  }
  
  sim_holo[,c(1:4)] <- sapply(sim_holo[,c(1:4)], function(x) as.numeric(as.character(x)))
  sim_lgm[,c(1:4)]  <- sapply(sim_lgm[,c(1:4)], function(x) as.numeric(as.character(x)))
  sim_null[,c(1:4)] <- sapply(sim_null[,c(1:4)], function(x) as.numeric(as.character(x)))
  
  sim_holo$comparison <- "Historical and Holocene"
  sim_lgm$comparison  <- "Historical and LGM"
  sim_null$comparison <- "Holocene and LGM"
  
  similarity <- rbind(sim_holo,sim_lgm,sim_null)
  similarity$sample <- as.factor(sub("_", "", similarity$sample))
  similarity$comparison <- as.factor(similarity$comparison)
  similarity$index <- row.names(similarity)
  similarity <- merge(similarity,historical[,c("sample", "Lat", "Long")])
  
  names(similarity) <- c("sample","Estimate", "se", "lci", "uci", "comparison", "index", "lat", "long")
  
  write.csv(similarity, "similarity.csv", row.names = FALSE)

}else{
  similarity <- read.csv("similarity.csv", header = TRUE)
}


###
### Plot
###

sim_data0 <- similarity[grep("c02", similarity$index),] # Sorensen index (richness)
sim_data1 <- similarity[grep("c12", similarity$index),] # Horn index (relative abundance, abundant)
sim_data2 <- similarity[grep("c22", similarity$index),] # Morisita-Horn index (relative abundance, rare)
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

sim_data2$abs_lat <- round(abs(sim_data2$lat))
sim_data2 <- sim_data2[order(sim_data2$abs_lat),]
sim_data2$abs_lat <- round(abs(sim_data2$lat))
sim_data2$abs_lat <- as.factor(sim_data2$abs_lat)

sim_data_cs$abs_lat <- round(abs(sim_data_cs$lat))
sim_data_cs <- sim_data_cs[order(sim_data_cs$abs_lat),]
sim_data_cs$abs_lat <- round(abs(sim_data_cs$lat))
sim_data_cs$abs_lat <- as.factor(sim_data_cs$abs_lat)

sim_data_cj$abs_lat <- round(abs(sim_data_cj$lat))
sim_data_cj <- sim_data_cj[order(sim_data_cj$abs_lat),]
sim_data_cj$abs_lat <- round(abs(sim_data_cj$lat))
sim_data_cj$abs_lat <- as.factor(sim_data_cj$abs_lat)

  
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


# Morista-Horn 
sim_data2$comparison <- sub(pattern = "vs.", replacement = "and", x=sim_data2$comparison)
morisita_horn <- ggplot(sim_data2, aes(x=abs_lat, y=Estimate, shape=comparison, colour=comparison)) +
  geom_errorbar(position=position_dodge(width=0.5), aes(ymin=lci, ymax=uci), width=.2) +
  geom_point(position=position_dodge(width=0.5), size=4.5)  + ylim(0,1) +
  labs(y = "Compositional similarity", x = "Samples (by absolute latitude)") +
  theme_bw() + 
  theme(axis.text=element_text(size=20, colour = "black"), 
        axis.title=element_text(size=22, colour = "black"),
        legend.text = element_text(size=18, colour = "black"), 
        legend.title = element_blank(), 
        legend.position = c(0.25, 0.15),
        legend.background = element_rect(linetype="solid", size = 0.4, colour ="black")) +
  scale_color_manual(values=c("#a6611a", "#018571","#999999")) +
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


pdf(file = "fig_morisita-horn.pdf", width=9, height=6, paper = "special")
  print(morisita_horn)
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

