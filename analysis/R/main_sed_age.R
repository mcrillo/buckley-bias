# Created by Marina Costa Rillo in 05/July/2018 
rm(list=ls())

setwd("/Users/marinacostarillo/Google Drive/PhD/Projects")
setwd("./buckley-bias/analysis")

# Libraries
library(reshape)
library(geosphere)
library(SpadeR)

# Auxiliary functions
source("./R/aux_functions/find_neighbours.R")

###
### Data
###

historical <- read.csv("data_age/historical_data.csv", header = TRUE)

if (!file.exists("data_age/ForCenS_subset.csv") | !file.exists("data_age/LGM_subset.csv")){
  # Subsetting data to nearest neighbours
  holocene_full <- read.csv("data_age/ForCenS_woa.csv", header = TRUE)
  lgm_full <- read.csv("data_age/LGM_MARGO_renamed.csv", header = TRUE)

  lgm <- data.frame()
  holocene <- data.frame()

  for (i in 1 : nrow(historical)){
    point <- as.numeric(historical[i,c("Long","Lat")])

    lgm_neighb <- find_neighbours(point, findin = lgm_full[,c("Long","Lat")], distance = 0)
    lgm <- rbind(lgm, cbind(sample = historical$sample[i], lgm_neighb, lgm_full[lgm_neighb$row_findin,]))
    
    holocene_neighb <- find_neighbours(point, findin = holocene_full[,c("Longitude","Latitude")], distance = 0)
    holocene <- rbind(holocene, cbind(sample = historical$sample[i], holocene_neighb, holocene_full[holocene_neighb$row_findin,]))
  }

  write.csv(holocene, "data_age/ForCenS_subset.csv", row.names = F)
  write.csv(lgm, "data_age/LGM_subset.csv", row.names = F)

}else{
  holocene <- read.csv("data_age/ForCenS_subset.csv", header = TRUE)
  lgm <- read.csv("data_age/LGM_subset.csv", header = TRUE)
}

# Distance (in km) between historical sample and LGM and ForCenS nearest sample
data.frame(lgm=lgm$distance/1000,holocene=holocene$distance/1000)

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


###
### Analysis
###

sim_samples <- paste(historical$sample[which(historical$sample!="A66")],"_", sep="") # SimilarityPair can only be calculated for assemblages with more than ONE species
sim_holo <- data.frame()
sim_lgm <- data.frame()

for (i in sim_samples){ # i = sim_samples[1]
  
   sim_holo_data <- historical_holocene[,grep(names(historical_holocene), pattern = i)]
   sim_holo_i <- SimilarityPair(as.matrix(sim_holo_data), datatype = c("abundance"), nboot = 50)
   sim_holo <- rbind(sim_holo, cbind(rbind(
     c02 = sim_holo_i$Empirical_richness[1,],
     c12 = sim_holo_i$Empirical_relative[1,],
     c22 = sim_holo_i$Empirical_relative[2,]),
     sample = rep(i, 3)))
   
   sim_lgm_data <- historical_lgm[,grep(names(historical_lgm), pattern = i)]
   sim_lgm_i <- SimilarityPair(as.matrix(sim_lgm_data), datatype = c("abundance"), nboot = 50)
   sim_lgm <- rbind(sim_lgm, cbind(rbind(
     c02 = sim_lgm_i$Empirical_richness[1,],
     c12 = sim_lgm_i$Empirical_relative[1,],
     c22 = sim_lgm_i$Empirical_relative[2,] ),
     sample = rep(i, 3)))

   rm(sim_holo_data)
   rm(sim_holo_i)
   rm(sim_lgm_data)
   rm(sim_lgm_i)
   
}


##############################
### Assemblage Composition ###
##############################

### Data Holocene
cores10 <- c("#00b900","#0037c6", "#ff7314", "#ff60ff","#86442b","#004d41", "#db0011","#00b5ff","#8000b2", "#d0be00")
forcens_df <- get_forcens_subset(resamples_df,overwrite=FALSE) 
# Merging species counts (assemblages) from (A) re-samples , (B) Buckley collection and (C) ForCenS data:
assemb_counts_df <- get_abund_counts(forcens_df, overwrite = FALSE) # creates "data/counts_merged.csv"
assemb_relat_df <- get_abund_relat(forcens_df, overwrite = FALSE) # creates "data/counts_merged_relat.csv






### Analysis Holocene
# Calculating similarity index (based on Chao) for assemblages of Re-sampling X Buckley Collection X ForCenS
assemb_sim_list <- get_assemb_similarity(assemb_counts_df, resamples_df$sample, overwrite = FALSE)
### Plots Holocene
# Histograms of species relative abundances for each datasets (Bias, Buckley, ForCenS), per sample
suppressWarnings(plot_abund_histograms(assemb_relat_df, resamples_df, overwrite = FALSE)) # creates "output/abund_histograms"
# Chao similarity index plot
plot_assemb_similarity(assemb_sim_list, cores10, overwrite = FALSE) # creates "output/assemb_similarity"


### Data LGM
lgm_df <- get_abund_relat_lgm(overwrite=FALSE)

### Graph Holocene vs. LGM 
plot_similarity(overwrite=F)



