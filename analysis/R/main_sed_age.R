# Created by Marina Costa Rillo in 05/July/2018 
rm(list=ls())

setwd("/Users/marinacostarillo/Google Drive/PhD/Projects")
setwd("./buckley-bias/analysis")

# Libraries
source("R/library.R")

# Auxiliary functions
sourceDirectory("./R/aux_functions", modifiedOnly=FALSE)

### Data
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
    lgm <- rbind(lgm, cbind(lgm_neighb, lgm_full[lgm_neighb$row_findin,]))

    holocene_neighb <- find_neighbours(point, findin = holocene_full[,c("Longitude","Latitude")], distance = 0)
    holocene <- rbind(holocene, cbind(holocene_neighb, holocene_full[holocene_neighb$row_findin,]))
  }

  write.csv(holocene, "data_age/ForCenS_subset.csv", row.names = F)
  write.csv(lgm, "data_age/LGM_subset.csv", row.names = F)

}else{
  holocene <- read.csv("data_age/ForCenS_subset.csv", header = TRUE)
  lgm <- read.csv("data_age/LGM_subset.csv", header = TRUE)
}

# Distance (in km) between historical sample and LGM and ForCenS nearest sample
data.frame(lgm=lgm$distance/1000,holocene=holocene$distance/1000)

### Merging data counts
sort(names(holocene)[24:68])
sort(names(lgm)[19:56])
sort(names(historical)[13:46])

# BEFORE MERGING, CHECK RSpade package and function: how do they want the data?!



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



