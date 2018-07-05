############################################################
rm(list=ls())

setwd("/Users/marinacostarillo/Google Drive/PhD/Projects")
setwd("./buckley-bias/analysis")

# Libraries
source("R/library.R")

# Auxiliary functions
sourceDirectory("./R/aux_functions", modifiedOnly=FALSE)

### Data

holocene_full <- read.csv("data_age/ForCenS_woa.csv", header = TRUE)
lgm_full <- read.csv("data_age/LGM_MARGO_renamed.csv", header = TRUE)
hist <- read.csv("data_age/historical_data.csv", header = TRUE)

# Subsetting data to nearest neighbours

i=1

lgm_neighbs <- data.frame()

atl <- find_neighbours(point = as.numeric(resamples_df[i,c("Long","Lat")]), findin = atlantic_lgm[,c("Long","Lat")], distance = 0)
ip <- find_neighbours(point = as.numeric(resamples_df[i,c("Long","Lat")]), findin = indo_pacific_lgm[,c("Long","Lat")], distance = 0)
pac <- find_neighbours(point = as.numeric(resamples_df[i,c("Long","Lat")]), findin = pacific_lgm[,c("Long","Lat")], distance = 0)



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
atlantic_lgm <- read.csv("data/lgm_data/MARGO_Atlantic_LGM_PF.csv", header = TRUE, stringsAsFactors=FALSE)
indo_pacific_lgm <- read.csv("data/lgm_data/MARGO_IndoPac_LGM_PF.csv", header = TRUE, stringsAsFactors=FALSE)
pacific_lgm <- read.csv("data/lgm_data/MARGO_Pacific_LGM_PF.csv", header = TRUE, stringsAsFactors=FALSE)

indo_pacific_lgm <- indo_pacific_lgm [-which(indo_pacific_lgm[,c("Long")]>180),]

## point: vector of two numbers (longitude, latitude) 
## findin: a matrix of 2 columns (first one is longitude, second is latitude) 
## distance: if 0 finds nearest neighbour, if a positive value (in meters) finds neighbours within a circle with the value radius

i=1

lgm_neighbs <- data.frame()

atl <- find_neighbours(point = as.numeric(resamples_df[i,c("Long","Lat")]), findin = atlantic_lgm[,c("Long","Lat")], distance = 0)
ip <- find_neighbours(point = as.numeric(resamples_df[i,c("Long","Lat")]), findin = indo_pacific_lgm[,c("Long","Lat")], distance = 0)
pac <- find_neighbours(point = as.numeric(resamples_df[i,c("Long","Lat")]), findin = pacific_lgm[,c("Long","Lat")], distance = 0)


if(which.min(c(atl$distance, ip$distance, pac$distance))==1){ # Atlantic
  lgm_neighbs[i,] <- cbind(atl, atlantic_lgm[atl$row_findin,])
}

if(which.min(c(atl$distance, ip$distance, pac$distance))==2){ # Indo-Pacific
  
}

if(which.min(c(atl$distance, ip$distance, pac$distance))==3){ # Pacific
  
}



# Old

lgm_df <- get_abund_relat_lgm(overwrite=FALSE)

### Graph Holocene vs. LGM 
plot_similarity(overwrite=F)



