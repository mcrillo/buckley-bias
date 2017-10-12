rm(list=ls())

setwd("/Users/marinacostarillo/Google Drive/DOUTORADO/R_data")
setwd("./buckley-bias/analysis")

# Packages
library(ggplot2)
library(sp)
library(maps)
library(maptools)
library(mapproj)
library(geosphere) # function distCosine
library(matrixStats)
library(reshape2) # function melt
library(dplyr)
library(SpadeR) # Chao assemblage similarities package

# General Functions
source("R/operator_not_in.R")
source("R/write_list.R")
source("R/read_list.R")

# Colours for plots
cores10 <- c("#00b900","#0037c6", "#ff7314", "#ff60ff","#86442b","#004d41", "#db0011","#00b5ff","#8000b2", "#d0be00")


### Loading required Data:
wands_df <- read.csv("data/Wandsworth_loans_MRillo.csv", header = TRUE, stringsAsFactors=FALSE)
morpho_df <- read.csv(file=c("data/all_species_morpho.csv"), header = TRUE)
counts_raw_df <- read.csv("data/counts_raw_R.csv", header = TRUE, stringsAsFactors = FALSE)


##########################################
### OBD samples used for bias analysis ###
##########################################

# Getting info (coordinates) of re-sampled sediments
if(file.exists("data/resample_info.csv")){
  resamples_df <- read.csv("data/resample_info.csv", header = TRUE)
}else{
  source("R/get_resamples_info.R")
  source("R/plot_map_resamples.R")
  resamples_df <- suppressWarnings(get_resamples_info(wands_df, morpho_df)) # creates: data/resample_info.csv
  plot_map_resamples(resamples_df) # creates: output/resamples_map.pdf
}


##############################
### Assemblage Composition ###
##############################

###
### Data
###

# Subset ForCenS to nearest neighbours of resamples:
if(file.exists("data/forcens_subset.csv")){
  forcens_df <- read.csv("data/forcens_subset.csv", header = TRUE)
}else{
  source("R/find_neighbours.R") # NEAREST neighbour only for the bias analysis
  source("R/get_forcens_subset.R")
  forcens_df <- read.csv("data/ForCenS_woa.csv", header = TRUE)
  forcens_df <- get_forcens_subset(forcens_df,resamples_df) # creates: data/forcens_coord_dist.csv & data/forcens_subset.csv
}

# Merging species counts (assemblages) from (A) re-samples , (B) Buckley collection and (C) ForCenS data:
source("R/get_abund_counts.R")
source("R/get_abund_relat.R")
assemb_counts_df <- get_abund_counts(counts_raw_df , forcens_df) # creates "data/counts_merged.csv"
assemb_relat_df <- get_abund_relat(counts_raw_df , forcens_df) # creates "data/counts_merged_relat.csv"


###
### Analysis
###

# Calculating similarity index (based on Chao) for assemblages of Re-sampling X Buckley Collection X ForCenS
if(file.exists("output/assemb_similar_chao.xlsx")){
    assemb_sim_list <- function_read_list("output/assemb_similar_chao.xlsx")
}else{
    source("R/get_assemb_similarity.R") # NEAREST neighbour only for the bias analysis
    assemb_sim_list <- get_assemb_similarity(assemb_counts_df, resamples_df$sample)
}


###
### Plots
###

# Histograms of species relative abundances for each datasets (Bias, Buckley, ForCenS), per sample
# creates "output/abund_histograms"
source("R/plot_abund_histograms.R")
suppressWarnings(plot_abund_histograms(assemb_relat_df,resamples_df)) 

# Chao similarity index plot
source("R/plot_assemb_similarity.R")
plot_assemb_similarity(assemb_sim_list, cores10)




#########################
### Size Distribution ###
#########################

###
### Data
###



###
### Analysis
###



###
### Plots
###

