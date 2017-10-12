rm(list=ls())

setwd("/Users/marinacostarillo/Google Drive/DOUTORADO/Projects")
setwd("./buckley-bias/analysis")

# Libraries
source("R/library.R")

# Auxiliary functions
sourceDirectory("./R/aux_functions", modifiedOnly=FALSE)

# Colours for plots
cores10 <- c("#00b900","#0037c6", "#ff7314", "#ff60ff","#86442b","#004d41", "#db0011","#00b5ff","#8000b2", "#d0be00")

############
### Data ###
############

# General required Data:
wands_df <- read.csv("data/Wandsworth_loans_MRillo.csv", header = TRUE, stringsAsFactors=FALSE)
morpho_df <- read.csv(file=c("data/all_species_morpho.csv"), header = TRUE, stringsAsFactors = FALSE)
counts_raw_df <- read.csv("data/counts_raw_R.csv", header = TRUE, stringsAsFactors = FALSE)

# Data for assemblage similarity analysis
forcens_df <- get_forcens_subset(resamples_df,overwrite=FALSE) 
# requires: data/ForCenS_woa.csv"
# creates: data/forcens_coord_dist.csv & data/forcens_subset.csv
 
# Extracting info of re-sampled sediments
resamples_df <- suppressWarnings(get_resamples_info(wands_df, morpho_df, overwrite = FALSE)) # creates: data/resample_info.csv & data/resample_info_morpho.csv     
plot_map_resamples(resamples_df, overwrite = FALSE) # creates: output/resamples_map.pdf


##############################
### Assemblage Composition ###
##############################

### Data
# Merging species counts (assemblages) from (A) re-samples , (B) Buckley collection and (C) ForCenS data:
assemb_counts_df <- get_abund_counts(counts_raw_df , forcens_df) # creates "data/counts_merged.csv"
assemb_relat_df <- get_abund_relat(counts_raw_df , forcens_df) # creates "data/counts_merged_relat.csv"

### Analysis
# Calculating similarity index (based on Chao) for assemblages of Re-sampling X Buckley Collection X ForCenS
assemb_sim_list <- get_assemb_similarity(assemb_counts_df, resamples_df$sample, overwrite = FALSE)

### Plots
# Histograms of species relative abundances for each datasets (Bias, Buckley, ForCenS), per sample
suppressWarnings(plot_abund_histograms(assemb_relat_df, resamples_df)) # creates "output/abund_histograms"
# Chao similarity index plot
plot_assemb_similarity(assemb_sim_list, cores10) # creates "output/assemb_similarity"




#########################
### Size Distribution ###
#########################

### Data
resamp_morpho_df <- read.csv("data/resample_info_morpho.csv", header = TRUE, stringsAsFactors=FALSE)
names(morpho_df)
resamples <- resamples_df$sample

ssp.names <- c("G.calida","G.conglobatus","G.crassaformis","S.dehiscens","N.dutertrei","G.falconensis",
               "G.glutinata","T.humilis","G.inflata","G.menardii","P.obliquiloculata","N.pachyderma",
               "G.ruber","G.rubescens","G.sacculifer","G.scitula","G.siphonifera","G.tenellus",
               "G.truncatulinoides","G.tumida","O.universa")

ssp.grep <- c("calida","conglobatus","crassaformis","dehiscens","dutertrei","falconensis",
              "glutinata","humilis","inflata","menardii","obliquiloculata","pachyderma",
              "ruber","rubescens","sacculifer","scitula","siphonifera","tenellus",
              "truncatulinoides","tumida", "universa") 



zf_all <- get_buckley_zf(resamples_df) # all slides ZFs numbers from Buckley Collection used in the bias analysis
get_buckley_morpho(morpho_df, resamples_df) # creates one CSV for each species for each sample (data/morphometrics/___buckley)

  
###
### Analysis
###



###
### Plots
###

