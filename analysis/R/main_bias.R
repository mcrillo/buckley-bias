rm(list=ls())

setwd("/Users/marinacostarillo/Google Drive/PhD/Projects")
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
morpho_size_distrib <- read.csv(file=c("data/morpho_size-distrib.csv"), header = TRUE, stringsAsFactors = FALSE)

# Extracting info of re-sampled sediments
resamples_df <- suppressWarnings(get_resamples_info(morpho_size_distrib, overwrite = FALSE)) # creates: data/resample_info.csv & data/resample_info_morpho.csv     
plot_map_resamples(resamples_df, overwrite = FALSE) # creates: output/resamples_map.pdf


# Data for assemblage similarity analysis
forcens_df <- get_forcens_subset(resamples_df,overwrite=FALSE) 


##############################
### Assemblage Composition ###
##############################

### Data
# Merging species counts (assemblages) from (A) re-samples , (B) Buckley collection and (C) ForCenS data:
assemb_counts_df <- get_abund_counts(forcens_df) # creates "data/counts_merged.csv"
assemb_relat_df <- get_abund_relat(forcens_df) # creates "data/counts_merged_relat.csv"

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
if(!file.exists("data/morpho_bias-analysis.csv")){
  resamp_morpho_df <- read.csv("data/resample_info_morpho.csv", header = TRUE, stringsAsFactors=FALSE)
  # zf_all <- get_buckley_zf(resamples_df) # all slides ZFs numbers from Buckley Collection used in the bias analysis
  # creating one CSV for each species for each sample (data/morpho_R/.)
  get_morpho_buckley(morpho_size_distrib, resamp_morpho_df)
  get_morpho_bias() # requires CSV files from folder data/morpho_bias_buckley
  morpho_df <- merge_morpho()
}else{
  morpho_df <-read.csv("data/morpho_bias-analysis.csv", header = TRUE, stringsAsFactors=FALSE)
}





ssp.names <- c("G.calida","G.conglobatus","G.crassaformis","S.dehiscens","N.dutertrei","G.falconensis",
               "G.glutinata","T.humilis","G.inflata","G.menardii","P.obliquiloculata","N.pachyderma",
               "G.ruber","G.rubescens","G.sacculifer","G.scitula","G.siphonifera","G.tenellus",
               "G.truncatulinoides","G.tumida","O.universa")

ssp.grep <- c("calida","conglobatus","crassaformis","dehiscens","dutertrei","falconensis",
              "glutinata","humilis","inflata","menardii","obliquiloculata","pachyderma",
              "ruber","rubescens","sacculifer","scitula","siphonifera","tenellus",
              "truncatulinoides","tumida", "universa") 
sspname = rep(ssp.names[grep(gsub( "[1-9].*$", "", k ),ssp.grep)], length(data[,1])), 



  
###
### Analysis
###



###
### Plots
###

