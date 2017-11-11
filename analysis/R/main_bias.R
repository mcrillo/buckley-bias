rm(list=ls())

setwd("/Users/marinacostarillo/Google Drive/PhD/Projects")
setwd("./buckley-bias/analysis")

# Libraries
source("R/library.R")

# Auxiliary functions
sourceDirectory("./R/aux_functions", modifiedOnly=FALSE)

# Colours for plots
cores10 <- c("#00b900","#0037c6", "#ff7314", "#ff60ff","#86442b","#004d41", "#db0011","#00b5ff","#8000b2", "#d0be00")

# Extracting info of re-sampled sediments
morpho_size_distrib <- read.csv(file=c("data/morpho_size-distrib.csv"), header = TRUE, stringsAsFactors = FALSE)
resamples_df <- suppressWarnings(get_resamples_info(morpho_size_distrib, overwrite = FALSE)) # creates: data/resample_info.csv & data/resample_info_morpho.csv     
# plot_map_resamples(resamples_df, overwrite = FALSE) # creates: output/resamples_map.pdf



##############################
### Assemblage Composition ###
##############################

### Data
forcens_df <- get_forcens_subset(resamples_df,overwrite=FALSE) 
# Merging species counts (assemblages) from (A) re-samples , (B) Buckley collection and (C) ForCenS data:
assemb_counts_df <- get_abund_counts(forcens_df, overwrite = FALSE) # creates "data/counts_merged.csv"
assemb_relat_df <- get_abund_relat(forcens_df, overwrite = FALSE) # creates "data/counts_merged_relat.csv"

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

###
### Data
###

if(!file.exists("data/morpho_bias-analysis.csv")){
  resamp_morpho_df <- read.csv("data/resample_info_morpho.csv", header = TRUE, stringsAsFactors=FALSE)
  # zf_all <- get_buckley_zf(resamples_df) # all slides ZFs numbers from Buckley Collection used in the bias analysis
  get_size_data_buckley(morpho_size_distrib, resamp_morpho_df) # creates data/raw_data_morpho_R
  get_size_data_bias() # requires CSV files from folder data/raw_data/morpho_bias_buckley
  morpho_df <- merge_size_data()
}else{
  morpho_df <-read.csv("data/morpho_bias-analysis.csv", header = TRUE, stringsAsFactors=FALSE)
}

morpho_stats <- get_size_pop_stats(morpho_df) # summary statistics for each ssp population of morpho_df

###
### Analysis & Plots
###

### Individuals
boxplot_size_species(morpho_df)
boxplot_size_sample(morpho_df)
# test_size_ind(morpho_df)


### Populations
# test_size_pop(morpho_stats)


###
### Project size-distrib-forams
###
species_names <- species_names[-which(species_names == "dehiscens")] # S. dehiscens has too few samples
get_bias_size-distrib_project(morpho_df,morpho_stats,species_names) 



  


