rm(list=ls())

setwd("/Users/marinacostarillo/Google Drive/PhD/Projects")
setwd("./buckley-bias/analysis")

# Libraries
source("R/library.R")

# Auxiliary functions
sourceDirectory("./R/aux_functions", modifiedOnly=FALSE)

# General data
buckley_measurmts <- read.csv(file=c("data/buckley_measurmts.csv"), header = TRUE, stringsAsFactors = FALSE)
wands_df <- read.csv("data/Wandsworth_loans_MRillo.csv", header = TRUE, stringsAsFactors=FALSE)
resamples_df <- suppressWarnings(get_resamples_info(buckley_measurmts, wands_df, overwrite = T)) # creates: data/resample_info.csv & data/resample_info_morpho.csv     
resamp_size_df <- read.csv("data/resample_info_size.csv", header = TRUE, stringsAsFactors=FALSE)

plot_map_resamples(resamples_df, overwrite = FALSE) # creates: output/resamples_map.pdf



##############################
### Assemblage Composition ###
##############################

# Colours for plots
cores10 <- c("#00b900","#0037c6", "#ff7314", "#ff60ff","#86442b","#004d41", "#db0011","#00b5ff","#8000b2", "#d0be00")

### Data Holocene
forcens_df <- get_forcens_subset(resamples_df,overwrite=FALSE) 
# Merging species counts (assemblages) from (A) re-samples , (B) Buckley collection and (C) ForCenS data:
assemb_counts_df <- get_abund_counts(forcens_df, overwrite = FALSE) # creates "data/counts_merged.csv"
assemb_relat_df <- get_abund_relat(forcens_df, overwrite = FALSE) # creates "data/counts_merged_relat.csv"

### Analysis
# Calculating similarity index (based on Chao) for assemblages of Re-sampling X Buckley Collection X ForCenS
assemb_sim_list <- get_assemb_similarity(assemb_counts_df, resamples_df$sample, overwrite = FALSE)

### Plots
# Histograms of species relative abundances for each datasets (Bias, Buckley, ForCenS), per sample
suppressWarnings(plot_abund_histograms(assemb_relat_df, resamples_df, overwrite = FALSE)) # creates "output/abund_histograms"
# Chao similarity index plot
plot_assemb_similarity(assemb_sim_list, cores10) # creates "output/assemb_similarity"



#########################
### Size Distribution ###
#########################

### Data
# Creates files in data/bias_size_analysis
get_size_data(buckley_measurmts, resamp_size_df, overwrite = F) 
# Merges csv files from bias_size_analysis into one data.frame: bias_size_analysis.csv
size_ind_df <- merge_size_data(overwrite = F)
size_pop_df <- get_size_pop_data(size_ind_df, overwrite = F) # summary statistics for each ssp population of size_ind_df

### Analysis & Plots

# Individuals
boxplot_size_species(size_ind_df, overwrite = F)
boxplot_size_sample(size_ind_df, overwrite = F)
plot_size_histograms(size_ind_df, overwrite = F)
# test_size_ind(size_ind_df)

# Populations
resid_list <- get_size_pop_residuals(size_pop_df, file_name = "size_pop_residuals_allssp", overwrite = T)

transf<- "log" # "sqrt"
resid_list$stats[which.min(resid_list$stats[,paste("rss_",transf,sep="")]),"rownames"]
res <- melt(as.data.frame(resid_list[[transf]])) # warning ok 
plot_resid_histogram(res, transf, overwrite = F)
plot_resid_violin(res, transf, overwrite = F) 


