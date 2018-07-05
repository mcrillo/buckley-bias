############################################################
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


### Resample table for manuscript
# table_resample <- read.csv("data/manuscript_resample_info.csv", header = T)
# table_resample
# print(xtable(table_resample), include.rownames=FALSE) # LaTeX



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




#########################
### Size Distribution ###
#########################

###
### Data
###

# Merges resample size data with Buckley's size data, creates files in data/bias_size_analysis
get_size_data(buckley_measurmts, resamp_size_df, overwrite = F) 
# Merges csv files from bias_size_analysis into one data.frame: bias_size_analysis.csv
size_ind_df <- merge_size_data(overwrite = F)
size_pop_df <- get_size_pop_data(size_ind_df, overwrite = F) # summary statistics for each ssp population of size_ind_df
# Re-ordering the factors (species) to be phylogenetically meaningful in the plot (i.e. sister species closer in the plot)
# size_pop_df$species <- factor(size_pop_df$species, levels = c())

data.frame(ssp_resample = unique(size_pop_df[which(size_pop_df[,"datasetAB"] == "A"), "sspname"]))


max_diam <- by(size_ind_df$diam.max, size_ind_df$species, function(x) max(x, na.rm = T))
min_diam <- by(size_ind_df$diam.max, size_ind_df$species, function(x) min(x, na.rm = T))
cbind(max_diam, min_diam)


###
### Analysis & Plots
###

### Individuals
boxplot_size_species(size_ind_df, overwrite = F)
boxplot_size_sample(size_ind_df, overwrite = F)
plot_size_histograms(size_ind_df, overwrite = F)
# test_size_ind(size_ind_df)

### Populations
transf<- c("log") # "sqrt"

# checking order of sspnames by max area_log_95q, for colours gradient
size <- by(size_pop_df$area_log_95q, size_pop_df$sspname, max)
size <- sort(size, decreasing = TRUE)
# names(size)

# Regression plots (mean, median, 75q, 95q max)
regress <- dcast(size_pop_df, sspname+sample~datasetAB, value.var = paste("area_",transf,"_95q",sep=""))
plot95q <- plot_resid_regression(regress, tranfs, name="95q", overwrite=F)

# Calculates residuals from regression based on 1:1 model
resid_list <- get_size_pop_residuals(size_pop_df, file_name = "size_pop_residuals_allssp", overwrite = F)
# resid_list$stats[which.min(resid_list$stats[,paste("mse_",transf,sep="")]),"rownames"]
# Preparing data for residual plots & plots
res <- melt(as.data.frame(resid_list[[transf]]), id = c("species","sspname","sample")) # warning ok 
plot_resid_histogram(res, transf, overwrite = F)
violin <- plot_resid_violin(res, resid_list$stats, transf, overwrite = F) 

# Plot for publication
# Uses violin and plot95q - so set overwrite to TRUE to run the plot below
if (!is.null(violin) & !is.null(plot95q)){
  pdf(file = "output/manuscript_ggarrange.pdf", paper = "special", width=15, height=6)
    print(ggarrange(violin, plot95q, ncol = 2, nrow = 1, align="h", widths = c(0.45, 0.55), 
                    labels = c("(a)", "(b)"), label.x = c(0,-0.013),
                    font.label = list(size = 18, color = "black", face = "bold", family = NULL)))
dev.off()}





###############################
### Ecological Optimum test ###
###############################

### Linear mixed-effects regression
pop_data <- merge_size_abund(size_pop_df, assemb_relat_df, overwrite=F)
lmer_resample_size_pop(pop_data)




