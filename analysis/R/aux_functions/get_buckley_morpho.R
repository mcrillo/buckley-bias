### Description
# Subset samples (by species) from size-distrib-forams project used for bias analysis

### Arguments
# morpho_df
# resamples_df

get_buckley_morpho <- function(morpho_df, resamples_df){
  if (!file.exists("data/morpho_all")){
    dir.create("data/morpho_all")
  }
  for (j in resamples_df$sample){ # j = resamples_df$sample[3]
    for (i in unique(morpho_df$species)){ # i = unique(morpho_df$species)[11]
      rows <- intersect(which(morpho_df["sample"]==j), which(morpho_df["species"]==i))
      if (length(rows) != 0){
        data <- data.frame(area=morpho_df[rows,"area"], diam.max=morpho_df[rows,"diameter"])
        write.csv(data, file = paste("data/morpho_all/",i,j,"B.csv",sep=""),row.names=FALSE)
        rm(data)
      }
      rm(rows)
    }
  }
}

