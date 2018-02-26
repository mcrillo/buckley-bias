### Description
# merges relative abundance with populational size metrics for species in the re-samples (bias analysis)

### Usage
# merge_size_abund(size_pop_df, assemb_relat_df, overwrite = TRUE/FALSE)

### Arguments
# size_pop_df
# assemb_relat_df
# overwrite : TRUE re-run anyway, FALSE do not re-run if file already exist


merge_size_abund <- function(size_pop_df, assemb_relat_df, overwrite){ 
  
  if (!file.exists("data/resample_size_abund.csv") | overwrite == TRUE){
    
      # subsetting just re-sample assemblages (A)
      abund_data <- assemb_relat_df[, c(1,  grep("A",names(assemb_relat_df)))]
      pop_data <- size_pop_df[which(size_pop_df$datasetAB == "A"),]
      
      # species which I measured the size
      keep <- c("Globigerinella_calida",
                "Globigerinoides_conglobatus",
                "Globigerina_falconensis",
                "Globigerinita_glutinata",
                "Globoconella_inflata",
                "Pulleniatina_obliquiloculata",
                "Globigerinoides_ruber",
                "Trilobatus_sacculifer",
                "Globigerinoides_tenellus",
                "Globorotalia_truncatulinoides",
                "Orbulina_universa",
                "Neogloboquadrina_dutertrei",  
                "Globorotalia_menardii",
                "Globigerinella_siphonifera",
                "Globoturborotalita_rubescens",
                "Globorotalia_scitula",
                "Globorotalia_tumida",
                "Globorotalia_crassaformis",
                "Turborotalita_humilis",
                "Neogloboquadrina_pachyderma")
      
      abund_data <- abund_data[which(abund_data[,"species"] %in% keep),]
      abund_data$species <- factor(abund_data$species)
      names(abund_data) <- c("species", 5, 9, 18, 23, 25, 29, 42, 44, 53, 64) # new sample number
      
      
      for (i in 1:length(pop_data[, 1])){
        
        row_abund <- grep(pattern = pop_data[i, "species"], x = abund_data[,"species"])
        col_abund <- which(names(abund_data) == pop_data[i, "sample"])
        
        pop_data[i,"rel_abund"] <-  abund_data[row_abund, col_abund] 
      }
      
      write.csv(pop_data, "data/resample_size_abund.csv", row.names=FALSE)
      return(pop_data)
      
      
  }else{
    
    pop_data <- read.csv("data/resample_size_abund.csv", header=TRUE)
    return(pop_data)
    
  }
  
}