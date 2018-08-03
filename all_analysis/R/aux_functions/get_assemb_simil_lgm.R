### Description
# Calculating similarity index (based on Chao) for assemblages of Re-sampling X Buckley Collection X ForCenS
# creates: output/assemb_similar_chao.xlsx (list)

### Usage
# get_assemb_similarity(lgm_df, overwrite)

### Arguments
# lgm_df : 
# overwrite : TRUE or FALSE (if the output file already exist, do you want to re0run the function and overwrite it?)

# A = my assemblage (Resamples)
# B = Buckley's assemblage
# C = ForCenS
# L = LGM data

get_assemb_simil_lgm <- function(lgm_df, overwrite){
  
  if(file.exists("output/assemb_similar_chao_lgm.csv") && overwrite == FALSE){
    
    lgm_simil_list <- function_read_list("output/assemb_similar_chao_lgm.csv")
    return(lgm_simil_list)
    
  }else{
    
    ### Chao - SpadeR package
    
    rownames(lgm_df) <- lgm_df$species
    
    simchao_AC <- data.frame()
    simchao_AL <- data.frame()
    simchao_LC <- data.frame()
    
    samples <- unique(readr::parse_number(colnames(lgm_df)[2:ncol(lgm_df)]))
    
    print(noquote("Samples:"))
    
    for (i in samples[1:9]){ # i=samples[9]
      print(i)
      
      chao_df <- lgm_df[,c(sprintf("X%sC", i), sprintf("X%sA", i),sprintf("X%sL", i) )]
      names(chao_df) <- c("forcens", "resamples", "lgm")    
      chao_df <- chao_df[!rownames(chao_df) %in% c("unidentified","total_counts"),]
      chao_df[is.na(chao_df)] <- 0 # set abundances to 0 of NA species
      chao_df <- chao_df*100
      richness <- colSums(apply(chao_df, 2, function(x) x!=0), na.rm=TRUE)
      # SimilarityPair can only be calculated for assemblages with more than ONE species
      
      if(all(richness[c("resamples","forcens")]>1)){ 
        simchaoAC <- SimilarityPair(chao_df[,c("resamples","forcens")], datatype = c("abundance"), nboot = 500)
        simchao_AC <- rbind(simchao_AC,cbind(rbind(
          c02 = simchaoAC$Empirical_richness[1,],
          c12 = simchaoAC$Empirical_relative[1,],
          c22 = simchaoAC$Empirical_relative[2,] ),
          sample = rep(i, 3)))
        rm(simchaoAC)
      }
      
      
      if(all(richness[c("resamples","lgm")]>1)){ 
        simchaoAL <- SimilarityPair(chao_df[,c("resamples","lgm")], datatype = c("abundance"), nboot = 500)
        simchao_AL <- rbind(simchao_AL,cbind(rbind(
          c02 = simchaoAL$Empirical_richness[1,],
          c12 = simchaoAL$Empirical_relative[1,],
          c22 = simchaoAL$Empirical_relative[2,] ),
          sample = rep(i, 3)))
        rm(simchaoAL)
      }
      
      
      if(all(richness[c("lgm","forcens")]>1)){ 
        simchaoLC <- SimilarityPair(chao_df[,c("lgm","forcens")], datatype = c("abundance"), nboot = 500)
        simchao_LC <- rbind(simchao_LC,cbind(rbind(
          c02 = simchaoLC$Empirical_richness[1,],
          c12 = simchaoLC$Empirical_relative[1,],
          c22 = simchaoLC$Empirical_relative[2,] ),
          sample = rep(i, 3)))
        rm(simchaoLC)
      }
      
      rm(chao_df)
      rm(richness)
    }
    
    
    # Tranform row-names into column as well
    simchao_AC$CqN <- rownames(simchao_AC)
    simchao_AL$CqN <- rownames(simchao_AL)
    simchao_LC$CqN <- rownames(simchao_LC)
    
    
    # Merging three data.frames into one list:
    lgm_simil_list <- list(resamples_forcens = simchao_AC,  
                            resamples_lgm = simchao_AL,
                            lgm_forcens = simchao_LC)     
    
    function_write_list(lgm_simil_list, "output/assemb_simil_chao_lgm.xlsx")
    
    return(lgm_simil_list)
  }# else
}



