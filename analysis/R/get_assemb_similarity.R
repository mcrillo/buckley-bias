
get_assemb_similarity <- function(assemb_counts_df, samples){

      ### Chao - SpadeR package
      
      rownames(assemb_counts_df) <- assemb_counts_df$species
      
      simchao_AC <- data.frame()
      simchao_AB <- data.frame()
      simchao_BC <- data.frame()
      
      print(noquote("Samples:"))
      
      for (i in samples){ # i=samples[2]
            print(i)
            
            chao_df <- assemb_counts_df[,c(sprintf("%sC", i), sprintf("%sA", i),sprintf("%sB", i) )]
            names(chao_df) <- c("forcens", "resamples", "buckley")    
            chao_df <- chao_df[!rownames(chao_df) %in% c("unidentified","total_counts"),]
            chao_df[is.na(chao_df)] <- 0 # set abundances to 0 of NA species
            
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
            
            
            if(all(richness[c("resamples","buckley")]>1)){ 
                simchaoAB <- SimilarityPair(chao_df[,c("resamples","buckley")], datatype = c("abundance"), nboot = 500)
                simchao_AB <- rbind(simchao_AB,cbind(rbind(
                  c02 = simchaoAB$Empirical_richness[1,],
                  c12 = simchaoAB$Empirical_relative[1,],
                  c22 = simchaoAB$Empirical_relative[2,] ),
                  sample = rep(i, 3)))
                rm(simchaoAB)
            }
            
            
            if(all(richness[c("buckley","forcens")]>1)){ 
                  simchaoBC <- SimilarityPair(chao_df[,c("buckley","forcens")], datatype = c("abundance"), nboot = 500)
                  simchao_BC <- rbind(simchao_BC,cbind(rbind(
                    c02 = simchaoBC$Empirical_richness[1,],
                    c12 = simchaoBC$Empirical_relative[1,],
                    c22 = simchaoBC$Empirical_relative[2,] ),
                    sample = rep(i, 3)))
                  rm(simchaoBC)
            }

            rm(chao_df)
            rm(richness)
      }

      
      # Tranform row-names into column as well
      simchao_AC$CqN <- rownames(simchao_AC)
      simchao_AB$CqN <- rownames(simchao_AB)
      simchao_BC$CqN <- rownames(simchao_BC)
      
      
      # Merging three data.frames into one list:
      assemb_sim_list <- list(resamples_forcens = simchao_AC,  
                                resamples_buckley = simchao_AB,
                                buckley_forcens = simchao_BC)     

      function_write_list(assemb_sim_list, "output/assemb_similar_chao.xlsx")
        
      return(assemb_sim_list)
        
}



