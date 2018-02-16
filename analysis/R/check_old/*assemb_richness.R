
rownames(assemb_counts_df) <- assemb_counts_df$species
rownames(assemb_relat_df) <- assemb_relat_df$species

richness <- data.frame(forcens=integer(), resamples=integer(), buckley=integer())
for (i in samples){ # i=samples[2]
  chao_df <- assemb_counts_df[,c(sprintf("%sC", i), sprintf("%sA", i),sprintf("%sB", i) )]
  names(chao_df) <- c("forcens", "resamples", "buckley")    
  chao_df <- chao_df[!rownames(chao_df) %in% c("unidentified","total_counts"),]
  
  richness <- rbind(richness,colSums(apply(chao_df, 2, function(x) x!=0), na.rm=TRUE))
}
names(richness) <- c("forcens", "resamples", "buckley")
richness <- cbind(samples=samples, richness)
write.csv(richness, file = "Bias_analysis/data_assemblages/summary_richness.csv",row.names=T)









### other dissimilarity indexes

diff <- data.frame(sample = numeric(), bray_dissim = numeric(), L1_dist = numeric(), jaccard = numeric())
n = 1

for(i in samples){
  
  comm <- t(assemb_counts_df[, c(sprintf("%sA",i),sprintf("%sB",i))])
  colnames(comm) <- assemb_counts_df[, c("species")]
  
  diff[n,"sample"] <- i
  
  # Bray-Curtis dissimilarity: abundance
  diff[n,"bray_dissim"] <- sum(abs(comm[1,]-comm[2,])) / (sum(comm[1,]) + sum(comm[2,]))
  
  # L1_distance: relative abundance (absolute differences of their cartesian coordinates)
  diff[n,"L1_dist"] <- sum(abs( (comm[1,]/sum(comm[1,])) - comm[2,]/sum(comm[2,]) )) / 2
  
  # Jaccard dissimilarity: presence/absence
  a <- sum(comm[1,]!=0 & comm[2,]!=0)
  b <- sum(comm[1,]!=0 & comm[2,]==0)
  c <- sum(comm[1,]==0 & comm[2,]!=0)
  
  diff[n,"jaccard"] <-  (b+c)/(a+b+c)
  
  n = n+1
  rm(comm)
}

