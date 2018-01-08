### Description
# Finds neighbours in ForCenS (and matches species names)
# requires: data/forcens_woa.csv
# creates: data/forcens_coord_dist.csv & data/forcens_subset.csv

### Usage
# 

### Arguments
# resamples_df:
# overwrite : TRUE or FALSE (if the output file already exist, do you want to re0run the function and overwrite it?)


get_forcens_subset <- function(resamples_df, overwrite){ 
  
      if(file.exists("data/forcens_subset.csv") && overwrite == FALSE){
        
        forcens_df <- read.csv("data/forcens_subset.csv", header = TRUE)
        return(forcens_df)
        
      }else{
        
        forcens_df <- read.csv("data/raw_data/forcens_woa.csv", header = TRUE)
      
        # Joining Buckley + Resample + forcens (all transformed into relative abundance data)
        coord_dist <- data.frame()
        neighbours <- data.frame()
        print(noquote("Samples:"))
        
        for (i in resamples_df$sample){
              print(i)
              neighbours_i <- find_neighbours(point = resamples_df[which(resamples_df[,"sample"]==i),c("Long","Lat")], 
                                                     findin = forcens_df[,c("Longitude","Latitude")], distance = 0) # only nearest neightbours (for more neighbours, see old code assemblage_data.R from March 2017)
              neighbours_i <- cbind(sample = rep(i, length(neighbours_i[,1])), neighbours_i)
              neighbours <- rbind(neighbours, neighbours_i)
              
              coord_dist <- rbind(coord_dist,data.frame(resample = i,
                                            resamples_df[which(resamples_df[,"sample"]==i),c("Lat","Long")],
                                            distance = neighbours_i[,"distance"],
                                            forcens_row = neighbours_i[,"row_findin"],
                                            forcens_df[neighbours_i[,"row_findin"],c("Latitude", "Longitude")]))
              rm(neighbours_i)
            
        } # for i in resamples
        
        names(coord_dist) <- c("resample","lat","long","distance","forcens_row","forcens_lat","forcens_long")
      
        forcens_df <- forcens_df[coord_dist[,"forcens_row"],]
        forcens_df <- cbind(bias_buckley_resample = coord_dist[,"resample"], forcens_df)
        
        # Joining "ruber" pink and ruber "white" into "ruber" - no distiction in Buckley
        forcens_df[,"Globigerinoides_ruber"] <- rowSums(forcens_df[,c("Globigerinoides_ruber","Globigerinoides_white","Globigerinoides_ruber_._Globigerinoides_white")], na.rm = TRUE)
        exclude <- c("Globigerinoides_white","Globigerinoides_ruber_._Globigerinoides_white") # I've just included these in "Globigerinoides_ruber" (above)
        forcens_df <- forcens_df[, -which(names(forcens_df) %in% exclude)]
        # names(forcens_df)[grep("white", names(forcens_df))]   # double-check
        
        # 'Correcting' for samples that did not differentiate between menardii and tumida 
        # names(forcens_df)[grep("tumida", names(forcens_df))]   
        if(any(!is.na(forcens_df[,c("Globorotalia_menardii_._Globorotalia_tumida")]))){
          row = which(!is.na(forcens_df[,c("Globorotalia_menardii_._Globorotalia_tumida")]))
          forcens_df[,"Globorotalia_tumida"] <- forcens_df[row,c("Globorotalia_menardii_._Globorotalia_tumida")]/2 # I simply divided by two
          forcens_df[,"Globorotalia_menardii"] <- forcens_df[row,c("Globorotalia_menardii_._Globorotalia_tumida")]/2 # I simply divided by two
          forcens_df <- forcens_df[, -which(colnames(forcens_df)==c("Globorotalia_menardii_._Globorotalia_tumida"))]
        }
        
        
        write.csv(coord_dist, file = "data/forcens_coord_dist.csv",row.names=FALSE)
        write.csv(forcens_df, file = "data/forcens_subset.csv",row.names=FALSE)
        
        return(forcens_df)
        
      } # else
      
}
    
    
