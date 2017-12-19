### Description
# 
# requires: data/counts_LGM
# creates: data/lgm_neigbours.csv

### Usage
# 

### Arguments
# resamples_df:
# overwrite : TRUE or FALSE (if the output file already exist, do you want to re0run the function and overwrite it?)

### Data LGM (new: 13/12/2017)
lgm_neighb <- get_lgm_neighbours(resamples_df, overwrite = FALSE)
k = 20
p1 <- resamples_df[which(resamples_df$sample == k), c("Long", "Lat")]
lgm_neighb[which(lgm_neighb$resample == k),]
p2 <- c(109.87,8.73) 	
distGeo(p1,p2)/1000


get_lgm_neighbours <- function(resamples_df, overwrite){ 
  
  if(file.exists("data/lgm_neigbours.csv") && overwrite == FALSE){
    
    lgm_df <- read.csv("data/lgm_neigbours.csv", header = TRUE)
    return(lgm_df)
    
  }else{
    
    lgm_natl <- read.csv("data/raw_data/counts_LGM/MARGO_NAtlantic_LGM_PF.csv", header = TRUE)
    lgm_satl <- read.csv("data/raw_data/counts_LGM/MARGO_SAtlantic_LGM_PF.csv", header = TRUE)
    lgm_paci <- read.csv("data/raw_data/counts_LGM/MARGO_Pacific_LGM_PF.csv", header = TRUE)
    
    names(lgm_natl)[sapply(c("Longitude","Latitude"), FUN = function(x)grep(x,colnames(lgm_natl)))] <- c("Longitude","Latitude")
    names(lgm_satl)[sapply(c("Longitude","Latitude"), FUN = function(x)grep(x,colnames(lgm_satl)))] <- c("Longitude","Latitude")
    names(lgm_paci)[sapply(c("Longitude","Latitude"), FUN = function(x)grep(x,colnames(lgm_paci)))] <- c("Longitude","Latitude")
    
    # All coordinates of LGM dataset (to plot map):
    # rbind(lgm_natl[,c("Longitude","Latitude")],lgm_satl[,c("Longitude","Latitude")],lgm_paci[,c("Longitude","Latitude")])
    
    # Find row in LGM
    neighbours <- data.frame()
    print(noquote("Sample:"))
    
    for (i in resamples_df$sample){
      print(i)
      neighb_natl <- find_neighbours(point = resamples_df[which(resamples_df[,"sample"]==i),c("Long","Lat")], 
                                      findin = lgm_natl[,c("Longitude","Latitude")], distance = 0) # only nearest neightbours (for more neighbours, see old code assemblage_data.R from March 2017)
      neighb_satl <- find_neighbours(point = resamples_df[which(resamples_df[,"sample"]==i),c("Long","Lat")], 
                                     findin = lgm_satl[,c("Longitude","Latitude")], distance = 0) # only nearest neightbours (for more neighbours, see old code assemblage_data.R from March 2017)
      neighb_paci <- find_neighbours(point = resamples_df[which(resamples_df[,"sample"]==i),c("Long","Lat")], 
                                     findin = lgm_paci[,c("Longitude","Latitude")], distance = 0) # only nearest neightbours (for more neighbours, see old code assemblage_data.R from March 2017)
      
      dist <- data.frame(rbind(neighb_natl,neighb_satl,neighb_paci))
      dist <- cbind(resample = rep(i, 3), dataset = c("natl","satl","paci"), dist)
      # keeping shortest distance:
      neighbours <- rbind(neighbours, dist[which.min(dist$distance),])
    }
    neighbours <- cbind(neighbours, dist_km = neighbours$dist/1000)
    
    
    # subset LGM datasets
    lgm_list <- list()
    rows_paci <- neighbours[which(neighbours$dataset == "paci"), "row_findin"]
    lgm_list[["paci"]] <- cbind(neighbours[which(neighbours$dataset == "paci"), ],lgm_paci[rows_paci,])
    
    rows_satl <- neighbours[which(neighbours$dataset == "satl"), "row_findin"]
    lgm_list[["satl"]] <- cbind(neighbours[which(neighbours$dataset == "satl"), ],lgm_satl[rows_satl,])
    
    rows_natl <- neighbours[which(neighbours$dataset == "natl"), "row_findin"]
    lgm_list[["natl"]] <- cbind(neighbours[which(neighbours$dataset == "natl"), ],lgm_natl[rows_natl,])
    
    
    
    function_write_list(lgm_list, wb_name = "data/lgm_subset.xlsx") 
    write.csv(neighbours, file = "data/lgm_neigbours.csv",row.names=FALSE)
    
  } # else
  
}


