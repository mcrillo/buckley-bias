# Created by Marina Costa Rillo in 05/July/2018 
rm(list=ls())

# Libraries
library(reshape)
library(geosphere)
library(SpadeR)
library(ggplot2)
library(viridis)
library(dplyr)
library(ggpubr)
library(lme4)
library(xtable)     # prints table in Latex format


# Auxiliary functions
'%!in%' <- function(x,y)!('%in%'(x,y))

find_neighbours <- function(point, findin, distance) { # vector, data.frame, numeric
  ## point: vector of two numbers (longitude, latitude) 
  ## findin: a matrix of 2 columns (first one is longitude, second is latitude) 
  ## distance: if 0 finds nearest neighbour, if a positive value (in meters) finds neighbours within a circle with the value radius
  
  ## Returns a data.frame: 
  ## "row" = the row number of the neighbour in data.frame
  ## "distance" = the distance between the points
  
  dist_data <- apply(findin, 1, function(x) distCosine(point, x)) #Matrix
  
  if(distance>0) { # find neighbours within radius of distance
    neighb <- data.frame(row_findin = which(dist_data<=distance), distance = dist_data[which(dist_data<=distance)])
    if(length(neighb[,1])==0) distance = 0 
  }  
  
  if(distance==0) { # find nearest neighbour
    neighb <- data.frame(row_findin = which(dist_data == dist_data[which.min(dist_data)]), distance = min(dist_data))
  }
  
  return(neighb)
  
}

function_name_grid <- function(data){ 
  ### Description
  # This function receives the ForCenS data, and returns the ForCenS data with three extra columns: 'factorLatitude', 'factorLongitude' & 'grid_name'
  ### Arguments
  # data
  
  # transform the latitudes and longitudes to positive numbers
  data$factorLatitude <- (data$Lat + 90)
  data$factorLongitude <- (data$Long + 180)
  # print(data$factorLatitude)
  # print(data$factorLongitude)
  
  # creating names for each longitude interval (numbers 1 to 36)
  long_min <- seq(0, 350 , by=10)
  long_max <- seq(10, 360, by=10)
  long_name <- seq(1:36)
  long_data <- data.frame(long_min, long_max, long_name)
  
  # creating names for each latitude interval (letters)
  lat_min <- seq(0, 170, by=10)
  lat_max <- seq(10, 180, by= 10)
  lat_name <-  letters [1:18]
  lat_data <- data.frame(lat_min, lat_max, lat_name)
  
  # creating new column to add square/grid name
  data[,"grid_name"] <- NA 
  
  for (i in 1:nrow(data)){ # i goes through the rows of the ForCenS data set
    # i = 1
    
    lat_smaller <- which(lat_data$lat_min < data[i,"factorLatitude"]) # latitudes smaller than latitude from data
    lat_larger <- which(lat_data$lat_max >= data[i,"factorLatitude"]) # latitudes larger than latitude from data
    lat_line <- intersect(lat_smaller,lat_larger) # row of the lat_data that you want: intersect between all smaller and all larger gives the one you want
    # Double check:
    # print(paste("ForCenS lat:", data[i,"factorLatitude"])) 
    # print(lat_data[lat_line,])
    
    long_smaller <- which(long_data$long_min < data[i,"factorLongitude"]) # longitudes smaller than longitudes from data
    long_larger <- which(long_data$long_max >= data[i,"factorLongitude"]) # longitudes larger than longitudes from data
    long_line <- intersect(long_smaller,long_larger) # row of long_data that you want
    # Double check:
    # print(paste("ForCenS long:", data[i,"factorLongitude"]))
    # print(long_data[long_line,]) 
    
    data[i,"grid_name"] <- paste(lat_data[lat_line,"lat_name"], long_data[long_line,"long_name"], sep="")
    
  }
  
  # grid names as factors
  data$grid_name <- as.factor(data$grid_name)
  
  return(data)
  
}


###
### Data
###


setwd("/Users/marinacostarillo/Google Drive/PhD/Projects/buckley-bias/short_paper")
historical <- read.csv("historical_data.csv", header = TRUE)
holocene_full <- read.csv("ForCenS_woa.csv", header = TRUE)
lgm_full <- read.csv("LGM_MARGO_renamed.csv", header = TRUE)


###############################################
### Solving species names in the 3 datasets ###
###############################################

# (1) Globorotalia_menardii_._Globorotalia_tumida
holocene_full[which(!is.na(holocene_full$Globorotalia_menardii_._Globorotalia_tumida)), c("Globorotalia_menardii","Globorotalia_tumida")] <- rep(holocene_full[which(!is.na(holocene_full$Globorotalia_menardii_._Globorotalia_tumida)), c("Globorotalia_menardii_._Globorotalia_tumida")]/2,2)
lgm_full[which(!is.na(lgm_full$Globorotalia_menardii_._Globorotalia_tumida)), c("Globorotalia_menardii","Globorotalia_tumida")] <- rep(lgm_full[which(!is.na(lgm_full$Globorotalia_menardii_._Globorotalia_tumida)), c("Globorotalia_menardii_._Globorotalia_tumida")]/2,2)
holocene_full <- holocene_full[,-which(colnames(holocene_full)=="Globorotalia_menardii_._Globorotalia_tumida")]
lgm_full <- lgm_full[,-which(colnames(lgm_full)=="Globorotalia_menardii_._Globorotalia_tumida")]

# (2) Turborotalita_humilis_._Berggrenia_pumilio
holocene_full[, "Turborotalita_humilis"] <- rowSums(data.frame(holocene_full[, "Turborotalita_humilis"],holocene_full[, "Turborotalita_humilis_._Berggrenia_pumilio"]), na.rm = T)
holocene_full <- holocene_full[,-which(colnames(holocene_full)=="Turborotalita_humilis_._Berggrenia_pumilio")]
lgm_full[, "Turborotalita_humilis"] <- rowSums(data.frame(lgm_full[, "Turborotalita_humilis"],lgm_full[, "Turborotalita_humilis_._Berggrenia_pumilio"]), na.rm = T)
lgm_full <- lgm_full[,-which(colnames(lgm_full)=="Turborotalita_humilis_._Berggrenia_pumilio")]

# (3) Removing 61 rows with ruber + white together (most in the Mediterranean)
length(which(holocene_full[,which(colnames(holocene_full)=="Globigerinoides_ruber_._Globigerinoides_white")]!=0))
holocene_full <- holocene_full[-which(holocene_full[,which(colnames(holocene_full)=="Globigerinoides_ruber_._Globigerinoides_white")]!=0),]
# Now revoving whole column
holocene_full <- holocene_full[,-which(colnames(holocene_full)=="Globigerinoides_ruber_._Globigerinoides_white")]

# (4) Removing cavernula (ref: Siccha 2017)
holocene_full <- holocene_full[,-which(colnames(holocene_full)=="Globorotalia_cavernula")]

# (5) Removing Hastigerina_pelagica, too delicate to be preserved in LGM and none found in historical samples.
length(which(holocene_full[,"Hastigerina_pelagica"]!=0))
holocene_full <- holocene_full[,-which(colnames(holocene_full)=="Hastigerina_pelagica")]
length(which(holocene_full[,"Hastigerinella_digitata"]!=0))
holocene_full <- holocene_full[,-which(colnames(holocene_full)=="Hastigerinella_digitata")]
historical <- historical[,-which(colnames(historical)=="Hastigerina_pelagica")]

# (6) Filling missing species in each data set, using ForCenS as base
## ForCenS and historical
# sort(names(holocene_full)[22:59]) ; sort(names(historical)[12:45])
historical$Dentigloborotalia_anfracta <- 0
historical$Globigerinella_adamsi <- 0
historical$Globigerinita_minuta <- 0
historical$Globigerinita_uvula <- 0
historical$Tenuitella_iota <- 0
# data.frame(forcens = sort(names(holocene_full)[22:59]), historical = sort(names(historical)[12:49]))
## ForCenS and LGM
# sort(names(holocene_full)[22:59]) ; sort(names(lgm_full)[17:51])
lgm_full$Berggrenia_pumilio <- 0
lgm_full$Globigerinita_minuta <- 0
lgm_full$Globorotalia_ungulata <- 0
# data.frame(forcens = sort(names(holocene_full)[22:59]), lgm = sort(names(lgm_full)[c(17:51,56:58)]))
# data.frame(forcens = sort(names(holocene_full)[22:59]), historical = sort(names(historical)[12:49]) , lgm = sort(names(lgm_full)[c(17:51,56:58)]))

# (7) Transforming NA into 0
historical[is.na(historical)] <- 0
holocene_full[is.na(holocene_full)] <- 0

names(lgm_full)[11] <- "sed_rate_cmky"
if(!any(lgm_full$sed_rate_cmky == 0, na.rm = T)){
 lgm_full[is.na(lgm_full)] <- 0
}
# Sed. rate of LGM back to NA
lgm_full[which(lgm_full$sed_rate_cmky == 0),"sed_rate_cmky"] <- NA

###############################################
# Saving SEDIMENTATION RATE data

sed_rate_lgm <- lgm_full[!is.na(lgm_full$sed_rate_cmky), c("Core", "Lat", "Long", "Water.depth..m.","Ocean", "sed_rate_cmky",
                                           "calendar.age.estimate..cal.ky.BP.","Sample.depth...upper..m.","Sample.depth...lower..m.")]
mean(sed_rate_lgm$sed_rate_cmky)

###############################################

# Historical samples subset

# Subsetting data to nearest neighbours
if (!file.exists("ForCenS_historical_subset.csv") | !file.exists("LGM_historical_subset.csv")){
  
  lgm <- data.frame()
  holocene <- data.frame()

  for (i in 1 : nrow(historical)){
    point <- as.numeric(historical[i,c("Long","Lat")])

    lgm_neighb <- find_neighbours(point, findin = lgm_full[,c("Long","Lat")], distance = 0) # when there is more than one, returns both?
    lgm <- rbind(lgm, cbind(sample = historical$sample[i], lgm_neighb, lgm_full[lgm_neighb$row_findin,]))
    
    print(historical$sample[i])
    print(lgm_neighb)
    
    holocene_neighb <- find_neighbours(point, findin = holocene_full[,c("Longitude","Latitude")], distance = 0)
    holocene <- rbind(holocene, cbind(sample = historical$sample[i], holocene_neighb, holocene_full[holocene_neighb$row_findin,]))
  }

  write.csv(holocene, "ForCenS_historical_subset.csv", row.names = F)
  write.csv(lgm, "LGM_historical_subset.csv", row.names = F)

}else{
  holocene <- read.csv("ForCenS_historical_subset.csv", header = TRUE)
  lgm <- read.csv("LGM_historical_subset.csv", header = TRUE)
}
mean(holocene$distance/1000)
median(holocene$distance/1000)

#####################################
### Checking SST among neighbours ###
#####################################

# Finding WOA temperature for neighbouring points, to see if there are large differences because of distance
if (!file.exists("SST_neighbours.csv")){
  
  setwd("/Users/marinacostarillo/Google Drive/PhD/projects/competition-forams/data/ocean")
  tmn <- read.csv("woa13_decav_t00mn01v2.csv", header = T,stringsAsFactors = FALSE)
  tsd <- read.csv("woa13_decav_t00sd01v2.csv", header = T,stringsAsFactors = FALSE)
  setwd("/Users/marinacostarillo/Google Drive/PhD/Projects/buckley-bias/short_paper")
  
  neighb <- historical[,c("sample", "Museum_no", "Lat", "Long")]
  neighb$distance <- 0
  neighb$age <- c("Historical")
  
  neighb_holocene <- holocene[,c("sample", "Sample_ID", "Latitude", "Longitude", "distance")]
  neighb_holocene$age <- c("Holocene")
  
  neighb_lgm <- lgm[,c("sample", "Core", "Lat", "Long", "distance")]
  neighb_lgm$age <- c("LGM")
  
  names(neighb) <- names(neighb_holocene) <- names(neighb_lgm) <- c("sample", "Sample_Core_no", "Latitude", "Longitude", "distance", "age")
  neighb <- rbind(neighb, rbind(neighb_holocene,neighb_lgm))
  neighb <- neighb[order(neighb[,c("sample")]),]
  
  for(j in 1:nrow(neighb)){
    print(neighb[j, "sample"])
    point <- neighb[j,c("Longitude","Latitude")]
    
    woa_mn <- find_neighbours(point, findin = tmn[,c("Long","Lat")], distance = 0)
    woa_sd <- find_neighbours(point, findin = tsd[,c("Long","Lat")], distance = 0)
    
    neighb[j,"woamn"] <- mean(tmn[woa_mn$row_findin,"X0"], na.rm = T)
    neighb[j,"woasd"] <- mean(tsd[woa_sd$row_findin,"X0"], na.rm = T)
    neighb[j,"woamn_dist_km"] <-mean( woa_mn$distance, na.rm = T)
    neighb[j,"woasd_dist_km"] <- mean(woa_sd$distance, na.rm = T)
  }
  write.csv(neighb, "SST_neighbours.csv", row.names = F)
}else{
  neighb <- read.csv("SST_neighbours.csv", header = TRUE)
}

# neighb 

# Large SST differences for sample M.8780
neighb[which(neighb$sample=="A10"),]

# Find other ForCenS neighbours, within 500 km radius
new_neighb <- find_neighbours(point = neighb[which(neighb$sample=="A10" & neighb$age=="Historical"),c("Longitude","Latitude")], 
                              findin = holocene_full[,c("Longitude","Latitude")], distance = 500000)

# new neighbour of M.8780: the one with least SST difference
new_neighb <- new_neighb[which.min(abs(neighb[which(neighb$sample=="A10" & neighb$age=="Historical"),"woamn"] - 
                                         holocene_full[new_neighb$row_findin,"woa_tmn"])),]
new_holocene_neighb <- cbind(sample = "A10", new_neighb, holocene_full[new_neighb$row_findin,])

# Substitute nearest neighbour with neighbour with more similar SST
if(all(colnames(holocene) == colnames(new_holocene_neighb))){
  holocene <- rbind(holocene, new_holocene_neighb)
  sst_remove <- holocene[which(holocene$sample=="A10"),"woa_tmn"][which.max(abs(neighb[which(neighb$sample=="A10" & neighb$age=="Historical"),"woamn"] - holocene[which(holocene$sample=="A10"),"woa_tmn"]))]
  holocene <- holocene[-which(holocene$sample=="A10" & holocene$woa_tmn == sst_remove),]
}


neighb_new_holocene <- new_holocene_neighb[,c("sample", "Sample_ID", "Latitude", "Longitude", "distance")]
neighb_new_holocene$age <- c("Holocene")
neighb_new_holocene <- cbind(neighb_new_holocene,new_holocene_neighb[,c("woa_tmn","woa_tsd","woa_dist","woa_dist")])
names(neighb_new_holocene) <- c("sample", "Sample_Core_no", "Latitude", "Longitude", "distance", "age","woamn","woasd" ,"woamn_dist_km","woasd_dist_km")

neighb <- rbind(neighb,neighb_new_holocene)
neighb$lsd <- neighb$woamn - neighb$woasd
neighb$usd <- neighb$woamn + neighb$woasd


pdf("si_fig_sst_diff.pdf", paper = "special",  width = 10, height = 5)
ggplot(neighb, aes(x=sample, y=woamn, shape=age, colour=age)) +
  geom_errorbar(position=position_dodge(width=0.5), aes(ymin=lsd, ymax=usd), width=.2) +
  geom_point(position=position_dodge(width=0.5), size=4.5) +
  theme_bw() + 
  theme(plot.margin = margin(20, 5.5, 20, 5.5, "pt"),
        axis.text=element_text(size=16, colour = "black"), 
        axis.title=element_text(size=20, colour = "black"),
        legend.text = element_text(size=16, colour = "black"), 
        legend.title = element_blank(), 
        legend.position = c(0.5, 0.15),
        legend.background = element_rect(linetype="solid", size = 0.4, colour ="black")) +
  scale_color_manual(values=c("#e41a1c", "#4daf4a","#377eb8")) +
  scale_shape_manual(values=c(17, 15, 19)) +
  scale_y_continuous(name="Mean annual SST", breaks = seq(5, 30, by = 5),limits=c(4.5, 30.5)) +
  scale_x_discrete(name="Historical sites and corresponding neighbours", 
                   labels=c("A10" = "M.8780", 
                            "A20" = "M.5246",
                            "A25" = "M.408",
                            "A27" = "M.3787",
                            "A31" = "M.4080",
                            "A44" = "M.7487",
                            "A46" = "M.284",
                            "A5" =  "M.192",
                            "A55" = "M.25"))
dev.off()


neighb <- neighb[-which(neighb$Sample_Core_no == "CLIMAP_0070"),]
neighb <- neighb[order(neighb$sample),]
write.csv(neighb, "SST_new_neighbours.csv", row.names = F)


# Distance (in km) between historical sample and LGM and ForCenS nearest sample
round(mean(holocene$distance/1000),0)
round(mean(unique(lgm$distance)/1000),0)


###
### Organizating data for analysis
###
# data.frame(sort(names(holocene[,25:63])), sort(names(lgm[,c(20:55,59:61)])))
# Calculating relative abundance within each assemblage
holocene_species <- (holocene[,25:63]/rowSums(holocene[,25:63], na.rm = T))*100; rowSums(holocene_species, na.rm=T)
lgm_species <- (lgm[,c(20:55,59:61)]/rowSums(lgm[,c(20:55,59:61)], na.rm = T))*100; rowSums(lgm_species, na.rm=T)
historical_species <- (historical[12:49]/rowSums(historical[12:49], na.rm = T))*100; rowSums(historical_species, na.rm=T)

# Removing unidentified
holocene_species <- holocene_species[,-which(colnames(holocene_species)=="unidentified")]
lgm_species <- lgm_species[,-which(colnames(lgm_species)=="unidentified")]

### Merging historical data with holocene and LGM
holocene_species$sample <- paste(holocene$sample,"_holocene", sep="")
lgm_species$sample <- paste(lgm$sample,"_lgm", sep="")
historical_species$sample <- paste(historical$sample,"_hist", sep="")

# Spade R data: species (in rows) by community (in columns) matrix
historical_holocene <- rbind(historical_species,holocene_species)
historical_holocene <- melt(historical_holocene, id = "sample")
historical_holocene <- cast(historical_holocene, variable~sample)
row.names(historical_holocene) <- historical_holocene$variable
historical_holocene <- historical_holocene[,-which(names(historical_holocene)=="variable")]

historical_lgm <- rbind(historical_species,lgm_species)
historical_lgm <- melt(historical_lgm, id = "sample")
historical_lgm <- aggregate(value~variable+sample, data=historical_lgm, FUN=function(x) mean=mean(x))
historical_lgm <- cast(historical_lgm, variable~sample)
row.names(historical_lgm) <- historical_lgm$variable
historical_lgm <- historical_lgm[,-which(names(historical_lgm)=="variable")]


holocene_lgm <- rbind(holocene_species,lgm_species)
holocene_lgm <- melt(holocene_lgm, id = "sample")
holocene_lgm <- aggregate(value~variable+sample, data=holocene_lgm, FUN=function(x) mean=mean(x))
holocene_lgm <- cast(holocene_lgm, variable~sample)
row.names(holocene_lgm) <- holocene_lgm$variable
holocene_lgm <- holocene_lgm[,-which(names(holocene_lgm)=="variable")]


### SI TABLES
table_rel_abund <- cbind(historical_holocene, historical_lgm[,grep("lgm", colnames(historical_lgm))])
table_rel_abund <- table_rel_abund[order(row.names(table_rel_abund)),]
table_rel_abund <- table_rel_abund[,order(colnames(table_rel_abund))]
# write.csv(table_rel_abund, "table_rel_abund.csv")

# si_table <- read.csv(file= "si_table_raw_counts.csv",stringsAsFactors = FALSE)
# print(xtable(si_table), include.rownames=F) # LaTeX


###
### Analysis Chao
###

if (!file.exists("similarity.csv")){
  
  sim_samples <- paste(historical$sample[which(historical$sample!="A66")],"_", sep="") # SimilarityPair can only be calculated for assemblages with more than ONE species
  sim_holo <- data.frame()
  sim_lgm <- data.frame()
  sim_null <- data.frame()
  
  for (i in sim_samples){ # i = sim_samples[1]
    
    sim_holo_data <- historical_holocene[,grep(names(historical_holocene), pattern = i)]
    sim_holo_i <- SimilarityPair(as.matrix(sim_holo_data), datatype = c("abundance"), nboot = 100)
    sim_holo <- rbind(sim_holo, 
                      cbind(rbind(c02 = sim_holo_i$estimated_richness[1,],
                                  c12 = sim_holo_i$estimated_relative[1,],
                                  c22 = sim_holo_i$estimated_relative[2,],
                                  chao_jaccard  = sim_holo_i$estimated_relative[4,],
                                  chao_sorensen = sim_holo_i$estimated_relative[5,]),
                            sample = rep(i, 5)))
    
    sim_lgm_data <- historical_lgm[,grep(names(historical_lgm), pattern = i)]
    sim_lgm_i <- SimilarityPair(as.matrix(sim_lgm_data), datatype = c("abundance"), nboot = 100)
    sim_lgm <- rbind(sim_lgm, 
                     cbind(rbind(c02 = sim_lgm_i$estimated_richness[1,],
                                 c12 = sim_lgm_i$estimated_relative[1,],
                                 c22 = sim_lgm_i$estimated_relative[2,],
                                 chao_jaccard  = sim_lgm_i$estimated_relative[4,],
                                 chao_sorensen = sim_lgm_i$estimated_relative[5,]),
                           sample = rep(i, 5)))
    
    sim_null_data <- holocene_lgm[,grep(names(holocene_lgm), pattern = i)]
    sim_null_i <- SimilarityPair(as.matrix(sim_null_data), datatype = c("abundance"), nboot = 100)
    sim_null <- rbind(sim_null, 
                      cbind(rbind(c02 = sim_null_i$estimated_richness[1,],
                                  c12 = sim_null_i$estimated_relative[1,],
                                  c22 = sim_null_i$estimated_relative[2,],
                                  chao_jaccard  = sim_null_i$estimated_relative[4,],
                                  chao_sorensen = sim_null_i$estimated_relative[5,]),
                            sample = rep(i, 5)))
    
    rm(sim_holo_data)
    rm(sim_holo_i)
    rm(sim_lgm_data)
    rm(sim_lgm_i)
    rm(sim_null_data)
    rm(sim_null_i)
    
  }
  
  sim_holo[,c(1:4)] <- sapply(sim_holo[,c(1:4)], function(x) as.numeric(as.character(x)))
  sim_lgm[,c(1:4)]  <- sapply(sim_lgm[,c(1:4)], function(x) as.numeric(as.character(x)))
  sim_null[,c(1:4)] <- sapply(sim_null[,c(1:4)], function(x) as.numeric(as.character(x)))
  
  sim_holo$comparison <- "Historical and Holocene"
  sim_lgm$comparison  <- "Historical and LGM"
  sim_null$comparison <- "LGM and Holocene"
  
  similarity <- rbind(sim_holo,sim_lgm,sim_null)
  similarity$sample <- as.factor(sub("_", "", similarity$sample))
  similarity$comparison <- as.factor(similarity$comparison)
  similarity$index <- row.names(similarity)
  similarity <- merge(similarity,historical[,c("sample", "Lat", "Long")])
  
  names(similarity) <- c("sample","Estimate", "se", "lci", "uci", "comparison", "index", "lat", "long")
  
  write.csv(similarity, "similarity.csv", row.names = FALSE)

}else{
  similarity <- read.csv("similarity.csv", header = TRUE)
}
sim_data2 <- similarity[grep("c22", similarity$index),] # Morisita-Horn index (relative abundance, rare)

###
### Analysis GLM
###

stat_data <- sim_data2[which(sim_data2$comparison != "LGM and Holocene"), - 7]
write.csv(stat_data, "stat_comparison.csv", row.names = F)
ggplot(stat_data, aes(x=Estimate, fill=comparison)) + geom_histogram(alpha=0.5, position="identity", binwidth = 0.1)

stat_data_holo <- sim_data2[which(sim_data2$comparison != "LGM and Holocene"), - 7]
stat_data_lgm <- sim_data2[which(sim_data2$comparison != "LGM and Holocene"), - 7]

model <- glmer(data = stat_data, formula = Estimate ~ comparison +(1|sample), family = binomial)

binom.test(8, 9, p = .5, "two.sided")$p.value

###
### Historical samples plot
###

# Morista-Horn 
sim_data2$comparison <- sub(pattern = "vs.", replacement = "and", x=sim_data2$comparison)
morisita_horn <- ggplot(sim_data2, aes(x=sample, y=Estimate, shape=comparison, colour=comparison)) +
  geom_errorbar(position=position_dodge(width=0.5), aes(ymin=lci, ymax=uci), width=.2) +
  geom_point(position=position_dodge(width=0.5), size=4.5)  + ylim(0,1) +
  labs(y = "Compositional similarity", x = "Historical samples") +
  theme_bw() + 
  theme(plot.margin = margin(20, 5.5, 20, 5.5, "pt"),
        axis.text=element_text(size=16, colour = "black"), 
        axis.title=element_text(size=20, colour = "black"),
        legend.text = element_text(size=18, colour = "black"), 
        legend.title = element_blank(), 
        legend.position = c(0.17, 0.12),
        legend.background = element_rect(linetype="solid", size = 0.4, colour ="black")) +
  scale_color_manual(values=c("#a6611a", "#018571","#999999")) +
  scale_shape_manual(values=c(17, 15, 19)) +
  #scale_y_continuous(breaks = seq(0,1,0.25),limits=c(0,1), expand = c(0.02, 0)) +                         
  scale_x_discrete(labels=c("A10" = "M.8780", 
                            "A20" = "M.5246",
                            "A25" = "M.408",
                            "A27" = "M.3787",
                            "A31" = "M.4080",
                            "A44" = "M.7487",
                            "A46" = "M.284",
                            "A5" =  "M.192",
                            "A55" = "M.25"))

png(file = "fig_morisita-horn.png", width = 11.5, height = 7, units = "in", res = 300)
print(morisita_horn)
dev.off()



#######################
### LGM ForCenS map ###
#######################

### Data

# Finding ForCenS neigbours of LGM
if (!file.exists("LGM_ForCenS_neighbours.csv")){
  
  forcens_lgm <- data.frame()
  
  for(j in 1:nrow(lgm_full)){
    point <- as.numeric(lgm_full[j,c("Long","Lat")])
    
    forcens_neighb <- find_neighbours(point, findin = holocene_full[,c("Longitude","Latitude")], distance = 0)
    forcens_lgm <- rbind(forcens_lgm, cbind(lgm_row = j, lgm_long = point[1], lgm_lat = point[2],
                                            forcens_neighb, holocene_full[forcens_neighb$row_findin,]))
    
  }

  write.csv(forcens_lgm, "LGM_ForCenS_neighbours.csv", row.names = F)

}else{
  forcens_lgm <- read.csv("LGM_ForCenS_neighbours.csv", header = TRUE)
}

dim(forcens_lgm)
dim(unique(forcens_lgm[,c("lgm_lat", "lgm_long", "distance")]))
unique_lgm <- unique(forcens_lgm[,c("lgm_lat", "lgm_long", "distance")])
median(unique_lgm$distance/1000) # km

### Preparing data

# Creating species datasets by calculating relative abundance
holocene_species <- (forcens_lgm[27:65]/rowSums(forcens_lgm[27:65], na.rm = T))*100
lgm_species <- (lgm_full[,c(17:52,56:58)]/rowSums(lgm_full[,c(17:52,56:58)], na.rm = T))*100
# data.frame(forcens = sort(names(forcens_lgm)[27:65]), lgm = sort(names(lgm_full)[c(17:52,56:58)]))

# Removing unidentified
holocene_species <- holocene_species[,-which(colnames(holocene_species)=="unidentified")]
lgm_species <- lgm_species[,-which(colnames(lgm_species)=="unidentified")]

# Checking if species of both datasets match
data.frame(sort(names(holocene_species)), sort(names(lgm_species)))

# Checking if holocene rows are still in the same order, after all the species corrections
# WHY? To create a 'sample' index/number for each row which will be simply the row number
# ...and why? To be able to later merge metadata (lat, long)
all(row.names(holocene_species) == row.names(forcens_lgm))
all(row.names(lgm_species) == row.names(lgm_full))

# Creating "sample" number (row number) to merge datasets
holocene_species$sample <- paste(1:nrow(holocene_species),"_holocene", sep="")
lgm_species$sample <- paste(1:nrow(lgm_species),"_lgm", sep="")

# Spade R data: species (in rows) by community (in columns) matrix
forcens_lgm <- rbind(holocene_species,lgm_species)
forcens_lgm <- melt(forcens_lgm, id = "sample")
forcens_lgm <- cast(forcens_lgm, variable~sample)
row.names(forcens_lgm) <- forcens_lgm$variable
forcens_lgm <- forcens_lgm[,-which(names(forcens_lgm)=="variable")]
forcens_lgm[1:10,1:10]


################################################################################################################
### NOT RUN
### ERROR/PROBLEM with SimilarityPair function --> GitHub issue raised, have to run for loop "by hand"
### Similarity analysis 
# between local communities fo the Holocene and LGM
sim_null <- data.frame()
max_ssp <- 2
for (i in 1:nrow(lgm_species)){ 
  sim_null_data <- data.frame(holocene=forcens_lgm[,paste(i, "holocene", sep="_")],lgm=forcens_lgm[,paste(i, "lgm", sep="_")])
  # if(length(which(sim_null_data[,1]!=0))>1 & length(which(sim_null_data[,2]!=0))>1 & !any(sim_null_data==99.9)){ # SimilarityPair can only be calculated for assemblages with more than ONE species
  if(length(which(sim_null_data[,1]!=0))>=max_ssp & length(which(sim_null_data[,2]!=0))>=max_ssp){ # Trying to fix Error
    sim_null_i <- SimilarityPair(sim_null_data, datatype = c("abundance"), nboot = 10)
    sim_null <- rbind(sim_null, cbind(c22 = sim_null_i$estimated_relative[2,1], sample =i))
    rm(sim_null_i)
    rm(sim_null_data)
    sim_null
  } # if
} # for
i
write.csv(sim_null, "LGM_ForCenS_sim_null.csv", row.names = F)
sim_null <- read.csv("LGM_ForCenS_sim_null.csv", header = TRUE)

lgm_forcens_sim <- sim_null # 'backing up' sim_null
lgm_full$sample <- as.numeric(row.names(lgm_full)) # to be able to merge lat and long
lgm_forcens_sim <- merge(lgm_forcens_sim,lgm_full[,c("sample", "Lat", "Long","sed_rate_cmky")])
# write.csv(lgm_forcens_sim, "LGM_ForCenS_c22.csv", row.names = FALSE)
################################################################################################################


lgm_forcens_sim <- read.csv("LGM_ForCenS_c22.csv", header = TRUE)
names(lgm_forcens_sim)
dim(unique(lgm_forcens_sim[,c("Lat", "Long")]))

lgm_forcens_sim_mean <- aggregate(c22~Lat+Long, data=lgm_forcens_sim, FUN=function(x) mean=mean(x))
write.csv(lgm_forcens_sim_mean, "LGM_ForCenS_c22_mean.csv", row.names = FALSE)

# Mean similarity for each lat long grid square - NOT USED
lgm_forcens_sim <- function_name_grid(data = lgm_forcens_sim)
sim_grid_mean <- data.frame(do.call("rbind", by(lgm_forcens_sim[,c("c22","Lat","Long")], lgm_forcens_sim$grid_name, function(x) colMeans(x, na.rm = T))))
sim_grid_mean <- as.data.frame(cbind(sim_grid_mean, grid_name = row.names(sim_grid_mean)))
names(sim_grid_mean)



### Merging historical and LGM ForCenS data for plot
gradient <- c('#bd0026','#f03b20','#fd8d3c','#2b8cbe', 'black')

# Each 0.25
lgm_forcens_sim_mean$similarity <- NA
lgm_forcens_sim_mean[which(lgm_forcens_sim_mean$c22> 0.75),"similarity"] <- as.character("0.75 - 1.0")
lgm_forcens_sim_mean[which(lgm_forcens_sim_mean$c22<=0.75 & lgm_forcens_sim_mean$c22>0.5),"similarity"] <- as.character("0.5 - 0.75")
lgm_forcens_sim_mean[which(lgm_forcens_sim_mean$c22<=0.5 & lgm_forcens_sim_mean$c22>0.25),"similarity"] <- as.character("0.25 - 0.5")
lgm_forcens_sim_mean[which(lgm_forcens_sim_mean$c22< 0.25),"similarity"] <- as.character("0.0 - 0.25")
lgm_forcens_sim_mean$similarity <- as.factor(lgm_forcens_sim_mean$similarity)
lgm_forcens_sim_mean <- lgm_forcens_sim_mean[order(lgm_forcens_sim_mean$similarity, decreasing = T),]

map_data <-  lgm_forcens_sim_mean

historical$c22 <- NA
historical$similarity <- "H"

historical$point <- "Historical"
map_data$point <- "LGM"
map_data$sample <- row.names(map_data)

map_data$similarity <- as.character(map_data$similarity)
map_data <- rbind(map_data, historical[,c("sample", "c22","Lat","Long", "similarity", "point")])
map_data$point <- as.factor(map_data$point)
map_data$similarity <- as.factor(map_data$similarity)
str(map_data)


### Plotting map of geographical similarity
world <- map_data("world")
mapplot <- ggplot(world, mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "grey60", colour = "grey60") +
  theme_bw() + coord_fixed(1.3) +
  theme(axis.text=element_text(size=16, colour = "grey60"), 
        axis.title=element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(5.5, 5.5, 5.5, 5.5, "pt")) + # unit with the sizes of the top, right, bottom, and left margins)
  scale_x_continuous(breaks = seq(-180, +180, 40), position = "bottom", limits=c(-180, 180), expand = c(0.02, 0.02),
                     labels=c(expression("180"*degree*W), 
                              expression("140"*degree*W),
                              expression("100"*degree*W),
                              expression("60"*degree*W),
                              expression("20"*degree*W),
                              expression("20"*degree*E),
                              expression("60"*degree*E),
                              expression("100"*degree*E),
                              expression("140"*degree*E),
                              expression("180"*degree*E))
  ) +
  scale_y_continuous(breaks = seq(-80,80,20),limits=c(-90,90), expand = c(0, 0),
                     labels=c(expression("80"*degree*S), 
                              expression("60"*degree*S),
                              expression("40"*degree*S),
                              expression("20"*degree*S),
                              expression("0"*degree),
                              expression("20"*degree*N),
                              expression("40"*degree*N),
                              expression("60"*degree*N),
                              expression("80"*degree*N))
  )


mappoints_all <- mapplot + geom_point(data = map_data, 
                                  aes(x = Long, y = Lat, group = sample, fill = similarity, shape = point), 
                                  color = "black", size = 4, stroke = 0.2) + 
  scale_fill_manual(values = gradient, name = "Compositional similarity (LGM and Holocene)",
                    breaks = c("0.0 - 0.25","0.25 - 0.5","0.5 - 0.75","0.75 - 1.0")) +
  scale_shape_manual(values=c(24, 21), name = "Dataset") +
  theme(legend.text=element_text(size=17),   
        legend.direction = "horizontal", 
        #legend.position = c(0.275, 0.078),
        legend.position = c(0.5, 0.07),
        legend.box = "horizontal",
        legend.title = element_text(size=18), 
        legend.background = element_rect(linetype="solid", size = 0.4, colour ="black")) +
  guides(fill = guide_legend(override.aes=list(shape=21),nrow = 1, title.position = "top", order = 2),
         shape = guide_legend(override.aes = list(fill = "black"),nrow = 1, title.position = "top", order = 1))

# saving map
png(file = "map_lgm_similarity_mean.png", width = 11.5, height = 7, units = "in", res = 300)
print(mappoints_all)
dev.off()


# Final Figure 1
png(file = "figure_1.png", width=11.5, height=15, units = "in", res = 300)
print(ggarrange(morisita_horn, mappoints_all, ncol = 1, nrow = 2, align="v"))
dev.off()


# Sedimentation rate
sim_sed <- merge(lgm_forcens_sim_mean,
                 lgm_full[,c("Lat", "Long","Core","Water.depth..m.", "Ocean", "sed_rate_cmky")],
                 all.y=F)
sim_sed <- unique(sim_sed[,c("c22","Lat", "Long","Core","Water.depth..m.", "Ocean", "sed_rate_cmky")])


png(file = "si_fig_sed_rate.png", width = 12, height = 5, units = "in", res = 300)
ggplot(sim_sed, aes(x=sed_rate_cmky, y=c22)) +
  geom_point(size=2, pch = 21, stroke = 1, fill = alpha("blue",0.3)) + xlim(0,20) +
  labs(y = "Compositional similarity (LGM vs. Holocene)", x = "Sedimentation rate (cm/ky)") +
  theme_bw() +
  theme(axis.text=element_text(size=14, colour = "black"), 
        axis.title=element_text(size=16, colour = "black"))
dev.off()