## Created 23 / March / 2017
## Marina Costa Rillo
##
## Reads: LatLongTable.csv (Buckley_Collection)
## 		  resamples_coord.csv
## 		  bias_margo_coordinates_nearest.csv
##        
## Creates: maps plots
##


rm(list=ls())
library(maps) 

setwd("/Users/marinacostarillo/Google Drive/DOUTORADO/R_data")

latlong_table = read.csv("Buckley_Collection/LatLongTable.csv", header = TRUE)
df.resamples = read.csv("Bias_analysis/resamples_coord.csv", header = TRUE)
margo.neighbours = read.csv("Bias_analysis/data_assemblages/margo_coordinates_nearest.csv", header = TRUE)
  
resamples = c(5,10,20,25,27,31,44,46,55,66)
df.resamples = df.resamples[which(df.resamples$sample %in% resamples),]

modern_ssp =  c("adamsi","bulloides","calida","conglobatus","conglomerata","crassaformis", "dehiscens", 
                "digitata","dutertrei","falconensis","hexagona","hirsuta","humilis","inflata","menardii","obliquiloculata", 
                "pachyderma","quinqueloba","ruber","rubescens","sacculifera","scitula","siphonifera","tenellus","trilobus",
                "truncatulinoides","tumida","universa","glutinata", "iota", "nitida", "pelagica", "uvula")

# Keeping just modern species form latlong_table
keep = which(colnames(latlong_table) %in% modern_ssp)
keep = c(1:8, 68:70, keep)
latlong_table = latlong_table[,keep]

latlong_table = latlong_table[order(latlong_table[,"lat"],latlong_table[,"Sample_depth_min_cm"]),]

# Separating between tow and sediment
unique(latlong_table[,"sample_type"])

buckley.tow = latlong_table[which(latlong_table[,"sample_type"]==c("tow")),]
length(which(latlong_table[,"sample_type"]==c("tow")))

buckley.sediment = latlong_table[which(latlong_table[,"sample_type"] %in% c("top_core","no_info")),]


################## Removing duplicates - this should be in latlong_table.R!!! ####################################################################
which(duplicated(buckley.sediment[,c("lat","long")]))
buckley.sediment[c(16:17,70:71,106:107),]
buckley.sediment = buckley.sediment[-c(16,71,106),]
###################################################################################################################################################


# changing max latitude so that it appears on the map plot
buckley.sediment[which(buckley.sediment$lat == max(buckley.sediment$lat)), "lat"] <- 83
df.resamples[which(df.resamples$Lat == max(df.resamples$Lat)), "Lat"] <- 83


cores = c("#007327","#c133e8")

pdf(file = c("Bias_analysis/maps/Map_sediment.pdf"), paper = "a4r", width = 12, height = 14)
map('world', interior= FALSE, col = "grey55") 
with(buckley.sediment, points(long, lat, 	type = "p", pch = 21, bg = cores[1], col="black",	cex = 1.4, lwd = 0.5) )
dev.off()

pdf(file = c("Bias_analysis/maps/Map_sediment_resample.pdf"), paper = "a4r", width = 12, height = 14)
map('world', interior= FALSE, col = "grey55") 
with(buckley.sediment, points(long, lat, 	type = "p", pch = 21, bg = cores[1], col="black",	cex = 1.4, lwd = 0.5) )
with(df.resamples, points(Long, Lat, 	type = "p", pch = 21, bg = cores[2], col="black",	cex = 1.4, lwd = 0.5) )
dev.off()

pdf(file = c("Bias_analysis/maps/Map_tow.pdf"), paper = "a4r", width = 12, height = 14)
map('world', interior= FALSE, col = "grey55") 
with(buckley.tow, points(long, lat, 	type = "p", pch = 21, bg = "dodgerblue2", col="black",	cex = 1.2, lwd = 0.5) )
dev.off()


df.resamples[which(df.resamples$Lat == max(df.resamples$Lat)), "Lat"] <- 82
pdf(file = c("Bias_analysis/maps/Map_resamples.pdf"), paper = "a4r", width = 12, height = 14)
map('world', interior= FALSE, col = "grey55") 
with(df.resamples, points(Long, Lat, 	type = "p", pch = 21, bg = cores[2], col="black",	cex = 2, lwd = 0.5) )
with(df.resamples, text(Long, Lat, labels = sample, adj = NULL, pos = 4, offset = 0.5, vfont = NULL,cex = 1.3, col = "black", font = NULL))
dev.off()


dist_km <- round(margo.neighbours$distance/1000)
pdf(file = c("Bias_analysis/maps/Map_resamples_margo.pdf"), paper = "a4r", width = 12, height = 14)
map('world', interior= FALSE, col = "grey55") 
with(df.resamples, points(Long, Lat, 	type = "p", pch = 21, bg = cores[2], col=cores[2],	cex = 2, lwd = 0.5) )
with(df.resamples, text(Long, Lat, labels = sample, adj = NULL, pos = 4, offset = 0.5, vfont = NULL,cex = 1.3, col = "black", font = NULL))
with(margo.neighbours, points(long, lat, 	type = "p", pch = 1, bg = "white", col="blue",	cex = 2, lwd = 2) )
#with(margo.neighbours, text(long, lat, labels = paste(dist_km, "km", sep=" "), adj = NULL, pos = 4, offset = 0.5, vfont = NULL, cex = 1.3, col = "black", font = NULL))
dev.off()


map('world', interior= FALSE, col = "grey55") 
with(buckley.tow, points(long, lat, 	type = "p", pch = 21, bg = "dodgerblue2", col="black",	cex = 1.2, lwd = 0.5) )

