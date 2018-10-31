
rm(list=ls())

setwd("/Users/marinacostarillo/Google Drive/PhD/Projects")
setwd("./buckley-bias/short_paper")

library(maps)
library(rnaturalearth)
library(sp)
library(scales)

neighb <- read.csv("SST_new_neighbours.csv", header = TRUE)
neighb <- neighb[,c("age", "Latitude", "Longitude")]
names(neighb) <- c("Sample", "lat", "long")

historical_coords <- neighb[which(neighb$Sample=="Historical"), c("lat", "long")]
holocene_coords <- neighb[which(neighb$Sample=="Holocene"), c("lat", "long")]
lgm_coords <- neighb[which(neighb$Sample=="LGM"), c("lat", "long")]


# Guides
pdf("fig_map_dist_guide.pdf", paper = "special",  width = 5, height = 5)
ggplot(neighb, aes(x=lat, y=long, group = Sample, shape = Sample)) + 
  geom_point(aes(color =  Sample, fill = Sample), size = 3, stroke = 0.5) +
  scale_color_manual(values=c("black","black", "black")) +
  scale_fill_manual(values=c(alpha("#e41a1c",0.8), alpha("#4daf4a",0.8),alpha("#377eb8",0.8))) +
  scale_shape_manual(values=c(24, 22, 21)) 
dev.off()

# transforming points data into spatial
coordinates(historical_coords) <- ~long+lat 
proj4string(historical_coords) <- '+init=epsg:4326'
historical_coords <- spTransform(historical_coords, CRS("+proj=wintri"))

coordinates(holocene_coords) <- ~long+lat 
proj4string(holocene_coords) <- '+init=epsg:4326'
holocene_coords <- spTransform(holocene_coords, CRS("+proj=wintri"))

coordinates(lgm_coords) <- ~long+lat 
proj4string(lgm_coords) <- '+init=epsg:4326'
lgm_coords <- spTransform(lgm_coords, CRS("+proj=wintri"))


# world data
world <- rnaturalearth::countries110
# world$name 

# grid lines
grid.lines.mj <- gridlines(world,easts = seq(-180,180,by=30), norths = seq(-90,90,by=30))
grid.lines.mi <- gridlines(world,easts = seq(-165,195,by=15), norths = seq(-90,90,by=15))

# transform all to Winkel Tripel projection
historical_coords <- spTransform(historical_coords, CRS("+proj=wintri"))
holocene_coords <- spTransform(holocene_coords, CRS("+proj=wintri"))
lgm_coords <- spTransform(lgm_coords, CRS("+proj=wintri"))

world <- spTransform(world, CRS("+proj=wintri"))
grid.lines.mj <- spTransform(grid.lines.mj,CRS("+proj=wintri"))
grid.lines.mi <- spTransform(grid.lines.mi,CRS("+proj=wintri"))


# plot
# dev.off()

pdf("fig_map_dist.pdf", paper = "special",  width = 15, height = 8.8)

 par(mar = c(0, 0, 0, 0))
 plot(methods::as(world, 'Spatial'), expandBB=c(0,0,0.1,0))

 plot(grid.lines.mi, col=grey(0.9), add=T)
 plot(grid.lines.mj, col=grey(0.4), add=T)

 plot(world, add=TRUE, border=grey(0.6), col=grey(0.6))
 plot(holocene_coords, add=TRUE, col="black", bg=alpha("#4daf4a",0.8), pch=22, cex = 1, lwd = 1)
 plot(lgm_coords, add=TRUE, col="black", bg=alpha("#377eb8",0.8), pch=21, cex = 1, lwd = 1)
 plot(historical_coords, add=TRUE, col="black", bg=alpha("#e41a1c",0.8), pch=24, cex = 1, lwd = 1)
 
 text(labels(grid.lines.mj, side=c(2,3), labelCRS = CRS("+init=epsg:4326")), col = grey(.4), offset=0.5, cex = 1.5)
dev.off()



###

