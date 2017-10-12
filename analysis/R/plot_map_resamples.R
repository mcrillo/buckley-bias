
plot_map_resamples <- function(resamples_df){
  
  # Plotting map with morphometric samples and re-sampled ones (with sample number)
  # setwd("/Users/marinacostarillo/Desktop")

  # Maptools dataset
  data(wrld_simpl)
  world <- fortify(wrld_simpl)
  
  # plotting map
  mapplot <- ggplot(world, mapping = aes(x = long, y = lat, group = group)) +
    geom_polygon(fill = "black", colour = "black")
    # + coord_cartesian(xlim = c(-125, -30), ylim = c(-60, 35))
  
  # adding points and labels
  mapplot <- mapplot + geom_point(data=resamples_df, aes(x=Long, y=Lat, group = sample), color="red", size=3) +
      geom_text(data=resamples_df, aes(x=Long, y=Lat,group=sample,label=sample),hjust=-0.3, vjust=0,color="red")
  
  # saving
  pdf(file = c("output/resamples_map.pdf"), paper = "a4r", width = 12, height = 14)
      print(mapplot)
    dev.off()
  
}


# OLD
#  map('world', interior= FALSE, col = "grey55")
# with(resamples_df, points(Long, Lat, 	type = "p", pch = 21, bg ="white", col="black",	cex = 1.2, lwd = 1) )
# with(resamples_df, text(Long, Lat, labels = sample, adj = NULL, pos = 4, offset = 0.5, vfont = NULL,cex = 1, col = "black", font = NULL))

