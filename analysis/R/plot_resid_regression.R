### Description
# regression of the residuals of model buckley = resample

### Arguments
# res : melt(residuals[[i]]) (residuals: list, output of get_size_pop_residuals.R)
# overwrite : TRUE or FALSE (if the output file already exist, do you want to re-run the function and overwrite it?)

plot_resid_regression <- function(residuals, tranfs, overwrite){
  
  if (!file.exists(paste("output/resid_",transf,"_violin.pdf", sep = "")) | overwrite == TRUE){
  }
  

# Defining colors for each species
cores21 <- c("#7c3f27","#7144ca","#c1dd44","#c949bd","#67ce59","#4b2d6b","#ccab3e","#6479c8",
           "#d44430","#78dcc1","#ce3f71","#4d9060","#c588c4","#c3d088","#682b42","#7aa7c2",
           "#cd7f41","#3d4342","#cf7a80","#5f692d","#ccb6a8")
length(cores21) == length(unique(morpho_statsAB$species))


###########
### Max ###
###########
bmax <- ggplot(morpho_statsAB, aes(y = area_log_maxB, x = area_log_maxA) ) 

pdf(file = sprintf("Bias_analysis/plots_morpho/max_size.pdf"), width=16, height=10, paper = "special")
print(
bmax + geom_point(aes(colour = factor(sspname)), size = 3.5)  +  xlim(10, 14.5) + ylim(10, 14.5) +   
  geom_abline(intercept = 0, slope = 1) + geom_abline(intercept = lmax$coefficients[1], slope=lmax$coefficients[2], col="red")+
  labs(y = "Maximum log(area) - Buckley Collection", x = "Maximum log(area) - Resamples") +
  scale_color_manual(values=cores21, name= c("Species")) +
  guides(col = guide_legend(ncol = 1), fill = guide_legend(title = "Species")) +
  theme(axis.text=element_text(size=16,colour = "black"), axis.title=element_text(size=16,face="bold"),
        aspect.ratio = 1, legend.text = element_text(size = 16, face="italic"), legend.title = element_text(size=16, face="bold"))

) 
dev.off()


############
### Mean ###
############
bmean <- ggplot(morpho_statsAB, aes(y = area_log_meanB, x = area_log_meanA) ) 

pdf(file = sprintf("Bias_analysis/plots_morpho/mean_size.pdf"), width=16, height=10, paper = "special")
print(
bmean + geom_point(aes(colour = factor(sspname)), size = 3.5)  +  xlim(10, 14.5) + ylim(10, 14.5) +   
  geom_abline(intercept = 0) +
  labs(y = "Mean log(area) - Buckley Collection", x = "Mean log(area) - Resamples") +
  scale_color_manual(values=cores21, name= c("Species")) +
  guides(col = guide_legend(ncol = 1), fill = guide_legend(title = "Species")) +
  theme(axis.text=element_text(size=16,colour = "black"), axis.title=element_text(size=16,face="bold"),
        aspect.ratio = 1, legend.text = element_text(size = 16, face="italic"), legend.title = element_text(size=16, face="bold"))
) 
dev.off()


##############
### Median ###
##############
bmedian <- ggplot(morpho_statsAB, aes(y = area_log_medianB, x = area_log_medianA) ) 

pdf(file = sprintf("Bias_analysis/plots_morpho/median_size.pdf"), width=16, height=10, paper = "special")
print(
bmedian + geom_point(aes(colour = factor(sspname)), size = 3.5)  +  xlim(10, 14.5) + ylim(10, 14.5) +   
  geom_abline(intercept = 0) +
  labs(y = "Median log(area) - Buckley Collection", x = "Median log(area) - Resamples") +
  scale_color_manual(values=cores21, name= c("Species")) +
  guides(col = guide_legend(ncol = 1), fill = guide_legend(title = "Species")) +
  theme(axis.text=element_text(size=16,colour = "black"), axis.title=element_text(size=16,face="bold"),
        aspect.ratio = 1, legend.text = element_text(size = 16, face="italic"), legend.title = element_text(size=16, face="bold"))
) 
dev.off()

############
### 95-q ###
############
b95q <- ggplot(morpho_statsAB, aes(y = area_log_95qB, x = area_log_95qA) ) 

pdf(file = sprintf("Bias_analysis/plots_morpho/95q_size.pdf"), width=16, height=10, paper = "special")
print(
b95q + geom_point(aes(colour = factor(sspname)), size = 3.5)  +  xlim(10, 14.5) + ylim(10, 14.5) +   
  geom_abline(intercept = 0) +
  labs(y = "95-quantile log(area) - Buckley Collection", x = "95-quantile log(area) - Resamples") +
  scale_color_manual(values=cores21, name= c("Species")) +
  guides(col = guide_legend(ncol = 1), fill = guide_legend(title = "Species")) +
  theme(axis.text=element_text(size=16,colour = "black"), axis.title=element_text(size=16,face="bold"),
        aspect.ratio = 1, legend.text = element_text(size = 16, face="italic"), legend.title = element_text(size=16, face="bold"))
) 
dev.off()

############
### 75-q ###
############
b75q <- ggplot(morpho_statsAB, aes(y = area_log_75qB, x = area_log_75qA) ) 

pdf(file = sprintf("Bias_analysis/plots_morpho/75q_size.pdf"), width=16, height=10, paper = "special")
print(
b75q + geom_point(aes(colour = factor(sspname)), size = 3.5)  +  xlim(10, 14.5) + ylim(10, 14.5) +   
  geom_abline(intercept = 0) +
  labs(y = "75-quantile log(area) - Buckley Collection", x = "75-quantile log(area) - Resamples") +
  scale_color_manual(values=cores21, name= c("Species")) +
  guides(col = guide_legend(ncol = 1), fill = guide_legend(title = "Species")) +
  theme(axis.text=element_text(size=16,colour = "black"), axis.title=element_text(size=16,face="bold"),
        aspect.ratio = 1, legend.text = element_text(size = 16, face="italic"), legend.title = element_text(size=16, face="bold"))
) 
dev.off()

}