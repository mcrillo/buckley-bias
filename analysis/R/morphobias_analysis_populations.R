## Created 03 April 2017
## Marina Costa Rillo
## 
## Reads: all_bias_size.csv ; 
## Creates: 
##


df.stats <- read.csv("Bias_analysis/data_morpho/bias_morpho_populations.csv", header = TRUE, stringsAsFactors=FALSE)

list.split <- split(df.stats, df.stats$datasetAB) # split accordingly to datasetAB
colnames(list.split$A)[6:19] <- paste(colnames(list.split$A)[6:19],"A", sep = "")
colnames(list.split$B)[6:19] <- paste(colnames(list.split$B)[6:19],"B", sep = "")
df.statsAB <- cbind(list.split$A,list.split$B[6:19])
df.statsAB <- df.statsAB[,-4] # drop duplicate column 'datasetAB'



########################################################
######################## STATS #########################
########################################################
names(df.statsAB)

lmax <- lm(area_log_maxB ~ area_log_maxA, df.statsAB)
summary(lmax)

lmedian <- lm(area_log_medianB ~ area_log_medianA, df.statsAB)
summary(lmedian)

lmean <- lm(area_log_meanB ~ area_log_meanA, df.statsAB)
summary(lmean)

l75q <- lm(area_log_75qB ~ area_log_75qA, df.statsAB)
summary(l75q)

l95q <- lm(area_log_95qB ~ area_log_95qA, df.statsAB)
summary(l95q)

# Analysing a bit 
lmax$coefficients
lmax$predicted <- predict(lmax)   # Save the predicted values
lmax$residuals <- residuals(lmax)
flmax <- fortify(lmax)
ggplot(flmax, aes(x = .fitted, y = .resid)) + geom_point()
plot(lmax)
head(fortify(lmax))



### Fitting all to the model: intercept = 0, slope = 1

rmax <- df.statsAB$area_log_maxB - df.statsAB$area_log_maxA 
rmean <- df.statsAB$area_log_meanB - df.statsAB$area_log_meanA 
rmedian <- df.statsAB$area_log_medianB - df.statsAB$area_log_medianA 
r95q <- df.statsAB$area_log_95qB - df.statsAB$area_log_95qA 
r75q <- df.statsAB$area_log_75qB - df.statsAB$area_log_75qA 

residuals <- data.frame(rmean, rmedian, r75q, r95q, rmax)

apply(residuals, 2, var)
apply(residuals, 2, mean)

res <- melt(residuals)
str(res) 


#########################################################
######################### PLOTS #########################
#########################################################

### Violin plot of the residuals

b <- ggplot(data = res, aes(x=variable, y=value)) + geom_violin(aes(fill=variable), position=position_dodge(width=0.8))

b + labs(y = "Residuals", x = element_blank()) + ylim (-3,3) +
    scale_x_discrete(labels=c("Mean", "Median", "75%q", "95%q", "Maximum")) + 
    scale_fill_manual(values=c("#7c96a0","#5e5749","#a48e8e","#5d5f6d","#717f70")) +
       theme(axis.text.y = element_text(size=14, color = "black", face="bold"), 
            axis.title.y = element_text(size=16, color = "black", face="bold"), 
            axis.text.x = element_text(size=16, color = "black", face="bold"), 
            axis.ticks.x = element_blank(), legend.position="none") 



# Defining colors for each species
cores21 <- c("#7c3f27","#7144ca","#c1dd44","#c949bd","#67ce59","#4b2d6b","#ccab3e","#6479c8",
           "#d44430","#78dcc1","#ce3f71","#4d9060","#c588c4","#c3d088","#682b42","#7aa7c2",
           "#cd7f41","#3d4342","#cf7a80","#5f692d","#ccb6a8")
length(cores21) == length(unique(df.statsAB$species))


###########
### Max ###
###########
bmax <- ggplot(df.statsAB, aes(y = area_log_maxB, x = area_log_maxA) ) 

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
bmean <- ggplot(df.statsAB, aes(y = area_log_meanB, x = area_log_meanA) ) 

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
bmedian <- ggplot(df.statsAB, aes(y = area_log_medianB, x = area_log_medianA) ) 

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
b95q <- ggplot(df.statsAB, aes(y = area_log_95qB, x = area_log_95qA) ) 

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
b75q <- ggplot(df.statsAB, aes(y = area_log_75qB, x = area_log_75qA) ) 

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

