## Created 22 March 2017
## Marina Costa Rillo
## 
## Reads: all_bias_size.csv ; 
## Creates: 
##

rm(list=ls())
library(ggplot2)
library(latticeExtra)
library(RColorBrewer) # colours of plots

setwd("/Users/marinacostarillo/Google Drive/DOUTORADO/R_data")

df.bias <- read.csv("Bias_analysis/data_morpho/bias_morpho_individuals.csv", header = TRUE, stringsAsFactors=FALSE)
names(df.bias)


############################################### 
######### Size distributions boxplots ######### 
####### Buckley vs. Resamples  Datasets #######
###############################################

dwidth <- 0.8 # space between pair-boxplots

###### Boxplots by SAMPLE

for(i in unique(df.bias$sample)){ # i=25

  df.bias.subset <- df.bias[which(df.bias$sample == i), ]
  
  b <- ggplot(df.bias.subset, aes(x=sspname, y=area_log, fill=dataset)) + geom_boxplot(position = position_dodge(width=dwidth))
  
  pdf(file = paste("Bias_analysis/plots_morpho/boxplots_by_sample",i, "_sizebx.pdf", sep=""), paper = "a4r", width = 20, height = 8) # default: inches
  print(b + labs(title = paste("Sample",i,sep=" "), y = "Log(area)", x = element_blank()) +
    geom_text(data = df.bias.subset, aes(x = sspname, y = area_log_max+0.1, label = total_ind), size = 6, color="red",position = position_dodge(width=dwidth)) +
    theme(axis.text.y=element_text(size=14), 
          axis.text.x=element_text(size=11, face="italic", colour="black"), axis.ticks.x=element_blank(),
          axis.title=element_text(size=14),
          plot.title = element_text(size = 16, face="bold")) )
  dev.off()
  
}


###### Boxplots by SPECIES

for(i in unique(df.bias$species)){ # i = c("menardii")
  
  df.bias.subset <- df.bias[which(df.bias$species == i), ]
  df.bias.subset$sample <- as.character(df.bias.subset$sample)
  
  b <- ggplot(df.bias.subset, aes(x=sample, y=area_log, fill=dataset)) + geom_boxplot(position = position_dodge(width=dwidth))
  
  pdf(file = paste("Bias_analysis/plots_morpho/boxplots_by_species",i, "_sizebx.pdf", sep=""), paper = "a4r", width = 15, height = 8) # default: inches
  print(b + labs(title = unique(df.bias.subset$sspname), y = "Log(area)", x = "Sample ID number") +
    geom_text(data = df.bias.subset, aes(label = total_ind, y = area_log_max+0.1), size = 6, color="red", position = position_dodge(width=dwidth)) +
    theme(axis.text.y=element_text(size=14), 
          axis.text.x=element_text(size=14), axis.ticks.x=element_blank(),
          axis.title=element_text(size=16),
          plot.title = element_text(size = 18, face="bold.italic")) )
  dev.off()
}



#######################################################################
##########################  Statistical test ##########################
#######################################################################

# Two Datasets:
df.bias.subset <- df.bias[which(df.bias$species == "menardii"), ]

dfA <- df.bias.subset[which(df.bias.subset$datasetAB == "A"), ] # Resample
dfB <- df.bias.subset[which(df.bias.subset$datasetAB == "B"), ] # Buckley

ggplot(dfA, aes(x=area_log)) + geom_histogram(binwidth=0.05) + 
  labs(x="Log(area)",y="No. individuals", title="Size Distribution")

###### All samples together
ks.test(x=dfA$area_log, y=dfB$area_log, alternative = c("two.sided"),exact = NULL)
# H0: x and y were drawn from the same continuous distribution
# p-value < 0.05 : reject H0 = x and y come from different distributions


###### Per sample - see in which sample(s) they differ
ks.same <- data.frame(species = character(), sample = integer(), p = numeric(),stringsAsFactors=FALSE)
ks.diff <- data.frame(species = character(), sample = integer(), p = numeric(),stringsAsFactors=FALSE)


for (i in unique(df.bias$species)){ # i = "glutinata"
      df.bias.subset <- df.bias[which(df.bias$species == i), ]

      if(any(duplicated(df.bias.subset$area_log))){
        print(paste(i, "ties"))
        duplicates <- c()
        dupli <- df.bias.subset[which(duplicated(df.bias.subset$area_log)),"area_log"]
        for (i in 1:length(dupli)){
          duplicates <- c(duplicates, which(df.bias.subset$area_log==dupli[i]))
        }
        print(df.bias.subset[duplicates,])
      }
      
      dfA <- df.bias.subset[which(df.bias.subset$datasetAB == "A"), ] # Resample
      dfB <- df.bias.subset[which(df.bias.subset$datasetAB == "B"), ] # Buckley
      
      all(unique(dfA$sample) == unique(dfB$sample))
      ks.sample <- data.frame(sample=double(),D=double(), p=double())
      n=1
      for (j in unique(dfA$sample)){ # j=20
        dfAj <- dfA[which(dfA$sample == j), ]
        dfBj <- dfB[which(dfB$sample == j), ]
        test <- ks.test(x=dfBj$area_log, y=dfAj$area_log, alternative = c("two.sided"),exact = NULL)
        ks.sample[n,] <- c(j, as.numeric(test$statistic),test$p.value)
        n = n+1
        rm(dfAj,dfBj,test)
      }
      
      if(any(ks.sample$p>0.05)){
        same.sample <- ks.sample[which(ks.sample$p>0.05),"sample"]
        same.p <- ks.sample[which(ks.sample$p>0.05),"p"]
        same.ssp <- rep(i, length(same.p))
        ks.same <- rbind(ks.same, data.frame(same.ssp, same.sample, same.p))
        
      }
      if(any(ks.sample$p<0.05)){
        diff.sample <- ks.sample[which(ks.sample$p<0.05),"sample"]
        diff.p <- ks.sample[which(ks.sample$p<0.05),"p"]
        diff.ssp <- rep(i, length(diff.p))
        ks.diff <- rbind(ks.diff, data.frame(diff.ssp, diff.sample, diff.p))
      }
}


### Plotting
dfAj <- dfA[which(dfA$sample == 44), ]
dfBj <- dfB[which(dfB$sample == 44), ]
ecdf1 <- ecdf(dfAj$area_log)
ecdf2 <- ecdf(dfBj$area_log)

print(ecdf2)
plot(ecdf2, ylab="Fn(x)", verticals = TRUE, col.01line = "gray70", do.points=TRUE, pch = 19, col='blue')
plot(ecdf1, ylab="Fn(x)", verticals = TRUE, col.01line = "gray70", do.points=TRUE, pch = 19, col='red', add=TRUE)



### Checking normality of the data
# with(df1, tapply(area_log, species, shapiro.test))
shapiro.test(dfA$area_log)
shapiro.test(dfA$area)
shapiro.test(dfB$area_log)
shapiro.test(dfB$area)



