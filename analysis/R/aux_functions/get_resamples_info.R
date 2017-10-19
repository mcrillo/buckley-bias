### Description
# 
# creates data/resample_info.csv & data/resample_info_morpho.csv     

### Usage
# table <- get_resamples_info(wands_df, morpho_df, overwrite = TRUE)

### Arguments
# wands_df	: 
# morpho_df	: 
# overwrite : TRUE or FALSE (if the output file already exist, do you want to re0run the function and overwrite it?)

get_resamples_info <- function(morpho_df, overwrite){

  if(file.exists("data/resample_info.csv") && overwrite == FALSE){
      sample_table <- read.csv("data/resample_info.csv", header = TRUE)
      return(sample_table)
    
  }else{
      wands_df <- read.csv("data/raw_data/Wandsworth_loans_MRillo.csv", header = TRUE, stringsAsFactors=FALSE)
      # Organizing data
      wands_df$Sample.depth.MIN..cm.<-as.numeric(wands_df$Sample.depth.MIN..cm.) # NAs
      wands_df$Sample.depth.MAX..cm.<-as.numeric(wands_df$Sample.depth.MAX..cm.) # NAs
      wands_df <- wands_df[-which(wands_df[,"Sample..g..approx."]==0),] # deleting sediments that I did/could not sample
      # length(wands_df[,1]) # number of samples taken
      
      irn_morpho <- sort(unique(morpho_df[,"Residue.OBD.IRN"]))
      # length(irn_morpho) # samples that I used for the morphometric
      intersect_samples <- intersect(wands_df[,"Residue.IRN"],irn_morpho) # samples used for morphometric AND re-sampled in Wandsworth
      # length(intersect_samples) # samples that I re-sampled and used for morphometrics
      
      # Getting list of sampled sediments AND used for morphometrics
      morpho_sampled <- morpho_df[which(morpho_df[,"Residue.OBD.IRN"] %in% intersect_samples),]
      # morpho_not_sampled <- morpho_df[which(morpho_df[,"Residue.OBD.IRN"] %!in% intersect_samples),]
      wands_sampled <- wands_df[which(wands_df[,"Residue.IRN"] %in% intersect_samples),]
      
      # Checking if they match
      # identical(sort(unique(morpho_sampled[,"Residue.OBD.IRN"])),sort(intersect_samples))
      # identical(sort(unique(as.integer(wands_sampled[,"Residue.IRN"]))),sort(intersect_samples))
      # identical(sort(unique(morpho_sampled[,"Residue.OBD.IRN"])),sort(unique(as.integer(wands_sampled[,"Residue.IRN"]))))
      
      # Counting, for each sample, how many species I used for the morphometrics (the more, the better to do the shell size bias analysis)
      morpho_sampled[,"total_ssp_morpho"] <- c()
      for (i in 1 : length(unique(morpho_sampled[,"sample"]))){
        sample_no <- unique(morpho_sampled[,"sample"])[i]
        morpho_sampled[which(morpho_sampled[,"sample"]==sample_no),"total_ssp_morpho"] <- length(unique(morpho_sampled[which(morpho_sampled[,"sample"]==sample_no),"species"]))
      }
      
      
      # Creating re-samples info table with number of species measured for each sample
      sample_table <- unique(morpho_sampled[,c("sample","total_ssp_morpho","Lat.decimal","Long.decimal","Vessel","Collection.date","Museum.no.",
                                               "Residue.OBD.IRN","Sea.Depth..m....modern.methods","Sample.depth.MIN..cm.","Sample.depth.MAX..cm.")])
      names(sample_table) <- c("sample","total_ssp_morpho","Lat", "Long","Vessel","Date","Museum_no","OBD_IRN", "Sea_Depth", "cm_MIN", "cm_MAX")
      
      
      # Creating re-samples info table with names of the species measured for each sample
      sample_ssp_table <- unique(morpho_sampled[,c("ZF.PF.no.","sample","total_ssp_morpho","species","total_ind","Lat.decimal","Long.decimal","Vessel","Collection.date","Museum.no.",
                                                   "Residue.OBD.IRN","Sea.Depth..m....modern.methods","Sample.depth.MIN..cm.","Sample.depth.MAX..cm.")])
      names(sample_ssp_table) <- c("ZF.PF.no.","sample","total_ssp_morpho","species","total_ind_Buckley","Lat", "Long","Vessel","Date","Museum_no","OBD_IRN", "Sea Depth", "cm_MIN", "cm_MAX")
      
      
      # Samples that I chose to pick:
      chosen <- c(5,10,20,25,27,31,44,46,55,66)
      
      sample_table <- sample_table[which(sample_table[,"sample"] %in% chosen),]
      sample_ssp_table <- sample_ssp_table[which(sample_ssp_table[,"sample"] %in% chosen),]
      
      write.csv(sample_table, file = "data/resample_info.csv",row.names=FALSE)
      write.csv(sample_ssp_table, file = "data/resample_info_morpho.csv",row.names=FALSE)
      
      return(sample_table)
      
  } # else
}
