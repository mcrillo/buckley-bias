### Description
# Histograms (per sample) of relative abundances of 3 datasets (Boas, Buckley, ForCenS)
# ONLY PLOTS if folder "abund_histograms" DOES NOT exists


### Arguments
# assemb_relat_df
# resamples_df
# overwrite : TRUE or FALSE (if the output file already exist, do you want to re-run the function and overwrite it?)


plot_size_histograms <- function(){
  
  
  if (!file.exists("output/size_histograms") | overwrite == TRUE){
    
    if (!file.exists("output/size_histograms")){
      dir.create("output/size_histograms")}
    
    
    
    
    
  } # if
}