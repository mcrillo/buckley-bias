### Description
# Returns a list with residuals of the model: intercept = 0, slope = 1 (e.g. buckley = resample)
# Creates list: output/size_pop_residuals.xlsx

### Arguments
# size_pop_df: bias_size_pop.csv 
# overwrite: TRUE or FALSE (if the output file already exist, do you want to re-run the function and overwrite it?)

get_size_pop_residuals <- function(size_pop_df, file_name, overwrite){
  
  if(file.exists(paste("output/",file_name,".xlsx", sep = "")) && overwrite == FALSE){
    
    residuals <-  function_read_list(file= paste("output/",file_name,".xlsx", sep = ""))

    return(residuals)
    
  }else{
    
    # making life easier
    resample <- size_pop_df[which(size_pop_df$datasetAB %in% c("A")),]
    buckley <- size_pop_df[which(size_pop_df$datasetAB %in% c("B")),]
    
    cols_log <- grep(pattern = "log", names(buckley))
    # Residuals of model buckley = resample
    # res = y - x, for model y = f(x) = x, i.e. buckley = resample
    res_log <- sapply(cols_log, function(i) buckley[,i] - resample[,i])
    colnames(res_log) <- colnames(buckley[cols_log])
    
    # Same thing now for sqrt
    cols_sqrt <- grep(pattern = "sqrt", names(buckley))
    res_sqrt <- sapply(cols_sqrt, function(i) buckley[,i] - resample[,i])
    colnames(res_sqrt) <- colnames(buckley[cols_sqrt])
    
    
    # Residual sum of squares
    rss_log <- apply(res_log, 2, function(x) sum(x^2))
    rss_sqrt <- apply(res_sqrt, 2, function(x) sum(x^2))
    # Mean squared error
    mse_log <- apply(res_log, 2, function(x) sum(x^2)/length(x))
    mse_sqrt <- apply(res_sqrt, 2, function(x) sum(x^2)/length(x))
    rownames <- gsub("sqrt_","", colnames(res_sqrt))  # to save in list
    
    # Adding sample and species info
    res_log  <- cbind(buckley[,c("species","sspname","sample")],res_log)
    res_sqrt <- cbind(buckley[,c("species","sspname","sample")],res_sqrt)
    
    # Joining everything into a list
    residuals <- list(sqrt = res_sqrt, log = res_log, stats = data.frame(rownames,rss_log,rss_sqrt,mse_log,mse_sqrt))
    
    function_write_list(residuals, wb_name = paste("output/",file_name,".xlsx", sep = ""))
      
    return(residuals)
      
    
  } # if
}