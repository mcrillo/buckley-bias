### Description
# Saving all slides ZFs numbers from Buckley Collection used in the bias analysis

### Arguments
# resamp_morpho_df
# overwrite : TRUE or FALSE (if the output file already exist, do you want to re0run the function and overwrite it?)

get_buckley_zf <- function(resamples_df){
    zf_bias <- resamp_morpho_df$ZF.PF.no. # plus merged slides!!
    merged_slides <- c(6234,7782,6235,6236,6266,7777,7778,7779,6270,6271,5751,5752,5757,5758,7783,7784,7785,5982,5983,5986,5986,
                       5987,7772,7773,7774,7770,7775,7776,6316,7069,6723,6724,6725,6726,6727,6728,6729,6730,6406,6407,6423,6424,
                       6690,6691,6010,6013,6057,6019,6020,6021,6022,6023,6024,6040,6041,6011,6012,6018,6058) 
    # from dupli_merged_explained.rtf file 
    zf_all <- c(zf_bias,merged_slides)
    zf_all <- unique(zf_all)
    zf_all <- sort(zf_all)

    return(zf_all)
}


