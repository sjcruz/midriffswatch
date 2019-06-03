#' Marine Reserve patch model wrapper for various reserve sizes 
#' @param data is a dataframe with inputs required for the MPA_model.R function 
#' @param years number of years of projection
#' @param MPA.mat matrix of 0 and 1; 0= marine reserve patches, 1= open access patches
#' @param start.year refers to the year in which the maine reseves are implemented 
#' @return dataframe with biomass, catch, and profits made across all patches


MPA_size_fun<- function (data, years, MPA.mat, start.year){
  if (start.year == 0){
    size <- 0 }else{
    size <- c(seq(0.05, 0.90, .05)) }
  
  ########## Start parallel computing for other scenarios
  mycluster <- makeCluster(4)
  registerDoParallel(mycluster)
  
  exp <- foreach(s = c(size), .combine = rbind, .packages="dplyr") %dopar%{
  source(here::here("R", "MPA_wrapper.R"))
  if(s <= 0.05){
    MPA.mat <- MPA.mat
    } else {
    MPA.mat[sample(106*106, round(106*106*(s - 0.05)), replace = TRUE)] <- 0
      #MPA.mat <- matrix(nrow=106, ncol=106, 1)
      #p<- round(sqrt(106*106*size))
      #MPA.mat[sample(106*106*size, )] <- 0
    }
    
  MPA_wrapper(data=data, years=years, MPA.mat=MPA.mat, start.year=start.year, size=s)
  }
  stopCluster(mycluster)
  ########## Start parallel computing for other scenarios
  return(exp)
}
