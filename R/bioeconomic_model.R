#' Marine Reserve patch model for high and low bounds of each variable and every fishery 
#' @param df is a dataframe with inputs required for the MPA_model.R function 
#' @param years number of years of projection
#' @param MPA.mat matrix of 0 and 1; 0= marine reserve patches, 1= open access patches
#' @param start.year refers to the year in which the maine reseves are implemented 
#' @return dataframe with biomass, catch, and profits made across all patches includng average, high and low bounds


bioeconomic_model<- function(df, years, MPA.mat, start.year) {
  source(here::here("R", "Scenarios.R"))
  
  results <- data.frame(matrix(ncol = 36, nrow = 0))
  x <- c("Name",
         "Adjusted", 
         "Year",
         "Leave_est",
         "Arrive_est", 
         "Surplus_est",
         "Catch_est",
         "Biomass_est",
         "Profit_est",
         "Fishing_est",
         "PV_est",
         "Revenue_est",
         "bbmsy", 
         "ffmsy",
         "Leave_lo",
         "Arrive_lo", 
         "Surplus_lo",
         "Catch_lo",
         "Biomass_lo", 
         "Profit_lo",
         "Fishing_lo",
         "PV_lo",
         "Revenue_lo",
         "bbmsy_lo", 
         "ffmsy_lo",
         "Leave_hi",
         "Arrive_hi", 
         "Surplus_hi",
         "Catch_hi",
         "Biomass_hi",
         "Profit_hi",
         "Fishing_hi",
         "PV_hi", 
         "Revenue_hi",
         "bbmsy_hi", 
         "ffmsy_hi")
  colnames(results) <- x
  
  for(i in 1:nrow(df)) {
    
    data <- df[i,]
    
    scen <- Scenarios(data=data, years=years, MPA.mat=MPA.mat, start.year=start.year)  
    
    results <- rbind(results, scen)
  }
  results<-results%>%
    mutate(Implementation_year = start.year)
  return(results)
}
