#' Marine Reserve patch model wrapper for all scenarios 
#' @param data is a dataframe with inputs required for the MPA_model.R function 
#' @param years number of years of projection
#' @param MPA.mat matrix of 0 and 1; 0= marine reserve patches, 1= open access patches
#' @param start.year refers to the year in which the maine reseves are implemented 
#' @return dataframe with biomass, catch, and profits made across all patches includng average, high and low bounds

MPA_wrapper <- function(data, years, MPA.mat, start.year, size) {
  source(here::here("R", "MPA_model.R"))
  
  out<-data.frame(Name=NA, 
                  Adjusted=NA, 
                  Year=NA,
                  Catch=NA,
                  Biomass=NA,
                  Biomass_MPA=NA,
                  Fishing=NA,
                  PV=NA,
                  Implementation_year=NA,
                  Reserve_size=NA)

  for (s in 1:nrow(data)){
    Name <- data$Name[s]
    Adjusted <- data$Adjusted[s]
    r <- data$r[s]
    K <- data$k[s]
    Fishing <- data$f[s]
    B<- data$b[s]
    mrate<- data$m.rate[s]
    MSY <- data$msy[s]
    bmsy <- data$bmsy[s]
    fmsy <- data$fmsy [s]
    p <- data$p[s]
    c <- data$c[s] 
    profit.msy <- data$profit.msy[s]
    
    MPA<-MPA.Model(Name = Name, Adjusted= Adjusted, r=r, K=K, B=B, Fishing=Fishing, years=years, MPA.mat=MPA.mat, 
                   mrate=mrate, MSY=MSY, bmsy=bmsy, fmsy=fmsy, p=p, c=c, profit.msy=profit.msy, start.year=start.year)
    
    new <- MPA%>% mutate(Reserve_size = size)
    
    out <- rbind(out, new)
    
   
  }
  return(out[-1,])
}
