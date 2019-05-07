#' Marine Reserve patch model for high and low bounds of each variable 
#' @param data is a dataframe with inputs required for the MPA_model.R function 
#' @param years number of years of projection
#' @param MPA.mat matrix of 0 and 1; 0= marine reserve patches, 1= open access patches
#' @param start.year refers to the year in which the maine reseves are implemented 
#' @return dataframe with biomass, catch, and profits made across all patches includng average, high and low bounds


Scenarios <- function(data, years, MPA.mat, start.year) {
  source(here::here("R", "MPA_model.R"))
  
  out<-data.frame(Name=NA, Adjusted=NA)
  
  growth <- as.numeric(data[,c("r", "r.low", "r.hi")])
  car.cap <- as.numeric(data[,c("k", "k.low", "k.hi")])
  harvest <- as.numeric(data[,c("f", "f_lo", "f_hi")])
  Biom <- as.numeric(data[,c("b", "b_lo", "b_hi")])
  price <- as.numeric(data[,c("p", "p.lo", "p.hi")])
  msy <- as.numeric(data[,c("msy", "msy.low", "msy.hi")])
  Bmsy <- as.numeric(data[,c("bmsy", "bmsy.low", "bmsy.hi")])
  Fmsy <- as.numeric(data[,c("fmsy", "fmsy_lo", "fmsy_hi")])
  m.rate<- as.numeric(data[,"m.rate"])
  cost <- as.numeric(data[,c("c", "c.lo", "c.hi")])
  Profit_msy <- as.numeric(data[,c("profit.msy", "profit.msy.lo", "profit.msy.hi")])
  
  for (s in 1:3){
    r <- growth[s]
    K <- car.cap[s]
    Fishing <- harvest[s]
    B<- Biom[s]
    mrate<- m.rate
    MSY <- msy[s]
    bmsy <- Bmsy[s]
    fmsy <- Fmsy [s]
    p <- price[s]
    c <- cost[s] 
    profit.msy <- Profit_msy[s]
    
    MPA<-MPA.Model(r=r, K=K, B=B, Fishing=Fishing, years=years, MPA.mat=MPA.mat, 
                   mrate=mrate, MSY=MSY, bmsy=bmsy, fmsy=fmsy, p=p, c=c, profit.msy=profit.msy, start.year=start.year)
    
    out<-(cbind(out, MPA))
  }
  out<- out[,-c(15,27)]
  
  out<-out %>%
    mutate(Name=data$Name,
           Adjusted=data$Adjusted)
  
  names(out) <- c("Name",
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
  return(out)
}
