#' Function calculates equilibrium values based on market price 
#' @param mrate df with columns Name and mrate
#' @param pts df with fishery and biological parameters, output from the DLSA.R function
#' @param price df with columns Name and price (USD/MT)
#' @param eq B/Bmsy at which open access is at equilibrium- default set to 30%
#' @return A dataframe with cost and profits made at MSY

economic_model <- function (mrate, pts, price, eq=0.3){
  
  #simulating scearios with illegal fishing pressure removed
  IUU_20_legal <- pts%>% filter(Adjusted=="IUU_20")%>% mutate(f = f * 0.8, Adjusted ="IUU_20_legal")
  
  IUU_40_legal <- pts%>% filter(Adjusted=="IUU_40")%>% mutate(f = f * 0.6, Adjusted ="IUU_40_legal")
  
  IUU_60_legal <- pts%>% filter(Adjusted=="IUU_60")%>% mutate(f = f * 0.4, Adjusted ="IUU_60_legal")
  
  pts <- rbind(pts, IUU_20_legal, IUU_40_legal, IUU_60_legal)

  #open access equilibrium at 30%
  pts<- merge(pts, mrate, by="Name")
  data <- merge(pts, price, by="Name")%>%
    mutate (f_bar = 2 * (1- eq/2),
            c = (p * f_bar * eq *msy)/(fmsy * f_bar), 
            c.lo = (p.lo * f_bar * eq *msy.low)/(fmsy_lo * f_bar),
            c.hi = (p.hi * f_bar * eq *msy.hi)/(fmsy_hi * f_bar),
            profit.msy = p * msy - (c* fmsy),
            profit.msy.lo = p.lo * msy.low - (c.lo* fmsy_lo),
            profit.msy.hi = p.hi * msy.hi - (c.hi* fmsy_hi))%>%
    as.data.frame()
  
  return(data)
}

