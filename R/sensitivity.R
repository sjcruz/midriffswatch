#' Function calculates equilibrium values based on market price 
#' @param mrate df with columns Name and mrate
#' @param pts df with fishery and biological parameters, output from the DLSA.R function
#' @param price df with columns Name and price (USD/MT)
#' @param eq B/Bmsy at which open access is at equilibrium- default set to 30%
#' @return A dataframe with cost and profits made at MSY

econ_sens_funs <- function (Name, Adjusted, parms, eq=0.3){
  parms <- cbind(Name, Adjusted, parms)
  
  #simulating scearios with illegal fishing pressure removed
  IUU_20_legal <- parms%>% filter(Adjusted=="IUU_20")%>% mutate(f = f * 0.8, Adjusted ="IUU_20_legal")
  
  IUU_40_legal <- parms%>% filter(Adjusted=="IUU_40")%>% mutate(f = f * 0.6, Adjusted ="IUU_40_legal")
  
  IUU_60_legal <- parms%>% filter(Adjusted=="IUU_60")%>% mutate(f = f * 0.4, Adjusted ="IUU_60_legal")
  
  pts <- rbind(parms, IUU_20_legal, IUU_40_legal, IUU_60_legal)
  
  #open access equilibrium at 30%
  data <- pts%>%
    mutate (f_bar = 2 * (1- eq/2),
            c = (p * f_bar * eq *msy)/(fmsy * f_bar), 
            profit.msy = p * msy - (c* fmsy))%>%
    as.data.frame()
  
  return(data)
}


sens_payoff <- function(BAU, scenarios){
  require(dplyr)
  
  bau_20<- BAU%>% filter(Adjusted == "IUU_20")%>% mutate(Adjusted = "IUU_20_legal")
  bau_40<- BAU%>% filter(Adjusted == "IUU_40")%>% mutate(Adjusted = "IUU_40_legal")
  bau_60<- BAU%>% filter(Adjusted == "IUU_60")%>% mutate(Adjusted = "IUU_60_legal")
  
  BAU <- rbind(BAU, bau_20, bau_40, bau_60)
  
  All <- inner_join(scenarios, BAU, by=c("Name", "Adjusted", "Year"))%>%data.frame()
  
  scen <- All %>% group_by(Name, Adjusted, Implementation_year, Year)%>%
    arrange(Year, .by_group = TRUE)%>%
    mutate (C_cum = cumsum(Catch), BAU_C_cum = cumsum(BAU_C))

  cross_point <- scen %>%  group_by(Name, Adjusted, Implementation_year)%>% 
    subset(Year >= Implementation_year & Catch > BAU_C)%>% filter(Year==min(Year))%>%
    mutate(Transition_period= Year-Implementation_year, Loss = BAU_C_cum - C_cum)%>%
    select(Name, Adjusted, Implementation_year, Transition_period, Loss)
  
    
  payoff_point <- inner_join(scen, cross_point, by=c("Name","Adjusted", "Implementation_year")) %>%
    mutate(Payoff = BAU_C_cum + Loss)%>%
    group_by(Adjusted, Implementation_year)%>%
    subset(C_cum >= Payoff)%>%
    filter(Year==min(Year))%>%
    mutate(Payoff_period= Year - Implementation_year)%>%
    select(Name, Adjusted, Implementation_year, Payoff_period)
  
  t1 <- merge(cross_point, payoff_point, by=c("Name","Adjusted", "Implementation_year"), all.x=TRUE)
  
  B_50 <- scen %>% group_by(Name, Adjusted, Implementation_year) %>%
    filter(Year==max(Year))%>% mutate(B_per_50 = ((Biomass-BAU_B)/BAU_B)*100)
  
  B_50$B_per_50[is.nan(B_50$B_per_50)] <- 0
  B_50$B_per_50[mapply(is.infinite, B_50$B_per_50)] <- NA
  
  payoff_analysis <- B_50 %>%
    full_join(t1, by=c("Name","Adjusted", "Implementation_year"))%>%
    select(Name, Adjusted, Implementation_year, Transition_period, Payoff_period)
  
  output <- All %>% group_by(Name, Adjusted, Implementation_year)%>%
    summarise_at(.vars= vars(Catch, Biomass, Biomass_MPA, PV, BAU_C, BAU_B, BAU_B_MPA, BAU_PV), .funs =  sum)%>%
    mutate(C_change = Catch - BAU_C,
           B_change = Biomass - BAU_B, 
           PV_change = PV - BAU_PV, 
           C_per = (C_change/BAU_C)*100, 
           B_per = (B_change/BAU_B)*100,
           PV_per = (PV_change/BAU_PV)*100)%>%
    select(Name, Adjusted, Implementation_year, C_per, B_per, PV_per)%>%
    merge(payoff_analysis, by= c("Name","Adjusted", "Implementation_year"))
  
  output$C_per[is.nan(output$C_per)] <- 0
  output$B_per[mapply(is.infinite,output$B_per)] <- NA
  
  return(output)  
}



