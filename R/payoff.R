#' Function calculates transition and payoff period compared to business as usual
#' @param BAU df with models runs with no MPA implemented 
#' @param scenarios df with model runs for all scenatios (implementation year, reserve size etc)
#' @param type All for aggregate analysis, or fishery for fishery level analysis
#' @return A dataframe with each scnario and respective transition, payoff periods and Biomass in year 50 in the reserve patches.

payoff <- function(BAU, scenarios, type){
  require(dplyr)
  All <- inner_join(scenarios, BAU, by=c("Name", "Adjusted", "Year"))%>%data.frame()

  if (type=="All"){
   scen <- All %>% group_by(Adjusted, Implementation_year, Reserve_size, Year)%>%
     summarise_at(.vars= vars(Catch, Biomass, Biomass_MPA, PV, BAU_C, BAU_B, BAU_B_MPA, BAU_PV), .funs =  sum)%>%
     arrange(Year, .by_group = TRUE)%>%
     mutate (C_cum = cumsum(Catch), BAU_C_cum = cumsum(BAU_C))
   
   cross_point <- scen %>%  group_by(Adjusted, Implementation_year, Reserve_size)%>% 
     subset(Year >= Implementation_year & Catch >= BAU_C)%>% filter(Year==min(Year))%>%
     mutate(Transition_period= Year-Implementation_year, Loss = BAU_C_cum - C_cum)%>%
     select(Adjusted, Implementation_year, Reserve_size, Transition_period, Loss)
   
   payoff_point <- inner_join(scen, cross_point, by=c("Adjusted", "Implementation_year", "Reserve_size")) %>%
     mutate(Payoff = BAU_C_cum + Loss)%>%
     group_by(Adjusted, Implementation_year, Reserve_size)%>%
     subset(C_cum >= Payoff)%>%
     filter(Year==min(Year))%>%
     mutate(Payoff_period= Year - Implementation_year)%>%
     select(Adjusted, Implementation_year, Reserve_size, Payoff_period)
   
  t1 <- merge(cross_point, payoff_point, by=c("Adjusted", "Implementation_year", "Reserve_size"), all.x=TRUE)
   
  B_50 <- scen %>% group_by(Adjusted, Implementation_year, Reserve_size) %>%
      filter(Year==max(Year))%>% mutate(B_per_50 = ((Biomass-BAU_B)/BAU_B)*100)
    
    payoff_analysis <- B_50 %>%
      full_join(t1, by=c("Adjusted", "Implementation_year", "Reserve_size"))%>%
      select(Adjusted, Implementation_year, Reserve_size, Transition_period, Payoff_period, B_per_50, Loss)
  
    
    output <- All %>% filter(Year <= 2065)%>% 
      group_by(Adjusted, Implementation_year, Reserve_size)%>%
      summarise_at(.vars= vars(Catch, Biomass, Biomass_MPA, PV, BAU_C, BAU_B, BAU_B_MPA, BAU_PV), .funs =  sum)%>%
      mutate(C_change = Catch - BAU_C,
                             B_change = Biomass - BAU_B, 
                             PV_change = PV - BAU_PV, 
                             C_per = (C_change/BAU_C)*100, 
                             B_per = (B_change/BAU_B)*100,
                             PV_per = (PV_change/BAU_PV)*100)%>%
      select(Adjusted, Implementation_year, Reserve_size, C_change, B_change, PV_change, C_per, B_per, PV_per)%>%
      merge(payoff_analysis, by= c("Adjusted", "Implementation_year", "Reserve_size"))
    
    } else {
      scen <- All %>% group_by(Name, Adjusted, Implementation_year, Reserve_size, Year)%>%
        arrange(Year, .by_group = TRUE)%>%
        mutate (C_cum = cumsum(Catch), BAU_C_cum = cumsum(BAU_C))
      
      cross_point <- scen %>%  group_by(Name, Adjusted, Implementation_year, Reserve_size)%>% 
        subset(Year >= Implementation_year & Catch > BAU_C)%>% filter(Year==min(Year))%>%
        mutate(Transition_period= Year-Implementation_year, Loss = BAU_C_cum - C_cum)%>%
        select(Name, Adjusted, Implementation_year, Reserve_size, Transition_period, Loss)
      
      
      payoff_point <- inner_join(scen, cross_point, by=c("Name","Adjusted", "Implementation_year", "Reserve_size")) %>%
        mutate(Payoff = BAU_C_cum + Loss)%>%
        group_by(Adjusted, Implementation_year, Reserve_size)%>%
        subset(C_cum >= Payoff)%>%
        filter(Year==min(Year))%>%
        mutate(Payoff_period= Year - Implementation_year)%>%
        select(Name, Adjusted, Implementation_year, Reserve_size, Payoff_period)
      
      t1 <- merge(cross_point, payoff_point, by=c("Name","Adjusted", "Implementation_year", "Reserve_size"), all.x=TRUE)
      
      B_50 <- scen %>% group_by(Name, Adjusted, Implementation_year, Reserve_size) %>%
        filter(Year==max(Year))%>% mutate(B_per_50 = ((Biomass-BAU_B)/BAU_B)*100)
      
      payoff_analysis <- B_50 %>%
        full_join(t1, by=c("Name","Adjusted", "Implementation_year", "Reserve_size"))%>%
        select(Name ,Adjusted, Implementation_year, Reserve_size, Transition_period, Payoff_period, B_per_50, Loss)
      
      output <- All %>%  filter(Year <= 2065)%>% 
        group_by(Name, Adjusted, Implementation_year, Reserve_size)%>%
        summarise_at(.vars= vars(Catch, Biomass, Biomass_MPA, PV, BAU_C, BAU_B, BAU_B_MPA, BAU_PV), .funs =  sum)%>%
        mutate(C_change = Catch - BAU_C,
               B_change = Biomass - BAU_B, 
               PV_change = PV - BAU_PV, 
               C_per = (C_change/BAU_C)*100, 
               B_per = (B_change/BAU_B)*100,
               PV_per = (PV_change/BAU_PV)*100)%>%
        select(Name, Adjusted, Implementation_year, Reserve_size, C_change, B_change, PV_change, C_per, B_per, PV_per)%>%
        merge(payoff_analysis, by= c("Name","Adjusted", "Implementation_year", "Reserve_size"))
    }
  
  return(output)  
}
