library(dplyr)
library(ggplot2)
library(tidyr)
library(doParallel)
library(foreach)

######################################################################################
# Filtering CONAPESCA Catch Data
######################################################################################

landingsites<- read.csv (here::here("raw_data", "landingsites.csv"))%>%
  filter(Confirmado=="y")

fisheries<-read.csv(here::here("raw_data", "fisheries.csv"), stringsAsFactors = FALSE)

catch<- readRDS (here::here("raw_data", "conapesca.RDS"))%>%
  filter(Estado=="Baja california"| Estado=="Sonora")%>%
  filter(!(NombrePrincipal=="Macarela"|NombrePrincipal=="Sardina"|NombrePrincipal=="Camaron"|NombrePrincipal=="Corvina"
           |NombrePrincipal=="Calamar"|NombrePrincipal=="Anchoveta"))%>%
  tidyr::separate(NombreCientifico,into="Genus", sep=" ", extra='drop', remove=FALSE)%>%
  mutate_at(.vars = "Genus", .funs = gsub, pattern = " spp", replacement = "")%>%
  merge(landingsites, by="SitioDesembarque")%>%
  merge(fisheries, by="Genus")%>%
  group_by(Genus, Ano)%>%
  summarize(IUU=sum(PesoVivo)/1000)%>% #conversion of kg to MT
  mutate(IUU_20= IUU*1.2, IUU_40= IUU*1.4, IUU_60=IUU*1.6)%>% #calculating IUU scenarios
  filter(length(unique(Ano))>5|length(unique(Ano))==5) #filtering >5 years of data for DLSA


#write.csv(catch, file.path(here::here("inputs"), "catch.csv"))

######################################################################################
# Filtering CONAPESCA Catch Data
######################################################################################

start <- Sys.time()
#Fisheries filtered by most landed, most valuable and confidence in catch series
catch <- read.csv(here::here("inputs", "catch.csv"))%>%
  filter(Genus=="Atrina" | Genus == "Callinectes"|Genus=="Cephalopholis" | Genus == "Dasyatis"|
           Genus=="Epinephelus" | Genus == "Lutjanus"|Genus=="Micropogonias" | Genus == "Mugil"|
           Genus=="Octopus" | Genus == "Panulirus"|Genus=="Scomberomorus"| Genus=="Squatina")

priors <- read.csv(here::here("inputs", "priors.csv"))

source(here::here("R", "DLSA.R"))

stock <- stock_assessment(data=catch, priors = priors)

ref_pts<- stock[[1]]
ref_ts <- stock[[2]]
R_K <- stock[[3]]

write.csv(ref_pts, file.path(here::here("inputs"), "ref_pts.csv"))
write.csv(ref_ts, file.path(here::here("inputs"), "ref_ts.csv"))
write.csv(R_K, file.path(here::here("inputs"), "r_k_viables.csv"))

######################################################################################
# Calculating Open access equilibrium
######################################################################################

source(here::here("R", "EconomicModel.R"))

mrate <- read.csv(here::here("raw_data", "mrate.csv"))
pts <- read.csv(here::here("inputs","ref_pts.csv"))
price <- read.csv(here::here("raw_data", "MarketPrice.csv"))

inputs <- economic_model(mrate=mrate, pts=pts, price=price)

BAU_inputs<- inputs%>%
  filter(!(Adjusted=="IUU_20_legal" |Adjusted=="IUU_40_legal" |Adjusted=="IUU_60_legal"))

#MPA.mat <- read.csv(here::here("inputs", "MPA.matrix.csv"))

######################################################################################
# Running the MPA model
######################################################################################

source(here::here("R", "MPA_size_fun.R"))

BAU_out <- MPA_size_fun (data=BAU_inputs, years=115, start.year=0)

bau_20<- BAU_out%>% filter(Adjusted == "IUU_20")%>% mutate(Adjusted = "IUU_20_legal")
bau_40<- BAU_out%>% filter(Adjusted == "IUU_40")%>% mutate(Adjusted = "IUU_40_legal")
bau_60<- BAU_out%>% filter(Adjusted == "IUU_60")%>% mutate(Adjusted = "IUU_60_legal")

BAU <- rbind(BAU_out, bau_20, bau_40, bau_60)

#write.csv(BAU, file.path("inputs", "BAU.csv"))

ggplot(BAU, aes(x=Year, y = Catch, group=Adjusted, color =Adjusted))+
  geom_line()+
  facet_wrap(~Name, scales ="free")

system.time({

  scenarios <- foreach(i = seq(2015, 2030, 5), .combine = rbind) %do%
    MPA_size_fun (data=inputs, years=100, start.year=i)
})


plot<- scenarios%>%
  filter(Reserve_size==1.00)%>%filter(Implementation_year==2015)

ggplot(plot, aes(x=Year, y = Catch, group=Adjusted, color =Adjusted))+
  geom_line()+
  facet_wrap(~Name, scales ="free")

  
write.csv(scenarios, file.path("inputs", "Scenarios.csv"))


######################################################################################
#Payoff Analysis 
######################################################################################
source(here::here("R", "payoff.R"))
BAU <- read.csv(here::here("inputs", "BAU.csv"))%>%
  plyr::rename(c("Catch"="BAU_C", "Biomass"="BAU_B","Biomass_MPA"="BAU_B_MPA", "PV"="BAU_PV"))%>%
  select(Name, Adjusted, Year, BAU_C, BAU_B, BAU_B_MPA, BAU_PV)

scenarios <- read.csv(here::here("inputs", "Scenarios.csv"))

payoff_analysis <- payoff(BAU=BAU, scenarios=scenarios, type="All")

write.csv(payoff_analysis, file.path("inputs", "payoff_all.csv"))

payoff_analysis$Implementation_year <- as.factor(payoff_analysis$Implementation_year)

ggplot(payoff_analysis, aes(x=Reserve_size, y=Payoff_period, group=Implementation_year, color=Implementation_year))+
  geom_point(size=2)+
  geom_line()+
  facet_wrap(~Adjusted, scales="free")+
  theme_classic()

ggplot(payoff_analysis, aes(x=Reserve_size, y=B_per, group=Implementation_year, color=Implementation_year))+
  geom_point(size=2)+
  facet_wrap(~Adjusted, scales="free")+
  theme_classic()

ggplot(payoff_analysis, aes(x=Reserve_size, y=PV_per, group=Implementation_year, color=Implementation_year))+
  geom_point(size=2)+
  facet_wrap(~Adjusted, scales="free")+
  theme_classic()

end<- Sys.time()

end-start











