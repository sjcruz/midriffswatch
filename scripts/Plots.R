library(dplyr)
library(ggplot2)
library(tidyr)
library(ggrepel)

###############################
#Total catch and by genus

catch<- read.csv(here::here("inputs", "catch.csv"))

ggplot(catch, aes(x=Ano, y=IUU, color=Genus))+
  geom_line(size=1)+
  geom_point()+
  labs(x="Year", y="Yearly total catch catch (MT)", title="Catch series by Genus", subtitle= "2005-2015")+
  theme_classic()+
  scale_x_continuous(limits = c(2005, 2015), expand=c(0,0), breaks= c(2005, 2015), labels = c("2005", "2015"))+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.position = "none")+
  facet_wrap(~Genus, scales = "free", ncol = 7)


###############################
#Fishery status in 2015

status <- read.csv(here::here("inputs", "ref_pts.csv"))%>%
  filter(Adjusted=="IUU")

bmsy<- expression(paste(B/B[MSY]))
fmsy <- expression(paste(F/F[MSY]))

Kobe<- ggplot(status, aes(x= bbmsy, y=ffmsy))+
  geom_rect(xmin = 0.0, xmax = 1.0, ymin = 0.0, ymax = 1, fill = 'yellow', alpha = 0.1) +
  geom_rect(xmin = 0, xmax = 1, ymin = 1.0, ymax = 7.1, fill = 'red', alpha = 0.1) +
  geom_rect(xmin = 1, xmax = 1.5, ymin = 0, ymax = 1, fill = 'green', alpha = 0.1) +
  geom_rect(xmin = 1, xmax = 2.1, ymin = 1, ymax = 7.1, fill = 'orange', alpha = 0.1)+
  geom_point(aes(size=catch), show.legend = FALSE)+
  scale_size(range = c(2, 8))+
  labs(x=bmsy, y=fmsy)+
  geom_vline(xintercept = 1, linetype="dotted", color="black") +
  geom_hline(yintercept = 1, linetype="dotted", color="black") +
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(limits=c(0, 7.1),expand=c(0,0), breaks = c(0, 1, 2, 4, 6), labels= c(0, 1, 2, 4, 6))+
  theme_classic(base_size = 9)+
  geom_text_repel(aes(label = Name, size=4),show.legend=FALSE)

ggsave(here::here("images", "status_2015.jpg"),  width=4, height=2, dpi=900)

######################################################################
#Reserve size and payoff periods for all reserve sizes 5-50%

payoff_analysis <- read.csv(here::here("inputs", "payoff_all.csv"))
payoff_analysis$Implementation_year <- as.factor(payoff_analysis$Implementation_year)

ggplot(payoff_analysis, aes(x=Reserve_size, y=Payoff_period, group=Implementation_year, color=Implementation_year))+
  geom_point(size=2)+
  facet_wrap(~Adjusted)+
  theme_classic()

ggsave(here::here("images", "size_payoff.jpg"),  width=6, height=4, dpi=900)

######################################################################
#Line plots: comparisson of scenarios and BAU

BAU <- read.csv(here::here("inputs", "BAU.csv"))%>%
  plyr::rename(c("Catch"="BAU_C", "Biomass"="BAU_B","Biomass_MPA"="BAU_B_MPA", "PV"="BAU_PV"))%>%
  select(Name, Adjusted, Year, BAU_C, BAU_B, BAU_B_MPA, BAU_PV)
scenarios <- read.csv(here::here("inputs", "Scenarios.csv"))

All <- inner_join(scenarios, BAU, by=c("Name", "Adjusted", "Year"))%>%data.frame()

summary <- All %>% group_by(Adjusted, Implementation_year, Reserve_size, Year)%>%
    summarise_at(.vars= vars(Catch, Biomass, Biomass_MPA, PV, BAU_C, BAU_B, BAU_B_MPA, BAU_PV), .funs =  sum)%>%
  filter(Reserve_size==0.10, Implementation_year==2030, Adjusted=="IUU_20_legal")%>%
  ungroup()

p1<- ggplot(summary, aes(x=Year, y=Catch, group=Reserve_size))+
  geom_line()+
  geom_line(aes(x=Year, y=BAU_C, color="BAU"))+
  facet_wrap(~Adjusted)
p1

######################################################################
#Reserve size and payoff periods for 5% reserve size at the fishery level

payoff_fishery<- read.csv (here::here("inputs", "payoff_fishery.csv"))%>%filter(Adjusted=="IUU" , Implementation_year==2015, Reserve_size==0.05)

######################################################################
#Line plots: comparisson of scenarios and BAU at the fishery level 

five <- All %>% filter(Adjusted=="IUU_20_legal", Implementation_year==2020, Reserve_size==0.15)

ggplot(five, aes(x=Year, y=Catch, group=Name, color="scenario"))+
  geom_line()+
  geom_line(aes(x=Year, y=BAU_C, color="BAU"))+
  facet_wrap(~Name, scales = "free")



