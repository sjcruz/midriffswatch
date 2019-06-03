library(sensitivity)
library(tidyverse)
library(dplyr)
library(foreach)
library(doParallel)

source(here::here("R", "MPA_model.R"))
source(here::here("R", "sensitivity.R"))
MPA.mat <- read.csv(here::here("inputs", "MPA.matrix.csv"))
mrate <- read.csv(here::here("raw_data", "mrate.csv"))
price <- read.csv(here::here("raw_data", "MarketPrice.csv"))%>% merge(mrate, by="Name")

pts <- read.csv(here::here("inputs","ref_pts.csv"))%>% 
  merge(price, by="Name")%>%
  mutate (r.sd = (r.hi-r)/2, k.sd = (k.hi-k)/2, msy.sd = (msy.hi-msy)/2, 
          bmsy.sd= (bmsy.hi-bmsy)/2, b.sd = (b_hi-b)/2, f.sd = (f_hi-f)/2,
          fmsy.sd= (fmsy_hi-fmsy)/2)

nsamples <- 200 

r_k_viables <- read.csv(here::here("inputs", "r_k_viables.csv"))%>%filter(Name=="Atrina", Adjusted=="IUU")

sens <- pts[1,]

# create our two samples for Sobel
r_k <- r_k_viables %>% sample_n(nsamples)
ps1 = cbind.data.frame(r=r_k$viable_r, k=r_k$viable_k, 
                       msy=rnorm(nsamples,mean=sens$msy, sd=sens$msy.sd),
                       bmsy=rnorm(nsamples,mean=sens$bmsy, sd=sens$bmsy.sd), 
                       b=rnorm(nsamples,mean=sens$b, sd=sens$b.sd),
                       f=rnorm(nsamples,mean=sens$f, sd=sens$f.sd),
                       fmsy=rnorm(nsamples,mean=sens$fmsy, sd=sens$fmsy.sd),
                       p=runif(min=sens$p.lo, max=sens$p.hi, n=nsamples),
                       mrate=runif(min=0, max=1, n=nsamples))

r_k <- r_k_viables %>% sample_n(nsamples)
ps2 <- cbind.data.frame(r=r_k$viable_r, k=r_k$viable_k, 
                       msy=rnorm(nsamples,mean=sens$msy, sd=sens$msy.sd),
                       bmsy=rnorm(nsamples,mean=sens$bmsy, sd=sens$bmsy.sd), 
                       b=rnorm(nsamples,mean=sens$b, sd=sens$b.sd),
                       f=rnorm(nsamples,mean=sens$f, sd=sens$f.sd),
                       fmsy=rnorm(nsamples,mean=sens$fmsy, sd=sens$fmsy.sd),
                       p=runif(min=sens$p.lo, max=sens$p.hi, n=nsamples),
                       mrate=runif(min=0, max=1, n=nsamples))


sens_micro <- sobol2007(model = NULL, ps1, ps2, nboot = 100)

nsim=nrow(sens_micro$X)

#Loop through nsamples of parameters 
start<- Sys.time()
mycluster <- makeCluster(4)
registerDoParallel(mycluster)

results <- foreach (r = 1:nsim, .combine = "rbind", .packages = c("dplyr", "foreach", "doParallel")) %dopar% {
  inputs <- econ_sens_funs(Name=sens$Name, Adjusted=sens$Adjusted,  parms=sens_micro$X[r,])
  #Calculates BAU 
  BAU <-MPA.Model(Name = inputs$Name, Adjusted=inputs$Adjusted, r=inputs$r, K=inputs$k, B=inputs$b, 
                  Fishing=inputs$f, years=65, MPA.mat=MPA.mat, mrate=inputs$mrate, MSY=inputs$msy, 
                  bmsy=inputs$bmsy, fmsy=inputs$fmsy, p=inputs$p, c=inputs$c, profit.msy=inputs$profit.msy, 
                  start.year=0)
  
  BAU<- BAU%>% plyr::rename(c("Catch"="BAU_C", "Biomass"="BAU_B","Biomass_MPA"="BAU_B_MPA", "PV"="BAU_PV"))%>%
    select(Name, Adjusted, Year, BAU_C, BAU_B, BAU_B_MPA, BAU_PV)
  
  #Calculates scenario 
  scenarios<- MPA.Model(Name = inputs$Name, Adjusted=inputs$Adjusted, r=inputs$r, K=inputs$k, B=inputs$b, 
                        Fishing=inputs$f, years=50, MPA.mat=MPA.mat, mrate=inputs$mrate, MSY=inputs$msy, 
                        bmsy=inputs$bmsy, fmsy=inputs$fmsy, p=inputs$p, c=inputs$c, profit.msy=inputs$profit.msy, 
                        start.year=2015)
  
  payoff <- sens_payoff(BAU = BAU, scenarios = scenarios)
  
  return(payoff)
}

stopCluster(mycluster)

end<- Sys.time()
end-start

payoff<- results[,4]


#conert Inf to 99999? 


sens_micro<- tell(sens_micro, payoff)

as.data.frame(sens_micro$S)
sens_micro$T

tmp = cbind.data.frame(sens_micro$X, pop12=sens_micro$y)
ggplot(tmp, aes(p12, pop12))+geom_point()+labs(x="Survivability of 1 to older",y="pop after 12 months")







pars <- pse::tell(pars, t(res[,4:6]), res.names=c("Transition_period", "Payoff_preriod", "B_per_50"))
pse::plotscatter(pars, col="blue", cex=5)

pse::plotprcc(pars)
pars$prcc
pse::plotecdf(pars)

end<- Sys.time()
end-start