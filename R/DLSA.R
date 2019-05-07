#' Function runs the catch only stock assessment method by Froese et al., 2017 using the datalimited2::cmsy2 function for all focal fisheries 
#' @param  data a df with columns: Genus, Ano, and catch time series from column 4-7 representing non-inflated catches and catches inflated by 20%, 40% and 60%, respectively
#' @param  priors a df with columns: Genus (matching each unique Genus in the data dataframe), Resilience (estimated by genus from fishbase), r_lo, r_hi, stb.low, stb.hi,	endb.low,	endb.high
#' @return a df with biological quantity and reference point estimates with 95% confidence intervals and a data frame with B/BMSY, F/FMSY, biomass, and fishing mortality time series with 95% confidence intervals

library(datalimited2)
?cmsy2

stock_assessment<-function(data, priors){
  library(dplyr)
  require(datalimited2)
  
  ref_pts <- data.frame(Name=NA,
                          Adjusted=NA,
                          r= NA, r.low = NA, r.hi = NA,
                          k= NA, k.low = NA,k.hi = NA,
                          msy= NA,msy.low = NA, msy.hi = NA,
                          bmsy= NA, bmsy.low = NA, bmsy.hi = NA,
                          year=NA,	
                          catch=NA,	
                          catch_ma=NA,	
                          b=NA,	
                          b_lo=NA,	
                          b_hi=NA,	
                          bbmsy=NA,	
                          bbmsy_lo=NA,	
                          bbmsy_hi=NA,	
                          s=NA,	
                          s_lo=NA,	
                          s_hi=NA,	
                          f=NA,	
                          f_lo=NA,	
                          f_hi=NA,	
                          fmsy=NA,	
                          fmsy_lo=NA,	
                          fmsy_hi=NA,	
                          ffmsy=NA,	
                          ffmsy_lo=NA,	
                          ffmsy_hi=NA,	
                          er=NA)
  
  ref_ts <- data.frame(Name=NA,
                       Adjusted=NA,
                       year=NA,	
                       catch=NA,	
                       catch_ma=NA,	
                       b=NA,	
                       b_lo=NA,	
                       b_hi=NA,	
                       bbmsy=NA,	
                       bbmsy_lo=NA,	
                       bbmsy_hi=NA,	
                       s=NA,	
                       s_lo=NA,	
                       s_hi=NA,	
                       f=NA,	
                       f_lo=NA,	
                       f_hi=NA,	
                       fmsy=NA,	
                       fmsy_lo=NA,	
                       fmsy_hi=NA,	
                       ffmsy=NA,	
                       ffmsy_lo=NA,	
                       ffmsy_hi=NA,	
                       er=NA)
  
  Genus <- as.vector(unique(data$Genus))
  
  for (s in Genus){
    df <- data%>% filter(Genus == s) 
    year<-as.vector(df$Ano, mode='numeric')
    catches<-(df[,4:7])
    var <-subset(priors, Genus == s)
    resilience <- var$Resilience
    r.low<- var$r_lo
    r.hi<- var$r_hi
    stb.low<- var$stb.low
    stb.hi<- var$stb.hi
    endb.low<- var$endb.low
    endb.hi<- var$endb.hi
    
    for (i in 1:ncol(catches)){
      catch <- catches[,i]
      x_last<- nrow(catch)
      adjusted <- colnames(catches[i])
      
      cmsy <- datalimited2::cmsy2(year=year, catch=catch, resilience = resilience, 
                                  r.low=r.low, r.hi=r.hi, 
                                  stb.low =stb.low, stb.hi=stb.hi,
                                  endb.low=endb.low, endb.hi=endb.hi)
      
      output<- cmsy$ref_ts
      
      Name <- s
      Adjusted <- adjusted
      
      output<- output%>%
        mutate(Name = Name, 
               Adjusted = Adjusted)
      
      R <- cmsy$ref_pts[1,2:4]
      K <- cmsy$ref_pts[2,2:4]
      MSY <- cmsy$ref_pts[3,2:4]
      BMSY <- cmsy$ref_pts[5,2:4]
      tail <- tail(cmsy$ref_ts, 1)
      out <- cbind(Name, Adjusted, R, K, MSY, BMSY, tail)
      
      names(out)[1:14] <- c("Name", "Adjusted",
                            "r", "r.low", "r.hi", 
                            "k", "k.low", "k.hi",
                            "msy", "msy.low", "msy.hi",
                            "bmsy", "bmsy.low", "bmsy.hi")
      
      ref_pts <- rbind(ref_pts, out)
      
      ref_ts<- rbind(ref_ts, output)
    }
  }
  return (list(ref_pts=ref_pts[-1,], ref_ts=ref_ts[-1,]))
}
