#' Marine Reserve patch model  
#' @param r is the intrinisc growth rate, output from the DLSA.R function 
#' @param K is the fishery's carrying capcity, output from the DLSA.R function
#' @param Fishing is the fishing effort, output from the DLSA.R function
#' @param B is the available biomass, output from the DLSA.R function
#' @param years number of years of projection
#' @param MPA.mat matrix of 0 and 1; 0= marine reserve patches, 1= open access patches
#' @param m.rate proportion of available biomass moving/spilling over
#' @param MSY is the maximum sustainable yield, output from the DLSA.R function
#' @param bmsy is the biomass that enables fishery to to deliver MSY, output from the DLSA.R function
#' @param fmsy sit he fishing effort that enables a fishery to deliver MSY, output from the DLSA.R function 
#' @param p is the price per metric ton ($/ MT)
#' @param c is the cost associated with fishing 
#' @param profit.msy is the profit that is made at MSY 
#' @param start.year refers to the year in which the maine reseves are implemented 
#' @return dataframe with biomass, catch, and profits made across all patches 


MPA.Model <- function(Name, Adjusted, r, K, Fishing, B, years, MPA.mat, mrate, MSY, bmsy, fmsy, p, c, profit.msy, start.year){
  source(here::here("R", "fishing_effort.R"))
 
  patches <- 106 #matrix is 106 by 106 patches for a total of 11236 patches 
  F.mat <- matrix(ncol=patches, nrow=patches, Fishing) 
  K.mat <- matrix(ncol=patches, nrow=patches, K/11236)  
  B.mat <- matrix(ncol=patches, nrow=patches, B/11236)  
  arriving <- matrix(nrow=patches, ncol=patches, 0) 
  procrastination <- max(start.year-2015, 0)
  Years<- as.numeric(2015:(2015+ years + procrastination))
  
  l.patch <- as.numeric(c(patches, 1: (patches-1)))
  r.patch <- as.numeric(c(2: patches, 1))
  u.patch <- as.numeric(c(patches, 1: (patches-1)))
  d.patch <- as.numeric(c(2: patches, 1))
  
  summary<- data.frame(Name=NA, 
                       Adjusted=NA, 
                       Year=NA,
                       Catch=NA,
                       Biomass=NA,
                       Biomass_MPA=NA,
                       Fishing=NA,
                       PV=NA, 
                       Implementation_year=NA)
  
  for (i in Years){
    
    if(i == start.year){F.mat = F.mat*MPA.mat} else {F.mat = F.mat}
    
    t <- i - 2015
    
    F.out <- fishing_effort (t=t, p=p, MSY=MSY, r=r, bmsy=bmsy, fmsy=fmsy, 
                             F.mat=F.mat, B.mat=B.mat, c=c, profit.msy = profit.msy)
    
    F.mat <- as.matrix(F.out[[1]])
    PV <-  as.matrix(F.out[[3]])
    
    leaving <- mrate*B.mat 
    leaving[leaving < 0] <- 0
    
    for(row in 1:nrow(arriving)) {
      for(col in 1:ncol(arriving)) {
        
        arriving [row, col] <-  0.25*leaving[row, l.patch[col]]+ 
                                0.25*leaving[row, r.patch[col]] + 
                                0.25*leaving[u.patch[row], col] + 
                                0.25*leaving[d.patch[row], col]
      }
    } #close nested forloop for calculating arriving
    
    arriving[arriving < 0] <- 0
    surplus <- r * B.mat *(1 - B.mat/K.mat)
    surplus[surplus < 0] <- 0
    catches <- F.mat * B.mat
    catches[catches < 0] <- 0
    B.mat <- B.mat + surplus - catches - leaving + arriving
    B.mat[B.mat < 0] <- 0
    
    outside <- B.mat*MPA.mat
   
    output<- data.frame (Name = Name,
                         Adjusted = Adjusted,
                         Year= i,
                         Catch = sum(catches),
                         Biomass = sum(B.mat),
                         Biomass_MPA = sum(B.mat) - sum(outside),
                         Fishing = mean(F.mat), 
                         PV = sum(PV), 
                         Implementation_year=start.year)
    
    summary <- rbind (summary, output)
  }
  return(summary[-1,])
}