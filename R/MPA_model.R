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


MPA.Model <- function(r, K, Fishing, B, years, MPA.mat, mrate, MSY, bmsy, fmsy, p, c, profit.msy, start.year){
  source(here::here("R", "fishing_effort.R"))
  
  patches <- 106 #matrix is 106 by 106 patches for a total of 11236 patches 
  F.mat <- matrix(ncol=patches, nrow=patches, Fishing) 
  K.mat <- matrix(ncol=patches, nrow=patches, K/11236)  
  B.mat <- matrix(ncol=patches, nrow=patches, B/11236)  
  arriving <- matrix(nrow=nrow(MPA.mat), ncol= ncol(MPA.mat), 0) 
  Years<- as.vector(2015:(2015+ years))
  
  l.patch <- c(patches, 1: (patches-1))
  left.patch<-as.matrix(do.call(rbind, replicate(patches, l.patch, simplify=FALSE)))
  r.patch <- c(2: patches, 1)
  right.patch<-as.matrix(do.call(rbind, replicate(patches, l.patch, simplify=FALSE)))
  u.patch <- c(patches, 1: (patches-1))
  up.patch<-as.matrix(do.call(cbind, replicate(patches, u.patch, simplify=FALSE)))
  d.patch <- c(2: patches, 1)
  down.patch<-as.matrix(do.call(cbind, replicate(patches, d.patch, simplify=FALSE)))
  
  summary<- data.frame(Year=NA,
                       Leave=NA, 
                       Arrive=NA,
                       Surplus=NA,
                       Catch=NA,
                       Biomass=NA, 
                       Profit=NA,
                       Fishing=NA,
                       PV=NA, 
                       Revenue=NA,
                       bbmsy=NA,
                       ffmsy=NA)
  
  for (i in Years){
    
    if(i == start.year){F.mat <- F.mat*MPA.mat} else {F.mat <- F.mat}
    
    t <- i - 2015
    
    F.out <- fishing_effort (t=t, p=p, MSY=MSY, r=r, bmsy=bmsy, fmsy=fmsy, 
                                F.mat=F.mat, B.mat=B.mat, c=c, profit.msy = profit.msy)
    F.mat <- (F.out[[1]])
    profit <- (F.out[[2]])
    PV <- (F.out[[3]])
    revenue <- F.out[[4]]
    
    leaving <- mrate*B.mat 
    leaving[leaving < 0] <- 0
    
    for(row in 1:nrow(arriving)) {
      for(col in 1:ncol(arriving)) {
        arriving [row, col] <- 0.25*leaving[left.patch[row, col]]+ 
          0.25*leaving[right.patch[row, col]] + 
          0.25*leaving[up.patch[row, col]] + 
          0.25*leaving[down.patch[row, col]]
        
      }
    } #close nested forloop for calculating arriving
    
    arriving[arriving < 0] <- 0
    surplus <- r * B.mat *(1 - B.mat/K.mat)
    surplus[surplus < 0] <- 0
    catches <- F.mat * B.mat
    catches[catches < 0] <- 0
    B.mat <- B.mat + surplus - catches - leaving + arriving
    B.mat[B.mat < 0] <- 0
    
    output<- data.frame (Year= i, 
                         Leave = sum(leaving) , 
                         Arrive = sum(arriving) , 
                         Surplus = sum(surplus),
                         Catch = sum(catches),
                         Biomass = sum(B.mat),
                         Profit = sum (profit),
                         Fishing = mean (F.mat), 
                         PV = sum(PV), 
                         Revenue= sum(revenue),
                         bbmsy = sum(B.mat)/bmsy,
                         ffmsy = mean (F.mat)/fmsy)
    summary <- rbind (summary, output)
  }
  return(summary[-1,])
}