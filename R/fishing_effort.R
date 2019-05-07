#' Function calculates fishing effort in the next time step based on profits that can be made  
#' @param  p is the price per metric ton ($/ MT)
#' @param MSY is the maximum sustainable yield, output from the DLSA.R function
#' @param r is the intrinisc growth rate, output from the DLSA.R function 
#' @param bmsy is the biomass that enables fishery to to deliver MSY, output from the DLSA.R function
#' @param fmsy sit he fishing effort that enables a fishery to deliver MSY, output from the DLSA.R function 
#' @param F.mat is a matrix of fishing effort in each patch of the model
#' @param B.mat is the biomass available in each patch of the model 
#' @param c is the cost associated with fishing 
#' @param profit.msy is the profit that is made at MSY 
#' @param t is the n year, used for discounting 
#' @param  discount is the discount rate, default of 10% 
#' @return a list including F.mat (fishing effort for the next time step), profit, PV (present value of profit) and revenue 

fishing_effort<- function (p, MSY, r, bmsy, fmsy, F.mat, B.mat, c, profit.msy, t, discount=0.1){
  ffmsy.mat <- F.mat/fmsy 
  bbmsy.mat <- B.mat/bmsy
  MSY <- MSY/11236 
  c <- c/11236 
  profit.msy <- profit.msy/11236 
  
  revenue <- p * F.mat * B.mat
  profit <-  revenue - (c * fmsy * ffmsy.mat)
  
  PV <- profit/((1 + discount)^t) #10% discount rate
  
  ffmsy.mat <- ffmsy.mat + (0.1 * (profit/profit.msy)) #lambda = 0.1
  F.mat <- ffmsy.mat * fmsy
  
  return(list(F.mat, profit, PV, revenue))}