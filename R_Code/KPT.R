#Code by R.Kronenberg [06112017]
#
#
#
##****************************************************************************
FDPSIDWF<-function(i){
#d PSI / d WETNES, used in 2nd approximation to iteration timestep
#input
#  WETNES     wetness, fraction of saturation
#  PSIF       matrix potential at field capacity, kPa
#  BEXP       exponent for psi-theta relation
#  WETINF     wetness at dry end of near-saturation range
#  WETF       saturation fraction at field capacity
#  CHM        Clapp and Hornberger m, kPa
#  CHN        Clapp and Hornberger n
#output
#  FDPSIDW    d PSI / d WETNES, kPa
#
  if (WETNES[i] < WETINF[i]){ 
    FDPSIDW <- (-BEXP[i] * PSIF[i] / WETF[i]) * (WETNES[i] / WETF[i]) ^ (-BEXP[i] - 1)
  }else if (WETNES[i] < 1){
    #  in near-saturated range
    FDPSIDW <- CHM[i] * (2 * WETNES[i] - CHN[i] - 1)
  }else{
    #  saturated
    FDPSIDW <- 0
  }
  return(FDPSIDW)
}

#****************************************************************************
FPSIMF<-function(WETNESi, PSIFi, BEXPi, WETINFi, WETFi, CHMi, CHNi){
#matric potential from wetness
#input
#  WETNES     wetness, fraction of saturation
#  PSIF       matrix potential at field capacity, kPa
#  BEXP       exponent for psi-theta relation
#  WETINF     wetness at dry end of near-saturation range
#  WETF       saturation fraction at field capacity
#  CHM        Clapp and Hornberger m, kPa
#  CHN        Clapp and Hornberger n
#output
#  FPSIM     matric potential, kPa
#
  if (WETNESi <= 0){ 
    #  arbitrary very negative value
    FPSIM <- -10000000000#
  }else if (WETNESi < WETINFi) {
    FPSIM <- PSIFi * (WETNESi / WETFi) ^ (-BEXPi)
  }else if (WETNESi < 1) {
    #  in near-saturated range
    FPSIM <- CHMi* (WETNESi - CHNi) * (WETNESi - 1)
  }else{
    #  saturated
  FPSIM <- 0
  }
  
  return(FPSIM)
}

#******************************************************************************
SOILPAR<-function(){
#calculated soil water parameters and initial variables
#input
#  NLAYER%  #number of soil layers
#  THICK() layer thicknesses, mm"
#  THSAT() theta at saturation, matrix porosity
#  STONEF()stone volume fraction, unitless"
#  THETAF()volumetric water content at field capacity"
#  PSIF()  matric potential at field capacity, kPa
#  BEXP()  exponent for psi-theta relation
#  WETINF()wetness at dry end of near-saturation range
#  PSIM()  matric soil water potential for layer, kPa
#  KF()    hydraulic conductivity at field capacity, mm/d
#  PSICR   minimum plant leaf water potential, MPa
#output
#  PSIG()  gravity potential, kPa
#  SWATMX()maximum water storage for layer, mm
#  WETF()  wetness at field capacity, dimensionless
#  WETC()  wetness at PSICR, dimensionless
#  CHM()   Clapp and Hornberger m, kPa
#  CHN()   Clapp and Hornberger n
#  WETNES()wetness, fraction of saturation
#  SWATI() water volume in layer, mm
#  KSAT()  saturated hydraulic conductivity, mm/d
#local
#Dim i%      #soil layer
  PSIINF<-rep(1,50)
#          potential at dry end of near saturation range, kPa
#constant
#  RHOWG   density of water times acceleration of gravity, kPa/mm
#
  wetff<-WETF
  psigg<-PSIG
  thickk<-THICK
  SWATMx<-SWATMX
  PSIINf<-  PSIINF
  CHm<-CHM
  CHn<-CHN
  WETNEs<-WETNES
  SWATi<-SWATI
  KSAt<-KSAT
  WETc<-WETC
  
  for(i in 1:NLAYER){
    #  gravity potential is negative down from surface
    if (i == 1) {
      psigg[1] <- -RHOWG * thickk[1] / 2
    }else{
      psigg[i] <- psigg[i - 1] - RHOWG * ((thickk[i - 1] + thickk[i]) / 2)
    }
    SWATMx[i] <- thickk[i] * THSAT[i] * (1 - STONEF[i])
    wetff[i] <- THETAF[i] / THSAT[i]
    PSIINf[i] <- PSIF[i] * (WETINF[i] / wetff[i]) ^ -BEXP[i]
    CHm[i] <- (-PSIINf[i] / (1 - WETINF[i]) ^ 2) - BEXP[i] * (-PSIINf[i]) / (WETINF[i] * (1 - WETINF[i]))
    CHn[i] <- 2 * WETINF[i] - 1 - (-PSIINf[i] * BEXP[i] / (CHm[i] * WETINF[i]))
    if (PSIM[i] > 0) {
     # Stop
    }else if (PSIM[i] == 0) {
      WETNEs[i] <- 1
    }else{
      WETNEs[i]<- wetff[i] * (PSIM[i] / PSIF[i]) ^ (-1 / BEXP[i])
      if (WETNEs[i] > WETINF[i]){
        WETNEs[i] <- (1 + CHn[i]) / 2 + 0.5 * (CHn[i] ^ 2 - 2 * CHn[i] + 1 + 4 * PSIM[i] / CHm[i])^(1/2)
      }
    }
    SWATi[i] <- WETNEs[i] * SWATMx[i]
    KSAt[i] <- KF[i] * (1 / wetff[i]) ^ (2 * BEXP[i] + 3)
    WETc[i] <- wetff[i] * (1000 * PSICR / PSIF[i]) ^ (-1 / BEXP[i])
  }

return(list(PSICR, psigg, SWATMx, wetff, WETc, CHm, CHn, WETNEs, SWATi, KSAt))
}

#**************************************************************************
SOILVAR<-function(){
#soil water variables
#input
#  NLAYER% number of soil layers
#  PSIG()  gravity potential, kPa
#  PSIM()  matric soil water potential for layer, kPa
#  WETNES()wetness, fraction of saturation
#  THSAT() theta at saturation, matrix porosity
#  KF()    hydraulic conductivity at field capacity, mm/d
#  BEXP()  exponent for psi-theta relation
#  WETF()  wetness at field capacity, dimensionless
#  SWATI() water volume in layer, mm
  PSITi<-PSITI
  THETa<-THETA
  Kk<-KK
#output
#  PSITI() total potential, kPa
#  THETA() water content, mm water / mm soil matrix
#  SWAT    total soil water in all layers, mm
#  KK()    hydraulic conductivity, mm/d
#local
#Dim i%     # soil layer

  SWAt <- 0
  for (i in 1:NLAYER){
    PSITi[i] <- PSIM[i] + PSIG[i]
    THETa[i] <- WETNES[i] * THSAT[i]
    if(WETNES[i] > 0.0001){
      Kk[i] <- KF[i] * (WETNES[i] / WETF[i]) ^ (2 * BEXP[i] + 3)
    }else{  #extremely dry
      Kk[i] <- 0.0000000001
    }
    SWAt <- SWAt + SWATI[i]
    #if (IDAY >= 12 && i==NLAYER) {
    #  Kk[1:5]
    #  SWAt <- SWAt
    #}
    
    
  }
return(c(PSITi, THETa, Kk, SWAt))
}

