
#   Code by R.Kronenberg as RK [06112017]  based on Brook90 by Federer et.al
#  modified RK [16112017]
#
#   TU Dresden 
#   Institut f?r Hydrologie und Meteorologie
#   Professur f?r Meteorologie
#   2017
#
#
#
#
#
#
#
#************************************************************************
BYFLFR<-function(){
#bypass flow fraction of infiltration to layer
#input
#  BYPAR%    1 to allow BYFL, or 0 to prevent BYFL
#  NLAYER%   number of soil layers to be used in model, <= ML%
#  WETNES()  wetness, fraction of saturation
#  WETF()    wetness at field capacity, dimensionless
#  QFFC      BYFL fraction at field capacity
#  QFPAR     quick flow parameter
#output
#  BYFRAC()  fraction of layer infiltration to bypass flow
#
for(i in 1:NLAYER){
  if (BYPAR == 1) {
    if (QFPAR > 0.01){
      BYFRAC[i] <- QFFC ^ (1 - (1 / QFPAR) * (WETNES[i] - WETF[i]) / (1 - WETF[i]))
      if (BYFRAC[i] > 1)  BYFRAC[i] <- 1
    }else{ # bucket for the layer
      if(WETNES[i] >= WETF[i]){
        BYFRAC[i] <- 1
      }else{
        BYFRAC[i] <- 0
      }
    }
  }else{
    BYFRAC[i] <- 0
  }
}
  
    #
return(BYFRAC)
}
    
    #************************************************************************
DSLOP<-function(i){
    #downslope flow rate from layer
    #input
    #  DSLOPE     slope for soil water flow, radians
    #             no DSFLI if DSLOPE = 0
    #  LENGTH     slope length (for DSFLI), m
    #  THICK[]      layer thicknesses, mm
    #  STONEF[]     stone volume fraction, unitless
    #  PSIM[]       matric soil water potential, kPa
    #  RHOWG      density of water times acceleration of gravity, kPa/mm
    #  KK[]         hydraulic conductivity, mm/d
    #output
    #  DSFLI[]      downslope flow rate from layer, mm/d
    #local
    LL<-0        # LENGTH in mm
    GRAD <-0      #downslope potential gradient, kPa/mm
    ARATIO  <-0   #outflow area / map area
    #intrinsic
    #  SIN, COS
    #
    LL <- 1000 * LENGTH
    GRAD <- RHOWG * sin(DSLOPE) + (2 * PSIM[i] / LL) * cos(DSLOPE)
    ARATIO <- THICK[i] * (1 - STONEF[i]) * cos(DSLOPE) / LL
    DSFLi <- KK[i] * ARATIO * GRAD / RHOWG
    #no water uptake into dry soil because no free water at outflow face
    if (DSFLi < 0)  DSFLi <- 0
return(DSFLi)
}
    
    #******************************************************************************
GWATER<-function(GWAT, GSC, GSP, DT, VRFLN){
    #calculates groundwater flow and seepage loss
    #input
    #  GWAT     groundwater storage below soil layers, mm
    #  GSC      discharge from GWAT, fraction per day, d-1
    #  GSP      fraction of discharge to seepage
    #  DT       time step for interval, d
    #  VRFLN    vertical drainage rate from lowest layer, mm/d
    #output
    #  GWFL      streamflow from groundwater discharge, mm/d
    #  SEEP      deep seepage loss from groundwater, mm/d
    #
    if (GSC < 0.00000001){
    #  no groundwater
      SEEP <- GSP * VRFLN
      GWFL <- VRFLN - SEEP
    }else{
      SEEP <- GWAT * GSC * GSP
      GWFL <- GWAT * GSC * (1 - GSP)
    #  prevent negative GWAT
      if (GWAT / DT - (GWFL + SEEP) < 0){
        SEEP <- GSP * GWAT / DT
        GWFL <- (1 - GSP) * GWAT / DT
      }
    }
  
  #if (IDAY >= 14) {
 #   GWFL <- GWFL
  #}
return(list(GWFL, SEEP))
}
    
    #***************************************************************************
INFLOW<-function(){
    #inflow and byflow for each layer, and net inflow including E and T withdrawal
    #input
    #  NLAYER%    number of soil layers being used, max of 20
    #  DTI        time step for iteration interval, d
    #  INFRAC()   fraction of infiltration to each layer
    #  BYFRAC()   fraction of layer infiltration to bypass flow
    #  SLFL       input rate to soil surface, mm/d
    #  DSFLI()    downslope flow rate from layer, mm/d
    #  TRANI()    transpiration rate from layer, mm/d
    #  SLVP       evaporation rate from soil, mm/d
    #  SWATMX()   maximum water storage for layer, mm
    #  SWATI()    water volume in layer, mm
    #  VRFLI()    vertical drainage rate from layer, mm/d
    #output
    #  VV()       modified VRFLI, mm/d
    #  BYFLI()    bypass flow rate from layer, mm/d
    #  INFLI()    infiltration rate into layer, mm/d
    #  NTFLI()    net flow rate into layer, mm/d
    #local
    #Dim i%       #index variable for layer number
    INFIL<-0    # water reaching layer, SLFL * INFRAC(i%), mm/d
    MAXIN<-0    # maximum allowed rate of input of water to layer, mm/d
    INFLi<-INFLI
    #
    for(i in seq(NLAYER,1,-1)){
    #  need to do from bottom up
      INFIL <- SLFL * INFRAC[i]
      BYFLI[i] <- BYFRAC[i] * INFIL
      INFLi[i] <- INFIL - BYFLI[i]
      if (i == NLAYER)
        {VV[i] <- VRFLI[i]
           }
      
      if (i > 1) {
       MAXIN <- (SWATMX[i] - SWATI[i]) / DTI + VV[i] + DSFLI[i] + TRANI[i]
      
        if (VRFLI[i - 1] + INFLi[i] > MAXIN) {
    #        adjust to prevent oversaturation
          if (BYFRAC[i] > 0) {
            if (VRFLI[i - 1] < MAXIN) {
    #              reduce INFLI, increase BYFLI
              BYFLI[i] <- BYFLI[i] + INFLi[i] - (MAXIN - VRFLI[i - 1])
              INFLi[i] <- MAXIN - VRFLI[i - 1]
              VV[i-1] <- VRFLI[i-1]
            }else{
                #              shift all INFLI to BYFLI and reduce VRFLI(i% - 1)
              BYFLI[i] <- BYFLI[i] + INFLi[i]
              INFLi[i] <- 0
              VV[i-1] <- MAXIN
            }
          }else{
#           no bypass flow allowed, reduce VRFLI(i%-1), INFLI(i%) unchanged
            VV[i-1] <- MAXIN - INFLi[i]
          }
        }else{
            #        BYFLI and INFLI unchanged
          VV[i-1] <- VRFLI[i-1]
        }
       NTFLI[i] <- VV[i-1] + INFLi[i] - VV[i] - DSFLI[i] - TRANI[i]

      }else{
#     i% = 1
        MAXIN <- (SWATMX[1] - SWATI[1]) / DTI + VV[1] + DSFLI[1] + TRANI[1] + SLVP
        if (INFLi[1] > MAXIN) {
#        increase BYFLI(1) to prevent oversaturation
          BYFLI[1] <- BYFLI[1] + INFLi[1] - MAXIN
          INFLi[1] <- MAXIN
#        may be negative  
        }
        NTFLI[1] <- INFLi[1] - VV[1] - DSFLI[1] - TRANI[1] - SLVP
      }
    }
    
   INFLI<- INFLi
return(list(VV, INFLI, BYFLI, NTFLI))
}

#******************************************************************************
INFPAR<-function(INFEXP, IDEPTH, NLAYER, THICK){
#modified for Version 4, June 2, 1999
#input
# INFEXP     infiltration exponent, 0 all to top, 1 uniform with depth
#                    >1.0=more at bottom than at top
# IDEPTH     depth over which infiltration is distributed
# NLAYER%    number of soil layers being used
# THICK      layer thicknesses, mm
#output
# ILAYER%    number of layers over which infiltration is distributed
# INFRAC()   fraction of infiltration to each layer
#local
THICKT<-0   #total thickness of ILAYERs, mm
THICKA<-rep(0,ML)   #accumulated thickness downward, mm
#
if (INFEXP <= 0 || IDEPTH == 0) {
  ILAYER <- 1  # probably not used
  INFRAC[1] <- 1
  for (i in seq(2,NLAYER,1)){
    INFRAC[i] <- 0
  }
}else{
#must have at least one layer
  THICKT <- THICK[1]
  ILAYER <- 1
  for (i in 2:NLAYER){
    if (THICKT + 0.5 * THICK[i] <= IDEPTH){
      #include layer
      ILAYER <- ILAYER + 1
      THICKT <- THICKT + THICK[i]
    }else{
      i<-NLAYER
    }
  }
  THICKA[1] <- 0
  for(i in 1:NLAYER){  #  oder doch ab 1 ???
    if (i <= ILAYER) {
      
      
      if(i==1){
        THICKA[i] <- THICK[i]
        INFRAC[i] <- (THICKA[i] / THICKT) ^ INFEXP - (0 / THICKT) ^ INFEXP
      }else{
        THICKA[i] <- THICKA[i - 1] + THICK[i]
        INFRAC[i] <- (THICKA[i] / THICKT) ^ INFEXP - (THICKA[i - 1] / THICKT) ^ INFEXP
      }
      
    }else{
      INFRAC[i] <- 0
    }
  }  
    
}
return(list(ILAYER, INFRAC))
}

#*************************************************************************
ITER<-function(NLAYER, DTI, DPSIDW, NTFLI, SWATMX, PSITI, DSWMAX, DPSIMX){
#input
#  NLAYER%    number of soil layers to be used in model
#  DTI        time step for iteration interval, d
#  DPSIDW()   rate of change of total potential with water content, kPa/mm
#  NTFLI()    net flow rate into layer, mm/d
#  SWATMX()   maximum water storage for layer, mm
#  PSITI()    total potential, kPa
#  DSWMAX     maximum change allowed in SWATI, percent of SWATMX(i)
#  DPSIMX     maximum potential difference considered "equal", kPa
#output
#  DTINEW     second estimate of DTI
#local
A<- rep(0,50)
temp<-rep(0,50)
TT  <-0      #new potential difference between layers
PP<-0        #original potential difference between layers
#intrinsic
#  ABS, SGN
#external functions needed
#  RMIN, RMAX
#
#first approximation to new total potential
for ( i in 1:NLAYER){
    A[i] <- NTFLI[i] * DPSIDW[i] / SWATMX[i]
    temp[i] <- PSITI[i] + A[i] * DTI
}
#test to see if DTI should be reduced
DTINEW <- DTI
for( i in 1:NLAYER){
  #  prevent too large a change in water content
  DTINEW <- RMINF(DTINEW, 0.01 * DSWMAX * SWATMX[i] / RMAXF(0.000001, abs(NTFLI[i])))
#  prevent oscillation of potential gradient
  if (i < NLAYER) {
#     total potential difference at beginning of iteration
    PP <- PSITI[i] - PSITI[i + 1]
#     first approx to total potential difference at end of iteration
    TT <- temp[i] - temp[i + 1]
    if ((abs(TT) > DPSIMX) && (abs(PP) > DPSIMX) && (sign(TT) != sign(PP))){
      DTINEW <- RMINF(DTINEW, -PP / (A[i] - A[i + 1]))
    }
  }
}

return(DTINEW)
}

#*************************************************************************
RTDEN<-function(ROOTDEN, NLAYER, THICK){
#for Version 4, June 2, 1999
#25 and 50 here refer to number of values in ROOTDEN parameter array
#input
#  ROOTDEN()  array (1-50) of root layer thickness and root density per unit stonefree volume
#  NLAYER%  number of soil layers being used
#  THICK    soil layer thicknesses, mm
#output
#  RELDEN   relative root density per unit stonefree volume for soil layer
#local
#Dim i%    # soil layer
#Dim J%    # root layer
#Dim DONE As Integer
RTHICK<-rep(0,ML) #root layer thickness
RDEN<-rep(0,ML) #relative root density in layer
RREMAIN<-0 # remaining thickness of root layer
TREMAIN<-0 # remaining thickness of soil layer
#
for( J in 1:ML){
  RTHICK[J] <- ROOTDEN[2 * J - 1]
  RDEN[J] <- ROOTDEN[2 * J]
}
DONE <- FALSE
j <- 1
RREMAIN <- RTHICK[j]
for( i in 1:NLAYER){ # new soil layer
     #accumulate RELDEN as total root length in soil layer
  RELDEN[i] <- 0
  if (!DONE){
    TREMAIN <- THICK[i]
    while(RREMAIN < TREMAIN && j < ML-1){
       #remaining root layer thickness < remaining soil layer thickness
      RELDEN[i] <- RELDEN[i] + RDEN[j] * RREMAIN
      TREMAIN <- TREMAIN - RREMAIN
      j <- j + 1
      if( j == ML){
        DONE <- TRUE
      }
      RREMAIN <- RTHICK[j]
    }
      #remaining root layer thickness >= remaining soil layer thickness
    if(!DONE){
       RELDEN[i] <- RELDEN[i] + RDEN[j] * TREMAIN
       RREMAIN <- RREMAIN - TREMAIN
    }
  }
#convert back to unit volume basis
RELDEN[i] <- RELDEN[i] / THICK[i]
}
return(RELDEN)
}

#*************************************************************************
SRFLFR<-function(){
#input
#  QLAYER%  number of soil layers for SRFL
#  SWATI()  water volume by layer, mm
#  SWATQX   maximum water storage for layers 1 through QLAYER%
#  QFPAR    quickflow parameter, 0 for bucket
#  SWATQF   water storage at field capacity for layers 1 through QLAYER%, mm
#  QFFC     SRFL fraction at field capacity
#output
#  SAFRAC   source area fraction
#local
SUM<-0      #soil water in layers 1 through QLAYER%
safra<-0
for( i in  1:QLAYER){
  SUM <- SUM + SWATI[i]
}
if( QFPAR > 0.01){
  safra <- QFFC ^ (1 - (1 / QFPAR) * (SUM - SWATQF) / (SWATQX - SWATQF))
  if (safra > 1) {safra <- 1}
}else{ # bucket over QLAYERs
  if (SUM >= SWATQF){
      safra <- 1
  }else{
      safra <- 0
  }
}

#
return(safra)
}

#***********************************************************************
SRFPAR<-function(QDEPTH, NLAYER, THETAF, THICK, STONEF, SWATMX){
#Modified for Version 4, June 2, 1999
#source area parameters
#input
#  QDEPTH    soil depth for SRFL calculation, 0 to prevent SRFL
#  NLAYER%   number of soil layers to be used
#  THETAF()  volumetric water content of layer at field capacity
#  THICK()   layer thickness, mm
#  STONEF()  stone volume fraction of layer
#  SWATMX()  maximum water storage for layer, mm
#output
#  QLAYER%   number of soil layers for SRFL
#  SWATQX    maximum water storage for layers 1 through QLAYER%, mm
#  SWATQF    water storage at field capacity for layers 1 through QLAYER%, mm
#local
THICKT<-0
#
if( QDEPTH == 0 ){
#no SRFL with QLAYER% = 0, SWATQX and SWATQF not used
  QLAYER <- 0
  SWATQX <- 0
  SWATQF <- 0
  return(list( QLAYER, SWATQX, SWATQF))
}
QLAYER <- 1
THICKT <- THICK[1]
for( i in  2:NLAYER){
  if( THICKT + 0.5 * THICK[i] <= QDEPTH){
    THICKT <- THICKT + THICK[i]
    QLAYER <- QLAYER+ 1
  }else{
    i<-NLAYER
  }
}
  SWATQX <- 0
  SWATQF <- 0
  for( i in  1:QLAYER){
    SWATQX <- SWATQX + SWATMX[i]
    SWATQF <- SWATQF + THETAF[i] * THICK[i] * (1 - STONEF[i])
  }
#
return(list( QLAYER, SWATQX, SWATQF))
}

#**************************************************************************
VERT<-function(i){
#modified March 17, 2001 to change KKMEAN
#vertical flow rate
  #VERT(KK[i], KK[i+1], KSAT[i], KSAT[i+1], THICK[i], THICK[i+1], PSITI[i], PSITI[i+1], STONEF[i], STONEF[i+1], RHOWG, VRFLI[i],i)
###     
#  flow rate = gradient * cond    / rhog
#   mm/day   = kPa/mm   * mm/day  / kPa/mm
#input
 # KK         hydraulic conductivity for upper layer, mm/d
 # KK1        hydraulic conductivity for lower layer, mm/d
 # KSAT       saturated hydraulic conductivity of upper layer, mm/d
 # KSAT1      saturated hydraulic conductivity of lower layer, mm/d
 # THICK      thickness of upper layer, mm
 # THICK1     thickness of lower layer, mm
 # PSIT       total potential of upper layer, kPa
 # PSIT1      total potential of lower layer, kPa
 # STONEF      stone volume fraction of upper layer, unitless
 # STONE1     stone volume fraction of lower layer, unitless
 # RHOWG      density of water times gravity acceleration, kPa/mm
 # i          Layer
#output
#  VRFLI      vertical drainage rate from layer i, mm/d
#local
GRAD <-0     # potential gradient, positive downward, kPa/mm
KKMEAN<-0     #geometric mean conductivity
#
KKMEAN <- exp((log(KK[i]) + log(KK[i+1])) / 2)
#for Version 4.2 and 4.3 was
#KKMEAN = Exp((THICK * Log(KK) + THICK1 * Log(KK1)) / (THICK + THICK1))
#for Version 4.1 and 3.25a and earlier was
#  KKMEAN = Exp((THICK1 * Log(KK) + THICK * Log(KK1)) / (THICK + THICK1))
#limit KKMEAN to lesser saturated conductivity
if (KKMEAN > KSAT[i])  KKMEAN <- KSAT[i]
if (KKMEAN > KSAT[i+1])  KKMEAN <- KSAT[i+1]
#through Version 4.3a was GRAD = (PSIT - PSIT1) / ((THICK + THICK1) / 2!)
GRAD <- (PSITI[i] - PSITI[i+1]) / RMINF(THICK[i], THICK[i+1])
VRFLi <- (GRAD * KKMEAN / RHOWG) * (1 - (STONEF[i] + STONEF[i+1]) / 2)

return(VRFLi)
}

