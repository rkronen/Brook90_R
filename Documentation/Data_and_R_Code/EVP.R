
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

#******************************************************************************
INTER<-function(RFAL, PINT, LAI, SAI, FRINTL, FRINTS, CINTRL, CINTRS, DTP, INTR, RINT, IRVP){
#rain interception, used when NPINT% > 1
#same routine is used for snow interception, with different calling variables
#input
#   RFAL      rainfall rate, mm/d
#   PINT      potential interception rate, mm/d
#   LAI       projected leaf area index, m2/m2
#   SAI       projected stem area index, m2/m2
#   FRINTL    intercepted fraction of RFAL per unit LAI
#   FRINTS    intercepted fraction of RFAL per unit SAI
#   CINTRL    maximum interception storage of rain per unit LAI, mm
#   CINTRS    maximum interception storage of rain per unit SAI, mm
#   DTP       precipitation interval time step, d
#   INTR      intercepted rain, mm
#output
#   RINT      rain catch rate, mm/d
#   IRVP      evaporation rate of intercepted rain, mm/d
#local
  INTRMX<-0   #maximum canopy storage for rain, mm
  CATCH<-0    #maximum RINT, mm/d
  NEWINT<-0   #first approximation to new canopy storage (INTR)
  #
  CATCH <- (FRINTL * LAI + FRINTS * SAI) * RFAL
  INTRMX <- CINTRL * LAI + CINTRS * SAI
  NEWINT <- INTR + (CATCH - PINT) * DTP

  if(NEWINT > 0){
    #  canopy is wet throughout DTP
    IRVP <- PINT
    if(NEWINT > INTRMX){ 
      #     canopy capacity is reached
      RINT <- PINT + (INTRMX - INTR) / DTP
      #     RINT can be negative if INTR exists and LAI or SAI is decreasing over time
    }else{
      #     canopy capacity is not reached
      RINT <- CATCH
    }
  }else{
    #  canopy dries during interval or stays dry
    RINT <- CATCH
    IRVP <- (INTR / DTP) + CATCH
    #  IRVP is < PINT

  }
  return(list(RINT,IRVP));
}

#**************************************************************************
INTER24<-function(RFAL, PINT, LAI, SAI, FRINTL, FRINTS, CINTRL, CINTRS, DURATN, INTR, RINT, IRVP, MONTHN){
#rain interception with duration in hours, used when NPINT% = 1
#same routine is used for snow interception, with different calling variables
#input
#   RFAL      24-hour average rainfall rate, mm/d
#   PINT      potential interception rate, mm/d
#   LAI       projected leaf area index, m2/m2
#   SAI       projected stem area index, m2/m2
#   FRINTL    intercepted fraction of RFAL per unit LAI
#   FRINTS    intercepted fraction of RFAL per unit SAI
#   CINTRL    maximum interception storage of rain per unit LAI, mm
#   CINTRS    maximum interception storage of rain per unit SAI, mm
#   DURATN    average storm duration, hr
#   INTR      intercepted rain storage, mm,
#   MONTHN    Month of the year
#output
#   RINT      rain catch rate, mm/d
#   IRVP      evaporation rate of intercepted rain, mm/d
#local
  INTRMX<-0    #maximum canopy storage for rain, mm
  INTRNU<-0    #canopy storage at end of hour, mm
  NEWINT<-0    #first approximation to INTRNU, mm
  RINTHR<-0    #rain catch rate for hour, mm/hr
  CATCH<-0     #maximum RINTHR, mm/hr
  IRVPHR<-0    #evaporation rate for hour, mm/hr
  SMINT<-0     #daily accumulated actual catch, mm
  SMVP<-0      #daily accumulated actual evaporation, mm
  IHD<-0      #half DURATN in truncated integer hours
  #hh<<-0        #hour, 0 to 23
  DTH<-0       #time step, = 1 hr
  #intrinsic
  #   CSNG, INT
  #
  IHD <- as.integer((DURATN[MONTHN] + 0.1) / 2)
  INTRMX <- CINTRL * LAI + CINTRS * SAI
  INTRNU <- INTR
  SMINT <- 0
  SMVP <- 0
  DTH <- 1
  for(i in seq(0,23,1)){
    if((i < (12 - IHD)) || (i >= (12 + IHD))){
      #     before or after rain
      CATCH <- 0
    }else{
      #     during rain, mm/hr is rate in mm/d divided by hr of rain/d
      CATCH <- (FRINTL * LAI + FRINTS * SAI) * RFAL / (2 * IHD)
    }
    NEWINT <- INTRNU + (CATCH - PINT / 24) * DTH
    if (NEWINT > 0.0001) {
      #     canopy is wet throughout hour, evap rate is PINT
      IRVPHR <- PINT / 24
      if (NEWINT > INTRMX) {
        #        canopy capacity is reached
        RINTHR <- IRVPHR + (INTRMX - INTRNU) / DTH
        #        INTRMX - INTRNU can be negative if LAI or SAI is decreasing over time
      }else{
        #        canopy capacity is not reached
        RINTHR <- CATCH
      }
    }else{
        #     canopy dries during hour or stays dry
        RINTHR <- CATCH
        IRVPHR <- INTRNU / DTH + CATCH
        #     IRVPHR for hour is < PI/24
    }
  INTRNU <- INTRNU + (RINTHR - IRVPHR) * DTH
  SMVP <- SMVP + IRVPHR * DTH
  SMINT <- SMINT + RINTHR * DTH
  }
  IRVP <- SMVP
  #           / 1 d
  RINT <- SMINT
#            / 1 d
  return(list(RINT,IRVP))
}

#******************************************************************************
PLNTRES<-function(NLAYER, THICK, STONEF, RTLEN, RELDEN, RTRAD, RPLANT, FXYLEM, RXYLEM, RROOTI, ALPHA){
#allocates total plant resistance to xylem and root layers
#input
#DIM NLAYER AS INTEGER # number of soil layers (max 50)
#   THICK() layer thicknesses, mm
#   STONEF()stone volume fraction, unitless
#   RTLEN   root length per unit land area, m/m2, MXRTLN * RELHT * DENSEF
#   RELDEN()relative values of root length per unit volume
#   RTRAD   average root radius, mm
#   RPLANT  plant resistance to water flow, MPa d/mm, 1/(KPLANT*RELHT*DENSEF)
#   FXYLEM  fraction of plant resistance in xylem
#output
#   RXYLEM  xylem resistance, MPa d/mm, 1E20 if no roots
#   RROOTI()root resistance for layer, MPa d/mm, 1E20 if no roots
#   ALPHA() modified Cowan alpha, MPa
#local
#Dim I As Integer # layer counter
  Dic<-c(seq(1,50,1))  #stonefree layer thickness
  SUM<-0     #total relative length, mm
  RTFRAC<-0  #fraction of total root length in layer
  RTDENI<-0  #root density for layer, mm/mm3
  DELT<-0    #root cross-sectional area * LI, dimensionless
  RXYLEm<-0
  RROOTi<-rep(0,ML)
  ALPHa<-rep(0,ML)
  #constants
  #   RHOWG, PI
  #intrinsic function
  #   LOG
  #
  #xylem resistance
  RXYLEm <- FXYLEM * RPLANT
  #
  #SUM = 0
  for( i in seq( 1,NLAYER, 1)){
    Dic[i] <- THICK[i] * (1 - STONEF[i])
    SUM <- SUM + RELDEN[i] * Dic[i]
  }
  for( i in seq( 1,NLAYER,1)){
    if ((RELDEN[i] < 0.00001) || (RTLEN < 0.1)){
      #     no roots in layer
      RROOTi[i] <- 1E+20
      ALPHa[i] <- 1E+20
    }else{
      RTFRAC <- RELDEN[i] * Dic[i] / SUM
      #     root resistance for layer
      RROOTi[i] <- (RPLANT - RXYLEm) / RTFRAC
      #     rhizosphere resistance for layer
      RTDENI <- RTFRAC * 0.001 * RTLEN / Dic[i]
      #                       .001 is (mm/mm2)/(m/m2) conversion
      DELT <- PI * RTRAD ^ 2 * RTDENI
      ALPHa[i] <- (1 / (8 * PI * RTDENI)) * (DELT - 3 - 2 * (log(DELT)) / (1 - DELT))
      ALPHa[i] <- ALPHa[i] * 0.001 * RHOWG / Dic[i]
      #                           .001 is MPa/kPa conversion
    }
  }
  return(c(RXYLEm,RROOTi,ALPHa))
           
}

#****************************************************************************
TBYLAYER<-function(J, PTR, DISPC, ALPHA, KK, RROOTI, RXYLEM, PSITI, NLAYER, PSICR, NOOUTF){
#  actual transpiration rate by layers
#  watch MPa - kPa conversions carefully
#input
#   J%             1 for daytime, 2 for nighttime
#   PTR            average potential transpiration rate over time period, mm/d
#   DISPC          zero-plane displacement for closed canopy, m
#   ALPHA()        modified Cowan alpha, MPa
#   KK()           hydraulic conductivity, mm/d
#   RROOTI()       root resistance for layer, MPa d/mm
#   RXYLEM         xylem resistance, MPa d/mm
#   PSITI()        total soil water potential, kPa
#   NLAYER%        number of soil layers (max 20)
#   PSICR          critical potential for plant, MPa
#   NOOUTF%        1 if no outflow allowed from roots, otherwise 0
#output
#   ATR            actual transpiration rate over time period, mm/d
#   ATRANi()       actual transpiration rate from layer over time period, mm/d
#local
#Dim i%             #layer counter
  RI<-rep(0,50)    #root plus rhizosphere resistance, MPa d/mm
  RT<-0             #combined root resistance from unflagged layers, MPa d/mm
  SUM<-0            #sum of layer conductances, (mm/d)/MPa
  TRMIN<-0          #largest negative transpiration loss, mm/d
  PSIT<-0           #weighted average total soil water potential for unflagged layers, kPa
  R<-0              #(2/pi)(SUPPLY/PTR)
  SUPPLY<-0         #soil water supply rate, mm/d
  IDEL<-0          #subscript of flagged layer
  FLAG<-rep(0,50) #1 if layer has no transpiration uptake, otherwise 0
  NEGFLAG<-0       #1 if second iteration is needed
  ATr<-0
  ATRANi<-rep(0,ML)
  #intrinsic function
  #   SIN
  #external function needed
  #   ACOS, RMIN
  #constants
  #   RHOWG          density of water times gravity acceleration, MPa/m
  #   PI
  #
  #flag layers with no roots, indicated by RROOTI = 1E20
  #if outflow from roots is prevented, flag layers with PSITI <= PSICR
  for (i in seq( 1,NLAYER,1)){
    if (RROOTI[i] > 1E+15){
      FLAG[i] <- 1
    } else if((NOOUTF == 1) && (PSITI[i] / 1000 <= PSICR)) {
      FLAG[i] <- 1
    } else {
      FLAG[i]<- 0#  thi slayer has roots
    }
  }
  dfw<-0
  #
  #top of loop for recalculation of transpiration if more layers get flagged
 repeat{
    NEGFLAG <- 0
    SUM <- 0
    for(i in 1:NLAYER){
      if (FLAG[i]== 0){
        RI[i] <- RROOTI[i] + ALPHA[i] / KK[i]
        SUM <- SUM + 1 / RI[i]
      }else{
        #        flagged
        ATRANi[i] <- 0
      }
    }
    if (SUM < 1E-20){
        #     all layers flagged, no transpiration
        ATr <- 0
        PSIT <- -10000000000#
        return(list(ATr,ATRANi))
    }else{
        RT <- 1 / SUM
    }
    #  weighted mean soil water potential
    PSIT <- 0
    for (i in 1:NLAYER){
        if (FLAG[i] == 0){
          PSIT <- PSIT + RT * PSITI[i] / RI[i]
        }
    }
    #  soil water supply rate, assumed constant over day
    SUPPLY <- (PSIT / 1000 - PSICR - RHOWG * DISPC) / (RT + RXYLEM)
    #  transpiration rate limited by either PTR or SUPPLY
    if (J == 1){
      #     daytime, PTR is average of a half sine over daytime
      R <- (2 / PI) * (SUPPLY / PTR)
      if (R <= 0){ 
        ATr <- 0
      }else if (R < 1){
        ATr <- PTR * (1 + R * ACOSF(R) - sin(ACOSF(R)))
      }else{
        ATr <- PTR
      }
    }else{
      #     nighttime, PTR assumed constant over nighttime
      if ((SUPPLY <= 0) || (PTR <= 0)){
        ATr <- 0
      }else {
        ATr <- RMINF(SUPPLY, PTR)
      }
    }
    #  distribute total transpiration rate to layers
    for (i in 1:NLAYER){
      if (FLAG[i] == 1){
        ATRANi[i] <- 0
      }else{
        ATRANi[i] <- ((PSITI[i]- PSIT) / 1000 + RT * ATr) / RI[i]
        #        check for any negative transpiration losses
        if (ATRANi[i] < -0.000001){
          NEGFLAG <- 1
        }
      }  
    }
    dfw<-dfw+1
    if (NOOUTF == 1 && NEGFLAG == 1){ 
           #     find layer with most negative transpiration and omit it
           IDEL <- 0
           TRMIN <- 0
           for(i in 1:NLAYER){
            if (ATRANi[i] < TRMIN) {
              TRMIN <- ATRANi[i]
              IDEL <- i
            }
           }
           FLAG[IDEL] <- 1
           #     repeat main loop with flagged layers excluded
    }else{
          #     done
       return(list(ATr,ATRANi))
    }
   # 
  }
}
           
           