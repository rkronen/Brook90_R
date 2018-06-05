
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
#
#***************************************************************************
SNOENRGY<-function(TSNOW, TA, DAYLEN, CCFAC, MELFAC, SLFDAY, LAI, SAI, LAIMLT, SAIMLT){
#  snow surface energy balance
#input
#   TSNOW      snowpack temperature (isothermal assumed), degC
#   TA         "mean" temperature for the day, ?C
#   DAYLEN     daylength in fraction of day
#   CCFAC      cold content factor, MJ m-2 d-1 K-1
#   MELFAC     degree day melt factor for open, MJ m-2 d-1 K-1
#   SLFDAY     ratio of potential insolation on slope to on horizontal for day
#   LAI        leaf area index, m2/m2
#   SAI        stem area index, m2/m2
#   LAIMLT     parameter for snowmelt dependence on LAI, dimensionless
#   SAIMLT     parameter for snowmelt dependence on SAI, dimensionless
#output
#   SNOEN      energy flux density to snow surface, MJ m-2 d-1
#intrinsic
#   EXP
#
if (TA <= 0) {
#  snow warms or cools proportional to snow-air temperature difference
  SNOEN <- CCFAC * 2 * DAYLEN * (TA - TSNOW)
}else{
#  energy input proportional to TA, modified by cover, slope-aspect, and daylength
  SNOEN <- MELFAC * 2 * DAYLEN * TA * exp(-SAIMLT * SAI) * exp(-LAIMLT * LAI) * SLFDAY
}
 # if (IDAY >= 766) {
#    SNOEN <- SNOEN
 # }
return(SNOEN)
}

#****************************************************************************
SNOFRAC<-function (TMAX, TMIN, RSTEMP){
#separates RFAL from SFAL
#input
#   TMAX       maximum temperature for the day, ?C
#   TMIN       minimum temperature for the day, ?C
#   RSTEMP     base temperature for snow-rain transition, ?C
#output
#   SNOFRC     fraction of precipitation for the day as SFAL, unitless
#
if (TMIN >= RSTEMP) {
  SNOFRC <- 0
}else if (TMAX < RSTEMP){
  SNOFRC <- 1
}else{
  SNOFRC <- 1 - (TMAX - RSTEMP) / (TMAX - TMIN)
}
return(SNOFRC)
}

#*************************************************************************
SNOVAP<-function (TSNOW, TA, EA, UA, ZA, HEIGHT, Z0, DISP, Z0C, DISPC, Z0GS, LWIDTH, RHOTP, NN, LAI, SAI, KSNVP){
#snow evaporation and condensation
#input
#   DISP              zero-plane displacement, m
#   DISPC             zero-plane displacement for closed canopy of HEIGHT, m
#   EA                vapor pressure for the day, kPa
#   HEIGHT            canopy height, m
#   KSNVP             multiplier to fix snow evaporation problem
#   LAI               leaf area index, m2/m2
#   LWIDTH            leaf width, m
#   NN                wind/diffusivity extinction coefficient
#   RHOTP             ratio of total leaf area to projected area
#   SAI               stem area index, m2/m2
#   TA                mean  temperature for the day at reference height, ?C
#   TSNOW             snowpack temperature (isothermal assumed), ?C
#   UA                average wind speed for the day at reference height, m/s
#   Z0                roughness parameter, m
#   Z0C               roughness parameter for closed canopy of HEIGHT, m
#   Z0GS              snow surface roughness, m
#   ZA                reference height for TA, EA, UA, above ground, m
#output
#   PSNVP             potential snow evaporation, mm/d
#local
ESNOW <-0         #  vapor pressure at snow surface, kPa
RAA   <-0         #  Shuttleworth-Wallace atmosphere aerodynamic resistance,s/m
RAS   <-0         #  Shuttleworth-Wallace ground aerodynamic resistance, s/m
#external functions needed
#   ESAT, SWGRA, RMIN
#
#  ignores effect of interception on PSNVP or of PSNVP on PTRAN
if (TSNOW > -0.1) {
  ESNOW <- 0.61
}else{
#     snow surface vapor pressure saturated at lower of TA and TSNOW
  esatt<-ESAT(RMINF(TA, TSNOW), ESNOW, dummy)
   ESNOW<-unlist(esatt[1])
}
swgra<-SWGRA(UA, ZA, HEIGHT, Z0, DISP, Z0C, DISPC, Z0GS, LWIDTH, RHOTP, NN, LAI, SAI, RAA, RAC, RAS)
  RAA<-unlist(swgra[1])
  RAC<-unlist(swgra[2])
  RAS<-unlist(swgra[3])

###                                                                                    ^^^       ^^^
PSNVP <- (WTOMJ / LS) * (CPRHO / GAMMA) * (ESNOW - EA) / (RAA + RAS)
#  fix for PSNVP overestimate
PSNVP <- KSNVP * PSNVP

return(PSNVP)
}

#***************************************************************************
SNOWPACK<-function(RTHR, STHR, PSNVP, SNOEN, CC, SNOW, SNOWLq, DTP, TA, MAXLQF, GRDMLT){
#   adds net snowfall or rainfall to snowpack, subtracts groundmelt, evaporation, and melt

#input
#   RTHR    rain throughfall rate, mm/d
#   STHR    SNOW throughfall rate, mm/d
#   PSNVP   potential evaporation rate from snowpack, mm/d
#   SNOEN   energy flux density to SNOW surface, MJ m-2 d-1
#   DTP     time step for precipitation interval, may be <= 1 d
#   TA      "mean" temperature for the day, ?C
#   MAXLQF  maximum liquid water fraction of SNOW, dimensionless
#   GRDMLT  rate of groundmelt of snowpack, mm/d
#input and output
#   CC      cold content of snowpack (positive), MJ/m2
#   SNOW    water equivalent of SNOW on the ground, mm
#   SNOWLQ  liquid water content of SNOW on the ground, mm
#output
#   RSNO    rain added to snowpack, mm/d
#   SNVP    evaporation rate from snowpack, mm/d
#   SMLT    melt drainage rate from snowpack, mm/d
#local
SNOWLQ<-SNOWLq
FRAC <-0 # groundmelt and evaporation fraction of SNOW, dimensionless
EQEN <-0 # meltwater equivalent of energy input, including warm rain, mm
NMLT<-0 # -EQEN when EQEN is negative, "negative melt", mm
ALQ <-0 # MAXLQF*SNOW - SNOWLQ, available space for liquid water, mm
RIN <-0 # RTHR*DTP, rain input to SNOW, mm
#external functions needed
#   RMIN, RMAX
#
#SNOW throughfall and its cold content, SNOWLQ unchanged
SNOW <- SNOW + STHR * DTP
CC <- CC + CVICE * RMAXF(-TA, 0) * STHR * DTP

if (CC > 0 && SNOWLQ > 0) {
  if (CC > SNOWLQ * LF) {
#     refreeze all liquid
    CC <- CC - SNOWLQ * LF
    SNOWLQ <- 0
  }else{
    #     refreeze part
    SNOWLQ <- SNOWLQ - CC / LF
    CC <- 0
  }
}

#if (SNOW > 0) {#
#groundmelt and evaporation loss as fraction of SNOW
FRAC <- (GRDMLT + PSNVP) * DTP / SNOW
#FRAC can be negative if condensation exceeds groundmelt
if (FRAC < 1) {
  SMLT <- GRDMLT
  SNVP <- PSNVP
#  reduce CC, SNOWLQ, and SNOW proportionally for groundmelt and evaporation
#  increase them proportionally if condensation exceeds groundmelt
  CC <- CC * (1 - FRAC)
  SNOWLQ <- SNOWLQ * (1 - FRAC)
  SNOW <- SNOW * (1 - FRAC)
}else{
#  all SNOW disappears from groundmelt and/or evaporation
  SMLT <- GRDMLT / FRAC
  SNVP <- PSNVP / FRAC
  RSNO <- 0
  CC <- 0
  SNOWLQ <- 0
  SNOW <- 0
}
#}

#snowpack cooling or warming
if (SNOW > 0) {
#  equivalent ice melted by energy input including warm rain, mm
  EQEN <- DTP * (SNOEN + RTHR * RMAXF(TA, 0) * CVLQ) / LF
  if (EQEN <= 0) {
    #     snowpack cooling
    NMLT <- -EQEN
    if (NMLT < SNOWLQ) {
    #        only part of SNOWLQ refreezes
      CC <- 0
      #        should be 0 already because SNOWLQ is positive
      SNOWLQ <- SNOWLQ - NMLT
    }else{
      #        all SNOWLQ (if any) refreezes, remaining NMLT increases CC
      NMLT <- NMLT - SNOWLQ
      SNOWLQ <- 0
      CC <- CC + NMLT * LF
      #        do not allow TSNOW to cool below TA
      CC <- RMINF(CC, -TA * SNOW * CVICE)
    }
  }else{
    #     snowpack warming  (can#t have both CC and SNOWLQ)
    if (EQEN * LF < CC || TA < 0) {
      #        reduce but don#t eliminate CC
      if (TA < 0) {
      #           do not allow TSNOW to warm above TA when TA < 0
        CC <- RMAXF(CC - EQEN * LF, -TA * SNOW * CVICE)
      }else{
        CC <- CC - EQEN * LF
      }
      SNOWLQ <- 0
    }else{
      #        CC eliminated
      EQEN <- EQEN - CC / LF
      CC <- 0
      if (EQEN <= MAXLQF * SNOW - SNOWLQ){
        #           remaining energy increases liquid water
        SNOWLQ <- SNOWLQ + EQEN
        #           SMLT and SNOW unchanged
      }else{
        #           liquid water capacity reached, SNOW melt produced
        EQEN <- EQEN - (MAXLQF * SNOW - SNOWLQ)
        if (SNOW * (1 - MAXLQF) > EQEN) {
          #              melt is ice plus the liquid included in it
          SMLT <- SMLT + (EQEN / DTP) / (1 - MAXLQF)
          SNOW <- SNOW - EQEN / (1 - MAXLQF)
          SNOWLQ <- MAXLQF * SNOW
        }else{
          #              all SNOW melts
          SMLT <- SMLT + SNOW / DTP
          SNOW <- 0
          SNOWLQ <- 0
        }
      }
    }
  }

#  add rain to snowpack,
if (RTHR == 0 || SNOW == 0) {
  RSNO <- 0
}else{
  #     rain on SNOW
  RIN <- RTHR * DTP
  if (CC > 0) {
      #        use CC to refreeze rain
      if (CC > RIN * LF) {
        #           refreezes all rain
        CC <- CC - RIN * LF
        RSNO <- RTHR
        SNOW <- SNOW + RIN
      }else{
        #           CC refreezes part of rain
        SNOW <- SNOW + CC / LF
        RSNO <- (CC / LF) / DTP
        CC <- 0
        #           remaining rain
        RIN <- RIN - RSNO * DTP
        #           increase liquid water, SNOWLQ initially zero
        if (RIN < MAXLQF * SNOW / (1 - MAXLQF)) {
          #              remaining RIN all to SNOWLQ
          SNOWLQ <- RIN
          RSNO <- RSNO + RIN / DTP
          SNOW <- SNOW + RIN
        }else{
          SNOWLQ <- MAXLQF * SNOW / (1 - MAXLQF)
          RSNO <- RSNO + SNOWLQ / DTP
          SNOW <- SNOW + SNOWLQ
        }
      }
    }else{
        #        CC = 0.
        if (SNOWLQ >= MAXLQF * SNOW) {
          #           SNOW already holding maximum liquid
          RSNO <- 0
        }else{
          ALQ <- MAXLQF * SNOW - SNOWLQ
          if (RIN < ALQ) {
            #              all RIN to SNOW
            RSNO <- RTHR
            SNOWLQ <- SNOWLQ + RIN
            SNOW <- SNOW + RIN
          }else{
            #              maximum liquid reached
            RSNO <- (ALQ / (1 - MAXLQF)) / DTP
            SNOW <- SNOW + RSNO * DTP
            SNOWLQ <- MAXLQF * SNOW
          }
        }
      }
    }
}    

#if (IDAY >= 766) {
#  SNOW <- SNOW
#}

return (list(CC,SNOW,SNOWLQ,RSNO, SNVP, SMLT))
}
