
#   Code by R.Kronenberg as RK [06112017]  based on Brook90 by Federer et.al
#  modified RK [28022018]
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
##**************************************************************************
CANOPY<-function(DOY, MAXHT, RELHT, MAXLAI, RELLAI, SNOW, SNODEN, MXRTLN, MXKPL, CS, DENSEF){
#canopy parameters
#input
#   DOY%    day of year (first day of DFILE and run)"
#   MAXHT   maximum height for the year, m, minimum of 0.01 m
#   RELHT() ten pairs of DOY% and relative canopy height
#   MAXLAI  maximum projected leaf area index for the year, m2/m2
#   RELLAI()ten pairs of DOY% and relative LAI
#   SNOW    water equivalent of snow on the ground, mm
#   SNODEN  snow density, mm/mm
#   MXRTLN  maximum root length per unit land area, m/m2
#   MXKPL   maximum plant conductivity, (mm/d)/MPa
#   CS      ratio of projected SAI to canopy height, m-1
#   DENSEF  density factor
#output
#   HEIGHT  canopy height above any snow, m, minimum of 0.01 m
#   LAI     leaf area index, m2/m2, minimum of 0.00001
#   SAI     stem area index, m2/m2
#   RTLEN   root length per unit land area, m/m2
#   RPLANT  plant resistivity to water flow, MPa d/mm
#local
  SNODEP<-0  #snow depth
  HNOSNO<-0  #height of canopy without snow
  HSNO<-0    #height of canopy above snow
  RATIO<-0   #fraction of canopy above snow
  RELHIT<-0  #RELHT for DOY%
  KPL<-0     #plant conductivity, mm d-1 MPa-1
  #intrinsic
  #   CSNG
  #external functions needed
  #   INTERP, RMAX
  #
  RELHIT <- INTERP(10, RELHT, DOY)
  SNODEP <- 0.001 * SNOW / SNODEN
  HNOSNO <- RMAXF(0.01, RELHIT * MAXHT)
  HSNO <- RMAXF(0, HNOSNO - SNODEP)
  RATIO <- HSNO / HNOSNO
  HEIGHT <- RMAXF(0.01, HSNO)
  #
  LAI <- RATIO * DENSEF * INTERP(10, RELLAI, DOY) * MAXLAI
  SAI <- DENSEF * CS * HEIGHT
  if(LAI < 0.00001)  LAI <- 0.00001
  #
  RTLEN <- DENSEF * RELHIT * MXRTLN
  KPL <- DENSEF * RELHIT * MXKPL
  if (KPL < 0.00000001)  KPL <- 0.00000001
  RPLANT <- 1 / KPL
  #
return(c(HEIGHT, LAI, SAI, RTLEN, RPLANT))
}

#***************************************************************************
ESAT<-function(TA, ES, DELTA){
#calculates saturated vp and DELTA from temp
#from Murray J Applied Meteorol 6:203
#input
#   TA      air temperature, degC
#output
#   ES      saturated vapor pressure at TA, kPa
#   DELTA   dES/dTA at TA, kPa/K
#intrinsic
#   EXP
  Es <- 0.61078 * exp(17.26939 * TA / (TA + 237.3))
  DELTa <- 4098 * Es / (TA + 237.3) ^ 2
  if (TA < 0) {
    Es <- 0.61078 * exp(21.87456 * TA / (TA + 265.5))
    DELTa <- 5808 * Es / (TA + 265.5) ^ 2
  }
return(c(Es, DELTa))
}

#***************************************************************************
FRSS<-function(RSSA, RSSB, PSIF, PSIM){
#soil surface resistance to evaporation
#input
#  RSSA      soil evaporation resistance at field capacity, s/m
#  RSSB      exponent in relation of soil evap res to water potential
#  PSIF      water potential at field capacity, kPa
#  PSIM      water potential of evaporating layer. kPa
#output
#  FRSS      Shuttleworth-Wallace soil surface resistance, s/m
#
if (RSSA < 0.0001) {
  FRSs <- 10000000000#
# forces SLVP=0
}else{
  FRSs <- RSSA * (PSIM / PSIF) ^ RSSB
}
return(FRSs)
}

#****************************************************************************
INTERP<-function(NPAIRS, FUNCT, XVALUE){
#interpolates between points in data functions
#input
#  NPAIRS%         number of pairs of values to be used
#  FUNCT()         array of pairs of values: x1, y1, x2, y2, ...
#  XVALUE          x value
#output
#  INTERP          y value
#local
#Dim I%, J%          #DO indexes
 XX<-c(seq(1,10,1))   #series of x values of FUNCT
 YY<-c(seq(1,10,1))   #series of y values of FUNCT
#
#put FUNCT into XX and YY
  i <- 0
  for (J in seq(1,(2 * NPAIRS - 1),2)){
    i <- i + 1
    XX[i] <- FUNCT[J]
    YY[i] <- FUNCT[J + 1]
  }
#interpolate using XX and YY
  for (J in 1:NPAIRS){
    if (XVALUE == XX[J]){
      INTERp <- YY[J]
      return(INTERp)
    }else if (XVALUE < XX[J]){
      INTERp <- YY[J - 1] + (XVALUE - XX[J - 1]) * (YY[J] - YY[J - 1]) / (XX[J] - XX[J - 1])
      return(INTERp)
    }
  }
return(INTERp)
}

#***************************************************************************
PM<-function(AA, VPD, DELTA, RA, RC){
#Penman-Monteith transpiration rate equation
#input
#   AA      net energy input, Rn - S, W/m2
#   VPD      vapor pressure deficit, kPa
#   DELTA   dEsat/dTair, kPa/K
#   RA      boundary layer resistance, s/m
#   RC      canopy resistance, s/m
#output
#   PM      Penman-Monteith latent heat flux density, W/m2
#
Pm <- (RA * DELTA * AA + CPRHO * VPD) / ((DELTA + GAMMA) * RA + GAMMA * RC)
return(Pm)
}

#*************************************************************************
ROUGH<-function(HEIGHT, ZMINH, LAI, SAI, CZS, CZR, HS, HR, LPC, CS, Z0GS){
#closed canopy parameters
#input
#   HEIGHT  canopy height, m, minimum of 0.01 m
#   ZMINH   ZA minus HEIGHT, reference height above canopy top, m
#   LAI     leaf area index, m2/m2, minimum of 0.00001
#   SAI     stem area index, m2/m2
#   CZS     ratio of roughness to height for smooth closed canopies
#   CZR     ratio of roughness to height for rough closed canopies
#   HS      height below which CZS applies, m
#   HR      height above which CZR applies, m
#   LPC     minimum LAI defining a closed canopy
#   CS      ratio of projected SAI to canopy height, m-1
#input and output
#   Z0GS     roughness parameter of soil surface, m
#output
#   Z0C     roughness length for closed canopy, m
#   DISPC   zero-plane displacement for closed canopy, m
#   Z0      roughness parameter, m
#   DISP    zero-plane displacement, m
#   ZA      reference height for TA, EA, UA, above ground, m
#local
RATIO<-0   #(LAI + SAI) / (LAI + SAI for closed canopy)
XX<-0
#intrinsic
#   LOG, EXP
#
if (HEIGHT >= HR) {
  Z0C <- CZR * HEIGHT
}else if (HEIGHT <= HS){
  Z0C <- CZS * HEIGHT
}else{
  Z0C <- CZS * HS + (CZR * HR - CZS * HS) * (HEIGHT - HS) / (HR - HS)
}
  DISPC <- HEIGHT - Z0C / 0.3
  if (Z0GS > Z0C)  Z0GS <- Z0C
  RATIO <- (LAI + SAI) / (LPC + CS * HEIGHT)
  if (RATIO >= 1) {
    #  closed canopy
    Z0 <- Z0C
    DISP <- DISPC
  }else{
    #  sparse canopy modified from Shuttleworth and Gurney (1990)
    XX <- RATIO * (-1 + exp(0.909 - 3.03 * Z0C / HEIGHT)) ^ 4
    DISP <- 1.1 * HEIGHT * log(1 + XX ^ 0.25)
    Z0 <- RMINF(0.3 * (HEIGHT - DISP), Z0GS + 0.3 * HEIGHT * XX ^ 0.5)
  }
ZA <- HEIGHT + ZMINH
#
return(list(Z0GS, Z0C, DISPC, Z0, DISP, ZA))
}

#****************************************************************************
SRSC<-function(RAD, TA, VPD, LAI, SAI, GLMIN, GLMAX, R5, CVPD, RM, CR, TL, T1, T2, TH){
#canopy surface resistance, RSC, after Shuttleworth and Gurney (1990) and
#   Stewart (1988)
#input
#   RAD     solar radiation on canopy, W/m2
#   TA      mean  temperature for the day at reference height, degC
#   VPD     vapor pressure deficit, kPa
#   LAI     projected leaf area index
#   SAI     projected stem area index
#   GLMIN   minimum leaf conductance, closed stomates, all sides, s/m
#   GLMAX   maximum leaf conductance, open stomates, all sides, s/m
#   R5      solar radiation at which conductance is halved, W/m2
#   CVPD    vpd at which leaf conductance is halved, kPa
#   RM      maximum solar radiation, at which FR = 1, W/m2
#   CR      light extinction coefficient for LAI, projected area
#   TL      temperature below which stomates are closed, degC
#   T1      lowest temp. at which stomates not temp. limited, degC
#   T2      highest temp. at which stomates not temp. limited,degC
#   TH      temperature above which stomates are closed, degC
#output
#   RSC     canopy surface resistance, s/m
#local
FS<-0     # correction for stem area
R0 <-0    # a light response parameter
FRINT<-0  # integral of fR dL over Lp
FD <-0    # dependence of leaf conductance on vpd, 0 to 1
FT <-0    # dependence of leaf conductance on temperature, 0 to 1
GSC <-0   # canopy conductance, m/s
#intrinsic
#   LOG, EXP
#solar radiation limitation integrated down through canopy
#Stewart (1988) and Saugier and Katerji (1991)
FS <- (2 * LAI + SAI) / (2 * LAI)
if (RAD <= 0.0000000001){
    FRINT <- 0
}else{
  R0 <- RM * R5 / (RM - 2 * R5)
  FRINT <- ((RM + R0) / (RM * CR * FS)) * log((R0 + CR * RAD) / (R0 + CR * RAD * exp(-CR * FS * LAI)))
}
#vapor deficit limitation
#Lohammar et al. (1980) and Stannard (1993)
FD <- 1 / (1 + VPD / CVPD)
#temperature limitation
if (TA <= TL) {
  FT <- 0
}else if (TA > TL && TA < T1) {
  FT <- 1 - ((T1 - TA) / (T1 - TL)) ^ 2
}else if (TA >= T1 && TA <= T2) {
  FT <- 1
}else if (TA > T2 && TA < TH) {
  FT <- 1 - ((TA - T2) / (TH - T2)) ^ 2
}else{
  FT <- 0
}
GSC <- FD * FT * FRINT * (GLMAX - GLMIN) + LAI * GLMIN
RSC <- 1 / GSC
#
return(RSC)
}

#**************************************************************************
SWGE<-function(AA, ASUBS, VPD, RAA, RAS, RSS, DELTA, ARATE, ERATE){
#Shuttleworth and Wallace (1985) ground evaporation when transpiration known
#input
#   AA      net radiation at canopy top minus ground flux, W/m2
#   ASUBS   net radiation minus ground flux at ground, W/m2
#   VPD     vapor pressure deficit, kPa
#   RAA     boundary layer resistance, s/m
#   RAS     ground-air resitance, s/m
#   RSS     ground evaporation resistance, s/m
#   DELTA   dEsat/dTair, kPa/K
#   ARATE   actual transpiration rate, mm/d
#output
#   ERATE   ground evaporation rate, mm/d
#local
RS<-0
RA<-0 #as in Shuttleworth and Wallace (1985)
LE<-0   #total latent heat flux density, W/m2
LEC<-0    #actual transpiration latent heat flux density, W/m2
#
LEC <- ARATE / (ETOM * WTOMJ)
RS <- (DELTA + GAMMA) * RAS + GAMMA * RSS
RA <- (DELTA + GAMMA) * RAA
LE <- (RS / (RS + RA)) * LEC + (CPRHO * VPD + DELTA * RAS * ASUBS + DELTA * RAA * AA) / (RS + RA)
ERATE <- ETOM * WTOMJ * (LE - LEC)
#
return(ERATE)
}

#***************************************************************************
SWGRA<-function(UA, ZA, HEIGHT, Z0, DISP, Z0C, DISPC, Z0GS, LWIDTH, RHOTP, NN, LAI, SAI, RAA, RAC, RAS){
#atmospheric resistances RAA, RAC, and RAS
#from Shuttleworth and Gurney (1990)
#input
#   UA      wind speed at reference height, m/s
#   ZA      reference height, m
#   HEIGHT  canopy height, m
#   Z0      roughness parameter, m
#   DISP    zero-plane displacement, m
#   Z0C     roughness length for closed canopy, m
#   DISPC   zero-plane displacement for closed canopy, m
#   Z0GS    roughness parameter of soil surface, m
#   LWIDTH  characteristic leaf width, m
#   RHOTP   ratio of total leaf area to projected leaf area
#   NN      wind/diffusivity extinction coefficient
#   LAI     projected leaf area index
#   SAI     projected stem area index
#output
#   RAA     boundary layer resistance, s/m
#   RAC     leaf-air resistance, s/m
#   RAS     ground-air resitance, s/m
#local
USTAR<-0
KH<-0
UH<-0
RB<-0
#intrinsic
#   LOG, EXP
#
USTAR <- K * UA / (log((ZA - DISP) / Z0))
KH <- K * USTAR * (HEIGHT - DISP)
RAS <- (HEIGHT * exp(NN) / (NN * KH)) * (exp(-NN * Z0GS / HEIGHT) - exp(-NN * (Z0C + DISPC) / HEIGHT))
if (RAS < 1) RAS <- 1
RAA <- log((ZA - DISP) / (HEIGHT - DISP)) / (K * USTAR) + (HEIGHT / (NN * KH)) * (-1 + exp(NN * (HEIGHT - DISPC - Z0C) / HEIGHT))

UH <- (USTAR / K) * log((HEIGHT - DISP) / Z0)
#the Shuttleworth-Gurney RB equation is strictly for one side of flat leaves
#when RHOTP > 2, LWIDTH is small (needles) so RAC is small
#their equation should have NN in numerator, see Choudhury and Monteith(1988)
RB <- (100 * NN) * (LWIDTH / UH) ^ 0.5 / (1 - exp(-NN / 2))
RAC <- RB / (RHOTP * LAI + PI * SAI)
#note LAI is prevented from being less than 1E-5
return(list(RAA, RAC, RAS))
}

#*************************************************************************
SWPE<-function(AA, ASUBS, VPD, RAA, RAC, RAS, RSC, RSS, DELTA){
#Shuttleworth and Wallace (1985) transpiration and ground evaporation
#input
#   AA      net radiation at canopy top minus ground flux, W/m2
#   ASUBS   net radiation minus ground flux at ground, W/m2
#   VPD     vapor pressure deficit, kPa
#   RAA     boundary layer resistance, s/m
#   RAC     leaf-air resistance, s/m
#   RAS     ground-air resistance, s/m
#   RSC     canopy surface resistance, s/m
#   RSS     ground evaporation resistance, s/m
#   DELTA   dEsat/dTair, kPa/K
#output
#   PRATE   potential transpiration rate, mm/d
#   ERATE   ground evaporation rate, mm/d
#local
RS<-0
RC<-0
RA<-0
PMS<-0
PMC<-0
D0<-0 #as in Shuttleworth and Wallace (1985)
CCS<-0
CCC<-0 #as CC and CS in Shuttleworth and Wallace (1985)
LE<-0      #total latent heat flux density, W/m2
#external function needed
#   PM
#
RS <- (DELTA + GAMMA) * RAS + GAMMA * RSS
RC <- (DELTA + GAMMA) * RAC + GAMMA * RSC
RA <- (DELTA + GAMMA) * RAA
CCS <- 1 / (1 + RS * RA / (RC * (RS + RA)))
CCC <- 1 / (1 + RC * RA / (RS * (RC + RA)))
PMS <- PM(AA, VPD - DELTA * RAS * (AA - ASUBS) / CPRHO, DELTA, RAA + RAS, RSS)
PMC <- PM(AA, VPD - DELTA * RAC * ASUBS / CPRHO, DELTA, RAA + RAC, RSC)
LE <- (CCC * PMC + CCS * PMS)
D0 <- VPD + RAA * (DELTA * AA - (DELTA + GAMMA) * LE) / CPRHO
PRATE <- ETOM * WTOMJ * PM(AA - ASUBS, D0, DELTA, RAC, RSC)
ERATE <- ETOM * WTOMJ * PM(ASUBS, D0, DELTA, RAS, RSS)
return(list(PRATE, ERATE))
}
#****************************************************************************
WEATHER<-function(TMAX, TMIN, DAYLEN, I0HDAY, EA, UW, ZA, DISP, Z0, WNDRAT, FETCH, Z0W, ZW, SOLRAD, SOLRADC, TA, TADTM, TANTM, UA, UADTM, UANTM){
#input
#   TMAX       maximum temperature for the day, ?C
#   TMIN       minimum temperature for the day, ?C
#   DAYLEN     daylength in fraction of day
#   I0HDAY     potential insolation on horizontal, MJ m-2 d-1
#   EA         vapor pressure for the day, kPa
#   UW         average wind speed for day at weather station, m/s
#   ZA         reference height for TA, EA, UA, above ground, m
#   DISP       zero-plane displacement, m
#   Z0         roughness parameter, m
#   WNDRAT     ratio of nighttime to daytime wind speed
#   FETCH      weather station fetch, m
#   Z0W        weather station roughness parameter, m
#   ZW         weather station measurement height for wind, m
#   SOLRAD     solar radiation for the day, horizontal surface, MJ/m2
#output
#   SOLRADC    corrected solar radiation for the day, horizontal surface, MJ/m2
#   TA         mean temperature for the day, ?C
#   TADTM      average daytime temperature, ?C
#   TANTM      average nighttime temperature, ?C
#   UA         average wind speed for the day at reference height, m/s
#   UADTM      average wind speed for daytime at ZA, m/s
#   UANTM      average wind speed for nighttime at ZA, m/s
#local
dummy<-0
#intrinsic
#  SIN
#external function needed
#  WNDADJ
#
#estimate SOLRAD if missing or limit if too high
if (SOLRAD < 0.001) {
  SOLRADC <<- RRD * I0HDAY
}else if (SOLRAD > I0HDAY) {
  SOLRADC <<- 0.99 * I0HDAY
}else{
  SOLRADC <<- SOLRAD
}
#average temperature for day
TA <<- (TMAX + TMIN) / 2
#daytime and nighttime average air temperature
TADTM <<- TA + ((TMAX - TMIN) / (2 * PI * DAYLEN)) * sin(PI * DAYLEN)
TANTM <<- TA - ((TMAX - TMIN) / (2 * PI * (1 - DAYLEN))) * sin(PI * DAYLEN)
#if no vapor pressure data, use saturated vapor pressure at minimum temp.
if (EA == 0) {esat<-ESAT(TMIN, EA, dummy)
  EA<<-unlist(esat[1])
}
#if no wind data, use value from frmmainb90 - Measured wind of zero must be input as 0.1, a problem
if (UW == 0) UW <<- UWD  #[28022018]  changed after Federer:be <<- UWD, where UWD is specified as 3.0 or some other value
#if wind < 0.2 m/s, set to 0.2 to prevent zero divide
if (UW < 0.2) UW <<- 0.2
#adjust wind speed from weather station to ZA
if (Z0W < 0.000001) {
UA <<- UW
}else{
UA <<- UW * WNDADJ(ZA, DISP, Z0, FETCH, ZW, Z0W)
}
#daytime and nighttime average wind speed
UADTM <<- UA / (DAYLEN + (1 - DAYLEN) * WNDRAT)
UANTM <<- WNDRAT * UADTM
#
return(list(SOLRADC, TA, TADTM, TANTM, UA, UADTM, UANTM))
}

#**************************************************************************
WNDADJ<-function(ZA, DISP, Z0, FETCH, ZW, Z0W){
#ratio of wind speed at reference height (above canopy) to
#   wind speed at weather station
#input
#  ZA      reference height, m
#  DISP    height of zero-plane, m
#  Z0      roughness parameter, m
#  FETCH   weather station fetch, m
#  ZW      weather station measurement height for wind,above any zero plane, m
#  Z0W     weather station roughness parameter, m
#output
#  WNDADJ  ratio
#local
HIBL<-0    #height of internal boundary layer, m
#intrinsic
#  LOG
#
#Brutsaert (1982) equation 7-39
HIBL <- 0.334 * FETCH ^ 0.875 * Z0W ^ 0.125
#Brutsaert equations 7-41 and 4-3
WNDADj <- log(HIBL / Z0W) * log((ZA - DISP) / Z0) / (log(HIBL / Z0) * log(ZW / Z0W))
#
return(WNDADj)
}

