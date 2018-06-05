
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

#***************************************************************************
AVAILEN<-function (SLRAD, ALBEDO, C1, C2, C3, TA, EA, RATIO, SHEAT, CR, LAI, SAI){
#available energy at canopy and ground
#longwave equations and parameters from Brutsaert (1982)
#net radiation extinction from Shuttleworth and Wallace(1985)
#input
#   SLRAD   solar radiation on slope, W/m2
#   ALBEDO  albedo
#   C1      intercept of relation of solar radiation to sunshine duration
#   C2      slope of relation of solar radiation to sunshine duration
#   C3      longwave correction factor for overcast sky
#   TA      air temperature, degC
#   RATIO   ratio of solar radiation on horizontal to potential insolation for day
#   EA      vapor pressure, kPa
#   SHEAT   average soil heat flux for the day, W/m2, usually 0
#   CR      light extinction coefficient for projected LAI + SAI"
#   LAI     leaf area index, m2/m2
#   SAI     stem area index, m2/m2
#output
#   AA      available energy, W/m2
#   ASUBS   availble energy at ground, W/m2
#local
 SOLNET <-0  #net solar radiation, W/m2
 EFFEM<-0   #effective emissivity from clear sky
 NOVERN<-0  #sunshine duration fraction of daylength
 CLDCOR<-0  #cloud cover correction to net longwave under clear sky
 LNGNET<-0   #net longwave radiation, W/m2
 RN  <-0    # net radiation, W/m2
#intrinsic
#   EXP
#constant
#   SIGMA
#
SOLNET <- (1 - ALBEDO) * SLRAD
#Brutsaert equation for effective clear sky emissivity
EFFEM <- 1.24 * (EA * 10 / (TA + 273.15)) ^ (1 / 7)
NOVERN <- (RATIO - C1) / C2
if (NOVERN > 1)  NOVERN <- 1
if (NOVERN < 0)  NOVERN <- 0
CLDCOR <- C3 + (1 - C3) * NOVERN
#emissivity of the surface taken as 1.0 to also account for reflected
LNGNET <- (EFFEM - 1) * CLDCOR * SIGMA * (TA + 273.15) ^ 4
RN <- SOLNET + LNGNET
AA <- RN - SHEAT
ASUBS <- RN * exp(-CR * (LAI + SAI)) - SHEAT

#if(AA>0) points(IDAY,AA,col="black")

return(list(RN, AA, ASUBS))
}

#*************************************************************************
EQUIVSLP<-function (LAT, SLOPE, ASPECT){
#latitude and time shift of "equivalent slope", the point on globe where a
#  horizontal surface is parallel to the slope
#needed only once for each non-horizontal slope
#Swift#s L1 and L2, Lee (3.31, 3.32)
#inputs
#  LAT     latitude, radians (S neg)
#  SLOPE   slope, radians
#  ASPECT  aspect, radians from N thru E
#outputs
#  L1      latitude of equivalent slope, radians
#  L2      time shift of equivalent slope, radians
#local
D1<-0
#external function needed
#  ASIN
#intrinsic
#  SIN, COS, ATN
#
L1 <- ASINF(cos(SLOPE) * sin(LAT) + sin(SLOPE) * cos(LAT) * cos(ASPECT))
D1 <- cos(SLOPE) * cos(LAT) - sin(SLOPE) * sin(LAT) * cos(ASPECT)
if (D1 == 0)  D1 <- .0000000001
L2 <- atan(sin(SLOPE) * sin(ASPECT) / D1)
if (D1 < 0) L2 <- L2 + PI
return(list(L1, L2))
}

#*****************************************************************************
FUNC3<-function(DEC, L2, L1, T3, T2){
#daily integration for slope after Swift (1976), d
#input
#  DEC     declination of the sun, radians
#  L2      time shift of equivalent slope, radians
#  L1      latitude of equivalent slope, radians
#  T3      hour angle of sunset on slope
#  T2      hour angle of sunrise on slope
#intrinsic
#  SIN, COS
FUnC3 <- (1 / (2 * 3.14159)) * (sin(DEC) * sin(L1) * (T3 - T2) + cos(DEC) * cos(L1) * (sin(T3 + L2) - sin(T2 + L2)))
return(FUnC3)
}

#******************************************************************************
HAFDAY<-function (LAT, DEC){
#half day length in radians
#inputs
#  LAT      latitude, radians (S neg)
#  DEC      declination of the sun, radians
#output
#  HAFDAy   half daylength, radians
#local
ARG<-0
#external function needed
#  ACOS
#intrinsic
#  ABS, SGN, TAN
#
if (abs(LAT) >= PI / 2)  LAT <- sign(LAT) * (PI / 2 - 0.01)
ARG <- -tan(DEC) * tan(LAT)
if (ARG >= 1) {
#  sun stays below horizon
HAFDAy <- 0
}else if (ARG <= -1) {
#  sun stays above horizon
HAFDAy <- PI
}else{
HAFDAy <- ACOSF(ARG)
}
return(HAFDAy)
}
#******************************************************************************
SUNDS<-function (LAT, SLOPE, DOY, L1, L2, DAYLEN, I0HDAY, SLFDAY){
#daylength, potential daily solar radiation on horizontal,
#  and ratio of potential on slope (map area) to horizontal
#from Swift (1976)
# input
#  LAT     latitude, radians
#  SLOPE   slope, radians
#  DOY     day of the year
#  L1      latitude of equivalent slope, radians, from EQUIVSLP
#  L2      time shift of equivalent slope, radians, from EQUIVSLP
#outputs
#  DAYLEN  daylength (sun above horizontal) in fraction of day, d
#  I0HDAY  potential insolation on horizontal surface, MJ/m2
#  SLFDAY  ratio of potential insolation on slope to horizontal, map area
#local
I0SDAY <-0 #potential insolation on slope, map area basis, MJ/m2
SCD <-0   # solar constant for day, W/m2
DEC <-0   # declination of the sun, radians
TWORIS <-0 # if two sunrises on slope
Temp  <-0     #temporary variable
T0 <-0     #hour angle of sunrise on horizontal, radians
T1 <-0     #hour angle of sunset on horizontal
T2 <-0    # hour angle of sunrise on slope
T3 <-0    # hour angle of sunset on slope
T6 <-0     #hour angle of sunrise on equivalent slope
T7 <-0     #hour angle of sunset on equivalent slope
T8 <-0    # hour angle of second sunrise on slope
T9 <-0     #hour angle of second sunset on slope
#constants
#  WTOMJ   (MJ m-2 d-1) / (W/m2)
#  PI
#  SC       solar constant, W/m2
#external functions needed
#  HAFDAY, FUNC3, RMIN, RMAX, ASIN
#intrinsic
#  COS, SIN
#
SCD <- SC / (1 - .0167 * cos(.0172 * (DOY - 3))) ^ 2
DEC <- ASINF(.39785 * sin(4.868961 + .017203 * DOY + .033446 * sin(6.224111 + .017202 * DOY)))
Temp <- HAFDAY(LAT, DEC)
DAYLEN <- RMAXF(.0001, RMINF(.9999, Temp / PI))
#  to avoid zero divides for 0 and 1
T1 <- Temp
T0 <- -Temp
Temp <- HAFDAY(L1, DEC)
T7 <- Temp - L2
T6 <- -Temp - L2
T3 <- RMINF(T1, T7)
T2 <- RMAXF(T0, T6)
  if (T3 < T2) {
    T2 <- 0
     T3 <- 0
  }
T6 <- T6 + 2 * PI
  if (T6 < T1) {
  T8 <- T6
  T9 <- T1
  TWORIS <- 1
  }
  T7 <- T7 - 2 * PI
  if (T7 > T0) {
    T8 <- T0
    T9 <- T7
    TWORIS <- 1
  }else{
  TWORIS <- 0
  }
if (TWORIS == 1) {   # two sunrises
  I0SDAY <- WTOMJ * SCD * (FUNC3(DEC, L2, L1, T3, T2) + FUNC3(DEC, L2, L1, T9, T8)) / cos(SLOPE)
#  "daylength" on the slope = ((T3 - T2) + (T9 - T8)) / (2. * PI)
}else{    #  one sunrise
  I0SDAY <- WTOMJ * SCD * FUNC3(DEC, L2, L1, T3, T2) / cos(SLOPE)
#  COS(SLOPE) adjusts from slope area to map area
#  "daylength" on the slope = (T3 - T2) / (2. * PI)
}
  I0HDAY <- WTOMJ * SCD * FUNC3(DEC, 0, LAT, T1, T0)
  if (I0HDAY <= 0){
    SLFDAY <- 0
  }else{
    SLFDAY <- I0SDAY / I0HDAY
  }

#  if (IDAY >= 766) {
 #   DAYLEN <- DAYLEN
#  }
  return(list(DAYLEN, I0HDAY, SLFDAY))
}

