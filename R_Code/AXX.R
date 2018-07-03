
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
#***********************************************************************
ACCUM <-function(A1, A2, A3, A4, A5, B1, B2, B3, B4, B5){
# accumulator; adds Aj to Bj
B1 <- B1 + A1
B2 <- B2 + A2
B3 <- B3 + A3
B4 <- B4 + A4
B5 <- B5 + A5
return(list(B1, B2, B3, B4, B5))
}

#***************************************************************************
ACCUMI<-function (N, A1, A2, A3, A4, A5, B1, B2, B3, B4, B5){
# accumulator for array components; adds Aj(i) to Bj(i) for each i up to n
  B1 <- B1 + A1
  B2 <- B2 + A2
  B3 <- B3 + A3
  B4 <- B4 + A4
  B5 <- B5 + A5
#for( i in 1:N){
#  B1[i] <- B1[i] + A1[i]
#  B2[i] <- B2[i] + A2[i]
#  B3[i] <- B3[i] + A3[i]
#  B4[i] <- B4[i] + A4[i]
#  B5[i] <- B5[i] + A5[i]
#}
return(list(B1, B2, B3, B4, B5))
}

#*******************************************************************
ACOSF<-function (T){
#arc cosine in radians from 0 to pi
TA<-0
AC<-0
TA <- abs(T)
if (TA > 1) {#

}
if (TA < 0.7) {
  AC <- 1.570796 - atan(TA / (1 - TA * TA)^(1/2))
}else{
  AC <- atan((1 - TA * TA)^(1/2) / TA)
}
if (T < 0) {
  ACOS <- 3.141593 - AC
}else{
  ACOS <- AC
}
return(ACOS)#acos(T))
}

#**********************************************************
ASINF<-function (temp){
#arc sine in radians from -pi/2 to pi/2
TA<-0
TA <- abs(temp)
if(TA > 1){
#  
}
if (TA < 0.7) {
  ASIN <- sign(temp) * (atan(TA / (1 - TA * TA)^(1/2)))
}else{
  ASIN <- sign(temp) * (1.570796 - atan((1 - TA * TA)^(1/2) / TA))
}
return(ASIN)#asin(temp))
}

#**********************************************************
RMAXF <-function(T1, T2){
#real single precision maximum
if (T1 < T2) {
  RMAX <- T2
}else{
  RMAX <- T1
}
return(RMAX)
}

#**************************************************
RMINF<-function (T1, T2){
#real single precision minimum
if (T1 > T2) {
RMIN <- T2
}else{
RMIN <- T1
}
return(RMIN)
}

#************************************************************************
SUMI<-function (N, A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6){
#array summer; sums Aj(i) for i = 1,n with result in Bj
B<-rep(0,N)
#ListB<-  ZERO(B1, B2, B3, B4, B5, B6)
for( i in 1:N){
B[1] <- B[1] + A1[i]
B[2] <- B[2] + A2[i]
B[3] <- B[3] + A3[i]
B[4] <- B[4] + A4[i]
B[5] <- B[5] + A5[i]
B[6] <- B[6] + A6[i]
}
return(list(B[1], B[2], B[3], B[4], B[5], B[6]))
}

#***************************************************************************
ZERO<-function (V1, V2, V3, V4, V5, V6){
#zeroes variables
  V1 <- 0
  V2 <- 0
  V3 <- 0
  V4 <- 0
  V5 <- 0
  V6 <- 0
    return(list(V1, V2, V3, V4, V5, V6))
}

#***************************************************************************
ZEROA <-function(N,A1,A2,A3,A4){
#zeroes arrays
#for( i in 1:N){
  A1<-rep(0,ML)
  A2<-rep(0,ML)
  A3<-rep(0,ML)
  A4<-rep(0,ML)
#}
return(list(A1,A2,A3,A4))
}
