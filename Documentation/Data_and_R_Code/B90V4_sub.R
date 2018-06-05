
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
MSBSETVARS<-function(){
#solar parameters depending on DOY%
sundss<-SUNDS(LAT, ESLOPE, DOY, L1, L2)
  
  DAYLEN<<-unlist(sundss[1])
  I0HDAY<<-unlist(sundss[2])
  SLFDAY<<-unlist(sundss[3])
#                                     ^^^^^^  ^^^^^^  ^^^^^^
#canopy parameters depending on DOY%
cano<-CANOPY(DOY, MAXHT, RELHT, MAXLAI, RELLAI, SNOW, SNODEN, MXRTLN, MXKPL, CS, DENSEF)
    HEIGHT<<-unlist(cano[1])
    LAI<<-unlist(cano[2])
    SAI<<-unlist(cano[3])
    RTLEN<<-unlist(cano[4])
    RPLANT<<-unlist(cano[5])
###                                                                                          ^^^^^^  ^^^  ^^^  ^^^^^  ^^^^^^
#roughness parameters
if (SNOW > 0) {
  Z0GS <<- Z0S
}else{
  Z0GS <<- Z0G
}
rough<-ROUGH(HEIGHT, ZMINH, LAI, SAI, CZS, CZR, HS, HR, LPC, CS, Z0GS)
  Z0GS<<-unlist(rough[1])
  Z0C<<-unlist(rough[2])
  DISPC<<-unlist(rough[3])
  Z0<<-unlist(rough[4])
  DISP<<-unlist(rough[5])
  ZA<<-unlist(rough[6])
#                                                                    ^^^  ^^^^^  ^^  ^^^^  ^^
#plant resistance components
plnt<-PLNTRES(NLAYER, THICK, STONEF, RTLEN, RELDEN, RTRAD, RPLANT, FXYLEM)
   RXYLEM<<-plnt[1]
   RROOTI<<-plnt[2:(ML+1)]
   ALPHA<<-plnt[(ML+2):(ML*2+1)]
###                                                                             ^^^^^^  ^^^^^^^^  ^^^^^^^
#calculated weather data
SHEAT <<- 0
WEATHER(TMAX, TMIN, DAYLEN, I0HDAY, EA, UW, ZA, DISP, Z0, WNDRAT, FETCH, Z0W, ZW, SOLRAD, SOLRADC, TA, TADTM, TANTM, UA, UADTM, UANTM)
###                                                                                            ^^^^^^^  ^^  ^^^^^  ^^^^^  ^^  ^^^^^  ^^^^^
#fraction of precipitation as SFAL
SNOFRC<<- SNOFRAC(TMAX, TMIN, RSTEMP)
#                                ^^^^^^
if (SNOW > 0) {
  #  snowpack temperature at beginning of day
  TSNOW <<- -CC / (CVICE * SNOW)
  #  potential snow evaporation
  PSNVP<<-SNOVAP(TSNOW, TA, EA, UA, ZA, HEIGHT, Z0, DISP, Z0C, DISPC, Z0GS, LWIDTH, RHOTP, NN, LAI, SAI, KSNVP)
  ###                                                                                                           ^^^^^
  ALBEDO <<- ALBSN
  RSS <<- 0
}else{
  TSNOW <<- 0
  PSNVP <<- 0
  ALBEDO <<- ALB
  #  soil evaporation resistance
  RSS <<- FRSS(RSSA, RSSB, PSIF[1], PSIM[1])
  # check for zero or negative RSS
  if (RSS < 0.000001) {
    #MsgBox ("RSS is very small or negative. Run ends. Check RSSA and RSSB values.")
    rstop <<- 3
  }
}
#snow surface energy balance (even if SNOW = 0 in case of snow during day)
SNOEN<<-SNOENRGY(TSNOW, TA, DAYLEN, CCFAC, MELFAC, SLFDAY, LAI, SAI, LAIMLT, SAIMLT)
###                                                                               ^^^^^
  
}

subdatafileline<-function(row){

YY<<- MData[[1]][row]
#change two-digit year to four-digit
if( YY < 100){
  if (YY > 20){
    YY <<- YY + 1900
  }else{
    YY <<- YY + 2000
  }
}
MM<<- MData[[2]][row]
DD<<- MData[[3]][row]
SOLRAD<<- MData[[4]][row]
TMAX <<- MData[[5]][row]
TMIN <<- MData[[6]][row]
EA <<- MData[[7]][row]
UW <<- MData[[8]][row]
PRECIN <<- MData[[9]][row]
MESFL <<- MData[[10]][row]
}

subprfileline<-function(row){
YY <<- MhhData[[1]][row]
#change two-digit year to four-digit
if( YY < 100){
  if( YY > 20 ){
YY <<- YY + 1900
}else{
YY <<- YY + 2000
}
}
MM <<- MhhData[[2]][row]
DD <<- MhhData[[3]][row]
II <<- MhhData[[4]][row]
PREINT <<- MhhData[[5]][row]
MESFLP <<- MhhData[[6]][row]
}

subprfilelineStatPrecip<-function(row,precip){
  YY <<- MhhData[[1]][row]
  #change two-digit year to four-digit
  if( YY < 100){
    if( YY > 20 ){
      YY <<- YY + 1900
    }else{
      YY <<- YY + 2000
    }
  }
  MM <<- MhhData[[2]][row]
  DD <<- MhhData[[3]][row]
  II <<- MhhData[[4]][row]
  PREINT <<- precip
  MESFLP <<- MhhData[[6]][row]
}
 
msum<-function(){
PRECM <<- RFALM + SFALM
STHRM <<- SFALM - SINTM
RTHRM <<- RFALM - RINTM
RNETM <<- RTHRM - RSNOM
EVAPM <<- IRVPM + ISVPM + SNVPM + SLVPM + TRANM
FLOWM <<- SRFLM + BYFLM + DSFLM + GWFLM
}

paccum<-function(){
#     accumulate flows over precip interval (below ground only)
#     zeroed by ZPINT.INC
#for(i in 1:NLAYER){
  VRFLPI <<- VRFLPI + VRFLI * DTI
  SLFLPI <<- SLFLPI + SLFLI * DTI
  INFLPI <<- INFLPI + INFLI * DTI
  BYFLPI <<- BYFLPI + BYFLI * DTI
  DSFLPI <<- DSFLPI + DSFLI * DTI
  NTFLPI <<- NTFLPI + NTFLI * DTI
  TRANPI <<- TRANPI + TRANI * DTI
#        note TRANI() are constant over precipitation interval
#}
SRFLP <<- SRFLP + SRFL * DTI
SLFLP <<- SLFLP + SLFL * DTI
GWFLP <<- GWFLP + GWFL * DTI

#if (IDAY >= 1529) {
#  GWFLP <<- GWFLP
#}

SEEPP <<- SEEPP + SEEP * DTI
#  sum flows for precip interval from components
sumii<-SUMI(NLAYER, BYFLPI, INFLPI, DSFLPI, TRANPI, DUMM, DUMM, BYFLP, INFLP, DSFLP, TRANP, dummy, dummy)
BYFLP<<-unlist(sumii[1])
INFLP<<-unlist(sumii[2])
DSFLP<<-unlist(sumii[3])
TRANP<<-unlist(sumii[4])
}

yaccum<-function(){
#  accumulate flows over year
#  zeroed by ZYEAR.INC
ACCUMI(NLAYER, VRFLMI, INFLMI, BYFLMI, DSFLMI, NTFLMI, VRFLYI, INFLYI, BYFLYI, DSFLYI, NTFLYI)
#                                                                         ^^^^^^^^  ^^^^^^^^  ^^^^^^^^  ^^^^^^^^  ^^^^^^^^
ACCUMI(NLAYER, TRANMI, SLFLMI, DUMM, DUMM, DUMM, TRANYI, SLFLYI, DUMM, DUMM, DUMM)
#                                                                   ^^^^^^^^
ACCUM(SRFLM, SLFLM, GWFLM, SEEPM, dummy, SRFLY, SLFLY, GWFLY, SEEPY, dummy)
#                                                ^^^^^  ^^^^^  ^^^^^  ^^^^^
ACCUM(ISVPM, IRVPM, SNVPM, SLVPM, SFALM, ISVPY, IRVPY, SNVPY, SLVPY, SFALY)
#                                                ^^^^^  ^^^^^  ^^^^^  ^^^^^  ^^^^^
ACCUM(RFALM, SINTM, RINTM, RSNOM, SMLTM, RFALY, SINTY, RINTY, RSNOY, SMLTY)
#                                                ^^^^^  ^^^^^  ^^^^^  ^^^^^  ^^^^^
ACCUM(MESFLM, PTRANM, PINTM, dummy, dummy, MESFLY, PTRANY, PINTY, dummy, dummy)
#                                                  ^^^^^^  ^^^^^^  ^^^^^
  #  sum flows for year from components
SUMI(NLAYER, BYFLYI, INFLYI, DSFLYI, TRANYI, DUMM, DUMM, BYFLY, INFLY, DSFLY, TRANY, dummy, dummy)
}

#Sub youtput()
#Dim strng$
##flows as amounts in mm for year
#If runflag% = 1 Then   # Rerun
#Line Input #31, strng$
#Print #21, strng$;
#Else
#Print #21, Format$(YEARN%, "0000"); "   "; "   "; "   ";
#End If
#Call msbvaroutput(1, yvars%, yvar%(), 21)
#Print #21,   # to end output line
#End Sub

ysum<-function(){
PRECY <<- RFALY + SFALY
STHRY <<- SFALY - SINTY
RTHRY <<- RFALY - RINTY
RNETY <<- RTHRY - RSNOY
EVAPY <<- IRVPY + ISVPY + SNVPY + SLVPY + TRANY
FLOWY <<- SRFLY + BYFLY + DSFLY + GWFLY
}

zday<-function(){
#zero daily accumulators
VRFLDI<<-rep(0,ML)
INFLDI<<-rep(0,ML)
BYFLDI<<-rep(0,ML)
DSFLDI<<-rep(0,ML)
NTFLDI<<-rep(0,ML)
TRANDI<<-rep(0,ML) 
SLFLDI<<-rep(0,ML)
SRFLD<<-0
GWFLD<<-0
SEEPD<<-0
SLFLD<<-0

IRVPD<<-0
ISVPD<<-0
SLVPD<<-0
SNVPD<<-0
SFALD<<-0
RFALD<<-0
SINTD<<-0
RINTD<<-0
RSNOD<<-0
SMLTD<<-0
MESFLD<<-0
PTRAND<<-0
PINTD<<-0
}

zmonth<-function(){
#zero monthly accumulators
 
  VRFLMI<<-rep(0,ML) 
  INFLMI<<-rep(0,ML) 
  BYFLMI<<-rep(0,ML) 
  DSFLMI<<-rep(0,ML) 
  NTFLMI<<-rep(0,ML) 
  TRANMI<<-rep(0,ML) 
  SLFLMI<<-rep(0,ML)

  SRFLM<<-0
  GWFLM<<-0
  SEEPM<<-0
  SLFLM<<-0
IRVPM<<-0 
  ISVPM<<-0 
  SLVPM<<-0 
  SNVPM<<-0
  SFALM<<-0
RFALM<<-0 
  SINTM<<-0 
  RINTM<<-0 
  RSNOM<<-0 
  SMLTM<<-0
  MESFLM<<-0 
  PTRANM<<-0 
  PINTM<<-0
}

zpint<-function(){
#zero precip interval accumulators
VRFLPI<<-rep(0,ML)
INFLPI<<-rep(0,ML)
BYFLPI<<-rep(0,ML)
DSFLPI<<-rep(0,ML)
 NTFLPI<<-rep(0,ML)
 TRANPI<<-rep(0,ML)
 SLFLPI<<-rep(0,ML)
SRFLP<<-0
SLFLP<<-0
GWFLP<<-0
SEEPP<<-0

}

zyear<-function(){
#zero annual accumulators
  VRFLYI<<-rep(0,ML)
  INFLYI<<-rep(0,ML)
  BYFLYI<<-rep(0,ML)
  DSFLYI<<-rep(0,ML)
 NTFLYI<<-rep(0,ML)
 TRANYI<<-rep(0,ML)
 SLFLYI<<-rep(0,ML) 
SRFLY<<-0
GWFLY<<-0
SEEPY<<-0
SLFLY<<-0
IRVPY<<-0
ISVPY<<-0
SLVPY<<-0
SNVPY<<-0
SFALY<<-0
RFALY<<-0
SINTY<<-0
RINTY<<-0
RSNOY<<-0
SMLTY<<-0
MESFLY<<-0
PTRANY<<-0
PINTY<<-0
}


psum<-function(){
 EVAPP <<- (ISVP + IRVP + SNVP + SLVP) * DTP + TRANP
 FLOWP <<- SRFLP + BYFLP + DSFLP + GWFLP
}


MSBPREINT<-function(){
#
#  convert precipitation interval mm to rate in mm/d
 PREC <<- PREINT / DTP
 SFAL <<- SNOFRC * PREC
 RFAL <<- PREC - SFAL
if (NPINT > 1) {
  #     more than one precip interval in day
  #     snow interception
  if (PINT < 0 && TA > 0) {
    #        prevent frost when too warm, carry negative PINT to rain
    temppp<-INTER(SFAL, 0, LAI, SAI, FSINTL, FSINTS, CINTSL, CINTSS, DTP, INTS, SINT, ISVP)
    SINT<<-unlist(temppp[1])
    ISVP<<-unlist(temppp[2])
    #                                                                                 ^^^^  ^^^^
  }else{
    temppp<-INTER(SFAL, PINT, LAI, SAI, FSINTL, FSINTS, CINTSL, CINTSS, DTP, INTS, SINT, ISVP)
      SINT<<-unlist(temppp[1])
      ISVP<<-unlist(temppp[2])
#                                                                                      ^^^^  ^^^^
  }
#     rain interception,  note potential interception rate is PID/DT-ISVP
  temppp<-INTER(RFAL, PINT - ISVP, LAI, SAI, FRINTL, FRINTS, CINTRL, CINTRS, DTP, INTR, RINT, IRVP)
    RINT<<-unlist(temppp[1])
    IRVP<<-unlist(temppp[2])
#                                                                                        ^^^^  ^^^^
}else{
  #     one precip interval in day, use storm DURATN and INTER24
  #     snow interception
  if (PINT < 0 && TA > 0) {
    #        prevent frost when too warm, carry negative PINT to rain
    temm<-INTER24(SFAL, 0, LAI, SAI, FSINTL, FSINTS, CINTSL, CINTSS, DURATN, INTS, SINT, ISVP, MONTHN)
      SINT<<-unlist(temm[1])
      ISVP<<-unlist(temm[2])
    #                                                                                               ^^^^  ^^^^
  }else{
    temm<-INTER24(SFAL, PINT, LAI, SAI, FSINTL, FSINTS, CINTSL, CINTSS, DURATN, INTS, SINT, ISVP, MONTHN)
      SINT<<-unlist(temm[1])
      ISVP<<-unlist(temm[2])
  #                                                                                                  ^^^^  ^^^^
  }
  #     rain interception,  note potential interception rate is PID/DT-ISVP
   temm<-INTER24(RFAL, PINT - ISVP, LAI, SAI, FRINTL, FRINTS, CINTRL, CINTRS, DURATN, INTR, RINT, IRVP, MONTHN)
     RINT<<-unlist(temm[1])
     IRVP<<-unlist(temm[2])
  #                                                                                                      ^^^^  ^^^^
}
#  throughfall
RTHR <<- RFAL - RINT
STHR <<- SFAL - SINT
#
#  reduce transpiration for fraction of precip interval that canopy is wet
WETFR <<- RMINF(1, (IRVP + ISVP) / PINT)
PTRAN <<- (1 - WETFR) * PTRAN
for( i in 1:NLAYER){
  TRANI[i] <<- (1 - WETFR) * TRANI[i]
}
#
if (SNOW <= 0 && STHR <= 0) {
  #     no snow, soil evaporation weighted for WETFR
  SLVP <<- WETFR * GIVP + (1 - WETFR) * GEVP
  RNET <<- RTHR
  RSNO <<- 0
  SNVP <<- 0
  SMLT <<- 0
}else{
  if (SNOW <= 0 && STHR > 0){
    #        new snow only, zero CC and SNOWLQ assumed
    CC <<- 0
    SNOWLQ <<- 0
  }
  #     snow accumulation and melt
  spa<-SNOWPACK(RTHR, STHR, PSNVP, SNOEN, CC, SNOW, SNOWLQ, DTP, TA, MAXLQF, GRDMLT)
    CC      <<-unlist(spa[1])
    SNOW    <<-unlist(spa[2])
    SNOWLQ  <<-unlist(spa[3])
    RSNO    <<-unlist(spa[4])
    SNVP    <<-unlist(spa[5])
    SMLT    <<-unlist(spa[6])
  #                                             ^^  ^^^^  ^^^^^^                           ^^^^  ^^^^  ^^^^
  RNET <<- RTHR - RSNO
  SLVP <<- 0
}

}

MSBITERATE<-function(){
#
#     source area flow rate
#  print("Currently here")
if (QLAYER > 0) {
  SAFRAC<<-SRFLFR()
}else{
  SAFRAC <<- 0
}
SRFL <<- RMINF(1, (IMPERV + SAFRAC)) * (RNET + SMLT)
#
#     water supply rate to soil surface
SLFL <<- RNET + SMLT - SRFL
#
#     bypass fraction of infiltration to each layer
BYFRAC<<-BYFLFR()

#                                                                 ^^^^^^^^
#     begin layer loop
for( i in  seq(NLAYER,1,-1)){
    #        downslope flow rates
    if( LENGTH == 0 || DSLOPE == 0){        # added in Version 4
      DSFLI[i]<<- 0
    }else{
      DSFLI[i]<<-DSLOP(i)
    ###                                                                                    ^^^^^^^^^
    }
    #        vertical flow rates
    if (i < NLAYER) {
      if (abs(PSITI[i] - PSITI[i+1]) < DPSIMX) {
        VRFLI[i] <<- 0
       
      }else{
        VRFLI[i]<<-VERT(i)
      ###                                                                                                                                                         ^^^^^^^^^
      }
    }else{
      #           bottom layer
      if( DRAIN > 0.0001){
        #              gravity drainage only
        VRFLI[NLAYER] <<- DRAIN * KK[NLAYER] * (1 - STONEF[NLAYER])
        #if(IDAY>=14){
        #  DRAIN<-DRAIN
        #}
        #            ElseIf DRAIN < -.0001 Then
        #              fixed water table at bottom of profile - not implemented, oscillation problems
        #               VRFLI(NLAYER%) = -DRAIN * (1 - STONEF(NLAYER%)) * KK(NLAYER%) * (PSIM(NLAYER%) / RHOWG + THICK(NLAYER%) / 2)
      }else{
        #              bottom of profile sealed
        VRFLI[NLAYER] <<- 0
      }
    }  
    if (IDAY >= 6 && i==NLAYER) {
      DRAIN<-DRAIN
    #  VRFLI[NLAYER] <- VRFLI[NLAYER]
    }
}

#     end of layer loop
#
#     first approximation for iteration time step, time remaining or DTIMAX
DTI <<- RMINF(DTRI, DTIMAX)
#
#     net inflow to each layer including E and T withdrawal adjusted for interception
inflo<-INFLOW()
   VV<<-unlist(inflo[1]) 
   INFLI<<-unlist(inflo[2])
   BYFLI<<-unlist(inflo[3])
   NTFLI<<-unlist(inflo[4])
###                                                                                                           ^^    ^^^^^    ^^^^^    ^^^^^
#
#     second approximation to iteration time step
for( i in 1:NLAYER){
  DPSIDW[i] <<- FDPSIDWF(i)
}
DTINEW<<-ITER(NLAYER, DTI, DPSIDW, NTFLI, SWATMX, PSITI, DSWMAX, DPSIMX)
###                                                                                 ^^^^^^
  if (DTINEW < DTI) {
    #        recalculate flow rates with new DTI
    if (mnuhalfiter == FALSE) {
      DTI <<- DTINEW
    }else{
      DTI <<- 0.5 * DTINEW
    }
    inflo<-INFLOW()
       VV<<-unlist(inflo[1]) 
       INFLI<<-unlist(inflo[2])
       BYFLI<<-unlist(inflo[3])
       NTFLI<<-unlist(inflo[4])
###                                                                                                              ^^    ^^^^^    ^^^^^    ^^^^^
  }
#     VV is the new VRFLI
for( i in 1:NLAYER){
  VRFLI[i] <<- VV[i]
}
#
#     groundwater flow and seepage loss
gwa<-GWATER(GWAT, GSC, GSP, DT, VRFLI[NLAYER])
    GWFL<<-unlist(gwa[1])
    SEEP<<-unlist(gwa[2])
#                                                     ^^^^  ^^^^
  #     end of rate calculations

}

daccum<-function(){
#  accumulate above ground flows over day
#  zeroed by ZDAY.INC   SRFLD + BYFLD + DSFLD + GWFLD
ISVPD <<- ISVPD + ISVP * DTP
IRVPD <<- IRVPD + IRVP * DTP
SNVPD <<- SNVPD + SNVP * DTP
SLVPD <<- SLVPD + SLVP * DTP
SFALD <<- SFALD + SFAL * DTP
RFALD <<- RFALD + RFAL * DTP
SINTD <<- SINTD + SINT * DTP
RINTD <<- RINTD + RINT * DTP
RSNOD <<- RSNOD + RSNO * DTP
SMLTD <<- SMLTD + SMLT * DTP
MESFLD <<- MESFLD + MESFLP * DTP
PTRAND <<- PTRAND + PTRAN * DTP
PINTD <<- PINTD + PINT * DTP
#  accumulate below ground flows over day
accumi<-ACCUMI(NLAYER, VRFLPI, INFLPI, BYFLPI, DSFLPI, NTFLPI, VRFLDI, INFLDI, BYFLDI, DSFLDI, NTFLDI)
#                                                                         ^^^^^^^^  ^^^^^^^^  ^^^^^^^^  ^^^^^^^^  ^^^^^^^^
VRFLDI<<-unlist(accumi[1]) 
INFLDI<<-unlist(accumi[2])
BYFLDI<<-unlist(accumi[3]) 
DSFLDI<<-unlist(accumi[4]) 
NTFLDI<<-unlist(accumi[5])

accumi<-ACCUMI(NLAYER, TRANPI, SLFLPI, DUMM, DUMM, DUMM, TRANDI, SLFLDI, DUMM, DUMM, DUMM)
#                                                                   ^^^^^^^^  ^^^^^^
TRANDI<<-unlist(accumi[1])
SLFLDI<<-unlist(accumi[2])

accum<-ACCUM(SRFLP, SLFLP, GWFLP, SEEPP, dummy, SRFLD, SLFLD, GWFLD, SEEPD, dummy)
SRFLD<<-unlist(accum[1])
SLFLD<<-unlist(accum[2])
GWFLD<<-unlist(accum[3])
SEEPD<<-unlist(accum[4])
#                                                ^^^^^  ^^^^^  ^^^^^  ^^^^^

#sum flows for day from components
summii<-SUMI(NLAYER, BYFLDI, INFLDI, DSFLDI, TRANDI, DUMM, DUMM, BYFLD, INFLD, DSFLD, TRAND, dummy, dummy)

BYFLD<<-unlist(summii[1])
INFLD<<-unlist(summii[2])
DSFLD<<-unlist(summii[3])
TRAND<<-unlist(summii[4])
}



maccum<-function(){
#accumulate flows over month
#zeroed by ZMONTH.INC
accumi<-ACCUMI(NLAYER, VRFLDI, INFLDI, BYFLDI, DSFLDI, NTFLDI, VRFLMI, INFLMI, BYFLMI, DSFLMI, NTFLMI)
  VRFLMI<<-unlist(accumi[1])
  INFLMI<<-unlist(accumi[2])
  BYFLMI<<-unlist(accumi[3])
  DSFLMI<<-unlist(accumi[4])
  NTFLMI<<-unlist(accumi[5])
#                                                                      ^^^^^^^^  ^^^^^^^^  ^^^^^^^^  ^^^^^^^^  ^^^^^^^^
accumi<-ACCUMI(NLAYER, TRANDI, SLFLDI, DUMM, DUMM, DUMM, TRANMI, SLFLMI, DUMM, DUMM, DUMM)
TRANMI<<-unlist(accumi[1])
SLFLMI<<-unlist(accumi[2])
#                                                                ^^^^^^^^  ^^^^^^
accumii<-ACCUM(SRFLD, SLFLD, GWFLD, SEEPD, dummy, SRFLM, SLFLM, GWFLM, SEEPM, dummy)
SRFLM<<-unlist(accumii[1])
SLFLM<<-unlist(accumii[2])
GWFLM<<-unlist(accumii[3])
SEEPM<<-unlist(accumii[4])
#                                             ^^^^^  ^^^^^  ^^^^^  ^^^^^
accumii<-ACCUM(ISVPD, IRVPD, SNVPD, SLVPD, SFALD, ISVPM, IRVPM, SNVPM, SLVPM, SFALM)
ISVPM<<-unlist(accumii[1])
IRVPM<<-unlist(accumii[2])
SNVPM<<-unlist(accumii[3] )
SLVPM<<-unlist(accumii[4])
SFALM<<-unlist(accumii[5])
#                                             ^^^^^  ^^^^^  ^^^^^  ^^^^^  ^^^^^
acumii<-ACCUM(RFALD, SINTD, RINTD, RSNOD, SMLTD, RFALM, SINTM, RINTM, RSNOM, SMLTM)
RFALM<<-unlist(acumii[1])
SINTM<<-unlist(acumii[2])
RINTM<<-unlist(acumii[3])
RSNOM<<-unlist(acumii[4])
SMLTM<<-unlist(acumii[5])
#                                             ^^^^^  ^^^^^  ^^^^^  ^^^^^  ^^^^^
acumii<-ACCUM(MESFLD, PTRAND, PINTD, dummy, dummy, MESFLM, PTRANM, PINTM, dummy, dummy)
MESFLM<<-unlist(acumii[1] )
PTRANM<<-unlist(acumii[2])
PINTM<<-unlist(acumii[3])
#                                               ^^^^^^  ^^^^^^  ^^^^^
  #  sum flows for month from components
summi<-SUMI(NLAYER, BYFLMI, INFLMI, DSFLMI, TRANMI, DUMM, DUMM, BYFLM, INFLM, DSFLM, TRANM, dummy, dummy)
BYFLM<<-unlist(summi[1])
INFLM<<-unlist(summi[2])
DSFLM<<-unlist(summi[3])
TRANM<<-unlist(summi[4])
}


dsum<-function(){
PRECD <<- RFALD + SFALD
STHRD <<- SFALD - SINTD
RTHRD <<- RFALD - RINTD
RNETD <<- RTHRD - RSNOD
EVAPD <<- IRVPD + ISVPD + SNVPD + SLVPD + TRAND
FLOWD <<- SRFLD + BYFLD + DSFLD + GWFLD

#if (IDAY >= 14) {
#    FLOWD <- FLOWD
#   }
}

fnleap<-function(){
if ((YEARN %% 4 == 0)  && ((YEARN %% 100 != 0) || (YEARN %% 400 == 0))) {
return(TRUE)
}else{
return(FALSE)
}
}


MSBDAYNIGHT<-function(){
SOVERI<<-0
for( J in  1:2){
  #  1 for daytime, 2 for nighttime
  #  net radiation
  if (J ==1){
    SLRAD <<- SLFDAY * SOLRADC / (WTOMJ * DAYLEN)
    SLRADd<<-SLRAD
    TAJ <<- TADTM
    UAJ <<- UADTM
  }else{
    SLRAD <<- 0
    TAJ <<- TANTM
    UAJ <<- UANTM
  }
  if (I0HDAY <= 0.01){
    #     no sunrise, assume 50% clouds for longwave
    SOVERI <<- 0.5
  }else{
    SOVERI <<- SOLRADC / I0HDAY
  }
  avai<-AVAILEN(SLRAD, ALBEDO, C1, C2, C3, TAJ, EA, SOVERI, SHEAT, CR, LAI, SAI)
    AA<<-unlist(avai[2])
    ASUBS<<-unlist(avai[3])
  
  ###                                                                                       ^^  ^^^^^
  #  vapor pressure deficit
  esat<-ESAT(TAJ, ES, DELTA)
    ES<<-unlist(esat[1])
    DELTA<<-unlist(esat[2])
  
  #                 ^^  ^^^^^
  VPD <<- ES - EA
  #  Shuttleworth-Wallace resistances
  swgra<-SWGRA(UAJ, ZA, HEIGHT, Z0, DISP, Z0C, DISPC, Z0GS, LWIDTH, RHOTP, NN, LAI, SAI, RAA, RAC, RAS)
    RAA<<-unlist(swgra[1])
    RAC<<-unlist(swgra[2])
    RAS<<-unlist(swgra[3])
  ###                                                                                     ^^^  ^^^  ^^^
  if (J == 1) {
    RSC<<-SRSC(SLRAD, TA, VPD, LAI, SAI, GLMIN, GLMAX, R5, CVPD, RM, CR, TL, T1, T2, TH)
    ###                                                                                       ^^^
  }else{
    RSC <<- 1 / (GLMIN * LAI)
  }
    #  Shuttleworth-Wallace potential transpiration and ground evaporation rates
  swpe<-  SWPE(AA, ASUBS, VPD, RAA, RAC, RAS, RSC, RSS, DELTA)
    PTR[J]<<-unlist(swpe[1])
    GER[J]<<-unlist(swpe[2])
    #                                                            ^^^^^^^  ^^^^^^^
    #  Shuttleworth-Wallace potential interception and ground evap. rates
    #  RSC = 0, RSS not changed
  swpe<-  SWPE(AA, ASUBS, VPD, RAA, RAC, RAS, 0, RSS, DELTA)
    PIR[J]<<-unlist(swpe[1])
    GIR[J]<<-unlist(swpe[2])
    #                                                           ^^^^^^^  ^^^^^^^
    #  actual transpiration and ground evaporation rates
    if (PTR[J] > 0.001) {
      rbl<-TBYLAYER(J, PTR[J], DISPC, ALPHA, KK, RROOTI, RXYLEM, PSITI, NLAYER, PSICR, NOOUTF)
       # ATR[J]<-rbl[1]
      #  ATRANI<-rbl[1:ML+1]
      #PSIT<<-unlist(tbl[1])
      ATR[J]<<-unlist(rbl[1])
      ATRANI<<-unlist(rbl[2])###                                                                                                        ^^^^^^^  ^^^^^^^^
    for (i in 1:NLAYER){
      ATRI[J,i] <<- ATRANI[i]
    }
    if (ATR[J] < PTR[J]){
      #        soil water limitation, new GER
      GER[J]<<-SWGE(AA, ASUBS, VPD, RAA, RAS, RSS, DELTA, ATR[J], GER[J])
      
#                                                                 ^^^^^^^
    }
  }else{
    #     no transpiration, condensation ignored, new GER
    PTR[J] <<- 0
    ATR[J] <<- 0
    for( i in 1:NLAYER){
      ATRI[J,i] <<- 0
    }
    GER[J]<<-SWGE(AA, ASUBS, VPD, RAA, RAS, RSS, DELTA, 0, GER[J])
#                                                         ^^^^^^^
  }
#
}

}

swchek<-function(i){
#test for SWATI(I%) < 0 or > SWATMX(I%)

  if (SWATI[i] <= 0) {
   
    if(swatproblem >0){}
  }else if (SWATI[i] > SWATMX[i]){
  
  if (SWATI[i] > SWATMX[i] + 0.00001) {
    if(swatproblem >0){}
  }else{
    ##     rounding error only
    SWATI[i] <<- SWATMX[i]
  }
}
}
DOYF<-function(day,month, daymo){
  
  doyy<-0
  if(fnleap()){
    daymo[2]<-29
  }else{
    daymo[2]<-28
  }
  
  if(month>1)
    doyy<-daymo[1]+doyy
  if(month>2)
    doyy<-daymo[2]+doyy
  if(month>3)
    doyy<-daymo[3]+doyy
  if(month>4)
    doyy<-daymo[4]+doyy
  if(month>5)
    doyy<-daymo[5]+doyy
  if(month>6)
    doyy<-daymo[6]+doyy
  if(month>7)
    doyy<-daymo[7]+doyy
  if(month>8)
    doyy<-daymo[8]+doyy
  if(month>9)
    doyy<-daymo[9]+doyy
  if(month>10)
    doyy<-daymo[10]+doyy 
  if(month>11)
    doyy<-daymo[11]+doyy
  if(month>12)
    doyy<-daymo[12]+doyy 
  
  doyy<-doyy+day
  
  return(doyy)
}