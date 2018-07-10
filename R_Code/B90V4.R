
#   Code by R.Kronenberg as RK [06112017]  based on Brook90 by Federer et.al
#  modified RK [16112017]
#
#   TU Dresden 
#   Institut fuer Hydrologie und Meteorologie
#   Professur fuer Meteorologie
#   2017
#
#
#
#
#
#
#
#
#
#B90<-function(){   ANMACHEN WIEDER
#called and returned only from one location in msbrunB990



#modified for Version 4, June 3, 1999
#
#  ^^^^ shows variables that are returned from or altered by subroutines (requires a fixed width font)
#
#intrinsic functions needed
#  CSNG, INT, INKEY$, CHR$
#
#
#program initializations if New Run or Rerun
if ((runflag == 0) || (runflag == 1)){
  #
  DAYMO[1] = 31
  DAYMO[2] = 28
  DAYMO[3] = 31
  DAYMO[4] = 30 
  DAYMO[5] = 31
  DAYMO[6] = 30
  DAYMO[7] = 31
  DAYMO[8] = 31
  DAYMO[9] = 30
  DAYMO[10] = 31
  DAYMO[11] = 30
  DAYMO[12] = 31
	IDAY =1#766#11689#12054# 1
	IInterValDay=1
	NDAYS=length(MData[[1]])#12418#length(MData[[1]])#12418#length(MData[[1]])
	NITSR = 0
	NITSY = 0
	NITSM = 0
	YEARN = as.numeric(MData[[1]][IDAY])
	
	daymax=NDAYS-IDAY+1
	maxF=0
	timeseries_prec=rep(0,daymax)
	timeseries_evp=rep(0,daymax)
	timeseries_flow=rep(0,daymax)
	timeseries_rnet=rep(0,daymax)
	timeseries_ptran=rep(0,daymax)
	timeseries_irvp=rep(0,daymax)
	timeseries_isvp=rep(0,daymax)
	timeseries_snow=rep(0,daymax)
	timeseries_swat=rep(0,daymax)
	timeseries_pint=rep(0,daymax)
	timeseries_snvp=rep(0,daymax)
	timeseries_slvp=rep(0,daymax)
	timeseries_trand=rep(0,daymax)
	timeseries_mesfld=rep(0,daymax)
	timeseries_smltd=rep(0,daymax)
	timeseries_slfld=rep(0,daymax)
	timeseries_rfald=rep(0,daymax)
	timeseries_sfald=rep(0,daymax)
	timeseries_awat=rep(0,daymax)
	timeseries_adef=rep(0,daymax)
	timeseries_sintd=rep(0,daymax)
	timeseries_rintd=rep(0,daymax)
	timeseries_rthrd=rep(0,daymax)
	timeseries_sthrd=rep(0,daymax)
	timeseries_rsnod=rep(0,daymax)
	
	# change two-digit year to four-digit
	if( YEARN < 100){
		if(YEARN > 20){
			YEARN = YEARN + 1900
		}else{
			YEARN = YEARN + 2000
		}
	}
  	MONTHN = as.numeric(MData[[2]][IDAY])
	DOM = as.numeric(MData[[3]][IDAY])
	#NPINT =1
	DOY = DOY=DOYF(DOM,MONTHN,DAYMO)#as.POSIXlt(paste(sprintf("%02d", DOM),sprintf("%02d", MONTHN),YEARN,sep=""), format = "%d%m%y")$yday+1  
		
	  if (fnleap()) {
			DAYMO[2] = 29
		}else{
			DAYMO[2] = 28
		}
	#open dfile
	##Close #1 # in case still open from previous run
	#Open frmmainB90.txtdfilename.Text For Input As #1
	#open prfile if needed
	if (SUBDAYDATA) {
  	DTP = DT / NPINT
	#	Close #2 # in case still open from previous run
	#	Open frmmainB90.txtprfilename.Text For Input As #2
	}else{
		DTP = DT
	}
	#
	#zero accumulators
	zyear()
	zmonth()
	#
	#initial values
	SNOW = SNOWIN
	GWAT = GWATIN
	INTR = INTRIN
	INTS = INTSIN
	for( i in 1:NLAYER){
		PSIM[i] = PSIMIN[i]
	}
	#
	#soil water parameters and initial variables
	soilp<-SOILPAR()
	  PSIG<-unlist(soilp[2])
	  SWATMX<-unlist(soilp[3])
	  WETF<-unlist(soilp[4])
	  WETC<-unlist(soilp[5])
	  CHM<-unlist(soilp[6])
	  CHN<-unlist(soilp[7]) 
	  WETNES<-unlist(soilp[8])
	  SWATI<-unlist(soilp[9])
	  KSAT<-unlist(soilp[10])
###                                                                                                        ^^^^^^  ^^^^^^^^  ^^^^^^  ^^^^^^  ^^^^^  ^^^^^  ^^^^^^^^  ^^^^^^^  ^^^^^^
#more initial soil water variables
	soil<-SOILVAR()
	  PSITI<-soil[1:ML]
	  THETA<-soil[(ML+1):(2*ML)]
	  KK<-soil[(2*ML+1):(3*ML)]
	  SWAT<-soil[(3*ML+1)]
###                                                                                     ^^^^^    ^^^^^    ^^    ^^^^
	#initial total water in system
	STORD = INTR + INTS + SNOW + SWAT + GWAT
	STORM = STORD
	STORY = STORD
	#any initial snow has zero liquid water and cold content
	CC = 0
	SNOWLQ = 0
}
#
#parameter initializations for New Run, Rerun, and Continue Run
#parameter conversions
GLMAX = GLMAXC / 100#
GLMIN = GLMINC / 100#
LAT = LATD / 57.296
ESLOPE = ESLOPED / 57.296
DSLOPE = DSLOPED / 57.296
ASPECT = ASPECTD / 57.296
#equivalent slope for radiation calculations
equi<-EQUIVSLP(LAT, ESLOPE, ASPECT)
  L1<-unlist(equi[1])
  L2<-unlist(equi[2])
#                                  ^^  ^^
  #infiltration parameters
infpa<-INFPAR(INFEXP, IDEPTH, NLAYER, THICK)
  ILAYER<-unlist(infpa[1])
  INFRAC<-unlist(infpa[2])
#                                             ^^^^^^   ^^^^^^
  #source area parameters
srfp<-SRFPAR(QDEPTH, NLAYER, THETAF, THICK, STONEF, SWATMX)
  QLAYER<-unlist(srfp[1]) 
  SWATQX<-unlist(srfp[2])
  SWATQF<-unlist(srfp[3])
#                                                                   ^^^^^^   ^^^^^^  ^^^^^^
  #root density parameterS
RELDEN<-RTDEN(ROOTDEN, NLAYER, THICK)
#                                       ^^^^^^
  #
#***************  B E G I N    D A Y   L O O P  ***************************
  #
while( IDAY <= NDAYS){  # ANMACHEN WIEDER
#
#yield time to Windows - may not be needed
#DoEvents
#
#initiate graphics window if necessary
#	if Not graphon% And frmmainB90!chkgraph Then
#If gnumber% = maxgraphs% Then
#MsgBox "You have reached the maximum allowed number of graphs." & Chr$(10) & "Only Run - New Run is #allowed; this will clear all graphs."
#rstop% = 4 # and run will stop
#Else
#gnumber% = gnumber% + 1
#Call subgraph(FG(gnumber%), 1)
#graphon% = True
#End If
#End If
#test if run should stop here (end of day)
#	if (rstop > 0) break
	#
	NITSD = 0
	#
#	if( IDAY == 1) {
		#read first data line from dfile, later lines are read at end of day loop
		subdatafileline(IDAY)
		#points(IDAY,TMIN,col="blue")
		#points(IDAY,TMAX,col="brown")
		#points(IDAY,TA,col="red")
		#points(IDAY,UW,col="yellow")
		#if( frmmainB90!chkdates) {  subcheckdata()
			#if (rstop = 5 ) break
#		}
#
#visible display of progress
#frmmainB90!lbldaynvalue = DOM%
#frmmainB90!lblmonthnvalue = MONTHN%
#frmmainB90!lblyearnvalue = YEARN%
#frmmainB90.lbldorvalue = IDAY&
  
  if( IDAY == INIDAYS + 1){
		#end of initialization, reinitialize year and month accumulators
		STORD = INTR + INTS + SNOW + SWAT + GWAT
		STORM = STORD
		STORY = STORD
		NITSY = 0
		NITSM = 0
		zyear()
		zmonth()
  }
	#
	#calculate derived variables
	MSBSETVARS()
	#if( rstop = 3) break
#
#* * * * *  B E G I N   D A Y - N I G H T   E T   L O O P  * * * * * * * * *
#
#potential and actual interception, evaporation, and transpiration
	MSBDAYNIGHT()
#
#* * * * * * * * *  E N D   D A Y - N I G H T   L O O P  * * * * * * * * * *
#
#average rates over day
	PTRAN = (PTR[1] * DAYLEN + PTR[2] * (1 - DAYLEN)) / DT
	GEVP = (GER[1] * DAYLEN + GER[2] * (1 - DAYLEN)) / DT
	PINT = (PIR[1] * DAYLEN + PIR[2] * (1 - DAYLEN)) / DT
	GIVP = (GIR[1] * DAYLEN + GIR[2] * (1 - DAYLEN)) / DT
	for(i in 1:NLAYER){
		TRANI[i] = (ATRI[1, i] * DAYLEN + ATRI[2, i] * (1 - DAYLEN)) / DT
	}
#TRAN from ATR(J%) is not needed
#
#zero daily integrators
	zday()
#
#* * * * * * * * B E G I N   P R E C I P   I N T E R V A L * * * * * * * * *
  #
for( N in 1:NPINT){  #ANMACHEN WIEDER
		#If (frmmainB90.txtprfilename.Text <> "None") Then
		#	#     precip data from prfile
		#	If EOF(2) Then
		#	If frmmainB90!chkdates Then
		##	MsgBox "End of precip interval file before end of dfile. Run ends."
		#		GoTo endrun
		#	Else
		#	#restart prfile
		#	Close #2
		#	Open frmmainB90.txtprfilename.Text For Input As #2
		#	End If
		#End If
	
	#If frmmainB90!chkdates Then
	#     check PRFILE order
	#If (YEARN% <> YY% Or MONTHN% <> MM% Or DOM% <> DD% Or N% <> II%) Then
	#Call msbprferrortext
	#frmprferror.Show 1
	#GoTo endrun
	#End If
	#End If
  
  if (SUBDAYDATA){
    subprfileline(IInterValDay)
	#if MESFLP = -1 use MESFL/DT for all precip intervals in day
	  #CHECK if there is als hourly data of precip
	  if (MESFLP <= -0.01) {MESFLP = MESFL / DT}
		#MESFLP = MESFL / DT
		
		
	}else{
	#     precip data from data file
		PREINT = PRECIN / DT
		MESFLP = MESFL / DT
	}
#
#  interception and snow accumulation/melt
	MSBPREINT()
	
#  
#  initialize for iterations
#  initial time remaining in iteration time step = precip time step
	DTRI = DTP
#  initialize iteration counter
	NITS = 0
#
#  zero precip interval integrators
	zpint()
#
#  *  *  *  *  *  *  B E G I N   I T E R A T I O N   *  *  *  *  *  *  *  *
  #
	

	while(!(DTRI <= 0)){  # 	print("Currently here")
		NITS = NITS + 1
		#     check for events
		if (NITS %% 100 == 0) {}
			#     test for Stop Iterations
		#if (rstop == 3) break
		#
		#     water movement through soil
		MSBITERATE() # iteration calculations
		#
		#     calculate SLFLI vertical macropore infiltration out of layer
		SLFLI[1] = SLFL - INFLI[1] - BYFLI[1]
		if (ILAYER >= 2){
		  if (NLAYER >= ILAYER +1){
		    for (i in 2:ILAYER){ # does not execute if ILAYER% = 1 or 0
			    SLFLI[i] = SLFLI[i - 1] - INFLI[i] - BYFLI[i]
		    }
		    for( i in (ILAYER + 1):NLAYER){ # does not execute if NLAYER% < ILAYER% + 1
			    SLFLI[i] = 0
		    }
		  }
		}
		#
		#     integrate below ground storages over iteration interval
		for( i in 1:NLAYER){
			SWATI[i] = SWATI[i] + NTFLI[i] * DTI
		}
		GWAT = GWAT + (VRFLI[NLAYER] - GWFL - SEEP) * DTI
		#
		#     new soil water variables and test for errors
		for (i in 1:NLAYER){
			swchek(i)
			#if(rstop== 3) break
			WETNES[i] = SWATI[i] / SWATMX[i]
			PSIM[i] = FPSIMF(WETNES[i], PSIF[i], BEXP[i], WETINF[i], WETF[i], CHM[i], CHN[i])
		}
		soil<-SOILVAR()
		   PSITI<-soil[1:ML]
		   THETA<-soil[(ML+1):(2*ML)]
		   KK<-soil[(2*ML+1):(3*ML)]
		   SWAT<-soil[(3*ML+1)]

      ###                                                                                           ^^^^^    ^^^^^    ^^    ^^^^
       #     iteration output
#	if (outselect[5] && IDAY > INIDAYS) ioutput()
       #     flows accumulated over precip interval
		 
		   	   
	paccum()
       #
       #     time remaining in precipitation time-step
	DTRI = DTRI - DTI
       #
	NITSR = NITSR + 1  # for visible display of iterations
      #	if( NITS Mod 200 == 0 )Then frmmainB90!lblitsrunvalue = NITSR&
      #DoEvents # to allow iteration update every 200 iterations
      #
}
#
#  *  *  *  *   E N D   i T E R A T I O N    L O O P  *  *  *  *  *  *  *  *
#
#  display iterations
#frmmainB90!lblitsrunvalue = NITSR&
#DoEvents # to allow iterations update every precip interval
#  integrate interception storages over precip interval
INTS = INTS + (SINT - ISVP) * DTP
INTR = INTR + (RINT - IRVP) * DTP
#
#  flows for precip interval summed from components
psum()
#  precipitation interval output
#If outselect(4) And IDAY& > INIDAYS& Then poutput
#  flows accumulated over day
daccum()

#  accumulate iterations
  NITSD = NITSD + NITS
  NITSM = NITSM + NITS
  NITSY = NITSY + NITS
  #
  IInterValDay<-IInterValDay+1
}
#
#* * * * *  E N D   P R E C I P   I N T E R V A L   L O O P  * * * * * * * *
#
#flows for day summed from components
dsum()
#
#check for water balance error
BALERD = STORD - (INTR + INTS + SNOW + SWAT + GWAT) + PRECD - EVAPD - FLOWD - SEEPD
#If (Abs(BALERD) > 0.003) Then
#   MsgBox "Run Stopped with water balance error >0.003. BALERD = " & BALERD
#   GoTo endrun
#End If
STORD = INTR + INTS + SNOW + SWAT + GWAT

#daily output, graphs only when requested
#If outselect(3) And IDAY& > INIDAYS& Then Call doutput
#If frmmainB90!chkgraph Then Call subgraph(FG(gnumber%), 2)
#
#flows accumulated over month
maccum()
#
#If frmmainB90.chkdates Then
#date checking on
if(DOM == DAYMO[MONTHN]){
#  end of month
#  flows for month summed from components
#msum()
#  monthly output
#If outselect(2) And IDAY& > INIDAYS& Then Call moutput
#If frmmainB90!chkgraph Then Call subgraph(FG(gnumber%), 2)
#  flows accumulated over year
#yaccum()
#  set up for next month
zmonth()
MONTHN = MONTHN + 1
DOM = 0
NITSM = 0
}  #for end of month
if (MONTHN == 13) {
    #  end of year
    #  flows for year summed from components
 # ysum()
#  annual output
#If outselect(1) And IDAY& > INIDAYS& Then Call youtput
#If frmmainB90!chkgraph Then
#graphon% = False
#End If
#  set up for next year
  MONTHN = 1
  DOM = 1
  DOY = 1
  YEARN = YEARN + 1
  zyear()
  if (fnleap() ){
    DAYMO[2] = 29
  }else{
    DAYMO[2] = 28
  }
NITSY = 0
NITSM = 0
} 

#for end of year
#If EOF(1) Then # End of data file with IDAY <= NDAYS
#rstop% = 2  # will disable runcontinue
#GoTo endrun
#End If
#set up for next day
#DOM = DOM + 1
#DOY = DOY + 1
IDAY = IDAY + 1

  MONTHN = as.numeric(MData[[2]][IDAY])
  DOM = as.numeric(MData[[3]][IDAY])
  YEARN = as.numeric(MData[[1]][IDAY])
  #NPINT =1
  if(IDAY <= NDAYS)
  DOY=DOYF(DOM,MONTHN,DAYMO)
 
#* * * I N P U T   W E A T H E R   L I N E   F R O M   D F I L E * * *
#subdatafileline()
#Call subcheckdata

#
# ***************   E N D    D A Y   L O O P    **************************
#

#


	
  timeseries_prec[daymax-NDAYS+IDAY-1]<-PRECD
  timeseries_evp[daymax-NDAYS+IDAY-1]<-EVAPD
  timeseries_flow[daymax-NDAYS+IDAY-1]<-FLOWD
  timeseries_rnet[daymax-NDAYS+IDAY-1]<-RNET
  timeseries_irvp[daymax-NDAYS+IDAY-1]<-IRVPD
  timeseries_isvp[daymax-NDAYS+IDAY-1]<-ISVPD
  timeseries_ptran[daymax-NDAYS+IDAY-1]<-PTRAND
  timeseries_snow[daymax-NDAYS+IDAY-1]<-SNOW
  timeseries_swat[daymax-NDAYS+IDAY-1]<-SWAT
  timeseries_pint[daymax-NDAYS+IDAY-1]<-PINTD
  timeseries_snvp[daymax-NDAYS+IDAY-1]<-SNVPD 
  timeseries_slvp[daymax-NDAYS+IDAY-1]<-SLVPD 
  timeseries_trand[daymax-NDAYS+IDAY-1]<-TRAND
  timeseries_mesfld[daymax-NDAYS+IDAY-1]<-MESFLD
  timeseries_smltd[daymax-NDAYS+IDAY-1]<-SMLTD
  timeseries_slfld[daymax-NDAYS+IDAY-1]<-SLFLD
  timeseries_rfald[daymax-NDAYS+IDAY-1]<-RFALD
  timeseries_awat[daymax-NDAYS+IDAY-1]<-AWAT
  timeseries_adef[daymax-NDAYS+IDAY-1]<-ADEF
  timeseries_sintd[daymax-NDAYS+IDAY-1]<-SINTD
  timeseries_rintd[daymax-NDAYS+IDAY-1]<-RINTD
  timeseries_sfald[daymax-NDAYS+IDAY-1]<-SFALD
  timeseries_rthrd[daymax-NDAYS+IDAY-1]<-RTHRD
  timeseries_sthrd[daymax-NDAYS+IDAY-1]<-STHRD
  timeseries_rsnod[daymax-NDAYS+IDAY-1]<-RSNOD
	
	
	
} #End B90

#png(file="c:\\Users\\LM\\Documents\\BROOK90\\Documentation\\Documentation\\Plot_output\\model_results.png")

plot(1:NDAYS,timeseries_flow[1:NDAYS],col="red",type="l",lwd=3,ylim=c(0.0, 50),xlab="Tage [d]",ylab="Werte [mm/d]")#,xlim=c(IDAY,NDAYS+1))

PrecBound<-10 #mm
#points(IDAY,PTRAN,col="black",pch="?")
points( which(timeseries_prec %in% timeseries_prec[timeseries_prec>PrecBound]),timeseries_prec[timeseries_prec>PrecBound],col="blue",pch="*")
#line(IDAY,EVAPD,col="green",pch="*")
#	points(IDAY,TA,col="red",pch="?")
#points(IDAY,ATR[1],col="red",pch="?")
#points(IDAY,GER[1],col="green",pch="?")
#points(IDAY,PTR[1],col="black",pch="?")
lines(1:NDAYS,timeseries_evp,col="blue",lwd=3)
lines(1:NDAYS,timeseries_mesfld,col="darkgreen",lwd=2)
lines(1:NDAYS,timeseries_flow,col="red",lwd=3)
#	points(IDAY,RNET,col="brown",pch="?")
legend("topright",
       inset=c(-0.2,0),
       xpd=TRUE,
       legend=c("Simulated flow [mm/d]","Observed precipitation [mm/d]","Simulated evapotranspiration [mm/d]","Observed flow [mm/d]"),
       col=c("red","blue","blue","darkgreen"),
       pch=c(NA,"*",NA,NA),
       lty=c("solid",NA,"solid","solid"),
       pt.cex=1, 
       cex=0.7,
       y.intersp = 0.5,
       bty="n")
dev.copy(png,'model_results.png')
dev.off()