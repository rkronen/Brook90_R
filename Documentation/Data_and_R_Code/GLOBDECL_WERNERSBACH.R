
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
#Parameter from files
#
##CTemperateDeciduousForest.txt
#SCscl.txt
#LMcu.txt
#IDefault.txt
#Xdefault.txt
#FUni50.txt

#
#   * indicates an input from the parameter file
#  ** indicates a variable in the data file or precip input file
# *** indicates a constant
#**** indicates a variable initialized or parameter set in the data file
#   & indicates a variable initialized in the parameter file
#   @ indicates a calculated parameter (constant for run)
#     all others are variables
# at end of variable name indicates integer variable
#& at end of variable name indicates long integer variable
#$ at end of variable name indicates string variable
#xxxxP      flow for precipitation interval, mm
#xxxxD      flow for day, mm
#xxxxM      flow for month, mm
#xxxxY      flow for year, mm



ML <- 25    #number of values in layer parameter arrays, set to 25 for B90V4
gctMaxoutvars <- 60 # maximum number of output variables, an array dimension
gctGenoutv <- 19 # number of variables in GENOUT form
gctIntoutv <- 22 # number of variables in INTRNOUT form
gctLayoutv <- 12 # number of variables in LAYOUT form
gvals <- 8 # max number of graph output values
maxgraphs <- 100
#
#A <- numeric(ML) # parsed values from INSTRNG
#aparsed  # parsed values from gsbparsestring, first subscript is 0
#bad      # error indicator passed back to calling routine
#B90path$  # path for running program, used for .INI and .TMP
#cancelsave As Boolean #true if save or print should be cancelled
#chkout(1 To gctMaxoutvars, 1 To 5) As Integer #1 for output, 5 intervals
#        indicates which variables/interval selected
#        stored in .INI
#commadec As Boolean
#dfiletype    #data file type, 1-csv,2-decimal period, 3-decimal comma
dum   <-0   # dummy variable
errnum <-0   # error number from ERR
#EV(1 To 25) As New frmevalout # EVAL windows
evalnumber <-0 # number of EVAL screens
#FG(1 To maxgraphs) As New frmgraph  # multiple instances of graph
#gnumber   # graph number
#graphon    # true if graph has been initialized
INIDAYS<-0  # number of days to initialize run
#inoutdir$  # latest input-output directory
#intrvl     #output interval index, 1 ANN, 2 MON, 3 DAY, 4 PRE, 5 ITR
#ivar(1 To gctMaxoutvars), pvar(1 To gctMaxoutvars), dvar(1 To gctMaxoutvars), mvar(1 To gctMaxoutvars), yvar(1 To gctMaxoutvars) # list of var numbers for output, does not count mulltiple soil layers
lstart <-0# starting output layer
lend <-0  #ending output layer
lstep <-0 #step for output layers
#msg As String#
#noenderror As Integer  # 0 if no END in parameter file, -1 if END is found
#noruncontinue #1 to prevent Run-Continue after soil parameter changes
#NLN As String  # new line
#outfilestatus(1 To 5) As Integer  #0 if no output, 1 if output, subscript 1 ann, 2 mon, 3 day, 4 pre, 5 itr
##        indicates presence of .TMP output files, controls mnuview???.Enabled
#outselect(1 To 5)  As Integer # True(-1) if output selected for that time interval, False(0) if not
#        controls checking in Select Output menu
parserr <-0  #true if parse error in Soil parameters
prfiletype  <-0  #precip file type, 1-csv,2-decimal period, 3-decimal comma
rerunnumber<-0 # 0 for new run, 1 to 9 for each successive add run
rstop <-0 # 1 if run should stop at end of day, 2 if EOF and date check on, 3 if crash error in run or immediate stop request, 4 if max number of graphs
RUNDAYS<-0  # number of output days to run after initialize
runflag <-0# 0 for new run, 1 for add run, 2 for continue run
#strng$
#Title$(1 To gctMaxoutvars)   # column titles for gctMaxoutvars variables
#txt$(1 To 30) # text strings for help
#userfont As String
#varno # output variable number, from 1 to gctMaxoutvars, always locally used
#yvars, mvars, dvars, pvars, ivars  # number of vars/cols in output

#DT  time step for DFILE interval,  must be 1 d
DT <- 1
#WTOMJ  (MJ m-2 d-1)/(watt/m2) = 86400 s/d * .000001 MJ/J
WTOMJ <- 0.0864
#ETOM  (mm water)/(MJ/m2) using Lv 2448 MJ/Mg and density of water 1 Mg/m3
#= 1E3 mm/m / (2448 MJ/Mg * 1 Mg/m3)
ETOM <- 0.4085
#CPRHO - volumetric heat capacity of air, J m-3 K-1)
CPRHO <- 1240#
#GAMMA - psychrometer constant, kPa/K
GAMMA <- 0.067
#CVLQ - volumetric heat capacity of water, MJ m-2 mm-1 K-1
CVLQ <- 0.00418
#CVICE - volumetric heat capacity of ice, MJ m-2 mm-1 K-1
CVICE <- 0.00192
#LF  heat of fusion of water, MJ m-2 mm-1
LF <- 0.335
#LS  latent heat of sublimation of snow, MJ m-2 mm-1
LS <- 2.824
#RHOWG  density of water times gravity acceleration, MPa/m or kPa/mm
RHOWG <- 0.00981
#SIGMA  Stefan-Boltzmann constant, W m-2 K-4)
SIGMA <- 0.0000000567
#SC  solar constant, value from Lean (1991), W/m2
SC <- 1367#
#K  vonKarman constant
K <- 0.4
#PI  pi
PI <- 3.1416

AA  <-0              # average available energy over daytime or nighttime, W/m2
ADEF<-0              # available water deficit in root zone, mm, output only
ALB   <-0.07            # * albedo with no snow
ALBEDO<-0            # albedo
ALBSN <-0.3            # * albedo with snow on the ground
ALPHA<-numeric(ML)   # modified Cowan alpha, MPa
ASPECT  <-0          # aspect, radians through east from north
ASPECTD <-0          # * aspect, degrees through east from north
ASUBS   <-0          # average avail. energy at ground over day or night, W/m2
ATR<-numeric(2)           #actual transpiration rate for daytime or night, mm/d
ATRANI<-numeric(ML)  #actual transp.rate from layer for daytime or night,mm/d
ATRI<-matrix(0,2,ML) #actual transp.rate from layer for daytime and night,mm/d
AWAT    <-0          # available soil water in root zone, mm, output only
BALERD<-0 
BALERM<-0  
BALERY<-0 
# error in water balance. mm
BEXP<-rep(0,ML)    #* exponent for psi-theta relation
BEXP[1]<-5.37433
BEXP[2]<-4.03320
BEXP[3]<-5.64096
BEXP[4]<-6
BEXP[5]<-5
BYFL   <-0           #bypass flow rate from all layers for iteration, mm/d
BYFLI<-numeric(ML)   #bypass flow rate from layer, mm/d
BYFLPI<-numeric(ML)
BYFLDI<-numeric(ML)
BYFLMI<-numeric(ML)
BYFLYI<-numeric(ML)
#                         bypass flow from layer, mm
BYFLP<-rep(0,ML)
BYFLD<-rep(0,ML) 
BYFLM<-rep(0,ML)  
BYFLY<-rep(0,ML)  # bypass flow, mm
BYFRAC<-numeric(ML)  #fraction of layer infiltration to bypass flow
BYPAR<-1             #* 1 to allow BYFL, or 0 to prevent BYFL
#C    <-0             #dummy input string
C1    <-0.25             #* intercept of relation of solar rad. to sunshine duration
C2    <-0.5             #* slope of relation of solar radiation to sunshine duration
C3    <-0.2             #* longwave correction factor for overcast sky
CC      <-0           #cold content of snowpack (positive), MJ m-2
CCFAC  <-0.3            #* cold content factor, MJ m-2 d-1 K-1
CHM<-numeric(ML)     #@ Clapp-Hornberger m parameter, kPa
CHN<-numeric(ML)     #@ Clapp-Hornberger n parameter
CINTRL<-0.15             #* maximum interception storage of rain per unit LAI, mm
CINTRS<-0.15             #* maximum interception storage of rain per unit SAI, mm
CINTSL<-0.6             #* maximum interception storage of snow per unit LAI, mm
CINTSS<-0.6             #* maximum interception storage of snow per unit SAI, mm
CR   <-0.5              #* light extinction coefficient for projected LAI + SAI
CS  <-0.035               #* ratio of projected SAI to canopy height, m-1
CVPD <-2              #* vapor pressure deficit at which conductance is halved, kPa
CZR  <-0.05              #* ratio of roughness to HEIGHT for rough closed canopies
CZS <-0.13               #* ratio of roughness to HEIGHT for smooth closed canopies
DAYLEN <-0            #daylength in fraction of day, d-1
DAYMO<-numeric(12)        #* days in month
DD  <-0              #** day of the month from data files
DELTA <-0             #dES/dT at some temperature T, kPa/K
DENSEF <-0.8580571            #* density or thinning multiplier for MAXLAI,CS,RTLEN,RPLANT, not <.001
DISP  <-0             #zero-plane displacement, m
DISPC  <-0            #zero-plane displacement for closed canopy of HEIGHT, m
DOM  <-0             #day of month
DOY  <-0             #**** first day of the year from DFILE header
DPSIDW<-numeric(ML)  #rate of change of total potential with watercontent,kPa/mm
DPSIMX  <-0.01           #* maximum potential difference considered equal, kPa
DRAIN  <-0.626259            #* multiplier of VFLUX(n) for drainage to groundwater
DSFL  <-0             #downslope flow rate from all layers for iteration, mm/d
DSFLI<-numeric(ML)   #downslope flow rate from layer, mm/d
DSFLP<-0 
DSFLD<-0 
DSFLM<-0 
DSFLY<-0   #downslope flow, mm
DSFLPI<-numeric(ML)
DSFLDI<-numeric(ML)
DSFLMI<-numeric(ML)
DSFLYI<-numeric(ML)
#                        #downslope drainage from layer, mm
DSLOPE  <-0           #slope for DSFL, radians
DSLOPED  <-0           #* slope for DSFL, degrees
DSWMAX   <-2          #* maximum change allowed in SWATI, percent of SWATMX(i)
DTI   <-0             #time step for iteration interval, d
DTIMAX  <-0.5           #* maximum iteration time step, d
DTINEW  <-0.0           #second estimate of DTI
DTP    <-1            #@ time step for precipitation interval, may be <= 1 d
DTRI  <-0             #time remaining in precipitation interval, d
DUMM<-rep(0,gctMaxoutvars)   #dummy array for subroutine calls
dummy   <-0           # dummy variable for subroutine calls
DURATN<-c(4,4,4,4,4,4,4,4,4,4,4,4)   #* average duration of daily precip by month, hr
EA    <-0             #** vapor pressure for the day, kPa
ES   <-0              #saturated vapor pressure, kPa
ESLOPE  <-0           #slope for evapotranspiration and snowmelt, radians
ESLOPED  <-2          #* slope for evapotranspiration and snowmelt, degrees
EVAPP<-0 
EVAPD<-0  
EVAPM<-0  
EVAPY <-0  #evapotranspiration
FARR<-rep(0,366)    #array of simulated daily flow for statistics
FETCH   <-5000           #* weather station fetch, m"
FLOWP<-0 
FLOWD<-0 
FLOWM<-0 
FLOWY<-0   #total flow
FRINTL  <-0.06           #* intercepted fraction of rain per unit LAI
FRINTS  <-0.06           #* intercepted fraction of rain per unit SAI
FSINTL  <-0.04           #* intercepted fraction of snow per unit LAI
FSINTS  <-0.04           #* intercepted fraction of snow per unit SAI
FXYLEM  <-0.5           #* fraction of plant resistance in xylem
GER<-numeric(2)            #ground evaporation rate for daytime or night, mm/d
GEVP  <-0             #average ground evaporation for day, mm/d
GIR<-numeric(2)            #ground evap. rate with intercep. for daytime or night,mm/d
GIVP <-0              #average ground evaporation for day with interception, mm/d
GLMAX  <-0            # maximum leaf conductance, m/s
GLMAXC  <-0.494377           # * maximum leaf conductance, cm/s
GLMIN <-0            # minimum leaf conductance, m/s
GLMINC  <-0.03           # * minimum leaf conductance, cm/s
GRAPH <-0            #* runtime graphics output, 0-none, 1-continuous, 2-pause
GRDMLT <-0.35            #* rate of groundmelt of snowpack, mm/d
GSC   <-0             #* discharge from GWAT, fraction per day, d-1
GSP  <-0.085              #* fraction of discharge to seepage
GWAT   <-0            #groundwater storage below soil layers, mm
GWATIN <-20            #**** initial groundwater storage below soil layers, mm
GWFL  <-0             #streamflow rate from groundwater, mm/d
GWFLP<-0 
GWFLD<-0 
GWFLM<-0 
GWFLY<-0   #groundwater flow, mm
mnuhalfiter<-FALSE
HEIGHT  <-0           #canopy height, m
HR   <-10              #* height above which CZR applies, m
HS    <-1             #* height below which CZS applies, m
#I    <-0             #index variable for layer number
I0HDAY <-0            #potential insolation on horizontal, MJ m-2 d-1
IDAY<-0             #day number in run
II <-0               #** input precipitation interval number
ILAYER <-0           # number of layers over which infiltration is distributed
IDEPTH <-1000            #* depth over which infiltration is distributed

IMPERV  <-0.025           #* impervious fraction of area for SRFL
INFEXP  <-0.8797636           #* infiltration exponent, 0-all to top to 1-uniform with depth
INFRAC<-rep(0,ML)  #@ fraction of infiltration to each layer
INFLI<-rep(0,ML)  #infiltration rate into layer, mm/d
INFLP<-rep(0,ML)
INFLD<-rep(0,ML)
INFLM<-rep(0,ML)
INFLY<-rep(0,ML)  # infiltration into soil matrix, mm
INFLPI<-rep(0,ML)
INFLDI<-rep(0,ML)
INFLMI<-rep(0,ML)
INFLYI<-rep(0,ML)
#                         infiltration to layer, mm
INTR   <-0            #intercepted rain, mm
INTRIN  <-0           #& initial intercepted rain, mm
INTS   <-0            #intercepted snow, mm
INTSIN <-0            #& initial intercepted snow, mm
IRVP <-0              #evaporation rate of intercepted rain, mm/d
IRVPD<-0 
IRVPM<-0 
IRVPY <-0             #evaporation of intercepted rain, mm
ISVP  <-0             #evaporation rate of intercepted snow, mm/d
ISVPD<-0 
ISVPM<-0 
ISVPY<-0   #evaporation of intercepted snow, mm
J   <-0              #index variable for day-night separation
KF<-rep(0,ML)      #* hydraulic conductivity at field capacity, mm/d
KF[1]<-6.9
KF[2]<-2.7
KF[3]<-2.9
KF[4]<-1
KF[5]<-3.5
KK<-rep(0,ML)      #hydraulic conductivity, mm/d
KSAT<-rep(0,ML)    #@ saturated hydraulic conductivity, mm/d
KSNVP <-0.009734             #* multiplier to fix snow evaporation problem
L1  <-0               #@ latitude of equivalent slope, radians
L2  <-0               #@ time shift of equivalent slope, radians
LAI  <-0              #leaf area index, m2/m2
LAIMLT <-0.2            #* parameter for snowmelt dependence on LAI, Globalensionless
LAT   <-0             #latitude, radians
LATD <-50.5              #**** latitude, degrees
LENGTH  <-0           #* slope length for DSFL, m
LPC  <-4              #* minimum LAI defining a closed canopy
LWIDTH    <-0.004         #* leaf width, m
MARR<-c(seq(1,366,1))    #array of measured daily flow for statistics
MAXHT<-25              #* maximum height for the year, m
MAXLAI  <-7.693270           #* maximum projected leaf area index for the year, m2/m2
MAXLQF <-0.05            #* maximum liquid water fraction of SNOW, Globalensionless
MELFAC <-1.728930            #* degree day melt factor for open, MJ m-2 d-1 K-1
MESFL <-0            #** measured streamflow for day, mm
MESFLD<-0
MESFLM<-0
MESFLY<-0 # measured streamflow, mm
MESFLP <-0           #** measured streamflow rate for precip interval, mm/d
MM    <-0           #** month from data files
MONTHN <-0          #month number
MXKPL  <-7.03463           #* maximum plant conductivity, (mm/d)/MPa
MXRTLN <-3000.001           #* maximum root length per unit land area, m/m2
N   <-0             #index variable for precipitation interval
NN  <-2.5              #* wind/diffusivity extinction coefficient
NDAYS<-0            #* number of days in run
NITS<-0             #number of iterations in precipitation interval
NITSD<-0            #total number of iterations for day
NITSM<-0            #total number of iterations for month
NITSY<-0            #total number of iterations for year
NITSR<-0            #total number of iterations for run
NLAYER  <-5         #* number of soil layers to be used in model, <= ML
NOOUTF <-1          #* 1 if no outflow allowed from roots, otherwise 0

NTFLI<-rep(0,ML)   #net flow rate into layer, mm/d
NTFLPI<-rep(0,ML)
NTFLDI<-rep(0,ML)
NTFLMI<-rep(0,ML)
NTFLYI<-rep(0,ML)
#                         net flow into layer, mm
PINT <-0             #average potential interception for day, mm/d
PINTD<-0 
PINTM<-0 
PINTY <-0 #  potential interception, mm
PIR<-c(0,0)            #potential interception rate for daytime or night, mm/d
PREC  <-0            #precipitation rate, mm/d
PRECD<-0 
PRECM<-0 
PRECY<-0             #  precipitation, mm
PREINT  <-0          #** precipitation for precipitation interval, mm
PRECIN  <-0          #** daily precipitation, mm
PSICR  <--2           #* minimum plant leaf water potential, MPa
PSIF<-rep(0,ML)    #* matric potential at field capacity, kPa
PSIF[1]<--11.818
PSIF[2]<--11.516 
PSIF[3]<--10.22
PSIF[4]<--10
PSIF[5]<--10
PSIG<-rep(0,ML)      #@ gravity potential, kPa
PSIM<-rep(0,ML)      #& matric soil water potential for layer, kPa
PSIMIN<-rep(-10,ML)    # initial PSIM()
PSITI<-rep(0,ML)     #total potential, kPa
PSNVP <-0            #potential snow evaporation, mm/d
PTR<-c(0,0)          #potential transpiration rate for daytime or night, mm/d
PTRAN  <-0           #average potential transpiration rate for day, mm/d
PTRAND<-0 
PTRANM<-0 
PTRANY<-0            # potential transpiration, mm
QFFC<-0.00104          #  0.00104  #* quick flow fraction (SRFL or BYFL) at field capacity
QFPAR <-0.834524        #0.834524    #* quick flow parameter (SRFL or BYFL)
QLAYER <-1          # number of soil layers for SRFL, 0 to prevent SRFL
QDEPTH <-0.1           #* soil depth for SRFL calculation, 0 to prevent SRFL
R5    <-100            #* solar radiation at which conductance is halved, W/m2
RAA    <-0           #Shuttleworth-Wallace atmosphere aerodynamic resistance,s/m
RAC   <-0            #Shuttleworth-Wallace canopy aerodynamic resistance, s/m
RAS  <-0            #Shuttleworth-Wallace ground aerodynamic resistance, s/m
RELHT<-c(1,1,366,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)    #* ten pairs of DOY and relative canopy height
RELLAI<-c(1,1,366,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)   #* ten pairs of DOY and relative LAI
ROOTDEN<-c(50,1,50,1,50,1,50,1,50,1,50,.3,50,.2,50,.1,50,.1,50,.1,50,0.0,50,0.0,50,0,50,0,50,0,50,0,50,0,50,0,50,0,50,0,50,0,50,0,50,0,50,0,50,0)  #* 25 pairs of root layer thickness (mm) and relative root density per unit volume
RELDEN<-rep(0,ML)  # relative root density per unit volume for soil layers
RFAL   <-0           #rainfall rate, mm/d
RFALD<-0
RFALM<-0
RFALY <-0            #  rainfall, mm
RHOTP <-2            #* ratio of total leaf area to projected area
RINT  <-0            #rainfall catch rate, mm/d
RINTD<-0
RINTM<-0
RINTY<-0           #  rain interception, mm
RM <-1000               #* maximum solar radiation, at which FR = 1, W/m2
RNET   <-0           #rain reaching soil surface, mm/d
RNETD<-0
RNETM<-0 
RNETY <-0            #  rainfall to soil surface, mm
RPLANT<-0            #plant resistivity to water flow, MPa d/mm
RROOTI<-rep(0,ML)    #root resistance for layer, MPa d/mm
RTHR   <-0           #rain throughfall rate, mm/d
RTHRD<-0
RTHRM<-0
RTHRY<-0             #  rain throughfall, mm
RTLEN <-0            #root length per unit land area, m/m2
RTRAD <-0.35            #* average root radius, mm
RRD<-0.55           #Default Radiation Ratio
RSC <-0              #Shuttleworth-Wallace canopy surface resistance, s/m
RSNO <-0             #rain added to snowpack, mm/d
RSNOD<-0
RSNOM<-0
RSNOY<-0            #  rain on snow, mm
RSS<-0              #Shuttleworth-Wallace soil surface resistance, s/m
RSSA <-500             #* soil evaporation resistance at field capacity, s/m
RSSB <-1             #* exponent in relation of soil evap res to water potential
RSTEMP<- -1.29978         #* base temperature for snow-rain transition, ?C
RXYLEM <-0           #xylem resistance, MPa d/mm
SAFRAC <-0           #source area fraction
SAI <-0              #stem area index, m2/m2
SAIMLT <-0.5           #* parameter for snowmelt dependence on SAI, Globalensionless
SEEP <-0             #deep seepage loss from groundwater, mm/d
SEEPP<-0
SEEPD<-0
SEEPM<-0
SEEPY<-0             # seepage loss, mm
SFAL <-0           #snowfall rate, mm/d
SFALD<-0
SFALM<-0
SFALY<-0             #  snowfall, mm
SHEAT<-0            #@ average soil heat flux for the day, W/m2, fixed at 0
SINT <-0             #snowfall catch rate, mm/d
SINTD<-0 
SINTM<-0
SINTY <-0# snow interception, mm
SLFDAY  <-0          #ratio of potential insolation on slope to horizontal, map area
SLFL  <-0            #input rate to soil surface, mm/d
SLFLP<-0#rep(0,ML)
SLFLD<-0#rep(0,ML)
SLFLM<-0#rep(0,ML)
SLFLY<-0#rep(0,ML)# input to soil surface, mm
SLFLI<-rep(0,ML)   #macropore infiltration rate down from layer, mm/d
SLFLPI<-rep(0,ML)
SLFLDI<-rep(0,ML)
SLFLMI<-rep(0,ML)
SLFLYI<-rep(0,ML)
#                         vertical macropore infiltration from layer, mm
SLRAD <-0            #average solar radiation on slope over daytime, W/m2
SLRADd<-0
SLVP <-0             #evaporation rate from soil, mm/d
SLVPD<-0
SLVPM<-0
SLVPY<-0             # soil evaporation, mm
SMLT  <-0            #melt drainage rate from snowpack, mm/d
SMLTD<-0
SMLTM<-0 
SMLTY <-0            # snowmelt, mm
SNODEN <-0.3         #* snow density, mm/mm
SNOEN <-0            #energy flux density to snow surface, MJ m-2 mm-1 d-1
SNOFRC <-0           #fraction of precipitation for the day as snow, unitless
SNOW <-0             #water equivalent of snow on the ground, mm
SNOWIN  <-20          #**** initial water equivalent of snow on the ground, mm
SNOWLQ<-0            #liquid water content of snow on the ground, mm
SNVP  <-0            #evaporation rate from snowpack, mm/d
SNVPD<-0
SNVPM<-0
SNVPY <-0            # evaporation from snowpack, mm
SOLRAD <-0           #** solar radiation for the day, horizontal surface, MJ/m2
SOLRADC<-0           #SOLRAD as corrected if necessary by WEATHER routine, MJ/m2
SRFL <-0             #source area flow rate, mm/d
SRFLP<-0
SRFLD<-0
SRFLM<-0
SRFLY <-0# source area flow, mm
STHR <-0             #snow throughfall rate, mm/d
STHRD<-0
STHRM<-0
STHRY<-0 # snow throughfall, mm
STONEF<-rep(0.00,ML) #* stone volume fraction, unitless
STONEF[1]<-0.02
STONEF[2]<-0.2
STONEF[3]<-0.25
STONEF[4]<-0.25
STONEF[5]<-0.7
STORD<-0
STORM<-0
STORY<-0 # total water storage in system, mm
STRES<-0             #TRAN / PTRAN for time period
STRX<-0             #string variable to trap q or esc
SWAT<-0    #total soil water in all layers, mm
SWATI<-rep(0,ML)  #water volume in layer, mm
SWATMX<-rep(0,ML)  #maximum water storage for layer, mm
SWATQF   <-0         #@water storage at field capacity for layers 1 to QLAYER,mm
SWATQX <-0           #@ maximum water storage for layers 1 to QLAYER, mm
T1    <-10            #* lowest temp. at which stomates not temp. limited, degC
T2    <-30            #* highest temp. at which stomates not temp. limited,degC
TA   <-40             #mean temperature for the day at reference height, degC
TADTM <-0            #average daytime temperature at reference height, degC
TAJ  <-0             #TADTM or TANTM depending on J
TANTM <-0            #average nighttime temperature at reference height, degC
TEMP  <-0           #temporary integer variable - apparently no longer used
TH   <-40             #* temperature above which stomates are closed, degC
THETA<-rep(0,ML)   #water content, mm water / mm soil matrix
THETAF<-rep(0,ML) #* volumetric water content at field capacity
THETAF[1]<-0.34062341
THETAF[2]<-0.39705807  
THETAF[3]<-0.24359704
THETAF[4]<-0.35
THETAF[5]<-0.23
THICK<-rep(0,ML)   #* layer thicknesses, mm
THICK[1]<-50
THICK[2]<-250
THICK[3]<-300
THICK[4]<-300
THICK[5]<-100
THSAT<-rep(0,ML)  #* theta at saturation, matrix porosity
THSAT[1]<-0.6313342
THSAT[2]<-0.6189386
THSAT[3]<-0.4930716
THSAT[4]<-0.680
THSAT[5]<-0.600

TL    <-0            #* temperature below which stomates are closed, degC
TMAX  <-0            #** maximum temperature for the day, degC
TMIN <-0             #** minimum temperature for the day, degC
TRANI<-rep(0,ML)   #average transpiration rate from layer, mm/d
TRANP<-0
TRAND<-0
TRANM<-0
TRANY<-0             # transpiration, mm
TRANPI<-rep(0,ML)
TRANDI<-rep(0,ML)
TRANMI<-rep(0,ML) 
TRANYI<-rep(0,ML)
#                        #layer transpiration, mm
TSNOW  <-0           #snowpack temperature (isothermal assumed), degC
UA     <-0           #average wind speed for the day at reference height, m/s
UADTM  <-0           #average wind speed for daytime at reference height, m/s
UAJ   <-0            #UADTN or UANTM depending on J
UANTM <-0            #average wind speed for nighttime at reference height, m/s
UW   <-0             #** average wind speed for day at weather station, m/s
VPD  <-0             #vapor pressure deficit at reference height, kPa
VRFLI<-rep(0,ML)   #vertical matrix drainage rate from layer, mm/d
VRFLPI<-rep(0,ML)
VRFLDI<-rep(0,ML)
VRFLMI<-rep(0,ML)
VRFLYI<-rep(0,ML)
#                         vertical matrix drainage from layer, mm
VV<-rep(0,ML)      #temporary VRFLI
WETC<-rep(0,ML)    #@ wetness at PSICR, Globalensionless
WETF<-rep(0,ML)   #@ wetness at field capacity, Globalensionless
WETFR  <-0           #fraction of precipitation interval that canopy is wet
WETINF<-rep(0,ML)  #* wetness at dry end of near-saturation range
WETINF[1]<-0.92
WETINF[2]<-0.92
WETINF[3]<-0.92
WETINF[4]<-0.92
WETINF[5]<-0.92
WETNES<-rep(0,ML)  #wetness, fraction of saturation
WNDRAT <-.3           #* ratio of nighttime to daytime wind speed
XMAX  <-0            #maximum value for x-axis
XMIN  <-0            #minimum value for x-axis
YEARN <-0           #**** first year from DFILE header
YMAX<-numeric(gvals)      #* maximum value for y-axis
YMIN<-numeric(gvals)      #@ minimum value for y-axis
YNAME<-numeric(gvals)    #@ variable name
YVAL<-numeric(gvals)      #plot value for y-axis
YY   <-0            #** year from data files - used only to check dates and to determine end of year for output
Z0   <-0             #roughness parameter, m
Z0C  <-0             #roughness parameter for closed canopy of HEIGHT, m
Z0G  <-0.02             #* ground surface roughness, m
Z0GS  <-0            #ground or snow surface roughness, m
Z0S  <-0.001             #* snow surface roughness, m
Z0W  <-2.8             #* weather station roughness parameter, m
ZA   <-0             #reference height for TA, EA, UA, above ground, m
ZMINH <-2            #* ZA minus HEIGHT, reference height above canopy top, m
ZW  <-14              #* weather station measurement height for wind, m"

