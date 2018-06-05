
#   Code by R.Kronenberg as RK [06112017]  based on Brook90 by Federer et.al
#   modified RK [28102018]
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
rm(list = setdiff(ls(), lsf.str())) #removes all objects except for functions

#Start Brook90
##
#
#
#
#
#Load all Variables

NPINT <-1#1           #**** number of precipitation intervals per day  1=1 Tag    24 = 24 Stunden pro Tag
SUBDAYDATA<-FALSE#TRUE#TRUE
RRD <- 0.55  # default ratio of solar radiation to potential
UWD <- 3.0  # default wind speed at weather station



SCRIPTPATH<-"c:\\Users\\LM\\Documents\\BROOK90\\Documentation\\Data_and_R_Code\\"

#source(file.path(SCRIPTPATH,"GLOBDECL_Parameters_WERNERSBACH.R"))
source(file.path(SCRIPTPATH,"GLOBDECL_WERNERSBACH.R"))
source(file.path(SCRIPTPATH,"AXX.R"))
source(file.path(SCRIPTPATH,"KPT.R"))
source(file.path(SCRIPTPATH,"EVP.R"))
source(file.path(SCRIPTPATH,"PET.R"))
source(file.path(SCRIPTPATH,"SUN.R"))
source(file.path(SCRIPTPATH,"WAT.R"))
source(file.path(SCRIPTPATH,"SNO.R"))
source(file.path(SCRIPTPATH,"B90V4_sub.R"))


#meteoFile <- read.fwf(file.path(SCRIPTPATH,"data_input_WB_1970_2016.txt", widths=c(4, 5, 5, 8, 6, 7, 7, 6, 7,8))
meteoFile <-read.table(file.path(SCRIPTPATH,"data_input_WB_1999_point.txt"))
MData<-matrix(meteoFile)

precFile <-read.table(file.path(SCRIPTPATH,"P_WBG_0060_Minuten_1999_point.txt"))
MhhData<-matrix(precFile)
