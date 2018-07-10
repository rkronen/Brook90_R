
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



SCRIPTPATH<-"c:\\Users\\LM\\Documents\\BROOK90\\Documentation\\R_Code\\"

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


Input_data<-"c:\\Users\\LM\\Documents\\BROOK90\\Documentation\\Input_data"

meteoFile <-read.table(file.path(Input_data,"data_input_WB_1999_point.txt"))
MData<-matrix(meteoFile)

precFile <-read.table(file.path(Input_data,"P_WBG_0060_Minuten_1999_point.txt"))
MhhData<-matrix(precFile)

########################catchment parameters
#####canopy
canopy_file <-read.table(file.path(Input_data,"canopy.txt"), header = FALSE, fill=TRUE , dec = "." , sep = ",", strip.white = TRUE)

#canopy parameters
canopy_varname <- as.character(canopy_file[,1])
canopy_varvalue <- as.character(canopy_file [,2])
quant_canopy <- 18

for (i in 1:quant_canopy){
  a <- unlist(canopy_varname[i])
  b <- as.numeric(unlist(canopy_varvalue[i]))
  assign(a,b)
  rm(a,b)
}

#ROOTDEN
roota <- strsplit(canopy_varname[20],"[ ]+")
rootb <- unlist(roota)
rootc <- as.numeric(rootb)
rootaa <- strsplit(canopy_varname[22],"[ ]+")
rootbb <- unlist(rootaa)
rootcc <- as.numeric(rootbb)
root_a <- c(rootc,rootcc)
root_a[is.na(root_a)] <- 0

rootd <- strsplit(canopy_varname[21],"[ ]+")
roote <- unlist(rootd)
rootf <- as.numeric(roote)
rootdd <- strsplit(canopy_varname[23],"[ ]+")
rootee <- unlist(rootdd)
rootff <- as.numeric(rootee)
root_b <- c(rootf,rootff)
root_b[is.na(root_b)] <- 0

rootden <- as.vector(rbind(root_a,root_b)) 
assign(canopy_varname[19],rootden)

rm(roota,rootb,rootc,rootd,roote,rootf,rootaa,rootbb,rootcc,root_a,rootdd,rootee,rootff,root_b,quant_canopy,canopy_varvalue,canopy_varname,rootden)

#####fixed
fixed_file <-read.table(file.path(Input_data,"fixed.txt"), header = FALSE, fill=TRUE, sep = "," , dec = "." , row.names = 1)

fixed_varname <- rownames(fixed_file)
fixed_varvalue <- fixed_file [,c(1)]

quant_fixed = length(fixed_varname) - 1

for (i in 1:quant_fixed){
  a <- unlist(fixed_varname[i])
  b <- unlist(fixed_varvalue[i])
  assign(a,b)
  rm(a,b)
}

rm(fixed_varname,fixed_varvalue,quant_fixed)

#####flow_standard
flow_file <-read.table(file.path(Input_data,"flow_standard.txt"), header = FALSE, fill=TRUE, sep = "," , dec = "." , row.names = 1)

flow_varname <- rownames(flow_file)
flow_varvalue <- flow_file [,c(1)]

quant_flow = length(flow_varname) - 1

for (i in 1:quant_flow){
  a <- unlist(flow_varname[i])
  b <- unlist(flow_varvalue[i])
  assign(a,b)
  rm(a,b)
}

rm(flow_varname,flow_varvalue,quant_flow)

#####initial
initial_file <-read.table(file.path(Input_data,"initial.txt"), header = FALSE, fill=TRUE, sep = "," , dec = ".", row.names = 1)

initial_varname <- rownames(initial_file)
initial_varvalue <- as.character(initial_file [,1])
quant_initial <- 4

for (i in 1:quant_initial){
  a <- unlist(initial_varname[i])
  b <- as.numeric(unlist(initial_varvalue[i]))
  assign(a,b)
  rm(a,b)
}

#PSIMIN
PSIMIN <- as.numeric(initial_varvalue[6:30])

rm(initial_varname,initial_varvalue,quant_initial)

#####location
location_file <-read.table(file.path(Input_data,"location.txt"), header = FALSE, fill=TRUE, sep = "," , dec = ".", strip.white = TRUE)

location_varname <- as.character(location_file[,1])
location_varvalue <- as.character(location_file [,2])
quant_location <- 5

for (i in 1:quant_location){
  a <- unlist(location_varname[i])
  b <- as.numeric(unlist(location_varvalue[i]))
  assign(a,b)
  rm(a,b)
}

#DURATN
duratna <- strsplit(location_varvalue[6],"[ ]+")
duratnb <- unlist(duratna)
duratn<- as.numeric(duratnb)
assign(location_varname[6],duratn)

rm(duratna,duratnb,duratn)

#RELHT
hta <- strsplit(location_varvalue[7],"[ ]+")
htb<- unlist(hta)
ht_a <- suppressWarnings(as.numeric(htb))
ht_a[is.na(ht_a)] <- 0

htc <- strsplit(location_varname[8],"[ ]+")
htd <- unlist(htc)
ht_b <- suppressWarnings(as.numeric(htd))
ht_b[is.na(ht_b)] <- 0

relht <- as.vector(rbind(ht_a,ht_b)) 
assign(location_varname[7],relht)

rm(hta,htb,htc,htd,ht_a,ht_b,relht)

#RELLAI
laia <- strsplit(location_varvalue[9],"[ ]+")
laib<- unlist(laia)
lai_a <- suppressWarnings(as.numeric(laib))
lai_a[is.na(lai_a)] <- 0

laic <- strsplit(location_varname[10],"[ ]+")
laid <- unlist(laic)
lai_b <- suppressWarnings(as.numeric(laid))
lai_b[is.na(lai_b)] <- 0

rellai <- as.vector(rbind(lai_a,lai_b)) 
assign(location_varname[9],rellai)

rm(laia,laib,laic,laid,lai_a,lai_b,rellai)
rm(location_varname,location_varvalue,quant_location)

#####soil
soil_file <-read.table(file.path(Input_data,"soil.txt"), header = FALSE, fill=TRUE , dec = "." , sep = "", strip.white = TRUE)

soil_var1 <- as.character(soil_file[,1])
soil_var2 <- as.character(soil_file[,2])
soil_var3 <- as.character(soil_file[,3])
soil_var4 <- as.character(soil_file[,4])
soil_var5 <- as.character(soil_file[,5])
soil_var6 <- as.character(soil_file[,6])
soil_var7 <- as.character(soil_file[,7])
soil_var8 <- as.character(soil_file[,8])

# NLAYER
a_nlayer <- unlist(soil_var1[1])
a_nlayer <- as.character(gsub(",","",a_nlayer))
b_nlayer <- as.numeric(unlist(soil_var2[1]))
assign(a_nlayer,b_nlayer)
rm(a_nlayer,b_nlayer)

# THICK
a_thick <- unlist(soil_var1[2])
b_thick <- as.numeric(unlist(soil_var1[3:27]))
assign(a_thick,b_thick)
rm(a_thick,b_thick)

# STONEF
a_stonef <- unlist(soil_var2[2])
b_stonef <- as.numeric(unlist(soil_var2[3:27]))
assign(a_stonef,b_stonef)
rm(a_stonef,b_stonef)

# PSIF
a_psif <- unlist(soil_var3[2])
b_psif <- as.numeric(unlist(soil_var3[3:27]))
assign(a_psif,b_psif)
rm(a_psif,b_psif)

# THETAF
a_thetaf <- unlist(soil_var4[2])
b_thetaf <- as.numeric(unlist(soil_var4[3:27]))
assign(a_thetaf,b_thetaf)
rm(a_thetaf,b_thetaf)

# THSAT
a_thsat <- unlist(soil_var5[2])
b_thsat <- as.numeric(unlist(soil_var5[3:27]))
assign(a_thsat,b_thsat)
rm(a_thsat,b_thsat)

# BEXP
a_bexp <- unlist(soil_var6[2])
b_bexp <- as.numeric(unlist(soil_var6[3:27]))
assign(a_bexp,b_bexp)
rm(a_bexp,b_bexp)

# KF
a_kf <- unlist(soil_var7[2])
b_kf <- as.numeric(unlist(soil_var7[3:27]))
assign(a_kf,b_kf)
rm(a_kf,b_kf)

# WETINF
a_wetinf <- unlist(soil_var8[2])
b_wetinf <- as.numeric(unlist(soil_var8[3:27]))
assign(a_wetinf,b_wetinf)
rm(a_wetinf,b_wetinf)

rm(soil_var1,soil_var2,soil_var3,soil_var4,soil_var5,soil_var6,soil_var7,soil_var8)

