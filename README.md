# BROOK90 in R
by Rico Kronenberg and Lisa Marie Oehlschlägel

### Manual for the Usage of the BROOK90 in R Implementation
This manual describes how to run the BROOK90 as R implementation, based on the model by Federer, what files are necessary to download and what changes need to be done in the R-scripts.

![Output](https://github.com/rkronen/Brook90_R/blob/master/Documentation/Plot_Output/model_results.png)

Figure 1: One year of model results for the Wernersbach catchment in Germany. Data is included in the download.

#### Content

* [Overview of available files](#overview-of-available-files)
* [Download the data](#download-the-data)
* [Run the programm](#run-the-programm)
* [The output](#the-output)
* [Temporal model resolution (daily or subdaily)](#temporal-model-resolution---daily-or-subdaily)

#### Overview of available files
These files are available for usage:

##### Documentation
* html-Files
  + to download and read the full documentation
* plot output
  + example plot of calculated values
  
##### Rmd-files
* Documentation including the written R scripts to run the programm
  + html files are easier to read, Rmd-files for running the R-scripts

##### Input data
* precipitation data
    + P_WBG_0060_Minuten_1999_point.txt
* meteorological data
    + data_input_WB_1999_point.txt
* catchment parameters
    + 6 txt-files where the characteristics of the catchment can be changed
##### Input data fromat
Data files have to be formated as described [HERE](https://htmlpreview.github.io/?https://github.com/rkronen/Brook90_R/blob/master/Documentation/HTML_Files/MainProg.html#load-meteorological-and-precipitation-file). The minimal amount of meteorological information needed to run the model is daily precipitation, mininmal and maximal temperatures. Other variables have to be set to zero when not available.  

#### Download the data
As the second step the files can be downloaded and unpacked. 

#### Run the programm
To open the Rmd-files it is necessary to open R Studio first and to install the package for R-Markdown.

```{r}
install.packages("rmarkdown")
```
The next step is to open the Rmd-script [B90V4.Rmd](https://htmlpreview.github.io/?https://github.com/rkronen/Brook90_R/blob/master/Documentation/HTML_Files/B90V4.html). There the "projectpath" has to be changed in the first R-chunk. How this must be done is described in the file. Then [MainProg.Rmd](https://htmlpreview.github.io/?https://github.com/rkronen/Brook90_R/blob/master/Documentation/HTML_Files/MainProg.html) has to be opened. Important changes that have to be done in MainProg.Rmd are:

* change the "projectpath", it has to be the same as in B90V4.Rmd
* the input data (meteorological data, precipitatin data, catchment parameters) have to be in the same folder 
  + is already done if you unpack the zip-folder
* change catchment parameters (located in "Input_data" - folder) in these files, if necessary:
    + canopy.txt
    + fixed.txt
    + flow_standart.txt
    + initial.txt
    + location.txt
    + soil.txt
* if you are NOT using the example data, change the names of your input data files (meteoFile, precFile,...) and check the form of your data with [consistency_meteoFile.Rmd](https://htmlpreview.github.io/?https://github.com/rkronen/Brook90_R/blob/master/Documentation/HTML_Files/consistency_meteoFile.html).

The next step is to open again the script B90V4.Rmd. At the end of this script there is a code chunk to plot the output data. In this case precipitation, evaporation, calculated and measured flow are plotted. If other data should be shown, add or exchange the output data with "timeseries_" in its name. Finally B90V4.Rmd can also be started with "Run All" (click on the little arrow button next to "Run") and then the model is running and producing the output.

#### The output
The programm calculates 24 daily time series of different water balance components in the considered catchment as output of BROOK90 at the current state. The table shows the explanation of the output data visible with the name "timeseries_..." and their corresponding shortcut:

Shortcut|Explanation [d]
--------|-------------------------------------
adef    |available water deficit in root zone
awat    |available soil water in root zone
evp     |evapotranspiration
flow    |total flow
irvp    |evaporation rate of intercepted rain
isvp    |evaporation rate of intercepted snow
mesld   |measured flow
pint    |average potential interception for day
prec    |precipitation
ptran   |average potential transpiration rate for day
rfald   |rainfall rate
rintd   |rain interception
rnet    |rain reaching soil surface
rthrd   |rain throughfall
sfald   |snowfall
sintd   |snow interception
slfld   |input to soil surface
slvp    |soil evaporation
smltd   |snowmelt
snow    |water equivalent of snow on the ground
snvp    |evaporation from snowpack
sthrd   |snow throughfall
swat    |total soil water in all layers
trand   |transpiration

As mentioned in [Run the programm](#run-the-programm), all of this time series can be plotted if you add or exchange time series at the end of the script B90V4.Rmd.

#### Temporal model resolution - daily or subdaily
The default setting in this version is in daily resolution but the model can be driven by subdaily data like hourly data. This can be changed in the MainProg.Rmd (chunk2). For hourly values NPINT has to be changed from 1 to 24 and SUBDAYDATA has to be set TRUE. 

Parameter |Default setting|Description
----------|---------------|-------------
NPINT     |1      |number of precipitation intervals per day,  1 = 1 day, 24 = 24 hours per day
SUBDAYDATA|FALSE  |FALSE or TRUE

The following lines show the part of MainProg.Rmd where the changes can be done:

```{r chunk2}
NPINT <- 1     #change to 24    
SUBDAYDATA <- FALSE   #change to TRUE
RRD <- 0.55
UWD <- 3.0
```

The change of the temporal resolution varies the output. The results for daily and hourly resolution is shown in the next two figures.

![](https://github.com/rkronen/Brook90_R/blob/master/Documentation/Plot_Output/resolution_daily.png)

Figure 2: Temporal model resolution daily

![](https://github.com/rkronen/Brook90_R/blob/master/Documentation/Plot_Output/resolution_hourly.png)

Figure 3: Temporal model resolution hourly
