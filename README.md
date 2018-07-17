# Brook90_R
Brook90 coded in R

### Manual for the Use of the BROOK90 R Implementation
This manual describes how to run the BROOK90 R implementation, based on the model by Federer, what files are necessary to download and what changes need to be done in the R-scripts.

![Output](https://github.com/rkronen/Brook90_R/blob/master/Documentation/Plot_Output/model_results.png)

Figure: One year of model results for the Wernersbach catchment in Germany. Data is included in the download.

#### Content

* [Overview of available files](#overview-of-available-files)
* [Download the data](#download-the-data)
* [Run the programm](#run-the-programm)
* [The output](#the-output)

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
    + 6 .txt files where the characteristics of the catchment can be changed

#### Download the data
As the second step the files can be downloaded and unpacked. 

#### Run the programm
The first step is to open B90V4.Rmd. There the "projectpath" has to be changed. How this must be done is described there.
Then to run the programm in R, the MainProg.Rmd script has to be opened. Important changes that have to be done in MainProg.Rmd are:

* change the "projectpath", it has to be the same as in B90V4.Rmd
* the the input data (meteorological data, precipitatin data, catchment parameters, .txt files) have to be in the same folder (is already done if you unpack the zip-folder)
* change catchment parameters (located in "Input_data" - folder) in these files, if necessary:
    + canopy.txt
    + fixed.txt
    + flow_standart.txt
    + initial.txt
    + location.txt
    + soil.txt
* if you are NOT using the example data, change the names of your input data (meteoFile, precFile,...) and check the form of your data

The next step is to open again the script B90V4.Rmd. At the end of this script there is a code to plot the output data. In this case precipitation, evaporation, calculated and measured flow are plotted. If other data should be shown, add or exchange the output data with "timeseries_" in its name. Finally B90V4.Rmd can also be started with "Run All" (click on the little arrow button next to "Run") and then the model is running and producing the output.

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
