###############################################################################################################################################################
#Author: Adrianne Smits
#Date: 02/01/2022

# This script creates inputs necessary for estimating metabolism (LakeMetabolizer package) from
#corrected lake sensor data. Inputs to LakeMetabolizer include:
# doobs: observed DO concentration (mg/L), corrected for sensor drift
# do.sat: equilibrium DO concentration at temperature measured at DO sensor depth (corrected for lake altitude)
# k.gas: temperature specific gas transfer velocity (from k600 calculated from wind speed, or constant value if windspeed missing)
# irr: PAR in units of umol m^-2 sec^-1 (or vector of day/night for bookkeeping method; can estimate PAR from shortwave radiation)
# z.mix: mixed layer depth (meters) (calculated using LakeAnalyzer, or lake depth if polymictic)
# wtr: dataframe of water temperatures or single water temperature time series
#datetime: time series of datetimes (POSIXct class)

#Note: if no radiation data available, create a vector indicating day/night times using Jeff Dozier's topographic shading model output
###############################################################################################################################################################
library(LakeMetabolizer)
library(rLakeAnalyzer)
library(lubridate)
library(tidyverse)
###############################################################################################################################################################
##set working directory, load data, select desired year and lake to analyze
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd)

#Choose year and lake
year <- 2023 #desired open water period
lake <- 'Emerald' #choose desired lake

###############################################################################################################################################################
#Load list of lake sensor dataframes (clipped to open water period in chosen year and corrected for DO sensor drift)
# files <- list.files()#all the files in wd
# sensor_data <- list(0)
# load(files[grep(paste(lake,year,'openwater_hourly_corr.Rdata',sep="_"),files)])
# sensor_data <- openwater_data_hourly
load("Emerald_2023_winter_data.rdata")
sensor_data <- output.list[[1]]

#set index of desired dataset (most summers have two datasets corresponding to sensor array retrieval)
#indx <- 1
sensor_data$datetime <- as.character(sensor_data$datetime)
sensor_data$datetime <- as.POSIXct(sensor_data$datetime,format='%Y-%m-%d %H:%M:%S',tz="Etc/GMT-8")
#sensor_data[[1]]$datetime <- force_tz(sensor_data[[1]]$datetime,tzone="UTC")

#clip to relevant time period - may to end of summer? 2022-03-26 14:00:00; 2022-07-15 17:00:00
sensor_data <- sensor_data[complete.cases(sensor_data$datetime), ]
sensor_data <- sensor_data[with(sensor_data, datetime >= "2022-03-27 14:00:00" & datetime <= "2022-07-15 17:00:00"),]

#sensor data is missing 00:00 timestamp; fill this in. 
sensor_data <-  sensor_data %>% mutate(Hour = hour(datetime))
sensor_data <- sensor_data %>%
  filter(Hour == 1) %>%
  mutate(
    Hour = 24,
    datetime = as.POSIXct(paste(format(datetime, "%Y-%m-%d"), "00:00:00"), tz = "Etc/GMT-8")
  ) %>%
  bind_rows(sensor_data) %>%
  arrange(datetime) %>%
  ungroup()


#depth of DO sensor
DO_depth <-strsplit(names(sensor_data)[grep('DO',names(sensor_data))][1],"_")
DO_depth <- as.numeric(DO_depth[[1]][2])

#remove temperature data from conductivity and light sensors:

    #no conductivity sensor in this dataframe


#depth of light sensor
light_depth <- strsplit(names(sensor_data)[grep('light',names(sensor_data))],"_")
light_depth <- as.numeric(light_depth[[1]][2])
light_depth

#remove temperature reading from light sensor
if(!is.na(light_depth)){
  sensor_data <-  sensor_data[,-grep(paste('temp',light_depth,sep="_"),names(sensor_data))]
}

###############################################################################################################################################################
#Load site characteristics to get lake elevation and surface area
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd)
site_attributes <- read.csv('smoke_lakes_attributes.csv',header=TRUE)
elevation <-site_attributes$Elevation_m[site_attributes$Lake==lake]
Latitude <-site_attributes$Lat[site_attributes$Lake==lake]
LakeArea <- site_attributes$Lake_Area_m2_NG[site_attributes$Lake==lake] #m^2
MaxDepth <- site_attributes$Max_Depth_m_NG[site_attributes$Lake==lake] #m^2
kd <- site_attributes$Kd_mean[site_attributes$Lake==lake]


###############################################################################################################################################################
#Move to new wd to get topographic shading data to create day/night time series
# wd <- 'C:/Users/asmits/Dropbox/Todd_share/Sierra Postdoc/Sierra Multi Lakes Data/R Code/Camolan_sensor_data_smoke/Dozier_topo_shading'
# setwd(wd)
# files <-list.files()
# shading <-read.csv(files[grep(lake,files)],header=TRUE)
# shading$MeasDateTime <- as.character(shading$MeasDateTime)
# shading$MeasDateTime <- as.POSIXct(shading$MeasDateTime, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")



#MJF - used light sensor data to create day/night time series
#Threshold >= 10000 = day; <x = night. 1 is day, 0 is night.

sensor_data$shading = ifelse(sensor_data$light_2.77 > 10000, 1, 0)

#just to check
#library(lubridate)
DayCategory= sensor_data[, c("datetime","shading")]
DayCategory= na.omit(DayCategory)



###############################################################################################################################################################
#Move to new wd to get lake bathymetry
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd)

hypso <- read.csv("Emerald_hypsography.csv",header=TRUE)
#format bathymetry dataframe for use with LakeAnalyzer functions
bathy <- data.frame(depths=hypso$Depth_m,areas=hypso$SA_m2)
bathy <- bathy[bathy$areas>0,]

###############################################################################################################################################################
#If available, move to new wd to get weather station data

weather.data <- read.csv("Emerald_MET_data_2019_2023_hourly.csv",header=TRUE)
#weather.data <- read.csv("Topaz_MET_data_2019_2023_hourly.csv",header=TRUE)

head(weather.data)

weather.data$Datetime <-  as.POSIXct(weather.data$Datetime, format='%Y-%m-%d %H:%M:%S',tz="Etc/GMT-8")
weather.data$SW_solar <- weather.data$SW_solar*-1
weather.data$SW_solar[weather.data$SW_solar<0] <- 0 #convert nighttime negative values to zero
plot(weather.data$Datetime,weather.data$SW_solar,type='l')
plot(weather.data$Datetime,weather.data$WS,type='l')
plot(weather.data$Datetime,weather.data$AirTemp,type='l')

weather.data <- weather.data[complete.cases(weather.data$Datetime), ]


which(weather.data$WS <0)
#clip weather data to same time period as lake sensor data
weather.clipped <- weather.data[weather.data$Datetime %in% sensor_data$datetime,]

#2022 EML weather data is missing 00:00:00 timestep. 
# Identify missing hours for each day and duplicate data
weather.clipped <- weather.clipped %>%
  filter(Hour == 1) %>%
  mutate(
    Hour = 24,
    Datetime = as.POSIXct(paste(format(Datetime, "%Y-%m-%d"), "00:00:00"), tz = "Etc/GMT-8")
  ) %>%
  bind_rows(weather.clipped) %>%
  arrange(Datetime) %>%
  ungroup()

weather.clipped$date=NULL

weather.clipped <- weather.clipped[weather.clipped$Datetime %in% sensor_data$datetime,]




swrad <- weather.clipped$SW_solar
wind <-weather.clipped$WS #meters per second
AirTemp <- weather.clipped$AirTemp

###############################################################################################################################################################
#Create vector of mixed layer depths
#For a polymictic lake, this will be lake depth at the sensor array, otherwise
#use output from LakeAnalyzer
#format dataframe of water temperatures
wtr <- sensor_data[c(grep('datetime',names(sensor_data)),grep('temp',names(sensor_data)))]
#change column names to "wtr_1.35' etc
depths <- strsplit(names(wtr)[2:length(names(wtr))],"_")
depths <- as.numeric(sapply(depths, "[[", 2))
names(wtr)[2:length(names(wtr))] <- paste('wtr',depths, sep="_")
plot(wtr$datetime,wtr[,2],type='l')
for(i in 2:length(names(wtr))){
  lines(wtr$datetime, wtr[,i])
}
#calculate thermo depths (seasonal and diel?)
thermo <- ts.thermo.depth(wtr=wtr, Smin=1)#need to play with Smin parameter
thermo.seasonal <-ts.thermo.depth(wtr=wtr, seasonal=TRUE)
plot(thermo$datetime,thermo$thermo.depth)
plot(weather.clipped$Datetime,weather.clipped$WS,type='l')
plot(weather.clipped$Datetime,weather.clipped$AirTemp,type='l')

#if lake polymictic, mixed layer depth is deepest sensor depth
zmix.poly <- strsplit(names(wtr)[length(names(wtr))],"_")
zmix.poly <- as.numeric(zmix.poly[[1]][2])
z.mix <- thermo$thermo.depth
z.mix[is.na(z.mix)] <- zmix.poly#set zmix to deepest sensor depth when thermo depth is undefined (e.g. whole lake is mixed)
z.mix

#if mooring is in littoral zone, zmix is set to lake depth at the mooring
#z.mix <-rep(MaxDepth,length(wtr$datetime))

#calculate water column schmidt stability
ss <- ts.schmidt.stability(wtr=wtr, bathy=bathy)
par(mfrow=c(4,1),mar=c(1,2,1,1))
plot(ss$datetime,ss$schmidt.stability,type='l')
plot(weather.clipped$Datetime,weather.clipped$WS,type='l')
plot(weather.clipped$Datetime,weather.clipped$AirTemp)
plot(weather.clipped$Datetime,weather.clipped$SW_solar,type='l')
###############################################################################################################################################################
#Create day/night time series or PAR time series (irr)

#to get day/night time series, create vector of zeroes of same length as sensor time series (0 = night)
#for each day and time in the time series, match to a day and time in shading. The 'Hidden' column shows the
#fraction of that hour that the lake was shaded. If shading$Hidden < 0.5,
#then change the vector value to one (to signify daytime)
# daynight <- data.frame(datetime=sensor_data[[indx]]$datetime, daytime=rep(0,length(sensor_data[[indx]]$datetime)))
# shading$datetime <- as.POSIXct(paste(year,month(shading$MeasDateTime),day(shading$MeasDateTime),sep='-'))
# hour(shading$datetime) <- hour(shading$MeasDateTime)
# shading$datetime <- force_tz(shading$datetime,tzone="Etc/GMT-8")
# daynight$daytime[daynight$datetime %in% shading$datetime[shading$Hidden < 0.5]] <- 1
# plot(daynight$datetime,daynight$daytime,type='l')

#if no shading data are available, estimate day/night times using is.day function
daynight <- data.frame(datetime=sensor_data$datetime, daytime=rep(0,length(sensor_data$datetime)))
daynight$daytime[is.day(daynight$datetime,lat=Latitude)] <- 1

#if available, converst shortwave radiation to surface PAR, then estimate mean PAR in upper mixed layer
#using Equation 5 in Staehr et al. 2016 Inland Waters
irr_surface <- sw.to.par.base(sw=swrad)
irr_mean <- (1-0.1)*irr_surface*(1-exp(-kd*z.mix))/(kd*z.mix)
irr <- irr_mean
plot(wtr$datetime,irr,type='l',ylim=c(0,2000))
lines(wtr$datetime,irr_surface,lty=2)

#if available, use measured underwater PAR
irr <- sensor_data[grep('PAR',names(sensor_data))]
irr <- irr[,1]
irr[irr<0] <-0


###############################################################################################################################################################
#Create vector of gas exchange based on wind speed and lake surface area (Vachon model)
#convert wind speed to U10
U10 <- wind.scale.base(wnd=wind, wnd.z=10)
k600 <- k.vachon.base(wnd=U10, lake.area=LakeArea)
#convert k600 to ko2 based on water temperature at DO sensor depth
#water temperature at DO depth:
DOtemp <- sensor_data[[grep(paste('temp',DO_depth,sep="_"),names(sensor_data))]]
k.gas <- k600.2.kGAS.base(k600=k600, temperature=DOtemp, gas="O2")
#Optional: turn k.gas 'off' during periods of diel stratification
k.gas.diurnal <- k.gas
k.gas.diurnal[!is.na(thermo$thermo.depth)&thermo$thermo.depth<DO_depth] <- 0


###############################################################################################################################################################
#Select doobs (mg/L) and create vector of DO saturation at given temperature and elevation
do.obs <- sensor_data[grep('DO',names(sensor_data))]
do.obs <-do.obs[,1]
# Get equilibrium saturation concentration of oxygen in water 
do.sat <-o2.at.sat.base(DOtemp , altitude=elevation)

###############################################################################################################################################################
#create dataframe of inputs for LakeMetabolizer, indicate whether book-keeping or other method will be used
ts.data <- data.frame(datetime=wtr$datetime,do.obs=do.obs, do.sat=do.sat, k.gas=k.gas.diurnal,
                      z.mix=z.mix, irr=irr,daytime=daynight$daytime,swrad=swrad,wind_ms =wind,airt=AirTemp,schmidt=ss$schmidt.stability)
ts.data <- cbind(ts.data,wtr[2:length(names(wtr))])
#ts.data <- cbind(ts.data,data.frame(wtr[,2:length(names(wtr))]))
#names(ts.data)[length(ts.data)] <- 'wtr_1.62'
plot(ts.data$datetime,ts.data$do.obs,type='l',ylim=c(7,13))
plot(ts.data$datetime,ts.data$z.mix,type='l')
abline(h=DO_depth)
plot(ts.data$datetime,ts.data$irr,type='l')
plot(ts.data$datetime,ts.data$swrad,type='l')




###############################################################################################################################################################

#Save ts.data and DO.depth to use as inputs for LakeMetabolizer in separate script
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd)

inputs <-list(ts.data,DO_depth)
save(inputs,file=paste("05",lake,year,"LakeMetabolizer_inputs.Rdata",sep="_"))

###############################################################################################################################################################
##Exploratory
# Calculate metabolism using the kalman method (for exploration)

kalman.res = metab(ts.data, method='kalman', 
                   wtr.name=paste('wtr',DO_depth,sep="_"), do.obs.name='do.obs', irr.name='irr')
names(attributes(kalman.res))


###############################################################################################################################################################
#Calculate metabolism using the book-keeping methods (for exploration)
bookkeep.res = metab(ts.data, method='bookkeep', 
                     wtr.name=paste('wtr',DO_depth,sep="_"), do.obs.name='do.obs', irr.name='daytime')
bookkeep.res
###############################################################################################################################################################
#examine outputs
par(mfrow=c(3,1))
plot(bookkeep.res$doy,bookkeep.res$GPP,type='b',pch=21,col='darkgreen',ylim=c(-0.5,6.5))
points(kalman.res$doy,kalman.res$GPP,type='b',pch=19,col='darkgreen')
abline(h=0,lty=2)
abline(v=230)
abline(v=251)

plot(bookkeep.res$doy,bookkeep.res$R,type='b',pch=21,col='brown',ylim=c(-2,0.5))
points(kalman.res$doy,kalman.res$R,type='b',pch=19,col='brown')
abline(h=0,lty=2)
abline(v=230)
abline(v=251)

plot(bookkeep.res$doy,bookkeep.res$NEP,type='b',pch=21,col='darkblue',ylim=c(-0.5,0.5))
points(kalman.res$doy,kalman.res$NEP,type='b',pch=19,col='darkblue')
abline(h=0,lty=2)
abline(v=230)
abline(v=251)

