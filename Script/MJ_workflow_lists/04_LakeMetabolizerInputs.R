###############################################################################################################################################################
#Author: Adrianne Smits
#Date: 02/01/2022

#Modified for ponds by MJ Farruggia

# This script creates inputs necessary for estimating metabolism (LakeMetabolizer package) from
#corrected lake sensor data. Inputs to LakeMetabolizer include:
# doobs: observed DO concentration (mg/L), corrected for sensor drift
# do.sat: equilibrium DO concentration at temperature measured at DO sensor depth (corrected for lake altitude)
# k.gas: temperature specific gas transfer velocity (from k600 calculated from wind speed, or constant value if windspeed missing)
# irr: PAR in units of umol m^-2 sec^-1 (or vector of day/night for bookkeeping method; can estimate PAR from shortwave radiation)
# z.mix: mixed layer depth (meters) (calculated using LakeAnalyzer, or lake depth if polymictic)
# wtr: dataframe of water temperatures or single water temperature time series
#datetime: time series of datetimes (POSIXct class)



###############################################################################################################################################################
library(LakeMetabolizer)
library(rLakeAnalyzer)
library(lubridate)
library(dplyr)
###############################################################################################################################################################
##set working directory, load data, select desired year and lake to analyze
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data/ponds/corrected"
setwd(wd)

#Choose year and lake
year <- 2022
lake <- 'EMLPOND1' 
season.year <- 'S22' 

###############################################################################################################################################################
#Load lake sensor data frame

files <- list.files() # all the files in wd
sensor_data <- list(0)
matching_file <- grep(paste(lake, season.year, 'temp_DO_corr.Rdata', sep = "_"), files, value = TRUE)


if (length(matching_file) > 0) {
  loaded_objects <- load(matching_file)
  sensor_data <- loaded_objects[1]
} else {
  sensor_data <- NULL
}


sensor_data=merged_df
colnames(sensor_data)[colnames(sensor_data) == "GMT.08.00"] <- "datetime"

#fix topazpond 2022 temp col name
#names(sensor_data)[names(sensor_data) == "temp_0.28"] <- "temp_0.77"

###############################################################################################################################################################
# 
# #Choose year and lake
# year <- 2020
# lake <- 'TOPAZPOND' 
# season.year <- 'W20' 
# 
# ###############################################################################################################################################################
# #Load lake sensor data frame
# 
# files <- list.files() # all the files in wd
# sensor_data_w <- list(0)
# matching_file_w <- grep(paste(lake, season.year, 'temp_DO_corr.Rdata', sep = "_"), files, value = TRUE)
# 
# 
# if (length(matching_file_w) > 0) {
#   loaded_objects_w <- load(matching_file_w)
#   sensor_data_w <- loaded_objects_w[1]
# } else {
#   sensor_data_w <- NULL
# }
# 
# sensor_data_w=merged_df
# colnames(sensor_data_w)[colnames(sensor_data_w) == "GMT.08.00"] <- "datetime"
# 
# colnames(sensor_data_w)[colnames(sensor_data_w) == "temp_0.2"] <- "temp_0.98"
# sensor_data_w$temp_0.98 = sensor_data_w$temp_0.08
# colnames(sensor_data_w)[colnames(sensor_data_w) == "DO_0.2"] <- "DO_0.98"
# colnames(sensor_data_w)[colnames(sensor_data_w) == "Sat_0.2"] <- "Sat_0.98"
# colnames(sensor_data_w)[colnames(sensor_data_w) == "temp_1.2"] <- "temp_1.66"
# colnames(sensor_data_w)[colnames(sensor_data_w) == "cond_1.2"] <- "cond_1.66"
# colnames(sensor_data_w)[colnames(sensor_data_w) == "corrected_DO_0.2"] <- "corrected_DO_0.98"
# colnames(sensor_data_w)[colnames(sensor_data_w) == "corrected_Sat_0.2"] <- "corrected_Sat_0.98"
# sensor_data_w$AirTemperature=NULL
# sensor_data_w$SiteName=NULL
# sensor_data_w$SampleDate=NULL
# sensor_data_w$X=NULL
# sensor_data_w$X.1=NULL
# sensor_data_w$X.2=NULL
# sensor_data_w$X.3=NULL
# sensor_data_w$X.4=NULL
#sensor_data=merge(sensor_data, sensor_data_w, all=T)

###############################################################################################################################################################


#site attributes
setwd("C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data")
site_attributes <- read.csv('All_PondCharacteristics_2020_2022.csv',header=TRUE)
site_attributes <- filter(site_attributes, Lake == lake & Year == year)


#pond depths
pdepth=read.csv(file="20-22_PondDepth.csv", header=T)
pdepth$GMT.08.00=as.POSIXct(pdepth$GMT.08.00, format='%m/%d/%Y %H:%M', tz="Etc/GMT-8")

######################change for summer datasets##########################################
#clip the df because light logger has 1 extra day than the rest...(mjf)
#sensor_data=sensor_data[with(sensor_data, datetime > "2020-06-19 00:00:00" & datetime < "2020-10-30 00:00:00"),]
#sensor_data=sensor_data[with(sensor_data, datetime > "2021-06-19 00:00:00" & datetime < "2021-10-29 00:00:00"),]
sensor_data=sensor_data[with(sensor_data, datetime > "2022-06-19 00:00:00" & datetime < "2022-10-06 00:00:00"),]



#set index of desired dataset (most summers have two datasets corresponding to sensor array retrieval)
#indx <- 2
#sensor_data[[indx]]$datetime <- as.character(sensor_data[[indx]]$datetime)
sensor_data$datetime=as.character(sensor_data$datetime)
#sensor_data[[indx]]$datetime <- as.POSIXct(sensor_data[[indx]]$datetime,tz="Etc/GMT-8")
#sensor_data$datetime=as.POSIXct(sensor_data$datetime, tz="Etc/GMT-8", format = "%Y-%m-%d %H:%M:%S")
sensor_data$datetime=as.POSIXct(sensor_data$datetime, tz="Etc/GMT-8", format = "%Y-%m-%d %H:%M:%S")
#sensor_data[[1]]$datetime <- force_tz(sensor_data[[1]]$datetime,tzone="UTC")
#sensor_data$datetime=force_tz(sensor_data$datetime,tz="UTC")                         

#take hourly mean of temp and DO data (MJF)
library(lubridate)
library(tidyverse)

sensor_data_group=group_by(sensor_data, datetime = floor_date(datetime, "hour")) 
sensor_data_means=summarize(sensor_data_group,  temp_2.8 = mean(temp_2.8, na.rm=T), temp_1.58= mean(temp_1.58, na.rm=T), corrected_DO_1.58= mean(corrected_DO_1.58, na.rm=T), corrected_Sat_1.58 = mean(corrected_Sat_1.58, na.rm=T)) #%>%
sensor_data=sensor_data_means

sensor_data <- sensor_data[complete.cases(sensor_data), ]





#plot 2 days of DO just to check

# Find the range of dates in the dataset
date_range <- range(sensor_data$datetime)

# Initialize variables for the loop
max_attempts <- 1000  # Set a limit for attempts
attempt <- 1

# Loop until a valid subset is found or the maximum attempts are reached
while (attempt <= max_attempts) {
  # Generate a random start date within the range
  random_start <- as.POSIXct(runif(1, as.numeric(date_range[1]), as.numeric(date_range[2])), origin = "1970-01-01")
  
  # Calculate the end date for the 2-day window
  random_end <- random_start + days(2)
  
  # Select the subset of data within the 2-day window using the with() function
  random_window_data <- with(sensor_data, sensor_data[datetime >= random_start & datetime <= random_end, ])
  
  # Find the column name that starts with "corrected_DO"
  corr_DO_col <- grep("^corrected_DO", colnames(random_window_data), value = TRUE)
  
  # Check if the column was found and contains non-NA values
  if (length(corr_DO_col) > 0 && any(!is.na(random_window_data[[corr_DO_col]]))) {
    # Perform further data processing or visualization using random_window_data
    break  # Exit the loop since a valid subset is found
  }
  
  # Increment the attempt counter
  attempt <- attempt + 1
}

if (attempt > max_attempts) {
  print("Could not find a valid subset with matching column and non-NA values")
} else {
  library(ggplot2)
  # Find the column name that matches the pattern using grep
  corr_DO_col <- grep("^corrected_DO_", colnames(random_window_data), value = TRUE)
  
  # Remove NA values from the column
  random_window_data <- random_window_data[!is.na(random_window_data[[corr_DO_col]]), ]
  
  # Create the ggplot plot
  DOplot <- ggplot() +
    geom_line(data = random_window_data, aes(x = datetime, y = .data[[corr_DO_col]])) +
    scale_x_datetime(date_breaks = "2 hour", guide = guide_axis(angle = 90)) +
    theme_classic()
  
  DOplot
  }



#depth of DO sensor
#I made depth of DO sensor half of the maximum depth MJF
#DO_depth <- site_attributes$Max_Depth_m_NG/2

#I made depth of DO sensor equal to the depth of the sensor labeled on the column name.
# Find column names that match the pattern
matching_column <- grep("^corrected_DO_[0-9.]+$", names(sensor_data), value = TRUE)

# Extract the numeric part from the column name
if (length(matching_column) > 0) {
  DO_depth <- as.numeric(sub("^corrected_DO_", "", matching_column))
} else {
  DO_depth <- NA
}

# Print the value of DO_depth
cat("DO_depth:", DO_depth, "\n")

#remove temperature data from conductivity and light sensors:
#depth of temperature measurements from conductivity and light sensors:
#cond_depth <- strsplit(names(sensor_data[[indx]])[grep('cond',names(sensor_data[[indx]]))],"_")
#cond_depth <- as.numeric(cond_depth[[1]][2])
#cond_depth
#conductivity was always at the bottom, so made it equal to maximum depth MJF
#cond_depth <- site_attributes$Max_Depth_m_NG

#remove temperature reading from conductivity sensor
#if(!is.na(cond_depth)){
#  sensor_data[[indx]] <-  sensor_data[[indx]][,-grep(paste('temp',cond_depth,sep="_"),names(sensor_data[[indx]]))]
#}
#I intercalibrated within ponds so leaving the temp data in. MJF

#depth of light sensor
#light was at the surface, so made it equal to zero. MJF
light_depth <- 0



###############################################################################################################################################################
#Load site characteristics to get lake elevation and surface area

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



###############################################################################################################################################################
#MJF - I don't have topographic shading data, but did write code to determine day/night based on light sensors. 
#Threshold >= 10000 = day; <x = night. 1 is day, 0 is night.
# 
# sensor_data$shading = ifelse(sensor_data$IntensityLux > 10000, 1, 0)
# 
# #just to check
# #library(lubridate)
# DayCategory= sensor_data[, c("datetime","shading")]
# DayCategory= na.omit(DayCategory)
# 

###############################################################################################################################################################
#Move to new wd to get lake bathymetry
# wd <- 'C:/Users/asmits/Dropbox/Todd_share/Sierra Postdoc/Sierra Multi Lakes Data/R Code/Camolan_sensor_data_smoke/Hypsographic_curves'
# setwd(wd)
# files <-list.files()
# hypso <- read.csv(files[grep(lake,files)],header=TRUE)
# #format bathymetry dataframe for use with LakeAnalyzer functions
# bathy <- data.frame(depths=hypso$Depth_m,areas=hypso$SA_m2)
# bathy <- bathy[bathy$areas>0,]
# 
# #For lakes lacking bathymetry, estimate using surface area and max depth
bathy <-approx.bathy(Zmax=MaxDepth,lkeArea=LakeArea,zinterval=0.25)#use cone method if no Zmean is known

names(bathy) <-c('depths','areas')
###############################################################################################################################################################
#get weather station data
#Weather.data <- read.csv("Emerald_MET_data_2019_2021_hourly.csv",header=TRUE)
# Weather.data <- read.csv("Topaz_MET_data_2019_2021_hourly.csv",header=TRUE)
Weather.data <- read.csv("Topaz_MET_data_2019_2023_hourly.csv",header=TRUE)



head(Weather.data)
Weather.data$Datetime <-  as.POSIXct(Weather.data$Datetime, format='%m/%d/%y %H:%M',tz="Etc/GMT-8")
#weather.data$Datetime <-  as.POSIXct(weather.data$Datetime, format='%m/%d/%y %H:%M',tz="Etc/GMT-8")
#weather.data$SW_solar <- weather.data$SW_solar*-1
#weather.data$SW_solar[weather.data$SW_solar<0] <- 0 #convert nighttime negative values to zero
plot(Weather.data$Datetime,Weather.data$SW_solar,type='l')
plot(Weather.data$Datetime,Weather.data$WS,type='l')
plot(Weather.data$Datetime,Weather.data$AirTemp,type='l')
which(Weather.data$WS <0)
#clip weather data to same time period as lake sensor data
Weather.data <- subset(Weather.data, format(Datetime, "%Y") == "2022")
weather.clipped <- Weather.data[Weather.data$Datetime %in% sensor_data$datetime,] 

#2022 EML weather data is missing 00:00:00 timestep. 
# Identify missing hours for each day and duplicate data from 01:00:00
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
weather.clipped <- weather.clipped[complete.cases(weather.clipped), ]



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
#wtr = na.omit(wtr)
#plot(wtr$datetime,wtr[,2],type='l')

#for(i in 2:length(names(wtr))){
 # lines(wtr$datetime, wtr[,i])
#}
#calculate thermo depths (seasonal and diel?)
# thermo <- ts.thermo.depth(wtr=wtr, Smin=0.1)#need to play with Smin parameter         #I'm just getting NA for thermo depth 
# thermo.seasonal <-ts.thermo.depth(wtr=wtr, seasonal=TRUE)
# plot(thermo$datetime,thermo$thermo.depth)
# plot(weather.clipped$Datetime,weather.clipped$WS,type='l')
plot(weather.clipped$Datetime,weather.clipped$AirTemp,type='l')

#if lake polymictic, mixed layer depth is deepest sensor depth
# zmix.poly <- strsplit(names(wtr)[length(names(wtr))],"_")
# zmix.poly <- as.numeric(zmix.poly[[1]][2])
# z.mix <- thermo$thermo.depth
# z.mix[is.na(z.mix)] <- zmix.poly#set zmix to deepest sensor depth when thermo depth is undefined (e.g. whole lake is mixed)
# z.mix

pdepth=pdepth[pdepth$SiteName == lake,]
pdepth <- subset(pdepth, format(GMT.08.00, "%Y") == "2022")


IP.depth <- data.frame(approx(pdepth$GMT.08.00, pdepth$MaxDepth, xout = sensor_data$datetime, rule = 2, method = "linear", ties = mean))

# Surface mixed depth 
z.mix = IP.depth$y



#calculate water column schmidt stability
ss=NA
# ss <- ts.schmidt.stability(wtr=wtr, bathy=bathy)
# par(mfrow=c(4,1),mar=c(1,2,1,1))
# plot(ss$datetime,ss$schmidt.stability,type='l')
plot(weather.clipped$Datetime,weather.clipped$WS,type='l')
plot(weather.clipped$Datetime,weather.clipped$AirTemp)
plot(weather.clipped$Datetime,weather.clipped$SW_solar,type='l')
###############################################################################################################################################################
#Create day/night time series or PAR time series (irr)

#to get day/night time series, create vector of zeroes of same length as sensor time series (0 = night)
#for each day and time in the time series, match to a day and time in shading. The 'Hidden' column shows the
#fraction of that hour that the lake was shaded. If shading$Hidden < 0.5,
#then change the vector value to one (to signify daytime)


#I did this above using the light logger - will just convert it to match the naming here. MJF
# daynight <- sensor_data[, c("datetime","shading")] #mjf
# 
# daynight$daytime=daynight$shading + 0
# plot(daynight$datetime,daynight$daytime,type='l')


#if no shading data are available, estimate day/night times using is.day function
daynight <- data.frame(datetime=sensor_data$datetime, daytime=rep(0,length(sensor_data$datetime)))
daynight$daytime[is.day(daynight$datetime,lat=Latitude)] <- 1


#if available, convert shortwave radiation to surface PAR, then estimate mean PAR in upper mixed layer
#using Equation 5 in Staehr et al. 2016 Inland Waters
irr_surface <- sw.to.par.base(sw=swrad)
irr_mean <- (1-0.1)*irr_surface*(1-exp(-kd*z.mix))/(kd*z.mix)
irr <- irr_mean
plot(wtr$datetime,irr,type='l',ylim=c(0,2000))
lines(wtr$datetime,irr_surface,lty=2)




###############################################################################################################################################################
#Create vector of gas exchange based on wind speed and lake surface area (Vachon model)
#convert wind speed to U10
U10 <- wind.scale.base(wnd=wind, wnd.z=10)

#if no wind data, use a constant
#U10 <- wind.scale.base(wnd=10, wnd.z=10)

k600 <- k.vachon.base(wnd=U10, lake.area=LakeArea)
#convert k600 to ko2 based on water temperature at DO sensor depth
#water temperature at DO depth:
DOtemp <- sensor_data[[grep(paste('temp',DO_depth,sep="_"),names(sensor_data))]]
k.gas <- k600.2.kGAS.base(k600=k600, temperature=DOtemp, gas="O2")
#Optional: turn k.gas 'off' during periods of diel stratification
k.gas.diurnal <- k.gas
#k.gas.diurnal[!is.na(thermo$thermo.depth)&thermo$thermo.depth<DO_depth] <- 0


###############################################################################################################################################################
#Select corr_DO (mg/L) and create vector of DO saturation at given temperature and elevation
do.obs <- sensor_data[grep('corrected_DO',names(sensor_data))]
do.obs <-do.obs[,1]
# Get equilibrium saturation concentration of oxygen in water 
do.sat <-o2.at.sat.base(DOtemp , altitude=elevation)

###############################################################################################################################################################
#create dataframe of inputs for LakeMetabolizer, indicate whether book-keeping or other method will be used
ts.data <- data.frame(datetime=wtr$datetime,do.obs=do.obs, do.sat=do.sat, k.gas=k.gas.diurnal,
                      z.mix=z.mix, irr=irr,daytime=daynight$daytime,swrad=swrad,wind_ms =wind,airt=AirTemp) #removed schmidt stability
ts.data <- cbind(ts.data,wtr[2:length(names(wtr))])

#names(ts.data)[names(ts.data) == 'corrected_DO_1.55'] <- 'do.obs'
# Find column names that start with "corrected_DO"
matching_columns <- grep("^corrected_DO", names(ts.data), value = TRUE)

# Rename the matching columns
new_column_name <- "do.obs"
names(ts.data)[names(ts.data) %in% matching_columns] <- new_column_name

#ts.data <- cbind(ts.data,data.frame(wtr[,2:length(names(wtr))]))
#names(ts.data)[length(ts.data)] <- 'wtr_1.62'
plot(ts.data$datetime,ts.data$do.obs,type='l',ylim=c(3,8))
plot(ts.data$datetime,ts.data$z.mix,type='l')
abline(h=DO_depth)
plot(ts.data$datetime,ts.data$irr,type='l')
plot(ts.data$datetime,ts.data$swrad,type='l')
###############################################################################################################################################################

#Save ts.data and DO.depth to use as inputs for LakeMetabolizer in separate script
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd)
inputs <-list(ts.data,DO_depth)
save(inputs,file=paste("05",lake,season.year,"LakeMetabolizer_inputs.Rdata",sep="_"))

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
plot(bookkeep.res$doy,bookkeep.res$GPP,type='b',pch=21,col='darkgreen',ylim=c(-3,3))
points(kalman.res$doy,kalman.res$GPP,type='b',pch=19,col='darkgreen')
abline(h=0,lty=2)
abline(v=230)
abline(v=251)

plot(bookkeep.res$doy,bookkeep.res$R,type='b',pch=21,col='brown',ylim=c(-3,3))
points(kalman.res$doy,kalman.res$R,type='b',pch=19,col='brown')
abline(h=0,lty=2)
abline(v=230)
abline(v=251)

plot(bookkeep.res$doy,bookkeep.res$NEP,type='b',pch=21,col='darkblue',ylim=c(-3,3))
points(kalman.res$doy,kalman.res$NEP,type='b',pch=19,col='darkblue')
abline(h=0,lty=2)
abline(v=230)
abline(v=251)

