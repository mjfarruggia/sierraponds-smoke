#this code is to produce a csv that contains all instrument data from ponds in summer 2022, with the following cleaning:
#1. Position of instrument on chain was added as a column
#2. Naming of variables and column names were standardized
#3. Data was cut to relevant time frames (some loggers start/end logging when not in the water, I trimmed to just the times they were supposed to be actually deployed in the water column.)
#4. Temperature intercalibrations were applied, see in-line comments for each value for each instrument. Code for determining intercalibrations is in file named S2022_tokopah_calibration.R


setwd("C:/Users/maryj/Documents/R/SierraPonds/RawLoggerData")
library(ggplot2)
library(lubridate)
library(dplyr)
library(cowplot) #use plot_grid(), can specify columns with ncol=x. allows you to combine multiple ggplots into a single figure.

#SUMMER 2022 PONDS - EMLPOND1; TOK 11; TOPAZPOND; TOK30

##EMLPOND1 - 3 INSTRUMENTS

#load data by pond, and change format of columns to make uniform/change date format to POSIX 

###eml pond 1 - 3 instruments
#load data, remove unnecessary columns, fix date/time column
eml1_minidot=read.csv(file='EMLPOND1_MINIDOT_S2022_PONDEC.csv', header=T)
eml1_minidot$SiteName="EML1MINIDOT"
eml1_minidot$Unix.Timestamp=NULL
eml1_minidot$UTC_Date_Time=NULL
eml1_minidot$Q=NULL
eml1_minidot$Battery=NULL
eml1_minidot$GMT.08.00=as.POSIXct(eml1_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
eml1_minidot$InstrumentType="MINIDOT"
eml1_temp=read.csv(file='EMLPOND1_CONDUCTIVITY_TOP_S2022.csv',header=T)
eml1_temp$SiteName="EML1TEMP"
names(eml1_temp)[names(eml1_temp) == '?..GMT.08.00'] <- 'GMT.08.00'
eml1_temp$GMT.08.00=as.character(eml1_temp$GMT.08.00)
eml1_temp$GMT.08.00=as.POSIXct(eml1_temp$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
eml1_temp$InstrumentType="CONDUCTIVITY"
eml1_conductivity=read.csv(file='EMLPOND1_CONDUCTIVITY_S2022.csv',header=T)
eml1_conductivity$SiteName="EML1CONDUCTIVITY"
names(eml1_conductivity)[names(eml1_conductivity) == '?..GMT.08.00'] <- 'GMT.08.00'
eml1_conductivity$GMT.08.00=as.character(eml1_conductivity$GMT.08.00)
eml1_conductivity$GMT.08.00=as.POSIXct(eml1_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
eml1_conductivity$InstrumentType="CONDUCTIVITY"
eml1_light=read.csv(file='EMLPOND1_LIGHT_S2022.csv', header=T)
eml1_light$GMT.08.00=as.character(eml1_light$GMT.08.00)
eml1_light$GMT.08.00=as.POSIXct(eml1_light$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
names(eml1_light)[names(eml1_light) == 'Temperature'] <- 'AirTemperature'
eml1_light$InstrumentType="LIGHTLOGGER"

#apply water temperature calibration correction. Light logger technically measures air temp, so wasn't included.
eml1_minidot$Temperature=eml1_minidot$Temperature - 0 ###############NEEDS CORRECTION########################################
eml1_temp$Temperature=eml1_temp$Temperature  -0.379983389
eml1_conductivity$Temperature=eml1_conductivity$Temperature -0.065232558



#add column specifying instrument's position on the chain.
eml1_minidot$Position="Middle"
eml1_temp$Position="Top"
eml1_conductivity$Position="Bottom"
eml1_light$Position="Surface"

#add logger serial
eml1_temp$LoggerSerial=20636144
eml1_minidot$LoggerSerial=549033
eml1_conductivity$LoggerSerial=20438689


#add columns that identify the position of the loggers
eml1_temp$temp_0.2= eml1_temp$Temperature
eml1_minidot$temp_1.58=eml1_minidot$Temperature
eml1_minidot$DO_1.58=eml1_minidot$Dissolved.Oxygen
eml1_minidot$Sat_1.58=eml1_minidot$Dissolved.Oxygen.Saturation
eml1_conductivity$temp_2.8=eml1_conductivity$Temperature
eml1_conductivity$cond_2.8=eml1_conductivity$Conductivity

#combine instruments from this pond into one data frame
eml1.1=merge(eml1_minidot, eml1_temp, all=T)
eml1.2=merge(eml1.1, eml1_conductivity, all=T)
eml1.3=merge(eml1.2, eml1_light, all=T)

#restrict dates to only include the sample time
eml1.3=eml1.3[with(eml1.3, GMT.08.00 > "2022-06-04 12:00:00" & GMT.08.00 < "2022-10-07 14:00:00"),]

#add SiteName and SampleDate column
eml1.3$SiteName="EMLPOND1"
eml1.3$SampleDate=as.Date(eml1.3$GMT.08.00)
eml1.3$X............Unix.Timestamp=NULL



###TOK11 - 3 instruments
#load data, remove unnecessary columns, fix date/time column
TOK11_minidot=read.csv(file='TOK11_MINIDOT_S2022_PONDEC.csv', header=T)
TOK11_minidot$SiteName="TOK11MINIDOT"
TOK11_minidot$Unix.Timestamp=NULL
TOK11_minidot$UTC_Date_Time=NULL
TOK11_minidot$Q=NULL
TOK11_minidot$Battery=NULL
TOK11_minidot$GMT.08.00=as.POSIXct(TOK11_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
TOK11_minidot$InstrumentType="MINIDOT"
TOK11_temp=read.csv(file='TOK11_TEMPERATURE_S2022.csv',header=T)
TOK11_temp$SiteName="TOK11TEMP"
names(TOK11_temp)[names(TOK11_temp) == '?..GMT.08.00'] <- 'GMT.08.00'
TOK11_temp$GMT.08.00=as.character(TOK11_temp$GMT.08.00)
TOK11_temp$GMT.08.00=as.POSIXct(TOK11_temp$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
TOK11_temp$InstrumentType="TEMPPRO"
TOK11_conductivity=read.csv(file='TOK11_CONDUCTIVITY_S2022.csv',header=T)
TOK11_conductivity$SiteName="TOK11CONDUCTIVITY"
names(TOK11_conductivity)[names(TOK11_conductivity) == '?..GMT.08.00'] <- 'GMT.08.00'
TOK11_conductivity$GMT.08.00=as.character(TOK11_conductivity$GMT.08.00)
TOK11_conductivity$GMT.08.00=as.POSIXct(TOK11_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
TOK11_conductivity$InstrumentType="CONDUCTIVITY"
TOK11_light=read.csv(file='TOK11_LIGHT_S2022.csv', header=T)
names(TOK11_light)[names(TOK11_light) == '?..GMT.08.00'] <- 'GMT.08.00'
TOK11_light$GMT.08.00=as.character(TOK11_light$GMT.08.00)
TOK11_light$GMT.08.00=as.POSIXct(TOK11_light$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
names(TOK11_light)[names(TOK11_light) == 'Temperature'] <- 'AirTemperature'
TOK11_light$InstrumentType="LIGHTLOGGER"

#apply water temperature calibration correction. Light logger technically measures air temp, so wasn't included.
TOK11_minidot$Temperature=TOK11_minidot$Temperature + 0.118920266
TOK11_temp$Temperature=TOK11_temp$Temperature - 0
TOK11_conductivity$Temperature=TOK11_conductivity$Temperature -0.348920266



#add column specifying instrument's position on the chain.
TOK11_minidot$Position="Middle"
TOK11_temp$Position="Top"
TOK11_conductivity$Position="Bottom"
TOK11_light$Position="Surface"

#add columns that identify the position of the loggers
TOK11_temp$temp_0.22= TOK11_temp$Temperature
TOK11_minidot$temp_1.24=TOK11_minidot$Temperature
TOK11_minidot$DO_1.24=TOK11_minidot$Dissolved.Oxygen
TOK11_minidot$Sat_1.24=TOK11_minidot$Dissolved.Oxygen.Saturation
TOK11_conductivity$temp_2.0=TOK11_conductivity$Temperature
TOK11_conductivity$cond_2.0=TOK11_conductivity$Conductivity

#add logger serial
TOK11_temp$LoggerSerial=20868832
TOK11_minidot$LoggerSerial=332834
TOK11_conductivity$LoggerSerial=20649551


#combine instruments from this pond into one data frame
TOK11.1=merge(TOK11_minidot, TOK11_temp, all=T)
TOK11.2=merge(TOK11.1, TOK11_conductivity, all=T)
TOK11.3=merge(TOK11.2, TOK11_light, all=T)

#restrict dates to only include the sample time
TOK11.3=TOK11.3[with(TOK11.3, GMT.08.00 > "2022-06-04 17:00:00" & GMT.08.00 < "2022-10-07 11:00:00"),]

#add SiteName and SampleDate column
TOK11.3$SiteName="TOK11"
TOK11.3$SampleDate=as.Date(TOK11.3$GMT.08.00)







###TOK30 - 3 instruments
#load data, remove unnecessary columns, fix date/time column
TOK30_minidot=read.csv(file='TOK30_MINIDOT_S2022_PONDEC.csv', header=T)
TOK30_minidot$SiteName="TOK30MINIDOT"
TOK30_minidot$Unix.Timestamp=NULL
TOK30_minidot$UTC_Date_Time=NULL
TOK30_minidot$Q=NULL
TOK30_minidot$Battery=NULL
TOK30_minidot$GMT.08.00=as.POSIXct(TOK30_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
TOK30_minidot$InstrumentType="MINIDOT"
TOK30_temp=read.csv(file='TOK30_CONDUCTIVITY_TOP_S2022.csv',header=T)
TOK30_temp$SiteName="TOK30TEMP"
names(TOK30_temp)[names(TOK30_temp) == '?..GMT.08.00'] <- 'GMT.08.00'
TOK30_temp$GMT.08.00=as.character(TOK30_temp$GMT.08.00)
TOK30_temp$GMT.08.00=as.POSIXct(TOK30_temp$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
TOK30_temp$InstrumentType="CONDUCTIVITY"
TOK30_conductivity=read.csv(file='TOK30_CONDUCTIVITY_S2022.csv',header=T)
TOK30_conductivity$SiteName="TOK30CONDUCTIVITY"
names(TOK30_conductivity)[names(TOK30_conductivity) == '?..GMT.08.00'] <- 'GMT.08.00'
TOK30_conductivity$GMT.08.00=as.character(TOK30_conductivity$GMT.08.00)
TOK30_conductivity$GMT.08.00=as.POSIXct(TOK30_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
TOK30_conductivity$InstrumentType="CONDUCTIVITY"
TOK30_light=read.csv(file='TOK30_LIGHT_S2022.csv', header=T)
names(TOK30_light)[names(TOK30_light) == '?..GMT.08.00'] <- 'GMT.08.00'
TOK30_light$GMT.08.00=as.character(TOK30_light$GMT.08.00)
TOK30_light$GMT.08.00=as.POSIXct(TOK30_light$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
names(TOK30_light)[names(TOK30_light) == 'Temperature'] <- 'AirTemperature'
TOK30_light$InstrumentType="LIGHTLOGGER"

#apply water temperature calibration correction. Light logger technically measures air temp, so wasn't included.
TOK30_minidot$Temperature=TOK30_minidot$Temperature +0.108495017
TOK30_temp$Temperature=TOK30_temp$Temperature -0.467026578
TOK30_conductivity$Temperature=TOK30_conductivity$Temperature-0.118322259



#add column specifying instrument's position on the chain.
TOK30_minidot$Position="Middle"
TOK30_temp$Position="Top"
TOK30_conductivity$Position="Bottom"
TOK30_light$Position="Surface"

#add columns that identify the position of the loggers
TOK30_temp$temp_0.28= TOK30_temp$Temperature
TOK30_minidot$temp_0.28=TOK30_minidot$Temperature
TOK30_minidot$DO_0.77=TOK30_minidot$Dissolved.Oxygen
TOK30_minidot$Sat_0.77=TOK30_minidot$Dissolved.Oxygen.Saturation
TOK30_conductivity$temp_1.34=TOK30_conductivity$Temperature
TOK30_conductivity$cond_1.34=TOK30_conductivity$Conductivity

#add logger serial
TOK30_temp$LoggerSerial=20636158
TOK30_minidot$LoggerSerial=895518
TOK30_conductivity$LoggerSerial=20636153

#combine instruments from this pond into one data frame
TOK30.1=merge(TOK30_minidot, TOK30_temp, all=T)
TOK30.2=merge(TOK30.1, TOK30_conductivity, all=T)
TOK30.3=merge(TOK30.2, TOK30_light, all=T)

#restrict dates to only include the sample time
TOK30.3=TOK30.3[with(TOK30.3, GMT.08.00 > "2022-06-04 10:30:00" & GMT.08.00 < "2022-10-06 09:00:00"),]

#add SiteName and SampleDate column
TOK30.3$SiteName="TOK30"
TOK30.3$SampleDate=as.Date(TOK30.3$GMT.08.00)






###TOPAZPOND - 3 instruments
#load data, remove unnecessary columns, fix date/time column
TOPAZPOND_minidot=read.csv(file='TOPAZPOND_MINIDOT_S2022_PONDEC.csv', header=T)
TOPAZPOND_minidot$SiteName="TOPAZPONDMINIDOT"
TOPAZPOND_minidot$Unix.Timestamp=NULL
TOPAZPOND_minidot$UTC_Date_Time=NULL
TOPAZPOND_minidot$Q=NULL
TOPAZPOND_minidot$Battery=NULL
TOPAZPOND_minidot$GMT.08.00=as.POSIXct(TOPAZPOND_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
TOPAZPOND_minidot$InstrumentType="MINIDOT"
TOPAZPOND_temp=read.csv(file='TOPAZPOND_TEMPERATURE_S2022.csv',header=T)
TOPAZPOND_temp$SiteName="TOPAZPONDTEMP"
names(TOPAZPOND_temp)[names(TOPAZPOND_temp) == '?..GMT.08.00'] <- 'GMT.08.00'
TOPAZPOND_temp$GMT.08.00=as.character(TOPAZPOND_temp$GMT.08.00)
TOPAZPOND_temp$GMT.08.00=as.POSIXct(TOPAZPOND_temp$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
TOPAZPOND_temp$InstrumentType="TEMPPRO"
TOPAZPOND_conductivity=read.csv(file='TOPAZPOND_CONDUCTIVITY_S2022.csv',header=T)
TOPAZPOND_conductivity$SiteName="TOPAZPONDCONDUCTIVITY"
names(TOPAZPOND_conductivity)[names(TOPAZPOND_conductivity) == '?..GMT.08.00'] <- 'GMT.08.00'
TOPAZPOND_conductivity$GMT.08.00=as.character(TOPAZPOND_conductivity$GMT.08.00)
TOPAZPOND_conductivity$GMT.08.00=as.POSIXct(TOPAZPOND_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
TOPAZPOND_conductivity$InstrumentType="CONDUCTIVITY"
TOPAZPOND_light=read.csv(file='TOPAZPOND_LIGHT_S2022.csv', header=T)
names(TOPAZPOND_light)[names(TOPAZPOND_light) == '?..GMT.08.00'] <- 'GMT.08.00'
TOPAZPOND_light$GMT.08.00=as.character(TOPAZPOND_light$GMT.08.00)
TOPAZPOND_light$GMT.08.00=as.POSIXct(TOPAZPOND_light$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
names(TOPAZPOND_light)[names(TOPAZPOND_light) == 'Temperature'] <- 'AirTemperature'
TOPAZPOND_light$InstrumentType="LIGHTLOGGER"

#apply water temperature calibration correction. Light logger technically measures air temp, so wasn't included.
TOPAZPOND_minidot$Temperature=TOPAZPOND_minidot$Temperature +0.12327907
TOPAZPOND_temp$Temperature=TOPAZPOND_temp$Temperature +0.069880399
TOPAZPOND_conductivity$Temperature=TOPAZPOND_conductivity$Temperature -0.41769103




#add column specifying instrument's position on the chain.
TOPAZPOND_minidot$Position="Middle"
TOPAZPOND_temp$Position="Top"
TOPAZPOND_conductivity$Position="Bottom"
TOPAZPOND_light$Position="Surface"

#add columns that identify the position of the loggers
TOPAZPOND_temp$temp_0.8 = TOPAZPOND_temp$Temperature
TOPAZPOND_minidot$temp_0.8=TOPAZPOND_minidot$Temperature
TOPAZPOND_minidot$DO_0.98=TOPAZPOND_minidot$Dissolved.Oxygen
TOPAZPOND_minidot$Sat_0.98=TOPAZPOND_minidot$Dissolved.Oxygen.Saturation
TOPAZPOND_conductivity$temp_1.66=TOPAZPOND_conductivity$Temperature
TOPAZPOND_conductivity$cond_1.66=TOPAZPOND_conductivity$Conductivity


#add logger serial
TOPAZPOND_temp$LoggerSerial=20868837
TOPAZPOND_minidot$LoggerSerial=865210
TOPAZPOND_conductivity$LoggerSerial=20636156


#combine instruments from this pond into one data frame
TOPAZPOND.1=merge(TOPAZPOND_minidot, TOPAZPOND_temp, all=T)
TOPAZPOND.2=merge(TOPAZPOND.1, TOPAZPOND_conductivity, all=T)
TOPAZPOND.3=merge(TOPAZPOND.2, TOPAZPOND_light, all=T)

#restrict dates to only include the sample time
TOPAZPOND.3=TOPAZPOND.3[with(TOPAZPOND.3, GMT.08.00 > "2021-06-06 12:00:00" & GMT.08.00 < "2022-10-03 10:00:00"),]

#add SiteName and SampleDate column
TOPAZPOND.3$SiteName="TOPAZPOND"
TOPAZPOND.3$SampleDate=as.Date(TOPAZPOND.3$GMT.08.00)




# Create a list with the data frames (each pond stored separately)
data_list <- list(EMLPOND1_S22=eml1.2, TOK11_S22=TOK11.2, TOK30_S22=TOK30.3, TOPAZPOND_S22=TOPAZPOND.3)
#data_list2 <- list(TOK30_S22=TOK30.3, TOPAZPOND_S22=TOPAZPOND.3)

# Save the list as an .rdata file
save(data_list, file = "S2022_Ponds_tempcorrected.rdata")
#save(data_list2, file = "S21_S22_Ponds_tempcorrected.rdata")

setwd("C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data/ponds")

# Save each data frame in the list as an individual .rdata file
for (name in names(data_list)) {
  assign(name, data_list[[name]])
  file_name <- paste0(name, ".rdata")
  save(list = name, file = file_name)
  rm(list = name)
}

setwd("C:/Users/maryj/Documents/R/SierraPonds/RawLoggerData")





#merge all the instruments/ponds together into one big dataframe to prepare for plotting
merge1=merge(eml1.3, TOK11.3, all=T)
merge2=merge(merge1, TOK30.3, all=T)
merge3=merge(merge2, TOPAZPOND.3, all=T)


#export csv of cleaned, temp corrected 2022 summer thermistor data
write.csv(merge3,"S2022_PondsAll_tempcorrected.csv")
