#this code is to produce a csv that contains all instrument data from ponds in summer 2021, with the following cleaning:
#1. Position of instrument on chain was added as a column
#2. Naming of variables and column names were standardized
#3. Data was cut to relevant time frames (some loggers start/end logging when not in the water, I trimmed to just the times they were supposed to be actually deployed in the water column.)
#4. Intercalibrations were applied, see in-line comments for each value for each instrument. Code for determining intercalibrations is in file named TemperatureIntercalibrations_Summer2021.R


setwd("C:/Users/maryj/Documents/R/SierraPonds/RawLoggerData")
library(ggplot2)
library(lubridate)
library(dplyr)
library(cowplot) #use plot_grid(), can specify columns with ncol=x. allows you to combine multiple ggplots into a single figure.

#TOKOPAH - EMLPOND1; TOK 11
#BOUNDARY - BP1; IR32; BP5
#IRELAND - IR31; IR32; IR11

#########TOKOPAH##############
##EMLPOND1 - 3 INSTRUMENTS

#load data by pond, and change format of columns to make uniform/change date format to POSIX 

###eml pond 1 - 3 instruments
#load data, remove unnecessary columns, fix date/time column
eml1_minidot=read.csv(file='EMLPOND1_MINIDOT_S2021.csv', header=T)
  eml1_minidot$SiteName="EML1MINIDOT"
  eml1_minidot$Unix.Timestamp=NULL
  eml1_minidot$UTC_Date_Time=NULL
  eml1_minidot$Q=NULL
  eml1_minidot$Battery=NULL
  eml1_minidot$GMT.08.00=as.POSIXct(eml1_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
  eml1_minidot$InstrumentType="MINIDOT"
eml1_temp=read.csv(file='EMLPOND1_TEMPERATURE_S2021.csv',header=T)
  eml1_temp$SiteName="EML1TEMP"
  names(eml1_temp)[names(eml1_temp) == '?..GMT.08.00'] <- 'GMT.08.00'
  eml1_temp$GMT.08.00=as.character(eml1_temp$GMT.08.00)
  eml1_temp$GMT.08.00=as.POSIXct(eml1_temp$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
  eml1_temp$InstrumentType="TEMPPRO"
eml1_conductivity=read.csv(file='EMLPOND1_CONDUCTIVITY_S2021.csv',header=T)
  eml1_conductivity$SiteName="EML1CONDUCTIVITY"
  names(eml1_conductivity)[names(eml1_conductivity) == '?..GMT.08.00'] <- 'GMT.08.00'
  eml1_conductivity$GMT.08.00=as.character(eml1_conductivity$GMT.08.00)
  eml1_conductivity$GMT.08.00=as.POSIXct(eml1_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
  eml1_conductivity$InstrumentType="CONDUCTIVITY"
eml1_light=read.csv(file='EMLPOND1_LIGHT_S2021.csv', header=T)
  eml1_light$GMT.08.00=as.character(eml1_light$GMT.08.00)
  eml1_light$GMT.08.00=as.POSIXct(eml1_light$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
  names(eml1_light)[names(eml1_light) == 'Temperature'] <- 'AirTemperature'
  eml1_light$InstrumentType="LIGHTLOGGER"
  
#apply water temperature calibration correction. Light logger technically measures air temp, so wasn't included.
eml1_minidot$Temperature=eml1_minidot$Temperature + 0.062833333
eml1_temp$Temperature=eml1_temp$Temperature  -0.372666666666667
eml1_conductivity$Temperature=eml1_conductivity$Temperature  -0.598000000000003





#add column specifying instrument's position on the chain.
eml1_minidot$Position="Middle"
eml1_temp$Position="Top"
eml1_conductivity$Position="Bottom"
eml1_light$Position="Surface"


#add logger serial
eml1_temp$LoggerSerial=20868832
eml1_minidot$LoggerSerial=549033
eml1_conductivity$LoggerSerial=20636144


#add columns that identify the position of the loggers
eml1_temp$temp_0.1= eml1_temp$Temperature
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
eml1.3=eml1.3[with(eml1.3, GMT.08.00 > "2021-06-01 12:00:00" & GMT.08.00 < "2021-10-31 14:00:00"),]

#add SiteName and SampleDate column
eml1.3$SiteName="EMLPOND1"
eml1.3$SampleDate=as.Date(eml1.3$GMT.08.00)




###TOK11 - 3 instruments
#load data, remove unnecessary columns, fix date/time column
TOK11_minidot=read.csv(file='TOK11_MINIDOT_S2021.csv', header=T)
  TOK11_minidot$SiteName="TOK11MINIDOT"
  TOK11_minidot$Unix.Timestamp=NULL
  TOK11_minidot$UTC_Date_Time=NULL
  TOK11_minidot$Q=NULL
  TOK11_minidot$Battery=NULL
  TOK11_minidot$GMT.08.00=as.POSIXct(TOK11_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
  TOK11_minidot$InstrumentType="MINIDOT"
TOK11_temp=read.csv(file='TOK11_TEMPERATURE_S2021.csv',header=T)
  TOK11_temp$SiteName="TOK11TEMP"
  names(TOK11_temp)[names(TOK11_temp) == '?..GMT.08.00'] <- 'GMT.08.00'
  TOK11_temp$GMT.08.00=as.character(TOK11_temp$GMT.08.00)
  TOK11_temp$GMT.08.00=as.POSIXct(TOK11_temp$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
  TOK11_temp$InstrumentType="TEMPPRO"
TOK11_conductivity=read.csv(file='TOK11_CONDUCTIVITY_S2021.csv',header=T)
  TOK11_conductivity$SiteName="TOK11CONDUCTIVITY"
  names(TOK11_conductivity)[names(TOK11_conductivity) == '?..GMT.08.00'] <- 'GMT.08.00'
  TOK11_conductivity$GMT.08.00=as.character(TOK11_conductivity$GMT.08.00)
  TOK11_conductivity$GMT.08.00=as.POSIXct(TOK11_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
  TOK11_conductivity$InstrumentType="CONDUCTIVITY"
TOK11_light=read.csv(file='TOK11_LIGHT_S2021.csv', header=T)
  names(TOK11_light)[names(TOK11_light) == '?..GMT.08.00'] <- 'GMT.08.00'
  TOK11_light$GMT.08.00=as.character(TOK11_light$GMT.08.00)
  TOK11_light$GMT.08.00=as.POSIXct(TOK11_light$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
  names(TOK11_light)[names(TOK11_light) == 'Temperature'] <- 'AirTemperature'
  TOK11_light$InstrumentType="LIGHTLOGGER"

#apply water temperature calibration correction. Light logger technically measures air temp, so wasn't included.
TOK11_minidot$Temperature=TOK11_minidot$Temperature +0.054
TOK11_temp$Temperature=TOK11_temp$Temperature - 0
TOK11_conductivity$Temperature=TOK11_conductivity$Temperature  -0.750666667



#add column specifying instrument's position on the chain.
TOK11_minidot$Position="Middle"
TOK11_minidot$LoggerSerial=
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
TOK11_temp$LoggerSerial=20868837
TOK11_minidot$LoggerSerial=865210
TOK11_conductivity$LoggerSerial=20438689

#combine instruments from this pond into one data frame
TOK11.1=merge(TOK11_minidot, TOK11_temp, all=T)
TOK11.2=merge(TOK11.1, TOK11_conductivity, all=T)
TOK11.3=merge(TOK11.2, TOK11_light, all=T)

#restrict dates to only include the sample time
TOK11.3=TOK11.3[with(TOK11.3, GMT.08.00 > "2021-06-01 17:00:00" & GMT.08.00 < "2021-10-31 11:00:00"),]

#add SiteName and SampleDate column
TOK11.3$SiteName="TOK11"
TOK11.3$SampleDate=as.Date(TOK11.3$GMT.08.00)







###TOK30 - 3 instruments
#load data, remove unnecessary columns, fix date/time column
TOK30_minidot=read.csv(file='TOK30_MINIDOT_SW2021.csv', header=T)
TOK30_minidot$SiteName="TOK30MINIDOT"
TOK30_minidot$Unix.Timestamp=NULL
TOK30_minidot$UTC_Date_Time=NULL
TOK30_minidot$Q=NULL
TOK30_minidot$Battery=NULL
TOK30_minidot$GMT.08.00=as.POSIXct(TOK30_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
TOK30_minidot$InstrumentType="MINIDOT"
TOK30_temp=read.csv(file='TOK30_TEMPERATURE_SW2021.csv',header=T)
TOK30_temp$SiteName="TOK30TEMP"
names(TOK30_temp)[names(TOK30_temp) == '?..GMT.08.00'] <- 'GMT.08.00'
TOK30_temp$GMT.08.00=as.character(TOK30_temp$GMT.08.00)
TOK30_temp$GMT.08.00=as.POSIXct(TOK30_temp$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
TOK30_temp$InstrumentType="TEMPPRO"
TOK30_conductivity=read.csv(file='TOK30_CONDUCTIVITY_SW2021.csv',header=T)
TOK30_conductivity$SiteName="TOK30CONDUCTIVITY"
names(TOK30_conductivity)[names(TOK30_conductivity) == '?..GMT.08.00'] <- 'GMT.08.00'
TOK30_conductivity$GMT.08.00=as.character(TOK30_conductivity$GMT.08.00)
TOK30_conductivity$GMT.08.00=as.POSIXct(TOK30_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
TOK30_conductivity$InstrumentType="CONDUCTIVITY"
TOK30_light=read.csv(file='TOK30_LIGHT_SW2021.csv', header=T)
names(TOK30_light)[names(TOK30_light) == '?..GMT.08.00'] <- 'GMT.08.00'
TOK30_light$GMT.08.00=as.character(TOK30_light$GMT.08.00)
TOK30_light$GMT.08.00=as.POSIXct(TOK30_light$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
names(TOK30_light)[names(TOK30_light) == 'Temperature'] <- 'AirTemperature'
TOK30_light$InstrumentType="LIGHTLOGGER"

#apply water temperature calibration correction. Light logger technically measures air temp, so wasn't included.
TOK30_minidot$Temperature=TOK30_minidot$Temperature -0.076111111

TOK30_temp$Temperature=TOK30_temp$Temperature +0.068416667

TOK30_conductivity$Temperature=TOK30_conductivity$Temperature - 0.917444444


#add column specifying instrument's position on the chain.
TOK30_minidot$Position="Middle"
TOK30_temp$Position="Top"
TOK30_conductivity$Position="Bottom"
TOK30_light$Position="Surface"

#add columns that identify the position of the loggers
TOK30_temp$temp_0.28= TOK30_temp$Temperature
TOK30_minidot$temp_0.77=TOK30_minidot$Temperature
TOK30_minidot$DO_0.77=TOK30_minidot$Dissolved.Oxygen
TOK30_minidot$Sat_0.77=TOK30_minidot$Dissolved.Oxygen.Saturation
TOK30_conductivity$temp_1.34=TOK30_conductivity$Temperature
TOK30_conductivity$cond_1.34=TOK30_conductivity$Conductivity


#add logger serial
TOK30_temp$LoggerSerial=20868838
TOK30_minidot$LoggerSerial=979330
TOK30_conductivity$LoggerSerial=20636145

#combine instruments from this pond into one data frame
TOK30.1=merge(TOK30_minidot, TOK30_temp, all=T)
TOK30.2=merge(TOK30.1, TOK30_conductivity, all=T)
TOK30.3=merge(TOK30.2, TOK30_light, all=T)

#restrict dates to only include the sample time
TOK30.3=TOK30.3[with(TOK30.3, GMT.08.00 > "2021-06-06 10:30:00" & GMT.08.00 < "2022-06-03 09:00:00"),]

#add SiteName and SampleDate column
TOK30.3$SiteName="TOK30"
TOK30.3$SampleDate=as.Date(TOK30.3$GMT.08.00)






###TOPAZPOND - 3 instruments
#load data, remove unnecessary columns, fix date/time column
TOPAZPOND_minidot=read.csv(file='TOPAZPOND_MINIDOT_SW2021.csv', header=T)
TOPAZPOND_minidot$SiteName="TOPAZPONDMINIDOT"
TOPAZPOND_minidot$Unix.Timestamp=NULL
TOPAZPOND_minidot$UTC_Date_Time=NULL
TOPAZPOND_minidot$Q=NULL
TOPAZPOND_minidot$Battery=NULL
TOPAZPOND_minidot$GMT.08.00=as.POSIXct(TOPAZPOND_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
TOPAZPOND_minidot$InstrumentType="MINIDOT"
TOPAZPOND_temp=read.csv(file='TOPAZPOND_TEMPERATURE_SW2021.csv',header=T)
TOPAZPOND_temp$SiteName="TOPAZPONDTEMP"
names(TOPAZPOND_temp)[names(TOPAZPOND_temp) == '?..GMT.08.00'] <- 'GMT.08.00'
TOPAZPOND_temp$GMT.08.00=as.character(TOPAZPOND_temp$GMT.08.00)
TOPAZPOND_temp$GMT.08.00=as.POSIXct(TOPAZPOND_temp$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
TOPAZPOND_temp$InstrumentType="TEMPPRO"
TOPAZPOND_conductivity=read.csv(file='TOPAZPOND_CONDUCTIVITY_SW2021.csv',header=T)
TOPAZPOND_conductivity$SiteName="TOPAZPONDCONDUCTIVITY"
names(TOPAZPOND_conductivity)[names(TOPAZPOND_conductivity) == '?..GMT.08.00'] <- 'GMT.08.00'
TOPAZPOND_conductivity$GMT.08.00=as.character(TOPAZPOND_conductivity$GMT.08.00)
TOPAZPOND_conductivity$GMT.08.00=as.POSIXct(TOPAZPOND_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
TOPAZPOND_conductivity$InstrumentType="CONDUCTIVITY"
TOPAZPOND_light=read.csv(file='TOPAZPOND_LIGHT_SW2021.csv', header=T)
names(TOPAZPOND_light)[names(TOPAZPOND_light) == '?..GMT.08.00'] <- 'GMT.08.00'
TOPAZPOND_light$GMT.08.00=as.character(TOPAZPOND_light$GMT.08.00)
TOPAZPOND_light$GMT.08.00=as.POSIXct(TOPAZPOND_light$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
names(TOPAZPOND_light)[names(TOPAZPOND_light) == 'Temperature'] <- 'AirTemperature'
TOPAZPOND_light$InstrumentType="LIGHTLOGGER"

#apply water temperature calibration correction. Light logger technically measures air temp, so wasn't included.
TOPAZPOND_minidot$Temperature=TOPAZPOND_minidot$Temperature +0.048638889

TOPAZPOND_temp$Temperature=TOPAZPOND_temp$Temperature +0

TOPAZPOND_conductivity$Temperature=TOPAZPOND_conductivity$Temperature +2.672277778



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
TOPAZPOND_temp$LoggerSerial=20868844
TOPAZPOND_minidot$LoggerSerial=966413
TOPAZPOND_conductivity$LoggerSerial=20438693

#combine instruments from this pond into one data frame
TOPAZPOND.1=merge(TOPAZPOND_minidot, TOPAZPOND_temp, all=T)
TOPAZPOND.2=merge(TOPAZPOND.1, TOPAZPOND_conductivity, all=T)
TOPAZPOND.3=merge(TOPAZPOND.2, TOPAZPOND_light, all=T)

#restrict dates to only include the sample time
TOPAZPOND.3=TOPAZPOND.3[with(TOPAZPOND.3, GMT.08.00 > "2021-06-06 12:00:00" & GMT.08.00 < "2022-06-03 10:00:00"),]

#add SiteName and SampleDate column
TOPAZPOND.3$SiteName="TOPAZPOND"
TOPAZPOND.3$SampleDate=as.Date(TOPAZPOND.3$GMT.08.00)




##########################BOUNDARY####################
###BP1 - 2 instruments
#load data, remove unnecessary columns, fix date/time column
BP1_minidot=read.csv(file='BP1_MINIDOT_S2021.csv', header=T)
  BP1_minidot$SiteName="BP1MINIDOT"
  BP1_minidot$Unix.Timestamp=NULL
  BP1_minidot$UTC_Date_Time=NULL
  BP1_minidot$Q=NULL
  BP1_minidot$Battery=NULL
  BP1_minidot$GMT.08.00=as.POSIXct(BP1_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
  BP1_minidot$InstrumentType="MINIDOT"
BP1_conductivity=read.csv(file='BP1_CONDUCTIVITY_S2021.csv',header=T)
  BP1_conductivity$SiteName="BP1CONDUCTIVITY"
  names(BP1_conductivity)[names(BP1_conductivity) == '?..GMT.08.00'] <- 'GMT.08.00'
  BP1_conductivity$GMT.08.00=as.character(BP1_conductivity$GMT.08.00)
  BP1_conductivity$GMT.08.00=as.POSIXct(BP1_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
  BP1_conductivity$InstrumentType="CONDUCTIVITY"


#apply water temperature calibration correction. Light logger technically measures air temp, so wasn't included.
BP1_minidot$Temperature=BP1_minidot$Temperature + 0.252617
BP1_conductivity$Temperature=BP1_conductivity$Temperature - 0



#add column specifying instrument's position on the chain.
BP1_minidot$Position="Top"
BP1_conductivity$Position="Bottom"

#add columns that identify the position of the loggers
BP1_minidot$temp_0.1=BP1_minidot$Temperature
BP1_minidot$DO_0.1=BP1_minidot$Dissolved.Oxygen
BP1_minidot$Sat_0.1=BP1_minidot$Dissolved.Oxygen.Saturation
BP1_conductivity$temp_3.4=BP1_conductivity$Temperature
BP1_conductivity$cond_3.4=BP1_conductivity$Conductivity


#add logger serial
BP1_minidot$LoggerSerial=332834
BP1_conductivity$LoggerSerial=20636153



#combine instruments from this pond into one data frame
BP1.1=merge(BP1_minidot, BP1_conductivity, all=T)


#restrict dates to only include the sample time
BP1.1=BP1.1[with(BP1.1, GMT.08.00 > "2021-07-13 12:00:00" & GMT.08.00 < "2021-10-28 08:00:00"),]

#add SiteName and SampleDate column
BP1.1$SiteName="BP1"
BP1.1$SampleDate=as.Date(BP1.1$GMT.08.00)





###BAYPOND - 2 instruments
#load data, remove unnecessary columns, fix date/time column
BAYPOND_minidot=read.csv(file='BAYPOND_MINIDOT_S2021.csv', header=T)
  BAYPOND_minidot$SiteName="BAYPONDMINIDOT"
  BAYPOND_minidot$Unix.Timestamp=NULL
  BAYPOND_minidot$UTC_Date_Time=NULL
  BAYPOND_minidot$Q=NULL
  BAYPOND_minidot$Battery=NULL
  BAYPOND_minidot$GMT.08.00=as.POSIXct(BAYPOND_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
  BAYPOND_minidot$InstrumentType="MINIDOT"
BAYPOND_conductivity=read.csv(file='BAYPOND_CONDUCTIVITY_S2021.csv',header=T)
  BAYPOND_conductivity$SiteName="BAYPONDCONDUCTIVITY"
  names(BAYPOND_conductivity)[names(BAYPOND_conductivity) == '?..GMT.08.00'] <- 'GMT.08.00'
  BAYPOND_conductivity$GMT.08.00=as.character(BAYPOND_conductivity$GMT.08.00)
  BAYPOND_conductivity$GMT.08.00=as.POSIXct(BAYPOND_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
  BAYPOND_conductivity$InstrumentType="CONDUCTIVITY"


#apply water temperature calibration correction. Light logger technically measures air temp, so wasn't included.
BAYPOND_minidot$Temperature=BAYPOND_minidot$Temperature + 0.257513
BAYPOND_conductivity$Temperature=BAYPOND_conductivity$Temperature - 0.39303


#add column specifying instrument's position on the chain.
BAYPOND_minidot$Position="Top"
BAYPOND_conductivity$Position="Bottom"

#add columns that identify the position of the loggers
BAYPOND_minidot$temp_0.1=BAYPOND_minidot$Temperature
BAYPOND_minidot$DO_0.1=BAYPOND_minidot$Dissolved.Oxygen
BAYPOND_minidot$Sat_0.1=BAYPOND_minidot$Dissolved.Oxygen.Saturation
BAYPOND_conductivity$temp_2.2=BAYPOND_conductivity$Temperature
BAYPOND_conductivity$cond_2.2=BAYPOND_conductivity$Conductivity


#add logger serial
BAYPOND_minidot$LoggerSerial=565424
BAYPOND_conductivity$LoggerSerial=20636156

#combine instruments from this pond into one data frame
BAYPOND.1=merge(BAYPOND_minidot, BAYPOND_conductivity, all=T)


#restrict dates to only include the sample time
BAYPOND.1=BAYPOND.1[with(BAYPOND.1, GMT.08.00 > "2021-07-13 14:00:00" & GMT.08.00 < "2021-10-28 07:00:00"),]

#add SiteName and SampleDate column
BAYPOND.1$SiteName="BAYPOND"
BAYPOND.1$SampleDate=as.Date(BAYPOND.1$GMT.08.00)




###BP5 - 3 instruments
#load data, remove unnecessary columns, fix date/time column
BP5_minidot=read.csv(file='BP5_MINIDOT_S2021.csv', header=T)
  BP5_minidot$SiteName="BP5MINIDOT"
  BP5_minidot$Unix.Timestamp=NULL
  BP5_minidot$UTC_Date_Time=NULL
  BP5_minidot$Q=NULL
  BP5_minidot$Battery=NULL
  BP5_minidot$GMT.08.00=as.POSIXct(BP5_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
  BP5_minidot$InstrumentType="MINIDOT"
BP5_conductivity=read.csv(file='BP5_CONDUCTIVITY_S2021.csv',header=T)
  BP5_conductivity$SiteName="BP5CONDUCTIVITY"
  names(BP5_conductivity)[names(BP5_conductivity) == '?..GMT.08.00'] <- 'GMT.08.00'
  BP5_conductivity$GMT.08.00=as.character(BP5_conductivity$GMT.08.00)
  BP5_conductivity$GMT.08.00=as.POSIXct(BP5_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
  BP5_conductivity$InstrumentType="CONDUCTIVITY"
BP5_light=read.csv(file='BP5_LIGHT_S2021.csv', header=T)
  names(BP5_light)[names(BP5_light) == '?..GMT.08.00'] <- 'GMT.08.00'
  BP5_light$GMT.08.00=as.character(BP5_light$GMT.08.00)
  BP5_light$GMT.08.00=as.POSIXct(BP5_light$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
  names(BP5_light)[names(BP5_light) == 'Temperature'] <- 'AirTemperature'
  BP5_light$InstrumentType="LIGHTLOGGER"
  

#apply water temperature calibration correction. Light logger technically measures air temp, so wasn't included.
BP5_minidot$Temperature=BP5_minidot$Temperature + 0.252617
BP5_conductivity$Temperature=BP5_conductivity$Temperature - 0.39303


#add column specifying instrument's position on the chain.
BP5_minidot$Position="Top"
BP5_conductivity$Position="Bottom"
BP5_light$Position="Surface"

#add columns that identify the position of the loggers
BP5_minidot$temp_0.1=BP5_minidot$Temperature
BP5_minidot$DO_0.1=BP5_minidot$Dissolved.Oxygen
BP5_minidot$Sat_0.1=BP5_minidot$Dissolved.Oxygen.Saturation
BP5_conductivity$temp_1.8=BP5_conductivity$Temperature
BP5_conductivity$cond_1.8=BP5_conductivity$Conductivity


#add logger serial
BP5_minidot$LoggerSerial=895518
BP5_conductivity$LoggerSerial=20636158



#combine instruments from this pond into one data frame
BP5.1=merge(BP5_minidot, BP5_conductivity, all=T)
BP5.2=merge(BP5.1, BP5_light, all=T)


#restrict dates to only include the sample time
BP5.2=BP5.2[with(BP5.2, GMT.08.00 > "2021-07-13 15:00:00" & GMT.08.00 < "2021-10-28 07:00:00"),]

#add SiteName and SampleDate column
BP5.2$SiteName="BP5"
BP5.2$SampleDate=as.Date(BP5.2$GMT.08.00)




###############IRELAND#############

###IR31 - 3 instruments
#load data, remove unnecessary columns, fix date/time column
IR31_minidot=read.csv(file='IR31_MINIDOT_S2021.csv', header=T)
  IR31_minidot$SiteName="IR31MINIDOT"
  IR31_minidot$Unix.Timestamp=NULL
  IR31_minidot$UTC_Date_Time=NULL
  IR31_minidot$Q=NULL
  IR31_minidot$Battery=NULL
  IR31_minidot$GMT.08.00=as.POSIXct(IR31_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
  IR31_minidot$InstrumentType='MINIDOT'
IR31_conductivity=read.csv(file='IR31_CONDUCTIVITY_S2021.csv',header=T)
  IR31_conductivity$SiteName="IR31CONDUCTIVITY"
  names(IR31_conductivity)[names(IR31_conductivity) == '?..GMT.08.00'] <- 'GMT.08.00'
  IR31_conductivity$GMT.08.00=as.character(IR31_conductivity$GMT.08.00)
  IR31_conductivity$GMT.08.00=as.POSIXct(IR31_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
  IR31_conductivity$InstrumentType="CONDUCTIVITY"
IR31_light=read.csv(file='IR31_LIGHT_S2021.csv', header=T)
  names(IR31_light)[names(IR31_light) == '?..GMT.08.00'] <- 'GMT.08.00'
  IR31_light$GMT.08.00=as.character(IR31_light$GMT.08.00)
  IR31_light$GMT.08.00=as.POSIXct(IR31_light$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
  names(IR31_light)[names(IR31_light) == 'Temperature'] <- 'AirTemperature'
  IR31_light$InstrumentType="LIGHTLOGGER"


#apply water temperature calibration correction. Light logger technically measures air temp, so wasn't included.
IR31_minidot$Temperature=IR31_minidot$Temperature + 0
IR31_conductivity$Temperature=IR31_conductivity$Temperature - 0.66692


#add column specifying instrument's position on the chain.
IR31_minidot$Position="Top"
IR31_conductivity$Position="Bottom"
IR31_light$Position="Surface"

#add columns that identify the position of the loggers
IR31_minidot$temp_0.1=IR31_minidot$Temperature
IR31_minidot$DO_0.1=IR31_minidot$Dissolved.Oxygen
IR31_minidot$Sat_0.1=IR31_minidot$Dissolved.Oxygen.Saturation
IR31_conductivity$temp_4.2=IR31_conductivity$Temperature
IR31_conductivity$cond_4.2=IR31_conductivity$Conductivity


#add logger serial
IR31_minidot$LoggerSerial=50604
IR31_conductivity$LoggerSerial=20636157


#combine instruments from this pond into one data frame
IR31.1=merge(IR31_minidot, IR31_conductivity, all=T)
IR31.2=merge(IR31.1, IR31_light, all=T)


#restrict dates to only include the sample time
IR31.2=IR31.2[with(IR31.2, GMT.08.00 > "2021-08-03 12:00:00" & GMT.08.00 < "2021-10-17 08:00:00"),]

#add SiteName and SampleDate column
IR31.2$SiteName="IR31"
IR31.2$SampleDate=as.Date(IR31.2$GMT.08.00)




###IR32 - 2 instruments
#load data, remove unnecessary columns, fix date/time column
IR32_minidot=read.csv(file='IR32_MINIDOT_S2021.csv', header=T)
  IR32_minidot$SiteName="IR32MINIDOT"
  IR32_minidot$Unix.Timestamp=NULL
  IR32_minidot$UTC_Date_Time=NULL
  IR32_minidot$Q=NULL
  IR32_minidot$Battery=NULL
  IR32_minidot$GMT.08.00=as.POSIXct(IR32_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
  IR32_minidot$InstrumentType="MINIDOT"
IR32_conductivity=read.csv(file='IR32_CONDUCTIVITY_S2021.csv',header=T)
  IR32_conductivity$SiteName="IR32CONDUCTIVITY"
  names(IR32_conductivity)[names(IR32_conductivity) == '?..GMT.08.00'] <- 'GMT.08.00'
  IR32_conductivity$GMT.08.00=as.character(IR32_conductivity$GMT.08.00)
  IR32_conductivity$GMT.08.00=as.POSIXct(IR32_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
  IR32_conductivity$InstrumentType="CONDUCTIVITY"

#apply water temperature calibration correction. Light logger technically measures air temp, so wasn't included.
IR32_minidot$Temperature=IR32_minidot$Temperature - 0.00986842105263167
IR32_conductivity$Temperature=IR32_conductivity$Temperature - 0.56775


#add column specifying instrument's position on the chain.
IR32_minidot$Position="Top"
IR32_conductivity$Position="Bottom"

#add columns that identify the position of the loggers
IR32_minidot$temp_0.1=IR32_minidot$Temperature
IR32_minidot$DO_0.1=IR32_minidot$Dissolved.Oxygen
IR32_minidot$Sat_0.1=IR32_minidot$Dissolved.Oxygen.Saturation
IR32_conductivity$temp_0.95=IR32_conductivity$Temperature
IR32_conductivity$cond_0.95=IR32_conductivity$Conductivity


#add logger serial
IR32_minidot$LoggerSerial=468192
IR32_conductivity$LoggerSerial=20636154

#combine instruments from this pond into one data frame
IR32.1=merge(IR32_minidot, IR32_conductivity, all=T)


#restrict dates to only include the sample time
IR32.1=IR32.1[with(IR32.1, GMT.08.00 > "2021-07-13 14:00:00" & GMT.08.00 < "2021-10-28 07:00:00"),]

#add SiteName and SampleDate column
IR32.1$SiteName="IR32"
IR32.1$SampleDate=as.Date(IR32.1$GMT.08.00)





###IR11 - 2 instruments
#load data, remove unnecessary columns, fix date/time column
IR11_minidot=read.csv(file='IR11_MINIDOT_S2021.csv', header=T)
  IR11_minidot$SiteName="IR11MINIDOT"
  IR11_minidot$Unix.Timestamp=NULL
  IR11_minidot$UTC_Date_Time=NULL
  IR11_minidot$Q=NULL
  IR11_minidot$Battery=NULL
  IR11_minidot$GMT.08.00=as.POSIXct(IR11_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
  IR11_minidot$InstrumentType="MINIDOT"
IR11_conductivity=read.csv(file='IR11_CONDUCTIVITY_S2021.csv',header=T)
  IR11_conductivity$SiteName="IR11CONDUCTIVITY"
  names(IR11_conductivity)[names(IR11_conductivity) == '?..GMT.08.00'] <- 'GMT.08.00'
  IR11_conductivity$GMT.08.00=as.character(IR11_conductivity$GMT.08.00)
  IR11_conductivity$GMT.08.00=as.POSIXct(IR11_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
  IR11_conductivity$InstrumentType="CONDUCTIVITY"


#apply water temperature calibration correction. Light logger technically measures air temp, so wasn't included.
IR11_minidot$Temperature=IR11_minidot$Temperature +0.018579
IR11_conductivity$Temperature=IR11_conductivity$Temperature - 0.52442


#add column specifying instrument's position on the chain.
IR11_minidot$Position="Top"
IR11_conductivity$Position="Bottom"

#add columns that identify the position of the loggers
IR11_minidot$temp_0.1=IR11_minidot$Temperature
IR11_minidot$DO_0.1=IR11_minidot$Dissolved.Oxygen
IR11_minidot$Sat_0.1=IR11_minidot$Dissolved.Oxygen.Saturation
IR11_conductivity$temp_1.9=IR11_conductivity$Temperature
IR11_conductivity$cond_1.9=IR11_conductivity$Conductivity


#add logger serial
IR11_minidot$LoggerSerial=285695
IR11_conductivity$LoggerSerial=20649551


#combine instruments from this pond into one data frame
IR11.1=merge(IR11_minidot, IR11_conductivity, all=T)


#restrict dates to only include the sample time
IR11.1=IR11.1[with(IR11.1, GMT.08.00 > "2021-07-13 14:00:00" & GMT.08.00 < "2021-10-28 07:00:00"),]

#add SiteName and SampleDate column
IR11.1$SiteName="IR11"
IR11.1$SampleDate=as.Date(IR11.1$GMT.08.00)



# Create a list with the data frames (each pond stored separately)
data_list <- list(EMLPOND1_S21=eml1.3, TOK11_S21=TOK11.3, TOK30_S21=TOK30.3, TOPAZPOND_S21=TOPAZPOND.3, BP1_S21=BP1.1, BP5_S21=BP5.2, BAYPOND_S21=BAYPOND.1, IR31_S21=IR31.2, IR32_S21=IR32.1, IR11_S21=IR11.1)

# Save the list as an .rdata file
save(data_list, file = "S2021_Ponds_tempcorrected.rdata")

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
merge2=merge(merge1, BP1.1, all=T)
merge3=merge(merge2, BAYPOND.1, all=T)
merge4=merge(merge3, BP5.2, all=T)
merge5=merge(merge4, IR31.2, all=T)
merge6=merge(merge5, IR32.1, all=T)
merge7=merge(merge6, IR11.1, all=T)
merge8=merge(merge7, TOK30.3, all=T)
merge9=merge(merge8, TOPAZPOND.3, all=T)


#export csv of cleaned 2021 summer thermistor data
#write.csv(merge9,"S2021_PondsAll_tempcorrected.csv")
