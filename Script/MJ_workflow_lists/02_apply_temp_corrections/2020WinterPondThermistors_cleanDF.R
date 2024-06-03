#create a clean dataframe for winter 20-21 data in the tokopah

#this code is to produce a csv that contains all instrument data from ponds in winter 2020 - 2021, with the following cleaning:
#1. Position of instrument on chain was added as a column
#2. Naming of variables and column names were standardized
#3. Data was cut to relevant time frames (some loggers start/end logging when not in the water, I trimmed to just the times they were supposed to be actually deployed in the water column.)

#Still needs to be done:
#4. Intercalibrations were applied, see in-line comments for each value for each instrument. 

#three ponds: EMLPOND1, TOK11, TOPAZPOND

setwd("C:/Users/maryj/Documents/R/SierraPonds/RawLoggerData")
library(ggplot2)
library(lubridate)
library(dplyr)
library(cowplot) #use plot_grid(), can specify columns with ncol=x. allows you to combine multiple ggplots into a single figure.



###eml pond 1 - 3 instruments
#load data, remove unnecessary columns, fix date/time column
eml1_minidot=read.csv(file='EMLPOND1_MINIDOT_W2020.csv', header=T)
  eml1_minidot$SiteName="EML1MINIDOT"
  eml1_minidot$Unix.Timestamp=NULL
  eml1_minidot$UTC_Date_Time=NULL
  eml1_minidot$Q=NULL
  eml1_minidot$Battery=NULL
  eml1_minidot$GMT.08.00=as.POSIXct(eml1_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
  eml1_minidot$InstrumentType="MINIDOT"
  eml1_minidot$LoggerSerial=979330
 

eml1_conductivity=read.csv(file='EMLPOND1_CONDUCTIVITY_W2020.csv',header=T)
  eml1_conductivity$SiteName="EML1CONDUCTIVITY"
  names(eml1_conductivity)[names(eml1_conductivity) == '?..GMT.08.00'] <- 'GMT.08.00'
  eml1_conductivity$GMT.08.00=as.character(eml1_conductivity$GMT.08.00)
  eml1_conductivity$GMT.08.00=as.POSIXct(eml1_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
  eml1_conductivity$InstrumentType="CONDUCTIVITY"
  eml1_conductivity$LoggerSerial = 20636145

  
eml1_light=read.csv(file='EMLPOND1_LIGHT_W2020.csv', header=T)
  names(eml1_light)[names(eml1_light) == '?..GMT.08.00'] <- 'GMT.08.00'
  eml1_light$GMT.08.00=as.character(eml1_light$GMT.08.00)
  eml1_light$GMT.08.00=as.POSIXct(eml1_light$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
  names(eml1_light)[names(eml1_light) == 'Temperature'] <- 'AirTemperature'
  eml1_light$InstrumentType="LIGHTLOGGER"
  eml1_light$LoggerSerial= 20874708


#apply water temperature calibration correction. Light logger technically measures air temp, so wasn't included.
eml1_minidot$Temperature=eml1_minidot$Temperature -  0
eml1_conductivity$Temperature=eml1_conductivity$Temperature -2.52533913



#add column specifying instrument's position on the chain.
eml1_minidot$Position="Top"
eml1_conductivity$Position="Bottom"
eml1_light$Position="Surface"

#add columns that identify the position of the loggers
eml1_minidot$temp_1.6=eml1_minidot$Temperature
eml1_minidot$DO_1.6=eml1_minidot$Dissolved.Oxygen
eml1_minidot$Sat_1.6=eml1_minidot$Dissolved.Oxygen.Saturation
eml1_conductivity$temp_2.6=eml1_conductivity$Temperature
eml1_conductivity$cond_2.6=eml1_conductivity$Conductivity


#combine instruments from this pond into one data frame
eml1.1=merge(eml1_minidot, eml1_conductivity, all=T)
eml1.2=merge(eml1.1, eml1_light, all=T)

#restrict dates to only include the sample time
eml1.2=eml1.2[with(eml1.2, GMT.08.00 > "2020-10-11 00:00:00" & GMT.08.00 < "2021-06-01 00:00:00"),]

#add SiteName and SampleDate column
eml1.2$SiteName="EMLPOND1"
eml1.2$SampleDate=as.Date(eml1.2$GMT.08.00)




###TOK11 - 3 instruments
#load data, remove unnecessary columns, fix date/time column
TOK11_minidot=read.csv(file='TOK11_MINIDOT_W2020.csv', header=T)
  TOK11_minidot$SiteName="TOK11MINIDOT"
  TOK11_minidot$Unix.Timestamp=NULL
  TOK11_minidot$UTC_Date_Time=NULL
  TOK11_minidot$Q=NULL
  TOK11_minidot$Battery=NULL
  TOK11_minidot$GMT.08.00=as.POSIXct(TOK11_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
  TOK11_minidot$InstrumentType="MINIDOT"
  TOK11_minidot$LoggerSerial=442116

  
TOK11_conductivity=read.csv(file='TOK11_CONDUCTIVITY_W2020.csv',header=T)
  TOK11_conductivity$SiteName="TOK11CONDUCTIVITY"
  names(TOK11_conductivity)[names(TOK11_conductivity) == '?..GMT.08.00'] <- 'GMT.08.00'
  TOK11_conductivity$GMT.08.00=as.character(TOK11_conductivity$GMT.08.00)
  TOK11_conductivity$GMT.08.00=as.POSIXct(TOK11_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
  TOK11_conductivity$InstrumentType="CONDUCTIVITY"
  TOK11_conductivity$LoggerSerial=20438693

  
TOK11_light=read.csv(file='TOK11_LIGHT_W2020.csv', header=T)
  names(TOK11_light)[names(TOK11_light) == '?..GMT.08.00'] <- 'GMT.08.00'
  TOK11_light$GMT.08.00=as.character(TOK11_light$GMT.08.00)
  TOK11_light$GMT.08.00=as.POSIXct(TOK11_light$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
  names(TOK11_light)[names(TOK11_light) == 'Temperature'] <- 'AirTemperature'
  TOK11_light$InstrumentType="LIGHTLOGGER"
  TOK11_light$LoggerSerial=20874699


#apply water temperature calibration correction. Light logger technically measures air temp, so wasn't included.
TOK11_minidot$Temperature=TOK11_minidot$Temperature  -0.009694215

TOK11_conductivity$Temperature=TOK11_conductivity$Temperature -2.141773913



#add column specifying instrument's position on the chain.
TOK11_minidot$Position="Top"
TOK11_conductivity$Position="Bottom"
TOK11_light$Position="Surface"


#add columns that identify the position of the loggers
TOK11_minidot$temp_0.5=TOK11_minidot$Temperature
TOK11_minidot$DO_0.5=TOK11_minidot$Dissolved.Oxygen
TOK11_minidot$Sat_0.5=TOK11_minidot$Dissolved.Oxygen.Saturation
TOK11_conductivity$temp_1.6=TOK11_conductivity$Temperature
TOK11_conductivity$cond_1.6=TOK11_conductivity$Conductivity

#combine instruments from this pond into one data frame
TOK11.1=merge(TOK11_minidot, TOK11_conductivity, all=T)
TOK11.2=merge(TOK11.1, TOK11_light, all=T)

#restrict dates to only include the sample time
TOK11.2=TOK11.2[with(TOK11.2, GMT.08.00 > "2020-10-13 00:00:00" & GMT.08.00 < "2021-06-01 00:00:00"),]

#add SiteName and SampleDate column
TOK11.2$SiteName="TOK11"
TOK11.2$SampleDate=as.Date(TOK11.2$GMT.08.00)




###TOPAZPOND - 3 instruments
#load data, remove unnecessary columns, fix date/time column
TOPAZPOND_minidot=read.csv(file='TOPAZPOND_MINIDOT_W2020.csv', header=T)
TOPAZPOND_minidot$SiteName="TOPAZPONDMINIDOT"
TOPAZPOND_minidot$Unix.Timestamp=NULL
TOPAZPOND_minidot$UTC_Date_Time=NULL
TOPAZPOND_minidot$Q=NULL
TOPAZPOND_minidot$Battery=NULL
TOPAZPOND_minidot$GMT.08.00=as.POSIXct(TOPAZPOND_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
TOPAZPOND_minidot$InstrumentType="MINIDOT"
TOPAZPOND_minidot$LoggerSerial=332834


TOPAZPOND_conductivity=read.csv(file='TOPAZPOND_CONDUCTIVITY_W2020.csv',header=T)
TOPAZPOND_conductivity$SiteName="TOPAZPONDCONDUCTIVITY"
names(TOPAZPOND_conductivity)[names(TOPAZPOND_conductivity) == '?..GMT.08.00'] <- 'GMT.08.00'
TOPAZPOND_conductivity$GMT.08.00=as.character(TOPAZPOND_conductivity$GMT.08.00)
TOPAZPOND_conductivity$GMT.08.00=as.POSIXct(TOPAZPOND_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
TOPAZPOND_conductivity$InstrumentType="CONDUCTIVITY"
TOPAZPOND_conductivity$LoggerSerial= 20636155


TOPAZPOND_light=read.csv(file='TOPAZPOND_LIGHT_W2020.csv', header=T)
names(TOPAZPOND_light)[names(TOPAZPOND_light) == '?..GMT.08.00'] <- 'GMT.08.00'
TOPAZPOND_light$GMT.08.00=as.character(TOPAZPOND_light$GMT.08.00)
TOPAZPOND_light$GMT.08.00=as.POSIXct(TOPAZPOND_light$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
names(TOPAZPOND_light)[names(TOPAZPOND_light) == 'Temperature'] <- 'AirTemperature'
TOPAZPOND_light$InstrumentType="LIGHTLOGGER"
TOPAZPOND_light$LoggerSerial= 20874700


#apply water temperature calibration correction. Light logger technically measures air temp, so wasn't included.
TOPAZPOND_minidot$Temperature=TOPAZPOND_minidot$Temperature +0.001115702

TOPAZPOND_conductivity$Temperature=TOPAZPOND_conductivity$Temperature -2.289426087



#add column specifying instrument's position on the chain.
TOPAZPOND_minidot$Position="Top"
TOPAZPOND_conductivity$Position="Bottom"
TOPAZPOND_light$Position="Surface"


#add columns that identify the position of the loggers
TOPAZPOND_minidot$temp_0.2=TOPAZPOND_minidot$Temperature
TOPAZPOND_minidot$DO_0.2=TOPAZPOND_minidot$Dissolved.Oxygen
TOPAZPOND_minidot$Sat_0.2=TOPAZPOND_minidot$Dissolved.Oxygen.Saturation
TOPAZPOND_conductivity$temp_1.2=TOPAZPOND_conductivity$Temperature
TOPAZPOND_conductivity$cond_1.2=TOPAZPOND_conductivity$Conductivity


#combine instruments from this pond into one data frame
TOPAZPOND.1=merge(TOPAZPOND_minidot, TOPAZPOND_conductivity, all=T)
TOPAZPOND.2=merge(TOPAZPOND.1, TOPAZPOND_light, all=T)

#restrict dates to only include the sample time
TOPAZPOND.2=TOPAZPOND.2[with(TOPAZPOND.2, GMT.08.00 > "2020-10-12 00:00:00" & GMT.08.00 < "2021-06-06 00:00:00"),]

#add SiteName and SampleDate column
TOPAZPOND.2$SiteName="TOPAZPOND"
TOPAZPOND.2$SampleDate=as.Date(TOPAZPOND.2$GMT.08.00)

# Create a list with the data frames
data_list <- list(EMLPOND1_W20 = eml1.2, TOK11_W20 = TOK11.2, TOPAZPOND_W20 = TOPAZPOND.2)

# Save the list as an .rdata file
save(data_list, file = "W2020_Ponds_tempcorrected.rdata")




merge1=merge(eml1.2, TOK11.2, all=T)
merge2=merge(merge1, TOPAZPOND.2, all=T)

#export csv of cleaned 2020 winter thermistor data
#write.csv(merge2,"W2020_PondsAll_tempcorrected.csv")



setwd("C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data/ponds")

# Save each data frame in the list as an individual .rdata file
for (name in names(data_list)) {
  assign(name, data_list[[name]])
  file_name <- paste0(name, ".rdata")
  save(list = name, file = file_name)
  rm(list = name)
}


