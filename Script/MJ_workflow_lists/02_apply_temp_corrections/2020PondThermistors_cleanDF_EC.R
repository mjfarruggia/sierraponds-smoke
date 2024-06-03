#this code cleans the thermistor data from 2020 and combines it into a single dataset ready for metabolism calculations
#this version includes elevation corrected minidot data

setwd("C:/Users/maryj/Documents/R/SierraPonds/RawLoggerData")
library(ggplot2)
library(lubridate)
library(dplyr)


#load temp data and combine surface and bottom (adding a column to distinguish which is which after rbind)
#load oxygen data; change date to POSIXct 

#EML Pond 2 - miniDOT, Conductivity, Pendant light logger
EMLPond2_ox=read.csv(file="EMLPOND2_MINIDOT_S2020_EC.csv", header=T)
EMLPond2_ox$GMT.08.00=as.POSIXct(EMLPond2_ox$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
EMLPond2_ox$Position="Top"
EMLPond2_ox$Unix.Timestamp=NULL
EMLPond2_ox$UTC_Date_Time=NULL
EMLPond2_ox$Battery=NULL
EMLPond2_ox$Q=NULL
EMLPond2_ox$InstrumentType="MINIDOT"
#apply temp calibration correction, EML2minidot = + 0.161427616
EMLPond2_ox$Temperature=EMLPond2_ox$Temperature + 0.161427616

EMLPond2_cond=read.csv(file="EMLPOND2_CONDUCTIVITY_S2020.csv", header=T)
names(EMLPond2_cond)[names(EMLPond2_cond) == 'Temp'] = 'Temperature'
names(EMLPond2_cond)[names(EMLPond2_cond) == "?..GMT.08.00"] = 'GMT.08.00'
EMLPond2_cond$GMT.08.00=as.character(EMLPond2_cond$GMT.08.00)
EMLPond2_cond$GMT.08.00=as.POSIXct(EMLPond2_cond$GMT.08.00, format='%m/%d/%Y %H:%M', tz="Etc/GMT-8")
EMLPond2_cond$Position = "Bottom"
EMLPond2_cond$InstrumentType="CONDUCTIVITY"
#apply temp calibration correction, EML2CONDUCTIVITY = -0.597637313
EMLPond2_cond$Temperature=EMLPond2_cond$Temperature - 0.597637313
EMLPond2_light=read.csv(file="EMLPOND2_LIGHT_S2020.csv", header=T)


#add columns that identify the position of the loggers
EMLPond2_ox$temp_0.19=EMLPond2_ox$Temperature
EMLPond2_ox$DO_0.19=EMLPond2_ox$Dissolved.Oxygen
EMLPond2_ox$Sat_0.19=EMLPond2_ox$Dissolved.Oxygen.Saturation
EMLPond2_cond$temp_0.8=EMLPond2_cond$Temperature
EMLPond2_cond$cond_0.8=EMLPond2_cond$Conductivity

#add logger serial
EMLPond2_ox$LoggerSerial=332834
EMLPond2_cond$LoggerSerial=20636145

#Combine temp data from DO and conductivity instruments

EMLPond2_temp=merge(EMLPond2_cond, EMLPond2_ox, all=T)

#select only the time deployed
EMLPond2_temp=EMLPond2_temp[with(EMLPond2_temp, GMT.08.00 > "2020-06-17 07:20:00" & GMT.08.00 < "2020-09-08 06:10:00"),]


#light data
names(EMLPond2_light)[names(EMLPond2_light) == "?..GMT.08.00"] = 'GMT.08.00'
names(EMLPond2_light)[names(EMLPond2_light) == "Temp"] = 'Air Temperature'
EMLPond2_light$GMT.08.00=as.character(EMLPond2_light$GMT.08.00)
EMLPond2_light$GMT.08.00=as.POSIXct(EMLPond2_light$GMT.08.00, format='%m/%d/%Y %H:%M','Etc/GMT-8')
EMLPond2_light=EMLPond2_light[with(EMLPond2_light, GMT.08.00 > "2020-06-30 09:15:00" & GMT.08.00 < "2020-09-08 06:10:00"),]
EMLPond2_light$Position="Surface"


#combine instruments from this pond into one data frame
EMLPOND2.1=merge(EMLPond2_temp, EMLPond2_light, all=T)




###########################################################
#Flat Pond - miniDOT, Conductivity, Pendant light logger
Flat_ox=read.csv(file="FLAT_MINIDOT_S2020_EC.csv", header=T)
Flat_ox$GMT.08.00=as.POSIXct(Flat_ox$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
Flat_ox$Temperature=Flat_ox$Temperature + 0.04559542
Flat_ox$Position="Top"
Flat_ox$Unix.Timestamp=NULL
Flat_ox$UTC_Date_Time=NULL
Flat_ox$Battery=NULL
Flat_ox$Q=NULL
Flat_ox$InstrumentType="MINIDOT"

Flat_cond=read.csv(file="FLAT_CONDUCTIVITY_S2020.csv", header=T)
names(Flat_cond)[names(Flat_cond) == 'Temp'] = 'Temperature'
Flat_cond$Temperature=Flat_cond$Temperature -0.128340261
names(Flat_cond)[names(Flat_cond) == "?..GMT.08.00"] = 'GMT.08.00'
Flat_cond$GMT.08.00=as.character(Flat_cond$GMT.08.00)
Flat_cond$GMT.08.00=as.POSIXct(Flat_cond$GMT.08.00, format='%m/%d/%Y %H:%M','Etc/GMT-8')
Flat_cond$Position = "Bottom"
Flat_cond$InstrumentType="CONDUCTIVITY"



#add columns that identify the position of the loggers
Flat_ox$temp_0.1=Flat_ox$Temperature
Flat_ox$DO_0.1=Flat_ox$Dissolved.Oxygen
Flat_ox$Sat_0.1=Flat_ox$Dissolved.Oxygen.Saturation
Flat_cond$temp_0.48=Flat_cond$Temperature
Flat_cond$cond_0.48=Flat_cond$Conductivity

#add logger serial
Flat_ox$LoggerSerial=50604
Flat_cond$LoggerSerial=20438693

#Combine temp data from DO and conductivity instruments

Flat_temp=merge(Flat_cond, Flat_ox, all=T)

Flat_light=read.csv(file="FLAT_LIGHT_S2020.csv", header=T)
#select only the time deployed
Flat_temp=Flat_temp[with(Flat_temp, GMT.08.00 > "2020-06-23 15:20:00" & GMT.08.00 < "2020-07-09 11:40:00"),]


#light data
names(Flat_light)[names(Flat_light) ==  "?..GMT.08.00"] = 'GMT.08.00'
names(Flat_light)[names(Flat_light) == "Temp"] = 'Air Temperature'
Flat_light$GMT.08.00=as.character(Flat_light$GMT.08.00)
Flat_light$GMT.08.00=as.POSIXct(Flat_light$GMT.08.00, format='%m/%d/%Y %H:%M','Etc/GMT-8')
Flat_light=Flat_light[with(Flat_light, GMT.08.00 > "2020-07-01 09:45:00" & GMT.08.00 < "2020-07-09 11:30:00"),]
Flat_light$Position="Surface"


#combine instruments from this pond into one data frame
FLAT.1=merge(Flat_temp, Flat_light, all=T)





############################################################
#Grouse Pond - miniDOT, Conductivity
Grouse_ox=read.csv(file="GROUSE_MINIDOT_S2020_EC.csv", header=T)
Grouse_ox$GMT.08.00=as.POSIXct(Grouse_ox$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
Grouse_ox$Temperature=Grouse_ox$Temperature +0.012229008
Grouse_ox$Position="Top"
Grouse_ox$Unix.Timestamp=NULL
Grouse_ox$UTC_Date_Time=NULL
Grouse_ox$Battery=NULL
Grouse_ox$Q=NULL
Grouse_ox$InstrumentType="MINIDOT"

Grouse_cond=read.csv(file="GROUSE_CONDUCTIVITY_S2020.csv", header=T)
names(Grouse_cond)[names(Grouse_cond) == 'Temp'] = 'Temperature'
Grouse_cond$Temperature=Grouse_cond$Temperature -0.418206107
names(Grouse_cond)[names(Grouse_cond) == "?..GMT.08.00"] = 'GMT.08.00'
Grouse_cond$GMT.08.00=as.character(Grouse_cond$GMT.08.00)
Grouse_cond$GMT.08.00=as.POSIXct(Grouse_cond$GMT.08.00, format='%m/%d/%Y %H:%M','Etc/GMT-8')
Grouse_cond$Position = "Bottom"
Grouse_cond$InstrumentType="CONDUCTIVITY"


#add columns that identify the position of the loggers
Grouse_ox$temp_0.25=Grouse_ox$Temperature
Grouse_ox$DO_0.25=Grouse_ox$Dissolved.Oxygen
Grouse_ox$Sat_0.25=Grouse_ox$Dissolved.Oxygen.Saturation
Grouse_cond$temp_0.43=Grouse_cond$Temperature
Grouse_cond$cond_0.43=Grouse_cond$Conductivity

#add logger serial
Grouse_ox$LoggerSerial=565424
Grouse_cond$LoggerSerial=20636153


#Combine temp data from DO and conductivity instruments

Grouse_temp=merge(Grouse_cond, Grouse_ox, all=T)

#select only the time deployed
Grouse_temp=Grouse_temp[with(Grouse_temp, GMT.08.00 > "2020-06-19 08:00:00" & GMT.08.00 < "2020-07-02 12:00:00"),]


#combine instruments from this pond into one data frame
GROUSE.1=Grouse_temp




####################################################################
#New Pond - miniDOT, Conductivity, light (light logger split with TOK 11 - deployed in New, then removed and redeployed in TOK11, no download in between)
New_ox=read.csv(file="NEW_MINIDOT_S2020_EC.csv", header=T)
New_ox$GMT.08.00=as.POSIXct(New_ox$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
New_ox$Temperature=New_ox$Temperature+0.030053435
New_ox$Position="Top"
New_ox$Unix.Timestamp=NULL
New_ox$UTC_Date_Time=NULL
New_ox$Battery=NULL
New_ox$Q=NULL
New_ox$InstrumentType="MINIDOT"

New_cond=read.csv(file="NEW_CONDUCTIVITY_S2020.csv", header=T)
names(New_cond)[names(New_cond) == 'Temp'] = 'Temperature'
New_cond$Temperature=New_cond$Temperature -0.521564885
names(New_cond)[names(New_cond) == "DateTime"] = 'GMT.08.00'
New_cond$GMT.08.00=as.character(New_cond$GMT.08.00)
New_cond$GMT.08.00=as.POSIXct(New_cond$GMT.08.00, format='%m/%d/%Y %H:%M','Etc/GMT-8')
New_cond$Position = "Bottom"
New_cond$InstrumentType="CONDUCTIVITY"


#add columns that identify the position of the loggers
New_ox$temp_0.1=New_ox$Temperature
New_ox$DO_0.1=New_ox$Dissolved.Oxygen
New_ox$Sat_0.1=New_ox$Dissolved.Oxygen.Saturation
New_cond$temp_0.48=New_cond$Temperature
New_cond$cond_0.48=New_cond$Conductivity

#add logger serial
New_ox$LoggerSerial=468192
New_cond$LoggerSerial=20636156



#Combine temp data from DO and conductivity instruments
New_temp=merge(New_cond, New_ox, all=T)

#select only the time deployed
New_temp=New_temp[with(New_temp, GMT.08.00 > "2020-06-18 16:20:00" & GMT.08.00 < "2020-07-09 12:00:00"),]

#light logger
New_light=read.csv(file="NEW_TOK11_LIGHT_S2020.csv", header=T)
names(New_light)[names(New_light) == "?..GMT.08.00"] = 'GMT.08.00'
names(New_light)[names(New_light) == "Temp"] = 'Air Temperature'
New_light$GMT.08.00=as.character(New_light$GMT.08.00)
New_light$GMT.08.00=as.POSIXct(New_light$GMT.08.00, format='%m/%d/%Y %H:%M','Etc/GMT-8')
New_light=New_light[with(New_light, GMT.08.00 > "2020-07-01 11:10:00" & GMT.08.00 < "2020-07-09 12:40:00"),]
New_light$Position="Surface"



#combine instruments from this pond into one data frame
NEW.1=merge(New_temp, New_light, all=T)



##################################################################
#Star Pond - miniDOT, Conductivity, Pendant
Star_ox=read.csv(file="STAR_MINIDOT_S2020_EC.csv", header=T)
Star_ox$GMT.08.00=as.POSIXct(Star_ox$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
Star_ox$Temperature=Star_ox$Temperature +0.014772564
Star_ox$Position="Top"
Star_ox$Unix.Timestamp=NULL
Star_ox$UTC_Date_Time=NULL
Star_ox$Battery=NULL
Star_ox$Q=NULL
Star_ox$InstrumentType="MINIDOT"

Star_cond=read.csv(file="STAR_CONDUCTIVITY_S2020.csv", header=T)
names(Star_cond)[names(Star_cond) == 'Temp'] = 'Temperature'
Star_cond$Temperature=Star_cond$Temperature -0.528816794
names(Star_cond)[names(Star_cond) == "?..GMT.08.00"] = 'GMT.08.00'
Star_cond$GMT.08.00=as.character(Star_cond$GMT.08.00)
Star_cond$GMT.08.00=as.POSIXct(Star_cond$GMT.08.00, format='%m/%d/%Y %H:%M','Etc/GMT-8')
Star_cond$Position = "Bottom"
Star_cond$InstrumentType="CONDUCTIVITY"

Star_light=read.csv(file="STAR_LIGHT_S2020.csv", header=T)


#add columns that identify the position of the loggers
Star_ox$temp_0.24=Star_ox$Temperature
Star_ox$DO_0.24=Star_ox$Dissolved.Oxygen
Star_ox$Sat_0.24=Star_ox$Dissolved.Oxygen.Saturation
Star_cond$temp_0.75=Star_cond$Temperature
Star_cond$cond_0.75=Star_cond$Conductivity
#add logger serial
Star_ox$LoggerSerial=979330
Star_cond$LoggerSerial=20636154


#Combine temp data from DO and conductivity instruments

Star_temp=merge(Star_cond, Star_ox, all=T)

#select only the time deployed
Star_temp=Star_temp[with(Star_temp, GMT.08.00 > "2020-06-24 08:10:00" & GMT.08.00 < "2020-08-11 14:00:00"),]


#light data
names(Star_light)[names(Star_light) == "?..GMT.08.00"] = 'GMT.08.00'
names(Star_light)[names(Star_light) == "Temp"] = 'Air Temperature'
Star_light$GMT.08.00=as.character(Star_light$GMT.08.00)
Star_light$GMT.08.00=as.POSIXct(Star_light$GMT.08.00, format='%m/%d/%Y %H:%M','Etc/GMT-8')
Star_light=Star_light[with(Star_light, GMT.08.00 > "2020-07-01 15:10:00" & GMT.08.00 < "2020-08-11 14:10:00"),]
Star_light$Position="Surface"

#combine instruments from this pond into one data frame
STAR.1=merge(Star_temp, Star_light, all=T)





####################################################################
#TOK71 Pond - miniDOT, Conductivity, Pendant
TOK71_ox=read.csv(file="TOK71_MINIDOT_S2020_EC.csv", header=T)
TOK71_ox$GMT.08.00=as.POSIXct(TOK71_ox$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
TOK71_ox$Temperature=TOK71_ox$Temperature +0.126953749
TOK71_ox$Position="Top"
TOK71_ox$Unix.Timestamp=NULL
TOK71_ox$UTC_Date_Time=NULL
TOK71_ox$Battery=NULL
TOK71_ox$Q=NULL
TOK71_ox$InstrumentType="MINIDOT"

TOK71_cond=read.csv(file="TOK71_CONDUCTIVITY_S2020.csv", header=T)
names(TOK71_cond)[names(TOK71_cond) == 'Temp'] = 'Temperature'
TOK71_cond$Temperature=TOK71_cond$Temperature-0.433396947
names(TOK71_cond)[names(TOK71_cond) == "?..GMT.08.00"] = 'GMT.08.00'
TOK71_cond$GMT.08.00=as.character(TOK71_cond$GMT.08.00)
TOK71_cond$GMT.08.00=as.POSIXct(TOK71_cond$GMT.08.00, format='%m/%d/%Y %H:%M','Etc/GMT-8')
TOK71_cond$Position = "Bottom"
TOK71_cond$InstrumentType="CONDUCTIVITY"

TOK71_light=read.csv(file="TOK71_LIGHT_S2020.csv", header=T)

#add columns that identify the position of the loggers
TOK71_ox$temp_0.19=TOK71_ox$Temperature
TOK71_ox$DO_0.19=TOK71_ox$Dissolved.Oxygen
TOK71_ox$Sat_0.19=TOK71_ox$Dissolved.Oxygen.Saturation
TOK71_cond$temp_0.8=TOK71_cond$Temperature
TOK71_cond$cond_0.8=TOK71_cond$Conductivity

#add logger serial
TOK71_ox$LoggerSerial=442116
TOK71_cond$LoggerSerial=20649551

#Combine temp data from DO and conductivity instruments

TOK71_temp=merge(TOK71_cond, TOK71_ox, all=T)

#select only the time deployed
TOK71_temp=TOK71_temp[with(TOK71_temp, GMT.08.00 > "2020-06-17 10:00:00" & GMT.08.00 < "2020-08-18 18:00:00"),]


#light data
names(TOK71_light)[names(TOK71_light) == "?..GMT.08.00"] = 'GMT.08.00'
names(TOK71_light)[names(TOK71_light) == "Temp"] = 'Air Temperature'
TOK71_light$GMT.08.00=as.character(TOK71_light$GMT.08.00)
TOK71_light$GMT.08.00=as.POSIXct(TOK71_light$GMT.08.00, format='%m/%d/%Y %H:%M','Etc/GMT-8')
TOK71_light=TOK71_light[with(TOK71_light, GMT.08.00 > "2020-06-30 07:15:00" & GMT.08.00 < "2020-08-18 18:10:00"),]
TOK71_light$Position="Surface"


#combine instruments from this pond into one data frame
TOK71.1=merge(TOK71_temp, TOK71_light, all=T)







### the next ponds have 3 loggers (plus pendant), with a top temp logger that switched out partway through the season - need to combine the top sensors for each pond...

#######################################
#EML Pond 1 - pendant plus 3 subsurface loggers; top logger switched out partway through season

#load first top logger
EML1_top=read.csv(file="EMLPOND1_MINIDOT_TOP_S2020_EC.csv", header=T)
EML1_top$GMT.08.00=as.character(EML1_top$GMT.08.00)
EML1_top$GMT.08.00=as.POSIXct(EML1_top$GMT.08.00, format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
EML1_top$Temperature=EML1_top$Temperature +0.059083969
EML1_top$InstrumentType="MINIDOT"
#select only the time deployed
EML1_top=EML1_top[with(EML1_top, GMT.08.00 > "2020-06-17 09:00:00" & GMT.08.00 < "2020-06-30 00:00:00"),]

#load second top logger
EML1_templogger=read.csv(file="EMLPOND1_TEMPERATURE_S2020.csv", header=T)
names(EML1_templogger)[names(EML1_templogger) == "?..GMT.08.00"] = 'GMT.08.00'
names(EML1_templogger)[names(EML1_templogger) == "Temp"] = 'Temperature'
EML1_templogger$Temperature=EML1_templogger$Temperature -0.065572519
EML1_templogger$GMT.08.00=as.character(EML1_templogger$GMT.08.00)
EML1_templogger$GMT.08.00=as.POSIXct(EML1_templogger$GMT.08.00, format='%m/%d/%Y %H:%M','Etc/GMT-8')
EML1_templogger$InstrumentType="TEMPPRO"
#select only the time deployed
EML1_templogger=EML1_templogger[with(EML1_templogger, GMT.08.00 > "2020-06-30 20:00:00" & GMT.08.00 < "2020-10-10 00:00:00"),]


EML1_temperature=merge(EML1_templogger, EML1_top, all=T)
EML1_temperature$Position = "Top"


EML1_ox=read.csv(file="EMLPOND1_MINIDOT_S2020_EC.csv", header=T)
EML1_ox$GMT.08.00=as.POSIXct(EML1_ox$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
EML1_ox$Temperature=EML1_ox$Temperature +0.034343511
EML1_ox$Position="Middle"
EML1_ox$Unix.Timestamp=NULL
EML1_ox$UTC_Date_Time=NULL
EML1_ox$Battery=NULL
EML1_ox$Q=NULL
EML1_ox$InstrumentType="MINIDOT"
EML1_ox=EML1_ox[with(EML1_ox, GMT.08.00 > "2020-06-17 09:00:00" & GMT.08.00 < "2020-10-10 00:00:00"),]


EML1_cond=read.csv(file="EMLPOND1_CONDUCTIVITY_S2020.csv", header=T)
names(EML1_cond)[names(EML1_cond) == 'Temp'] = 'Temperature'
EML1_cond$Temperature=EML1_cond$Temperature -0.92759542
names(EML1_cond)[names(EML1_cond) == "?..GMT.08.00"] = 'GMT.08.00'
EML1_cond$GMT.08.00=as.character(EML1_cond$GMT.08.00)
EML1_cond$GMT.08.00=as.POSIXct(EML1_cond$GMT.08.00, format='%m/%d/%Y %H:%M','Etc/GMT-8')
EML1_cond$Position = "Bottom"
EML1_cond$InstrumentType="CONDUCTIVITY"
EML1_cond=EML1_cond[with(EML1_cond, GMT.08.00 > "2020-06-17 09:00:00" & GMT.08.00 < "2020-10-10 00:00:00"),]


#add columns that identify the position of the loggers
EML1_temperature$temp_0.2= EML1_temperature$Temperature
EML1_ox$temp_1.58=EML1_ox$Temperature
EML1_ox$DO_1.58=EML1_ox$Dissolved.Oxygen
EML1_ox$Sat_1.58=EML1_ox$Dissolved.Oxygen.Saturation
EML1_cond$temp_2.8=EML1_cond$Temperature
EML1_cond$cond_2.8=EML1_cond$Conductivity

#add logger serial
EML1_temperature$LoggerSerial="895518, 20868844"
EML1_ox$LoggerSerial=939315
EML1_cond$LoggerSerial=20636144


#Combine temp data from DO and conductivity instruments

EML1_temp1=merge(EML1_cond, EML1_ox, all=T)
EML1_temp=merge(EML1_temp1, EML1_temperature, all=T)

#select only the time deployed
EML1_temp=EML1_temp[with(EML1_temp, GMT.08.00 > "2020-06-17 09:00:00" & GMT.08.00 < "2020-10-10 00:00:00"),]


ggplot(data=EML1_temp, aes(GMT.08.00, Temperature, colour=Position))+geom_line()

#remove the weird spike in temp on Jun 30 2020
EML1_temp = EML1_temp [!grepl("2020-06-30", EML1_temp$GMT.08.00),]

ggplot(data=EML1_temp, aes(GMT.08.00, Temperature, colour=Position))+geom_line()


#light data
EML1_light=read.csv(file="EMLPOND1_LIGHT_S2020.csv", header=T)
names(EML1_light)[names(EML1_light) == "?..GMT.08.00"] = 'GMT.08.00'
names(EML1_light)[names(EML1_light) == "Temp"] = 'Air Temperature'
EML1_light$GMT.08.00=as.character(EML1_light$GMT.08.00)
EML1_light$GMT.08.00=as.POSIXct(EML1_light$GMT.08.00, format='%m/%d/%Y %H:%M','Etc/GMT-8')
EML1_light=EML1_light[with(EML1_light, GMT.08.00 > "2020-06-30 09:00:00" & GMT.08.00 < "2020-10-10 16:00:00"),]
EML1_light$Position="Surface"


#combine instruments from this pond into one data frame
EML1.1=merge(EML1_temp, EML1_light, all=T)

###############################
#TOK11 - pendant plus 3 subsurface loggers; top logger switched out partway through season

#load first top logger
TOK11_top=read.csv(file="TOK11_MINIDOT_TOP_S2020_EC.csv", header=T)
TOK11_top$Temperature=TOK11_top$Temperature -0.037587786
TOK11_top$GMT.08.00=as.character(TOK11_top$GMT.08.00)
TOK11_top$GMT.08.00=as.POSIXct(TOK11_top$GMT.08.00, format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
#select only the time deployed
TOK11_top=TOK11_top[with(TOK11_top, GMT.08.00 > "2020-06-17 12:30:00" & GMT.08.00 < "2020-07-10 10:00:00"),]
TOK11_top$InstrumentType="MINIDOT"

#load second top logger
TOK11_templogger=read.csv(file="TOK11_TEMPERATURE_S2020.csv", header=T)
names(TOK11_templogger)[names(TOK11_templogger) == "?..GMT.08.00"] = 'GMT.08.00'
names(TOK11_templogger)[names(TOK11_templogger) == "Temp"] = 'Temperature'
TOK11_templogger$Temperature=TOK11_templogger$Temperature +0 #calibrated against this instrument, so no correction
TOK11_templogger$GMT.08.00=as.character(TOK11_templogger$GMT.08.00)
TOK11_templogger$GMT.08.00=as.POSIXct(TOK11_templogger$GMT.08.00, format='%m/%d/%Y %H:%M','Etc/GMT-8')
TOK11_templogger$InstrumentType="TEMPPRO"
#select only the time deployed
TOK11_templogger=TOK11_templogger[with(TOK11_templogger, GMT.08.00 > "2020-07-10 10:00:00" & GMT.08.00 < "2020-10-12 08:50:00"),]


TOK11_temperature=merge(TOK11_templogger, TOK11_top, all=T)
TOK11_temperature$Position = "Top"


TOK11_ox=read.csv(file="TOK11_MINIDOT_S2020_EC.csv", header=T)
TOK11_ox$GMT.08.00=as.POSIXct(TOK11_ox$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
TOK11_ox$Temperature=TOK11_ox$Temperature +0.030687023
TOK11_ox$Position="Middle"
TOK11_ox$Unix.Timestamp=NULL
TOK11_ox$UTC_Date_Time=NULL
TOK11_ox$Battery=NULL
TOK11_ox$Q=NULL
TOK11_ox$InstrumentType="MINIDOT"

TOK11_cond=read.csv(file="TOK11_CONDUCTIVITY_S2020.csv", header=T)
names(TOK11_cond)[names(TOK11_cond) == 'Temp'] = 'Temperature'
TOK11_cond$Temperature=TOK11_cond$Temperature -0.671412214
names(TOK11_cond)[names(TOK11_cond) == "?..GMT.08.00"] = 'GMT.08.00'
TOK11_cond$GMT.08.00=as.character(TOK11_cond$GMT.08.00)
TOK11_cond$GMT.08.00=as.POSIXct(TOK11_cond$GMT.08.00, format='%m/%d/%Y %H:%M','Etc/GMT-8')
TOK11_cond$Position = "Bottom"
TOK11_cond$InstrumentType="CONDUCTIVITY"

#add columns that identify the position of the loggers
TOK11_temperature$temp_0.22= TOK11_temperature$Temperature
TOK11_ox$temp_1.24=TOK11_ox$Temperature
TOK11_ox$DO_1.24=TOK11_ox$Dissolved.Oxygen
TOK11_ox$Sat_1.24=TOK11_ox$Dissolved.Oxygen.Saturation
TOK11_cond$temp_2.0=TOK11_cond$Temperature
TOK11_cond$cond_2.0=TOK11_cond$Conductivity

#add logger serial
TOK11_temperature$LoggerSerial="285695, 20868838"
TOK11_ox$LoggerSerial=089520
TOK11_cond$LoggerSerial=20636158


#Combine temp data from DO and conductivity instruments

TOK11_temp1=merge(TOK11_cond, TOK11_ox, all=T)
TOK11_temp=merge(TOK11_temp1, TOK11_temperature, all=T)


#select only the time deployed
TOK11_temp=TOK11_temp[with(TOK11_temp, GMT.08.00 > "2020-06-17 12:30:00" & GMT.08.00 < "2020-10-11 16:00:00"),]

ggplot(data=TOK11_temp, aes(GMT.08.00, Temperature, colour=Position))+geom_line()

#remove day with suspect points: 2020-07-10 10:30:00; 2020-07-10 11:38:00; 2020-07-10 10:40:00
TOK11_temp = TOK11_temp [!grepl("2020-07-10", TOK11_temp$GMT.08.00),]

ggplot(data=TOK11_temp, aes(GMT.08.00, Temperature, colour=Position))+geom_line()



#light data
TOK11_light=read.csv(file="TOK11_NEW_LIGHT_S2020.csv", header=T)
names(TOK11_light)[names(TOK11_light) == "?..GMT.08.00"] = 'GMT.08.00'
names(TOK11_light)[names(TOK11_light) == "Temp"] = 'Air Temperature'
TOK11_light$GMT.08.00=as.character(TOK11_light$GMT.08.00)
TOK11_light$GMT.08.00=as.POSIXct(TOK11_light$GMT.08.00, format='%m/%d/%Y %H:%M','Etc/GMT-8')
TOK11_light=TOK11_light[with(TOK11_light, GMT.08.00 > "2020-07-10 10:10:00" & GMT.08.00 < "2020-10-12 08:50:00"),]
TOK11_light$Position="Surface"


#combine instruments from this pond into one data frame
TOK11.1=merge(TOK11_temp, TOK11_light, all=T)


###########################################
#TOK30 - pendant plus 3 subsurface loggers; top logger switched out partway through season

#load first top logger
TOK30_top=read.csv(file="TOK30_MINIDOT_TOP_S2020_EC.csv", header=T)
TOK30_top$Temperature=TOK30_top$Temperature +0.047328244
TOK30_top$GMT.08.00=as.character(TOK30_top$GMT.08.00)
TOK30_top$GMT.08.00=as.POSIXct(TOK30_top$GMT.08.00, format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
TOK30_top$InstrumentType="MINIDOT"
#select only the time deployed
TOK30_top=TOK30_top[with(TOK30_top, GMT.08.00 > "2020-06-18 14:00:00" & GMT.08.00 < "2020-07-01 09:00:00"),]

#load second top logger
TOK30_templogger=read.csv(file="TOK30_TEMPERATURE_S2020.csv", header=T)
names(TOK30_templogger)[names(TOK30_templogger) == "?..GMT.08.00"] = 'GMT.08.00'
names(TOK30_templogger)[names(TOK30_templogger) == "Temp"] = 'Temperature'
TOK30_templogger$Temperature=TOK30_templogger$Temperature -0.024030534
TOK30_templogger$GMT.08.00=as.character(TOK30_templogger$GMT.08.00)
TOK30_templogger$GMT.08.00=as.POSIXct(TOK30_templogger$GMT.08.00, format='%m/%d/%Y %H:%M','Etc/GMT-8')
TOK30_templogger$InstrumentType="TEMPPRO"
#select only the time deployed
TOK30_templogger=TOK30_templogger[with(TOK30_templogger, GMT.08.00 > "2020-07-01 09:10:00" & GMT.08.00 < "2020-10-11 10:20:00"),]


TOK30_temperature=merge(TOK30_templogger, TOK30_top, all=T)
TOK30_temperature$Position = "Top"


TOK30_ox=read.csv(file="TOK30_MINIDOT_S2020_EC.csv", header=T)
TOK30_ox$GMT.08.00=as.POSIXct(TOK30_ox$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
TOK30_ox$Temperature=TOK30_ox$Temperature +0.01121374
TOK30_ox$Position="Middle"
TOK30_ox$Unix.Timestamp=NULL
TOK30_ox$UTC_Date_Time=NULL
TOK30_ox$Battery=NULL
TOK30_ox$Q=NULL
TOK30_ox$InstrumentType="MINIDOT"

TOK30_cond=read.csv(file="TOK30_CONDUCTIVITY_S2020.csv", header=T)
names(TOK30_cond)[names(TOK30_cond) == 'Temp'] = 'Temperature'
TOK30_cond$Temperature=TOK30_cond$Temperature -0.753396947
names(TOK30_cond)[names(TOK30_cond) == "?..GMT.08.00"] = 'GMT.08.00'
TOK30_cond$GMT.08.00=as.character(TOK30_cond$GMT.08.00)
TOK30_cond$GMT.08.00=as.POSIXct(TOK30_cond$GMT.08.00, format='%m/%d/%Y %H:%M','Etc/GMT-8')
TOK30_cond$Position = "Bottom"
TOK30_cond$InstrumentType="CONDUCTIVITY"


#add columns that identify the position of the loggers
TOK30_temperature$temp_0.28= TOK30_temperature$Temperature
TOK30_ox$temp_0.77=TOK30_ox$Temperature
TOK30_ox$DO_0.77=TOK30_ox$Dissolved.Oxygen
TOK30_ox$Sat_0.77=TOK30_ox$Dissolved.Oxygen.Saturation
TOK30_cond$temp_1.34=TOK30_cond$Temperature
TOK30_cond$cond_1.34=TOK30_cond$Conductivity

#add logger serial
TOK30_temperature$LoggerSerial="152972, 20868837"
TOK30_ox$LoggerSerial=543493
TOK30_cond$LoggerSerial=20636157


#Combine temp data from DO and conductivity instruments

TOK30_temp1=merge(TOK30_cond, TOK30_ox, all=T)
TOK30_temp=merge(TOK30_temp1, TOK30_temperature, all=T)


#select only the time deployed
TOK30_temp=TOK30_temp[with(TOK30_temp, GMT.08.00 > "2020-06-18 14:00:00" & GMT.08.00 < "2020-10-10 00:00:00"),]

ggplot(data=TOK30_temp, aes(GMT.08.00, Temperature, colour=Position))+geom_line()


#remove day with suspect points: 2020-07-10 10:30:00; 2020-07-10 11:38:00; 2020-07-10 10:40:00
TOK30_temp = TOK30_temp [!grepl("2020-07-10", TOK30_temp$GMT.08.00),]

ggplot(data=TOK30_temp, aes(GMT.08.00, Temperature, colour=Position))+geom_line()



#light data
TOK30_light=read.csv(file="TOK30_LIGHT_S2020.csv", header=T)
names(TOK30_light)[names(TOK30_light) == "?..GMT.08.00"] = 'GMT.08.00'
names(TOK30_light)[names(TOK30_light) == "Temp"] = 'Air Temperature'
TOK30_light$GMT.08.00=as.character(TOK30_light$GMT.08.00)
TOK30_light$GMT.08.00=as.POSIXct(TOK30_light$GMT.08.00, format='%m/%d/%Y %H:%M','Etc/GMT-8')
TOK30_light=TOK30_light[with(TOK30_light, GMT.08.00 > "2020-06-30 7:30:00" & GMT.08.00 < "2020-10-11 10:25:00"),]
TOK30_light$Position="Surface"


#combine instruments from this pond into one data frame
TOK30.1=merge(TOK30_temp, TOK30_light, all=T)



############################################
#Topaz Pond - pendant plus 3 subsurface loggers; top logger switched out partway through season

#load first top logger
Topaz_top=read.csv(file="TOPAZPOND_CONDUCTIVITY_TOP_S2020.csv", header=T)
names(Topaz_top)[names(Topaz_top) == "?..GMT.08.00"] = 'GMT.08.00'
names(Topaz_top)[names(Topaz_top) == "Temp"] = 'Temperature'
Topaz_top$Temperature=Topaz_top$Temperature -0.354440034
Topaz_top$GMT.08.00=as.character(Topaz_top$GMT.08.00)
Topaz_top$GMT.08.00=as.POSIXct(Topaz_top$GMT.08.00, format='%m/%d/%Y %H:%M','Etc/GMT-8')
Topaz_top$InstrumentType="MINIDOT"
#select only the time deployed
Topaz_top=Topaz_top[with(Topaz_top, GMT.08.00 > "2020-06-23 16:10:00" & GMT.08.00 < "2020-06-30 00:00:00"),]

#load second top logger
Topaz_templogger=read.csv(file="TOPAZPOND_TEMPERATURE_S2020.csv", header=T)
names(Topaz_templogger)[names(Topaz_templogger) == "?..GMT.08.00"] = 'GMT.08.00'
names(Topaz_templogger)[names(Topaz_templogger) == "Temp"] = 'Temperature'
Topaz_templogger$Temperature=Topaz_templogger$Temperature -0.087717557
Topaz_templogger$GMT.08.00=as.character(Topaz_templogger$GMT.08.00)
Topaz_templogger$GMT.08.00=as.POSIXct(Topaz_templogger$GMT.08.00, format='%m/%d/%Y %H:%M','Etc/GMT-8')
#select only the time deployed
Topaz_templogger=Topaz_templogger[with(Topaz_templogger, GMT.08.00 > "2020-07-02 00:00:00" & GMT.08.00 < "2020-10-10 00:00:00"),]
Topaz_templogger$InstrumentType="TEMPPRO"




Topaz_temperature=merge(Topaz_templogger, Topaz_top, all=T)
Topaz_temperature$Position = "Top"


Topaz_ox=read.csv(file="TOPAZPOND_MINIDOT_S2020_EC.csv", header=T)
Topaz_ox$GMT.08.00=as.POSIXct(Topaz_ox$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
Topaz_ox$Temperature=Topaz_ox$Temperature -0.013961832
Topaz_ox$Position="Middle"
Topaz_ox$Unix.Timestamp=NULL
Topaz_ox$UTC_Date_Time=NULL
Topaz_ox$Battery=NULL
Topaz_ox$Q=NULL
Topaz_ox$InstrumentType="MINIDOT"

Topaz_cond=read.csv(file="TOPAZPOND_CONDUCTIVITY_S2020.csv", header=T)
names(Topaz_cond)[names(Topaz_cond) == 'Temp'] = 'Temperature'
Topaz_cond$Temperature=Topaz_cond$Temperature -0.39980916
names(Topaz_cond)[names(Topaz_cond) == "?..GMT.08.00"] = 'GMT.08.00'
Topaz_cond$GMT.08.00=as.character(Topaz_cond$GMT.08.00)
Topaz_cond$GMT.08.00=as.POSIXct(Topaz_cond$GMT.08.00, format='%m/%d/%Y %H:%M','Etc/GMT-8')
Topaz_cond$Position = "Bottom"
Topaz_cond$InstrumentType="CONDUCTIVITY"



#add columns that identify the position of the loggers
Topaz_temperature$temp_0.08= Topaz_temperature$Temperature
Topaz_ox$temp_0.98=Topaz_ox$Temperature
Topaz_ox$DO_0.98=Topaz_ox$Dissolved.Oxygen
Topaz_ox$Sat_0.98=Topaz_ox$Dissolved.Oxygen.Saturation
Topaz_cond$temp_1.66=Topaz_cond$Temperature
Topaz_cond$cond_1.66=Topaz_cond$Conductivity

#add logger serial
Topaz_temperature$LoggerSerial="20636155, 20868832"
Topaz_ox$LoggerSerial=159482
Topaz_cond$LoggerSerial=20438689

#Combine temp data from DO and conductivity instruments

Topaz_temp1=merge(Topaz_cond, Topaz_ox, all=T)
Topaz_temp=merge(Topaz_temp1, Topaz_temperature, all=T)


#select only the time deployed
Topaz_temp=Topaz_temp[with(Topaz_temp, GMT.08.00 > "2020-06-23 16:00:00" & GMT.08.00 < "2020-10-10 00:00:00"),]

ggplot(data=Topaz_temp, aes(GMT.08.00, Temperature, colour=Position))+geom_line()



#light data
Topaz_light=read.csv(file="TOPAZPOND_LIGHT_S2020.csv", header=T)
names(Topaz_light)[names(Topaz_light) == "?..GMT.08.00"] = 'GMT.08.00'
names(Topaz_light)[names(Topaz_light) == "Temp"] = 'Air Temperature'
Topaz_light$GMT.08.00=as.character(Topaz_light$GMT.08.00)
Topaz_light$GMT.08.00=as.POSIXct(Topaz_light$GMT.08.00, format='%m/%d/%Y %H:%M','Etc/GMT-8')
Topaz_light=Topaz_light[with(Topaz_light, GMT.08.00 > "2020-07-01 10:30:00" & GMT.08.00 < "2020-10-11 10:25:00"),]
Topaz_light$Position="Surface"


#combine instruments from this pond into one data frame
TOPAZPOND.1=merge(Topaz_temp, Topaz_light, all=T)

#for metabolism, create one data frame with all ponds
EML1_temp$SiteName="EMLPOND1"
EMLPond2_temp$SiteName="EMLPOND2"
TOK11_temp$SiteName="TOK11"
TOK30_temp$SiteName="TOK30"
TOK71_temp$SiteName="TOK71"
Topaz_temp$SiteName="TOPAZPOND"
Grouse_temp$SiteName="GROUSEPOND"
Star_temp$SiteName="STARPOND"
Flat_temp$SiteName="FLATPOND"
New_temp$SiteName="NEWPOND"

merge1=merge(EML1_temp, EMLPond2_temp, all=T)
merge2=merge(merge1, TOK11_temp, all=T)
merge3=merge(merge2, TOK30_temp, all=T)
merge4=merge(merge3, TOK71_temp, all=T)
merge5=merge(merge4, Topaz_temp, all=T)
merge6=merge(merge5, Grouse_temp, all=T)
merge7=merge(merge6, Star_temp, all=T)
merge8=merge(merge7, Flat_temp, all=T)
merge9=merge(merge8, New_temp, all=T)
#move site name to first column
merge9=merge9%>%
  select(SiteName, everything())
merge9$Battery=NULL
merge9$Q=NULL
merge9$Unix.Timestamp=NULL
merge9$UTC_Date_Time=NULL



EML1_light$SiteName="EMLPOND1"
EMLPond2_light$SiteName="EMLPOND2"
TOK11_light$SiteName="TOK11"
TOK30_light$SiteName="TOK30"
TOK71_light$SiteName="TOK71"
Topaz_light$SiteName="TOPAZPOND"
Star_light$SiteName="STARPOND"
Flat_light$SiteName="FLATPOND"
New_light$SiteName="NEWPOND"

EML1_light$InstrumentType="LIGHTLOGGER"
EMLPond2_light$InstrumentType="LIGHTLOGGER"
TOK11_light$InstrumentType="LIGHTLOGGER"
TOK30_light$InstrumentType="LIGHTLOGGER"
TOK71_light$InstrumentType="LIGHTLOGGER"
Topaz_light$InstrumentType="LIGHTLOGGER"
Star_light$InstrumentType="LIGHTLOGGER"
Flat_light$InstrumentType="LIGHTLOGGER"
New_light$InstrumentType="LIGHTLOGGER"


# Create a list with the data frames (each pond stored separately)
data_list <- list(EMLPOND1_S20=EML1.1, EMLPOND2_S20=EMLPOND2.1, FLAT_S20=FLAT.1, GROUSE_S20=GROUSE.1, NEW_S20=NEW.1, STAR_S20=STAR.1, TOK11_S20=TOK11.1, TOK30_S20=TOK30.1, TOK71_S20=TOK71.1, TOPAZPOND_S20=TOPAZPOND.1)

# Save the list as an .rdata file
#save(data_list, file = "S2020_Ponds_tempcorrected.rdata")


setwd("C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data/ponds")

# Save each data frame in the list as an individual .rdata file
for (name in names(data_list)) {
  assign(name, data_list[[name]])
  file_name <- paste0(name, ".rdata")
  save(list = name, file = file_name)
  rm(list = name)
}



merge10=merge(merge9, EML1_light, all=T)
merge11=merge(merge10, EMLPond2_light, all=T)
merge12=merge(merge11, Flat_light, all=T)
merge13=merge(merge12, Star_light, all=T)
merge14=merge(merge13, TOK11_light, all=T)
merge15=merge(merge14, TOK30_light, all=T)
merge16=merge(merge15, TOK71_light, all=T)
merge17=merge(merge16, Topaz_light, all=T)
merge18=merge(merge17, New_light, all=T)

#this csv contains all instrument data from ponds in 2020, with the following cleaning:
#1. Position of instrument on chain was added as a column
#2. Naming of variables and column names were standardized
#3. Data was cut to relevant time frames (some loggers start/end logging when not in the water, I trimmed to just the times they were supposed to be actually deployed in the water column.)
#4. Intercalibrations were applied, see in-line comments for each value for each instrument. Code for determining intercalibrations is in file named TemperatureIntercalibrations.R

setwd("C:/Users/maryj/Documents/R/SierraPonds/RawLoggerData")
#write.csv(merge18, "S2020_PondsAll_tempcorrected.csv")



