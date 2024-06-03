#summer 2021 Logger Temperature Intercalibrations

#each deployment region had its own calibration water bath
#pre-deployment instruments were done in a shared water bath of tap water; 100% saturation 
#post-deployment instruments also done in shared water bath; 100% saturation

#choose a single instrument to calibrate against, and snap all others to that value
#First check the plots to just check to see if it looks like anything shifted



#load function to manipulate minutes (to make sure everything matches - need to adjust minidots because they were manually launched so vary by a few minutes)
mns <- function(m) {
  x <- m * 60
  return(x)
}
#load post-deployment in shared water bath and compare those values. for 2021, need to do this by site, since they were deployed/retrieved on different timelines and calibrated separately
setwd("C:/Users/maryj/Documents/R/SierraPonds/RawLoggerData")

#load data by pond, and change format of columns to make uniform/change date format to POSIX 

###eml pond 1 - 3 instruments
eml1_minidot=read.csv(file='EMLPOND1_MINIDOT_S2021.csv', header=T)
eml1_minidot$SiteName="EML1MINIDOT"
eml1_minidot$Unix.Timestamp=NULL
eml1_minidot$UTC_Date_Time=NULL
eml1_minidot$Q=NULL
eml1_temp=read.csv(file='EMLPOND1_TEMPERATURE_S2021.csv',header=T)
eml1_temp$SiteName="EML1TEMP"
names(eml1_temp)[names(eml1_temp) == '?..GMT.08.00'] <- 'GMT.08.00'
eml1_conductivity=read.csv(file='EMLPOND1_CONDUCTIVITY_S2021.csv',header=T)
eml1_conductivity$SiteName="EML1CONDUCTIVITY"
names(eml1_conductivity)[names(eml1_conductivity) == '?..GMT.08.00'] <- 'GMT.08.00'

eml1_minidot$GMT.08.00=as.POSIXct(eml1_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
eml1_minidot$GMT.08.00 <- eml1_minidot$GMT.08.00  + mns(3) #add minutes to round out minidot numbers to the nearest 10 min
eml1_minidot=eml1_minidot[with(eml1_minidot, GMT.08.00 >"2021-11-02 18:00:00" & GMT.08.00<"2021-11-03 17:00:00"),]
eml1_minidot$GMT.08.00 <- eml1_minidot$GMT.08.00  + mns(4) #add minutes to round out minidot numbers to the nearest 10 min

eml1_temp$GMT.08.00=as.character(eml1_temp$GMT.08.00)
eml1_temp$GMT.08.00=as.POSIXct(eml1_temp$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
eml1_temp=eml1_temp[with(eml1_temp, GMT.08.00 >"2021-11-02 18:00:00" & GMT.08.00<"2021-11-03 17:00:00"),]

eml1_conductivity$GMT.08.00=as.character(eml1_conductivity$GMT.08.00)
eml1_conductivity$GMT.08.00=as.POSIXct(eml1_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
eml1_conductivity=eml1_conductivity[with(eml1_conductivity, GMT.08.00 >"2021-11-02 18:00:00" & GMT.08.00<"2021-11-03 17:00:00"),]


#combine instruments from this pond into one data frame
eml1.1=merge(eml1_minidot, eml1_temp, all=T)
eml1.2=merge(eml1.1, eml1_conductivity, all=T)
#restrict dates to only include the calibration time
eml1.2=eml1.2[with(eml1.2, GMT.08.00 > "2021-11-02 18:00:00" & GMT.08.00 < "2021-11-03 17:00:00"),]
eml1.2$Site="EMLPOND1"









###TOK11 - 3 instruments
TOK11_minidot=read.csv(file='TOK11_MINIDOT_S2021.csv', header=T)
TOK11_minidot$SiteName="TOK11MINIDOT"
TOK11_minidot$Unix.Timestamp=NULL
TOK11_minidot$UTC_Date_Time=NULL
TOK11_minidot$Q=NULL
TOK11_temp=read.csv(file='TOK11_TEMPERATURE_S2021.csv',header=T)
TOK11_temp$SiteName="TOK11TEMP"
names(TOK11_temp)[names(TOK11_temp) == '?..GMT.08.00'] <- 'GMT.08.00'
TOK11_conductivity=read.csv(file='TOK11_CONDUCTIVITY_S2021.csv',header=T)
TOK11_conductivity$SiteName="TOK11CONDUCTIVITY"
names(TOK11_conductivity)[names(TOK11_conductivity) == '?..GMT.08.00'] <- 'GMT.08.00'

TOK11_minidot$GMT.08.00=as.POSIXct(TOK11_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
TOK11_minidot$GMT.08.00 <- TOK11_minidot$GMT.08.00  + mns(8) #add minutes to round out minidot numbers to the nearest 10 min
TOK11_minidot=TOK11_minidot[with(TOK11_minidot, GMT.08.00 >"2021-11-02 18:00:00" & GMT.08.00<"2021-11-03 17:00:00"),]
TOK11_minidot$GMT.08.00 <- TOK11_minidot$GMT.08.00  + mns(6) #add minutes to round out minidot numbers to the nearest 10 min

TOK11_temp$GMT.08.00=as.character(TOK11_temp$GMT.08.00)
TOK11_temp$GMT.08.00=as.POSIXct(TOK11_temp$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
TOK11_temp=TOK11_temp[with(TOK11_temp, GMT.08.00 >"2021-11-02 18:00:00" & GMT.08.00<"2021-11-03 17:00:00"),]


TOK11_conductivity$GMT.08.00=as.character(TOK11_conductivity$GMT.08.00)
TOK11_conductivity$GMT.08.00=as.POSIXct(TOK11_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M','GMT')
TOK11_temp=TOK11_temp[with(TOK11_temp, GMT.08.00 >"2021-11-02 18:00:00" & GMT.08.00<"2021-11-03 17:00:00"),]


#combine instruments from this pond into one data frame
TOK11.1=merge(TOK11_minidot, TOK11_temp, all=T)
TOK11.2=merge(TOK11.1, TOK11_conductivity, all=T)
#restrict dates to only include the calibration time
#TOK11.2=TOK11.2[with(TOK11.2, GMT.08.00 >"2021-11-02 18:00:00" & GMT.08.00<"2021-11-03 17:00:00"),]


TOK11.2$Site="TOK11"





###BP1 - 2 instruments
BP1_minidot=read.csv(file='BP1_MINIDOT_S2021.csv', header=T)
BP1_minidot$SiteName="BP1MINIDOT"
BP1_minidot$Unix.Timestamp=NULL
BP1_minidot$UTC_Date_Time=NULL
BP1_minidot$Q=NULL
BP1_conductivity=read.csv(file='BP1_CONDUCTIVITY_S2021.csv',header=T)
BP1_conductivity$SiteName="BP1CONDUCTIVITY"
names(BP1_conductivity)[names(BP1_conductivity) == '?..GMT.08.00'] <- 'GMT.08.00'

BP1_minidot$GMT.08.00=as.POSIXct(BP1_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', 'GMT')
BP1_minidot$GMT.08.00 <- BP1_minidot$GMT.08.00  + mns(5) #add minutes to round out minidot numbers to the nearest 10 min
BP1_conductivity$GMT.08.00=as.character(BP1_conductivity$GMT.08.00)
BP1_conductivity$GMT.08.00=as.POSIXct(BP1_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M','GMT')


#combine instruments from this pond into one data frame
BP1.1=merge(BP1_minidot, BP1_conductivity, all=T)
#restrict dates to only include the calibration time
BP1.1=BP1.1[with(BP1.1, GMT.08.00 > "2021-10-28 18:00:00" & GMT.08.00 < "2021-11-02 7:00:00"),]
BP1.1$Site="BP1"



###BAYPOND - 2 instruments
BAYPOND_minidot=read.csv(file='BAYPOND_MINIDOT_S2021.csv', header=T)
BAYPOND_minidot$SiteName="BAYPONDMINIDOT"
BAYPOND_minidot$Unix.Timestamp=NULL
BAYPOND_minidot$UTC_Date_Time=NULL
BAYPOND_minidot$Q=NULL
BAYPOND_conductivity=read.csv(file='BAYPOND_CONDUCTIVITY_S2021.csv',header=T)
BAYPOND_conductivity$SiteName="BAYPONDCONDUCTIVITY"
names(BAYPOND_conductivity)[names(BAYPOND_conductivity) == '?..GMT.08.00'] <- 'GMT.08.00'

BAYPOND_minidot$GMT.08.00=as.POSIXct(BAYPOND_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', 'GMT')
BAYPOND_minidot$GMT.08.00 <- BAYPOND_minidot$GMT.08.00  + mns(1) #add minutes to round out minidot numbers to the nearest 10 min
BAYPOND_conductivity$GMT.08.00=as.character(BAYPOND_conductivity$GMT.08.00)
BAYPOND_conductivity$GMT.08.00=as.POSIXct(BAYPOND_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M','GMT')


#combine instruments from this pond into one data frame
BAYPOND.1=merge(BAYPOND_minidot, BAYPOND_conductivity, all=T)
#restrict dates to only include the calibration time
BAYPOND.1=BAYPOND.1[with(BAYPOND.1, GMT.08.00 > "2021-10-28 18:00:00" & GMT.08.00 < "2021-11-02 7:00:00"),]
BAYPOND.1$Site="BAYPOND"





###BP5 - 2 instruments
BP5_minidot=read.csv(file='BP5_MINIDOT_S2021.csv', header=T)
BP5_minidot$SiteName="BP5MINIDOT"
BP5_minidot$Unix.Timestamp=NULL
BP5_minidot$UTC_Date_Time=NULL
BP5_minidot$Q=NULL
BP5_conductivity=read.csv(file='BP5_CONDUCTIVITY_S2021.csv',header=T)
BP5_conductivity$SiteName="BP5CONDUCTIVITY"
names(BP5_conductivity)[names(BP5_conductivity) == '?..GMT.08.00'] <- 'GMT.08.00'

BP5_minidot$GMT.08.00=as.POSIXct(BP5_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', 'GMT')
BP5_minidot$GMT.08.00 <- BP5_minidot$GMT.08.00  + mns(2) #add minutes to round out minidot numbers to the nearest 10 min
BP5_conductivity$GMT.08.00=as.character(BP5_conductivity$GMT.08.00)
BP5_conductivity$GMT.08.00=as.POSIXct(BP5_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M','GMT')


#combine instruments from this pond into one data frame
BP5.1=merge(BP5_minidot, BP5_conductivity, all=T)
#restrict dates to only include the calibration time
BP5.1=BP5.1[with(BP5.1, GMT.08.00 > "2021-10-28 18:00:00" & GMT.08.00 < "2021-11-02 7:00:00"),]
BP5.1$Site="BP5"





###IR31 - 2 instruments
IR31_minidot=read.csv(file='IR31_MINIDOT_S2021.csv', header=T)
IR31_minidot$SiteName="IR31MINIDOT"
IR31_minidot$Unix.Timestamp=NULL
IR31_minidot$UTC_Date_Time=NULL
IR31_minidot$Q=NULL
IR31_conductivity=read.csv(file='IR31_CONDUCTIVITY_S2021.csv',header=T)
IR31_conductivity$SiteName="IR31CONDUCTIVITY"
names(IR31_conductivity)[names(IR31_conductivity) == '?..GMT.08.00'] <- 'GMT.08.00'
IR31_conductivity=IR31_conductivity[!is.na(IR31_conductivity$Temperature),]

IR31_minidot$GMT.08.00=as.POSIXct(IR31_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', 'GMT')
IR31_minidot$GMT.08.00 <- IR31_minidot$GMT.08.00  + mns(4) #add minutes to round out minidot numbers to the nearest 10 min
IR31_conductivity$GMT.08.00=as.character(IR31_conductivity$GMT.08.00)
IR31_conductivity$GMT.08.00=as.POSIXct(IR31_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M','GMT')


#combine instruments from this pond into one data frame
IR31.1=merge(IR31_minidot, IR31_conductivity, all=T)
#restrict dates to only include the calibration time
IR31.1=IR31.1[with(IR31.1, GMT.08.00 > "2021-10-18 22:00:00" & GMT.08.00 < "2021-10-19 19:00:00"),]
IR31.1$Site="IR31"




###IR32 - 2 instruments
IR32_minidot=read.csv(file='IR32_MINIDOT_S2021.csv', header=T)
IR32_minidot$SiteName="IR32MINIDOT"
IR32_minidot$Unix.Timestamp=NULL
IR32_minidot$UTC_Date_Time=NULL
IR32_minidot$Q=NULL
IR32_conductivity=read.csv(file='IR32_CONDUCTIVITY_S2021.csv',header=T)
IR32_conductivity$SiteName="IR32CONDUCTIVITY"
names(IR32_conductivity)[names(IR32_conductivity) == '?..GMT.08.00'] <- 'GMT.08.00'
IR32_conductivity=IR32_conductivity[!is.na(IR32_conductivity$Temperature),]


IR32_minidot$GMT.08.00=as.POSIXct(IR32_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', 'GMT')
IR32_minidot$GMT.08.00 <- IR32_minidot$GMT.08.00  + mns(7) #add minutes to round out minidot numbers to the nearest 10 min
IR32_conductivity$GMT.08.00=as.character(IR32_conductivity$GMT.08.00)
IR32_conductivity$GMT.08.00=as.POSIXct(IR32_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M','GMT')


#combine instruments from this pond into one data frame
IR32.1=merge(IR32_minidot, IR32_conductivity, all=T)
#restrict dates to only include the calibration time
IR32.1=IR32.1[with(IR32.1, GMT.08.00 > "2021-10-18 22:00:00" & GMT.08.00 < "2021-10-19 19:00:00"),]
IR32.1$Site="IR32"



###IR11 - 2 instruments
IR11_minidot=read.csv(file='IR11_MINIDOT_S2021.csv', header=T)
IR11_minidot$SiteName="IR11MINIDOT"
IR11_minidot$Unix.Timestamp=NULL
IR11_minidot$UTC_Date_Time=NULL
IR11_minidot$Q=NULL
IR11_conductivity=read.csv(file='IR11_CONDUCTIVITY_S2021.csv',header=T)
IR11_conductivity$SiteName="IR11CONDUCTIVITY"
names(IR11_conductivity)[names(IR11_conductivity) == '?..GMT.08.00'] <- 'GMT.08.00'
IR11_conductivity=IR11_conductivity[!is.na(IR11_conductivity$Temperature),]


IR11_minidot$GMT.08.00=as.POSIXct(IR11_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', 'GMT')
IR11_minidot$GMT.08.00 <- IR11_minidot$GMT.08.00  + mns(8) #add minutes to round out minidot numbers to the nearest 10 min
IR11_conductivity$GMT.08.00=as.character(IR11_conductivity$GMT.08.00)
IR11_conductivity$GMT.08.00=as.POSIXct(IR11_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M','GMT')


#combine instruments from this pond into one data frame
IR11.1=merge(IR11_minidot, IR11_conductivity, all=T)
#restrict dates to only include the calibration time
IR11.1=IR11.1[with(IR11.1, GMT.08.00 > "2021-10-18 22:00:00" & GMT.08.00 < "2021-10-19 19:00:00"),]
IR11.1$Site="IR11"



#merge all the instruments/ponds together into one big dataframe to prepare for plotting
merge1=merge(eml1.2, TOK11.2, all=T)
merge2=merge(merge1, BP1.1, all=T)
merge3=merge(merge2, BAYPOND.1, all=T)
merge4=merge(merge3, BP5.1, all=T)
merge5=merge(merge4, IR31.1, all=T)
merge6=merge(merge5, IR32.1, all=T)
merge7=merge(merge6, IR11.1, all=T)


#merge by shared baths
tokopah_merge=merge(eml1.2, TOK11.2, all=T)
boundary_merge1=merge(BP1.1, BAYPOND.1, all=T)
boundary_merge=merge(boundary_merge1, BP5.1, all=T)

ireland_merge1=merge(IR31.1, IR32.1, all=T)
ireland_merge=merge(ireland_merge1, IR11.1, all=T)

#plot temperature
#NEED TO SEPARATE TEMP BY INSTRUMENT - right now it's plotting all instruments within a single pond as a single instrument, which is WRONG

library(ggplot2)
library(directlabels)
library(tidyverse)
alltemp=ggplot(data=merge7, aes(x=GMT.08.00, y=Temperature, colour=SiteName))+
  geom_line()+
  labs(title="Post-deployment Temperature") + 
  theme_classic()+
  geom_text(data = filter(merge7, GMT.08.00 == median(GMT.08.00)),aes(label = SiteName))
alltemp




tokopahtemp=ggplot(data=tokopah_merge, aes(x=GMT.08.00, y=Temperature, colour=SiteName))+
  geom_line()+
  labs(title="Post-deployment Temperature") + 
  theme_classic()+
  geom_text(data = filter(tokopah_merge, GMT.08.00 == median(GMT.08.00)),aes(label = SiteName))
tokopahtemp

#calibration selection: tok11 temp

boundarytemp=ggplot(data=boundary_merge, aes(x=GMT.08.00, y=Temperature, colour=SiteName))+
  geom_line()+
  labs(title="Post-deployment Temperature") + 
  theme_classic()+
  geom_text(data = filter(boundary_merge, GMT.08.00 == median(GMT.08.00)),aes(label = SiteName))
boundarytemp
#calibration selection: bp1 conductivity 

irelandtemp=ggplot(data=ireland_merge, aes(x=GMT.08.00, y=Temperature, colour=SiteName))+
  geom_line()+
  labs(title="Post-deployment Temperature") + 
  theme_classic()+
  geom_text(data = filter(ireland_merge, GMT.08.00 == median(GMT.08.00)),aes(label = SiteName))
irelandtemp
#calibration selection: IR 31 minidot

IR11_temp=ireland_merge[ireland_merge$Site == "IR11",]
IR11temp=ggplot()+
  geom_line(data=IR11_temp, aes(x=GMT.08.00, y=Temperature, colour=SiteName))+
  labs(title="Post-deployment Temperature-IR11") + 
  theme_classic()
IR11temp

IR31_temp=ireland_merge[ireland_merge$Site == "IR31",]
IR31temp=ggplot()+
  geom_line(data=IR31_temp, aes(x=GMT.08.00, y=Temperature, colour=SiteName))+
  labs(title="Post-deployment Temperature-IR31") + 
  theme_classic()
IR31temp

IR32_temp=ireland_merge[ireland_merge$Site == "IR32",]
IR32temp=ggplot()+
  geom_line(data=IR32_temp, aes(x=GMT.08.00, y=Temperature, colour=SiteName))+
  labs(title="Post-deployment Temperature-IR32") + 
  theme_classic()
IR32temp


#to adjust, pick one instrument that looks good, then snap all others to that value. I'm choosing IR31_minidot/TOk11 temp/BP1 conductivity, because it's right in the middle of where everything is, and when I view it individually, it doesn't have any strange fluctuations anywhere during the water bath period. 
#I'm going to calculate the difference between x instrument and IR31_minidot  
#I want to subtract IR31_minidot from every time step, from x instrument measurement at every time step
#Then average the difference to find out the value to subtract or add to every instrument (a single value to apply to the actual data)

#because the time stamps are slightly different (because the miniDOTs were turned on manually, while the others were launched via computer delayed launch), need to adjust time stamps appropriately (make sure to snap to IR31_minidot)
#I need to match each time step because the temperature of the water bath is not constant. 
#I did this above using the function at the beginning of this code file. 
#the only discrepancy is that topazpondminidot logged every 5 minutes, instead of every 10 minutes. 


#I want to calculate the difference between each instrument and IR31_minidot, and find the mean difference over the water bath period. Then I'll apply that single correction (unique to each instrument) to the actual dataset. 

#the math will be temp of each logger - temp of the IR31_minidot logger
#do this for EVERY TIME STEP
#note that IR11_MINIDOT logged every 5 minutes, so need to deal with that - only calculate the matching date/times.


#join final table (ireland_merge) with another table (IR31_minidot) which specifies IR31_minidot with its own temperature column

TOKOPAH_calDF=TOK11_temp
names(TOKOPAH_calDF)[names(TOKOPAH_calDF) == 'Temperature'] = 'calDF_temperature'
TOKOPAH_calDF$SiteName=NULL

TOKOPAH_allDF=merge(tokopah_merge, TOKOPAH_calDF)

BP_calDF=BP1_conductivity
names(BP_calDF)[names(BP_calDF) == 'Temperature'] = 'calDF_temperature'
BP_calDF$SiteName=NULL
BP_calDF$Conductivity=NULL
BP_calDF=BP_calDF[with(BP_calDF, GMT.08.00 > "2021-10-28 18:00:00" & GMT.08.00 < "2021-11-02 7:00:00"),]
BP_allDF=merge(BP_calDF,boundary_merge)


IR_calDF=IR31_minidot
names(IR_calDF)[names(IR_calDF) == 'Temperature'] = 'calDF_temperature'
IR_calDF$SiteName=NULL
IR_calDF$Battery=NULL
IR_calDF$Dissolved.Oxygen.Saturation=NULL
IR_calDF$Dissolved.Oxygen=NULL
IR_allDF=merge(ireland_merge,IR_calDF)

#now I have a df (allDF) that has two temperature columns - one with the instrument reading, and one with values I'll calibrate against. 
#now just need the difference for each time step, which I'll get by subtracting columns and placing the result in a column called "Diff"

TOKOPAH_allDF$Diff=TOKOPAH_allDF$calDF_temperature - TOKOPAH_allDF$Temperature
BP_allDF$Diff=BP_allDF$calDF_temperature - BP_allDF$Temperature
IR_allDF$Diff=IR_allDF$calDF_temperature - IR_allDF$Temperature

#take the average of column "Diff" for each logger separately. 

library(doBy)
tokopah_meanDF=summaryBy(Diff~SiteName, TOKOPAH_allDF, FUN=mean, na.rm=T)
BP_meanDF=summaryBy(Diff~SiteName, BP_allDF, FUN=mean, na.rm=T)
IR_meanDF=summaryBy(Diff~SiteName, IR_allDF, FUN=mean, na.rm=T)


#export calibration values to csv - apply this to future manipulations of raw data.
setwd("C:/Users/maryj/Documents/R/SierraPonds/Calibrations")

write.csv(tokopah_meanDF, file="S2021_tokopah_calibration.csv")
write.csv(BP_meanDF, file="S2021_boundary_calibration.csv")
write.csv(IR_meanDF, file="S2021_ireland_calibration.csv")
