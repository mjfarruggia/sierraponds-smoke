#Winter 2021-2022 Logger Temperature Intercalibrations -plus Summer 2021 Tokopah loggers that were frozen in.


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
eml1_minidot=read.csv(file='EMLPOND1_MINIDOT_W21_POSTCAL.csv', header=T)
eml1_minidot$SiteName="EML1MINIDOT"
eml1_minidot$Unix.Timestamp=NULL
eml1_minidot$UTC_Date_Time=NULL
eml1_minidot$Q=NULL
eml1_minidot$Battery=NULL

eml1_conductivity=read.csv(file='EMLPOND1_CONDUCTIVITY_W21_POSTCAL.csv',header=T)
eml1_conductivity$SiteName="EML1CONDUCTIVITY"

eml1_minidot$GMT.08.00=as.POSIXct(eml1_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
#eml1_minidot$GMT.08.00 <- eml1_minidot$GMT.08.00  + mns(0) #add minutes to round out minidot numbers to the nearest 10 min


eml1_conductivity$GMT.08.00=as.character(eml1_conductivity$GMT.08.00)
eml1_conductivity$GMT.08.00=as.POSIXct(eml1_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")


#combine instruments from this pond into one data frame
eml1.1=merge(eml1_minidot, eml1_conductivity, all=T)
#restrict dates to only include the calibration time
# eml1.1=eml1.1[with(eml1.1, GMT.08.00 >="2022-06-07 08:35:00" & GMT.08.00 <= "2022-06-07 09:15:00"),]
eml1.1$Site="EMLPOND1"



###TOK11 - 3 instruments
TOK11_minidot=read.csv(file='TOK11_MINIDOT_W21_POSTCAL.csv', header=T)
TOK11_minidot$SiteName="TOK11MINIDOT"
TOK11_minidot$Unix.Timestamp=NULL
TOK11_minidot$UTC_Date_Time=NULL
TOK11_minidot$Q=NULL

TOK11_conductivity=read.csv(file='TOK11_CONDUCTIVITY_W21_POSTCAL.csv',header=T)
TOK11_conductivity$SiteName="TOK11CONDUCTIVITY"

TOK11_minidot$GMT.08.00=as.POSIXct(TOK11_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
#TOK11_minidot$GMT.08.00 <- TOK11_minidot$GMT.08.00  + mns(8) #add minutes to round out minidot numbers to the nearest 10 min

TOK11_conductivity$GMT.08.00=as.character(TOK11_conductivity$GMT.08.00)
TOK11_conductivity$GMT.08.00=as.POSIXct(TOK11_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")


#combine instruments from this pond into one data frame
TOK11.1=merge(TOK11_minidot, TOK11_conductivity, all=T)
#restrict dates to only include the calibration time
#TOK11.1=TOK11.1[TOK11.1$GMT.08.00>="2022-06-07 08:35:00" & GMT.08.00 <= "2022-06-07 09:15:00",]


TOK11.1$Site="TOK11"





###TOK30 - 3 instruments
TOK30_minidot=read.csv(file='TOK30_MINIDOT_SW21_POSTCAL.csv', header=T)
TOK30_minidot$SiteName="TOK30MINIDOT"
TOK30_minidot$Unix.Timestamp=NULL
TOK30_minidot$UTC_Date_Time=NULL
TOK30_minidot$Q=NULL
TOK30_temp=read.csv(file='TOK30_TEMPERATURE_SW21_POSTCAL.csv',header=T)
TOK30_temp$SiteName="TOK30TEMP"
TOK30_conductivity=read.csv(file='TOK30_CONDUCTIVITY_SW21_POSTCAL.csv',header=T)
TOK30_conductivity$SiteName="TOK30CONDUCTIVITY"

TOK30_minidot$GMT.08.00=as.POSIXct(TOK30_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
#TOK30_minidot$GMT.08.00 <- TOK30_minidot$GMT.08.00  + mns(8) #add minutes to round out minidot numbers to the nearest 10 min
TOK30_temp$GMT.08.00=as.character(TOK30_temp$GMT.08.00)
TOK30_temp$GMT.08.00=as.POSIXct(TOK30_temp$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
TOK30_conductivity$GMT.08.00=as.character(TOK30_conductivity$GMT.08.00)
TOK30_conductivity$GMT.08.00=as.POSIXct(TOK30_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")


#combine instruments from this pond into one data frame
TOK30.1=merge(TOK30_minidot, TOK30_temp, all=T)
TOK30.2=merge(TOK30.1, TOK30_conductivity, all=T)
#restrict dates to only include the calibration time
#TOK30.2=TOK30.2[TOK30.2$GMT.08.00>="2022-06-07 08:35:00" & GMT.08.00 <= "2022-06-07 09:15:00",]


TOK30.2$Site="TOK30"




###TOPAZPOND - 3 instruments
TOPAZPOND_minidot=read.csv(file='TOPAZPOND_MINIDOT_SW21_POSTCAL.csv', header=T)
TOPAZPOND_minidot$SiteName="TOPAZPONDMINIDOT"
TOPAZPOND_minidot$Unix.Timestamp=NULL
TOPAZPOND_minidot$UTC_Date_Time=NULL
TOPAZPOND_minidot$Q=NULL
TOPAZPOND_temp=read.csv(file='TOPAZPOND_TEMPERATURE_SW21_POSTCAL.csv',header=T)
TOPAZPOND_temp$SiteName="TOPAZPONDTEMP"
TOPAZPOND_conductivity=read.csv(file='TOPAZPOND_CONDUCTIVITY_SW21_POSTCAL.csv',header=T)
TOPAZPOND_conductivity$SiteName="TOPAZPONDCONDUCTIVITY"

TOPAZPOND_minidot$GMT.08.00=as.POSIXct(TOPAZPOND_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
#TOPAZPOND_minidot$GMT.08.00 <- TOPAZPOND_minidot$GMT.08.00  + mns(8) #add minutes to round out minidot numbers to the nearest 10 min
TOPAZPOND_temp$GMT.08.00=as.character(TOPAZPOND_temp$GMT.08.00)
TOPAZPOND_temp$GMT.08.00=as.POSIXct(TOPAZPOND_temp$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")
TOPAZPOND_conductivity$GMT.08.00=as.character(TOPAZPOND_conductivity$GMT.08.00)
TOPAZPOND_conductivity$GMT.08.00=as.POSIXct(TOPAZPOND_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")


#combine instruments from this pond into one data frame
TOPAZPOND.1=merge(TOPAZPOND_minidot, TOPAZPOND_temp, all=T)
TOPAZPOND.2=merge(TOPAZPOND.1, TOPAZPOND_conductivity, all=T)
#restrict dates to only include the calibration time
#TOPAZPOND.2=TOPAZPOND.2[TOPAZPOND.2$GMT.08.00>="2022-06-07 08:35:00" & GMT.08.00 <= "2022-06-07 09:15:00",]


TOPAZPOND.2$Site="TOPAZPOND"


#merge all the instruments/ponds together into one big dataframe to prepare for plotting
merge1=merge(eml1.1, TOK11.1, all=T)
merge2=merge(merge1, TOK30.2, all=T)
merge3=merge(merge2, TOPAZPOND.2, all=T)
merge3=merge3[merge3$GMT.08.00 <= as.POSIXct("2022-06-07 09:15:00", tz="Etc/GMT-8"),]
merge3=merge3[merge3$GMT.08.00 >= as.POSIXct("2022-06-07 08:40:00", tz="Etc/GMT-8"),]
merge3$Battery=NULL

#plot temperature
#NEED TO SEPARATE TEMP BY INSTRUMENT - right now it's plotting all instruments within a single pond as a single instrument, which is WRONG

library(ggplot2)
library(directlabels)
library(tidyverse)
alltemp=ggplot(data=merge3, aes(x=GMT.08.00, y=Temperature, colour=SiteName))+
  geom_line()+
  labs(title="Post-deployment Temperature") + 
  theme_classic()+
  geom_text(data = filter(merge3, GMT.08.00 == median(GMT.08.00)),aes(label = SiteName))
alltemp

#calibration selection: TOPAZPONDTEMP

TOPAZPOND_temp=merge3[merge3$SiteName == "TOPAZPONDTEMP",]
TOPAZPONDtemp=ggplot()+
  geom_line(data=TOPAZPOND_temp, aes(x=GMT.08.00, y=Temperature, colour=SiteName))+
  labs(title="Post-deployment Temperature-IR11") + 
  theme_classic()
TOPAZPONDtemp



#to adjust, pick one instrument that looks good, then snap all others to that value. I'm choosing TOPAZPONDTEMP, because it's right in the middle of where everything is, and when I view it individually, it doesn't have any strange fluctuations anywhere during the water bath period. 
#I'm going to calculate the difference between x instrument and TOPAZPONDTEMP  
#I want to subtract TOPAZPOND from every time step, from x instrument measurement at every time step
#Then average the difference to find out the value to subtract or add to every instrument (a single value to apply to the actual data)

#because the time stamps are slightly different (because the miniDOTs were turned on manually, while the others were launched via computer delayed launch), need to adjust time stamps appropriately (make sure to snap to TOPAZPOND)
#I need to match each time step because the temperature of the water bath is not constant. 
#I did this above using the function at the beginning of this code file. 
#the only discrepancy is that topazpondminidot logged every 5 minutes, instead of every 10 minutes. 


#I want to calculate the difference between each instrument and TOPAZPOND, and find the mean difference over the water bath period. Then I'll apply that single correction (unique to each instrument) to the actual dataset. 

#the math will be temp of each logger - temp of the TOPAZPOND logger
#do this for EVERY TIME STEP
#note that IR11_MINIDOT logged every 5 minutes, so need to deal with that - only calculate the matching date/times.


#join final table (ireland_merge) with another table (TOPAZPOND) which specifies TOPAZPOND with its own temperature column

calDF=TOPAZPOND_temp

names(calDF)[names(calDF) == 'Temperature'] = 'calDF_temperature'
calDF$SiteName=NULL
calDF$Dissolved.Oxygen=NULL
calDF$Dissolved.Oxygen.Saturation=NULL
calDF$Conductivity=NULL
calDF$Site=NULL
allDF=merge(merge3, calDF)



#now I have a df (allDF) that has two temperature columns - one with the instrument reading, and one with values I'll calibrate against. 
#now just need the difference for each time step, which I'll get by subtracting columns and placing the result in a column called "Diff"

allDF$Diff=allDF$calDF_temperature - allDF$Temperature
#take the average of column "Diff" for each logger separately. 

library(doBy)
meanDF=summaryBy(Diff~SiteName, allDF, FUN=mean, na.rm=T)


#export calibration values to csv - apply this to future manipulations of raw data.
write.csv(meanDF, file="Winter2021_2022_calibration.csv")
