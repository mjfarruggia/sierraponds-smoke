#Winter 2020-2021 Logger Temperature Intercalibrations : TOK11, TOPAZPOND, EMLPOND1


#pre-deployment instruments were done in a shared water bath of tap water; 100% saturation 
#post-deployment instruments also done in shared water bath; 100% saturation

#choose a single instrument to calibrate against, and snap all others to that value
#First check the plots to just check to see if it looks like anything shifted
library(tidyverse)
##set working directory
wd <- 'C:/Users/maryj/Documents/R/SierraPonds/Calibrations/Data/2_W2020/W2020_precal'
setwd(wd)



#load function to manipulate minutes (to make sure everything matches - need to adjust minidots because they were manually launched so vary by a few minutes)
mns <- function(m) {
  x <- m * 60
  return(x)
}
#load post-deployment in shared water bath and compare those values. 

#load data by pond, and change format of columns to make uniform/change date format to POSIX 

###eml pond 1 
eml1_minidot=read.csv(file='EMLPOND1_MINIDOT_W2020_PRECAL_DAVISEC.csv', header=T)
eml1_minidot$SiteName="EML1MINIDOT"
eml1_minidot$Unix.Timestamp=NULL
eml1_minidot$UTC_Date_Time=NULL
eml1_minidot$Q=NULL
eml1_minidot$Battery=NULL

eml1_conductivity=read.csv(file='EMLPOND1_CONDUCTIVITY_W2020_PRECAL_20636145.csv',header=T)
eml1_conductivity$SiteName="EML1CONDUCTIVITY"

eml1_minidot$GMT.08.00=as.POSIXct(eml1_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
eml1_minidot$GMT.08.00 <- eml1_minidot$GMT.08.00  + mns(3) #add minutes to round out minidot numbers to the nearest 10 min


eml1_conductivity$GMT.08.00=as.character(eml1_conductivity$GMT.08.00)
eml1_conductivity$GMT.08.00=as.POSIXct(eml1_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")


#combine instruments from this pond into one data frame
eml1.1=merge(eml1_minidot, eml1_conductivity, all=T)

#restrict dates to only include the calibration time
eml1.1 <- subset(eml1.1,
                           GMT.08.00 >= as.POSIXct("2020-09-20 12:00:00", tz = "Etc/GMT-8") &
                             GMT.08.00 <= as.POSIXct("2020-09-21 08:00:00", tz = "Etc/GMT-8"))



eml1.1$Site="EMLPOND1"



###TOK11 
TOK11_minidot=read.csv(file='TOK11_MINIDOT_W2020_PRECAL_DAVISEC.csv', header=T)
TOK11_minidot$SiteName="TOK11MINIDOT"
TOK11_minidot$Unix.Timestamp=NULL
TOK11_minidot$UTC_Date_Time=NULL
TOK11_minidot$Q=NULL

TOK11_conductivity=read.csv(file='TOK11_CONDUCTIVITY_W2020_PRECAL_20438693.csv',header=T)
TOK11_conductivity$SiteName="TOK11CONDUCTIVITY"

TOK11_minidot$GMT.08.00=as.POSIXct(TOK11_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
TOK11_minidot$GMT.08.00 <- TOK11_minidot$GMT.08.00  + mns(4) #add minutes to round out minidot numbers to the nearest 10 min

TOK11_conductivity$GMT.08.00=as.character(TOK11_conductivity$GMT.08.00)
TOK11_conductivity$GMT.08.00=as.POSIXct(TOK11_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")


#combine instruments from this pond into one data frame
TOK11.1=merge(TOK11_minidot, TOK11_conductivity, all=T)


#restrict dates to only include the calibration time
TOK11.1 <- subset(TOK11.1,
                 GMT.08.00 >= as.POSIXct("2020-09-20 12:00:00", tz = "Etc/GMT-8") &
                   GMT.08.00 <= as.POSIXct("2020-09-21 08:00:00", tz = "Etc/GMT-8"))


TOK11.1$Site="TOK11"








###TOPAZPOND 
TOPAZPOND_minidot=read.csv(file='TOPAZPOND_MINIDOT_W20_PRECAL_DAVISEC.csv', header=T)
TOPAZPOND_minidot$SiteName="TOPAZPONDMINIDOT"
TOPAZPOND_minidot$Unix.Timestamp=NULL
TOPAZPOND_minidot$UTC_Date_Time=NULL
TOPAZPOND_minidot$Q=NULL
TOPAZPOND_conductivity=read.csv(file='TOPAZPOND_CONDUCTIVITY_W2020_PRECAL_20636155.csv',header=T)
TOPAZPOND_conductivity$SiteName="TOPAZPONDCONDUCTIVITY"

TOPAZPOND_minidot$GMT.08.00=as.POSIXct(TOPAZPOND_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
TOPAZPOND_minidot$GMT.08.00 <- TOPAZPOND_minidot$GMT.08.00  + mns(4) #add minutes to round out minidot numbers to the nearest 10 min

TOPAZPOND_conductivity$GMT.08.00=as.character(TOPAZPOND_conductivity$GMT.08.00)
TOPAZPOND_conductivity$GMT.08.00=as.POSIXct(TOPAZPOND_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M',tz="Etc/GMT-8")


#combine instruments from this pond into one data frame
TOPAZPOND.1=merge(TOPAZPOND_minidot, TOPAZPOND_conductivity, all=T)

TOPAZPOND.1 <- subset(TOPAZPOND.1,
                 GMT.08.00 >= as.POSIXct("2020-09-20 12:00:00", tz = "Etc/GMT-8") &
                   GMT.08.00 <= as.POSIXct("2020-09-21 08:00:00", tz = "Etc/GMT-8"))


TOPAZPOND.1$Site="TOPAZPOND"


#merge all the instruments/ponds together into one big dataframe to prepare for plotting
merge1=merge(eml1.1, TOK11.1, all=T)
merge2=merge(merge1, TOPAZPOND.1, all=T)
merge2$Battery=NULL

#plot temperature
#NEED TO SEPARATE TEMP BY INSTRUMENT - right now it's plotting all instruments within a single pond as a single instrument, which is WRONG

library(ggplot2)
library(directlabels)
library(tidyverse)
alltemp=ggplot(data=merge2, aes(x=GMT.08.00, y=Temperature, colour=Site))+
  geom_line()+
  labs(title="Post-deployment Temperature") + 
  theme_classic()+
  geom_text(data = filter(merge2, GMT.08.00 == median(GMT.08.00)),aes(label = SiteName))
alltemp

#calibration selection: emlpond1minidot

eml1_minidot_temp=merge2[merge2$SiteName == "EML1MINIDOT",]
eml1_minidot_temp_plot=ggplot()+
  geom_line(data=eml1_minidot_temp, aes(x=GMT.08.00, y=Temperature, colour=SiteName))+
  labs(title="Post-deployment Temperature") + 
  theme_classic()
eml1_minidot_temp_plot


#to adjust, pick one instrument that looks good, then snap all others to that value. I'm choosing emlpond1minidot, because it's right in the middle of where everything is, and when I view it individually, it doesn't have any strange fluctuations anywhere during the water bath period. 
#I'm going to calculate the difference between x instrument and TOPAZPONDTEMP  
#I want to subtract TOPAZPOND from every time step, from x instrument measurement at every time step
#Then average the difference to find out the value to subtract or add to every instrument (a single value to apply to the actual data)

#because the time stamps are slightly different (because the miniDOTs were turned on manually, while the others were launched via computer delayed launch), need to adjust time stamps appropriately (make sure to snap to TOPAZPOND)
#I need to match each time step because the temperature of the water bath is not constant. 
#I did this above using the function at the beginning of this code file. 


#I want to calculate the difference between each instrument and TOPAZPOND, and find the mean difference over the water bath period. Then I'll apply that single correction (unique to each instrument) to the actual dataset. 

#the math will be temp of each logger - temp of the TOPAZPOND logger
#do this for EVERY TIME STEP
#note that IR11_MINIDOT logged every 5 minutes, so need to deal with that - only calculate the matching date/times.



calDF=eml1_minidot_temp

names(calDF)[names(calDF) == 'Temperature'] = 'calDF_temperature'
calDF$SiteName=NULL
calDF$Dissolved.Oxygen=NULL
calDF$Dissolved.Oxygen.Saturation=NULL
calDF$Conductivity=NULL
calDF$Site=NULL


allDF=merge(merge2, calDF)



#now I have a df (allDF) that has two temperature columns - one with the instrument reading, and one with values I'll calibrate against. 
#now just need the difference for each time step, which I'll get by subtracting columns and placing the result in a column called "Diff"

allDF$Diff=allDF$calDF_temperature - allDF$Temperature
#take the average of column "Diff" for each logger separately. 

library(doBy)
meanDF=summaryBy(Diff~SiteName, allDF, FUN=mean, na.rm=T)


#export calibration values to csv - apply this to future manipulations of raw data.
wd <- 'C:/Users/maryj/Documents/R/SierraPonds/Calibrations/Output'
setwd(wd)
write.csv(meanDF, file="Winter2020_2021_calibration.csv")
