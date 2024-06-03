#instrument calibration 2020
#MJ Farruggia





#pre-deployment instruments were done in separate water baths (conductivity in DI water, miniDOTs in 100% saturation water)
#post-deployment instruments done in separate water baths (conductivity in DI water, minidots in 0% and 100% saturation water)
#post-deployment instruments also done in shared water bath (execpt for those re-deployed into the Tokopah over winter)
#choose a single instrument to calibrate against, and snap all others to that value
#First check the plots to just check to see if it looks like anything shifted

#load function to manipulate minutes (to make sure everything matches - need to adjust minidots because they were manually launched so vary by a few minutes)
mns <- function(m) {
  x <- m * 60
  return(x)
}
#first load post-deployment in shared water bath and compare those values
setwd("C:/Users/maryj/Documents/R/SierraPonds/Calibrations/Data")

#load data by pond, and change format of columns to make uniform/change date format to POSIX 

###eml pond 1 - 4 instruments
eml1_minidotTOP=read.csv(file='EML1minidot_top.csv', header=T)
eml1_minidotTOP$SiteName="EML1MINIDOTTOP"
eml1_minidotMID=read.csv(file='EML1minidot_middle.csv', header=T)
eml1_minidotMID$SiteName="EML1MINIDOTMIDDLE"
eml1_tempTOP=read.csv(file='20868844_EML1temp.csv',header=T)
eml1_tempTOP$SiteName="EML1TEMPTOP"
eml1_conductivityBOTTOM=read.csv(file='20636144_EML1conductivity.csv',header=T)
eml1_conductivityBOTTOM$SiteName="EML1CONDUCTIVITYBOTTOM"

eml1_minidotTOP$GMT.08.00=as.POSIXct(eml1_minidotTOP$GMT.08.00,format='%Y-%m-%d %H:%M:%S', 'GMT')
eml1_minidotMID$GMT.08.00=as.POSIXct(eml1_minidotMID$GMT.08.00,format='%Y-%m-%d %H:%M:%S', 'GMT')
eml1_minidotTOP$GMT.08.00 <- eml1_minidotTOP$GMT.08.00  + mns(1)
eml1_minidotMID$GMT.08.00 <- eml1_minidotMID$GMT.08.00  + mns(7)
eml1_tempTOP$GMT.08.00=as.character(eml1_tempTOP$GMT.08.00)
eml1_tempTOP$GMT.08.00=as.POSIXct(eml1_tempTOP$GMT.08.00, format='%m/%d/%Y %H:%M','GMT')
eml1_conductivityBOTTOM$GMT.08.00=as.character(eml1_conductivityBOTTOM$GMT.08.00)
eml1_conductivityBOTTOM$GMT.08.00=as.POSIXct(eml1_conductivityBOTTOM$GMT.08.00, format='%m/%d/%Y %H:%M','GMT')
names(eml1_conductivityBOTTOM)[names(eml1_conductivityBOTTOM) == 'Temp'] = 'Temperature'
names(eml1_tempTOP)[names(eml1_tempTOP) == 'Temp'] = 'Temperature'
#combine instruments from this pond into one data frame
eml1.1=merge(eml1_minidotTOP, eml1_minidotMID, all=T)
eml1.2=merge(eml1.1, eml1_tempTOP, all=T)
eml1.3=merge(eml1.2, eml1_conductivityBOTTOM, all=T)
#restrict dates to only include the calibration time
eml1.3=eml1.3[with(eml1.3, GMT.08.00 > "2020-11-12 12:00:00" & GMT.08.00 < "2020-11-13 10:00:00"),]
eml1.3$Site="EML1"



###tok11 
tok11_minidotTOP=read.csv(file='TOK11minidot_top.csv', header=T)
tok11_minidotTOP$SiteName="TOK11MINIDOTTOP"
tok11_minidotMID=read.csv(file='TOK11minidot_middle.csv', header=T)
tok11_minidotMID$SiteName="TOK11MINIDOTMID"
tok11_tempTOP=read.csv(file='20868838_TOK11temp.csv',header=T)
tok11_tempTOP$SiteName="TOK11TEMPTOP"
tok11_conductivityBOTTOM=read.csv(file='20636158_TOK11conductivity.csv',header=T)
tok11_conductivityBOTTOM$SiteName="TOK11CONDUCTIVITYBOTTOM"

tok11_minidotTOP$GMT.08.00=as.POSIXct(tok11_minidotTOP$GMT.08.00,format='%Y-%m-%d %H:%M:%S', 'GMT')
tok11_minidotMID$GMT.08.00=as.POSIXct(tok11_minidotMID$GMT.08.00,format='%Y-%m-%d %H:%M:%S', 'GMT')
tok11_minidotTOP$GMT.08.00 <- tok11_minidotTOP$GMT.08.00  + mns(9)
tok11_minidotMID$GMT.08.00 <- tok11_minidotMID$GMT.08.00  + mns(7)

tok11_tempTOP$GMT.08.00=as.character(tok11_tempTOP$GMT.08.00)
tok11_tempTOP$GMT.08.00=as.POSIXct(tok11_tempTOP$GMT.08.00, format='%m/%d/%Y %H:%M','GMT')
tok11_conductivityBOTTOM$GMT.08.00=as.character(tok11_conductivityBOTTOM$GMT.08.00)
tok11_conductivityBOTTOM$GMT.08.00=as.POSIXct(tok11_conductivityBOTTOM$GMT.08.00, format='%m/%d/%Y %H:%M','GMT')
names(tok11_conductivityBOTTOM)[names(tok11_conductivityBOTTOM) == 'Temp'] = 'Temperature'
names(tok11_tempTOP)[names(tok11_tempTOP) == 'Temp'] = 'Temperature'
#combine instruments from this pond into one data frame
tok11.1=merge(tok11_minidotTOP, tok11_minidotMID, all=T)
tok11.2=merge(tok11.1, tok11_tempTOP, all=T)
tok11.3=merge(tok11.2, tok11_conductivityBOTTOM, all=T)
#restrict dates to only include the calibration time
tok11.3=tok11.3[with(tok11.3, GMT.08.00 > "2020-11-12 12:00:00" & GMT.08.00 < "2020-11-13 10:00:00"),]
tok11.3$Site="TOK11"




###TOK30 
tok30_minidotTOP=read.csv(file='TOK30minidot_top.csv', header=T)
tok30_minidotTOP$SiteName="TOK30MINIDOTTOP"
tok30_minidotMID=read.csv(file='TOK30minidot_middle.csv', header=T)
tok30_minidotMID$SiteName="TOK30MINIDOTMID"
tok30_tempTOP=read.csv(file='20868837_TOK30temp.csv',header=T)
tok30_tempTOP$SiteName="TOK30TEMPTOP"
tok30_conductivityBOTTOM=read.csv(file='20636157_TOK30conductivity.csv',header=T)
tok30_conductivityBOTTOM$SiteName="TOK30CONDUCTIVITYBOTTOM"

tok30_minidotTOP$GMT.08.00=as.POSIXct(tok30_minidotTOP$GMT.08.00,format='%Y-%m-%d %H:%M:%S', 'GMT')
tok30_minidotMID$GMT.08.00=as.POSIXct(tok30_minidotMID$GMT.08.00,format='%Y-%m-%d %H:%M:%S', 'GMT')
tok30_minidotTOP$GMT.08.00 <- tok30_minidotTOP$GMT.08.00  + mns(9)
tok30_minidotMID$GMT.08.00 <- tok30_minidotMID$GMT.08.00  + mns(8)

tok30_tempTOP$GMT.08.00=as.character(tok30_tempTOP$GMT.08.00)
tok30_tempTOP$GMT.08.00=as.POSIXct(tok30_tempTOP$GMT.08.00, format='%m/%d/%Y %H:%M','GMT')
tok30_conductivityBOTTOM$GMT.08.00=as.character(tok30_conductivityBOTTOM$GMT.08.00)
tok30_conductivityBOTTOM$GMT.08.00=as.POSIXct(tok30_conductivityBOTTOM$GMT.08.00, format='%m/%d/%Y %H:%M','GMT')
names(tok30_conductivityBOTTOM)[names(tok30_conductivityBOTTOM) == 'Temp'] = 'Temperature'
names(tok30_tempTOP)[names(tok30_tempTOP) == 'Temp'] = 'Temperature'
#combine instruments from this pond into one data frame
tok30.1=merge(tok30_minidotTOP, tok30_minidotMID, all=T)
tok30.2=merge(tok30.1, tok30_tempTOP, all=T)
tok30.3=merge(tok30.2, tok30_conductivityBOTTOM, all=T)
#restrict dates to only include the calibration time
tok30.3=tok30.3[with(tok30.3, GMT.08.00 > "2020-11-12 12:00:00" & GMT.08.00 < "2020-11-13 10:00:00"),]
tok30.3$Site="TOK30"




###topazpond - Top conductivity redeployed
topazpond_minidot=read.csv(file='TopazPondminidot.csv', header=T)
topazpond_minidot$SiteName="TOPAZPONDMINIDOT"
topazpond_tempTOP=read.csv(file='20868832_TopazPondtemp.csv',header=T)
topazpond_tempTOP$SiteName="TOPAZPONDTEMPTOP"
topazpond_conductivityBOTTOM=read.csv(file='20438689_Topazconductivitybottom.csv',header=T)
topazpond_conductivityBOTTOM$SiteName="TOPAZPONDCONDUCTIVITYBOTTOM"

topazpond_minidot$GMT.08.00=as.POSIXct(topazpond_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', 'GMT')
topazpond_minidot$GMT.08.00 <- topazpond_minidot$GMT.08.00  + mns(6)
topazpond_tempTOP$GMT.08.00=as.character(topazpond_tempTOP$GMT.08.00)
topazpond_tempTOP$GMT.08.00=as.POSIXct(topazpond_tempTOP$GMT.08.00, format='%m/%d/%Y %H:%M','GMT')

topazpond_conductivityBOTTOM$GMT.08.00=as.character(topazpond_conductivityBOTTOM$GMT.08.00)
topazpond_conductivityBOTTOM$GMT.08.00=as.POSIXct(topazpond_conductivityBOTTOM$GMT.08.00, format='%m/%d/%Y %H:%M','GMT')
names(topazpond_conductivityBOTTOM)[names(topazpond_conductivityBOTTOM) == 'Temp'] = 'Temperature'
names(topazpond_tempTOP)[names(topazpond_tempTOP) == 'Temp'] = 'Temperature'
#combine instruments from this pond into one data frame
topazpond.1=topazpond_minidot
topazpond.2=merge(topazpond.1, topazpond_tempTOP, all=T)
topazpond.3=merge(topazpond.2, topazpond_conductivityBOTTOM, all=T)
#restrict dates to only include the calibration time
topazpond.3=topazpond.3[with(topazpond.3, GMT.08.00 > "2020-11-12 12:00:00" & GMT.08.00 < "2020-11-13 10:00:00"),]
topazpond.3$Site="TOPAZPOND"

###ephemeral ponds
###eml pond 2  - instruments were redeployed


###tok71 - minidot was redeployed and wasn't in this shared water bath
tok71_conductivity=read.csv(file='20649551_TOK71conductivity.csv',header=T)
tok71_conductivity$GMT.08.00=as.character(tok71_conductivity$GMT.08.00)
tok71_conductivity$GMT.08.00=as.POSIXct(tok71_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M','GMT')
names(tok71_conductivity)[names(tok71_conductivity) == 'Temp'] = 'Temperature'
tok71.3=tok71_conductivity
tok71.3=tok71.3[with(tok71.3, GMT.08.00 > "2020-11-12 12:00:00" & GMT.08.00 < "2020-11-13 10:00:00"),]
tok71.3$SiteName="TOK71CONDUCTIVITY"
tok71.3$Site="TOK71"

###new pond
new_minidot=read.csv(file='Newminidot.csv', header=T)
new_minidot$SiteName="NEWMINIDOT"
new_conductivity=read.csv(file='20636156_Newconductivity.csv',header=T)
new_conductivity$SiteName="NEWCONDUCTIVITY"
new_minidot$GMT.08.00=as.POSIXct(new_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', 'GMT')
new_minidot$GMT.08.00 <- new_minidot$GMT.08.00  + mns(5)
new_conductivity$GMT.08.00=as.character(new_conductivity$GMT.08.00)
new_conductivity$GMT.08.00=as.POSIXct(new_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M','GMT')
names(new_conductivity)[names(new_conductivity) == 'Temp'] = 'Temperature'
#combine instruments from this pond into one data frame
new.1=merge(new_minidot,new_conductivity, all=T)
#restrict dates to only include the calibration time
new.1=new.1[with(new.1, GMT.08.00 > "2020-11-12 12:00:00" & GMT.08.00 < "2020-11-13 10:00:00"),]
new.1$Site="NEW"


###flatpond - conductivity redeployed
flat_minidot=read.csv(file='FlatPondminidot.csv', header=T)
flat_minidot$SiteName="FLATMINIDOT"
flat_minidot$GMT.08.00=as.POSIXct(flat_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', 'GMT')
flat_minidot$GMT.08.00 <- flat_minidot$GMT.08.00  + mns(1)
#combine instruments from this pond into one data frame
flat.1=flat_minidot
#restrict dates to only include the calibration time
flat.1=flat.1[with(flat.1, GMT.08.00 > "2020-11-12 12:00:00" & GMT.08.00 < "2020-11-13 10:00:00"),]
flat.1$Site="FLAT"


###starpond - minidot redeployed
star_conductivity=read.csv(file='20636154_Starconductivity.csv',header=T)
star_conductivity$SiteName="STARCONDUCTIVITY"
star_conductivity$GMT.08.00=as.character(star_conductivity$GMT.08.00)
star_conductivity$GMT.08.00=as.POSIXct(star_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M','GMT')
names(star_conductivity)[names(star_conductivity) == 'Temp'] = 'Temperature'
#combine instruments from this pond into one data frame
star.1=star_conductivity
#restrict dates to only include the calibration time
star.1=star.1[with(star.1, GMT.08.00 > "2020-11-12 12:00:00" & GMT.08.00 < "2020-11-13 10:00:00"),]
star.1$Site="STAR"



###grousepond
grouse_minidot=read.csv(file='GrousePondminidot.csv', header=T)
grouse_minidot$SiteName="GROUSEMINIDOT"
grouse_conductivity=read.csv(file='20636153_Grouseconductivity.csv',header=T)
grouse_conductivity$SiteName="GROUSECONDUCTIVITY"

grouse_minidot$GMT.08.00=as.POSIXct(grouse_minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', 'GMT')
#add 7 min to minidot
grouse_minidot$GMT.08.00 <- grouse_minidot$GMT.08.00  + mns(7)
grouse_conductivity$GMT.08.00=as.character(grouse_conductivity$GMT.08.00)
grouse_conductivity$GMT.08.00=as.POSIXct(grouse_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M','GMT')
names(grouse_conductivity)[names(grouse_conductivity) == 'Temp'] = 'Temperature'
#combine instruments from this pond into one data frame
grouse.1=merge(grouse_minidot,grouse_conductivity, all=T)
#restrict dates to only include the calibration time
grouse.1=grouse.1[with(grouse.1, GMT.08.00 > "2020-11-12 12:00:00" & GMT.08.00 < "2020-11-13 10:00:00"),]
grouse.1$Site="GROUSE"



#merge all the instruments/ponds together into one big dataframe to prepare for plotting
merge1=merge(eml1.3, flat.1, all=T)
merge2=merge(merge1, grouse.1, all=T)
merge3=merge(merge2, new.1, all=T)
merge4=merge(merge3, star.1, all=T)
merge5=merge(merge4, tok11.3, all=T)
merge6=merge(merge5, tok30.3, all=T)
merge7=merge(merge6, tok71.3, all=T)
merge8=merge(merge7, topazpond.3, all=T)


#plot temperature
#NEED TO SEPARATE TEMP BY INSTRUMENT - right now it's plotting all instruments within a single pond as a single instrument, which is WRONG

library(ggplot2)
library(directlabels)
library(tidyverse)
alltemp=ggplot(data=merge8, aes(x=GMT.08.00, y=Temperature, colour=SiteName))+
  geom_line()+
  labs(title="Post-deployment Temperature") + 
  theme_classic()+
  geom_text(data = filter(merge8, GMT.08.00 == median(GMT.08.00)),aes(label = SiteName))
alltemp

TOK11_temp=merge8[merge8$Site == "TOK11",]
TOK11temp=ggplot()+
  geom_line(data=TOK11_temp, aes(x=GMT.08.00, y=Temperature, colour=SiteName))+
  labs(title="Post-deployment Temperature-tok11") + 
  theme_classic()
TOK11temp


TOPAZ_temp=merge8[merge8$Site == "TOPAZPOND",]
TOPAZtemp=ggplot()+
  geom_line(data=TOPAZ_temp, aes(x=GMT.08.00, y=Temperature, colour=SiteName))+
  labs(title="Post-deployment Temperature-topaz") + 
  theme_classic()
TOPAZtemp



#to adjust, pick one instrument that looks good, then snap all others to that value. I'm choosing TOK11TEMPTOP, because it's right in the middle of where everything is, and when I view it individually, it doesn't have any strange fluctuations anywhere during the water bath period. 
#I'm going to calculate the difference between x instrument and TOK11TEMPTOP.  
#I want to subtract TOK11TEMPTOP from every time step, from x instrument measurement at every time step
#Then average the difference to find out the value to subtract or add to every instrument (a single value to apply to the actual data)

#because the time stamps are slightly different (because the miniDOTs were turned on manually, while the others were launched via computer delayed launch), need to adjust time stamps appropriately (make sure to snap to TOK11TEMPTOP)
#I need to match each time step because the temperature of the water bath is not constant. 
#I did this above using the function at the beginning of this code file. 
#the only discrepancy is that topazpondminidot logged every 5 minutes, instead of every 10 minutes. 


#I want to calculate the difference between each instrument and TOK11TEMPTOP, and find the mean difference over the water bath period. Then I'll apply that single correction (unique to each instrument) to the actual dataset. 

#the math will be temp of each logger - temp of the tok11temptop logger
#do this for EVERY TIME STEP
#note that TOPAZPONDMINIDOT logged every 5 minutes, so need to deal with that - only calculate the matching date/times.


#join final table (merge8) with another table (tok11_tempTOP) which specifies TOK11TEMPTOP with its own temperature column

calDF=tok11_tempTOP
names(calDF)[names(calDF) == 'Temperature'] = 'calDF_temperature'
calDF$SiteName=NULL
calDF$?...=NULL
allDF=merge(calDF, merge8, all=T)

#now I have a df (allDF) that has two temperature columns - one with the instrument reading, and one with values I'll calibrate against. 
#now just need the difference for each time step, which I'll get by subtracting columns and placing the result in a column called "Diff"

allDF$Diff=allDF$calDF_temperature - allDF$Temperature

#take the average of column "Diff" for each logger separately. 

library(doBy)
meanDF=summaryBy(Diff~SiteName, allDF, FUN=mean, na.rm=T)


#now need to address the instruments that were redeployed before running in the shared water bath (were run in baths of instruments of the same type, i.e. all miniDOTs, all conductivity loggers.)
#these 6 instruments are:
#Star miniDOT 979330
#TOK71 miniDOT 442116
#Emlpond2 minidot 332834
#Flat Conductivity 20438693
#Topaz top conductivity 20636155
#Emlpond2 conductivity 20636145

#Load the calibration data from these instruments
starminidot=read.csv("STARMINIDOT.csv", header=T)
TOK71minidot=read.csv("TOK71minidot.csv", header=T)
EML2minidot=read.csv("EML2minidot.csv", header=T)

starminidot$GMT.08.00=as.POSIXct(starminidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', 'GMT')
starminidot$GMT.08.00 <- starminidot$GMT.08.00  + mns(3)
starminidot$SiteName="STARMINIDOT"

TOK71minidot$GMT.08.00=as.POSIXct(TOK71minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', 'GMT')
TOK71minidot$GMT.08.00 <- TOK71minidot$GMT.08.00  + mns(4)
TOK71minidot$SiteName="TOK71MINIDOT"

EML2minidot$GMT.08.00=as.POSIXct(EML2minidot$GMT.08.00,format='%Y-%m-%d %H:%M:%S', 'GMT')
EML2minidot$GMT.08.00 <- EML2minidot$GMT.08.00  + mns(4)
EML2minidot$SiteName="EML2MINIDOT"

#need to compare to CORRECTED instruments that were in the same water bath
#so I need 1 CORRECTED minidot and 1 CORRECTED conductivity logger that has data from being run in those same buckets in Sept.  
#tok11 minidot
TOK11minidot2=tok11_minidotTOP #correction -0.03758779
TOK11minidot2$Temperature2=TOK11minidot2$Temperature - 0.03758779
TOK11minidot2$Temperature=NULL
TOK11minidot2$GMT.08.00 <- TOK11minidot2$GMT.08.00  + mns(1)
TOK11minidot2$Unix.Timestamp=NULL
TOK11minidot2$SiteName=NULL
TOK11minidot2$UTC_Date_Time=NULL
TOK11minidot2$Battery=NULL
TOK11minidot2$Dissolved.Oxygen=NULL
TOK11minidot2$Dissolved.Oxygen.Saturation=NULL
TOK11minidot2$Q=NULL

flatminidot2=flat_minidot #correction 0.04559542
flatminidot2$Temperature2=flatminidot2$Temperature + 0.04559542
flatminidot2$Temperature=NULL
#flatminidot2$GMT.08.00 <- flatminidot2$GMT.08.00  + mns(5)
flatminidot2$Unix.Timestamp=NULL
flatminidot2$SiteName=NULL
flatminidot2$UTC_Date_Time=NULL
flatminidot2$Battery=NULL
flatminidot2$Dissolved.Oxygen=NULL
flatminidot2$Dissolved.Oxygen.Saturation=NULL
flatminidot2$Q=NULL

GROUSE_SC=read.csv("GROUSEMINIDOT_SEPCAL.csv", header=T) #0.01222901
GROUSE_SC$Temperature2=GROUSE_SC$Temperature + 0.01222901
GROUSE_SC$GMT.08.00=as.POSIXct(GROUSE_SC$GMT.08.00,format='%Y-%m-%d %H:%M:%S', 'GMT')
GROUSE_SC$GMT.08.00 <- GROUSE_SC$GMT.08.00  + mns(4)
GROUSE_SC$Temperature=NULL
GROUSE_SC$Unix.Timestamp=NULL
GROUSE_SC$SiteName=NULL
GROUSE_SC$UTC_Date_Time=NULL
GROUSE_SC$Battery=NULL
GROUSE_SC$Dissolved.Oxygen=NULL
GROUSE_SC$Dissolved.Oxygen.Saturation=NULL
GROUSE_SC$Q=NULL


#combine instruments from this pond into one data frame
minidot1=merge(starminidot,TOK71minidot, all=T)
minidot2=merge(minidot1, EML2minidot, all=T)
minidot3=merge( GROUSE_SC, minidot2, all=T)
#restrict dates to only include the calibration time
minidot3=minidot3[with(minidot3, GMT.08.00 > "2020-09-21 12:00:00" & GMT.08.00 < "2020-09-23 12:00:00"),]

#apply correction to minidots
correctedmd=minidot3
correctedmd$Diff=correctedmd$Temperature2 - correctedmd$Temperature

meanDF_md=summaryBy(Diff~SiteName, correctedmd, FUN=mean, na.rm=T)







#need to compare to CORRECTED instruments that were in the same water bath
#so I need 1 CORRECTED minidot and 1 CORRECTED conductivity logger that has data from being run in those same buckets in Sept.  
#tok11 minidot
#tok71conductivity
TOK71_conductivity2=read.csv("20649551-TOK71conductivity2.csv",header=T) #correction -0.43339695
TOK71_conductivity2$GMT.08.00=as.character(TOK71_conductivity2$GMT.08.00)
TOK71_conductivity2$GMT.08.00=as.POSIXct(TOK71_conductivity2$GMT.08.00, format='%m/%d/%Y %H:%M','GMT')
names(TOK71_conductivity2)[names(TOK71_conductivity2) == 'Temp'] = 'Temperature'
TOK71_conductivity2$Temperature2=TOK71_conductivity2$Temperature - 0.43339695
TOK71_conductivity2$Conductivity=NULL
TOK71_conductivity2$Temperature=NULL
TOK71_conductivity2$?...=NULL

EML2_conductivity=read.csv("20636145-EML2conductivity.csv",header=T)
topazpond_conductivityTOP=read.csv("20636155-topaztopconductivity.csv",header=T)
flat_conductivity=read.csv("20438693-FlatConductivity.csv",header=T)


EML2_conductivity$GMT.08.00=as.character(EML2_conductivity$GMT.08.00)
EML2_conductivity$GMT.08.00=as.POSIXct(EML2_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M','GMT')
names(EML2_conductivity)[names(EML2_conductivity) == 'Temp'] = 'Temperature'
EML2_conductivity$SiteName="EML2CONDUCTIVITY"

topazpond_conductivityTOP$GMT.08.00=as.character(topazpond_conductivityTOP$GMT.08.00)
topazpond_conductivityTOP$GMT.08.00=as.POSIXct(topazpond_conductivityTOP$GMT.08.00, format='%m/%d/%Y %H:%M','GMT')
names(topazpond_conductivityTOP)[names(topazpond_conductivityTOP) == 'Temp'] = 'Temperature'
topazpond_conductivityTOP$SiteName="TOPAZPONDCONDUCTIVITY"

flat_conductivity$GMT.08.00=as.character(flat_conductivity$GMT.08.00)
flat_conductivity$GMT.08.00=as.POSIXct(flat_conductivity$GMT.08.00, format='%m/%d/%Y %H:%M','GMT')
names(flat_conductivity)[names(flat_conductivity) == 'Temp'] = 'Temperature'
flat_conductivity$SiteName="FLATCONDUCTIVITY"



#combine instruments from this pond into one data frame
cond1=merge(EML2_conductivity,topazpond_conductivityTOP, all=T)
cond2=merge(cond1, flat_conductivity, all=T)
cond3=merge( TOK71_conductivity2, cond2, all=T)
#restrict dates to only include the calibration time
cond3=cond3[with(cond3, GMT.08.00 > "2020-09-20 13:00:00" & GMT.08.00 < "2020-09-23 20:00:00"),]

#apply correction to loggers
correctedcond=cond3
correctedcond$Diff=correctedcond$Temperature2 - correctedcond$Temperature

meanDF_cond=summaryBy(Diff~SiteName, correctedcond, FUN=mean, na.rm=T)



#COMBINE ALL MEAN TABLES to show all adjustments on the same table
m1=rbind(meanDF_md, meanDF_cond)
m2=rbind(m1, meanDF)
#export as a table - these are the correction values to apply to the actual data set
#write.csv(m2, file="thermistorcalibrationvalues.csv")


#since miniDOT values were high, compare all the instruments in the bucket to see if it's real, or a product of my crude intercalibration
EML1_SC=read.csv("EML1MINIDOTMID_SEPCAL.csv", header=T)
EML1_SC$GMT.08.00=as.POSIXct(EML1_SC$GMT.08.00,format='%Y-%m-%d %H:%M:%S', 'GMT')
EML1_SC$GMT.08.00 <- EML1_SC$GMT.08.00  + mns(9)
EML1_SC$SiteName="EML1"

NEW_SC=read.csv("NEWMINIDOT_SEPCAL.csv", header=T)
NEW_SC$GMT.08.00=as.POSIXct(NEW_SC$GMT.08.00,format='%Y-%m-%d %H:%M:%S', 'GMT')
NEW_SC$GMT.08.00 <- NEW_SC$GMT.08.00  + mns(1)
NEW_SC$SiteName="NEW"

TOK11_SC=read.csv("TOK11MINIDOTTOP_SEPCAL.csv", header=T)
TOK11_SC$GMT.08.00=as.POSIXct(TOK11_SC$GMT.08.00,format='%Y-%m-%d %H:%M:%S', 'GMT')
#TOK11_SC$GMT.08.00 <- TOK11_SC$GMT.08.00  + mns(1)
TOK11_SC$SiteName="TOK11"

GROUSE_SC=read.csv("GROUSEMINIDOT_SEPCAL.csv", header=T)
GROUSE_SC$GMT.08.00=as.POSIXct(GROUSE_SC$GMT.08.00,format='%Y-%m-%d %H:%M:%S', 'GMT')
GROUSE_SC$GMT.08.00 <- GROUSE_SC$GMT.08.00  + mns(4)
GROUSE_SC$SiteName="GROUSE"

#merge with df minidot2
m1=merge(minidot2, EML1_SC, all=T)
m2=merge(m1, NEW_SC, all=T)
m3=merge(m2, TOK11_SC, all=T)
m4=merge(m3, GROUSE_SC, all=T)

#restrict dates to only include the calibration time
m4=m4[with(m4, GMT.08.00 > "2020-09-21 12:00:00" & GMT.08.00 < "2020-09-23 12:00:00"),]

#plot to see all minidots from this cal
allminidot=ggplot(data=m4, aes(x=GMT.08.00, y=Temperature, colour=SiteName))+
  geom_line()+
  labs(title="MiniDOT Post-deployment Temperature") + 
  theme_classic()+
  geom_text(data = filter(m4, GMT.08.00 == median(GMT.08.00)),aes(label = SiteName))
allminidot