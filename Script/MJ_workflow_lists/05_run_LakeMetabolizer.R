###############################################################################################################################################################
#Author: Adrianne Smits
#Date: 06/14/2022

# This script loads inputs necessary for estimating metabolism (LakeMetabolizer package) from
#corrected lake sensor data. Inputs to LakeMetabolizer include:
# doobs: observed DO concentration (mg/L), corrected for sensor drift
# do.sat: equilibrium DO concentration at temperature measured at DO sensor depth (corrected for lake altitude)
# k.gas: temperature specific gas transfer velocity (from k600 calculated from wind speed, or constant value if windspeed missing)
# irr: PAR in units of umol m^-2 sec^-1 (or vector of day/night for bookkeeping method; can estimate PAR from shortwave radiation)
# z.mix: mixed layer depth (meters) (calculated using LakeAnalyzer, or lake depth if polymictic)
# wtr: dataframe of water temperatures or single water temperature time series
#datetime: time series of datetimes (POSIXct class)
#Script then calculates daily metabolism using multiple models (bookkeeping,kalman, bayesian),
#plots and saves output

###############################################################################################################################################################
library(LakeMetabolizer)
library(rLakeAnalyzer)
library(lubridate)
library(R2jags)

###############################################################################################################################################################
##set working directory, load data, select desired year and lake to analyze
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data/ponds"
setwd(wd)

#Choose year and lake
year <- "2023" #desired open water period, including season
lake <- 'Emerald' #choose desired lake

###############################################################################################################################################################
#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_inputs.Rdata',sep="_"),files)])
ts.data <- inputs[[1]]
#ts.data$doy <- yday(ts.data$datetime)
DO_depth <- inputs[[2]]

###############################################################################################################################################################
# Calculate daily metabolism using the kalman method 
kalman.res = metab(ts.data, method='kalman', 
                   wtr.name=paste('wtr',DO_depth,sep="_"), do.obs.name='do.obs', irr.name='irr')
names(attributes(kalman.res))
kalman.params <- attributes(kalman.res)$params
kalman <- cbind(kalman.res, kalman.params[,c(3:6)])

#flag days with negative GPP or positive R
kalman$flag <- rep('n',length=length(kalman$doy))
kalman$flag[kalman$GPP<0]<-'y'
kalman$flag[kalman$R>0]<-'y'

#add a date and a datetime column (time is noon)
kalman$date <- as.Date(paste(kalman$year,"01-01",sep="-"))+(kalman$doy-1)
kalman$datetime <- as.POSIXct(paste(kalman$date,"12:00",sep=" "),tz="Etc/GMT-8")

###############################################################################################################################################################
#Calculate metabolism using the book-keeping methods 
bookkeep = metab(ts.data, method='bookkeep', 
                 wtr.name=paste('wtr',DO_depth,sep="_"), do.obs.name='do.obs', irr.name='daytime')
bookkeep
#flag days with negative GPP or positive R
bookkeep$flag <- rep('n',length=length(bookkeep$doy))
bookkeep$flag[bookkeep$GPP<0]<-'y'
bookkeep$flag[bookkeep$R>0]<-'y'

#add a date and a datetime column (time is noon)
bookkeep$date <- as.Date(paste(bookkeep$year,"01-01",sep="-"))+(bookkeep$doy-1)
bookkeep$datetime <- as.POSIXct(paste(bookkeep$date,"12:00",sep=" "),tz="Etc/GMT-8")
bookkeep

###############################################################################################################################################################
#Calculate metabolism using the bayesian method
bayesian = metab(ts.data, method='bayesian', 
                 wtr.name=paste('wtr',DO_depth,sep="_"), do.obs.name='do.obs', irr.name='irr')
bayesian 
attributes(bayesian)$metab#metabolism estimates
bayesian.sd <- attributes(bayesian)$metab.sd#standard deviations around the metabolism estimates
attributes(bayesian)$model#jags output/model runs
attributes(bayesian)$params#estimated parameters (coefficients for GPP and R models)
#add sd's to metabolism estimates dataframe
bayesian <- cbind(bayesian, bayesian.sd[,c(3:5)])
bayesian
#flag days with negative GPP or positive R
bayesian$flag <- rep('n',length=length(bayesian$doy))
bayesian$flag[bayesian$GPP<0]<-'y'
bayesian$flag[bayesian$R>0]<-'y'

#add a date and a datetime column (time is noon)
bayesian$date <- as.Date(paste(bayesian$year,"01-01",sep="-"))+(bayesian$doy-1)
bayesian$datetime <- as.POSIXct(paste(bayesian$date,"12:00",sep=" "),tz="Etc/GMT-8")
bayesian
###############################################################################################################################################################
##Plots
###############################################################################################################################################################
##plot metabolism estimates from 3 methods
par(mfrow=c(3,1))
plot(bookkeep$doy,bookkeep$GPP,type='b',pch=21,col='darkgreen',ylim=c(-0.5,4),xlim=c(225,280))
points(kalman$doy,kalman$GPP,type='b',pch=19,col='darkgreen')
points(bayesian$doy,bayesian$GPP,type='b',pch=22,col='darkgreen')
abline(h=0,lty=2)
abline(v=230)
abline(v=251)
mtext(side=2,line=2.5,'GPP',cex=1.2)
legend('topright',legend=c('bookkeep','kalman','bayesian'),pch=c(21,19,22),cex=1.1,bty='n')

plot(bookkeep$doy,bookkeep$R,type='b',pch=21,col='brown',ylim=c(-4,0.5),xlim=c(225,280))
points(kalman$doy,kalman$R,type='b',pch=19,col='brown')
points(bayesian$doy,bayesian$R,type='b',pch=22,col='brown')
abline(h=0,lty=2)
abline(v=230)
abline(v=251)
mtext(side=2,line=2.5,'R',cex=1.2)

plot(bookkeep$doy,bookkeep$NEP,type='b',pch=21,col='darkblue',ylim=c(-1.5,0.5),xlim=c(225,280))
points(kalman$doy,kalman$NEP,type='b',pch=19,col='darkblue')
points(bayesian$doy,bayesian$NEP,type='b',pch=22,col='darkblue')
abline(h=0,lty=2)
abline(v=230)
abline(v=251)
mtext(side=2,line=2.5,'NEP',cex=1.2)

##plot diel sw data, water temperature data, with metabolism estimates 
# date.lims <- c(as.POSIXct('2020-08-12 12:00',tz="Etc/GMT-8"),as.POSIXct('2020-09-15 12:00',tz="Etc/GMT-8"))
#date.lims <- c(as.POSIXct('2021-08-12 12:00',tz="Etc/GMT-8"),as.POSIXct('2021-09-15 12:00',tz="Etc/GMT-8"))
#date.lims <- c(as.POSIXct('2022-03-20 12:00',tz="Etc/GMT-8"),as.POSIXct('2022-07-15 12:00',tz="Etc/GMT-8"))
date.lims <- c(as.POSIXct('2022-08-20 12:00',tz="Etc/GMT-8"),as.POSIXct('2022-10-15 12:00',tz="Etc/GMT-8"))

par(mfrow=c(5,1))
# plot(ts.data$datetime,ts.data$swrad,type='l',xlim=date.lims)
plot(ts.data$datetime,ts.data$swrad,type='l')

mtext(expression(SW~(W~m^-2)),cex=1.2,line=2.5,side=2)

# plot(ts.data$datetime,ts.data[,grep('wtr',names(ts.data))[1]],type='l',xlim=date.lims,ylim=c(12,20))
plot(ts.data$datetime,ts.data[,grep('wtr',names(ts.data))[1]],type='l',ylim=c(12,20))
for(i in 1:length(grep('wtr',names(ts.data)))){
  lines(ts.data$datetime,ts.data[,grep('wtr',names(ts.data))[i]])
}
mtext(expression(Water~Temp~(degree*C)),cex=1.2,line=2.5,side=2)

# plot(bookkeep$datetime,bookkeep$GPP,type='b',ylim=c(-0.25,4),xlim=date.lims)
plot(bookkeep$datetime,bookkeep$GPP,type='b',ylim=c(-0.25,4))
points(kalman$datetime,kalman$GPP,type='b',pch=19)
points(bayesian$datetime,bayesian$GPP,type='b',pch=22)
abline(h=0,lty=2)
legend('topright',legend=c('bookkeep','kalman','bayesian'),pch=c(21,19,22),cex=1.1,bty='n')
mtext('GPP',cex=1.2,line=2.5,side=2)

# plot(bookkeep$datetime,bookkeep$R,type='b',ylim=c(-4,0.25),xlim=date.lims)
plot(bookkeep$datetime,bookkeep$R,type='b',ylim=c(-4,0.25))
points(kalman$datetime,kalman$R,type='b',pch=19)
points(bayesian$datetime,bayesian$R,type='b',pch=22)
abline(h=0,lty=2)
mtext('R',cex=1.2,line=2.5,side=2)

# plot(bookkeep$datetime,bookkeep$NEP,type='b',ylim=c(-1.5,0.5),xlim=date.lims)
plot(bookkeep$datetime,bookkeep$NEP,type='b',ylim=c(-1.5,0.5))
points(kalman$datetime,kalman$NEP,type='b',pch=19)
points(bayesian$datetime,bayesian$NEP,type='b',pch=22)
abline(h=0,lty=2)
mtext('NEP',cex=1.2,line=2.5,side=2)


###############################################################################################################################################################
##Save LakeMetabolizer outputs
outputs <- list(bookkeep,kalman,bayesian,ts.data,DO_depth)
save(outputs,file=paste("06",lake,year,"LakeMetabolizer_outputs.Rdata",sep="_"))

