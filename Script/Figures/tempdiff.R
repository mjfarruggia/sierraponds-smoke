
#temp diff plots

###############################################################################################################################################################
library(LakeMetabolizer)
library(rLakeAnalyzer)
library(lubridate)
library(R2jags)
library(ggplot2)
library(cowplot)
library(dplyr)


################################################################################################################################################################
#load met and smoke data
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd) 
files <- list.files()#all the files in wd

load('SmokeData_forMJ.Rdata')
smoke_topaz_20 = SmokeData_forMJ [[1]]
smoke_eml_20 = SmokeData_forMJ [[2]]
smoke_topaz_21 = SmokeData_forMJ [[3]]
smoke_eml_21 = SmokeData_forMJ [[4]]



smoke_topaz_20 %>%
  filter(month %in% c(8, 9, 10), Smoke.day == "y") %>%
  summarise(count = n())
smoke_topaz_21 %>%
  filter(month %in% c(8, 9, 10), Smoke.day == "y") %>%
  summarise(count = n())
smoke_eml_20 %>%
  filter(month %in% c(8, 9, 10), Smoke.day == "y") %>%
  summarise(count = n())
smoke_eml_20 %>%
  filter(month %in% c(8, 9, 10), Smoke.day == "y") %>%
  summarise(count = n())

rain = read.csv("Precip_05_21_EMLMET.csv", header=T)
rain$Datetime=as.POSIXct(rain$Datetime, format='%m/%d/%y %H:%M', tz="Etc/GMT-8")
rain=rain[with(rain, Datetime > "2019-07-01 00:00:00" & Datetime < "2021-10-30 00:00:00"),]
# If Rain_mm >4, make RainIntensity "High". If Rain_mm >2 but <4, make RainIntensity "Medium".  If Rain_mm <2, make RainIntensity "Low". 
rain <- rain %>%
  mutate(RainIntensity = case_when(
    Rain_mm > 4 ~ "High",
    Rain_mm > 2 ~ "Medium",
    Rain_mm > 0 & Rain_mm <= 2 ~ "Low",
    TRUE ~ NA
  ))


weather.data.e <- read.csv("Emerald_MET_data_2019_2021_hourly.csv",header=TRUE)
weather.data.e$Date = as.Date(weather.data.e$Datetime , format='%m/%d/%Y')
weather.data.e$Datetime=as.POSIXct(weather.data.e$Datetime, format='%m/%d/%Y %H:%M', tz="Etc/GMT-8")
weather.data.e=merge(weather.data.e, rain, all=T)

weather.data.t <- read.csv("Topaz_MET_data_2019_2021_hourly.csv",header=TRUE)
weather.data.t$Date = as.Date(weather.data.t$Datetime , format='%m/%d/%Y')
weather.data.t$DOY = yday(weather.data.t$Date)
weather.data.t$Datetime=as.POSIXct(weather.data.t$Datetime, format='%m/%d/%Y %H:%M', tz="Etc/GMT-8")
weather.data.t$SW_solar=weather.data.t$SW_solar * -1
weather.data.t=merge(weather.data.t, rain, all=T)

smoke.density=read.csv("RAPID_sites_smoke_density.csv", header=T)
smoke.density$Date <- as.Date(with(smoke.density, paste(Year, Month, Day, sep = "-")))
smoke.density = subset(smoke.density, select = c(Date, Year, Topaz.Lake, Emerald.Lake))
smoke.density$DOY=yday(smoke.density$Date)

weather.data.e=merge(weather.data.e, smoke.density, all=T)
weather.data.e$Topaz.Lake=NULL
weather.data.e.20=weather.data.e[with(weather.data.e, Datetime > "2020-08-02 00:00:00" & Datetime < "2020-10-30 00:00:00"),]
weather.data.e.21=weather.data.e[with(weather.data.e, Datetime > "2021-08-02 00:00:00" & Datetime < "2021-10-30 00:00:00"),]

weather.data.e.22=read.csv("Emerald_MET_data_2019_2023_hourly.csv",header=TRUE)
weather.data.e.22$Date = as.Date(weather.data.e.22$Datetime , format='%Y-%m-%d')
weather.data.e.22$DOY = yday(weather.data.e.22$Date)
weather.data.e.22$Datetime=as.POSIXct(weather.data.e.22$Datetime, format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
weather.data.e.22$SW_solar=weather.data.e.22$SW_solar * -1
weather.data.e.22=weather.data.e.22[with(weather.data.e.22, Date > "2022-04-02" & Date < "2022-10-30"),]
weather.data.e.22 <- weather.data.e.22 %>%
  mutate(RainIntensity = case_when(
    Rain_mm > 4 ~ "High",
    Rain_mm > 2 ~ "Medium",
    Rain_mm > 0 & Rain_mm <= 2 ~ "Low",
    TRUE ~ NA
  ))

weather.data.e.20 <- weather.data.e.20[complete.cases(weather.data.e.20$Datetime), ]
avg.sw.e.20 <- weather.data.e.20 %>%
  group_by(Date) %>%
  summarize(
    swrad = mean(SW_solar, na.rm = TRUE),
    airt = mean(AirTemp, na.rm = TRUE),
    rain= max(Rain_mm, na.rm=T),
    RainIntensity =RainIntensity  ) %>%
  mutate(Date = ymd_hms(paste(Date, "12:00:00")))

weather.data.e.21 <- weather.data.e.21[complete.cases(weather.data.e.21$Datetime), ]
avg.sw.e.21= weather.data.e.21%>%
  group_by(Date) %>%
  summarize(
    swrad = mean(SW_solar, na.rm = TRUE),
    airt = mean(AirTemp, na.rm = TRUE),
    rain= max(Rain_mm, na.rm=T),
    RainIntensity =RainIntensity 
  ) %>%
  mutate(Date = ymd_hms(paste(Date, "12:00:00")))

weather.data.e.22 <- weather.data.e.22[complete.cases(weather.data.e.22$Datetime), ]
avg.sw.e.22= weather.data.e.22%>%
  group_by(Date) %>%
  summarize(
    swrad = mean(SW_solar, na.rm = TRUE),
    airt = mean(AirTemp, na.rm = TRUE),
    rain= max(Rain_mm, na.rm=T),
    RainIntensity =RainIntensity 
  ) %>%
  mutate(Date = ymd_hms(paste(Date, "12:00:00")))
avg.sw.e.22$swrad=avg.sw.e.22$swrad*-1

weather.data.t=merge(weather.data.t, smoke.density, all=T)
weather.data.t$Emerald.Lake=NULL
weather.data.t.20=weather.data.t[with(weather.data.t, Datetime > "2020-08-02 00:00:00" & Datetime < "2020-10-30 00:00:00"),]
weather.data.t.21=weather.data.t[with(weather.data.t, Datetime > "2021-08-02 00:00:00" & Datetime < "2021-10-30 00:00:00"),]

weather.data.t.22=read.csv("Topaz_MET_data_2019_2023_hourly.csv",header=TRUE)
weather.data.t.22$Date = as.Date(weather.data.t.22$Datetime , format='%m/%d/%Y')
weather.data.t.22$DOY = yday(weather.data.t.22$Date)
weather.data.t.22$Datetime=as.POSIXct(weather.data.t.22$Datetime, format='%m/%d/%Y %H:%M', tz="Etc/GMT-8")
weather.data.t.22$SW_solar=weather.data.t.22$SW_solar * -1
weather.data.t.22=weather.data.t.22[with(weather.data.t.22, Date > "2022-04-02" & Date < "2022-10-30"),]
weather.data.t.22 <- weather.data.t.22 %>%
  mutate(RainIntensity = case_when(
    Rain_mm > 4 ~ "High",
    Rain_mm > 2 ~ "Medium",
    Rain_mm > 0 & Rain_mm <= 2 ~ "Low",
    TRUE ~ NA
  ))
weather.data.t.20 <- weather.data.t.20[complete.cases(weather.data.t.20$Datetime), ]
avg.sw.t.20= weather.data.t.20%>%
  group_by(Date) %>%
  summarize(
    swrad = mean(SW_solar, na.rm = TRUE),
    airt = mean(AirTemp, na.rm = TRUE),
    rain= max(Rain_mm, na.rm=T),
    RainIntensity =RainIntensity 
  ) %>%
  mutate(Date = ymd_hms(paste(Date, "12:00:00")))

weather.data.t.21 <- weather.data.t.21[complete.cases(weather.data.t.21$Datetime), ]
avg.sw.t.21 <- weather.data.t.21 %>%
  group_by(Date) %>%
  summarize(
    swrad = mean(SW_solar, na.rm = TRUE),
    airt = mean(AirTemp, na.rm = TRUE),
    rain= max(Rain_mm, na.rm=T),
    RainIntensity =RainIntensity 
  ) %>%
  mutate(Date = ymd_hms(paste(Date, "12:00:00", sep = " ")))


weather.data.t.22 <- weather.data.t.22[complete.cases(weather.data.t.22$Datetime), ]
avg.sw.t.22 <- weather.data.t.22 %>%
  group_by(Date) %>%
  summarize(
    swrad = mean(SW_solar, na.rm = TRUE),
    airt = mean(AirTemp, na.rm = TRUE),
    rain= max(Rain_mm, na.rm=T),
    RainIntensity =RainIntensity 
  ) %>%
  mutate(Date = ymd_hms(paste(Date, "12:00:00", sep = " ")))



################################################################################################################################################################
#add in pond/lake info
################################################################################################################################################################

##set working directory, load data, select desired year and lake to analyze
#load lake data
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd) 
files <- list.files()#all the files in wd
load('Emerald_Topaz_sensor_data_2020_forMJ.Rdata')
emerald20= ts.2020.forMJ  [[1]]
topaz20= ts.2020.forMJ  [[2]]

load('Emerald_Topaz_sensor_data_2021_forMJ.Rdata')
emerald21= ts.2021.forMJ  [[1]]
topaz21= ts.2021.forMJ  [[2]]

load('EML_Topaz_metab_forMJ.Rdata')
metab_topaz_20 = EML_Topaz_metab [[1]]
metab_eml_20 = EML_Topaz_metab [[2]]
metab_topaz_21 = EML_Topaz_metab [[3]]
metab_eml_21 = EML_Topaz_metab [[4]]


#load pond data 2020 - EMLPOND1
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data/ponds"
setwd(wd)

#Choose year 
year <- "S20" #desired open water period, including season

#choose lake
lake <- 'EMLPOND1' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
bookkeep.EMLPOND1.20 <- outputs[[1]]
kalman.EMLPOND1.20 <- outputs[[2]]
bayesian.EMLPOND1.20 <- outputs[[3]]
ts.data.EMLPOND1.20 <- outputs[[4]]
DO_depth.EMLPOND1.20 <- outputs[[5]]



#load pond data - TOK11
#Choose year and lake
lake <- 'TOK11' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
bookkeep.TOK11.20 <- outputs[[1]]
kalman.TOK11.20 <- outputs[[2]]
bayesian.TOK11.20 <- outputs[[3]]
ts.data.TOK11.20 <- outputs[[4]]
DO_depth.TOK11.20 <- outputs[[5]]




#load pond data - TOPAZPOND
#Choose year and lake
lake <- 'TOPAZPOND' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
bookkeep.TOPAZPOND.20 <- outputs[[1]]
kalman.TOPAZPOND.20 <- outputs[[2]]
bayesian.TOPAZPOND.20 <- outputs[[3]]
ts.data.TOPAZPOND.20 <- outputs[[4]]
DO_depth.TOPAZPOND.20 <- outputs[[5]]





#load pond data - TOK30
#Choose year and lake
lake <- 'TOK30' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
bookkeep.TOK30.20 <- outputs[[1]]
kalman.TOK30.20 <- outputs[[2]]
bayesian.TOK30.20 <- outputs[[3]]
ts.data.TOK30.20 <- outputs[[4]]
DO_depth.TOK30.20 <- outputs[[5]]

#load winter data to fill in missing gaps
#load pond data - TOPAZPOND
#Choose year and lake
year <- "W20" #desired open water period, including season
lake <- 'TOPAZPOND' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
bookkeep.TOPAZPOND.W20 <- outputs[[1]]
kalman.TOPAZPOND.W20 <- outputs[[2]]
bayesian.TOPAZPOND.W20 <- outputs[[3]]
ts.data.TOPAZPOND.W20 <- outputs[[4]]
DO_depth.TOPAZPOND.W20 <- outputs[[5]]


#load pond data 2021 - EMLPOND1
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data/ponds"
setwd(wd)

#Choose year 
year <- "S21" #desired open water period, including season

#choose lake
lake <- 'EMLPOND1' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
bookkeep.EMLPOND1.21 <- outputs[[1]]
kalman.EMLPOND1.21 <- outputs[[2]]
bayesian.EMLPOND1.21 <- outputs[[3]]
ts.data.EMLPOND1.21 <- outputs[[4]]
DO_depth.EMLPOND1.21 <- outputs[[5]]



#load pond data - TOK11
#Choose year and lake
lake <- 'TOK11' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
bookkeep.TOK11.21 <- outputs[[1]]
kalman.TOK11.21 <- outputs[[2]]
bayesian.TOK11.21 <- outputs[[3]]
ts.data.TOK11.21 <- outputs[[4]]
DO_depth.TOK11.21 <- outputs[[5]]




#load pond data - TOPAZPOND
#Choose year and lake
lake <- 'TOPAZPOND' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
bookkeep.TOPAZPOND.21 <- outputs[[1]]
kalman.TOPAZPOND.21 <- outputs[[2]]
bayesian.TOPAZPOND.21 <- outputs[[3]]
ts.data.TOPAZPOND.21 <- outputs[[4]]
DO_depth.TOPAZPOND.21 <- outputs[[5]]




#load pond data - TOK30
#Choose year and lake
lake <- 'TOK30' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
bookkeep.TOK30.21 <- outputs[[1]]
kalman.TOK30.21 <- outputs[[2]]
bayesian.TOK30.21 <- outputs[[3]]
ts.data.TOK30.21 <- outputs[[4]]
DO_depth.TOK30.21 <- outputs[[5]]





#load pond data 2022 - EMLPOND1
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data/ponds"
setwd(wd)

#Choose year 
year <- "S22" #desired open water period, including season

#choose lake
lake <- 'EMLPOND1' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
bookkeep.EMLPOND1.22 <- outputs[[1]]
kalman.EMLPOND1.22 <- outputs[[2]]
bayesian.EMLPOND1.22 <- outputs[[3]]
ts.data.EMLPOND1.22 <- outputs[[4]]
DO_depth.EMLPOND1.22 <- outputs[[5]]



#load pond data - TOK11
#Choose year and lake
lake <- 'TOK11' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
bookkeep.TOK11.22 <- outputs[[1]]
kalman.TOK11.22 <- outputs[[2]]
bayesian.TOK11.22 <- outputs[[3]]
ts.data.TOK11.22 <- outputs[[4]]
DO_depth.TOK11.22 <- outputs[[5]]




#load pond data - TOPAZPOND
#Choose year and lake
lake <- 'TOPAZPOND' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
bookkeep.TOPAZPOND.22 <- outputs[[1]]
kalman.TOPAZPOND.22 <- outputs[[2]]
bayesian.TOPAZPOND.22 <- outputs[[3]]
ts.data.TOPAZPOND.22 <- outputs[[4]]
DO_depth.TOPAZPOND.22 <- outputs[[5]]




#load pond data - TOK30
#Choose year and lake
lake <- 'TOK30' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
bookkeep.TOK30.22 <- outputs[[1]]
kalman.TOK30.22 <- outputs[[2]]
bayesian.TOK30.22 <- outputs[[3]]
ts.data.TOK30.22 <- outputs[[4]]
DO_depth.TOK30.22 <- outputs[[5]]




#Choose year 
year <- "2023" #desired open water period, including season

#choose lake
lake <- 'Emerald' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
bookkeep.emerald.22 <- outputs[[1]]
kalman.emerald.22 <- outputs[[2]]
bayesian.emerald.22 <- outputs[[3]]
ts.data.emerald.22 <- outputs[[4]]
DO_depth.emerald.22 <- outputs[[5]]


#Choose year 
year <- "2023" #desired open water period, including season

#choose lake
lake <- 'Topaz' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
bookkeep.topaz.22 <- outputs[[1]]
kalman.topaz.22 <- outputs[[2]]
bayesian.topaz.22 <- outputs[[3]]
ts.data.topaz.22 <- outputs[[4]]
DO_depth.topaz.22 <- outputs[[5]]


smoke_eml_20=smoke_eml_20[with(smoke_eml_20, Date> "2020-08-01 00:00:00" & Date < "2020-10-30 00:00:00"),]
ts.data.EMLPOND1.20.filter=ts.data.EMLPOND1.20[with(ts.data.EMLPOND1.20, datetime > "2020-08-01 00:00:00" & datetime < "2020-10-30 00:00:00" ),]
ts.data.TOK11.20.filter=ts.data.TOK11.20[with(ts.data.TOK11.20, datetime > "2020-08-01 00:00:00" & datetime < "2020-10-30 00:00:00" ),]
ts.data.TOPAZPOND.20.filter=ts.data.TOPAZPOND.20[with(ts.data.TOPAZPOND.20, datetime > "2020-08-01 00:00:00" & datetime < "2020-10-30 00:00:00"),]
ts.data.TOK30.20.filter=ts.data.TOK30.20[with(ts.data.TOK30.20, datetime > "2020-08-01 00:00:00" & datetime < "2020-10-30 00:00:00"),]
emerald20.filter=emerald20[with(emerald20, datetime > "2020-08-01 00:00:00" & datetime < "2020-10-30 00:00:00"),]
topaz20.filter=topaz20[with(topaz20, datetime > "2020-08-01 00:00:00"& datetime < "2020-10-30 00:00:00" ),]


avg.sw.e.21=avg.sw.e.21[with(avg.sw.e.21, Date >  "2021-08-01 00:00:00" & Date < "2021-10-30 00:00:00" ),]
avg.sw.t.21=avg.sw.t.21[with(avg.sw.t.21, Date >  "2021-08-01 00:00:00" & Date < "2021-10-30 00:00:00"  ),]
smoke_eml_21=smoke_eml_21[with(smoke_eml_21, Date >  "2021-08-01 00:00:00" & Date < "2021-10-30 00:00:00"  ),]
smoke_topaz_21=smoke_topaz_21[with(smoke_topaz_21, Date >  "2021-08-01 00:00:00" & Date < "2021-10-30 00:00:00"  ),]
ts.data.EMLPOND1.21.filter=ts.data.EMLPOND1.21[with(ts.data.EMLPOND1.21, datetime > "2021-08-01 00:00:00" & datetime < "2021-10-30 00:00:00" ),]
ts.data.TOK11.21.filter=ts.data.TOK11.21[with(ts.data.TOK11.21, datetime > "2021-08-01 00:00:00" & datetime < "2021-10-30 00:00:00"),]
ts.data.TOPAZPOND.21.filter=ts.data.TOPAZPOND.21[with(ts.data.TOPAZPOND.21, datetime > "2021-08-01 00:00:00" & datetime < "2021-10-30 00:00:00" ),]
ts.data.TOK30.21.filter=ts.data.TOK30.21[with(ts.data.TOK30.21, datetime > "2021-08-01 00:00:00" & datetime < "2021-10-30 00:00:00" ),]
emerald21.filter=emerald21[with(emerald21, datetime > "2021-08-01 00:00:00" & datetime < "2021-10-30 00:00:00"),]
topaz21.filter=topaz21[with(topaz21, datetime > "2021-08-01 00:00:00"& datetime < "2021-10-30 00:00:00" ),]


avg.sw.e.22.filter=avg.sw.e.22[with(avg.sw.e.22, Date > "2022-08-01 00:00:00" & Date < "2022-10-30 00:00:00"),]
avg.sw.t.22.filter=avg.sw.t.22[with(avg.sw.t.22, Date > "2022-08-01 00:00:00" & Date < "2022-10-30 00:00:00"),]
ts.data.EMLPOND1.22.filter=ts.data.EMLPOND1.22[with(ts.data.EMLPOND1.22, datetime > "2022-08-01 00:00:00" & datetime < "2022-10-07 00:00:00"),]
ts.data.TOK11.22.filter=ts.data.TOK11.22[with(ts.data.TOK11.22, datetime > "2022-08-01 00:00:00" & datetime < "2022-10-07 00:00:00"),]
ts.data.TOPAZPOND.22.filter=ts.data.TOPAZPOND.22[with(ts.data.TOPAZPOND.22, datetime > "2022-08-01 00:00:00" & datetime < "2022-10-07 00:00:00"),]
ts.data.TOK30.22.filter=ts.data.TOK30.22[with(ts.data.TOK30.22, datetime > "2022-08-01 00:00:00" & datetime < "2022-10-07 00:00:00"),]
ts.data.emerald.22=ts.data.emerald.22[with(ts.data.emerald.22, datetime > "2022-08-01 00:00:00" & datetime < "2022-10-30 00:00:00"),]
ts.data.topaz.22=ts.data.topaz.22[with(ts.data.topaz.22, datetime > "2022-08-01 00:00:00" & datetime < "2022-10-30 00:00:00"),]

#############################################################################################################################
######################################### tempdiff ##########################################################################
#############################################################################################################################
#############################################################################################################################

################### 2020 #############################################################################################################################################
#Add a column to all ts.data that calculates the difference between daily max surface vs bottom temp

# Aggregate hourly data to daily mean for tempdiff
eml20_daily_sw <- smoke_eml_20 %>%
  group_by(Date) %>%
  summarize(daily_swrad = mean(swrad, na.rm = TRUE))

topaz20_daily_sw <- smoke_topaz_20 %>%
  group_by(Date) %>%
  summarize(daily_swrad = mean(swrad, na.rm = TRUE))


#EMLPOND1
tempdiff.EMLPOND1.20 <- ts.data.EMLPOND1.20.filter %>%
  group_by(Date = as.Date(datetime)) %>%
  summarize(Max_wtr_1 = max(wtr_0.2, na.rm = TRUE),
            Max_wtr_2 = max(wtr_1.58, na.rm = TRUE),
            tempdiff = Max_wtr_1 - Max_wtr_2)%>%
  filter(!is.infinite(tempdiff))

tempdiff_SW_EMLPOND1 <- ggplot() +
  geom_rect(data = smoke_eml_20,
            aes(xmin = Date, xmax = lead(Date), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(values = c("n" = "white", "y" = "#fdbb84"))+
  geom_area(data = eml20_daily_sw, aes(x = Date, y = daily_swrad/100), color = "darkgray", fill = "white") +
  geom_line(data = tempdiff.EMLPOND1.20, aes(x = Date, y = tempdiff)) +
  scale_y_continuous(
    name = "max daily surface - bottom temp",
    sec.axis = sec_axis(~ .*100, name = "SW Radiation")) +
  labs(x="Date")+
  ggtitle("EMLPOND1 2020")+
  
  theme_minimal()+
  theme(legend.position = "bottom")
tempdiff_SW_EMLPOND1

#run a gam

library(mgcv)
test.EMLPOND1.20=merge(smoke_eml_20, tempdiff.EMLPOND1.20)
test.EMLPOND1.20$Smoke.day=as.factor(test.EMLPOND1.20$Smoke.day)
gam.test.EMLPOND1.20 <- gam(daily_mean_temp ~ s(doy, by=Smoke.day) + Smoke.day,  data = test.EMLPOND1.20)
summary(gam.test.EMLPOND1.20)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.EMLPOND1.20, pages = 1, shade = TRUE, by="Smoke.day")
title("EMLPOND1 2020")
gam.EMLPOND1.20 = recordPlot()



#run a gamm

library(mgcv)
test.EMLPOND1.20=merge(smoke_eml_20, tempdiff.EMLPOND1.20)
test.EMLPOND1.20$Smoke.day=as.factor(test.EMLPOND1.20$Smoke.day)
gamm.test.EMLPOND1.20 <- gamm(tempdiff ~ s(doy) + factor(Smoke.day),  data = test.EMLPOND1.20)
summary(gamm.test.EMLPOND1.20)
summary(gamm.test.EMLPOND1.20$gam)
summary(gamm.test.EMLPOND1.20$lme)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gamm.test.EMLPOND1.20, pages = 1, shade = TRUE, by="Smoke.day")
title("EMLPOND1 2020")
gamm.EMLPOND1.20 = recordPlot()


#TOK11
tempdiff.TOK11.20 <- ts.data.TOK11.20.filter %>%
  group_by(Date = as.Date(datetime)) %>%
  summarize(Max_wtr_1 = max(wtr_0.22, na.rm = TRUE),
            Max_wtr_2 = max(wtr_1.24, na.rm = TRUE),
            tempdiff = Max_wtr_1 - Max_wtr_2)%>%
  filter(!is.infinite(tempdiff))

tempdiff_SW_TOK11 <- ggplot() +
  geom_rect(data = smoke_eml_20,
            aes(xmin = Date, xmax = lead(Date), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(values = c("n" = "white", "y" = "#fdbb84"))+
  geom_area(data = eml20_daily_sw, aes(x = Date, y = daily_swrad/100), color = "darkgray", fill = "white") +
  geom_line(data = tempdiff.TOK11.20, aes(x = Date, y = tempdiff)) +
  scale_y_continuous(
    name = "max daily surface - bottom temp",
    sec.axis = sec_axis(~ .*100, name = "SW Radiation")) +
  labs(x="Date")+
  ggtitle("TOK11 2020")+
  theme_minimal()+
  theme(legend.position = "bottom")
tempdiff_SW_TOK11

#run a gam

library(mgcv)
test.TOK11.20=merge(smoke_eml_20, tempdiff.TOK11.20)
test.TOK11.20$Smoke.day=as.factor(test.TOK11.20$Smoke.day)
gam.test.TOK11.20 <- gam(tempdiff ~ s(doy, by=Smoke.day),  data = test.TOK11.20)
summary(gam.test.TOK11.20)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.TOK11.20, pages = 1, shade = TRUE, by="Smoke.day")
title("TOK11 2020")
gam.TOK11.20 = recordPlot()


#TOPAZPOND
tempdiff.TOPAZPOND.20 <- ts.data.TOPAZPOND.20.filter %>%
  group_by(Date = as.Date(datetime)) %>%
  summarize(Max_wtr_1 = max(wtr_0.08, na.rm = TRUE),
            Max_wtr_2 = max(wtr_0.98, na.rm = TRUE),
            tempdiff = Max_wtr_1 - Max_wtr_2)%>%
  filter(!is.infinite(tempdiff))

tempdiff_SW_TOPAZPOND <- ggplot() +
  geom_rect(data = smoke_topaz_20,
            aes(xmin = Date, xmax = lead(Date), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(values = c("n" = "white", "y" = "#fdbb84"))+
  geom_area(data = topaz20_daily_sw, aes(x = Date, y = daily_swrad/100), color = "darkgray", fill = "white") +
  geom_line(data = tempdiff.TOPAZPOND.20, aes(x = Date, y = tempdiff)) +
  scale_y_continuous(
    name = "max daily surface - bottom temp",
    sec.axis = sec_axis(~ .*100, name = "SW Radiation")) +
  labs(x="Date")+
  ggtitle("TOPAZPOND 2020")+
  theme_minimal()+
  theme(legend.position = "bottom")
tempdiff_SW_TOPAZPOND


library(mgcv)
test.TOPAZPOND.20=merge(smoke_topaz_20, tempdiff.TOPAZPOND.20)
test.TOPAZPOND.20$Smoke.day=as.factor(test.TOPAZPOND.20$Smoke.day)
gam.test.TOPAZPOND.20 <- gam(tempdiff ~ s(doy, by=Smoke.day),  data = test.TOPAZPOND.20)
summary(gam.test.TOPAZPOND.20)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.TOPAZPOND.20, pages = 1, shade = TRUE, by="Smoke.day")
title("TOPAZPOND 2020")
gam.TOPAZPOND.20 = recordPlot()

#TOK30
tempdiff.TOK30.20 <- ts.data.TOK30.20.filter %>%
  group_by(Date = as.Date(datetime)) %>%
  summarize(Max_wtr_1 = max(wtr_0.28, na.rm = TRUE),
            Max_wtr_2 = max(wtr_0.77, na.rm = TRUE),
            tempdiff = Max_wtr_1 - Max_wtr_2)%>%
  filter(!is.infinite(tempdiff))

tempdiff_SW_TOK30 <- ggplot() +
  geom_rect(data = smoke_topaz_20,
            aes(xmin = Date, xmax = lead(Date), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(values = c("n" = "white", "y" = "#fdbb84"))+
  geom_area(data = topaz20_daily_sw, aes(x = Date, y = daily_swrad/100), color = "darkgray", fill = "white") +
  geom_line(data = tempdiff.TOK30.20, aes(x = Date, y = tempdiff)) +
  scale_y_continuous(
    name = "max daily surface - bottom temp",
    sec.axis = sec_axis(~ .*100, name = "SW Radiation")) +
  labs(x="Date")+
  ggtitle("TOK30 2020")+
  theme_minimal()+
  theme(legend.position = "bottom")
tempdiff_SW_TOK30

library(mgcv)
test.TOK30.20=merge(smoke_topaz_20, tempdiff.TOK30.20)
test.TOK30.20$Smoke.day=as.factor(test.TOK30.20$Smoke.day)
gam.test.TOK30.20 <- gam(tempdiff ~ s(doy, by=Smoke.day),  data = test.TOK30.20)
summary(gam.test.TOK30.20)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.TOK30.20, pages = 1, shade = TRUE, by="Smoke.day")
title("TOK30 2020")
gam.TOK30.20 = recordPlot()

#EMERALD LAKE
tempdiff.EML.20 <- emerald20.filter %>%
  group_by(Date = as.Date(datetime)) %>%
  summarize(Max_wtr_1 = max(wtr_3.58, na.rm = TRUE),
            Max_wtr_2 = max(wtr_9.14, na.rm = TRUE),
            tempdiff = Max_wtr_1 - Max_wtr_2)%>%
  filter(!is.infinite(tempdiff))

tempdiff_SW_EML <- ggplot() +
  geom_rect(data = smoke_eml_20,
            aes(xmin = Date, xmax = lead(Date), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(values = c("n" = "white", "y" = "#fdbb84"))+
  geom_area(data = eml20_daily_sw, aes(x = Date, y = daily_swrad/100), color = "darkgray", fill = "white") +
  geom_line(data = tempdiff.EML.20, aes(x = Date, y = tempdiff)) +
  scale_y_continuous(
    name = "max daily surface - bottom temp",
    sec.axis = sec_axis(~ .*100, name = "SW Radiation")) +
  labs(x="Date")+
  ggtitle("Emerald lake 2020")+
  theme_minimal()+
  theme(legend.position = "bottom")
tempdiff_SW_EML


#run a gam

library(mgcv)
test.EML.20=merge(smoke_eml_20, tempdiff.EML.20)
test.EML.20$Smoke.day=as.factor(test.EML.20$Smoke.day)
gam.test.EML.20 <- gam(tempdiff ~ s(doy, by=Smoke.day),  data = test.EML.20)
summary(gam.test.EML.20)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.EML.20, pages = 1, shade = TRUE, by="Smoke.day")
title("EML 2020")
gam.EML.20 = recordPlot()

#TOPAZ LAKE
tempdiff.topaz.20 <- topaz20.filter %>%
  group_by(Date = as.Date(datetime)) %>%
  summarize(Max_wtr_1 = max(wtr_1.91, na.rm = TRUE),
            Max_wtr_2 = max(wtr_2.78, na.rm = TRUE),
            tempdiff = Max_wtr_1 - Max_wtr_2)%>%
  filter(!is.infinite(tempdiff))

tempdiff_SW_topaz <- ggplot() +
  geom_rect(data = smoke_topaz_20,
            aes(xmin = Date, xmax = lead(Date), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(values = c("n" = "white", "y" = "#fdbb84"))+
  geom_area(data = topaz20_daily_sw, aes(x = Date, y = daily_swrad/100), color = "darkgray", fill = "white") +
  geom_line(data = tempdiff.topaz.20, aes(x = Date, y = tempdiff)) +
  scale_y_continuous(
    name = "max daily surface - bottom temp",
    sec.axis = sec_axis(~ .*100, name = "SW Radiation")) +
  labs(x="Date")+
  ggtitle("TOPAZ LAKE 2020")+
  theme_minimal()+
  theme(legend.position = "bottom")
tempdiff_SW_topaz

#run a gam

library(mgcv)
test.topaz.20=merge(smoke_topaz_20, tempdiff.topaz.20)
test.topaz.20$Smoke.day=as.factor(test.topaz.20$Smoke.day)
gam.test.topaz.20 <- gam(tempdiff ~ s(doy, by=Smoke.day),  data = test.topaz.20)
summary(gam.test.topaz.20)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.topaz.20, pages = 1, shade = TRUE, by="Smoke.day")
title("topaz 2020")
gam.topaz.20 = recordPlot()



plot_grid(tempdiff_SW_EML, tempdiff_SW_TOK11, tempdiff_SW_topaz, tempdiff_SW_TOPAZPOND, tempdiff_SW_EMLPOND1,   tempdiff_SW_TOK30, ncol = 2)


par(mfrow= c(6, 1))
gam.EML.20
gam.topaz.20
gam.EMLPOND1.20
gam.TOK11.20
gam.TOPAZPOND.20
gam.TOK30.20


par(mfrow = c(1, 1))

#make boxplots for each site that compare smoke vs. not smoke

#EMLPOND1
smoke.tempdiff.EMLPOND1.20 = merge(tempdiff.EMLPOND1.20, smoke_eml_20, by="Date")
# Perform t-test
t_test_result <- t.test(tempdiff ~ Smoke.day, data = smoke.tempdiff.EMLPOND1.20)


bp.smoke.tempdiff.EMLPOND1.20 = ggplot(smoke.tempdiff.EMLPOND1.20, aes(x = Smoke.day, y = tempdiff)) +
  geom_boxplot() +
  labs(title= "emlpond1",x = "Smoke.day", y = "tempdiff") +
  annotate("text", x = Inf, y = Inf,
           label = paste("p =", format(t_test_result$p.value, scientific = TRUE)),
           hjust = 1, vjust = 1, color = ifelse(t_test_result$p.value < 0.05, "darkorange3", "black"))+
  theme_minimal()
bp.smoke.tempdiff.EMLPOND1.20


#TOK11
smoke.tempdiff.TOK11.20 = merge(tempdiff.TOK11.20, smoke_eml_20, by="Date")
# Perform t-test
t_test_result <- t.test(tempdiff ~ Smoke.day, data = smoke.tempdiff.TOK11.20)


bp.smoke.tempdiff.TOK11.20 = ggplot(smoke.tempdiff.TOK11.20, aes(x = Smoke.day, y = tempdiff)) +
  geom_boxplot() +
  labs(title= "tok11",x = "Smoke.day", y = "tempdiff") +
  annotate("text", x = Inf, y = Inf,
           label = paste("p =", format(t_test_result$p.value, scientific = TRUE)),
           hjust = 1, vjust = 1, color = ifelse(t_test_result$p.value < 0.05, "darkorange3", "black"))+
  theme_minimal()
bp.smoke.tempdiff.TOK11.20

#topazpond
smoke.tempdiff.TOPAZPOND.20 = merge(tempdiff.TOPAZPOND.20, smoke_topaz_20, by="Date")
# Perform t-test
t_test_result <- t.test(tempdiff ~ Smoke.day, data = smoke.tempdiff.TOPAZPOND.20)


bp.smoke.tempdiff.TOPAZPOND.20 = ggplot(smoke.tempdiff.TOPAZPOND.20, aes(x = Smoke.day, y = tempdiff)) +
  geom_boxplot() +
  labs(title= "topaz pond",x = "Smoke.day", y = "tempdiff") +
  annotate("text", x = Inf, y = Inf,
           label = paste("p =", format(t_test_result$p.value, scientific = TRUE)),
           hjust = 1, vjust = 1, color = ifelse(t_test_result$p.value < 0.05, "darkorange3", "black"))+
  theme_minimal()
bp.smoke.tempdiff.TOPAZPOND.20


#tok30
smoke.tempdiff.TOK30.20 = merge(tempdiff.TOK30.20, smoke_topaz_20, by="Date")
# Perform t-test
t_test_result <- t.test(tempdiff ~ Smoke.day, data = smoke.tempdiff.TOK30.20)


bp.smoke.tempdiff.TOK30.20 = ggplot(smoke.tempdiff.TOK30.20, aes(x = Smoke.day, y = tempdiff)) +
  geom_boxplot() +
  labs(title= "tok30",x = "Smoke.day", y = "tempdiff") +
  annotate("text", x = Inf, y = Inf,
           label = paste("p =", format(t_test_result$p.value, scientific = TRUE)),
           hjust = 1, vjust = 1, color = ifelse(t_test_result$p.value < 0.05, "darkorange3", "black"))+
  theme_minimal()
bp.smoke.tempdiff.TOK30.20


#emerald lake
smoke.tempdiff.EML.20 = merge(tempdiff.EML.20, smoke_eml_20, by="Date")
# Perform t-test
t_test_result <- t.test(tempdiff ~ Smoke.day, data = smoke.tempdiff.EML.20)


bp.smoke.tempdiff.EML.20 = ggplot(smoke.tempdiff.EML.20, aes(x = Smoke.day, y = tempdiff)) +
  geom_boxplot() +
  labs(title= "emerald lake",x = "Smoke.day", y = "tempdiff") +
  annotate("text", x = Inf, y = Inf,
           label = paste("p =", format(t_test_result$p.value, scientific = TRUE)),
           hjust = 1, vjust = 1, color = ifelse(t_test_result$p.value < 0.05, "darkorange3", "black"))+
  theme_minimal()
bp.smoke.tempdiff.EML.20

#topaz lake
smoke.tempdiff.topaz.20 = merge(tempdiff.topaz.20, smoke_topaz_20, by="Date")
# Perform t-test
t_test_result <- t.test(tempdiff ~ Smoke.day, data = smoke.tempdiff.topaz.20)


bp.smoke.tempdiff.topaz.20 = ggplot(smoke.tempdiff.topaz.20, aes(x = Smoke.day, y = tempdiff)) +
  geom_boxplot() +
  labs(title= "topaz lake", x = "Smoke.day", y = "tempdiff") +
  annotate("text", x = Inf, y = Inf,
           label = paste("p =", format(t_test_result$p.value, scientific = TRUE)),
           hjust = 1, vjust = 1, color = ifelse(t_test_result$p.value < 0.05, "darkorange3", "black"))+
  theme_minimal()
bp.smoke.tempdiff.topaz.20

plot_grid(tempdiff_SW_EML, bp.smoke.tempdiff.EML.20, tempdiff_SW_TOK11, bp.smoke.tempdiff.TOK11.20, tempdiff_SW_topaz, bp.smoke.tempdiff.topaz.20, tempdiff_SW_TOPAZPOND, bp.smoke.tempdiff.TOPAZPOND.20, tempdiff_SW_EMLPOND1, bp.smoke.tempdiff.EMLPOND1.20,  tempdiff_SW_TOK30, bp.smoke.tempdiff.TOK30.20 ,ncol = 4)





########################### 2021 #####################################################################################################################################
#Add a column to all ts.data that calculates the difference between daily max surface vs bottom temp

# Aggregate hourly data to daily mean for tempdiff
eml21_daily_sw <- smoke_eml_21 %>%
  group_by(Date) %>%
  summarize(daily_swrad = mean(swrad, na.rm = TRUE))

topaz21_daily_sw <- smoke_topaz_21 %>%
  group_by(Date) %>%
  summarize(daily_swrad = mean(swrad, na.rm = TRUE))


#EMLPOND1
tempdiff.EMLPOND1.21 <- ts.data.EMLPOND1.21.filter %>%
  group_by(Date = as.Date(datetime)) %>%
  summarize(Max_wtr_1 = max(wtr_0.1, na.rm = TRUE),
            Max_wtr_2 = max(wtr_1.58, na.rm = TRUE),
            tempdiff = Max_wtr_1 - Max_wtr_2)%>%
  filter(!is.infinite(tempdiff))

tempdiff_SW_EMLPOND1 <- ggplot() +
  geom_rect(data = smoke_eml_21,
            aes(xmin = Date, xmax = lead(Date), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(values = c("n" = "white", "y" = "#fdbb84"))+
  geom_area(data = eml21_daily_sw, aes(x = Date, y = daily_swrad/100), color = "darkgray", fill = "white") +
  geom_line(data = tempdiff.EMLPOND1.21, aes(x = Date, y = tempdiff)) +
  scale_y_continuous(
    name = "max daily surface - bottom temp",
    sec.axis = sec_axis(~ .*100, name = "SW Radiation")) +
  labs(x="Date")+
  ggtitle("EMLPOND1 2021")+
  
  theme_minimal()+
  theme(legend.position = "bottom")
tempdiff_SW_EMLPOND1

#run a gam

library(mgcv)
test.EMLPOND1.21=merge(smoke_eml_21, tempdiff.EMLPOND1.21)
test.EMLPOND1.21$Smoke.day=as.factor(test.EMLPOND1.21$Smoke.day)
gam.test.EMLPOND1.21 <- gam(tempdiff ~ s(doy, by=Smoke.day),  data = test.EMLPOND1.21)
summary(gam.test.EMLPOND1.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.EMLPOND1.21, pages = 1, shade = TRUE, by="Smoke.day")
title("EMLPOND1 2021")
gam.EMLPOND1.21 = recordPlot()


#TOK11
tempdiff.TOK11.21 <- ts.data.TOK11.21.filter %>%
  group_by(Date = as.Date(datetime)) %>%
  summarize(Max_wtr_1 = max(wtr_0.22, na.rm = TRUE),
            Max_wtr_2 = max(wtr_1.24, na.rm = TRUE),
            tempdiff = Max_wtr_1 - Max_wtr_2)%>%
  filter(!is.infinite(tempdiff))


tempdiff_SW_TOK11 <- ggplot() +
  geom_rect(data = smoke_eml_21,
            aes(xmin = Date, xmax = lead(Date), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(values = c("n" = "white", "y" = "#fdbb84"))+
  geom_area(data = eml21_daily_sw, aes(x = Date, y = daily_swrad/100), color = "darkgray", fill = "white") +
  geom_line(data = tempdiff.TOK11.21, aes(x = Date, y = tempdiff)) +
  scale_y_continuous(
    name = "max daily surface - bottom temp",
    sec.axis = sec_axis(~ .*100, name = "SW Radiation")) +
  labs(x="Date")+
  ggtitle("TOK11 2021")+
  theme_minimal()+
  theme(legend.position = "bottom")
tempdiff_SW_TOK11

#run a gam

library(mgcv)
test.TOK11.21=merge(smoke_eml_21, tempdiff.TOK11.21)
test.TOK11.21$Smoke.day=as.factor(test.TOK11.21$Smoke.day)
gam.test.TOK11.21 <- gam(tempdiff ~ s(doy, by=Smoke.day),  data = test.TOK11.21)
summary(gam.test.TOK11.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.TOK11.21, pages = 1, shade = TRUE, by="Smoke.day")
title("TOK11 2021")
gam.TOK11.21 = recordPlot()


#TOPAZPOND
tempdiff.TOPAZPOND.21 <- ts.data.TOPAZPOND.21.filter %>%
  group_by(Date = as.Date(datetime)) %>%
  summarize(Max_wtr_1 = max(wtr_0.8, na.rm = TRUE),
            Max_wtr_2 = max(wtr_0.98, na.rm = TRUE),
            tempdiff = Max_wtr_1 - Max_wtr_2)%>%
  filter(!is.infinite(tempdiff))

tempdiff_SW_TOPAZPOND <- ggplot() +
  geom_rect(data = smoke_topaz_21,
            aes(xmin = Date, xmax = lead(Date), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(values = c("n" = "white", "y" = "#fdbb84"))+
  geom_area(data = topaz21_daily_sw, aes(x = Date, y = daily_swrad/100), color = "darkgray", fill = "white") +
  geom_line(data = tempdiff.TOPAZPOND.21, aes(x = Date, y = tempdiff)) +
  scale_y_continuous(
    name = "max daily surface - bottom temp",
    sec.axis = sec_axis(~ .*100, name = "SW Radiation")) +
  labs(x="Date")+
  ggtitle("TOPAZPOND 2021")+
  theme_minimal()+
  theme(legend.position = "bottom")
tempdiff_SW_TOPAZPOND


library(mgcv)
test.TOPAZPOND.21=merge(smoke_topaz_21, tempdiff.TOPAZPOND.21)
test.TOPAZPOND.21$Smoke.day=as.factor(test.TOPAZPOND.21$Smoke.day)
gam.test.TOPAZPOND.21 <- gam(tempdiff ~ s(doy, by=Smoke.day),  data = test.TOPAZPOND.21)
summary(gam.test.TOPAZPOND.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.TOPAZPOND.21, pages = 1, shade = TRUE, by="Smoke.day")
title("TOPAZPOND 2021")
gam.TOPAZPOND.21 = recordPlot()

#TOK30
tempdiff.TOK30.21 <- ts.data.TOK30.21.filter %>%
  group_by(Date = as.Date(datetime)) %>%
  summarize(Max_wtr_1 = max(wtr_0.28, na.rm = TRUE),
            Max_wtr_2 = max(wtr_0.77, na.rm = TRUE),
            tempdiff = Max_wtr_1 - Max_wtr_2)%>%
  filter(!is.infinite(tempdiff))

tempdiff_SW_TOK30 <- ggplot() +
  geom_rect(data = smoke_topaz_21,
            aes(xmin = Date, xmax = lead(Date), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(values = c("n" = "white", "y" = "#fdbb84"))+
  geom_area(data = topaz21_daily_sw, aes(x = Date, y = daily_swrad/100), color = "darkgray", fill = "white") +
  geom_line(data = tempdiff.TOK30.21, aes(x = Date, y = tempdiff)) +
  scale_y_continuous(
    name = "max daily surface - bottom temp",
    sec.axis = sec_axis(~ .*100, name = "SW Radiation")) +
  labs(x="Date")+
  ggtitle("TOK30 2021")+
  theme_minimal()+
  theme(legend.position = "bottom")
tempdiff_SW_TOK30

library(mgcv)
test.TOK30.21=merge(smoke_topaz_21, tempdiff.TOK30.21)
test.TOK30.21$Smoke.day=as.factor(test.TOK30.21$Smoke.day)
gam.test.TOK30.21 <- gam(tempdiff ~ s(doy, by=Smoke.day),  data = test.TOK30.21)
summary(gam.test.TOK30.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.TOK30.21, pages = 1, shade = TRUE, by="Smoke.day")
title("TOK30 2021")
gam.TOK30.21 = recordPlot()

#EMERALD LAKE
tempdiff.EML.21 <- emerald21.filter %>%
  group_by(Date = as.Date(datetime)) %>%
  summarize(Max_wtr_1 = max(wtr_3.58, na.rm = TRUE),
            Max_wtr_2 = max(wtr_9.14, na.rm = TRUE),
            tempdiff = Max_wtr_1 - Max_wtr_2)%>%
  filter(!is.infinite(tempdiff))

tempdiff_SW_EML <- ggplot() +
  geom_rect(data = smoke_eml_21,
            aes(xmin = Date, xmax = lead(Date), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(values = c("n" = "white", "y" = "#fdbb84"))+
  geom_area(data = eml21_daily_sw, aes(x = Date, y = daily_swrad/100), color = "darkgray", fill = "white") +
  geom_line(data = tempdiff.EML.21, aes(x = Date, y = tempdiff)) +
  scale_y_continuous(
    name = "max daily surface - bottom temp",
    sec.axis = sec_axis(~ .*100, name = "SW Radiation")) +
  labs(x="Date")+
  ggtitle("Emerald lake 2021")+
  theme_minimal()+
  theme(legend.position = "bottom")
tempdiff_SW_EML


#run a gam

library(mgcv)
test.EML.21=merge(smoke_eml_21, tempdiff.EML.21)
test.EML.21$Smoke.day=as.factor(test.EML.21$Smoke.day)
gam.test.EML.21 <- gam(tempdiff ~ s(doy, by=Smoke.day),  data = test.EML.21)
summary(gam.test.EML.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.EML.21, pages = 1, shade = TRUE, by="Smoke.day")
title("EML 2021")
gam.EML.21 = recordPlot()

#TOPAZ LAKE
tempdiff.topaz.21 <- topaz21.filter %>%
  group_by(Date = as.Date(datetime)) %>%
  summarize(Max_wtr_1 = max(wtr_2.55, na.rm = TRUE),
            Max_wtr_2 = max(wtr_2.82, na.rm = TRUE),
            tempdiff = Max_wtr_1 - Max_wtr_2)%>%
  filter(!is.infinite(tempdiff))

tempdiff_SW_topaz <- ggplot() +
  geom_rect(data = smoke_topaz_21,
            aes(xmin = Date, xmax = lead(Date), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(values = c("n" = "white", "y" = "#fdbb84"))+
  geom_area(data = topaz21_daily_sw, aes(x = Date, y = daily_swrad/100), color = "darkgray", fill = "white") +
  geom_line(data = tempdiff.topaz.21, aes(x = Date, y = tempdiff)) +
  scale_y_continuous(
    name = "max daily surface - bottom temp",
    sec.axis = sec_axis(~ .*100, name = "SW Radiation")) +
  labs(x="Date")+
  ggtitle("TOPAZ LAKE 2021")+
  theme_minimal()+
  theme(legend.position = "bottom")
tempdiff_SW_topaz

#run a gam

library(mgcv)
test.topaz.21=merge(smoke_topaz_21, tempdiff.topaz.21)
test.topaz.21$Smoke.day=as.factor(test.topaz.21$Smoke.day)
gam.test.topaz.21 <- gam(tempdiff ~ s(doy, by=Smoke.day),  data = test.topaz.21)
summary(gam.test.topaz.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.topaz.21, pages = 1, shade = TRUE, by="Smoke.day")
title("topaz 2021")
gam.topaz.21 = recordPlot()



plot_grid(tempdiff_SW_EML, tempdiff_SW_TOK11, tempdiff_SW_topaz, tempdiff_SW_TOPAZPOND, tempdiff_SW_EMLPOND1,   tempdiff_SW_TOK30, ncol = 2)


par(mfrow= c(6, 1))
gam.EML.21
gam.topaz.21
gam.EMLPOND1.21
gam.TOK11.21
gam.TOPAZPOND.21
gam.TOK30.21


par(mfrow = c(1, 1))

#make boxplots for each site that compare smoke vs. not smoke

#EMLPOND1
smoke.tempdiff.EMLPOND1.21 = merge(tempdiff.EMLPOND1.21, smoke_eml_21, by="Date")
# Perform t-test
t_test_result <- t.test(tempdiff ~ Smoke.day, data = smoke.tempdiff.EMLPOND1.21)


bp.smoke.tempdiff.EMLPOND1.21 = ggplot(smoke.tempdiff.EMLPOND1.21, aes(x = Smoke.day, y = tempdiff)) +
  geom_boxplot() +
  labs(title= "emlpond1",x = "Smoke.day", y = "tempdiff") +
  annotate("text", x = Inf, y = Inf,
           label = paste("p =", format(t_test_result$p.value, scientific = TRUE)),
           hjust = 1, vjust = 1, color = ifelse(t_test_result$p.value < 0.05, "darkorange3", "black"))+
  theme_minimal()
bp.smoke.tempdiff.EMLPOND1.21


#TOK11
smoke.tempdiff.TOK11.21 = merge(tempdiff.TOK11.21, smoke_eml_21, by="Date")
# Perform t-test
t_test_result <- t.test(tempdiff ~ Smoke.day, data = smoke.tempdiff.TOK11.21)


bp.smoke.tempdiff.TOK11.21 = ggplot(smoke.tempdiff.TOK11.21, aes(x = Smoke.day, y = tempdiff)) +
  geom_boxplot() +
  labs(title= "tok11",x = "Smoke.day", y = "tempdiff") +
  annotate("text", x = Inf, y = Inf,
           label = paste("p =", format(t_test_result$p.value, scientific = TRUE)),
           hjust = 1, vjust = 1, color = ifelse(t_test_result$p.value < 0.05, "darkorange3", "black"))+
  theme_minimal()
bp.smoke.tempdiff.TOK11.21

#topazpond
smoke.tempdiff.TOPAZPOND.21 = merge(tempdiff.TOPAZPOND.21, smoke_topaz_21, by="Date")
# Perform t-test
t_test_result <- t.test(tempdiff ~ Smoke.day, data = smoke.tempdiff.TOPAZPOND.21)


bp.smoke.tempdiff.TOPAZPOND.21 = ggplot(smoke.tempdiff.TOPAZPOND.21, aes(x = Smoke.day, y = tempdiff)) +
  geom_boxplot() +
  labs(title= "topaz pond",x = "Smoke.day", y = "tempdiff") +
  annotate("text", x = Inf, y = Inf,
           label = paste("p =", format(t_test_result$p.value, scientific = TRUE)),
           hjust = 1, vjust = 1, color = ifelse(t_test_result$p.value < 0.05, "darkorange3", "black"))+
  theme_minimal()
bp.smoke.tempdiff.TOPAZPOND.21


#tok30
smoke.tempdiff.TOK30.21 = merge(tempdiff.TOK30.21, smoke_topaz_21, by="Date")
# Perform t-test
t_test_result <- t.test(tempdiff ~ Smoke.day, data = smoke.tempdiff.TOK30.21)


bp.smoke.tempdiff.TOK30.21 = ggplot(smoke.tempdiff.TOK30.21, aes(x = Smoke.day, y = tempdiff)) +
  geom_boxplot() +
  labs(title= "tok30",x = "Smoke.day", y = "tempdiff") +
  annotate("text", x = Inf, y = Inf,
           label = paste("p =", format(t_test_result$p.value, scientific = TRUE)),
           hjust = 1, vjust = 1, color = ifelse(t_test_result$p.value < 0.05, "darkorange3", "black"))+
  theme_minimal()
bp.smoke.tempdiff.TOK30.21


#emerald lake
smoke.tempdiff.EML.21 = merge(tempdiff.EML.21, smoke_eml_21, by="Date")
# Perform t-test
t_test_result <- t.test(tempdiff ~ Smoke.day, data = smoke.tempdiff.EML.21)


bp.smoke.tempdiff.EML.21 = ggplot(smoke.tempdiff.EML.21, aes(x = Smoke.day, y = tempdiff)) +
  geom_boxplot() +
  labs(title= "emerald lake",x = "Smoke.day", y = "tempdiff") +
  annotate("text", x = Inf, y = Inf,
           label = paste("p =", format(t_test_result$p.value, scientific = TRUE)),
           hjust = 1, vjust = 1, color = ifelse(t_test_result$p.value < 0.05, "darkorange3", "black"))+
  theme_minimal()
bp.smoke.tempdiff.EML.21

#topaz lake
smoke.tempdiff.topaz.21 = merge(tempdiff.topaz.21, smoke_topaz_21, by="Date")
# Perform t-test
t_test_result <- t.test(tempdiff ~ Smoke.day, data = smoke.tempdiff.topaz.21)


bp.smoke.tempdiff.topaz.21 = ggplot(smoke.tempdiff.topaz.21, aes(x = Smoke.day, y = tempdiff)) +
  geom_boxplot() +
  labs(title= "topaz lake", x = "Smoke.day", y = "tempdiff") +
  annotate("text", x = Inf, y = Inf,
           label = paste("p =", format(t_test_result$p.value, scientific = TRUE)),
           hjust = 1, vjust = 1, color = ifelse(t_test_result$p.value < 0.05, "darkorange3", "black"))+
  theme_minimal()
bp.smoke.tempdiff.topaz.21

plot_grid(tempdiff_SW_EML, bp.smoke.tempdiff.EML.21, tempdiff_SW_TOK11, bp.smoke.tempdiff.TOK11.21, tempdiff_SW_topaz, bp.smoke.tempdiff.topaz.21, tempdiff_SW_TOPAZPOND, bp.smoke.tempdiff.TOPAZPOND.21, tempdiff_SW_EMLPOND1, bp.smoke.tempdiff.EMLPOND1.21,  tempdiff_SW_TOK30, bp.smoke.tempdiff.TOK30.21 ,ncol = 4)





########################### 2022 #####################################################################################################################################
#Add a column to all ts.data that calculates the difference between daily max surface vs bottom temp

eml22_daily_sw=avg.sw.e.22
eml22_daily_sw$Date=as.Date(eml22_daily_sw$Date)
eml22_daily_sw=eml22_daily_sw[with(eml22_daily_sw, Date > "2022-08-01"& Date < "2022-10-10" ),]


topaz22_daily_sw=avg.sw.t.22
topaz22_daily_sw$Date=as.Date(topaz22_daily_sw$Date)
topaz22_daily_sw=topaz22_daily_sw[with(topaz22_daily_sw, Date > "2022-08-01"& Date < "2022-10-10" ),]


#EMLPOND1
tempdiff.EMLPOND1.22 <- ts.data.EMLPOND1.22.filter %>%
  group_by(Date = as.Date(datetime)) %>%
  summarize(Max_wtr_2 = max(wtr_2.8, na.rm = TRUE),
            Max_wtr_1 = max(wtr_1.58, na.rm = TRUE),
            tempdiff = Max_wtr_1 - Max_wtr_2)%>%
  filter(!is.infinite(tempdiff))

tempdiff_SW_EMLPOND1 <- ggplot() +
  geom_area(data = eml22_daily_sw, aes(x = Date, y = swrad/100), color = "darkgray", fill = "white") +
  geom_line(data = tempdiff.EMLPOND1.22, aes(x = Date, y = tempdiff)) +
  scale_y_continuous(
    name = "max daily surface - bottom temp",
    sec.axis = sec_axis(~ .*100, name = "SW Radiation")) +
  labs(x="Date")+
  ggtitle("EMLPOND1 2022")+
  
  theme_minimal()+
  theme(legend.position = "bottom")
tempdiff_SW_EMLPOND1




#TOK11
tempdiff.TOK11.22 <- ts.data.TOK11.22.filter %>%
  group_by(Date = as.Date(datetime)) %>%
  summarize(Max_wtr_2 = max(wtr_2, na.rm = TRUE),
            Max_wtr_1 = max(wtr_1.24, na.rm = TRUE),
            tempdiff = Max_wtr_1 - Max_wtr_2)%>%
  filter(!is.infinite(tempdiff))


tempdiff_SW_TOK11 <- ggplot() +
  geom_area(data = eml22_daily_sw, aes(x = Date, y = swrad/100), color = "darkgray", fill = "white") +
  geom_line(data = tempdiff.TOK11.22, aes(x = Date, y = tempdiff)) +
  scale_y_continuous(
    name = "max daily surface - bottom temp",
    sec.axis = sec_axis(~ .*100, name = "SW Radiation")) +
  labs(x="Date")+
  ggtitle("TOK11 2022")+
  theme_minimal()+
  theme(legend.position = "bottom")
tempdiff_SW_TOK11




#TOPAZPOND
tempdiff.TOPAZPOND.22 <- ts.data.TOPAZPOND.22.filter %>%
  group_by(Date = as.Date(datetime)) %>%
  summarize(Max_wtr_2 = max(wtr_1.66, na.rm = TRUE),
            Max_wtr_1 = max(wtr_0.98, na.rm = TRUE),
            tempdiff = Max_wtr_1 - Max_wtr_2)%>%
  filter(!is.infinite(tempdiff))

tempdiff_SW_TOPAZPOND <- ggplot() +
  geom_area(data = topaz22_daily_sw, aes(x = Date, y = swrad/100), color = "darkgray", fill = "white") +
  geom_line(data = tempdiff.TOPAZPOND.22, aes(x = Date, y = tempdiff)) +
  scale_y_continuous(
    name = "max daily surface - bottom temp",
    sec.axis = sec_axis(~ .*100, name = "SW Radiation")) +
  labs(x="Date")+
  ggtitle("TOPAZPOND 2022")+
  theme_minimal()+
  theme(legend.position = "bottom")
tempdiff_SW_TOPAZPOND




#TOK30
tempdiff.TOK30.22 <- ts.data.TOK30.22.filter %>%
  group_by(Date = as.Date(datetime)) %>%
  summarize(Max_wtr_2 = max(wtr_1.34, na.rm = TRUE),
            Max_wtr_1 = max(wtr_0.77, na.rm = TRUE),
            tempdiff = Max_wtr_1 - Max_wtr_2)%>%
  filter(!is.infinite(tempdiff))

tempdiff_SW_TOK30 <- ggplot() +
  geom_area(data = topaz22_daily_sw, aes(x = Date, y = swrad/100), color = "darkgray", fill = "white") +
  geom_line(data = tempdiff.TOK30.22, aes(x = Date, y = tempdiff)) +
  scale_y_continuous(
    name = "max daily surface - bottom temp",
    sec.axis = sec_axis(~ .*100, name = "SW Radiation")) +
  labs(x="Date")+
  ggtitle("TOK30 2022")+
  theme_minimal()+
  theme(legend.position = "bottom")
tempdiff_SW_TOK30


#EMERALD LAKE
emerald22.filter=ts.data.emerald.22

tempdiff.EML.22 <- emerald22.filter %>%
  group_by(Date = as.Date(datetime)) %>%
  summarize(Max_wtr_1 = max(wtr_3.8, na.rm = TRUE),
            Max_wtr_2 = max(wtr_9.16, na.rm = TRUE),
            tempdiff = Max_wtr_1 - Max_wtr_2)%>%
  filter(!is.infinite(tempdiff))

avg.sw.e.22.date=avg.sw.e.22
avg.sw.e.22.date$Date=as.Date(avg.sw.e.22.date$Date)
avg.sw.e.22.date=avg.sw.e.22.date[with(avg.sw.e.22.date, Date > "2022-04-01"& Date < "2022-07-20" ),]


tempdiff_SW_EML <- ggplot() +
  geom_area(data = avg.sw.e.22.date, aes(x = Date, y = swrad/100), color = "darkgray", fill = "white") +
  geom_line(data = tempdiff.EML.22, aes(x = Date, y = tempdiff)) +
  scale_y_continuous(
    name = "max daily surface - bottom temp",
    sec.axis = sec_axis(~ .*100, name = "SW Radiation")) +
  labs(x="Date")+
  ggtitle("Emerald lake 2022")+
  theme_minimal()+
  theme(legend.position = "bottom")
tempdiff_SW_EML




#TOPAZ LAKE
topaz22.filter=ts.data.topaz.22

avg.sw.t.22.date=avg.sw.t.22
avg.sw.t.22.date$Date=as.Date(avg.sw.t.22.date$Date)
avg.sw.t.22.date=avg.sw.t.22.date[with(avg.sw.t.22.date, Date > "2022-04-01"& Date < "2022-07-20" ),]

tempdiff.topaz.22 <- topaz22.filter %>%
  group_by(Date = as.Date(datetime)) %>%
  summarize(Max_wtr_1 = max(wtr_2.55, na.rm = TRUE),
            Max_wtr_2 = max(wtr_2.82, na.rm = TRUE),
            tempdiff = Max_wtr_1 - Max_wtr_2)%>%
  filter(!is.infinite(tempdiff))

tempdiff_SW_topaz <- ggplot() +
  geom_area(data = avg.sw.t.22.date, aes(x = Date, y = swrad/100), color = "darkgray", fill = "white") +
  geom_line(data = tempdiff.topaz.22, aes(x = Date, y = tempdiff)) +
  scale_y_continuous(
    name = "max daily surface - bottom temp",
    sec.axis = sec_axis(~ .*100, name = "SW Radiation")) +
  labs(x="Date")+
  ggtitle("TOPAZ LAKE 2022")+
  theme_minimal()+
  theme(legend.position = "bottom")
tempdiff_SW_topaz



plot_grid(tempdiff_SW_EML, tempdiff_SW_TOK11, tempdiff_SW_topaz, tempdiff_SW_TOPAZPOND, tempdiff_SW_EMLPOND1,   tempdiff_SW_TOK30, ncol = 2)


