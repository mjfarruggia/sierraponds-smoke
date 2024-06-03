#this script is for plotting tokopah met data framed around smoke/smoke periods

#temp time series
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
################################################################################################################################################################
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
    Rain_mm > 5 ~ ">5mm",
    Rain_mm > 0 & Rain_mm <5 ~ "<5mm",
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
weather.data.e.20=weather.data.e[with(weather.data.e, Datetime > "2020-07-02 00:00:00" & Datetime < "2020-10-05 00:00:00"),]
weather.data.e.21=weather.data.e[with(weather.data.e, Datetime > "2021-07-02 00:00:00" & Datetime < "2021-10-05 00:00:00"),]

weather.data.e.22=read.csv("Emerald_MET_data_2019_2023_hourly.csv",header=TRUE)
weather.data.e.22$Date = as.Date(weather.data.e.22$Datetime , format='%Y-%m-%d')
weather.data.e.22$DOY = yday(weather.data.e.22$Date)
weather.data.e.22$Datetime=as.POSIXct(weather.data.e.22$Datetime, format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
weather.data.e.22$SW_solar=weather.data.e.22$SW_solar * -1
weather.data.e.22=weather.data.e.22[with(weather.data.e.22, Date > "2022-07-02" & Date < "2022-10-05"),]
weather.data.e.22 <- weather.data.e.22 %>%
  mutate(RainIntensity = case_when(
    Rain_mm > 5 ~ ">5mm",
    Rain_mm > 0 & Rain_mm <5 ~ "<5mm",
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
weather.data.t.20=weather.data.t[with(weather.data.t, Datetime > "2020-07-02 00:00:00" & Datetime < "2020-10-05 00:00:00"),]
weather.data.t.21=weather.data.t[with(weather.data.t, Datetime > "2021-07-02 00:00:00" & Datetime < "2021-10-05 00:00:00"),]

weather.data.t.22=read.csv("Topaz_MET_data_2019_2023_hourly.csv",header=TRUE)
weather.data.t.22$Date = as.Date(weather.data.t.22$Datetime , format='%m/%d/%Y')
weather.data.t.22$DOY = yday(weather.data.t.22$Date)
weather.data.t.22$Datetime=as.POSIXct(weather.data.t.22$Datetime, format='%m/%d/%Y %H:%M', tz="Etc/GMT-8")
weather.data.t.22$SW_solar=weather.data.t.22$SW_solar * -1
weather.data.t.22=weather.data.t.22[with(weather.data.t.22, Date > "2022-07-02" & Date < "2022-10-05"),]
weather.data.t.22 <- weather.data.t.22 %>%
  mutate(RainIntensity = case_when(
    Rain_mm > 5 ~ ">5mm",
    Rain_mm > 0 & Rain_mm <5 ~ "<5mm",
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
#load in pond/lake info
################################################################################################################################################################

##set working directory, load data, select desired year and lake to analyze
#load lake data
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd) 
files <- list.files()#all the files in wd
load('Emerald_Topaz_sensor_data_2020_forMJ.Rdata')
emerald20= ts.2020.forMJ  [[1]]
topaz20= ts.2020.forMJ  [[2]]

#EMERALD Create a sequence of times (24 hours)
times <- format(seq(from = as.POSIXct("2020-07-15 00:00:00"), 
                    by = "hour", length.out = 24), "%H:%M:%S")
# Combine dates and times
emerald20$datetime <- as.POSIXct(paste(emerald20$datetime, times), format = "%Y-%m-%d %H:%M:%S")


# TOPAZ Create a sequence of times (24 hours)
topaz20=topaz20[with(topaz20, datetime > "2020-08-13"),]
times <- format(seq(from = as.POSIXct("2020-08-13 00:00:00"), 
                    by = "hour", length.out = 24), "%H:%M:%S")
# Combine dates and times
topaz20$datetime <- as.POSIXct(paste(topaz20$datetime, times), format = "%Y-%m-%d %H:%M:%S")


load('Emerald_Topaz_sensor_data_2021_forMJ.Rdata')
emerald21= ts.2021.forMJ  [[1]]
topaz21= ts.2021.forMJ  [[2]]

# EMERALD Create a sequence of times (24 hours)
times <- format(seq(from = as.POSIXct("2021-05-12 00:00:00"), 
                    by = "hour", length.out = 24), "%H:%M:%S")
# Combine dates and times
emerald21$datetime <- as.POSIXct(paste(emerald21$datetime, times), format = "%Y-%m-%d %H:%M:%S")


# TOPAZ Create a sequence of times (24 hours)
topaz21=topaz21[with(topaz21, datetime > "2021-08-21"),]
times <- format(seq(from = as.POSIXct("2021-08-21 00:00:00"), 
                    by = "hour", length.out = 24), "%H:%M:%S")
# Combine dates and times
topaz21$datetime <- as.POSIXct(paste(topaz21$datetime, times), format = "%Y-%m-%d %H:%M:%S")



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
year <- "2022" #desired open water period, including season

#choose lake
lake <- 'Emerald' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
# bookkeep.emerald.22 <- outputs[[1]]
# kalman.emerald.22 <- outputs[[2]]
# bayesian.emerald.22 <- outputs[[3]]
ts.data.emerald.22.early <- outputs[[4]]
# DO_depth.emerald.22 <- outputs[[5]]
colnames(ts.data.emerald.22.early)[colnames(ts.data.emerald.22.early) == "wtr_3.43"] <- "wtr_3.47"
colnames(ts.data.emerald.22.early)[colnames(ts.data.emerald.22.early) == "wtr_9.96"] <- "wtr_9.95"
colnames(ts.data.emerald.22.early)[colnames(ts.data.emerald.22.early) == "wtr_7.5"] <- "wtr_7.49"
ts.data.emerald.22 <- merge(ts.data.emerald.22.early, ts.data.emerald.22, all=T)



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

#Choose year 
year <- "2022" #desired open water period, including season

#choose lake
lake <- 'Topaz' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
ts.data.topaz.22.early <- outputs[[4]]

colnames(ts.data.topaz.22.early)[colnames(ts.data.topaz.22.early) == "wtr_2.55"] <- "wtr_3.19"
colnames(ts.data.topaz.22.early)[colnames(ts.data.topaz.22.early) == "wtr_2.82"] <- "wtr_3.38"
colnames(ts.data.topaz.22.early)[colnames(ts.data.topaz.22.early) == "wtr_3.37"] <- "wtr_3.94"
ts.data.topaz.22 <- merge(ts.data.topaz.22.early, ts.data.topaz.22, all=T)

################################################################################################################################################################


##################################water temp plots with sw in background, for each site###################################################
###########################2020 plots with smoke day y/n #############################################################################################

#add smoke rectangles based on smoke day y/n
smoke_eml_20=smoke_eml_20[with(smoke_eml_20, Date> "2020-07-02 00:00:00" & Date < "2020-10-05 00:00:00"),]
smoke_topaz_20=smoke_topaz_20[with(smoke_topaz_20, Date> "2020-07-02 00:00:00" & Date < "2020-10-05 00:00:00"),]

avg.sw.e.20=avg.sw.e.20[with(avg.sw.e.20, Date> "2020-07-02 00:00:00" & Date < "2020-10-05 00:00:00"),]


#EMLPOND1 
ts.data.EMLPOND1.20.filter=ts.data.EMLPOND1.20[with(ts.data.EMLPOND1.20, datetime > "2020-07-01 00:00:00" & datetime < "2020-10-05 00:00:00" ),]

WTEMP.EMLPOND1.20.smoke=ggplot()+
  geom_rect(data = smoke_eml_20,
            aes(xmin = Datetime, xmax = lead(Datetime), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "white","y" = "#fdbb84"), labels = c("No", "Yes"))+
  geom_area(data = avg.sw.e.20, aes(x = Date, y = (swrad-50)/10), color = "dimgray", fill = "white") +
  geom_line(data=ts.data.EMLPOND1.20.filter, aes(x=datetime, y = wtr_1.58), na.rm=T,colour="blue")+
  geom_line(data=ts.data.EMLPOND1.20.filter, aes(x=datetime, y = wtr_0.2), colour="dodgerblue", na.rm=T)+
  facet_wrap(~"EMLPOND1", ncol=2) +
  
  scale_y_continuous(
   name = " ",
    sec.axis = sec_axis(~ (.*10)+50 , name = " "),breaks = seq(0, 30, by = 5),
   )+
  
  coord_cartesian(ylim=c(3,27))+
  geom_point(data = avg.sw.e.20[!is.na(avg.sw.e.20$RainIntensity), ], 
             aes(x = Date, y = max(airt) +7, color = RainIntensity), 
             size = 3, shape = 16) +  
  scale_color_manual(values = c("<5mm" = "#cbc9e2",  ">5mm" = "#6a51a3"),
                     breaks = c("<5mm", ">5mm"),
                     labels = c("<5mm", ">5mm"),
                     limits = c("<5mm", ">5mm"),  # Include all levels in the legend
                     drop=FALSE) +   
  labs(x="Date", color="Rain Intensity")+
  theme_classic( )+
  theme(panel.background = element_rect(fill='transparent'), axis.title.y.left = element_text(color = "blue"))

WTEMP.EMLPOND1.20.smoke



#TOK11
ts.data.TOK11.20.filter=ts.data.TOK11.20[with(ts.data.TOK11.20, datetime > "2020-07-02 00:00:00" & datetime < "2020-10-05 00:00:00" ),]

WTEMP.TOK11.20.smoke=ggplot()+
  geom_rect(data = smoke_eml_20,
            aes(xmin = Datetime, xmax = lead(Datetime), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "white","y" = "#fdbb84"), labels = c("No", "Yes"))+
  geom_area(data = avg.sw.e.20, aes(x = Date, y = (swrad-50)/10), color = "dimgray", fill = "white") +
  geom_line(data=ts.data.TOK11.20.filter, aes(x=datetime, y = wtr_1.24), colour="dodgerblue")+
  geom_line(data=ts.data.TOK11.20.filter, aes(x=datetime, y = wtr_0.22), colour="lightseagreen")+
  facet_wrap(~"TOK11", ncol=2) +
  
  scale_y_continuous(
    #  name = "Water Temp °C",
    name = " ",
    sec.axis = sec_axis(~ (.*10)+50, name = " "),breaks = seq(0, 30, by = 5)) +
  coord_cartesian(ylim=c(3, 27))+
  
  geom_point(data = avg.sw.e.20[!is.na(avg.sw.e.20$RainIntensity), ], 
             aes(x = Date, y = max(airt) +7, color = RainIntensity), 
             size = 3, shape = 16) +  
  scale_color_manual(values = c("<5mm" = "#cbc9e2",  ">5mm" = "#6a51a3"),
                     breaks = c("<5mm", ">5mm"),
                     labels = c("<5mm", ">5mm"),
                     limits = c("<5mm", ">5mm"),  # Include all levels in the legend
                     drop=FALSE) +   
  labs(x="Date", color="Rain Intensity")+
  theme_classic()+theme(axis.line = element_line(color='black'),
                        plot.background = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.border = element_blank())
WTEMP.TOK11.20.smoke


#TOPAZPOND
ts.data.TOPAZPOND.20.filter=ts.data.TOPAZPOND.20[with(ts.data.TOPAZPOND.20, datetime > "2020-07-02 00:00:00" & datetime < "2020-10-05 00:00:00" ),]

WTEMP.TOPAZPOND.20.smoke=ggplot()+
  geom_rect(data = smoke_topaz_20,
            aes(xmin = Datetime, xmax = lead(Datetime), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "white","y" = "#fdbb84"), labels = c("No", "Yes"))+
  geom_area(data = avg.sw.t.20, aes(x = Date, y = (swrad-50)/10), color = "dimgray", fill = "white") +
  geom_line(data=ts.data.TOPAZPOND.20.filter, aes(x=datetime, y = wtr_0.98), colour="blue")+
  geom_line(data=ts.data.TOPAZPOND.20.filter, aes(x=datetime, y = wtr_0.08), colour="dodgerblue")+
  facet_wrap(~"TOPAZ POND", ncol=2) +
  
  scale_y_continuous(
    #  name = "Water Temp °C",
    name = " ",
    sec.axis = sec_axis(~ (.*10)+50, name = " "),breaks = seq(0, 30, by = 5)) +
  coord_cartesian(ylim=c(3, 27))+
  
  labs(x="Date", color="Rain Intensity")+
  geom_point(data = avg.sw.e.20[!is.na(avg.sw.e.20$RainIntensity), ], 
             aes(x = Date, y = max(airt) +7, color = RainIntensity), 
             size = 3, shape = 16) +  
  scale_color_manual(values = c("<5mm" = "#cbc9e2",  ">5mm" = "#6a51a3"),
                     breaks = c("<5mm", ">5mm"),
                     labels = c("<5mm", ">5mm"),
                     limits = c("<5mm", ">5mm"),  # Include all levels in the legend
                     drop=FALSE) +   
 # geom_text(aes(x = as.POSIXct("2020-11-02"),   y = 32, angle = 270, label = "TOPAZ POND", fontface="bold"))+
  theme_classic()+
  theme(panel.background = element_rect(fill='transparent'))
WTEMP.TOPAZPOND.20.smoke


#TOK30
ts.data.TOK30.20.filter=ts.data.TOK30.20[with(ts.data.TOK30.20, datetime >"2020-07-02 00:00:00" & datetime < "2020-10-05 00:00:00" ),]

WTEMP.TOK30.20.smoke=ggplot()+
  geom_rect(data = smoke_topaz_20,
            aes(xmin = Datetime, xmax = lead(Datetime), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "white","y" = "#fdbb84"), labels = c("No", "Yes"))+
  geom_area(data = avg.sw.t.20, aes(x = Date, y = (swrad-50)/10), color = "dimgray", fill = "white") +
  geom_line(data=ts.data.TOK30.20.filter, aes(x=datetime, y = wtr_0.77), colour="blue")+
  geom_line(data=ts.data.TOK30.20.filter, aes(x=datetime, y = wtr_0.28), colour="dodgerblue")+
  facet_wrap(~"TOK30", ncol=2) +
  
   scale_y_continuous(
    #  name = "Water Temp °C",
    name = " ",
    sec.axis = sec_axis(~ (.*10)+50, name = " "),breaks = seq(0, 30, by = 5)) +
  coord_cartesian(ylim=c(3, 27))+
  
  labs(x=" ")+
  geom_point(data = avg.sw.e.20[!is.na(avg.sw.e.20$RainIntensity), ], 
             aes(x = Date, y = max(airt) +7, color = RainIntensity), 
             size = 3, shape = 16) +  
  scale_color_manual(values = c("<5mm" = "#cbc9e2",  ">5mm" = "#6a51a3"),
                     breaks = c("<5mm", ">5mm"),
                     labels = c("<5mm", ">5mm"),
                     limits = c("<5mm", ">5mm"),  # Include all levels in the legend
                     drop=FALSE) +   
 # geom_text(aes(x = as.POSIXct("2020-11-02"),   y = 32, angle = 270, label = "TOK30 POND", fontface="bold"), show.legend = FALSE)+
  labs(x="Date", color="Rain Intensity")+

  theme_classic()+
  theme(panel.background = element_rect(fill='transparent'))
WTEMP.TOK30.20.smoke


wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd) 
files <- list.files()
load('Emerald_2020_winter_data.Rdata')
emerald1920 = output.list [[1]]
colnames(emerald1920)[colnames(emerald1920) == "temp_3.47"] <- "wtr_3.58"
colnames(emerald1920)[colnames(emerald1920) == "temp_9.12"] <- "wtr_9.14"
emerald20=merge(emerald20, emerald1920, all=T)

#EMERALD LAKE


emerald20.filter=emerald20[with(emerald20, datetime > "2020-07-02 00:00:00" & datetime < "2020-10-05 00:00:00" ),]

WTEMP.EML.20.smoke=ggplot()+
  geom_rect(data = smoke_eml_20,
            aes(xmin = Datetime, xmax = lead(Datetime), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "white","y" = "#fdbb84"), labels = c("No", "Yes"))+
  geom_area(data = avg.sw.e.20, aes(x = Date, y = (swrad-50)/10), color = "dimgray", fill = "white") +
  geom_line(data=emerald20.filter, aes(x=datetime, y = wtr_9.14), colour="blue")+
  geom_line(data=emerald20.filter, aes(x=datetime, y = wtr_3.58), colour="dodgerblue")+
  facet_wrap(~"EMERALD LAKE", ncol=2) +
  
  scale_y_continuous(
    name = "Water Temp °C",
    sec.axis = sec_axis(~ (.*10)+50, name = "Shortwave Radiation Wm-2"),breaks = seq(0, 30, by = 5)) +
  coord_cartesian(ylim=c(3, 27))+
  
  geom_point(data = avg.sw.e.20[!is.na(avg.sw.e.20$RainIntensity), ], 
             aes(x = Date, y = max(airt) +7, color = RainIntensity), 
             size = 3, shape = 16) +  
  scale_color_manual(values = c("<5mm" = "#cbc9e2",  ">5mm" = "#6a51a3"),
                     breaks = c("<5mm", ">5mm"),
                     labels = c("<5mm", ">5mm"),
                     limits = c("<5mm", ">5mm"),  # Include all levels in the legend
                     drop=FALSE) +   
  labs(x="Date", color="Rain Intensity")+
  
  #geom_text(aes(x = as.POSIXct("2020-11-02"),   y = 32, angle = 270, label = "EMERALD LAKE", fontface="bold"), show.legend = FALSE)+
  theme_classic()+
  theme(panel.background = element_rect(fill='transparent'), axis.title.y.left = element_text(color = "blue"))
WTEMP.EML.20.smoke







files <- list.files()
load('Topaz_2020_winter_data.Rdata')
topaz1920 = output.list [[1]]


#Topaz 20 shallowest temp is 1.91; closest to 3 is 2.78; adjust colnames in Topaz1920 to match so it merges into same column
colnames(topaz1920)[colnames(topaz1920) == "temp_2.8"] <- "wtr_2.78"
colnames(topaz1920)[colnames(topaz1920) == "temp_2.19"] <- "wtr_1.91"

topaz20=merge(topaz20, topaz1920, all=T)

#TOPAZ LAKE
topaz20.filter=topaz20[with(topaz20, datetime > "2020-07-02 00:00:00" & datetime < "2020-10-05 00:00:00" ),]

WTEMP.TOPAZLAKE.20.smoke=ggplot()+
  geom_rect(data = smoke_topaz_20,
            aes(xmin = Datetime, xmax = lead(Datetime), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "white","y" = "#fdbb84"), labels = c("No", "Yes"))+
  geom_area(data = avg.sw.t.20, aes(x = Date, y = (swrad-50)/10), color = "dimgray", fill = "white") +
  geom_line(data=topaz20.filter, aes(x=datetime, y = wtr_2.17), colour="blue")+
  geom_line(data=topaz20.filter, aes(x=datetime, y = wtr_1.91), colour="dodgerblue")+
  facet_wrap(~"TOPAZ LAKE", ncol=2) +
  
   scale_y_continuous(
   # name = "Water Temperature °C",
    name = " ",
    
    sec.axis = sec_axis(~ (.*10)+50, name = " "),breaks = seq(0, 30, by = 5)) +
  coord_cartesian(ylim=c(3, 27))+
  
  geom_point(data = avg.sw.e.20[!is.na(avg.sw.e.20$RainIntensity), ], 
             aes(x = Date, y = max(airt) +7, color = RainIntensity), 
             size = 3, shape = 16) +  
  scale_color_manual(values = c("<5mm" = "#cbc9e2",  ">5mm" = "#6a51a3"),
                     breaks = c("<5mm", ">5mm"),
                     labels = c("<5mm", ">5mm"),
                     limits = c("<5mm", ">5mm"),  # Include all levels in the legend
                     drop=FALSE) +   
  #labs(x="Date", y="Water Temp °C")+
  labs(x="Date", color="Rain Intensity")+
  
 # geom_text(aes(x = as.POSIXct("2020-11-02"),   y = 32, angle = 270, label = "TOPAZ LAKE", fontface="bold"), show.legend = FALSE)+
  theme_classic()+
  theme(panel.background = element_rect(fill='transparent'))
WTEMP.TOPAZLAKE.20.smoke



#combined air temp c and sw plot with smoke rectangles
airt_SW_20 <- ggplot() +
  geom_rect(data = smoke_topaz_20,
            aes(xmin = Datetime, xmax = lead(Datetime), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "white","y" = "#fdbb84"), labels = c("No", "Yes"))+
  geom_area(data = avg.sw.e.20, aes(x = Date, y = (swrad-50)/10), color = "dimgray", fill = "white") +
  geom_line(data = avg.sw.e.20, aes(x = Date, y = airt), color = "red") +
  geom_line(data = avg.sw.t.20, aes(x = Date, y = airt), color = "blue") +
  geom_point(data = subset(avg.sw.e.20, rain > 0), aes(x = Date, y = max(airt) + 15), color = "darkblue", size = 3) +
    scale_y_continuous(
    name = "Air Temp °C",
    sec.axis = sec_axis(~ (.*10)+50, name = " ")
  ) +
  coord_cartesian(ylim=c(3, 27))+
  geom_point(data = avg.sw.e.20[!is.na(avg.sw.e.20$RainIntensity), ], 
             aes(x = Date, y = max(airt) +7, color = RainIntensity), 
             size = 3, shape = 16) +  
  scale_color_manual(values = c("<5mm" = "#cbc9e2",  ">5mm" = "#6a51a3"),
                     breaks = c("<5mm", ">5mm"),
                     labels = c("<5mm", ">5mm"),
                     limits = c("<5mm", ">5mm"),  # Include all levels in the legend
                     drop=FALSE) +   
  labs(x="Date", color="Rain Intensity")+
  ggtitle("2020")+
  theme_classic()+
  theme(panel.background = element_rect(fill='transparent'))
airt_SW_20


#EMERALD combined air temp c and sw plot with smoke rectangles
airt_SW_eml <- ggplot() +
  geom_rect(data = smoke_eml_20,
            aes(xmin = Datetime, xmax = lead(Datetime), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "white","y" = "#fdbb84"), labels = c("No", "Yes"))+
  geom_area(data = avg.sw.e.20, aes(x = Date, y = (swrad-50)/10), color = "dimgray", fill = "white") +
  geom_line(data = avg.sw.e.20, aes(x = Date, y = airt), color = "red") +
  geom_point(data = avg.sw.e.20[!is.na(avg.sw.e.20$RainIntensity), ], 
             aes(x = Date, y = max(airt) +7, color = RainIntensity), 
             size = 3, shape = 16) +  
  scale_color_manual(values = c("<5mm" = "#cbc9e2",  ">5mm" = "#6a51a3"),
                     breaks = c("<5mm", ">5mm"),
                     labels = c("<5mm", ">5mm"),
                     limits = c("<5mm", ">5mm"),  # Include all levels in the legend
                     drop=FALSE) +     
  scale_y_continuous(
    name = "Air Temp °C   ",
    sec.axis = sec_axis(~ (.*10)+50, name = " "),breaks = seq(0, 30, by = 5)) +
  coord_cartesian(ylim=c(3, 27))+
  labs(x="Date", color="Rain Intensity")+
  facet_wrap(~"Emerald MET Station", ncol=2) +
  
#  geom_text(aes(x = as.POSIXct("2020-10-13"), y = 28, label = "Emerald MET Station 2020",  size = 3, fontface = "bold"), show.legend = FALSE)+
  theme_classic()+
  theme(panel.background = element_rect(fill='transparent'), axis.title.y = element_text(color = "red"))
airt_SW_eml

#TOPAZ combined air temp c and sw plot with smoke rectangles
airt_SW_topaz <- ggplot() +
  geom_rect(data = smoke_topaz_20,
            aes(xmin = Datetime, xmax = lead(Datetime), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "white","y" = "#fdbb84"), labels = c("No", "Yes"))+
  geom_area(data = avg.sw.t.20, aes(x = Date, y = (swrad-50)/10), color = "dimgray", fill = "white") +
  geom_line(data = avg.sw.t.20, aes(x = Date, y = airt), color = "red") +
  geom_point(data = avg.sw.e.20[!is.na(avg.sw.e.20$RainIntensity), ], 
             aes(x = Date, y = max(airt) +7, color = RainIntensity), 
             size = 3, shape = 16) +  
  scale_color_manual(values = c("<5mm" = "#cbc9e2",  ">5mm" = "#6a51a3"),
                     breaks = c("<5mm", ">5mm"),
                     labels = c("<5mm", ">5mm"),
                     limits = c("<5mm", ">5mm"),  # Include all levels in the legend
                     drop=FALSE) +   
  scale_y_continuous(
    name = "",
    
    sec.axis = sec_axis(~ (.*10)+50, name = " "),breaks = seq(0, 30, by = 5)) +
  coord_cartesian(ylim=c(3, 27))+
  labs(x="Date", color="Rain Intensity")+
  facet_wrap(~"Topaz MET Station", ncol=2) +
  
 # geom_text(aes(x = as.POSIXct("2020-10-13"), y = 28, label = "Topaz MET Station 2020",  size = 3, fontface = "bold"), show.legend = FALSE)+
  theme_classic() +
  theme(axis.title.y = element_text(hjust = 1, vjust = 0),panel.background = element_rect(fill='transparent'))+  # Adjust hjust and vjust for positioning
theme(axis.line = element_line(color='black'),
                      plot.background = element_blank(),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.border = element_blank()) 
airt_SW_topaz


#make a plot of smoke and swrad, with x axis on monthly scale
sw.20<- ggplot() +
  geom_rect(data = smoke_topaz_20,
            aes(xmin = Datetime, xmax = lead(Datetime), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "white","y" = "#fdbb84"), labels = c("No", "Yes"))+
  geom_area(data = avg.sw.e.20, aes(x = Date, y = (swrad-50)/10), color = "dimgray", fill = "white") +
  geom_line(data = avg.sw.e.20, aes(x = Date, y = airt), color = "red") +
  geom_line(data = avg.sw.t.20, aes(x = Date, y = airt), color = "blue") +
  scale_y_continuous(
    name = "Air Temp °C",
    sec.axis = sec_axis(~ (.*10)+50, name = " "),breaks = seq(0, 30, by = 5)) +
  coord_cartesian(ylim=c(3, 27))+
 # scale_x_date(labels = scales::date_format("%m")) +  # Format x-axis labels as months (numbers)
  labs(x="Date", color="Rain Intensity")+
  ggtitle("2020")+
  theme_classic()+
  theme(panel.background = element_rect(fill='transparent'))
sw.20




#plot_grid(airt_SW,NULL, WTEMP.EML.20.smoke, WTEMP.TOPAZLAKE.20.smoke, WTEMP.EMLPOND1.20.smoke, WTEMP.TOK11.20.smoke, WTEMP.TOPAZPOND.20.smoke, WTEMP.TOK30.20.smoke, ncol = 2)
plot_grid(airt_SW_20,NULL, WTEMP.EML.20.smoke, WTEMP.TOK11.20.smoke, WTEMP.TOPAZLAKE.20.smoke, WTEMP.TOPAZPOND.20.smoke, WTEMP.EMLPOND1.20.smoke,   WTEMP.TOK30.20.smoke, ncol = 2)

plot_grid(airt_SW_eml,airt_SW_topaz, WTEMP.EML.20.smoke, WTEMP.TOK11.20.smoke, WTEMP.TOPAZLAKE.20.smoke, WTEMP.TOPAZPOND.20.smoke, WTEMP.EMLPOND1.20.smoke,   WTEMP.TOK30.20.smoke, ncol = 2)









############################################################################################################################################################
###########################2021 plots with smoke day y/n #############################################################################################

#add smoke rectangles based on smoke day y/n
avg.sw.e.21=avg.sw.e.21[with(avg.sw.e.21, Date >  "2021-07-02 00:00:00" & Date < "2021-10-05 00:00:00" ),]
avg.sw.t.21=avg.sw.t.21[with(avg.sw.t.21, Date >  "2021-07-02 00:00:00" & Date < "2021-10-05 00:00:00"  ),]

smoke_eml_21=smoke_eml_21[with(smoke_eml_21, Date >  "2021-07-02 00:00:00" & Date < "2021-10-05 00:00:00"  ),]
smoke_topaz_21=smoke_topaz_21[with(smoke_topaz_21, Date >  "2021-07-02 00:00:00" & Date < "2021-10-05 00:00:00"  ),]

#EMLPOND1 
ts.data.EMLPOND1.21.filter=ts.data.EMLPOND1.21[with(ts.data.EMLPOND1.21, datetime > "2021-07-02 00:00:00" & datetime < "2021-10-05 00:00:00" ),]

WTEMP.EMLPOND1.21.smoke=ggplot()+
  geom_rect(data = smoke_eml_21,
            aes(xmin = Datetime, xmax = lead(Datetime), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "white","y" = "#fdbb84"), labels = c("No", "Yes"))+
  geom_area(data = avg.sw.e.21, aes(x = Date, y = (swrad-50)/10), color = "dimgray", fill = "white") +
  geom_line(data=ts.data.EMLPOND1.21.filter, aes(x=datetime, y = wtr_1.58), colour="blue")+
  geom_line(data=ts.data.EMLPOND1.21.filter, aes(x=datetime, y = wtr_0.1), colour="dodgerblue")+
  scale_y_continuous(
    name = " ",
  sec.axis = sec_axis(~ (.*10)+50, name = " "),breaks = seq(0, 30, by = 5)) +
  coord_cartesian(ylim=c(3, 27))+
  geom_point(data = avg.sw.e.21[!is.na(avg.sw.e.21$RainIntensity), ], 
             aes(x = Date, y = max(airt) +11, color = RainIntensity), 
             size = 3, shape = 16) +  
  scale_color_manual(values = c("<5mm" = "#cbc9e2",  ">5mm" = "#6a51a3"),
                     breaks = c("<5mm", ">5mm"),
                     labels = c("<5mm", ">5mm"),
                     limits = c("<5mm", ">5mm"),  # Include all levels in the legend
                     drop=FALSE) +   
  labs(x="Date", color="Rain Intensity")+
  #geom_text(aes(x = as.POSIXct("2021-10-15"), y = 28, label = "EMLPOND1 2021",  size = 3, fontface = "bold"), show.legend = FALSE)+
  facet_wrap(~"EMLPOND1", ncol=2) +
  
  theme_classic()+
  theme(panel.background = element_rect(fill='transparent'), axis.title.y.left = element_text(color = "blue"))
WTEMP.EMLPOND1.21.smoke


#TOK11
ts.data.TOK11.21.filter=ts.data.TOK11.21[with(ts.data.TOK11.21, datetime > "2021-07-02 00:00:00" & datetime < "2021-10-05 00:00:00"),]

WTEMP.TOK11.21.smoke=ggplot()+
  geom_rect(data = smoke_eml_21,
            aes(xmin = Datetime, xmax = lead(Datetime), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "white","y" = "#fdbb84"), labels = c("No", "Yes"))+
  geom_area(data = avg.sw.e.21, aes(x = Date, y = (swrad-50)/10), color = "dimgray", fill = "white") +
  geom_line(data=ts.data.TOK11.21.filter, aes(x=datetime, y = wtr_1.24), colour="dodgerblue")+
  geom_line(data=ts.data.TOK11.21.filter, aes(x=datetime, y = wtr_0.22), colour="lightseagreen")+
  scale_y_continuous(
    name = " ",
    sec.axis = sec_axis(~ (.*10)+50, name = " "),breaks = seq(0, 30, by = 5)) +
  coord_cartesian(ylim=c(3, 27))+
  
  geom_point(data = avg.sw.e.21[!is.na(avg.sw.e.21$RainIntensity), ], 
             aes(x = Date, y = max(airt) +11, color = RainIntensity), 
             size = 3, shape = 16) +  
  scale_color_manual(values = c("<5mm" = "#cbc9e2",  ">5mm" = "#6a51a3"),
                     breaks = c("<5mm", ">5mm"),
                     labels = c("<5mm", ">5mm"),
                     limits = c("<5mm", ">5mm"),  # Include all levels in the legend
                     drop=FALSE) +
  labs(x="Date", color="Rain Intensity")+
  #geom_text(aes(x = as.POSIXct("2021-10-15"), y = 28, label = "TOK11 2021",  size = 3, fontface = "bold"), show.legend = FALSE)+
  facet_wrap(~"TOK11", ncol=2) +
  theme_classic()+theme(axis.line = element_line(color='black'),
                        plot.background = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.border = element_blank())
WTEMP.TOK11.21.smoke

#TOK11 just a few days when the smoke event starts
ts.data.TOK11.21.filter.2=ts.data.TOK11.21[with(ts.data.TOK11.21, datetime > "2021-09-11 00:00:00" & datetime < "2021-09-17 00:00:00"),]
avg.sw.e.21.2=avg.sw.e.21[with(avg.sw.e.21, Date > "2021-09-11 00:00:00" & Date < "2021-09-18 00:00:00"),]
smoke_eml_21.2=smoke_eml_21[with(smoke_eml_21, Datetime > "2021-09-11 00:00:00" & Datetime < "2021-09-18 00:00:00"),]

WTEMP.TOK11.21.smoke.2=ggplot()+
  geom_rect(data = smoke_eml_21.2,
            aes(xmin = Datetime, xmax = lead(Datetime), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "white","y" = "#fdbb84"), labels = c("No", "Yes"))+
  geom_area(data = avg.sw.e.21.2, aes(x = Date, y = (swrad-50)/10), color = "dimgray", fill = "white") +
  geom_line(data=ts.data.TOK11.21.filter.2, aes(x=datetime, y = wtr_1.24), colour="blue")+
  geom_line(data=ts.data.TOK11.21.filter.2, aes(x=datetime, y = wtr_0.22), colour="dodgerblue")+
  scale_y_continuous(
    name = " ",
    sec.axis = sec_axis(~ (.*10)+50, name = " "),breaks = seq(0, 30, by = 5)) +
  coord_cartesian(ylim=c(3, 27))+
  geom_point(data = avg.sw.e.21[!is.na(avg.sw.e.21$RainIntensity), ], 
             aes(x = Date, y = max(airt) +11, color = RainIntensity), 
             size = 3, shape = 16) +  
  scale_color_manual(values = c("<5mm" = "#cbc9e2",  ">5mm" = "#6a51a3"),
                     breaks = c("<5mm", ">5mm"),
                     labels = c("<5mm", ">5mm"),
                     limits = c("<5mm", ">5mm"),  # Include all levels in the legend
                     drop=FALSE) +
  labs(x="Date", color="Rain Intensity")+
  #ggtitle("TOK11 2021")+
  theme_classic()+theme(axis.line = element_line(color='black'),
                        plot.background = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.border = element_blank())
WTEMP.TOK11.21.smoke.2

#TOPAZPOND
ts.data.TOPAZPOND.21.filter=ts.data.TOPAZPOND.21[with(ts.data.TOPAZPOND.21, datetime > "2021-07-02 00:00:00" & datetime < "2021-10-05 00:00:00"),]


WTEMP.TOPAZPOND.21.smoke=ggplot()+
  geom_rect(data = smoke_topaz_21,
            aes(xmin = Datetime, xmax = lead(Datetime), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "white","y" = "#fdbb84"), labels = c("No", "Yes"))+
  geom_area(data = avg.sw.t.21, aes(x = Date, y = (swrad-50)/10), color = "dimgray", fill = "white") +
  geom_line(data=ts.data.TOPAZPOND.21.filter, aes(x=datetime, y = wtr_0.98), colour="blue")+
  geom_line(data=ts.data.TOPAZPOND.21.filter, aes(x=datetime, y = wtr_0.8), colour="dodgerblue")+
  scale_y_continuous(
    name = " ",
    sec.axis = sec_axis(~ (.*10)+50, name = " "),breaks = seq(0, 30, by = 5)) +
  coord_cartesian(ylim=c(3, 27))+
  geom_point(data = avg.sw.t.21[!is.na(avg.sw.t.21$RainIntensity), ], 
             aes(x = Date, y = max(airt) +13, color = RainIntensity), 
             size = 3, shape = 16) +  
  scale_color_manual(values = c("<5mm" = "#cbc9e2",  ">5mm" = "#6a51a3"),
                     breaks = c("<5mm", ">5mm"),
                     labels = c("<5mm", ">5mm"),
                     limits = c("<5mm", ">5mm"),  # Include all levels in the legend
                     drop=FALSE) +
  labs(x="Date", color="Rain Intensity")+
  #geom_text(aes(x = as.POSIXct("2021-10-15"), y = 28, label = "TOPAZPOND 2021",  size = 3, fontface = "bold"), show.legend = FALSE)+
  facet_wrap(~"TOPAZ POND", ncol=2) +
  theme_classic()+
  theme(panel.background = element_rect(fill='transparent'))
WTEMP.TOPAZPOND.21.smoke


#TOK30
ts.data.TOK30.21.filter=ts.data.TOK30.21[with(ts.data.TOK30.21, datetime > "2021-07-02 00:00:00" & datetime < "2021-10-05 00:00:00"),]

WTEMP.TOK30.21.smoke=ggplot()+
  geom_rect(data = smoke_topaz_21,
            aes(xmin = Datetime, xmax = lead(Datetime), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "white","y" = "#fdbb84"), labels = c("No", "Yes"))+
  geom_area(data = avg.sw.t.21, aes(x = Date, y = (swrad-50)/10), color = "dimgray", fill = "white") +
  geom_line(data=ts.data.TOK30.21.filter, aes(x=datetime, y = wtr_0.77), colour="blue")+
  geom_line(data=ts.data.TOK30.21.filter, aes(x=datetime, y = wtr_0.28), colour="dodgerblue")+
  scale_y_continuous(
    name = " ",
    sec.axis = sec_axis(~ (.*10)+50, name = " "),breaks = seq(0, 30, by = 5)) +
  coord_cartesian(ylim=c(3, 27))+
  geom_point(data = avg.sw.t.21[!is.na(avg.sw.t.21$RainIntensity), ], 
             aes(x = Date, y = max(airt) +14, color = RainIntensity), 
             size = 3, shape = 16) +  
  scale_color_manual(values = c("<5mm" = "#cbc9e2",  ">5mm" = "#6a51a3"),
                     breaks = c("<5mm", ">5mm"),
                     labels = c("<5mm", ">5mm"),
                     limits = c("<5mm", ">5mm"),  # Include all levels in the legend
                     drop=FALSE) +
  labs(x="Date", color="Rain Intensity")+
  #geom_text(aes(x = as.POSIXct("2021-10-15"), y = 28, label = "TOK30 2021",  size = 3, fontface = "bold"), show.legend = FALSE)+
  facet_wrap(~"TOK30", ncol=2) +
  theme_classic()+
  theme(panel.background = element_rect(fill='transparent'))
WTEMP.TOK30.21.smoke


#EMERALD LAKE
emerald21.filter=emerald21[with(emerald21, datetime > "2021-07-02 00:00:00" & datetime < "2021-10-05 00:00:00"),]

WTEMP.EML.21.smoke=ggplot()+
  geom_rect(data = smoke_eml_21,
            aes(xmin = Datetime, xmax = lead(Datetime), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "white","y" = "#fdbb84"), labels = c("No", "Yes"))+
  geom_area(data = avg.sw.e.21, aes(x = Date, y = (swrad-50)/10), color = "dimgray", fill = "white") +
  geom_line(data=emerald21.filter, aes(x=datetime, y = wtr_9.14), colour="blue")+
  geom_line(data=emerald21.filter, aes(x=datetime, y = wtr_3.58), colour="dodgerblue")+
  scale_y_continuous(
    name = "Water Temp °C",
    sec.axis = sec_axis(~ (.*10)+50, name = "Shortwave Radiation Wm−2"),breaks = seq(0, 30, by = 5)) +
  coord_cartesian(ylim=c(3, 27))+
  geom_point(data = avg.sw.e.21[!is.na(avg.sw.e.21$RainIntensity), ], 
             aes(x = Date, y = max(airt) +11, color = RainIntensity), 
             size = 3, shape = 16) +  
  scale_color_manual(values = c("<5mm" = "#cbc9e2",  ">5mm" = "#6a51a3"),
                     breaks = c("<5mm", ">5mm"),
                     labels = c("<5mm", ">5mm"),
                     limits = c("<5mm", ">5mm"),  # Include all levels in the legend
                     drop=FALSE) +
  labs(x="Date", color="Rain Intensity")+
  #geom_text(aes(x = as.POSIXct("2021-10-15"), y = 28, label = "Emerald Lake 2021",  size = 3, fontface = "bold"), show.legend = FALSE)+
  facet_wrap(~"EMERALD LAKE", ncol=2) +
  theme_classic()+
theme(panel.background = element_rect(fill='transparent'), axis.title.y.left = element_text(color = "blue"))

WTEMP.EML.21.smoke




wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd) 
files <- list.files()
load('04_Topaz_2021_winterdata_corr.Rdata')
topaz21_early <- output.list[[1]]

#change temp_2.17 to wtr_2.55 so it merges into the same column
colnames(topaz21_early)[colnames(topaz21_early) == "temp_2.17"] <- "wtr_2.55"

#merge the two topaz datasets to get full 2021 data
topaz21=merge(topaz21_early, topaz21, all=T)
#TOPAZ LAKE
topaz21.filter=topaz21[with(topaz21, datetime > "2021-07-02 00:00:00" & datetime < "2021-10-05 00:00:00"),]

WTEMP.TOPAZLAKE.21.smoke=ggplot()+
  geom_rect(data = smoke_topaz_21,
            aes(xmin = Datetime, xmax = lead(Datetime), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "white","y" = "#fdbb84"), labels = c("No", "Yes"))+
  geom_area(data = avg.sw.t.21, aes(x = Date, y = (swrad-50)/10), color = "dimgray", fill = "white") +
  geom_line(data=topaz21.filter, aes(x=datetime, y = wtr_2.82), colour="blue")+
  geom_line(data=topaz21.filter, aes(x=datetime, y = wtr_2.55), colour="dodgerblue")+
  scale_y_continuous(
    name = " ",
    sec.axis = sec_axis(~ (.*10)+50, name = " "),breaks = seq(0, 30, by = 5)) +
  coord_cartesian(ylim=c(3, 27))+
  geom_point(data = avg.sw.t.21[!is.na(avg.sw.t.21$RainIntensity), ], 
             aes(x = Date, y = max(airt) +14, color = RainIntensity), 
             size = 3, shape = 16) +  
  scale_color_manual(values = c("<5mm" = "#cbc9e2",  ">5mm" = "#6a51a3"),
                     breaks = c("<5mm", ">5mm"),
                     labels = c("<5mm", ">5mm"),
                     limits = c("<5mm", ">5mm"),  # Include all levels in the legend
                     drop=FALSE) +
  labs(x="Date", color="Rain Intensity")+
  #geom_text(aes(x = as.POSIXct("2021-10-15"), y = 28, label = "Topaz Lake 2021",  size = 3, fontface = "bold"), show.legend = FALSE)+
  facet_wrap(~"TOPAZ LAKE", ncol=2) +
  theme_classic()+theme(axis.line = element_line(color='black'),
                        plot.background = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.border = element_blank())
WTEMP.TOPAZLAKE.21.smoke



#combined air temp c and sw plot with smoke rectangles
airt_SW_21 <- ggplot() +
  geom_rect(data = smoke_topaz_21,
            aes(xmin = Datetime, xmax = lead(Datetime), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "white","y" = "#fdbb84"), labels = c("No", "Yes"))+
  geom_area(data = avg.sw.e.21, aes(x = Date, y = (swrad-50)/10), color = "dimgray", fill = "white") +
  geom_line(data = avg.sw.e.21, aes(x = Date, y = airt), color = "red") +
  geom_line(data = avg.sw.t.21, aes(x = Date, y = airt), color = "blue") +
  scale_y_continuous(
    name = " ",
    sec.axis = sec_axis(~ (.*10)+50, name = " "),breaks = seq(0, 30, by = 5)) +
  coord_cartesian(ylim=c(3, 27))+
 
  labs(x="Date", color="Rain Intensity")+
  ggtitle("2021")+
  theme_classic()+
  theme(panel.background = element_rect(fill='transparent'))
airt_SW_21


#EMERALD combined air temp c and sw plot with smoke rectangles
airt_SW_eml_21 <- ggplot() +
  geom_rect(data = smoke_eml_21,
            aes(xmin = Datetime, xmax = lead(Datetime), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "white","y" = "#fdbb84"), labels = c("No", "Yes"))+
  geom_area(data = avg.sw.e.21, aes(x = Date, y = (swrad-50)/10), color = "dimgray", fill = "white") +
  geom_line(data = avg.sw.e.21, aes(x = Date, y = airt), color = "red") +
  geom_point(data = avg.sw.e.21[!is.na(avg.sw.e.21$RainIntensity), ], 
             aes(x = Date, y = max(airt) +11, color = RainIntensity), 
             size = 3, shape = 16) +  
  scale_color_manual(values = c("<5mm" = "#cbc9e2",  ">5mm" = "#6a51a3"),
                     breaks = c("<5mm", ">5mm"),
                     labels = c("<5mm", ">5mm"),
                     limits = c("<5mm", ">5mm"),  # Include all levels in the legend
                     drop=FALSE) +   
  scale_y_continuous(
    name = "Air Temp °C                ",
    sec.axis = sec_axis(~ (.*10)+50, name = " "),breaks = seq(0, 30, by = 5)) +
  coord_cartesian(ylim=c(3, 27))+
  labs(x="Date", color="Rain Intensity")+
  #geom_text(aes(x = as.POSIXct("2021-10-13"), y = 28, label = "Emerald MET Station 2021",  size = 3, fontface = "bold"), show.legend = FALSE)+
  facet_wrap(~"Emerald MET Station", ncol=2) +
  theme_classic()+
  theme(panel.background = element_rect(fill='transparent'), axis.title.y = element_text(color = "red"))

airt_SW_eml_21

#TOPAZ combined air temp c and sw plot with smoke rectangles
airt_SW_topaz_21 <- ggplot() +
  geom_rect(data = smoke_topaz_21,
            aes(xmin = Datetime, xmax = lead(Datetime), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "white","y" = "#fdbb84"), labels = c("No", "Yes"))+
  geom_area(data = avg.sw.t.21, aes(x = Date, y = (swrad-50)/10), color = "dimgray", fill = "white") +
  geom_line(data = avg.sw.t.21, aes(x = Date, y = airt), color = "red") +
  geom_point(data = avg.sw.t.21[!is.na(avg.sw.t.21$RainIntensity), ], 
             aes(x = Date, y = max(airt) +14, color = RainIntensity), 
             size = 3, shape = 16) +  
  scale_color_manual(values = c("<5mm" = "#cbc9e2",  ">5mm" = "#6a51a3"),
                     breaks = c("<5mm", ">5mm"),
                     labels = c("<5mm", ">5mm"),
                     limits = c("<5mm", ">5mm"),  # Include all levels in the legend
                     drop=FALSE) +     
  scale_y_continuous(
    name = " ",
    sec.axis = sec_axis(~ (.*10)+50, name = " "),breaks = seq(0, 30, by = 5)) +
  coord_cartesian(ylim=c(3, 27))+
  labs(x="Date", color="Rain Intensity")+
  #geom_text(aes(x = as.POSIXct("2021-10-13"), y = 28, label = "Topaz MET Station 2021",  size = 3, fontface = "bold"), show.legend = FALSE)+
  facet_wrap(~"Topaz MET Station", ncol=2) +
  theme_classic()+theme(axis.line = element_line(color='black'),
                        plot.background = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.border = element_blank())
airt_SW_topaz_21


#plot_grid(airt_SW,NULL, WTEMP.EML.21.smoke, WTEMP.TOPAZLAKE.21.smoke, WTEMP.EMLPOND1.21.smoke, WTEMP.TOK11.21.smoke, WTEMP.TOPAZPOND.21.smoke, WTEMP.TOK30.21.smoke, ncol = 2)
plot_grid(airt_SW_21,NULL, WTEMP.EML.21.smoke, WTEMP.TOK11.21.smoke, WTEMP.TOPAZLAKE.21.smoke, WTEMP.TOPAZPOND.21.smoke, WTEMP.EMLPOND1.21.smoke,   WTEMP.TOK30.21.smoke, ncol = 2)

plot_grid(airt_SW_eml_21,airt_SW_topaz_21, WTEMP.EML.21.smoke, WTEMP.TOK11.21.smoke, WTEMP.TOPAZLAKE.21.smoke, WTEMP.TOPAZPOND.21.smoke, WTEMP.EMLPOND1.21.smoke,   WTEMP.TOK30.21.smoke, ncol = 2)







############################################################################################################################################################
###########################2022 plots with sw #############################################################################################
#water temp plots with sw in background, for each site

avg.sw.e.22.filter=avg.sw.e.22[with(avg.sw.e.22, Date > "2022-07-02 00:00:00" & Date < "2022-10-05 00:00:00"),]

avg.sw.t.22.filter=avg.sw.t.22[with(avg.sw.t.22, Date > "2022-07-02 00:00:00" & Date < "2022-10-05 00:00:00"),]

#EMLPOND1 

ts.data.EMLPOND1.22.filter=ts.data.EMLPOND1.22[with(ts.data.EMLPOND1.22, datetime > "2022-07-02 00:00:00" & datetime < "2022-10-05 00:00:00"),]

WTEMP.EMLPOND1.22=ggplot()+
  geom_area(data = avg.sw.e.22.filter, aes(x = Date, y = (swrad-50)/10), color = "dimgray", fill = "white") +
  geom_line(data=ts.data.EMLPOND1.22.filter, aes(x=datetime, y = wtr_2.8), colour="blue")+
  geom_line(data=ts.data.EMLPOND1.22.filter, aes(x=datetime, y = wtr_1.58), colour="dodgerblue")+
  scale_y_continuous(
    name = " ",
  sec.axis = sec_axis(~ (.*10)+50, name = " "),breaks = seq(0, 30, by = 5)) +
  coord_cartesian(ylim=c(3, 27))+
  geom_point(data = avg.sw.e.22.filter[!is.na(avg.sw.e.22.filter$RainIntensity), ], 
             aes(x = Date, y = max(airt) +8, color = RainIntensity), 
             size = 3, shape = 16) +  
  scale_color_manual(values = c("<5mm" = "#cbc9e2",  ">5mm" = "#6a51a3"),
                     breaks = c("<5mm", ">5mm"),
                     labels = c("<5mm", ">5mm"),
                     limits = c("<5mm", ">5mm"),  # Include all levels in the legend
                     drop=FALSE) +
  labs(x="Date", color="Rain Intensity")+
 # geom_text(aes(x = as.POSIXct("2022-10-15"), y = 28, label = "EMLPOND1 2022",  size = 3, fontface = "bold"), show.legend = FALSE)+
  facet_wrap(~"EMLPOND1", ncol=2) +
    theme_classic()+
  theme(panel.background = element_rect(fill='transparent'), axis.title.y.left = element_text(color = "blue"))
WTEMP.EMLPOND1.22




#TOK11
ts.data.TOK11.22.filter=ts.data.TOK11.22[with(ts.data.TOK11.22, datetime > "2022-07-02 00:00:00" & datetime < "2022-10-05 00:00:00"),]

WTEMP.TOK11.22=ggplot()+
  geom_area(data = avg.sw.e.22.filter, aes(x = Date, y = (swrad-50)/10), color = "dimgray", fill = "white") +
  geom_line(data=ts.data.TOK11.22.filter, aes(x=datetime, y = wtr_2), colour="dodgerblue")+
  geom_line(data=ts.data.TOK11.22.filter, aes(x=datetime, y = wtr_1.24), colour="lightseagreen")+
  scale_y_continuous(
    name = " ",
    sec.axis = sec_axis(~ (.*10)+50, name = " "),breaks = seq(0, 30, by = 5)) +
  coord_cartesian(ylim=c(3, 27))+
  geom_point(data = avg.sw.e.22.filter[!is.na(avg.sw.e.22.filter$RainIntensity), ], 
             aes(x = Date, y = max(airt) +8, color = RainIntensity), 
             size = 3, shape = 16) +  
  scale_color_manual(values = c("<5mm" = "#cbc9e2",  ">5mm" = "#6a51a3"),
                     breaks = c("<5mm", ">5mm"),
                     labels = c("<5mm", ">5mm"),
                     limits = c("<5mm", ">5mm"),  # Include all levels in the legend
                     drop=FALSE) +
  labs(x="Date", color="Rain Intensity")+
  #geom_text(aes(x = as.POSIXct("2022-10-15"), y = 28, label = "TOK11 2022",  size = 3, fontface = "bold"), show.legend = FALSE)+
  facet_wrap(~"TOK11", ncol=2) +
  theme_classic()+theme(axis.line = element_line(color='black'),
                        plot.background = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.border = element_blank())
WTEMP.TOK11.22



#TOpazpond
ts.data.TOPAZPOND.22.filter=ts.data.TOPAZPOND.22[with(ts.data.TOPAZPOND.22, datetime > "2022-07-02 00:00:00" & datetime < "2022-10-05 00:00:00"),]

WTEMP.TOPAZPOND.22=ggplot()+
  geom_area(data = avg.sw.t.22.filter, aes(x = Date, y = (swrad-50)/10), color = "dimgray", fill = "white") +
  geom_line(data=ts.data.TOPAZPOND.22.filter, aes(x=datetime, y = wtr_1.66), colour="blue")+
  geom_line(data=ts.data.TOPAZPOND.22.filter, aes(x=datetime, y = wtr_0.98), colour="dodgerblue")+
  scale_y_continuous(
    name = " ",
    sec.axis = sec_axis(~ (.*10)+50, name = " "),breaks = seq(0, 30, by = 5)) +
  coord_cartesian(ylim=c(3, 27))+
  geom_point(data = avg.sw.t.22.filter[!is.na(avg.sw.t.22.filter$RainIntensity), ], 
             aes(x = Date, y = max(airt) +11, color = RainIntensity), 
             size = 3, shape = 16) +  
  scale_color_manual(values = c("<5mm" = "#cbc9e2",  ">5mm" = "#6a51a3"),
                     breaks = c("<5mm", ">5mm"),
                     labels = c("<5mm", ">5mm"),
                     limits = c("<5mm", ">5mm"),  # Include all levels in the legend
                     drop=FALSE) +
  labs(x="Date", color="Rain Intensity")+
# geom_text(aes(x = as.POSIXct("2022-10-15"), y = 28, label = "TOPAZPOND 2022",  size = 3, fontface = "bold"), show.legend = FALSE)+
  facet_wrap(~"TOPAZ POND", ncol=2) +
  theme_classic()+
  theme(panel.background = element_rect(fill='transparent'))
WTEMP.TOPAZPOND.22


#TOK30
ts.data.TOK30.22.filter=ts.data.TOK30.22[with(ts.data.TOK30.22, datetime > "2022-07-02 00:00:00" & datetime < "2022-10-05 00:00:00"),]

WTEMP.TOK30.22=ggplot()+
  geom_area(data = avg.sw.t.22.filter, aes(x = Date, y = (swrad-50)/10), color = "dimgray", fill = "white") +
  geom_line(data=ts.data.TOK30.22.filter, aes(x=datetime, y = wtr_1.34), colour="blue")+
  geom_line(data=ts.data.TOK30.22.filter, aes(x=datetime, y = wtr_0.77), colour="dodgerblue")+
  scale_y_continuous(
    name = " ",
    sec.axis = sec_axis(~ (.*10)+50, name = " "),breaks = seq(0, 30, by = 5)) +
  coord_cartesian(ylim=c(3, 27))+
  geom_point(data = avg.sw.t.22.filter[!is.na(avg.sw.t.22.filter$RainIntensity), ], 
             aes(x = Date, y = max(airt) +11, color = RainIntensity), 
             size = 3, shape = 16) +  
  scale_color_manual(values = c("<5mm" = "#cbc9e2",  ">5mm" = "#6a51a3"),
                     breaks = c("<5mm", ">5mm"),
                     labels = c("<5mm", ">5mm"),
                     limits = c("<5mm", ">5mm"),  # Include all levels in the legend
                     drop=FALSE) +

  labs(x=" ", color="Rain Intensity")+
  
  #geom_text(aes(x = as.POSIXct("2022-10-15"), y = 28, label = "TOK30 2022",  size = 3, fontface = "bold"), show.legend = FALSE)+
  facet_wrap(~"TOK30", ncol=2) +
  theme_classic()+
  theme(panel.background = element_rect(fill='transparent'))
WTEMP.TOK30.22



#Emerald lake
#ts.data.EMLPOND1.22.filter=ts.data.EMLPOND1.22[with(ts.data.EMLPOND1.22, datetime > "2022-08-01 00:00:00" & datetime < "2022-10-06 00:00:00"),]
avg.sw.e.22.filter2=avg.sw.e.22[with(avg.sw.e.22, Date > "2022-07-02 00:00:00" & Date < "2022-10-05 00:00:00"),]
ts.data.emerald.22=ts.data.emerald.22[with(ts.data.emerald.22, datetime > "2022-07-02 00:00:00" & datetime < "2022-10-05 00:00:00"),]

WTEMP.EML.22=ggplot()+
  geom_area(data = avg.sw.e.22.filter2, aes(x = Date, y = (swrad-50)/10), color = "dimgray", fill = "white") +
  geom_line(data=ts.data.emerald.22, aes(x=datetime, y = wtr_7.49), colour="blue")+
  geom_line(data=ts.data.emerald.22, aes(x=datetime, y = wtr_3.47), colour="dodgerblue")+
  
  scale_y_continuous(
    name = "Water Temp °C",
    sec.axis = sec_axis(~ (.*10)+50, name = "Shortwave Radiation Wm−2"),breaks = seq(0, 30, by = 5)) +
  coord_cartesian(ylim=c(3, 27))+
  geom_point(data = avg.sw.e.22.filter[!is.na(avg.sw.e.22.filter$RainIntensity), ], 
             aes(x = Date, y = max(airt) +8, color = RainIntensity), 
             size = 3, shape = 16) +  
  scale_color_manual(values = c("<5mm" = "#cbc9e2",  ">5mm" = "#6a51a3"),
                     breaks = c("<5mm", ">5mm"),
                     labels = c("<5mm", ">5mm"),
                     limits = c("<5mm", ">5mm"),  # Include all levels in the legend
                     drop=FALSE) +
  labs(x="Date", color="Rain Intensity")+
  #geom_text(aes(x = as.POSIXct("2022-10-15"), y = 28, label = "Emerald Lake 2022",  size = 3, fontface = "bold"), show.legend = FALSE)+
  facet_wrap(~"EMERALD LAKE", ncol=2) +
  theme_classic()+
  theme(panel.background = element_rect(fill='transparent'), axis.title.y.left = element_text(color = "blue"))
WTEMP.EML.22



#topaz lake
avg.sw.t.22.filter2=avg.sw.t.22[with(avg.sw.t.22, Date > "2022-07-02 00:00:00" & Date < "2022-10-05 00:00:00"),]
ts.data.topaz.22=ts.data.topaz.22[with(ts.data.topaz.22, datetime > "2022-07-02 00:00:00" & datetime < "2022-10-05 00:00:00"),]


WTEMP.TOPAZLAKE.22=ggplot()+
  geom_area(data = avg.sw.t.22.filter2, aes(x = Date, y = (swrad-50)/10), color = "dimgray", fill = "white") +
  geom_line(data=ts.data.topaz.22, aes(x=datetime, y = wtr_3.38), colour="blue")+
  geom_line(data=ts.data.topaz.22, aes(x=datetime, y = wtr_3.19), colour="dodgerblue")+
  scale_y_continuous(
    name = " ",
    sec.axis = sec_axis(~ (.*10)+50, name = " "),breaks = seq(0, 30, by = 5)) +
  coord_cartesian(ylim=c(3, 27))+
  geom_point(data = avg.sw.t.22.filter[!is.na(avg.sw.t.22.filter$RainIntensity), ], 
             aes(x = Date, y = max(airt) +11, color = RainIntensity), 
             size = 3, shape = 16) +  
  scale_color_manual(values = c("<5mm" = "#cbc9e2",  ">5mm" = "#6a51a3"),
                     breaks = c("<5mm", ">5mm"),
                     labels = c("<5mm", ">5mm"),
                     limits = c("<5mm", ">5mm"),  # Include all levels in the legend
                     drop=FALSE) +
  labs(x="Date", color="Rain Intensity")+
 # geom_text(aes(x = as.POSIXct("2022-10-15"), y = 28, label = "Topaz Lake 2022",  size = 3, fontface = "bold"), show.legend = FALSE)+
  facet_wrap(~"TOPAZ LAKE", ncol=2) +
  theme_classic()+
  theme(panel.background = element_rect(fill='transparent'))
WTEMP.TOPAZLAKE.22




#EMERALD combined air temp c and sw plot with smoke rectangles
airt_SW_eml_22 <- ggplot() +
   geom_area(data = avg.sw.e.22.filter, aes(x = Date, y = (swrad-50)/10), color = "dimgray", fill = "white") +
  geom_line(data = avg.sw.e.22.filter, aes(x = Date, y = airt), color = "red") +
  geom_point(data = avg.sw.e.22.filter[!is.na(avg.sw.e.22.filter$RainIntensity), ], 
             aes(x = Date, y = max(airt) +8, color = RainIntensity), 
             size = 3, shape = 16) +  
  scale_color_manual(values = c("<5mm" = "#cbc9e2",  ">5mm" = "#6a51a3"),
                     breaks = c("<5mm", ">5mm"),
                     labels = c("<5mm", ">5mm"),
                     limits = c("<5mm", ">5mm"),  # Include all levels in the legend
                     drop=FALSE) +   scale_y_continuous(
    name = "Air Temp °C           ",
    sec.axis = sec_axis(~ (.*10)+50, name = " "),breaks = seq(0, 30, by = 5)) +
  coord_cartesian(ylim=c(3, 27))+
  labs(x="Date", color = "Rain Intensity")+
 # geom_text(aes(x = as.POSIXct("2022-10-13"), y = 28, label = "Emerald MET Station 2022",  size = 3, fontface = "bold"), show.legend = FALSE)+
  facet_wrap(~"Emerald MET Station", ncol=2) +
  theme_classic()+
  theme(panel.background = element_rect(fill='transparent'), axis.title.y = element_text(color = "red"))
airt_SW_eml_22

#TOPAZ combined air temp c and sw plot with smoke rectangles
airt_SW_topaz_22 <- ggplot() +
  geom_area(data = avg.sw.t.22.filter, aes(x = Date, y = (swrad-50)/10), color = "dimgray", fill = "white") +
  geom_line(data = avg.sw.t.22.filter, aes(x = Date, y = airt), color = "red") +
  geom_point(data = avg.sw.t.22.filter[!is.na(avg.sw.t.22.filter$RainIntensity), ], 
             aes(x = Date, y = max(airt) +11, color = RainIntensity), 
             size = 3, shape = 16) +  
  scale_color_manual(values = c("<5mm" = "#cbc9e2",  ">5mm" = "#6a51a3"),
                     breaks = c("<5mm", ">5mm"),
                     labels = c("<5mm", ">5mm"),
                     limits = c("<5mm", ">5mm"),  # Include all levels in the legend
                     drop=FALSE) +  
  scale_y_continuous(
      #   name = "Air Temperature °C",
       name = " ",
       sec.axis = sec_axis(~ (.*10)+50, name = " "),breaks = seq(0, 30, by = 5)) +
  coord_cartesian(ylim=c(3, 27))+
  labs(x="Date", color="Rain Intensity")+
  #geom_text(aes(x = as.POSIXct("2022-10-13"), y = 28, label = "Topaz MET Station 2022", size = 3, fontface = "bold"), show.legend = FALSE)+
  facet_wrap(~"Topaz MET Station", ncol=2) +
  theme_classic()+theme(axis.line = element_line(color='black'),
                        plot.background = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.border = element_blank())
airt_SW_topaz_22


#eml and topaz combined air temp c and sw plot with smoke rectangles
airt_SW_22 <- ggplot() +
  geom_line(data = avg.sw.e.22.filter, aes(x = Date, y = swrad/10), color = "pink") +
  geom_line(data = avg.sw.t.22.filter, aes(x = Date, y = swrad/10), color = "dodgerblue") +
  geom_line(data = avg.sw.e.22.filter, aes(x = Date, y = airt), color = "red") +
  geom_line(data = avg.sw.t.22.filter, aes(x = Date, y = airt), color = "blue") +
    geom_point(data = subset(avg.sw.e.22.filter, rain > 0), aes(x = Date, y = max(airt) + 15), color = "darkblue", size = 3) +
  scale_y_continuous(
    name = "Air Temp °C",
    sec.axis = sec_axis(~ .*10, name = "SW Radiation")
  ) +
  labs(x="Date")+
  ggtitle("2022 eml MET")+
  theme_classic()+
  theme(panel.background = element_rect(fill='transparent'))
airt_SW_22


plot_grid(airt_SW_eml_22,airt_SW_topaz_22, WTEMP.EML.22, WTEMP.TOK11.22, WTEMP.TOPAZLAKE.22, WTEMP.TOPAZPOND.22, WTEMP.EMLPOND1.22, WTEMP.TOK30.22, ncol = 2)













#############################################################################################################################
####################################ARRANGE PLOTS #########################################################################################
plot_grid(airt_SW_eml,airt_SW_topaz, WTEMP.EML.20.smoke, WTEMP.TOK11.20.smoke, WTEMP.TOPAZLAKE.20.smoke, WTEMP.TOPAZPOND.20.smoke, WTEMP.EMLPOND1.20.smoke,   WTEMP.TOK30.20.smoke, ncol = 2)

plot_grid(airt_SW_eml_21,airt_SW_topaz_21, WTEMP.EML.21.smoke, WTEMP.TOK11.21.smoke, WTEMP.TOPAZLAKE.21.smoke, WTEMP.TOPAZPOND.21.smoke, WTEMP.EMLPOND1.21.smoke,   WTEMP.TOK30.21.smoke, ncol = 2)

plot_grid(airt_SW_eml_22,airt_SW_topaz_22, WTEMP.EML.22, WTEMP.TOK11.22, WTEMP.TOPAZLAKE.22, WTEMP.TOPAZPOND.22, WTEMP.EMLPOND1.22, WTEMP.TOK30.22, ncol = 2)

library(patchwork)

# Create lists of ggplots
# plot_list_20 <- list(airt_SW_eml, airt_SW_topaz, WTEMP.EML.20.smoke, WTEMP.TOPAZLAKE.20.smoke, WTEMP.EMLPOND1.20.smoke, WTEMP.TOK11.20.smoke, WTEMP.TOPAZPOND.20.smoke, WTEMP.TOK30.20.smoke)
# plot_list_21 <- list(airt_SW_eml_21, airt_SW_topaz_21, WTEMP.EML.21.smoke, WTEMP.TOPAZLAKE.21.smoke, WTEMP.EMLPOND1.21.smoke, WTEMP.TOK11.21.smoke, WTEMP.TOPAZPOND.21.smoke, WTEMP.TOK30.21.smoke)
# plot_list_22 <- list(airt_SW_eml_22, airt_SW_topaz_22, WTEMP.EML.22, WTEMP.TOPAZLAKE.22, WTEMP.EMLPOND1.22, WTEMP.TOK11.22, WTEMP.TOPAZPOND.22, WTEMP.TOK30.22)

#alternative plot list with subset of sites
plot_list_20 <- list(airt_SW_eml,  WTEMP.EML.20.smoke, WTEMP.TOK11.20.smoke)
plot_list_21 <- list(airt_SW_eml_21, WTEMP.EML.21.smoke,  WTEMP.TOK11.21.smoke)
plot_list_22 <- list(airt_SW_eml_22,  WTEMP.EML.22,  WTEMP.TOK11.22)

# Create a data frame to represent the combination of plots and years
years <- c(2020, 2021, 2022)
#sites <- c("SW_eml", "SW_topaz", "EML", "TOPAZLAKE", "EMLPOND1", "TOK11", "TOPAZPOND", "TOK30")
sites <- c("SW_eml", "EML",  "EMLPOND1", "TOK30")

df <- expand.grid(year = years, site = sites)

# Function to extract ggplots from the combined list
get_plot <- function(year, site) {
  plot_index <- (year - 2020) * length(sites) + match(site, sites)
  plot_list <- c(plot_list_20, plot_list_21, plot_list_22)
  plot <- plot_list[[plot_index]]
  
 
  # Remove left y-axis title and text for all plots except year 2020
  if (year != 2020) {
    plot <- plot + theme(axis.title.y.left = element_blank(), axis.text.y.left = element_blank())
  }
  
  # Remove secondary y-axis title and text for all plots except year 2022
  if (year != 2022) {
    plot <- plot + theme(axis.title.y.right = element_blank(), axis.text.y.right = element_blank())
  }
  
  # Remove x-axis title and text for all plots except TOK30
  if (site != "TOK30") {
    plot <- plot + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  }
  
  
  return(plot)
}

# Create a list to store individual plots
plots <- list()

# Add individual plots to the list
for (i in 1:nrow(df)) {
  plots[[i]] <- get_plot(df$year[i], df$site[i])
}

# Use patchwork to arrange the plots
combined_plot <- wrap_plots(plots, ncol = length(unique(df$year)))

# Add legend to the bottom of the combined plot
combined_plot <- combined_plot +
  plot_layout(guides = "collect")

# Show the combined plot
print(combined_plot)


###########################################################################################
######### switch orientation of plot grid so year is rows
library(patchwork)

# Create lists of ggplots
plot_list_20 <- list(airt_SW_eml, airt_SW_topaz, WTEMP.EML.20.smoke, WTEMP.TOPAZLAKE.20.smoke, WTEMP.EMLPOND1.20.smoke, WTEMP.TOK11.20.smoke, WTEMP.TOPAZPOND.20.smoke, WTEMP.TOK30.20.smoke)
plot_list_21 <- list(airt_SW_eml_21, airt_SW_topaz_21, WTEMP.EML.21.smoke, WTEMP.TOPAZLAKE.21.smoke, WTEMP.EMLPOND1.21.smoke, WTEMP.TOK11.21.smoke, WTEMP.TOPAZPOND.21.smoke, WTEMP.TOK30.21.smoke)
plot_list_22 <- list(airt_SW_eml_22, airt_SW_topaz_22, WTEMP.EML.22, WTEMP.TOPAZLAKE.22, WTEMP.EMLPOND1.22, WTEMP.TOK11.22, WTEMP.TOPAZPOND.22, WTEMP.TOK30.22)

# Create a data frame to represent the combination of plots and years
years <- c(2020, 2021, 2022)
sites <- c("SW_eml", "SW_topaz", "EML", "TOPAZLAKE", "EMLPOND1", "TOK11", "TOPAZPOND", "TOK30")
df <- expand.grid(site = sites, year = years)  # Switched the order of site and year

# Function to extract ggplots from the combined list
get_plot <- function(site, year) {  # Switched the order of site and year
  plot_index <- (year - 2020) * length(sites) + match(site, sites)
  plot_list <- c(plot_list_20, plot_list_21, plot_list_22)
  plot <- plot_list[[plot_index]]
  
  # Remove left y-axis title and text for all plots except year 2020
  if (year != 2020) {
    plot <- plot + theme(axis.title.y.left = element_blank(), axis.text.y.left = element_blank())
  }
  
  # Remove secondary y-axis title and text for all plots except year 2022
  if (year != 2022) {
    plot <- plot + theme(axis.title.y.right = element_blank(), axis.text.y.right = element_blank())
  }
  
  # Remove x-axis title and text for all plots except TOK30
  if (site != "TOK30") {
    plot <- plot + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  }
  
  return(plot)
}

# Create a list to store individual plots
plots <- list()

# Add individual plots to the list
for (i in 1:nrow(df)) {
  plots[[i]] <- get_plot(df$site[i], df$year[i])  # Switched the order of site and year
}

# Use patchwork to arrange the plots
combined_plot <- wrap_plots(plots, ncol = length(unique(df$site)))  # Switched the order of site and year

# Add legend to the bottom of the combined plot
combined_plot <- combined_plot +
  plot_layout(guides = "collect")

# Show the combined plot
print(combined_plot)





############################################################################################################################
################  2020 only
library(patchwork)

# Create a data frame to represent the combination of plots and years
years <- c(2020)
#sites <- c("SW_eml", "SW_topaz", "EML", "TOPAZLAKE", "EMLPOND1", "TOK11", "TOPAZPOND", "TOK30")
sites <- c("SW_eml", "EML",  "TOK11")

df <- expand.grid(year = years, site = sites)

# Function to extract ggplots from the combined list
get_plot <- function(year, site) {
  plot_index <- (year - 2020) * length(sites) + match(site, sites)
  plot_list <- c(plot_list_20, plot_list_21, plot_list_22)
  plot <- plot_list[[plot_index]]
  
  
  # Remove left y-axis title and text for all plots except year 2020
  if (year != 2020) {
    plot <- plot + theme(axis.title.y.left = element_blank(), axis.text.y.left = element_blank())
  }
  

  # Remove x-axis title and text for all plots except TOK30
  if (site != "TOK30") {
    plot <- plot + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  }
  
  
  return(plot)
}

# Create a list to store individual plots
plots <- list()

# Add individual plots to the list
for (i in 1:nrow(df)) {
  plots[[i]] <- get_plot(df$year[i], df$site[i])
}

# Use patchwork to arrange the plots
combined_plot <- wrap_plots(plots, ncol = length(unique(df$year)))

# Add legend to the bottom of the combined plot
combined_plot <- combined_plot +
  plot_layout(guides = "collect")


# Show the combined plot
print(combined_plot)

####################################################################################################################################################
################  2021 only
library(patchwork)

# Create a data frame to represent the combination of plots and years
years <- c(2021)
#sites <- c("SW_eml", "SW_topaz", "EML", "TOPAZLAKE", "EMLPOND1", "TOK11", "TOPAZPOND", "TOK30")
sites <- c("SW_eml", "EML",  "TOK11")

df <- expand.grid(year = years, site = sites)

# Function to extract ggplots from the combined list
get_plot <- function(year, site) {
  plot_index <- (year - 2020) * length(sites) + match(site, sites)
  plot_list <- c(plot_list_20, plot_list_21, plot_list_22)
  plot <- plot_list[[plot_index]]
  
  
  # Remove left y-axis title and text for all plots except year 2020
  if (year != 2021) {
    plot <- plot + theme(axis.title.y.left = element_blank(), axis.text.y.left = element_blank())
  }
  
  
  # Remove x-axis title and text for all plots except TOK30
  if (site != "TOK30") {
    plot <- plot + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  }
  
  
  return(plot)
}

# Create a list to store individual plots
plots <- list()

# Add individual plots to the list
for (i in 1:nrow(df)) {
  plots[[i]] <- get_plot(df$year[i], df$site[i])
}

# Use patchwork to arrange the plots
combined_plot <- wrap_plots(plots, ncol = length(unique(df$year)))

# Add legend to the bottom of the combined plot
combined_plot <- combined_plot +
  plot_layout(guides = "collect")

# Show the combined plot
print(combined_plot)




####################################################################################################################################################
################  2022 only
library(patchwork)

# Create a data frame to represent the combination of plots and years
years <- c(2022)
#sites <- c("SW_eml", "SW_topaz", "EML", "TOPAZLAKE", "EMLPOND1", "TOK11", "TOPAZPOND", "TOK30")
sites <- c("SW_eml", "EML",  "TOK11")
df <- expand.grid(year = years, site = sites)

# Function to extract ggplots from the combined list
get_plot <- function(year, site) {
  plot_index <- (year - 2020) * length(sites) + match(site, sites)
  plot_list <- c(plot_list_20, plot_list_21, plot_list_22)
  plot <- plot_list[[plot_index]]
  
  
  # Remove left y-axis title and text for all plots except year 2020
  if (year != 2022) {
    plot <- plot + theme(axis.title.y.left = element_blank(), axis.text.y.left = element_blank())
  }
  
  
  # Remove x-axis title and text for all plots except TOK30
  if (site != "TOK30") {
    plot <- plot + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  }
  
  
  return(plot)
}

# Create a list to store individual plots
plots <- list()

# Add individual plots to the list
for (i in 1:nrow(df)) {
  plots[[i]] <- get_plot(df$year[i], df$site[i])
}

# Use patchwork to arrange the plots
combined_plot <- wrap_plots(plots, ncol = length(unique(df$year)))

# Add legend to the bottom of the combined plot
combined_plot <- combined_plot +
  plot_layout(guides = "collect")

# Show the combined plot
print(combined_plot)



#############################################################################################################################
#############################################################################################################################

#only 3 sites
############################################################################################################################
################  2020 only
library(patchwork)
plot_list_20 <- list(airt_SW_eml,  WTEMP.EML.20.smoke, WTEMP.TOK11.20.smoke)
plot_list_21 <- list(airt_SW_eml_21, WTEMP.EML.21.smoke,  WTEMP.TOK11.21.smoke)
plot_list_22 <- list(airt_SW_eml_22,  WTEMP.EML.22,  WTEMP.TOK11.22)

# Create a data frame to represent the combination of plots and years
years <- c(2020)
sites <- c("SW_eml", "EML",  "TOK11")
df <- expand.grid(year = years, site = sites)

# Function to extract ggplots from the combined list
get_plot <- function(year, site) {
  plot_index <- (year - 2020) * length(sites) + match(site, sites)
  plot_list <- c(plot_list_20, plot_list_21, plot_list_22)
  plot <- plot_list[[plot_index]]
  
  
  # Remove left y-axis title and text for all plots except year 2020
  if (year != 2020) {
    plot <- plot + theme(axis.title.y.left = element_blank(), axis.text.y.left = element_blank())
  }
  
  
  # Remove x-axis title and text for all plots except TOK30
  if (site != "TOK11") {
    plot <- plot + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  }
  
  
  return(plot)
}

# Create a list to store individual plots
plots <- list()

# Add individual plots to the list
for (i in 1:nrow(df)) {
  plots[[i]] <- get_plot(df$year[i], df$site[i])
}

# Use patchwork to arrange the plots
combined_plot_2020 <- wrap_plots(plots, ncol = length(unique(df$year)))

# Add legend to the bottom of the combined plot
combined_plot_2020 <- combined_plot_2020 +
  plot_layout(guides = "collect")


# Show the combined plot
print(combined_plot_2020)

####################################################################################################################################################
################  2021 only
library(patchwork)

# Create a data frame to represent the combination of plots and years
years <- c(2021)
sites <- c("SW_eml", "EML",  "TOK11")
df <- expand.grid(year = years, site = sites)

# Function to extract ggplots from the combined list
get_plot <- function(year, site) {
  plot_index <- (year - 2020) * length(sites) + match(site, sites)
  plot_list <- c(plot_list_20, plot_list_21, plot_list_22)
  plot <- plot_list[[plot_index]]
  
  
  # Remove left y-axis title and text for all plots except year 2020
  if (year != 2021) {
    plot <- plot + theme(axis.title.y.left = element_blank(), axis.text.y.left = element_blank())
  }
  
  
  # Remove x-axis title and text for all plots except TOK30
  if (site != "TOK11") {
    plot <- plot + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  }
  
  
  return(plot)
}

# Create a list to store individual plots
plots <- list()

# Add individual plots to the list
for (i in 1:nrow(df)) {
  plots[[i]] <- get_plot(df$year[i], df$site[i])
}

# Use patchwork to arrange the plots
combined_plot_2021 <- wrap_plots(plots, ncol = length(unique(df$year)))

# Add legend to the bottom of the combined plot
combined_plot_2021 <- combined_plot_2021 +
  plot_layout(guides = "collect")

# Show the combined plot
print(combined_plot_2021)




####################################################################################################################################################
################  2022 only
library(patchwork)

# Create a data frame to represent the combination of plots and years
years <- c(2022)
sites <- c("SW_eml", "EML",  "TOK11")
df <- expand.grid(year = years, site = sites)

# Function to extract ggplots from the combined list
get_plot <- function(year, site) {
  plot_index <- (year - 2020) * length(sites) + match(site, sites)
  plot_list <- c(plot_list_20, plot_list_21, plot_list_22)
  plot <- plot_list[[plot_index]]
  
  
  # Remove left y-axis title and text for all plots except year 2020
  if (year != 2022) {
    plot <- plot + theme(axis.title.y.left = element_blank(), axis.text.y.left = element_blank())
  }
  
  
  # Remove x-axis title and text for all plots except TOK30
  if (site != "TOK11") {
    plot <- plot + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  }
  
  
  return(plot)
}

# Create a list to store individual plots
plots <- list()

# Add individual plots to the list
for (i in 1:nrow(df)) {
  plots[[i]] <- get_plot(df$year[i], df$site[i])
}

# Use patchwork to arrange the plots
combined_plot_2022 <- wrap_plots(plots, ncol = length(unique(df$year)))

# Add legend to the bottom of the combined plot
combined_plot_2022 <- combined_plot_2022 +
  plot_layout(guides = "collect")

# Show the combined plot
print(combined_plot_2022)



#make a legend for water temp line colors
temp_legend_plot <- ggplot() +
  geom_line(aes(x = c(0, 0), y = c(0, 0), color = "Surface water"), size = 2) +
  geom_line(aes(x = c(0, 0), y = c(0, 0), color = "Mid-depth (ponds); ~3m (lakes)"), size = 2) +
  geom_line(aes(x = c(0, 0), y = c(0, 0), color = "Hypolimnion"), size = 2) +
  scale_color_manual(values = c("Surface water" = "lightseagreen", "Mid-depth (ponds); ~3m (lakes)" = "dodgerblue", "Hypolimnion" = "blue")) +
  labs(color = "Temperature logger position") +
  theme_void()
temp_legend_plot

plot_grid(combined_plot_2020, combined_plot_2021, combined_plot_2022, temp_legend_plot, ncol=4)


wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Output"
setwd(wd) 

ggsave("combined_temp_2020.png", plot = combined_plot_2020, width = 5, height = 4.5, units = "in", dpi = 300)
ggsave("combined_temp_2021.png", plot = combined_plot_2021, width = 5, height = 4.5, units = "in", dpi = 300)
ggsave("combined_temp_2022.png", plot = combined_plot_2022, width = 5, height = 4.5, units = "in", dpi = 300)
ggsave("temp_legend.png", plot = temp_legend_plot, width = 3, height = 3, units = "in", dpi = 300)


############################################################################################################################################################
###########################2021 plots just a few days when the smoke event starts #############################################################################################

#add smoke rectangles based on smoke day y/n
avg.sw.e.21.2=avg.sw.e.21[with(avg.sw.e.21, Date > "2021-08-25 00:00:00" & Date < "2021-09-28 00:00:00"),]
smoke_eml_21.2=smoke_eml_21[with(smoke_eml_21, Datetime > "2021-09-01 00:00:00" & Datetime < "2021-09-28 00:00:00"),]

avg.sw.t.21.2=avg.sw.t.21[with(avg.sw.t.21, Date > "2021-08-25 00:00:00" & Date < "2021-09-28 00:00:00"),]
smoke_topaz_21.2=smoke_topaz_21[with(smoke_topaz_21, Datetime > "2021-08-25 00:00:00" & Datetime < "2021-09-28 00:00:00"),]

weather.data.e.21.2=weather.data.e.21[with(weather.data.e.21, Date > "2021-08-25 00:00:00" & Date < "2021-09-28 00:00:00"),]


#TOK11 just a few days when the smoke event starts
ts.data.TOK11.21.filter.2=ts.data.TOK11.21[with(ts.data.TOK11.21, datetime > "2021-08-25 00:00:00" & datetime < "2021-09-28 00:00:00"),]

WTEMP.TOK11.21.smoke.2=ggplot()+
  geom_rect(data = smoke_eml_21.2,
            aes(xmin = Datetime, xmax = lead(Datetime), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "white","y" = "#fdbb84"), labels = c("No", "Yes"))+
  #geom_area(data = weather.data.e.21.2, aes(x = Datetime, y = SW_solar/30), color = "dimgray", fill = "white") +
  geom_line(data=ts.data.TOK11.21.filter.2, aes(x=datetime, y = wtr_1.24), colour="blue")+
  geom_line(data=ts.data.TOK11.21.filter.2, aes(x=datetime, y = wtr_0.22), colour="dodgerblue")+
  scale_y_continuous(
    name = "Water Temp °C",
    limits=c(8,25),
   # sec.axis = sec_axis(~ .*30 , name = "SW Radiation")
    ) +
  labs(x="Date")+
  ggtitle("TOK11 2021")+
  theme_classic()
WTEMP.TOK11.21.smoke.2


#EMLPOND1 just a few days when the smoke event starts
ts.data.EMLPOND1.21.filter.2=ts.data.EMLPOND1.21[with(ts.data.EMLPOND1.21, datetime >  "2021-08-25 00:00:00" & datetime < "2021-09-28 00:00:00"),]

WTEMP.EMLPOND1.21.smoke.2=ggplot()+
  geom_rect(data = smoke_eml_21.2,
            aes(xmin = Datetime, xmax = lead(Datetime), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "white","y" = "#fdbb84"), labels = c("No", "Yes"))+
 # geom_area(data = weather.data.e.21.2, aes(x = Datetime, y = SW_solar/30), color = "dimgray", fill = "white") +
  geom_line(data=ts.data.EMLPOND1.21.filter.2, aes(x=datetime, y = wtr_1.58), colour="blue")+
  geom_line(data=ts.data.EMLPOND1.21.filter.2, aes(x=datetime, y = wtr_0.1), colour="dodgerblue")+
  scale_y_continuous(
    name = "Water Temp °C",
    limits=c(10,23),
    
   # sec.axis = sec_axis(~ . *30 , name = "SW Radiation", limits = c(300, 800),breaks = seq(300, 800, by = 100))
  ) +
  labs(x="Date")+
  ggtitle("EMLPOND1 2021")+
  theme_classic()
WTEMP.EMLPOND1.21.smoke.2


#TOK30
ts.data.TOK30.21.filter.2=ts.data.TOK30.21[with(ts.data.TOK30.21, datetime > "2021-08-25 00:00:00" & datetime < "2021-09-28 00:00:00"),]

WTEMP.TOK30.21.smoke.2=ggplot()+
  geom_rect(data = smoke_topaz_21.2,
            aes(xmin = Datetime, xmax = lead(Datetime), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "white","y" = "#fdbb84"), labels = c("No", "Yes"))+
 # geom_area(data = avg.sw.t.21.2, aes(x = Date, y = swrad/10), color = "dimgray", fill = "white") +
  geom_line(data=ts.data.TOK30.21.filter.2, aes(x=datetime, y = wtr_0.77), colour="blue")+
  geom_line(data=ts.data.TOK30.21.filter.2, aes(x=datetime, y = wtr_0.28), colour="dodgerblue")+
  scale_y_continuous(
    name = "Water Temp °C",
    limits=c(5,22),
   # sec.axis = sec_axis(~ .*10, name = "SW Radiation")
    ) +
  labs(x="Date")+
  ggtitle("TOK30 2021")+
  theme_classic()
WTEMP.TOK30.21.smoke.2

#EMERALD LAKE



emerald21.filter.2=emerald21[with(emerald21, datetime > "2021-08-25 00:00:00" & datetime < "2021-09-28 00:00:00"),]
scaleFactor <- max(emerald21.filter.2$wtr_9.14) / max(avg.sw.e.21.2$swrad)

WTEMP.emerald.21.smoke.2=ggplot()+
  geom_rect(data = smoke_eml_21.2,
            aes(xmin = Datetime, xmax = lead(Datetime), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "white","y" = "#fdbb84"), labels = c("No", "Yes"))+
# geom_area(data = avg.sw.e.21.2, aes(x = Date, y = swrad*scaleFactor), color = "dimgray", fill = "white") +
  geom_line(data=emerald21.filter.2, aes(x=datetime, y = wtr_9.14), colour="blue")+
  geom_line(data=emerald21.filter.2, aes(x=datetime, y = wtr_3.58), colour="dodgerblue")+
  scale_y_continuous(
    name = "Water Temp °C",
    limits=c(8,25),
   #     sec.axis = sec_axis(~ ./scaleFactor, name = "SW Radiation")
  ) +
  labs(x="Date")+
  ggtitle("emerald lake 2021")+
  theme_classic()
WTEMP.emerald.21.smoke.2

plot_grid(WTEMP.TOK11.21.smoke.2, WTEMP.emerald.21.smoke.2, ncol=2)

#EMERALD combined air temp c and sw plot with smoke rectangles
airt_SW_eml_21.2 <- ggplot() +
  geom_rect(data = smoke_eml_21.2,
            aes(xmin = Datetime, xmax = lead(Datetime), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "white","y" = "#fdbb84"), labels = c("No", "Yes"))+
  geom_area(data = avg.sw.e.21.2, aes(x = Date, y = swrad/10), color = "dimgray", fill = "white") +
  geom_line(data = avg.sw.e.21.2, aes(x = Date, y = airt), color = "red") +
  scale_y_continuous(
    name = "Air Temp °C",
    sec.axis = sec_axis(~ .*10, name = "SW Radiation")
  ) +
  labs(x="Date")+
  geom_text(aes(x = as.POSIXct("2021-09-13"), y = 28, label = "Emerald MET Station 2021",  size = 3, fontface = "bold"), show.legend = FALSE)+
  theme_classic()
airt_SW_eml_21.2


#TOPAZ combined air temp c and sw plot with smoke rectangles
airt_SW_topaz_21.2 <- ggplot() +
  geom_rect(data = smoke_topaz_21.2,
            aes(xmin = Datetime, xmax = lead(Datetime), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "white","y" = "#fdbb84"), labels = c("No", "Yes"))+
  geom_area(data = avg.sw.t.21.2, aes(x = Date, y = swrad/10), color = "dimgray", fill = "white") +
  geom_line(data = avg.sw.t.21.2, aes(x = Date, y = airt), color = "red") +
  scale_y_continuous(
    name = "Air Temp °C",
    sec.axis = sec_axis(~ .*10, name = "SW Radiation")
  ) +
  labs(x="Date")+
  geom_text(aes(x = as.POSIXct("2021-09-13"), y = 28, label = "Topaz MET Station 2021",  size = 3, fontface = "bold"), show.legend = FALSE)+
  theme_classic()
airt_SW_topaz_21.2



##  ARRANGE PLOTS ###################

plot_grid(airt_SW_eml_21.2, airt_SW_topaz_21.2, WTEMP.emerald.21.smoke.2, WTEMP.EMLPOND1.21.smoke.2,WTEMP.TOK11.21.smoke.2, WTEMP.TOK30.21.smoke.2, ncol=2)





