#GAMS

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
library(tidyverse)

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


weather.data.e <- read.csv("Emerald_MET_data_2019_2021_hourly.csv",header=TRUE)
weather.data.e$Date = as.Date(weather.data.e$Datetime , format='%m/%d/%Y')
weather.data.e$Datetime=as.POSIXct(weather.data.e$Datetime, format='%m/%d/%Y %H:%M', tz="Etc/GMT-8")


weather.data.t <- read.csv("Topaz_MET_data_2019_2021_hourly.csv",header=TRUE)
weather.data.t$Date = as.Date(weather.data.t$Datetime , format='%m/%d/%Y')
weather.data.t$DOY = yday(weather.data.t$Date)
weather.data.t$Datetime=as.POSIXct(weather.data.t$Datetime, format='%m/%d/%Y %H:%M', tz="Etc/GMT-8")
weather.data.t$SW_solar=weather.data.t$SW_solar * -1

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

weather.data.e.20 <- weather.data.e.20[complete.cases(weather.data.e.20$Datetime), ]
avg.sw.e.20 <- weather.data.e.20 %>%
  group_by(Date) %>%
  summarize(
    swrad = mean(SW_solar, na.rm = TRUE),
    airt = mean(AirTemp, na.rm = TRUE)
  ) %>%
  mutate(Date = ymd_hms(paste(Date, "12:00:00")))

weather.data.e.21 <- weather.data.e.21[complete.cases(weather.data.e.21$Datetime), ]
avg.sw.e.21= weather.data.e.21%>%
  group_by(Date) %>%
  summarize(
    swrad = mean(SW_solar, na.rm = TRUE),
    airt = mean(AirTemp, na.rm = TRUE)
  ) %>%
  mutate(Date = ymd_hms(paste(Date, "12:00:00")))

weather.data.e.22 <- weather.data.e.22[complete.cases(weather.data.e.22$Datetime), ]
avg.sw.e.22= weather.data.e.22%>%
  group_by(Date) %>%
  summarize(
    swrad = mean(SW_solar, na.rm = TRUE),
    airt = mean(AirTemp, na.rm = TRUE)
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

weather.data.t.20 <- weather.data.t.20[complete.cases(weather.data.t.20$Datetime), ]
avg.sw.t.20= weather.data.t.20%>%
  group_by(Date) %>%
  summarize(
    swrad = mean(SW_solar, na.rm = TRUE),
    airt = mean(AirTemp, na.rm = TRUE)
  ) %>%
  mutate(Date = ymd_hms(paste(Date, "12:00:00")))

weather.data.t.21 <- weather.data.t.21[complete.cases(weather.data.t.21$Datetime), ]
avg.sw.t.21 <- weather.data.t.21 %>%
  group_by(Date) %>%
  summarize(
    swrad = mean(SW_solar, na.rm = TRUE),
    airt = mean(AirTemp, na.rm = TRUE)
  ) %>%
  mutate(Date = ymd_hms(paste(Date, "12:00:00", sep = " ")))


weather.data.t.22 <- weather.data.t.22[complete.cases(weather.data.t.22$Datetime), ]
avg.sw.t.22 <- weather.data.t.22 %>%
  group_by(Date) %>%
  summarize(
    swrad = mean(SW_solar, na.rm = TRUE),
    airt = mean(AirTemp, na.rm = TRUE)
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
################################################################################################################################################################
################################################################################################################################################################

smoke_eml_20=smoke_eml_20[with(smoke_eml_20, Date > "2020-08-01 00:00:00" ),]



avg.sw.e.21=avg.sw.e.21[with(avg.sw.e.21, Date > "2021-08-01 00:00:00" ),]
avg.sw.t.21=avg.sw.t.21[with(avg.sw.t.21, Date > "2021-08-01 00:00:00" ),]

smoke_eml_21=smoke_eml_21[with(smoke_eml_21, Date > "2021-08-01 00:00:00" ),]
smoke_topaz_21=smoke_topaz_21[with(smoke_topaz_21, Date > "2021-08-01 00:00:00" ),]




############################################################################################################################################################
###########################2022 plots with sw #############################################################################################
#water temp plots with sw in background, for each site

avg.sw.e.22.filter=avg.sw.e.22[with(avg.sw.e.22, Date > "2022-08-01 00:00:00" & Date < "2022-10-30 00:00:00"),]

avg.sw.t.22.filter=avg.sw.t.22[with(avg.sw.t.22, Date > "2022-08-01 00:00:00" & Date < "2022-10-30 00:00:00"),]


####################################### GAMs ##########################################################################
#############################################################################################################################
#############################################################################################################################

################### 2020 #############################################################################################################################################

# Aggregate hourly data to daily mean for tempdiff
eml20_daily_sw <- smoke_eml_20 %>%
  group_by(Date) %>%
  summarize(daily_swrad = mean(swrad, na.rm = TRUE))

topaz20_daily_sw <- smoke_topaz_20 %>%
  group_by(Date) %>%
  summarize(daily_swrad = mean(swrad, na.rm = TRUE))



##########################################
#EMLPOND1


## Aggregate temp data to a daily mean
ts.data.EMLPOND1.20.filter.2 <- ts.data.EMLPOND1.20 %>%
  select(datetime, starts_with("wtr_")) %>%
  pivot_longer(cols = starts_with("wtr_"), names_to = "variable", values_to = "value") %>%
  mutate(Date = as.Date(datetime)) %>%
  group_by(Date, variable) %>%
  summarise(daily_mean_temp = mean(value, na.rm = TRUE)) %>%
  pivot_wider(names_from = variable, values_from = daily_mean_temp) %>%
  group_by(Date, .add = TRUE) %>%  # Add .add = TRUE to prevent the warning
  summarise(across(everything(), ~mean(., na.rm = TRUE)))

# Aggregate sw data to a daily mean
ts.data.EMLPOND1.20.filter.2_swrad <- ts.data.EMLPOND1.20 %>%
  select(datetime, swrad) %>%
  mutate(Date = as.Date(datetime)) %>%
  group_by(Date) %>%
  summarise(daily_mean_swrad = mean(swrad, na.rm = TRUE))
# Merge with the original dataframe
ts.data.EMLPOND1.20.filter.2 <- left_join(ts.data.EMLPOND1.20.filter.2, ts.data.EMLPOND1.20.filter.2_swrad, by = "Date")

ts.data.EMLPOND1.20.filter.2$daily_mean_temp  <- rowMeans(ts.data.EMLPOND1.20.filter.2[,c('wtr_0.2', 'wtr_1.58')], na.rm=TRUE)

##############
test.EMLPOND1.20=merge(smoke_eml_20, ts.data.EMLPOND1.20.filter.2)
test.EMLPOND1.20$Smoke.day=as.factor(test.EMLPOND1.20$Smoke.day)

#run a gam

library(mgcv)
test.EMLPOND1.20=merge(smoke_eml_20, ts.data.EMLPOND1.20.filter.2, all=T)
test.EMLPOND1.20$Smoke.day=as.factor(test.EMLPOND1.20$Smoke.day)
test.EMLPOND1.20 <- test.EMLPOND1.20[complete.cases(test.EMLPOND1.20$doy), ]


gam.test.EMLPOND1.20 <- gam(daily_mean_temp ~ s(doy, by=Smoke.day) + Smoke.day,  data = test.EMLPOND1.20)
summary(gam.test.EMLPOND1.20)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.EMLPOND1.20, pages = 1, shade = TRUE, by="Smoke.day")
title("EMLPOND1 2020")
gam.EMLPOND1.20 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.EMLPOND1.20$doy), max(test.EMLPOND1.20$doy), length.out = 200),
                           Smoke.day = levels(test.EMLPOND1.20$Smoke.day))
smooth_data$smooth_pred <- predict(gam.test.EMLPOND1.20, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.EMLPOND1.20, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.EMLPOND1.20=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "EMLPOND1 2020", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.EMLPOND1.20

##########################################
#tok11
## Aggregate temp data to a daily mean
ts.data.TOK11.20.filter.2 <- ts.data.TOK11.20 %>%
  select(datetime, starts_with("wtr_")) %>%
  pivot_longer(cols = starts_with("wtr_"), names_to = "variable", values_to = "value") %>%
  mutate(Date = as.Date(datetime)) %>%
  group_by(Date, variable) %>%
  summarise(daily_mean_temp = mean(value, na.rm = TRUE)) %>%
  pivot_wider(names_from = variable, values_from = daily_mean_temp) %>%
  group_by(Date, .add = TRUE) %>%  # Add .add = TRUE to prevent the warning
  summarise(across(everything(), ~mean(., na.rm = TRUE)))

# Aggregate sw data to a daily mean
ts.data.TOK11.20.filter.2_swrad <- ts.data.TOK11.20 %>%
  select(datetime, swrad) %>%
  mutate(Date = as.Date(datetime)) %>%
  group_by(Date) %>%
  summarise(daily_mean_swrad = mean(swrad, na.rm = TRUE))
# Merge with the original dataframe
ts.data.TOK11.20.filter.2 <- left_join(ts.data.TOK11.20.filter.2, ts.data.TOK11.20.filter.2_swrad, by = "Date")

ts.data.TOK11.20.filter.2$daily_mean_temp  <- rowMeans(ts.data.TOK11.20.filter.2[,c('wtr_0.22', 'wtr_1.24')], na.rm=TRUE)

##############
test.TOK11.20=merge(smoke_eml_20, ts.data.TOK11.20.filter.2)
test.TOK11.20$Smoke.day=as.factor(test.TOK11.20$Smoke.day)

#run a gam

library(mgcv)
test.TOK11.20=merge(smoke_eml_20, ts.data.TOK11.20.filter.2, all=T)
test.TOK11.20$Smoke.day=as.factor(test.TOK11.20$Smoke.day)
test.TOK11.20 <- test.TOK11.20[complete.cases(test.TOK11.20$doy), ]

gam.test.TOK11.20 <- gam(daily_mean_temp ~ s(doy, by=Smoke.day) + Smoke.day,  data = test.TOK11.20)
summary(gam.test.TOK11.20)


# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.TOK11.20, pages = 1, shade = TRUE, by="Smoke.day")
title("TOK11 2020")
gam.TOK11.20 = recordPlot()


#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.TOK11.20$doy), max(test.TOK11.20$doy), length.out = 200),
                           Smoke.day = levels(test.TOK11.20$Smoke.day))
smooth_data$smooth_pred <- predict(gam.test.TOK11.20, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.TOK11.20, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.TOK11.20=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "TOK11 2020", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.TOK11.20



##########################################
#TOPAZPOND
## Aggregate temp data to a daily mean
ts.data.TOPAZPOND.20.filter.2 <- ts.data.TOPAZPOND.20 %>%
  select(datetime, starts_with("wtr_")) %>%
  pivot_longer(cols = starts_with("wtr_"), names_to = "variable", values_to = "value") %>%
  mutate(Date = as.Date(datetime)) %>%
  group_by(Date, variable) %>%
  summarise(daily_mean_temp = mean(value, na.rm = TRUE)) %>%
  pivot_wider(names_from = variable, values_from = daily_mean_temp) %>%
  group_by(Date, .add = TRUE) %>%  # Add .add = TRUE to prevent the warning
  summarise(across(everything(), ~mean(., na.rm = TRUE)))

# Aggregate sw data to a daily mean
ts.data.TOPAZPOND.20.filter.2_swrad <- ts.data.TOPAZPOND.20 %>%
  select(datetime, swrad) %>%
  mutate(Date = as.Date(datetime)) %>%
  group_by(Date) %>%
  summarise(daily_mean_swrad = mean(swrad, na.rm = TRUE))
# Merge with the original dataframe
ts.data.TOPAZPOND.20.filter.2 <- left_join(ts.data.TOPAZPOND.20.filter.2, ts.data.TOPAZPOND.20.filter.2_swrad, by = "Date")

ts.data.TOPAZPOND.20.filter.2$daily_mean_temp  <- rowMeans(ts.data.TOPAZPOND.20.filter.2[,c('wtr_0.08', 'wtr_0.98')], na.rm=TRUE)

##############
test.TOPAZPOND.20=merge(smoke_topaz_20, ts.data.TOPAZPOND.20.filter.2)
test.TOPAZPOND.20$Smoke.day=as.factor(test.TOPAZPOND.20$Smoke.day)

#run a gam

library(mgcv)
test.TOPAZPOND.20=merge(smoke_topaz_20, ts.data.TOPAZPOND.20.filter.2, all=T)
test.TOPAZPOND.20$Smoke.day=as.factor(test.TOPAZPOND.20$Smoke.day)
test.TOPAZPOND.20 <- test.TOPAZPOND.20[complete.cases(test.TOPAZPOND.20$doy), ]


gam.test.TOPAZPOND.20 <- gam(daily_mean_temp ~ s(doy, by=Smoke.day) + Smoke.day,  data = test.TOPAZPOND.20)
summary(gam.test.TOPAZPOND.20)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.TOPAZPOND.20, pages = 1, shade = TRUE, by="Smoke.day")
title("TOPAZPOND 2020")
gam.TOPAZPOND.20 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.TOPAZPOND.20$doy), max(test.TOPAZPOND.20$doy), length.out = 200),
                           Smoke.day = levels(test.TOPAZPOND.20$Smoke.day))
smooth_data$smooth_pred <- predict(gam.test.TOPAZPOND.20, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.TOPAZPOND.20, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.TOPAZPOND.20=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "TOPAZPOND 2020", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.TOPAZPOND.20



##########################################
#TOK30
## Aggregate temp data to a daily mean
ts.data.TOK30.20.filter.2 <- ts.data.TOK30.20 %>%
  select(datetime, starts_with("wtr_")) %>%
  pivot_longer(cols = starts_with("wtr_"), names_to = "variable", values_to = "value") %>%
  mutate(Date = as.Date(datetime)) %>%
  group_by(Date, variable) %>%
  summarise(daily_mean_temp = mean(value, na.rm = TRUE)) %>%
  pivot_wider(names_from = variable, values_from = daily_mean_temp) %>%
  group_by(Date, .add = TRUE) %>%  # Add .add = TRUE to prevent the warning
  summarise(across(everything(), ~mean(., na.rm = TRUE)))

# Aggregate sw data to a daily mean
ts.data.TOK30.20.filter.2_swrad <- ts.data.TOK30.20 %>%
  select(datetime, swrad) %>%
  mutate(Date = as.Date(datetime)) %>%
  group_by(Date) %>%
  summarise(daily_mean_swrad = mean(swrad, na.rm = TRUE))
# Merge with the original dataframe
ts.data.TOK30.20.filter.2 <- left_join(ts.data.TOK30.20.filter.2, ts.data.TOK30.20.filter.2_swrad, by = "Date")

ts.data.TOK30.20.filter.2$daily_mean_temp  <- rowMeans(ts.data.TOK30.20.filter.2[,c('wtr_0.28', 'wtr_0.77')], na.rm=TRUE)

##############
test.TOK30.20=merge(smoke_topaz_20, ts.data.TOK30.20.filter.2)
test.TOK30.20$Smoke.day=as.factor(test.TOK30.20$Smoke.day)

#run a gam

library(mgcv)
test.TOK30.20=merge(smoke_topaz_20, ts.data.TOK30.20.filter.2, all=T)
test.TOK30.20$Smoke.day=as.factor(test.TOK30.20$Smoke.day)
test.TOK30.20 <- test.TOK30.20[complete.cases(test.TOK30.20$doy), ]


gam.test.TOK30.20 <- gam(daily_mean_temp ~ s(doy, by=Smoke.day) + Smoke.day,  data = test.TOK30.20)
summary(gam.test.TOK30.20)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.TOK30.20, pages = 1, shade = TRUE, by="Smoke.day")
title("TOK30 2020")
gam.TOK30.20 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.TOK30.20$doy), max(test.TOK30.20$doy), length.out = 200),
                           Smoke.day = levels(test.TOK30.20$Smoke.day))
smooth_data$smooth_pred <- predict(gam.test.TOK30.20, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.TOK30.20, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.TOK30.20=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "TOK30 2020", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.TOK30.20




##########################################
#emerald lake
## Aggregate temp data to a daily mean
ts.data.emerald.20.filter.2 <- emerald20 %>%
  select(datetime, starts_with("wtr_")) %>%
  pivot_longer(cols = starts_with("wtr_"), names_to = "variable", values_to = "value") %>%
  mutate(Date = as.Date(datetime)) %>%
  group_by(Date, variable) %>%
  summarise(daily_mean_temp = mean(value, na.rm = TRUE)) %>%
  pivot_wider(names_from = variable, values_from = daily_mean_temp) %>%
  group_by(Date, .add = TRUE) %>%  # Add .add = TRUE to prevent the warning
  summarise(across(everything(), ~mean(., na.rm = TRUE)))

# Aggregate sw data to a daily mean
ts.data.emerald.20.filter.2_swrad <- emerald20 %>%
  select(datetime, swrad) %>%
  mutate(Date = as.Date(datetime)) %>%
  group_by(Date) %>%
  summarise(daily_mean_swrad = mean(swrad, na.rm = TRUE))
# Merge with the original dataframe
ts.data.emerald.20.filter.2 <- left_join(ts.data.emerald.20.filter.2, ts.data.emerald.20.filter.2_swrad, by = "Date")

ts.data.emerald.20.filter.2$daily_mean_temp  <- rowMeans(ts.data.emerald.20.filter.2[,c('wtr_3.58', 'wtr_8.64')], na.rm=TRUE)

##############
test.emerald.20=merge(smoke_eml_20, ts.data.emerald.20.filter.2)
test.emerald.20$Smoke.day=as.factor(test.emerald.20$Smoke.day)

#run a gam

library(mgcv)
test.emerald.20=merge(smoke_topaz_20, ts.data.emerald.20.filter.2, all=T)
test.emerald.20$Smoke.day=as.factor(test.emerald.20$Smoke.day)
test.emerald.20 <- test.emerald.20[complete.cases(test.emerald.20$doy), ]


gam.test.emerald.20 <- gam(daily_mean_temp ~ s(doy, by=Smoke.day) + Smoke.day,  data = test.emerald.20)
summary(gam.test.emerald.20)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.emerald.20, pages = 1, shade = TRUE, by="Smoke.day")
title("emerald 2020")
gam.emerald.20 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.emerald.20$doy), max(test.emerald.20$doy), length.out = 200),
                           Smoke.day = levels(test.emerald.20$Smoke.day))
smooth_data$smooth_pred <- predict(gam.test.emerald.20, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.emerald.20, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.emerald.20=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "emerald 2020", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.emerald.20


##########################################
#topaz lake
## Aggregate temp data to a daily mean
ts.data.topaz.20.filter.2 <- topaz20 %>%
  select(datetime, starts_with("wtr_")) %>%
  pivot_longer(cols = starts_with("wtr_"), names_to = "variable", values_to = "value") %>%
  mutate(Date = as.Date(datetime)) %>%
  group_by(Date, variable) %>%
  summarise(daily_mean_temp = mean(value, na.rm = TRUE)) %>%
  pivot_wider(names_from = variable, values_from = daily_mean_temp) %>%
  group_by(Date, .add = TRUE) %>%  # Add .add = TRUE to prevent the warning
  summarise(across(everything(), ~mean(., na.rm = TRUE)))

# Aggregate sw data to a daily mean
ts.data.topaz.20.filter.2_swrad <- topaz20 %>%
  select(datetime, swrad) %>%
  mutate(Date = as.Date(datetime)) %>%
  group_by(Date) %>%
  summarise(daily_mean_swrad = mean(swrad, na.rm = TRUE))
# Merge with the original dataframe
ts.data.topaz.20.filter.2 <- left_join(ts.data.topaz.20.filter.2, ts.data.topaz.20.filter.2_swrad, by = "Date")

ts.data.topaz.20.filter.2$daily_mean_temp  <- rowMeans(ts.data.topaz.20.filter.2[,c('wtr_1.91', 'wtr_2.78')], na.rm=TRUE)

##############
test.topaz.20=merge(smoke_eml_20, ts.data.topaz.20.filter.2)
test.topaz.20$Smoke.day=as.factor(test.topaz.20$Smoke.day)

#run a gam

library(mgcv)
test.topaz.20=merge(smoke_topaz_20, ts.data.topaz.20.filter.2, all=T)
test.topaz.20$Smoke.day=as.factor(test.topaz.20$Smoke.day)
test.topaz.20 <- test.topaz.20[complete.cases(test.topaz.20$doy), ]


gam.test.topaz.20 <- gam(daily_mean_temp ~ s(doy, by=Smoke.day) + Smoke.day,  data = test.topaz.20)
summary(gam.test.topaz.20)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.topaz.20, pages = 1, shade = TRUE, by="Smoke.day")
title("topaz 2020")
gam.topaz.20 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.topaz.20$doy), max(test.topaz.20$doy), length.out = 200),
                           Smoke.day = levels(test.topaz.20$Smoke.day))
smooth_data$smooth_pred <- predict(gam.test.topaz.20, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.topaz.20, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.topaz.20=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "topaz 2020", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.topaz.20


plot_grid(gam.emerald.20, gam.topaz.20, gam.EMLPOND1.20, gam.TOK11.20, gam.TOPAZPOND.20, gam.TOK30.20)

plot_grid(gam.emerald.20, gam.TOK11.20, gam.topaz.20, gam.TOPAZPOND.20, gam.EMLPOND1.20, gam.TOK30.20, ncol=2)









################### 2021 #############################################################################################################################################

# Aggregate hourly data to daily mean for tempdiff
eml21_daily_sw <- smoke_eml_21 %>%
  group_by(Date) %>%
  summarize(daily_swrad = mean(swrad, na.rm = TRUE))

topaz21_daily_sw <- smoke_topaz_21 %>%
  group_by(Date) %>%
  summarize(daily_swrad = mean(swrad, na.rm = TRUE))



##########################################
#EMLPOND1
library(dplyr)

# Aggregate temp data to a daily mean
ts.data.EMLPOND1.21.filter.2 <- ts.data.EMLPOND1.21 %>%
  select(datetime, starts_with("wtr_")) %>%
  pivot_longer(cols = starts_with("wtr_"), names_to = "variable", values_to = "value") %>%
  mutate(Date = as.Date(datetime)) %>%
  group_by(Date, variable) %>%
  summarise(daily_mean_temp = mean(value, na.rm = TRUE)) %>%
  pivot_wider(names_from = variable, values_from = daily_mean_temp) %>%
  group_by(Date, .add = TRUE) %>%  # Add .add = TRUE to prevent the warning
  summarise(across(everything(), ~mean(., na.rm = TRUE)))

# Aggregate sw data to a daily mean
ts.data.EMLPOND1.21.filter.2_swrad <- ts.data.EMLPOND1.21 %>%
  select(datetime, swrad) %>%
  mutate(Date = as.Date(datetime)) %>%
  group_by(Date) %>%
  summarise(daily_mean_swrad = mean(swrad, na.rm = TRUE))

# Merge with the original dataframe
ts.data.EMLPOND1.21.filter.2 <- left_join(ts.data.EMLPOND1.21.filter.2, ts.data.EMLPOND1.21.filter.2_swrad, by = "Date")

ts.data.EMLPOND1.21.filter.2$daily_mean_temp  <- rowMeans(ts.data.EMLPOND1.21.filter.2[,c('wtr_0.1', 'wtr_1.58')], na.rm=TRUE)


##############
test.EMLPOND1.21=merge(smoke_eml_21, ts.data.EMLPOND1.21.filter.2)
test.EMLPOND1.21$Smoke.day=as.factor(test.EMLPOND1.21$Smoke.day)

#run a gam

library(mgcv)
test.EMLPOND1.21=merge(smoke_eml_21, ts.data.EMLPOND1.21.filter.2, all=T)
test.EMLPOND1.21$Smoke.day=as.factor(test.EMLPOND1.21$Smoke.day)
test.EMLPOND1.21 <- test.EMLPOND1.21[complete.cases(test.EMLPOND1.21$doy), ]


gam.test.EMLPOND1.21 <- gam(daily_mean_temp ~ s(doy, by=Smoke.day) + Smoke.day,  data = test.EMLPOND1.21)
summary(gam.test.EMLPOND1.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.EMLPOND1.21, pages = 1, shade = TRUE, by="Smoke.day")
title("EMLPOND1 2021")
gam.EMLPOND1.21 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.EMLPOND1.21$doy), max(test.EMLPOND1.21$doy), length.out = 210),
                           Smoke.day = levels(test.EMLPOND1.21$Smoke.day))
smooth_data$smooth_pred <- predict(gam.test.EMLPOND1.21, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.EMLPOND1.21, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.EMLPOND1.21=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "EMLPOND1 2021", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.EMLPOND1.21

##########################################
#tok11
# Aggregate temp data to a daily mean
ts.data.TOK11.21.filter.2 <- ts.data.TOK11.21 %>%
  select(datetime, starts_with("wtr_")) %>%
  pivot_longer(cols = starts_with("wtr_"), names_to = "variable", values_to = "value") %>%
  mutate(Date = as.Date(datetime)) %>%
  group_by(Date, variable) %>%
  summarise(daily_mean_temp = mean(value, na.rm = TRUE)) %>%
  pivot_wider(names_from = variable, values_from = daily_mean_temp) %>%
  group_by(Date, .add = TRUE) %>%  # Add .add = TRUE to prevent the warning
  summarise(across(everything(), ~mean(., na.rm = TRUE)))

# Aggregate sw data to a daily mean
ts.data.TOK11.21.filter.2_swrad <- ts.data.TOK11.21 %>%
  select(datetime, swrad) %>%
  mutate(Date = as.Date(datetime)) %>%
  group_by(Date) %>%
  summarise(daily_mean_swrad = mean(swrad, na.rm = TRUE))

# Merge with the original dataframe
ts.data.TOK11.21.filter.2 <- left_join(ts.data.TOK11.21.filter.2, ts.data.TOK11.21.filter.2_swrad, by = "Date")

ts.data.TOK11.21.filter.2$daily_mean_temp  <- rowMeans(ts.data.TOK11.21.filter.2[,c('wtr_0.22', 'wtr_1.24')], na.rm=TRUE)


##############
test.TOK11.21=merge(smoke_eml_21, ts.data.TOK11.21.filter.2)
test.TOK11.21$Smoke.day=as.factor(test.TOK11.21$Smoke.day)

#run a gam

library(mgcv)
test.TOK11.21=merge(smoke_eml_21, ts.data.TOK11.21.filter.2, all=T)
test.TOK11.21$Smoke.day=as.factor(test.TOK11.21$Smoke.day)
test.TOK11.21 <- test.TOK11.21[complete.cases(test.TOK11.21$doy), ]

gam.test.TOK11.21 <- gam(daily_mean_temp ~ s(doy, by=Smoke.day) + Smoke.day,  data = test.TOK11.21)
summary(gam.test.TOK11.21)


# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.TOK11.21, pages = 1, shade = TRUE, by="Smoke.day")
title("TOK11 2021")
gam.TOK11.21 = recordPlot()


#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.TOK11.21$doy), max(test.TOK11.21$doy), length.out = 210),
                           Smoke.day = levels(test.TOK11.21$Smoke.day))
smooth_data$smooth_pred <- predict(gam.test.TOK11.21, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.TOK11.21, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.TOK11.21=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "TOK11 2021", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.TOK11.21



##########################################
#TOPAZPOND
# Aggregate temp data to a daily mean
ts.data.TOPAZPOND.21.filter.2 <- ts.data.TOPAZPOND.21 %>%
  select(datetime, starts_with("wtr_")) %>%
  pivot_longer(cols = starts_with("wtr_"), names_to = "variable", values_to = "value") %>%
  mutate(Date = as.Date(datetime)) %>%
  group_by(Date, variable) %>%
  summarise(daily_mean_temp = mean(value, na.rm = TRUE)) %>%
  pivot_wider(names_from = variable, values_from = daily_mean_temp) %>%
  group_by(Date, .add = TRUE) %>%  # Add .add = TRUE to prevent the warning
  summarise(across(everything(), ~mean(., na.rm = TRUE)))

# Aggregate sw data to a daily mean
ts.data.TOPAZPOND.21.filter.2_swrad <- ts.data.TOPAZPOND.21 %>%
  select(datetime, swrad) %>%
  mutate(Date = as.Date(datetime)) %>%
  group_by(Date) %>%
  summarise(daily_mean_swrad = mean(swrad, na.rm = TRUE))


# Merge with the original dataframe
ts.data.TOPAZPOND.21.filter.2 <- left_join(ts.data.TOPAZPOND.21.filter.2, ts.data.TOPAZPOND.21.filter.2_swrad, by = "Date")

ts.data.TOPAZPOND.21.filter.2$daily_mean_temp  <- rowMeans(ts.data.TOPAZPOND.21.filter.2[,c('wtr_0.98', 'wtr_0.8')], na.rm=TRUE)


##############
test.TOPAZPOND.21=merge(smoke_topaz_21, ts.data.TOPAZPOND.21.filter.2)
test.TOPAZPOND.21$Smoke.day=as.factor(test.TOPAZPOND.21$Smoke.day)

#run a gam

library(mgcv)
test.TOPAZPOND.21=merge(smoke_topaz_21, ts.data.TOPAZPOND.21.filter.2, all=T)
test.TOPAZPOND.21$Smoke.day=as.factor(test.TOPAZPOND.21$Smoke.day)
test.TOPAZPOND.21 <- test.TOPAZPOND.21[complete.cases(test.TOPAZPOND.21$doy), ]


gam.test.TOPAZPOND.21 <- gam(daily_mean_temp ~ s(doy, by=Smoke.day) + Smoke.day,  data = test.TOPAZPOND.21)
summary(gam.test.TOPAZPOND.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.TOPAZPOND.21, pages = 1, shade = TRUE, by="Smoke.day")
title("TOPAZPOND 2021")
gam.TOPAZPOND.21 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.TOPAZPOND.21$doy), max(test.TOPAZPOND.21$doy), length.out = 210),
                           Smoke.day = levels(test.TOPAZPOND.21$Smoke.day))
smooth_data$smooth_pred <- predict(gam.test.TOPAZPOND.21, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.TOPAZPOND.21, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.TOPAZPOND.21=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "TOPAZPOND 2021", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.TOPAZPOND.21



##########################################
#TOK30
# Aggregate temp data to a daily mean
ts.data.TOK30.21.filter.2 <- ts.data.TOK30.21 %>%
  select(datetime, starts_with("wtr_")) %>%
  pivot_longer(cols = starts_with("wtr_"), names_to = "variable", values_to = "value") %>%
  mutate(Date = as.Date(datetime)) %>%
  group_by(Date, variable) %>%
  summarise(daily_mean_temp = mean(value, na.rm = TRUE)) %>%
  pivot_wider(names_from = variable, values_from = daily_mean_temp) %>%
  group_by(Date, .add = TRUE) %>%  # Add .add = TRUE to prevent the warning
  summarise(across(everything(), ~mean(., na.rm = TRUE)))

# Aggregate sw data to a daily mean
ts.data.TOK30.21.filter.2_swrad <- ts.data.TOK30.21 %>%
  select(datetime, swrad) %>%
  mutate(Date = as.Date(datetime)) %>%
  group_by(Date) %>%
  summarise(daily_mean_swrad = mean(swrad, na.rm = TRUE))


# Merge with the original dataframe
ts.data.TOK30.21.filter.2 <- left_join(ts.data.TOK30.21.filter.2, ts.data.TOK30.21.filter.2_swrad, by = "Date")

ts.data.TOK30.21.filter.2$daily_mean_temp  <- rowMeans(ts.data.TOK30.21.filter.2[,c('wtr_0.28', 'wtr_0.77')], na.rm=TRUE)

##############
test.TOK30.21=merge(smoke_topaz_21, ts.data.TOK30.21.filter.2)
test.TOK30.21$Smoke.day=as.factor(test.TOK30.21$Smoke.day)

#run a gam

library(mgcv)
test.TOK30.21=merge(smoke_topaz_21, ts.data.TOK30.21.filter.2, all=T)
test.TOK30.21$Smoke.day=as.factor(test.TOK30.21$Smoke.day)
test.TOK30.21 <- test.TOK30.21[complete.cases(test.TOK30.21$doy), ]


gam.test.TOK30.21 <- gam(daily_mean_temp ~ s(doy, by=Smoke.day) + Smoke.day,  data = test.TOK30.21)
summary(gam.test.TOK30.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.TOK30.21, pages = 1, shade = TRUE, by="Smoke.day")
title("TOK30 2021")
gam.TOK30.21 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.TOK30.21$doy), max(test.TOK30.21$doy), length.out = 210),
                           Smoke.day = levels(test.TOK30.21$Smoke.day))
smooth_data$smooth_pred <- predict(gam.test.TOK30.21, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.TOK30.21, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.TOK30.21=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "TOK30 2021", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.TOK30.21




##########################################
#emerald lake
# Aggregate temp data to a daily mean
ts.data.emerald.21.filter.2 <- emerald21 %>%
  select(datetime, starts_with("wtr_")) %>%
  pivot_longer(cols = starts_with("wtr_"), names_to = "variable", values_to = "value") %>%
  mutate(Date = as.Date(datetime)) %>%
  group_by(Date, variable) %>%
  summarise(daily_mean_temp = mean(value, na.rm = TRUE)) %>%
  pivot_wider(names_from = variable, values_from = daily_mean_temp) %>%
  group_by(Date, .add = TRUE) %>%  # Add .add = TRUE to prevent the warning
  summarise(across(everything(), ~mean(., na.rm = TRUE)))

# Aggregate sw data to a daily mean
ts.data.emerald.21.filter.2_swrad <- emerald21 %>%
  select(datetime, swrad) %>%
  mutate(Date = as.Date(datetime)) %>%
  group_by(Date) %>%
  summarise(daily_mean_swrad = mean(swrad, na.rm = TRUE))

# Merge with the original dataframe
ts.data.emerald.21.filter.2 <- left_join(ts.data.emerald.21.filter.2, ts.data.emerald.21.filter.2_swrad, by = "Date")

ts.data.emerald.21.filter.2$daily_mean_temp  <- rowMeans(ts.data.emerald.21.filter.2[,c('wtr_3.58', 'wtr_8.64')], na.rm=TRUE)

##############
test.emerald.21=merge(smoke_eml_21, ts.data.emerald.21.filter.2)
test.emerald.21$Smoke.day=as.factor(test.emerald.21$Smoke.day)

#run a gam

library(mgcv)
test.emerald.21=merge(smoke_topaz_21, ts.data.emerald.21.filter.2, all=T)
test.emerald.21$Smoke.day=as.factor(test.emerald.21$Smoke.day)
test.emerald.21 <- test.emerald.21[complete.cases(test.emerald.21$doy), ]


gam.test.emerald.21 <- gam(daily_mean_temp ~ s(doy, by=Smoke.day) + Smoke.day,  data = test.emerald.21)
summary(gam.test.emerald.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.emerald.21, pages = 1, shade = TRUE, by="Smoke.day")
title("emerald 2021")
gam.emerald.21 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.emerald.21$doy), max(test.emerald.21$doy), length.out = 210),
                           Smoke.day = levels(test.emerald.21$Smoke.day))
smooth_data$smooth_pred <- predict(gam.test.emerald.21, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.emerald.21, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.emerald.21=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "emerald 2021", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.emerald.21


##########################################
#topaz lake
ts.data.topaz.21.filter.2 <- topaz21 %>%
  select(datetime, starts_with("wtr_")) %>%
  pivot_longer(cols = starts_with("wtr_"), names_to = "variable", values_to = "value") %>%
  mutate(Date = as.Date(datetime)) %>%
  group_by(Date, variable) %>%
  summarise(daily_mean_temp = mean(value, na.rm = TRUE)) %>%
  pivot_wider(names_from = variable, values_from = daily_mean_temp) %>%
  group_by(Date, .add = TRUE) %>%  # Add .add = TRUE to prevent the warning
  summarise(across(everything(), ~mean(., na.rm = TRUE)))

# Aggregate sw data to a daily mean
ts.data.topaz.21.filter.2_swrad <- topaz21 %>%
  select(datetime, swrad) %>%
  mutate(Date = as.Date(datetime)) %>%
  group_by(Date) %>%
  summarise(daily_mean_swrad = mean(swrad, na.rm = TRUE))

# Merge with the original dataframe
ts.data.topaz.21.filter.2 <- left_join(ts.data.topaz.21.filter.2, ts.data.topaz.21.filter.2_swrad, by = "Date")

ts.data.topaz.21.filter.2$daily_mean_temp  <- rowMeans(ts.data.topaz.21.filter.2[,c('wtr_2.55', 'wtr_3.37')], na.rm=TRUE)

##############
test.topaz.21=merge(smoke_eml_21, ts.data.topaz.21.filter.2)
test.topaz.21$Smoke.day=as.factor(test.topaz.21$Smoke.day)

#run a gam

library(mgcv)
test.topaz.21=merge(smoke_topaz_21, ts.data.topaz.21.filter.2, all=T)
test.topaz.21$Smoke.day=as.factor(test.topaz.21$Smoke.day)
test.topaz.21 <- test.topaz.21[complete.cases(test.topaz.21$doy), ]


gam.test.topaz.21 <- gam(daily_mean_temp ~ s(doy, by=Smoke.day) + Smoke.day,  data = test.topaz.21)
summary(gam.test.topaz.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.topaz.21, pages = 1, shade = TRUE, by="Smoke.day")
title("topaz 2021")
gam.topaz.21 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.topaz.21$doy), max(test.topaz.21$doy), length.out = 210),
                           Smoke.day = levels(test.topaz.21$Smoke.day))
smooth_data$smooth_pred <- predict(gam.test.topaz.21, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.topaz.21, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.topaz.21=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "topaz 2021", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.topaz.21


plot_grid(gam.emerald.21, gam.TOK11.21, gam.topaz.21, gam.TOPAZPOND.21, gam.EMLPOND1.21, gam.TOK30.21, ncol=2)




###########################################################################################################
#combine 2020 and 2021

#emlpond1
test.EMLPOND1.20.21=merge(test.EMLPOND1.20, test.EMLPOND1.21, all=T)

test.EMLPOND1.20.21$Smoke.day=as.factor(test.EMLPOND1.20.21$Smoke.day)


#run a gam

library(mgcv)
test.EMLPOND1.20.21 <- test.EMLPOND1.20.21[complete.cases(test.EMLPOND1.20.21$doy), ]


gam.test.EMLPOND1.20.21 <- gam(daily_mean_temp ~ s(doy, by=Smoke.day) + Smoke.day ,  data = test.EMLPOND1.20.21)
summary(gam.test.EMLPOND1.20.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.EMLPOND1.20.21, pages = 1, shade = TRUE, by="Smoke.day")
title("EMLPOND1 2021")
plot.gam.EMLPOND1.20.21 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.EMLPOND1.20.21$doy), max(test.EMLPOND1.20.21$doy), length.out = 210),
                           Smoke.day = levels(test.EMLPOND1.20.21$Smoke.day))
smooth_data$smooth_pred <- predict(gam.test.EMLPOND1.20.21, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.EMLPOND1.20.21, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.EMLPOND1.20.21=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "EMLPOND1 20-21", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.EMLPOND1.20.21






#TOK11
test.TOK11.20.21=merge(test.TOK11.20, test.TOK11.21, all=T)

test.TOK11.20.21$Smoke.day=as.factor(test.TOK11.20.21$Smoke.day)

#run a gam

library(mgcv)
est.TOK11.20.21 <- test.TOK11.20.21[complete.cases(test.TOK11.20.21$doy), ]


gam.test.TOK11.20.21 <- gam(daily_mean_temp ~ s(doy, by=Smoke.day) + Smoke.day,  data = test.TOK11.20.21)
summary(gam.test.TOK11.20.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.TOK11.20.21, pages = 1, shade = TRUE, by="Smoke.day")
title("TOK11 2020-2021")
gam.TOK11.21 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.TOK11.20.21$doy), max(test.TOK11.20.21$doy), length.out = 210),
                           Smoke.day = levels(test.TOK11.20.21$Smoke.day))
smooth_data$smooth_pred <- predict(gam.test.TOK11.20.21, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.TOK11.20.21, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.TOK11.20.21=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "TOK11 20-21", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.TOK11.20.21




#TOPAZPOND
test.TOPAZPOND.20.21=merge(test.TOPAZPOND.20, test.TOPAZPOND.21, all=T)

test.TOPAZPOND.20.21$Smoke.day=as.factor(test.TOPAZPOND.20.21$Smoke.day)

#run a gam

library(mgcv)
test.TOPAZPOND.20.21 <- test.TOPAZPOND.20.21[complete.cases(test.TOPAZPOND.20.21$doy), ]


gam.test.TOPAZPOND.20.21 <- gam(daily_mean_temp ~ s(doy, by=Smoke.day) + Smoke.day,  data = test.TOPAZPOND.20.21)
summary(gam.test.TOPAZPOND.20.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.TOPAZPOND.20.21, pages = 1, shade = TRUE, by="Smoke.day")
title("TOPAZPOND 2020-2021")
gam.TOPAZPOND.21 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.TOPAZPOND.20.21$doy), max(test.TOPAZPOND.20.21$doy), length.out = 210),
                           Smoke.day = levels(test.TOPAZPOND.20.21$Smoke.day))
smooth_data$smooth_pred <- predict(gam.test.TOPAZPOND.20.21, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.TOPAZPOND.20.21, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.TOPAZPOND.20.21=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "TOPAZPOND 20-21", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.TOPAZPOND.20.21





#TOK30
test.TOK30.20.21=merge(test.TOK30.20, test.TOK30.21, all=T)

test.TOK30.20.21$Smoke.day=as.factor(test.TOK30.20.21$Smoke.day)

#run a gam

library(mgcv)
test.TOK30.20.21 <- test.TOK30.20.21[complete.cases(test.TOK30.20.21$doy), ]


gam.test.TOK30.20.21 <- gam(daily_mean_temp ~ s(doy, by=Smoke.day) + Smoke.day,  data = test.TOK30.20.21)
summary(gam.test.TOK30.20.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.TOK30.20.21, pages = 1, shade = TRUE, by="Smoke.day")
title("TOK30 2020-2021")
gam.TOK30.21 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.TOK30.20.21$doy), max(test.TOK30.20.21$doy), length.out = 210),
                           Smoke.day = levels(test.TOK30.20.21$Smoke.day))
smooth_data$smooth_pred <- predict(gam.test.TOK30.20.21, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.TOK30.20.21, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.TOK30.20.21=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "TOK30 20-21", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.TOK30.20.21




#emerald LAKE
test.emerald.20.21=merge(test.emerald.20, test.emerald.21, all=T)


#run a gam

library(mgcv)
test.emerald.20.21$Smoke.day=as.factor(test.emerald.20.21$Smoke.day)
test.emerald.20.21 <- test.emerald.20.21[complete.cases(test.emerald.20.21$doy), ]


gam.test.emerald.20.21 <- gam(daily_mean_temp ~ s(doy, by=Smoke.day) + Smoke.day,  data = test.emerald.20.21)
summary(gam.test.emerald.20.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.emerald.20.21, pages = 1, shade = TRUE, by="Smoke.day")
title("emerald 2020-2021")
gam.emerald.21 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.emerald.20.21$doy), max(test.emerald.20.21$doy), length.out = 210),
                           Smoke.day = levels(test.emerald.20.21$Smoke.day))
smooth_data$smooth_pred <- predict(gam.test.emerald.20.21, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.emerald.20.21, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.emerald.20.21=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "emerald 20-21", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.emerald.20.21




#topaz LAKE
test.topaz.20.21=merge(test.topaz.20, test.topaz.21, all=T)

test.topaz.20.21$Smoke.day=as.factor(test.topaz.20.21$Smoke.day)

#run a gam

library(mgcv)
test.topaz.20.21 <- test.topaz.20.21[complete.cases(test.topaz.20.21$doy), ]


gam.test.topaz.20.21 <- gam(daily_mean_temp ~ s(doy, by=Smoke.day) + Smoke.day,  data = test.topaz.20.21)
summary(gam.test.topaz.20.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.topaz.20.21, pages = 1, shade = TRUE, by="Smoke.day")
title("topaz 2020-2021")
gam.topaz.21 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.topaz.20.21$doy), max(test.topaz.20.21$doy), length.out = 210),
                           Smoke.day = levels(test.topaz.20.21$Smoke.day))
smooth_data$smooth_pred <- predict(gam.test.topaz.20.21, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.topaz.20.21, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.topaz.20.21=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "topaz 20-21", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.topaz.20.21


plot_grid(gam.emerald.20.21, gam.TOK11.20.21, gam.topaz.20.21, gam.TOPAZPOND.20.21, gam.EMLPOND1.20.21, gam.TOK30.20.21, ncol=2)





###########################################################################################################

#combine 2020 and 2021; include year as a random effect

#emlpond1
test.EMLPOND1.20.21=merge(test.EMLPOND1.20, test.EMLPOND1.21, all=T)

test.EMLPOND1.20.21$Smoke.day=as.factor(test.EMLPOND1.20.21$Smoke.day)

test.EMLPOND1.20.21$year=year(test.EMLPOND1.20.21$Date)

#run a gam

library(mgcv)
test.EMLPOND1.20.21 <- test.EMLPOND1.20.21[complete.cases(test.EMLPOND1.20.21$doy), ]


gam.test.EMLPOND1.20.21 <- gam(daily_mean_temp ~ s(doy, by=Smoke.day) + Smoke.day + s(year, bs= "re"),  data = test.EMLPOND1.20.21)
summary(gam.test.EMLPOND1.20.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.EMLPOND1.20.21, pages = 1, shade = TRUE, by="Smoke.day")
title("EMLPOND1 2021")
plot.gam.EMLPOND1.20.21 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.EMLPOND1.20.21$doy), max(test.EMLPOND1.20.21$doy), length.out = 210),
                           Smoke.day = levels(test.EMLPOND1.20.21$Smoke.day),
                           year = unique(test.EMLPOND1.20.21$year))
smooth_data$smooth_pred <- predict(gam.test.EMLPOND1.20.21, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.EMLPOND1.20.21, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.EMLPOND1.20.21=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "EMLPOND1 20-21", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.EMLPOND1.20.21






#TOK11
test.TOK11.20.21=merge(test.TOK11.20, test.TOK11.21, all=T)

test.TOK11.20.21$Smoke.day=as.factor(test.TOK11.20.21$Smoke.day)
test.TOK11.20.21$year=year(test.TOK11.20.21$Date)

#run a gam

library(mgcv)
est.TOK11.20.21 <- test.TOK11.20.21[complete.cases(test.TOK11.20.21$doy), ]


gam.test.TOK11.20.21 <- gam(daily_mean_temp ~ s(doy, by=Smoke.day) + Smoke.day+ s(year, bs= "re"),  data = test.TOK11.20.21)
summary(gam.test.TOK11.20.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.TOK11.20.21, pages = 1, shade = TRUE, by="Smoke.day")
title("TOK11 2020-2021")
gam.TOK11.21 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.TOK11.20.21$doy), max(test.TOK11.20.21$doy), length.out = 210),
                           Smoke.day = levels(test.TOK11.20.21$Smoke.day),
                           year = unique(test.EMLPOND1.20.21$year))
smooth_data$smooth_pred <- predict(gam.test.TOK11.20.21, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.TOK11.20.21, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.TOK11.20.21=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "TOK11 20-21", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.TOK11.20.21




#TOPAZPOND
test.TOPAZPOND.20.21=merge(test.TOPAZPOND.20, test.TOPAZPOND.21, all=T)

test.TOPAZPOND.20.21$Smoke.day=as.factor(test.TOPAZPOND.20.21$Smoke.day)
test.TOPAZPOND.20.21$year=year(test.TOPAZPOND.20.21$Date)

#run a gam

library(mgcv)
test.TOPAZPOND.20.21 <- test.TOPAZPOND.20.21[complete.cases(test.TOPAZPOND.20.21$doy), ]


gam.test.TOPAZPOND.20.21 <- gam(daily_mean_temp ~ s(doy, by=Smoke.day) + Smoke.day+ s(year, bs= "re"),  data = test.TOPAZPOND.20.21)
summary(gam.test.TOPAZPOND.20.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.TOPAZPOND.20.21, pages = 1, shade = TRUE, by="Smoke.day")
title("TOPAZPOND 2020-2021")
gam.TOPAZPOND.21 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.TOPAZPOND.20.21$doy), max(test.TOPAZPOND.20.21$doy), length.out = 210),
                           Smoke.day = levels(test.TOPAZPOND.20.21$Smoke.day),
                           year = unique(test.EMLPOND1.20.21$year))
smooth_data$smooth_pred <- predict(gam.test.TOPAZPOND.20.21, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.TOPAZPOND.20.21, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.TOPAZPOND.20.21=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "TOPAZPOND 20-21", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.TOPAZPOND.20.21





#TOK30
test.TOK30.20.21=merge(test.TOK30.20, test.TOK30.21, all=T)

test.TOK30.20.21$Smoke.day=as.factor(test.TOK30.20.21$Smoke.day)
test.TOK30.20.21$year=year(test.TOK30.20.21$Date)

#run a gam

library(mgcv)
test.TOK30.20.21 <- test.TOK30.20.21[complete.cases(test.TOK30.20.21$doy), ]


gam.test.TOK30.20.21 <- gam(daily_mean_temp ~ s(doy, by=Smoke.day) + Smoke.day+ s(year, bs= "re"),  data = test.TOK30.20.21)
summary(gam.test.TOK30.20.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.TOK30.20.21, pages = 1, shade = TRUE, by="Smoke.day")
title("TOK30 2020-2021")
gam.TOK30.21 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.TOK30.20.21$doy), max(test.TOK30.20.21$doy), length.out = 210),
                           Smoke.day = levels(test.TOK30.20.21$Smoke.day),
                           year = unique(test.EMLPOND1.20.21$year))
smooth_data$smooth_pred <- predict(gam.test.TOK30.20.21, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.TOK30.20.21, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.TOK30.20.21=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "TOK30 20-21", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.TOK30.20.21




#emerald LAKE
test.emerald.20.21=merge(test.emerald.20, test.emerald.21, all=T)
test.emerald.20.21$year=year(test.emerald.20.21$Date)


#run a gam

library(mgcv)
test.emerald.20.21$Smoke.day=as.factor(test.emerald.20.21$Smoke.day)
test.emerald.20.21 <- test.emerald.20.21[complete.cases(test.emerald.20.21$doy), ]


gam.test.emerald.20.21 <- gam(daily_mean_temp ~ s(doy, by=Smoke.day) + Smoke.day+ s(year, bs= "re"),  data = test.emerald.20.21)
summary(gam.test.emerald.20.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.emerald.20.21, pages = 1, shade = TRUE, by="Smoke.day")
title("emerald 2020-2021")
gam.emerald.21 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.emerald.20.21$doy), max(test.emerald.20.21$doy), length.out = 210),
                           Smoke.day = levels(test.emerald.20.21$Smoke.day),
                           year = unique(test.EMLPOND1.20.21$year))
smooth_data$smooth_pred <- predict(gam.test.emerald.20.21, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.emerald.20.21, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.emerald.20.21=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "emerald 20-21", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.emerald.20.21




#topaz LAKE
test.topaz.20.21=merge(test.topaz.20, test.topaz.21, all=T)

test.topaz.20.21$Smoke.day=as.factor(test.topaz.20.21$Smoke.day)
test.topaz.20.21$year=year(test.topaz.20.21$Date)

#run a gam

library(mgcv)
test.topaz.20.21 <- test.topaz.20.21[complete.cases(test.topaz.20.21$doy), ]


gam.test.topaz.20.21 <- gam(daily_mean_temp ~ s(doy, by=Smoke.day) + Smoke.day+ s(year, bs= "re"),  data = test.topaz.20.21)
summary(gam.test.topaz.20.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.topaz.20.21, pages = 1, shade = TRUE, by="Smoke.day")
title("topaz 2020-2021")
gam.topaz.21 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.topaz.20.21$doy), max(test.topaz.20.21$doy), length.out = 210),
                           Smoke.day = levels(test.topaz.20.21$Smoke.day),
                           year = unique(test.EMLPOND1.20.21$year))
smooth_data$smooth_pred <- predict(gam.test.topaz.20.21, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.topaz.20.21, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.topaz.20.21=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "topaz 20-21", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.topaz.20.21


plot_grid(gam.emerald.20.21, gam.TOK11.20.21, gam.topaz.20.21, gam.TOPAZPOND.20.21, gam.EMLPOND1.20.21, gam.TOK30.20.21, ncol=2)








###########################################################################################################

#combine 2020 and 2021; leave smoke out of smooth term 

#emlpond1
test.EMLPOND1.20.21=merge(test.EMLPOND1.20, test.EMLPOND1.21, all=T)

test.EMLPOND1.20.21$Smoke.day=as.factor(test.EMLPOND1.20.21$Smoke.day)

test.EMLPOND1.20.21$year=year(test.EMLPOND1.20.21$Date)

#run a gam

library(mgcv)
test.EMLPOND1.20.21 <- test.EMLPOND1.20.21[complete.cases(test.EMLPOND1.20.21$doy), ]


gam.test.EMLPOND1.20.21 <- gam(daily_mean_temp ~ Smoke.day + s(doy),  data = test.EMLPOND1.20.21)

summary(gam.test.EMLPOND1.20.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.EMLPOND1.20.21, pages = 1, shade = TRUE, by="Smoke.day")
title("EMLPOND1 2021")
plot.gam.EMLPOND1.20.21 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.EMLPOND1.20.21$doy), max(test.EMLPOND1.20.21$doy), length.out = 210),
                           Smoke.day = levels(test.EMLPOND1.20.21$Smoke.day),
                           year = unique(test.EMLPOND1.20.21$year))
smooth_data$smooth_pred <- predict(gam.test.EMLPOND1.20.21, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.EMLPOND1.20.21, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.EMLPOND1.20.21=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "EMLPOND1 20-21", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.EMLPOND1.20.21






#TOK11
test.TOK11.20.21=merge(test.TOK11.20, test.TOK11.21, all=T)

test.TOK11.20.21$Smoke.day=as.factor(test.TOK11.20.21$Smoke.day)
test.TOK11.20.21$year=year(test.TOK11.20.21$Date)

#run a gam

library(mgcv)
est.TOK11.20.21 <- test.TOK11.20.21[complete.cases(test.TOK11.20.21$doy), ]


gam.test.TOK11.20.21 <- gam(daily_mean_temp ~ Smoke.day + s(doy),  data = test.TOK11.20.21)
summary(gam.test.TOK11.20.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.TOK11.20.21, pages = 1, shade = TRUE, by="Smoke.day")
title("TOK11 2020-2021")
gam.TOK11.21 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.TOK11.20.21$doy), max(test.TOK11.20.21$doy), length.out = 210),
                           Smoke.day = levels(test.TOK11.20.21$Smoke.day),
                           year = unique(test.EMLPOND1.20.21$year))
smooth_data$smooth_pred <- predict(gam.test.TOK11.20.21, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.TOK11.20.21, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.TOK11.20.21=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "TOK11 20-21", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.TOK11.20.21




#TOPAZPOND
test.TOPAZPOND.20.21=merge(test.TOPAZPOND.20, test.TOPAZPOND.21, all=T)

test.TOPAZPOND.20.21$Smoke.day=as.factor(test.TOPAZPOND.20.21$Smoke.day)
test.TOPAZPOND.20.21$year=year(test.TOPAZPOND.20.21$Date)

#run a gam

library(mgcv)
test.TOPAZPOND.20.21 <- test.TOPAZPOND.20.21[complete.cases(test.TOPAZPOND.20.21$doy), ]


gam.test.TOPAZPOND.20.21 <- gam(daily_mean_temp ~ Smoke.day + s(doy)+ s(year, bs= "re"),  data = test.TOPAZPOND.20.21)
summary(gam.test.TOPAZPOND.20.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.TOPAZPOND.20.21, pages = 1, shade = TRUE, by="Smoke.day")
title("TOPAZPOND 2020-2021")
gam.TOPAZPOND.21 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.TOPAZPOND.20.21$doy), max(test.TOPAZPOND.20.21$doy), length.out = 210),
                           Smoke.day = levels(test.TOPAZPOND.20.21$Smoke.day),
                           year = unique(test.EMLPOND1.20.21$year))
smooth_data$smooth_pred <- predict(gam.test.TOPAZPOND.20.21, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.TOPAZPOND.20.21, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.TOPAZPOND.20.21=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "TOPAZPOND 20-21", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.TOPAZPOND.20.21





#TOK30
test.TOK30.20.21=merge(test.TOK30.20, test.TOK30.21, all=T)

test.TOK30.20.21$Smoke.day=as.factor(test.TOK30.20.21$Smoke.day)
test.TOK30.20.21$year=year(test.TOK30.20.21$Date)

#run a gam

library(mgcv)
test.TOK30.20.21 <- test.TOK30.20.21[complete.cases(test.TOK30.20.21$doy), ]


gam.test.TOK30.20.21 <- gam(daily_mean_temp ~ Smoke.day + s(doy),  data = test.TOK30.20.21)
summary(gam.test.TOK30.20.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.TOK30.20.21, pages = 1, shade = TRUE, by="Smoke.day")
title("TOK30 2020-2021")
gam.TOK30.21 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.TOK30.20.21$doy), max(test.TOK30.20.21$doy), length.out = 210),
                           Smoke.day = levels(test.TOK30.20.21$Smoke.day),
                           year = unique(test.EMLPOND1.20.21$year))
smooth_data$smooth_pred <- predict(gam.test.TOK30.20.21, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.TOK30.20.21, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.TOK30.20.21=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "TOK30 20-21", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.TOK30.20.21




#emerald LAKE
test.emerald.20.21=merge(test.emerald.20, test.emerald.21, all=T)
test.emerald.20.21$year=year(test.emerald.20.21$Date)


#run a gam

library(mgcv)
test.emerald.20.21$Smoke.day=as.factor(test.emerald.20.21$Smoke.day)
test.emerald.20.21 <- test.emerald.20.21[complete.cases(test.emerald.20.21$doy), ]


gam.test.emerald.20.21 <- gam(daily_mean_temp ~ Smoke.day + s(doy),  data = test.emerald.20.21)
summary(gam.test.emerald.20.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.emerald.20.21, pages = 1, shade = TRUE, by="Smoke.day")
title("emerald 2020-2021")
gam.emerald.21 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.emerald.20.21$doy), max(test.emerald.20.21$doy), length.out = 210),
                           Smoke.day = levels(test.emerald.20.21$Smoke.day),
                           year = unique(test.EMLPOND1.20.21$year))
smooth_data$smooth_pred <- predict(gam.test.emerald.20.21, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.emerald.20.21, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.emerald.20.21=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "emerald 20-21", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.emerald.20.21




#topaz LAKE
test.topaz.20.21=merge(test.topaz.20, test.topaz.21, all=T)

test.topaz.20.21$Smoke.day=as.factor(test.topaz.20.21$Smoke.day)
test.topaz.20.21$year=year(test.topaz.20.21$Date)

#run a gam

library(mgcv)
test.topaz.20.21 <- test.topaz.20.21[complete.cases(test.topaz.20.21$doy), ]


gam.test.topaz.20.21 <- gam(daily_mean_temp ~ Smoke.day + s(doy),  data = test.topaz.20.21)
summary(gam.test.topaz.20.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.topaz.20.21, pages = 1, shade = TRUE, by="Smoke.day")
title("topaz 2020-2021")
gam.topaz.21 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.topaz.20.21$doy), max(test.topaz.20.21$doy), length.out = 210),
                           Smoke.day = levels(test.topaz.20.21$Smoke.day),
                           year = unique(test.EMLPOND1.20.21$year))
smooth_data$smooth_pred <- predict(gam.test.topaz.20.21, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.topaz.20.21, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.topaz.20.21=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "topaz 20-21", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.topaz.20.21


plot_grid(gam.emerald.20.21, gam.TOK11.20.21, gam.topaz.20.21, gam.TOPAZPOND.20.21, gam.EMLPOND1.20.21, gam.TOK30.20.21, ncol=2)






###########################################################################################################

#combine 2020 and 2021; leave smoke out of smooth term 

#emlpond1
test.EMLPOND1.20.21=merge(test.EMLPOND1.20, test.EMLPOND1.21, all=T)

test.EMLPOND1.20.21$Smoke.day=as.factor(test.EMLPOND1.20.21$Smoke.day)

test.EMLPOND1.20.21$year=year(test.EMLPOND1.20.21$Date)

#run a gam

library(mgcv)
test.EMLPOND1.20.21 <- test.EMLPOND1.20.21[complete.cases(test.EMLPOND1.20.21$doy), ]


gam.test.EMLPOND1.20.21 <- gam(daily_mean_temp ~ s(doy)+Smoke.day ,  data = test.EMLPOND1.20.21)

summary(gam.test.EMLPOND1.20.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.EMLPOND1.20.21, pages = 1, shade = TRUE, by="Smoke.day")
title("EMLPOND1 2021")
plot.gam.EMLPOND1.20.21 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.EMLPOND1.20.21$doy), max(test.EMLPOND1.20.21$doy), length.out = 210),
                           Smoke.day = levels(test.EMLPOND1.20.21$Smoke.day),
                           year = unique(test.EMLPOND1.20.21$year))
smooth_data$smooth_pred <- predict(gam.test.EMLPOND1.20.21, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.EMLPOND1.20.21, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.EMLPOND1.20.21=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "EMLPOND1 20-21", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.EMLPOND1.20.21






#TOK11
test.TOK11.20.21=merge(test.TOK11.20, test.TOK11.21, all=T)

test.TOK11.20.21$Smoke.day=as.factor(test.TOK11.20.21$Smoke.day)
test.TOK11.20.21$year=year(test.TOK11.20.21$Date)

#run a gam

library(mgcv)
est.TOK11.20.21 <- test.TOK11.20.21[complete.cases(test.TOK11.20.21$doy), ]


gam.test.TOK11.20.21 <- gam(daily_mean_temp ~ s(doy)+Smoke.day,  data = test.TOK11.20.21)
summary(gam.test.TOK11.20.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.TOK11.20.21, pages = 1, shade = TRUE, by="Smoke.day")
title("TOK11 2020-2021")
gam.TOK11.21 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.TOK11.20.21$doy), max(test.TOK11.20.21$doy), length.out = 210),
                           Smoke.day = levels(test.TOK11.20.21$Smoke.day),
                           year = unique(test.EMLPOND1.20.21$year))
smooth_data$smooth_pred <- predict(gam.test.TOK11.20.21, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.TOK11.20.21, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.TOK11.20.21=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "TOK11 20-21", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.TOK11.20.21




#TOPAZPOND
test.TOPAZPOND.20.21=merge(test.TOPAZPOND.20, test.TOPAZPOND.21, all=T)

test.TOPAZPOND.20.21$Smoke.day=as.factor(test.TOPAZPOND.20.21$Smoke.day)
test.TOPAZPOND.20.21$year=year(test.TOPAZPOND.20.21$Date)

#run a gam

library(mgcv)
test.TOPAZPOND.20.21 <- test.TOPAZPOND.20.21[complete.cases(test.TOPAZPOND.20.21$doy), ]


gam.test.TOPAZPOND.20.21 <- gam(daily_mean_temp ~ s(doy)+Smoke.day+ s(year, bs= "re"),  data = test.TOPAZPOND.20.21)
summary(gam.test.TOPAZPOND.20.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.TOPAZPOND.20.21, pages = 1, shade = TRUE, by="Smoke.day")
title("TOPAZPOND 2020-2021")
gam.TOPAZPOND.21 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.TOPAZPOND.20.21$doy), max(test.TOPAZPOND.20.21$doy), length.out = 210),
                           Smoke.day = levels(test.TOPAZPOND.20.21$Smoke.day),
                           year = unique(test.EMLPOND1.20.21$year))
smooth_data$smooth_pred <- predict(gam.test.TOPAZPOND.20.21, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.TOPAZPOND.20.21, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.TOPAZPOND.20.21=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "TOPAZPOND 20-21", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.TOPAZPOND.20.21





#TOK30
test.TOK30.20.21=merge(test.TOK30.20, test.TOK30.21, all=T)

test.TOK30.20.21$Smoke.day=as.factor(test.TOK30.20.21$Smoke.day)
test.TOK30.20.21$year=year(test.TOK30.20.21$Date)

#run a gam

library(mgcv)
test.TOK30.20.21 <- test.TOK30.20.21[complete.cases(test.TOK30.20.21$doy), ]


gam.test.TOK30.20.21 <- gam(daily_mean_temp ~ s(doy)+Smoke.day,  data = test.TOK30.20.21)
summary(gam.test.TOK30.20.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.TOK30.20.21, pages = 1, shade = TRUE, by="Smoke.day")
title("TOK30 2020-2021")
gam.TOK30.21 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.TOK30.20.21$doy), max(test.TOK30.20.21$doy), length.out = 210),
                           Smoke.day = levels(test.TOK30.20.21$Smoke.day),
                           year = unique(test.EMLPOND1.20.21$year))
smooth_data$smooth_pred <- predict(gam.test.TOK30.20.21, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.TOK30.20.21, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.TOK30.20.21=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "TOK30 20-21", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.TOK30.20.21




#emerald LAKE
test.emerald.20.21=merge(test.emerald.20, test.emerald.21, all=T)
test.emerald.20.21$year=year(test.emerald.20.21$Date)


#run a gam

library(mgcv)
test.emerald.20.21$Smoke.day=as.factor(test.emerald.20.21$Smoke.day)
test.emerald.20.21 <- test.emerald.20.21[complete.cases(test.emerald.20.21$doy), ]


gam.test.emerald.20.21 <- gam(daily_mean_temp ~ s(doy)+Smoke.day,  data = test.emerald.20.21)
summary(gam.test.emerald.20.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.emerald.20.21, pages = 1, shade = TRUE, by="Smoke.day")
title("emerald 2020-2021")
gam.emerald.21 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.emerald.20.21$doy), max(test.emerald.20.21$doy), length.out = 210),
                           Smoke.day = levels(test.emerald.20.21$Smoke.day),
                           year = unique(test.EMLPOND1.20.21$year))
smooth_data$smooth_pred <- predict(gam.test.emerald.20.21, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.emerald.20.21, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.emerald.20.21=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "emerald 20-21", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.emerald.20.21




#topaz LAKE
test.topaz.20.21=merge(test.topaz.20, test.topaz.21, all=T)

test.topaz.20.21$Smoke.day=as.factor(test.topaz.20.21$Smoke.day)
test.topaz.20.21$year=year(test.topaz.20.21$Date)

#run a gam

library(mgcv)
test.topaz.20.21 <- test.topaz.20.21[complete.cases(test.topaz.20.21$doy), ]


gam.test.topaz.20.21 <- gam(daily_mean_temp ~ s(doy)+Smoke.day,  data = test.topaz.20.21)
summary(gam.test.topaz.20.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.topaz.20.21, pages = 1, shade = TRUE, by="Smoke.day")
title("topaz 2020-2021")
gam.topaz.21 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.topaz.20.21$doy), max(test.topaz.20.21$doy), length.out = 210),
                           Smoke.day = levels(test.topaz.20.21$Smoke.day),
                           year = unique(test.EMLPOND1.20.21$year))
smooth_data$smooth_pred <- predict(gam.test.topaz.20.21, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.topaz.20.21, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.topaz.20.21=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "topaz 20-21", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.topaz.20.21


plot_grid(gam.emerald.20.21, gam.TOK11.20.21, gam.topaz.20.21, gam.TOPAZPOND.20.21, gam.EMLPOND1.20.21, gam.TOK30.20.21, ncol=2)


















###########################################################################################################

#combine 2020 and 2021; include year as a random effect; include sw radiation

#emlpond1
test.EMLPOND1.20.21=merge(test.EMLPOND1.20, test.EMLPOND1.21, all=T)

test.EMLPOND1.20.21$Smoke.day=as.factor(test.EMLPOND1.20.21$Smoke.day)

test.EMLPOND1.20.21$year=year(test.EMLPOND1.20.21$Date)

#run a gam

library(mgcv)
test.EMLPOND1.20.21 <- test.EMLPOND1.20.21[complete.cases(test.EMLPOND1.20.21$doy), ]

# Use 'swrad' as a fixed effect instead of a random effect
gam.test.EMLPOND1.20.21 <- gam(daily_mean_temp ~ s(doy, by = Smoke.day) + Smoke.day + daily_mean_swrad, data = test.EMLPOND1.20.21)

summary(gam.test.EMLPOND1.20.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.EMLPOND1.20.21, pages = 1, shade = TRUE, by = "Smoke.day")
title("EMLPOND1 2021")
plot.gam.EMLPOND1.20.21 = recordPlot()

# PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.EMLPOND1.20.21$doy), max(test.EMLPOND1.20.21$doy), length.out = 210),
                           Smoke.day = levels(test.EMLPOND1.20.21$Smoke.day),
                           daily_mean_swrad = unique(test.EMLPOND1.20.21$daily_mean_swrad))
smooth_data$smooth_pred <- predict(gam.test.EMLPOND1.20.21, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.EMLPOND1.20.21, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.EMLPOND1.20.21 = ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "EMLPOND1 20-21", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.EMLPOND1.20.21





#TOK11
test.TOK11.20.21=merge(test.TOK11.20, test.TOK11.21, all=T)

test.TOK11.20.21$Smoke.day=as.factor(test.TOK11.20.21$Smoke.day)
test.TOK11.20.21$year=year(test.TOK11.20.21$Date)

#run a gam

library(mgcv)
test.TOK11.20.21 <- test.TOK11.20.21[complete.cases(test.TOK11.20.21$doy), ]


gam.test.TOK11.20.21 <- gam(daily_mean_temp ~ s(doy, by=Smoke.day) + Smoke.day + daily_mean_swrad,  data = test.TOK11.20.21)
summary(gam.test.TOK11.20.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.TOK11.20.21, pages = 1, shade = TRUE, by="Smoke.day")
title("TOK11 2020-2021")
gam.TOK11.21 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(
            doy = seq(min(test.TOK11.20.21$doy), max(test.TOK11.20.21$doy), length.out = 210),
            Smoke.day = levels(test.TOK11.20.21$Smoke.day),
            daily_mean_swrad = test.TOK11.20.21$daily_mean_swrad)

smooth_data$smooth_pred <- predict(gam.test.TOK11.20.21, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.TOK11.20.21, newdata = smooth_data, se.fit = TRUE)$se.fit
# Plot using ggplot2
gam.TOK11.20.21=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "TOK11 20-21", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.TOK11.20.21




#TOPAZPOND
test.TOPAZPOND.20.21=merge(test.TOPAZPOND.20, test.TOPAZPOND.21, all=T)

test.TOPAZPOND.20.21$Smoke.day=as.factor(test.TOPAZPOND.20.21$Smoke.day)
test.TOPAZPOND.20.21$year=year(test.TOPAZPOND.20.21$Date)

#run a gam

library(mgcv)
test.TOPAZPOND.20.21 <- test.TOPAZPOND.20.21[complete.cases(test.TOPAZPOND.20.21$doy), ]

gam.test.TOPAZPOND.20.21 <- gam(daily_mean_temp ~ s(doy, by=Smoke.day) + Smoke.day+ daily_mean_swrad,  data = test.TOPAZPOND.20.21)
summary(gam.test.TOPAZPOND.20.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.TOPAZPOND.20.21, pages = 1, shade = TRUE, by="Smoke.day")
title("TOPAZPOND 2020-2021")
gam.TOPAZPOND.21 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.TOPAZPOND.20.21$doy), max(test.TOPAZPOND.20.21$doy), length.out = nrow(test.TOPAZPOND.20.21)),
                           Smoke.day = levels(test.TOPAZPOND.20.21$Smoke.day),
                           daily_mean_swrad = unique(test.TOPAZPOND.20.21$daily_mean_swrad))
smooth_data$smooth_pred <- predict(gam.test.TOPAZPOND.20.21, newdata = smooth_data, type = "response")

se_fit <- predict(gam.test.TOPAZPOND.20.21, newdata = smooth_data, se.fit = TRUE)$se.fit




# Plot using ggplot2
gam.TOPAZPOND.20.21=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "TOPAZPOND 20-21", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.TOPAZPOND.20.21





#TOK30
test.TOK30.20.21=merge(test.TOK30.20, test.TOK30.21, all=T)

test.TOK30.20.21$Smoke.day=as.factor(test.TOK30.20.21$Smoke.day)
test.TOK30.20.21$year=year(test.TOK30.20.21$Date)

#run a gam

library(mgcv)
test.TOK30.20.21 <- test.TOK30.20.21[complete.cases(test.TOK30.20.21$doy), ]


gam.test.TOK30.20.21 <- gam(daily_mean_temp ~ s(doy, by=Smoke.day) + Smoke.day+ daily_mean_swrad,  data = test.TOK30.20.21)
summary(gam.test.TOK30.20.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.TOK30.20.21, pages = 1, shade = TRUE, by="Smoke.day")
title("TOK30 2020-2021")
gam.TOK30.21 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.TOK30.20.21$doy), max(test.TOK30.20.21$doy), length.out = 210),
                           Smoke.day = levels(test.TOK30.20.21$Smoke.day),
                           daily_mean_swrad = test.TOK30.20.21$daily_mean_swrad)
smooth_data$smooth_pred <- predict(gam.test.TOK30.20.21, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.TOK30.20.21, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.TOK30.20.21=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "TOK30 20-21", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.TOK30.20.21




#emerald LAKE
test.emerald.20.21=merge(test.emerald.20, test.emerald.21, all=T)
test.emerald.20.21$year=year(test.emerald.20.21$Date)


#run a gam

library(mgcv)
test.emerald.20.21$Smoke.day=as.factor(test.emerald.20.21$Smoke.day)
test.emerald.20.21 <- test.emerald.20.21[complete.cases(test.emerald.20.21$doy), ]


gam.test.emerald.20.21 <- gam(daily_mean_temp ~ s(doy, by = Smoke.day) + Smoke.day + daily_mean_swrad,   data = test.emerald.20.21)
summary(gam.test.emerald.20.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.emerald.20.21, pages = 1, shade = TRUE, by="Smoke.day")
title("emerald 2020-2021")
gam.emerald.21 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.emerald.20.21$doy), max(test.emerald.20.21$doy), length.out = 210),
                           Smoke.day = levels(test.emerald.20.21$Smoke.day),
                           daily_mean_swrad = test.TOK30.20.21$daily_mean_swrad)
smooth_data$smooth_pred <- predict(gam.test.emerald.20.21, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.emerald.20.21, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.emerald.20.21=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "emerald 20-21", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.emerald.20.21




#topaz LAKE
test.topaz.20.21=merge(test.topaz.20, test.topaz.21, all=T)

test.topaz.20.21$Smoke.day=as.factor(test.topaz.20.21$Smoke.day)
test.topaz.20.21$year=year(test.topaz.20.21$Date)

#run a gam

library(mgcv)
test.topaz.20.21 <- test.topaz.20.21[complete.cases(test.topaz.20.21$doy), ]


gam.test.topaz.20.21 <- gam(daily_mean_temp ~ s(doy, by = Smoke.day) + Smoke.day + daily_mean_swrad,   data = test.topaz.20.21)
summary(gam.test.topaz.20.21)

# Plot the smooth term for DOY with separate lines for Smoke.day = "y" and "n"
plot(gam.test.topaz.20.21, pages = 1, shade = TRUE, by="Smoke.day")
title("topaz 2020-2021")
gam.topaz.21 = recordPlot()

#PLOT IN GGPLOT
# Predict smooth terms for plotting
smooth_data <- expand.grid(doy = seq(min(test.topaz.20.21$doy), max(test.topaz.20.21$doy), length.out = 210),
                           Smoke.day = levels(test.topaz.20.21$Smoke.day),
                           daily_mean_swrad = test.TOK30.20.21$daily_mean_swrad)
smooth_data$smooth_pred <- predict(gam.test.topaz.20.21, newdata = smooth_data, type = "response")
se_fit <- predict(gam.test.topaz.20.21, newdata = smooth_data, se.fit = TRUE)$se.fit

# Plot using ggplot2
gam.topaz.20.21=ggplot(smooth_data, aes(x = doy, y = smooth_pred, color = Smoke.day, fill = Smoke.day)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = smooth_pred - se_fit, ymax = smooth_pred + se_fit), alpha = 0.2) +
  scale_color_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  scale_fill_manual(values = c("y" = "#fdbb84", "n" = "gray")) +
  labs(title = "topaz 20-21", x = "Day of Year", y = "Smooth Term for DOY") +
  theme_bw()
gam.topaz.20.21


plot_grid(gam.emerald.20.21, gam.TOK11.20.21, gam.topaz.20.21, gam.TOPAZPOND.20.21, gam.EMLPOND1.20.21, gam.TOK30.20.21, ncol=2)


