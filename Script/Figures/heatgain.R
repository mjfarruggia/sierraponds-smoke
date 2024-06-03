#heat gain
###############################################################################################################################################################
library(LakeMetabolizer)
library(rLakeAnalyzer)
library(lubridate)
library(R2jags)
library(ggplot2)
library(cowplot)
library(dplyr)


################################################################################################################################################################
#######################################load pond temperature time series data 2021 #########################################################################################################################
#load pond temperature time series data 2021 

wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data/ponds"
setwd(wd)

#Choose year 
year <- "S21" #desired open water period, including season


#EMLPOND1
lake <- 'EMLPOND1' 
#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
# bookkeep.EMLPOND1.21 <- outputs[[1]]
# kalman.EMLPOND1.21 <- outputs[[2]]
# bayesian.EMLPOND1.21 <- outputs[[3]]
ts.data.EMLPOND1.21 <- outputs[[4]]
# DO_depth.EMLPOND1.21 <- outputs[[5]]



#load pond data - TOK11
lake <- 'TOK11' 
#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
# bookkeep.TOK11.21 <- outputs[[1]]
# kalman.TOK11.21 <- outputs[[2]]
# bayesian.TOK11.21 <- outputs[[3]]
ts.data.TOK11.21 <- outputs[[4]]
# DO_depth.TOK11.21 <- outputs[[5]]



#load pond data - TOPAZPOND
lake <- 'TOPAZPOND'
#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
# bookkeep.TOPAZPOND.21 <- outputs[[1]]
# kalman.TOPAZPOND.21 <- outputs[[2]]
# bayesian.TOPAZPOND.21 <- outputs[[3]]
ts.data.TOPAZPOND.21 <- outputs[[4]]
# DO_depth.TOPAZPOND.21 <- outputs[[5]]



#load pond data - TOK30
lake <- 'TOK30' 
#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
# bookkeep.TOK30.21 <- outputs[[1]]
# kalman.TOK30.21 <- outputs[[2]]
# bayesian.TOK30.21 <- outputs[[3]]
ts.data.TOK30.21 <- outputs[[4]]
# DO_depth.TOK30.21 <- outputs[[5]]


#load lake data
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd) 
files <- list.files()

load('Emerald_Topaz_sensor_data_2021_forMJ.Rdata')
emerald21= ts.2021.forMJ  [[1]]
topaz21_late = ts.2021.forMJ  [[2]]


load('04_Topaz_2021_winterdata_corr.Rdata')
topaz21_early <- output.list[[1]]


#merge the two topaz datasets to get full 2021 data
topaz21=merge(topaz21_early, topaz21_late, all=T)







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
weather.data.e.20=weather.data.e[with(weather.data.e, Datetime > "2020-05-02 00:00:00" & Datetime < "2020-10-30 00:00:00"),]
weather.data.e.21=weather.data.e[with(weather.data.e, Datetime > "2021-05-02 00:00:00" & Datetime < "2021-10-30 00:00:00"),]

weather.data.e.22=read.csv("Emerald_MET_data_2019_2023_hourly.csv",header=TRUE)
weather.data.e.22$Date = as.Date(weather.data.e.22$Datetime , format='%Y-%m-%d')
weather.data.e.22$DOY = yday(weather.data.e.22$Date)
weather.data.e.22$Datetime=as.POSIXct(weather.data.e.22$Datetime, format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
weather.data.e.22$SW_solar=weather.data.e.22$SW_solar * -1
# weather.data.e.22=weather.data.e.22[with(weather.data.e.22, Date > "2022-04-02" & Date < "2022-10-30"),]
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
weather.data.t.20=weather.data.t[with(weather.data.t, Datetime > "2020-05-02 00:00:00" & Datetime < "2020-10-30 00:00:00"),]
weather.data.t.21=weather.data.t[with(weather.data.t, Datetime > "2021-05-02 00:00:00" & Datetime < "2021-10-30 00:00:00"),]

weather.data.t.22=read.csv("Topaz_MET_data_2019_2023_hourly.csv",header=TRUE)
weather.data.t.22$Date = as.Date(weather.data.t.22$Datetime , format='%m/%d/%Y')
weather.data.t.22$DOY = yday(weather.data.t.22$Date)
weather.data.t.22$Datetime=as.POSIXct(weather.data.t.22$Datetime, format='%m/%d/%Y %H:%M', tz="Etc/GMT-8")
weather.data.t.22$SW_solar=weather.data.t.22$SW_solar * -1
weather.data.t.22=weather.data.t.22[with(weather.data.t.22, Date > "2022-05-02" & Date < "2022-10-30"),]
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

















############ emerald lake 2021 heat content ###############################
library(rLakeAnalyzer)
#get lake bathymetry
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd)

hypso <- read.csv("Emerald_hypsography.csv",header=TRUE)
#format bathymetry dataframe for use with LakeAnalyzer functions
bathy <- data.frame(depths=hypso$Depth_m,areas=hypso$SA_m2)
bathy <- bathy[bathy$areas>0,]

#load lake data
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd) 
files <- list.files()

load('Emerald_Topaz_sensor_data_2021_forMJ.Rdata')
emerald21= ts.2021.forMJ  [[1]]


# Create a sequence of times from "00:00:00" to "23:00:00" with 1-hour increments
time_sequence <- seq(from = as.POSIXct("00:00:00", format = "%H:%M:%S"),
                     to = as.POSIXct("23:00:00", format = "%H:%M:%S"),
                     by = "hour")

# Extract only the time portion
time_sequence <- format(time_sequence, format = "%H:%M:%S")

# Repeat the time sequence for the number of rows in the dataframe
repeated_times <- rep(time_sequence, length.out = nrow(emerald21))

# Add the repeated time sequence as a new column to the dataframe
emerald21$time <- repeated_times

emerald21$datetime <- as.POSIXct(paste(emerald21$datetime, emerald21$tim), format = "%Y-%m-%d %H:%M:%S")


#keep only datetime and wtr_
temp.data <- emerald21 %>%
  select(datetime, starts_with("wtr_"))

#Calculate whole lake heat content (internal energy in Joules); wtr is a dataframe of water , with columns 'datetime', 'wtr_1', 'wtr_2' etc.. corresponding to the depth (m) of the measurement
WL.heat <- ts.internal.energy(wtr=temp.data,bathy=bathy,na.rm = FALSE)#[,2]

##########WL.heat is hourly lake heat content; calculate daily gain directly from this ############
# Extract the date and hour from the datetime column
WL.heat <- WL.heat %>%
  mutate(date = as.Date(datetime),
         hour = hour(datetime))

# Find the minimum heat content for each day, considering only the morning hours (before noon)
daily_min_heat <- WL.heat %>%
  filter(hour <= 12) %>%
  group_by(date) %>%
  summarize(min_heat = min(internal.energy))

#find the maximum heat for each day
daily_max_heat <- WL.heat %>%
  group_by(date) %>%
  summarize(max_heat = max(internal.energy))

#average daily heat content of entire day
daily_heat_content <- WL.heat %>%
  group_by(date) %>%
  summarize(mean_heat = mean(internal.energy))

daily_heat = merge(daily_max_heat, daily_min_heat, all=T)
daily_heat$heatgain = daily_heat$max_heat - daily_heat$min_heat

daily_heat = merge(daily_heat, daily_heat_content, all=T)

heat_EMERALD_21= daily_heat
heat_EMERALD_21$SiteName="EMERALD"

# Create the ggplot object
heat_gain_plot <- ggplot(daily_heat, aes(x = date, y = heatgain)) +
  geom_line() +  # Add a line plot
  geom_point() +  # Add points
  labs(x = "Date", y = "Heat Gain") +  # Label axes
  ggtitle("Daily Heat Gain") +  # Add a title
  theme_bw() +  # Apply a theme
  theme(panel.grid = element_blank())  # Remove gridlines
heat_gain_plot

# mean_heat_plot <- ggplot( )+
#   geom_line(data=daily_heat, aes(x = date, y = mean_heat))+
#                             geom_rect(data = smoke_eml_21,
#                                       aes(xmin = Datetime, xmax = lead(Datetime), fill = Smoke.day),
#                                       ymin = 0, ymax = Inf, alpha = 0.5) +
#                             scale_fill_manual(name = "Smoke Day", values = c( "n" = "white","y" = "#fdbb84"), labels = c("No", "Yes"))+
#                             
#   labs(x = "Date", y = "Mean Heat") +  # Label axes
#   ggtitle("Daily mean heat content") +  # Add a title
#   theme_bw() +  # Apply a theme
#   theme(panel.grid = element_blank())  # Remove gridlines
#mean_heat_plot


# max_heat_plot <- ggplot() +
#   geom_line(data=daily_heat, aes(x = date, y = max_heat))+
#   geom_rect(data = smoke_eml_21,
#             aes(xmin = Datetime, xmax = lead(Datetime), fill = Smoke.day),
#             ymin = 0, ymax = Inf, alpha = 0.5) +
#   scale_fill_manual(name = "Smoke Day", values = c( "n" = "white","y" = "#fdbb84"), labels = c("No", "Yes"))+
# 
#   labs(x = "Date", y = "max Heat") +  # Label axes
#   ggtitle("Daily max heat content") +  # Add a title
#   theme_bw() +  # Apply a theme
#   theme(panel.grid = element_blank())  # Remove gridlines
#max_heat_plot

# Convert Date column to POSIXct class by adding a dummy time component
# daily_heat$date <- as.POSIXct(paste(daily_heat$date, "00:00:00"))
# 
# 
# EML_HEAT=ggplot()+
#   geom_rect(data = smoke_eml_21,
#             aes(xmin = Datetime, xmax = lead(Datetime), fill = Smoke.day),
#             ymin = 0, ymax = Inf, alpha = 0.5) +
#   scale_fill_manual(name = "Smoke Day", values = c( "n" = "white","y" = "#fdbb84"), labels = c("No", "Yes"))+
#   geom_line(data = avg.sw.e.21, aes(x = Date, y = swrad*100000), color = "gray") +
#   geom_line(data=daily_heat, aes(x=date, y = heatgain), colour="black")+
#   geom_point(data=daily_heat, aes(x=date, y = heatgain), colour="black")+
#     scale_y_continuous(
#       name = "heat gain J (daily max heat content - daily min before noon)",
#   
#     sec.axis = sec_axis(~ ./100000, name = "swrad")) +
#   labs(x="Date")+
#   theme_classic()+
#   theme(panel.background = element_rect(fill='transparent'))
# EML_HEAT













############ topaz lake 2021 heat content ###############################
library(rLakeAnalyzer)
#get lake bathymetry
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd)

hypso <- read.csv("Topaz_hypsography.csv",header=TRUE)
#format bathymetry dataframe for use with LakeAnalyzer functions
bathy <- data.frame(depths=hypso$Depth_m,areas=hypso$SA_m2)
bathy <- bathy[bathy$areas>0,]

#load lake data
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd) 
files <- list.files()

load('Emerald_Topaz_sensor_data_2021_forMJ.Rdata')
topaz21= ts.2021.forMJ  [[2]]

topaz21=topaz21[with(topaz21, datetime >= "2021-08-21 00:00:00" ),]

# Create a sequence of times from "00:00:00" to "23:00:00" with 1-hour increments
time_sequence <- seq(from = as.POSIXct("00:00:00", format = "%H:%M:%S"),
                     to = as.POSIXct("23:00:00", format = "%H:%M:%S"),
                     by = "hour")

# Extract only the time portion
time_sequence <- format(time_sequence, format = "%H:%M:%S")

# Repeat the time sequence for the number of rows in the dataframe
repeated_times <- rep(time_sequence, length.out = nrow(topaz21))

# Add the repeated time sequence as a new column to the dataframe
topaz21$time <- repeated_times

topaz21$datetime <- as.POSIXct(paste(topaz21$datetime, topaz21$tim), format = "%Y-%m-%d %H:%M:%S")


#keep only datetime and wtr_
temp.data <- topaz21 %>%
  select(datetime, starts_with("wtr_"))

#Calculate whole lake heat content (internal energy in Joules); wtr is a dataframe of water , with columns 'datetime', 'wtr_1', 'wtr_2' etc.. corresponding to the depth (m) of the measurement
WL.heat <- ts.internal.energy(wtr=temp.data,bathy=bathy,na.rm = FALSE)#[,2]

##########WL.heat is hourly lake heat content; calculate daily gain directly from this ############
# Extract the date and hour from the datetime column
WL.heat <- WL.heat %>%
  mutate(date = as.Date(datetime),
         hour = hour(datetime))

# Find the minimum heat content for each day, considering only the morning hours (before noon)
daily_min_heat <- WL.heat %>%
  filter(hour <= 12) %>%
  group_by(date) %>%
  summarize(min_heat = min(internal.energy))

#find the maximum heat for each day
daily_max_heat <- WL.heat %>%
  group_by(date) %>%
  summarize(max_heat = max(internal.energy))

#average daily heat content of entire day
daily_heat_content <- WL.heat %>%
  group_by(date) %>%
  summarize(mean_heat = mean(internal.energy))

daily_heat = merge(daily_max_heat, daily_min_heat, all=T)
daily_heat$heatgain = daily_heat$max_heat - daily_heat$min_heat

daily_heat = merge(daily_heat, daily_heat_content, all=T)

heat_TOPAZ_21= daily_heat
heat_TOPAZ_21$SiteName="TOPAZ"















######################## EMLPOND1 ######################################

#No bathymetry for ponds, so use inverted cone method
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd)

# hypso <- read.csv("Emerald_hypsography.csv",header=TRUE)
# #format bathymetry dataframe for use with LakeAnalyzer functions
# bathy <- data.frame(depths=hypso$Depth_m,areas=hypso$SA_m2)
# bathy <- bathy[bathy$areas>0,]
# 

# #For sites lacking bathymetry, estimate using surface area and max depth
#site attributes
setwd("C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data")
site_attributes <- read.csv('All_PondCharacteristics_2020_2022.csv',header=TRUE)
site_attributes <- filter(site_attributes, Lake == "EMLPOND1" & Year == 2021)
LakeArea <- site_attributes$Lake_Area_m2_NG[site_attributes$Lake=="EMLPOND1"] #m^2
MaxDepth <- site_attributes$Max_Depth_m_NG[site_attributes$Lake=="EMLPOND1"] #m^2


bathy <-approx.bathy(Zmax=MaxDepth,lkeArea=LakeArea,zinterval=0.1)#use cone method if no Zmean is known
colnames(bathy)[colnames(bathy) == "Area.at.z"] <- "areas"


#load lake data
head(ts.data.EMLPOND1.21)


#keep only datetime and wtr_
temp.data <- ts.data.EMLPOND1.21 %>%
  select(datetime, starts_with("wtr_"))

head(temp.data)

#Calculate whole lake heat content (internal energy in Joules); wtr is a dataframe of water , with columns 'datetime', 'wtr_1', 'wtr_2' etc.. corresponding to the depth (m) of the measurement
WL.heat <- ts.internal.energy(wtr=temp.data,bathy=bathy, na.rm=F)



##########WL.heat is hourly lake heat content; calculate daily gain directly from this ############
# Extract the date and hour from the datetime column
WL.heat <- WL.heat %>%
  mutate(date = as.Date(datetime),
         hour = hour(datetime))

# Find the minimum heat content for each day, considering only the morning hours (before noon)
daily_min_heat <- WL.heat %>%
  filter(hour <= 12) %>%
  group_by(date) %>%
  summarize(min_heat = min(internal.energy))

#find the maximum heat for each day
daily_max_heat <- WL.heat %>%
  group_by(date) %>%
  summarize(max_heat = max(internal.energy))

#average daily heat content of entire day
daily_heat_content <- WL.heat %>%
  group_by(date) %>%
  summarize(mean_heat = mean(internal.energy))

daily_heat = merge(daily_max_heat, daily_min_heat, all=T)
daily_heat$heatgain = daily_heat$max_heat - daily_heat$min_heat

daily_heat = merge(daily_heat, daily_heat_content, all=T)

heat_EMLPOND1_21=daily_heat
heat_EMLPOND1_21$SiteName="EMLPOND1"

# Create the ggplot object
heat_gain_plot <- ggplot(daily_heat, aes(x = date, y = heatgain)) +
  geom_line() +  # Add a line plot
  geom_point() +  # Add points
  labs(x = "Date", y = "Heat Gain") +  # Label axes
  ggtitle("Daily Heat Gain") +  # Add a title
  theme_bw() +  # Apply a theme
  theme(panel.grid = element_blank())  # Remove gridlines
heat_gain_plot

# mean_heat_plot <- ggplot( )+
#   geom_line(data=daily_heat, aes(x = date, y = mean_heat))+
#   geom_rect(data = smoke_eml_21,
#             aes(xmin = Datetime, xmax = lead(Datetime), fill = Smoke.day),
#             ymin = 0, ymax = Inf, alpha = 0.5) +
#   scale_fill_manual(name = "Smoke Day", values = c( "n" = "white","y" = "#fdbb84"), labels = c("No", "Yes"))+
#   
#   labs(x = "Date", y = "Mean Heat") +  # Label axes
#   ggtitle("Daily mean heat content") +  # Add a title
#   theme_bw() +  # Apply a theme
#   theme(panel.grid = element_blank())  # Remove gridlines
# mean_heat_plot
# 
# 
# max_heat_plot <- ggplot() +
#   geom_line(data=daily_heat, aes(x = date, y = max_heat))+
#   geom_rect(data = smoke_eml_21,
#             aes(xmin = Datetime, xmax = lead(Datetime), fill = Smoke.day),
#             ymin = 0, ymax = Inf, alpha = 0.5) +
#   scale_fill_manual(name = "Smoke Day", values = c( "n" = "white","y" = "#fdbb84"), labels = c("No", "Yes"))+
#   
#   labs(x = "Date", y = "max Heat") +  # Label axes
#   ggtitle("Daily max heat content") +  # Add a title
#   theme_bw() +  # Apply a theme
#   theme(panel.grid = element_blank())  # Remove gridlines
# max_heat_plot
# 
# # Convert Date column to POSIXct class by adding a dummy time component
# daily_heat$date <- as.POSIXct(paste(daily_heat$date, "00:00:00"))
# 
# 
# EML_HEAT=ggplot()+
#   geom_rect(data = smoke_eml_21,
#             aes(xmin = Datetime, xmax = lead(Datetime), fill = Smoke.day),
#             ymin = 0, ymax = Inf, alpha = 0.5) +
#   scale_fill_manual(name = "Smoke Day", values = c( "n" = "white","y" = "#fdbb84"), labels = c("No", "Yes"))+
#   geom_line(data = avg.sw.e.21, aes(x = Date, y = swrad*100000), color = "gray") +
#   geom_line(data=daily_heat, aes(x=date, y = heatgain), colour="black")+
#   geom_point(data=daily_heat, aes(x=date, y = heatgain), colour="black")+
#   scale_y_continuous(
#     name = "heat gain J (daily max heat content - daily min before noon)",
#     
#     sec.axis = sec_axis(~ ./100000, name = "swrad")) +
#   labs(x="Date")+
#   theme_classic()+
#   theme(panel.background = element_rect(fill='transparent'))
# EML_HEAT




######################## TOK11 ######################################

#No bathymetry for ponds, so use inverted cone method
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd)


# #For sites lacking bathymetry, estimate using surface area and max depth
#site attributes
setwd("C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data")
site_attributes <- read.csv('All_PondCharacteristics_2020_2022.csv',header=TRUE)
site_attributes <- filter(site_attributes, Lake == "TOK11" & Year == 2021)
LakeArea <- site_attributes$Lake_Area_m2_NG[site_attributes$Lake=="TOK11"] #m^2
MaxDepth <- site_attributes$Max_Depth_m_NG[site_attributes$Lake=="TOK11"] #m^2


bathy <-approx.bathy(Zmax=MaxDepth,lkeArea=LakeArea,zinterval=0.1)#use cone method if no Zmean is known
colnames(bathy)[colnames(bathy) == "Area.at.z"] <- "areas"


#load lake data
head(ts.data.TOK11.21)


#keep only datetime and wtr_
temp.data <- ts.data.TOK11.21 %>%
  select(datetime, starts_with("wtr_"))

head(temp.data)

#Calculate whole lake heat content (internal energy in Joules); wtr is a dataframe of water , with columns 'datetime', 'wtr_1', 'wtr_2' etc.. corresponding to the depth (m) of the measurement
WL.heat <- ts.internal.energy(wtr=temp.data,bathy=bathy, na.rm=F)



##########WL.heat is hourly lake heat content; calculate daily gain directly from this ############
# Extract the date and hour from the datetime column
WL.heat <- WL.heat %>%
  mutate(date = as.Date(datetime),
         hour = hour(datetime))

# Find the minimum heat content for each day, considering only the morning hours (before noon)
daily_min_heat <- WL.heat %>%
  filter(hour <= 12) %>%
  group_by(date) %>%
  summarize(min_heat = min(internal.energy))

#find the maximum heat for each day
daily_max_heat <- WL.heat %>%
  group_by(date) %>%
  summarize(max_heat = max(internal.energy))

#average daily heat content of entire day
daily_heat_content <- WL.heat %>%
  group_by(date) %>%
  summarize(mean_heat = mean(internal.energy))

daily_heat = merge(daily_max_heat, daily_min_heat, all=T)
daily_heat$heatgain = daily_heat$max_heat - daily_heat$min_heat

daily_heat = merge(daily_heat, daily_heat_content, all=T)

heat_TOK11_21=daily_heat
heat_TOK11_21$SiteName="TOK11"





######################## TOPAZPOND ######################################

#No bathymetry for ponds, so use inverted cone method
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd)


# #For sites lacking bathymetry, estimate using surface area and max depth
#site attributes
setwd("C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data")
site_attributes <- read.csv('All_PondCharacteristics_2020_2022.csv',header=TRUE)
site_attributes <- filter(site_attributes, Lake == "TOPAZPOND" & Year == 2021)
LakeArea <- site_attributes$Lake_Area_m2_NG[site_attributes$Lake=="TOPAZPOND"] #m^2
MaxDepth <- site_attributes$Max_Depth_m_NG[site_attributes$Lake=="TOPAZPOND"] #m^2


bathy <-approx.bathy(Zmax=MaxDepth,lkeArea=LakeArea,zinterval=0.1)#use cone method if no Zmean is known
colnames(bathy)[colnames(bathy) == "Area.at.z"] <- "areas"


#load lake data
head(ts.data.TOPAZPOND.21)


#keep only datetime and wtr_
temp.data <- ts.data.TOPAZPOND.21 %>%
  select(datetime, starts_with("wtr_"))

head(temp.data)

#Calculate whole lake heat content (internal energy in Joules); wtr is a dataframe of water , with columns 'datetime', 'wtr_1', 'wtr_2' etc.. corresponding to the depth (m) of the measurement
WL.heat <- ts.internal.energy(wtr=temp.data,bathy=bathy, na.rm=F)



##########WL.heat is hourly lake heat content; calculate daily gain directly from this ############
# Extract the date and hour from the datetime column
WL.heat <- WL.heat %>%
  mutate(date = as.Date(datetime),
         hour = hour(datetime))

# Find the minimum heat content for each day, considering only the morning hours (before noon)
daily_min_heat <- WL.heat %>%
  filter(hour <= 12) %>%
  group_by(date) %>%
  summarize(min_heat = min(internal.energy))

#find the maximum heat for each day
daily_max_heat <- WL.heat %>%
  group_by(date) %>%
  summarize(max_heat = max(internal.energy))

#average daily heat content of entire day
daily_heat_content <- WL.heat %>%
  group_by(date) %>%
  summarize(mean_heat = mean(internal.energy))

daily_heat = merge(daily_max_heat, daily_min_heat, all=T)
daily_heat$heatgain = daily_heat$max_heat - daily_heat$min_heat

daily_heat = merge(daily_heat, daily_heat_content, all=T)

heat_TOPAZPOND_21=daily_heat
heat_TOPAZPOND_21$SiteName="TOPAZPOND"






######################## TOK30 ######################################

#No bathymetry for ponds, so use inverted cone method
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd)


# #For sites lacking bathymetry, estimate using surface area and max depth
#site attributes
setwd("C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data")
site_attributes <- read.csv('All_PondCharacteristics_2020_2022.csv',header=TRUE)
site_attributes <- filter(site_attributes, Lake == "TOK30" & Year == 2021)
LakeArea <- site_attributes$Lake_Area_m2_NG[site_attributes$Lake=="TOK30"] #m^2
MaxDepth <- site_attributes$Max_Depth_m_NG[site_attributes$Lake=="TOK30"] #m^2


bathy <-approx.bathy(Zmax=MaxDepth,lkeArea=LakeArea,zinterval=0.1)#use cone method if no Zmean is known
colnames(bathy)[colnames(bathy) == "Area.at.z"] <- "areas"


#load lake data
head(ts.data.TOK30.21)

#keep only datetime and wtr_
temp.data <- ts.data.TOK30.21 %>%
  select(datetime, starts_with("wtr_"))

head(temp.data)

#Calculate whole lake heat content (internal energy in Joules); wtr is a dataframe of water , with columns 'datetime', 'wtr_1', 'wtr_2' etc.. corresponding to the depth (m) of the measurement
WL.heat <- ts.internal.energy(wtr=temp.data,bathy=bathy, na.rm=F)



##########WL.heat is hourly lake heat content; calculate daily gain directly from this ############
# Extract the date and hour from the datetime column
WL.heat <- WL.heat %>%
  mutate(date = as.Date(datetime),
         hour = hour(datetime))

# Find the minimum heat content for each day, considering only the morning hours (before noon)
daily_min_heat <- WL.heat %>%
  filter(hour <= 12) %>%
  group_by(date) %>%
  summarize(min_heat = min(internal.energy))

#find the maximum heat for each day
daily_max_heat <- WL.heat %>%
  group_by(date) %>%
  summarize(max_heat = max(internal.energy))

#average daily heat content of entire day
daily_heat_content <- WL.heat %>%
  group_by(date) %>%
  summarize(mean_heat = mean(internal.energy))

daily_heat = merge(daily_max_heat, daily_min_heat, all=T)
daily_heat$heatgain = daily_heat$max_heat - daily_heat$min_heat

daily_heat = merge(daily_heat, daily_heat_content, all=T)

heat_TOK30_21=daily_heat
heat_TOK30_21$SiteName="TOK30"




##################################### merge 2021 ##############################################
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd) 
files <- list.files()#all the files in wd

load('SmokeData_forMJ.Rdata')
smoke_topaz_20 = SmokeData_forMJ [[1]]
smoke_eml_20 = SmokeData_forMJ [[2]]
smoke_topaz_21 = SmokeData_forMJ [[3]]
smoke_eml_21 = SmokeData_forMJ [[4]]

smoke_eml_21 <- smoke_eml_21[, c("Date", "Smoke.day", "smoke.density")]
colnames(smoke_eml_21)[colnames(smoke_eml_21) == "Date"] <- "date"

heat_EMLPOND1_21=merge(heat_EMLPOND1_21, smoke_eml_21, by="date")
heat_TOK11_21=merge(heat_TOK11_21, smoke_eml_21, by="date")
heat_EMERALD_21=merge(heat_EMERALD_21, smoke_eml_21, by="date")

smoke_topaz_21 <- smoke_topaz_21[, c("Date", "Smoke.day", "smoke.density")]
colnames(smoke_topaz_21)[colnames(smoke_topaz_21) == "Date"] <- "date"

heat_TOPAZ_21=merge(heat_TOPAZ_21, smoke_topaz_21, by="date")
heat_TOK30_21=merge(heat_TOK30_21, smoke_topaz_21, by="date")
heat_TOPAZPOND_21=merge(heat_TOPAZPOND_21, smoke_topaz_21, by="date")

heat_2021=list(heat_EMLPOND1_21, heat_TOK11_21, heat_TOPAZPOND_21, heat_TOK30_21, heat_EMERALD_21, heat_TOPAZ_21)

# Merge all dataframes in the list by 'id' column
heat_2021 <- Reduce(function(x, y) merge(x, y, all = TRUE), heat_2021)


##################### plot all together ################################
# Convert Smoke.day to a factor 
heat_2021$Smoke.day <- factor(heat_2021$Smoke.day, levels = c("n", "y"))

# Boxplot of mean_heat by Smoke.day
boxplot(mean_heat ~ Smoke.day, data = heat_2021, 
        xlab = "Smoke Day", ylab = "Mean Heat",
        main = "Boxplot of Mean Heat by Smoke Day")

# Boxplot of heatgain by Smoke.day
boxplot(heatgain ~ Smoke.day, data = heat_2021, 
        xlab = "Smoke Day", ylab = "Heat Gain",
        main = "Boxplot of daily heat gain by Smoke Day")


#density plot
ggplot(heat_2021, aes(x = mean_heat, fill = Smoke.day)) +
  geom_density(alpha = 0.5) +
  labs(x = "Mean Heat", y = "Density", fill = "Smoke Day") +
  ggtitle("Density Plot of Mean Heat by Smoke Day")


#density plot
ggplot(heat_2021, aes(x = heatgain, fill = Smoke.day)) +
  geom_density(alpha = 0.5) +
  labs(x = "heatgain", y = "Density", fill = "Smoke Day") +
  ggtitle("Density Plot of heat gain by Smoke Day")

#by site
ggplot(heat_2021, aes(x = mean_heat, fill = Smoke.day)) +
  geom_density(alpha = 0.5) +
  labs(x = "Mean Heat", y = "Density", fill = "Smoke Day") +
  ggtitle("Density Plot of Mean Heat by Smoke Day") +
  facet_wrap(~ SiteName, ncol = 2) +
  theme_minimal()


#by site
ggplot(heat_2021, aes(x = heatgain, fill = Smoke.day)) +
  geom_density(alpha = 0.5) +
  labs(x = "heat gain", y = "Density", fill = "Smoke Day") +
  ggtitle("Density Plot of heat gain by Smoke Day") +
  facet_wrap(~ SiteName, ncol = 2) +
  theme_minimal()


# Calculate median for each SiteName and Smoke.day combination, mean heat
median_heat <- heat_2021 %>%
  group_by(SiteName, Smoke.day) %>%
  summarize(median_mean_heat = median(mean_heat))


# Calculate median for each SiteName and Smoke.day combination
mean_heat <- heat_2021 %>%
  group_by(SiteName, Smoke.day) %>%
  summarize(overall_mean_heat = mean(mean_heat))

# Calculate difference between median heat for Smoke.day y and n
median_diff <- median_heat %>%
  group_by(SiteName) %>%
  summarize(median_diff = median_mean_heat[Smoke.day == "y"] - median_mean_heat[Smoke.day == "n"])
# Calculate difference between median heat for Smoke.day y and n
mean_diff <- mean_heat %>%
  group_by(SiteName) %>%
  summarize(mean_diff = overall_mean_heat[Smoke.day == "y"] - overall_mean_heat[Smoke.day == "n"])

median_diff$SiteName <- factor(median_diff$SiteName, levels = c("EMERALD", "TOPAZ", "EMLPOND1", "TOK11", "TOPAZPOND", "TOK30"))
mean_diff$SiteName <- factor(mean_diff$SiteName, levels = c("EMERALD", "TOPAZ", "EMLPOND1", "TOK11", "TOPAZPOND", "TOK30"))

ggplot(median_diff, aes(x = SiteName, y = median_diff)) +
  geom_point(size = 3) +
  labs(x = "SiteName", y = "Median Difference") +
  ggtitle("Median Difference in mean Heat (Smoke day y - Smoke day n)")

ggplot(mean_diff, aes(x = SiteName, y = mean_diff)) +
  geom_point(size = 3) +
  labs(x = "SiteName", y = "Mean Difference") +
  ggtitle("Mean Difference in mean Heat (Smoke day y - Smoke day n)")


median_diff_2021=median_diff
mean_diff_2021=mean_diff

median_diff_2021$year=2021
mean_diff_2021$year=2021

########same but with heat gain###############
# Calculate median for each SiteName and Smoke.day combination,  heat gain
median_heatgain <- heat_2021 %>%
  group_by(SiteName, Smoke.day) %>%
  summarize(median_heatgain = median(heatgain))


# Calculate difference between median heat for Smoke.day y and n
median_diff_heatgain <- median_heatgain %>%
  group_by(SiteName) %>%
  summarize(median_diff_heatgain = median_heatgain[Smoke.day == "y"] - median_heatgain[Smoke.day == "n"])

median_diff_heatgain$SiteName <- factor(median_diff_heatgain$SiteName, levels = c("EMERALD", "TOPAZ", "EMLPOND1", "TOK11", "TOPAZPOND", "TOK30"))

ggplot(median_diff_heatgain, aes(x = SiteName, y = median_diff_heatgain)) +
  geom_point(size = 3) +
  labs(x = "SiteName", y = "Median Difference") +
  ggtitle("Median Difference in heat gain (Smoke day y - Smoke day n)")



median_diff_heatgain_2021=median_diff_heatgain
mean_diff_heatgain_2021=median_diff_heatgain

median_diff_heatgain_2021$year=2021
mean_diff_heatgain_2021$year=2021























######################### same for 2020########################################################

############ emerald lake 2020 heat content ###############################
library(rLakeAnalyzer)
#get lake bathymetry
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd)

hypso <- read.csv("Emerald_hypsography.csv",header=TRUE)
#format bathymetry dataframe for use with LakeAnalyzer functions
bathy <- data.frame(depths=hypso$Depth_m,areas=hypso$SA_m2)
bathy <- bathy[bathy$areas>0,]

#load lake data
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd) 
files <- list.files()

load('Emerald_Topaz_sensor_data_2020_forMJ.Rdata')
emerald20= ts.2020.forMJ  [[1]]


# Create a sequence of times from "00:00:00" to "23:00:00" with 1-hour increments
time_sequence <- seq(from = as.POSIXct("00:00:00", format = "%H:%M:%S"),
                     to = as.POSIXct("23:00:00", format = "%H:%M:%S"),
                     by = "hour")

# Extract only the time portion
time_sequence <- format(time_sequence, format = "%H:%M:%S")

# Repeat the time sequence for the number of rows in the dataframe
repeated_times <- rep(time_sequence, length.out = nrow(emerald20))

# Add the repeated time sequence as a new column to the dataframe
emerald20$time <- repeated_times

emerald20$datetime <- as.POSIXct(paste(emerald20$datetime, emerald20$tim), format = "%Y-%m-%d %H:%M:%S")


#keep only datetime and wtr_
temp.data <- emerald20 %>%
  select(datetime, starts_with("wtr_"))

#Calculate whole lake heat content (internal energy in Joules); wtr is a dataframe of water , with columns 'datetime', 'wtr_1', 'wtr_2' etc.. corresponding to the depth (m) of the measurement
WL.heat <- ts.internal.energy(wtr=temp.data,bathy=bathy,na.rm = FALSE)#[,2]

##########WL.heat is hourly lake heat content; calculate daily gain directly from this ############
# Extract the date and hour from the datetime column
WL.heat <- WL.heat %>%
  mutate(date = as.Date(datetime),
         hour = hour(datetime))

# Find the minimum heat content for each day, considering only the morning hours (before noon)
daily_min_heat <- WL.heat %>%
  filter(hour <= 12) %>%
  group_by(date) %>%
  summarize(min_heat = min(internal.energy))

#find the maximum heat for each day
daily_max_heat <- WL.heat %>%
  group_by(date) %>%
  summarize(max_heat = max(internal.energy))

#average daily heat content of entire day
daily_heat_content <- WL.heat %>%
  group_by(date) %>%
  summarize(mean_heat = mean(internal.energy))

daily_heat = merge(daily_max_heat, daily_min_heat, all=T)
daily_heat$heatgain = daily_heat$max_heat - daily_heat$min_heat

daily_heat = merge(daily_heat, daily_heat_content, all=T)

heat_EMERALD_20= daily_heat
heat_EMERALD_20$SiteName="EMERALD"

# Create the ggplot object
heat_gain_plot <- ggplot(daily_heat, aes(x = date, y = heatgain)) +
  geom_line() +  # Add a line plot
  geom_point() +  # Add points
  labs(x = "Date", y = "Heat Gain") +  # Label axes
  ggtitle("Daily Heat Gain") +  # Add a title
  theme_bw() +  # Apply a theme
  theme(panel.grid = element_blank())  # Remove gridlines
heat_gain_plot

# mean_heat_plot <- ggplot( )+
#   geom_line(data=daily_heat, aes(x = date, y = mean_heat))+
#   geom_rect(data = smoke_eml_20,
#             aes(xmin = Datetime, xmax = lead(Datetime), fill = Smoke.day),
#             ymin = 0, ymax = Inf, alpha = 0.5) +
#   scale_fill_manual(name = "Smoke Day", values = c( "n" = "white","y" = "#fdbb84"), labels = c("No", "Yes"))+
#   
#   labs(x = "Date", y = "Mean Heat") +  # Label axes
#   ggtitle("Daily mean heat content") +  # Add a title
#   theme_bw() +  # Apply a theme
#   theme(panel.grid = element_blank())  # Remove gridlines
# mean_heat_plot


# max_heat_plot <- ggplot() +
#   geom_line(data=daily_heat, aes(x = date, y = max_heat))+
#   geom_rect(data = smoke_eml_20,
#             aes(xmin = Datetime, xmax = lead(Datetime), fill = Smoke.day),
#             ymin = 0, ymax = Inf, alpha = 0.5) +
#   scale_fill_manual(name = "Smoke Day", values = c( "n" = "white","y" = "#fdbb84"), labels = c("No", "Yes"))+
#   
#   labs(x = "Date", y = "max Heat") +  # Label axes
#   ggtitle("Daily max heat content") +  # Add a title
#   theme_bw() +  # Apply a theme
#   theme(panel.grid = element_blank())  # Remove gridlines
# max_heat_plot

# Convert Date column to POSIXct class by adding a dummy time component
daily_heat$date <- as.POSIXct(paste(daily_heat$date, "00:00:00"))


EML_HEAT=ggplot()+
  geom_rect(data = smoke_eml_20,
            aes(xmin = Datetime, xmax = lead(Datetime), fill = Smoke.day),
            ymin = 0, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "white","y" = "#fdbb84"), labels = c("No", "Yes"))+
  geom_line(data = avg.sw.e.20, aes(x = Date, y = swrad*100000), color = "gray") +
  geom_line(data=daily_heat, aes(x=date, y = heatgain), colour="black")+
  geom_point(data=daily_heat, aes(x=date, y = heatgain), colour="black")+
  scale_y_continuous(
    name = "heat gain J (daily max heat content - daily min before noon)",
    
    sec.axis = sec_axis(~ ./100000, name = "swrad")) +
  labs(x="Date")+
  theme_classic()+
  theme(panel.background = element_rect(fill='transparent'))
EML_HEAT













############ topaz lake 2020 heat content ###############################
library(rLakeAnalyzer)
#get lake bathymetry
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd)

hypso <- read.csv("Topaz_hypsography.csv",header=TRUE)
#format bathymetry dataframe for use with LakeAnalyzer functions
bathy <- data.frame(depths=hypso$Depth_m,areas=hypso$SA_m2)
bathy <- bathy[bathy$areas>0,]

#load lake data
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd) 
files <- list.files()

load('Emerald_Topaz_sensor_data_2020_forMJ.Rdata')
topaz20= ts.2020.forMJ  [[2]]

topaz20=topaz20[with(topaz20, datetime >= "2020-08-21 00:00:00" ),]

# Create a sequence of times from "00:00:00" to "23:00:00" with 1-hour increments
time_sequence <- seq(from = as.POSIXct("00:00:00", format = "%H:%M:%S"),
                     to = as.POSIXct("23:00:00", format = "%H:%M:%S"),
                     by = "hour")

# Extract only the time portion
time_sequence <- format(time_sequence, format = "%H:%M:%S")

# Repeat the time sequence for the number of rows in the dataframe
repeated_times <- rep(time_sequence, length.out = nrow(topaz20))

# Add the repeated time sequence as a new column to the dataframe
topaz20$time <- repeated_times

topaz20$datetime <- as.POSIXct(paste(topaz20$datetime, topaz20$tim), format = "%Y-%m-%d %H:%M:%S")


#keep only datetime and wtr_
temp.data <- topaz20 %>%
  select(datetime, starts_with("wtr_"))

#Calculate whole lake heat content (internal energy in Joules); wtr is a dataframe of water , with columns 'datetime', 'wtr_1', 'wtr_2' etc.. corresponding to the depth (m) of the measurement
WL.heat <- ts.internal.energy(wtr=temp.data,bathy=bathy,na.rm = FALSE)#[,2]

##########WL.heat is hourly lake heat content; calculate daily gain directly from this ############
# Extract the date and hour from the datetime column
WL.heat <- WL.heat %>%
  mutate(date = as.Date(datetime),
         hour = hour(datetime))

# Find the minimum heat content for each day, considering only the morning hours (before noon)
daily_min_heat <- WL.heat %>%
  filter(hour <= 12) %>%
  group_by(date) %>%
  summarize(min_heat = min(internal.energy))

#find the maximum heat for each day
daily_max_heat <- WL.heat %>%
  group_by(date) %>%
  summarize(max_heat = max(internal.energy))

#average daily heat content of entire day
daily_heat_content <- WL.heat %>%
  group_by(date) %>%
  summarize(mean_heat = mean(internal.energy))

daily_heat = merge(daily_max_heat, daily_min_heat, all=T)
daily_heat$heatgain = daily_heat$max_heat - daily_heat$min_heat

daily_heat = merge(daily_heat, daily_heat_content, all=T)

heat_TOPAZ_20= daily_heat
heat_TOPAZ_20$SiteName="TOPAZ"













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




########### TOK30
###load pond data - TOK30
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


######################## EMLPOND1 ######################################

#No bathymetry for ponds, so use inverted cone method
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd)

# hypso <- read.csv("Emerald_hypsography.csv",header=TRUE)
# #format bathymetry dataframe for use with LakeAnalyzer functions
# bathy <- data.frame(depths=hypso$Depth_m,areas=hypso$SA_m2)
# bathy <- bathy[bathy$areas>0,]
# 

# #For sites lacking bathymetry, estimate using surface area and max depth
#site attributes
setwd("C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data")
site_attributes <- read.csv('All_PondCharacteristics_2020_2022.csv',header=TRUE)
site_attributes <- filter(site_attributes, Lake == "EMLPOND1" & Year == 2020)
LakeArea <- site_attributes$Lake_Area_m2_NG[site_attributes$Lake=="EMLPOND1"] #m^2
MaxDepth <- site_attributes$Max_Depth_m_NG[site_attributes$Lake=="EMLPOND1"] #m^2


bathy <-approx.bathy(Zmax=MaxDepth,lkeArea=LakeArea,zinterval=0.1)#use cone method if no Zmean is known
colnames(bathy)[colnames(bathy) == "Area.at.z"] <- "areas"


#load lake data
head(ts.data.EMLPOND1.20)


#keep only datetime and wtr_
temp.data <- ts.data.EMLPOND1.20 %>%
  select(datetime, starts_with("wtr_"))

head(temp.data)

#Calculate whole lake heat content (internal energy in Joules); wtr is a dataframe of water , with columns 'datetime', 'wtr_1', 'wtr_2' etc.. corresponding to the depth (m) of the measurement
WL.heat <- ts.internal.energy(wtr=temp.data,bathy=bathy, na.rm=F)



##########WL.heat is hourly lake heat content; calculate daily gain directly from this ############
# Extract the date and hour from the datetime column
WL.heat <- WL.heat %>%
  mutate(date = as.Date(datetime),
         hour = hour(datetime))

# Find the minimum heat content for each day, considering only the morning hours (before noon)
daily_min_heat <- WL.heat %>%
  filter(hour <= 12) %>%
  group_by(date) %>%
  summarize(min_heat = min(internal.energy))

#find the maximum heat for each day
daily_max_heat <- WL.heat %>%
  group_by(date) %>%
  summarize(max_heat = max(internal.energy))

#average daily heat content of entire day
daily_heat_content <- WL.heat %>%
  group_by(date) %>%
  summarize(mean_heat = mean(internal.energy))

daily_heat = merge(daily_max_heat, daily_min_heat, all=T)
daily_heat$heatgain = daily_heat$max_heat - daily_heat$min_heat

daily_heat = merge(daily_heat, daily_heat_content, all=T)

heat_EMLPOND1_20=daily_heat
heat_EMLPOND1_20$SiteName="EMLPOND1"

# Create the ggplot object
heat_gain_plot <- ggplot(daily_heat, aes(x = date, y = heatgain)) +
  geom_line() +  # Add a line plot
  geom_point() +  # Add points
  labs(x = "Date", y = "Heat Gain") +  # Label axes
  ggtitle("Daily Heat Gain") +  # Add a title
  theme_bw() +  # Apply a theme
  theme(panel.grid = element_blank())  # Remove gridlines
heat_gain_plot



######################## TOK11 ######################################

#No bathymetry for ponds, so use inverted cone method
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd)


# #For sites lacking bathymetry, estimate using surface area and max depth
#site attributes
setwd("C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data")
site_attributes <- read.csv('All_PondCharacteristics_2020_2022.csv',header=TRUE)
site_attributes <- filter(site_attributes, Lake == "TOK11" & Year == 2020)
LakeArea <- site_attributes$Lake_Area_m2_NG[site_attributes$Lake=="TOK11"] #m^2
MaxDepth <- site_attributes$Max_Depth_m_NG[site_attributes$Lake=="TOK11"] #m^2


bathy <-approx.bathy(Zmax=MaxDepth,lkeArea=LakeArea,zinterval=0.1)#use cone method if no Zmean is known
colnames(bathy)[colnames(bathy) == "Area.at.z"] <- "areas"


#load lake data
head(ts.data.TOK11.20)


#keep only datetime and wtr_
temp.data <- ts.data.TOK11.20 %>%
  select(datetime, starts_with("wtr_"))

head(temp.data)

#Calculate whole lake heat content (internal energy in Joules); wtr is a dataframe of water , with columns 'datetime', 'wtr_1', 'wtr_2' etc.. corresponding to the depth (m) of the measurement
WL.heat <- ts.internal.energy(wtr=temp.data,bathy=bathy, na.rm=F)



##########WL.heat is hourly lake heat content; calculate daily gain directly from this ############
# Extract the date and hour from the datetime column
WL.heat <- WL.heat %>%
  mutate(date = as.Date(datetime),
         hour = hour(datetime))

# Find the minimum heat content for each day, considering only the morning hours (before noon)
daily_min_heat <- WL.heat %>%
  filter(hour <= 12) %>%
  group_by(date) %>%
  summarize(min_heat = min(internal.energy))

#find the maximum heat for each day
daily_max_heat <- WL.heat %>%
  group_by(date) %>%
  summarize(max_heat = max(internal.energy))

#average daily heat content of entire day
daily_heat_content <- WL.heat %>%
  group_by(date) %>%
  summarize(mean_heat = mean(internal.energy))

daily_heat = merge(daily_max_heat, daily_min_heat, all=T)
daily_heat$heatgain = daily_heat$max_heat - daily_heat$min_heat

daily_heat = merge(daily_heat, daily_heat_content, all=T)

heat_TOK11_20=daily_heat
heat_TOK11_20$SiteName="TOK11"





######################## TOPAZPOND ######################################

#No bathymetry for ponds, so use inverted cone method
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd)


# #For sites lacking bathymetry, estimate using surface area and max depth
#site attributes
setwd("C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data")
site_attributes <- read.csv('All_PondCharacteristics_2020_2022.csv',header=TRUE)
site_attributes <- filter(site_attributes, Lake == "TOPAZPOND" & Year == 2020)
LakeArea <- site_attributes$Lake_Area_m2_NG[site_attributes$Lake=="TOPAZPOND"] #m^2
MaxDepth <- site_attributes$Max_Depth_m_NG[site_attributes$Lake=="TOPAZPOND"] #m^2


bathy <-approx.bathy(Zmax=MaxDepth,lkeArea=LakeArea,zinterval=0.1)#use cone method if no Zmean is known
colnames(bathy)[colnames(bathy) == "Area.at.z"] <- "areas"


#load lake data
head(ts.data.TOPAZPOND.20)


#keep only datetime and wtr_
temp.data <- ts.data.TOPAZPOND.20 %>%
  select(datetime, starts_with("wtr_"))

head(temp.data)

#Calculate whole lake heat content (internal energy in Joules); wtr is a dataframe of water , with columns 'datetime', 'wtr_1', 'wtr_2' etc.. corresponding to the depth (m) of the measurement
WL.heat <- ts.internal.energy(wtr=temp.data,bathy=bathy, na.rm=F)



##########WL.heat is hourly lake heat content; calculate daily gain directly from this ############
# Extract the date and hour from the datetime column
WL.heat <- WL.heat %>%
  mutate(date = as.Date(datetime),
         hour = hour(datetime))

# Find the minimum heat content for each day, considering only the morning hours (before noon)
daily_min_heat <- WL.heat %>%
  filter(hour <= 12) %>%
  group_by(date) %>%
  summarize(min_heat = min(internal.energy))

#find the maximum heat for each day
daily_max_heat <- WL.heat %>%
  group_by(date) %>%
  summarize(max_heat = max(internal.energy))

#average daily heat content of entire day
daily_heat_content <- WL.heat %>%
  group_by(date) %>%
  summarize(mean_heat = mean(internal.energy))

daily_heat = merge(daily_max_heat, daily_min_heat, all=T)
daily_heat$heatgain = daily_heat$max_heat - daily_heat$min_heat

daily_heat = merge(daily_heat, daily_heat_content, all=T)

heat_TOPAZPOND_20=daily_heat
heat_TOPAZPOND_20$SiteName="TOPAZPOND"






######################## TOK30 ######################################

#No bathymetry for ponds, so use inverted cone method
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd)


# #For sites lacking bathymetry, estimate using surface area and max depth
#site attributes
setwd("C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data")
site_attributes <- read.csv('All_PondCharacteristics_2020_2022.csv',header=TRUE)
site_attributes <- filter(site_attributes, Lake == "TOK30" & Year == 2020)
LakeArea <- site_attributes$Lake_Area_m2_NG[site_attributes$Lake=="TOK30"] #m^2
MaxDepth <- site_attributes$Max_Depth_m_NG[site_attributes$Lake=="TOK30"] #m^2


bathy <-approx.bathy(Zmax=MaxDepth,lkeArea=LakeArea,zinterval=0.1)#use cone method if no Zmean is known
colnames(bathy)[colnames(bathy) == "Area.at.z"] <- "areas"


#load lake data
head(ts.data.TOK30.20)

#keep only datetime and wtr_
temp.data <- ts.data.TOK30.20 %>%
  select(datetime, starts_with("wtr_"))

head(temp.data)

#Calculate whole lake heat content (internal energy in Joules); wtr is a dataframe of water , with columns 'datetime', 'wtr_1', 'wtr_2' etc.. corresponding to the depth (m) of the measurement
WL.heat <- ts.internal.energy(wtr=temp.data,bathy=bathy, na.rm=F)



##########WL.heat is hourly lake heat content; calculate daily gain directly from this ############
# Extract the date and hour from the datetime column
WL.heat <- WL.heat %>%
  mutate(date = as.Date(datetime),
         hour = hour(datetime))

# Find the minimum heat content for each day, considering only the morning hours (before noon)
daily_min_heat <- WL.heat %>%
  filter(hour <= 12) %>%
  group_by(date) %>%
  summarize(min_heat = min(internal.energy))

#find the maximum heat for each day
daily_max_heat <- WL.heat %>%
  group_by(date) %>%
  summarize(max_heat = max(internal.energy))

#average daily heat content of entire day
daily_heat_content <- WL.heat %>%
  group_by(date) %>%
  summarize(mean_heat = mean(internal.energy))

daily_heat = merge(daily_max_heat, daily_min_heat, all=T)
daily_heat$heatgain = daily_heat$max_heat - daily_heat$min_heat

daily_heat = merge(daily_heat, daily_heat_content, all=T)

heat_TOK30_20=daily_heat
heat_TOK30_20$SiteName="TOK30"




##################################### merge 2020 ##############################################
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd) 
files <- list.files()#all the files in wd

load('SmokeData_forMJ.Rdata')
smoke_topaz_20 = SmokeData_forMJ [[1]]
smoke_eml_20 = SmokeData_forMJ [[2]]
smoke_topaz_21 = SmokeData_forMJ [[3]]
smoke_eml_21 = SmokeData_forMJ [[4]]

smoke_eml_20 <- smoke_eml_20[, c("Date", "Smoke.day")]
colnames(smoke_eml_20)[colnames(smoke_eml_20) == "Date"] <- "date"

heat_EMLPOND1_20=merge(heat_EMLPOND1_20, smoke_eml_20, by="date")
heat_TOK11_20=merge(heat_TOK11_20, smoke_eml_20, by="date")
heat_EMERALD_20=merge(heat_EMERALD_20, smoke_eml_20, by="date")

smoke_topaz_20 <- smoke_topaz_20[, c("Date", "Smoke.day")]
colnames(smoke_topaz_20)[colnames(smoke_topaz_20) == "Date"] <- "date"

heat_TOPAZ_20=merge(heat_TOPAZ_20, smoke_topaz_20, by="date")
heat_TOK30_20=merge(heat_TOK30_20, smoke_topaz_20, by="date")
heat_TOPAZPOND_20=merge(heat_TOPAZPOND_20, smoke_topaz_20, by="date")

heat_2020=list(heat_EMLPOND1_20, heat_TOK11_20, heat_TOPAZPOND_20, heat_TOK30_20, heat_EMERALD_20, heat_TOPAZ_20)

# Merge all dataframes in the list by 'id' column
heat_2020 <- Reduce(function(x, y) merge(x, y, all = TRUE), heat_2020)


##################### plot all together ################################
# Convert Smoke.day to a factor 
heat_2020$Smoke.day <- factor(heat_2020$Smoke.day, levels = c("n", "y"))

# Boxplot of mean_heat by Smoke.day
boxplot(mean_heat ~ Smoke.day, data = heat_2020, 
        xlab = "Smoke Day", ylab = "Mean Heat",
        main = "Boxplot of Mean Heat by Smoke Day")

#density plot
ggplot(heat_2020, aes(x = mean_heat, fill = Smoke.day)) +
  geom_density(alpha = 0.5) +
  labs(x = "Mean Heat", y = "Density", fill = "Smoke Day") +
  ggtitle("Density Plot of Mean Heat by Smoke Day")

#by site
ggplot(heat_2020, aes(x = mean_heat, fill = Smoke.day)) +
  geom_density(alpha = 0.5) +
  labs(x = "Mean Heat", y = "Density", fill = "Smoke Day") +
  ggtitle("Density Plot of Mean Heat by Smoke Day") +
  facet_wrap(~ SiteName, ncol = 2) +
  theme_minimal()
###
# Boxplot of heatgain by Smoke.day
boxplot(heatgain ~ Smoke.day, data = heat_2020, 
        xlab = "Smoke Day", ylab = "Mean Heat",
        main = "Boxplot of heatgain by Smoke Day")

#density plot
ggplot(heat_2020, aes(x = heatgain, fill = Smoke.day)) +
  geom_density(alpha = 0.5) +
  labs(x = "heatgain", y = "Density", fill = "Smoke Day") +
  ggtitle("Density Plot of heatgain by Smoke Day")

#by site
ggplot(heat_2020, aes(x = heatgain, fill = Smoke.day)) +
  geom_density(alpha = 0.5) +
  labs(x = "heatgain", y = "Density", fill = "Smoke Day") +
  ggtitle("Density Plot of heatgain by Smoke Day") +
  facet_wrap(~ SiteName, ncol = 2) +
  theme_minimal()

heat_2020=na.omit(heat_2020)

# Calculate median for each SiteName and Smoke.day combination
median_heat <- heat_2020 %>%
  group_by(SiteName, Smoke.day) %>%
  summarize(median_mean_heat = median(mean_heat), na.rm=T)


# Calculate mean for each SiteName and Smoke.day combination
mean_heat <- heat_2020 %>%
  group_by(SiteName, Smoke.day) %>%
  summarize(overall_mean_heat = mean(mean_heat))

# Calculate difference between median heat for Smoke.day y and n
median_diff <- median_heat %>%
  group_by(SiteName) %>%
  summarize(median_diff = median_mean_heat[Smoke.day == "y"] - median_mean_heat[Smoke.day == "n"])
# Calculate difference between mean heat for Smoke.day y and n
mean_diff <- mean_heat %>%
  group_by(SiteName) %>%
  summarize(mean_diff = overall_mean_heat[Smoke.day == "y"] - overall_mean_heat[Smoke.day == "n"])

median_diff$SiteName <- factor(median_diff$SiteName, levels = c("EMERALD", "TOPAZ", "EMLPOND1", "TOK11", "TOPAZPOND", "TOK30"))
mean_diff$SiteName <- factor(mean_diff$SiteName, levels = c("EMERALD", "TOPAZ", "EMLPOND1", "TOK11", "TOPAZPOND", "TOK30"))

ggplot(median_diff, aes(x = SiteName, y = median_diff)) +
  geom_point(size = 3) +
  labs(x = "SiteName", y = "Median Difference") +
  ggtitle("Median Difference in mean Heat (Smoke day y - Smoke day n)")

ggplot(mean_diff, aes(x = SiteName, y = mean_diff)) +
  geom_point(size = 3) +
  labs(x = "SiteName", y = "Mean Difference") +
  ggtitle("Mean Difference in mean Heat (Smoke day y - Smoke day n)")

median_diff_2020=median_diff
mean_diff_2020=mean_diff

median_diff_2020$year=2020
mean_diff_2020$year=2020




########same but with heat gain###############
# Calculate median for each SiteName and Smoke.day combination,  heat gain
median_heatgain <- heat_2020 %>%
  group_by(SiteName, Smoke.day) %>%
  summarize(median_heatgain = median(heatgain))


# Calculate difference between median heat for Smoke.day y and n
median_diff_heatgain <- median_heatgain %>%
  group_by(SiteName) %>%
  summarize(median_diff_heatgain = median_heatgain[Smoke.day == "y"] - median_heatgain[Smoke.day == "n"])

median_diff_heatgain$SiteName <- factor(median_diff_heatgain$SiteName, levels = c("EMERALD", "TOPAZ", "EMLPOND1", "TOK11", "TOPAZPOND", "TOK30"))

ggplot(median_diff_heatgain, aes(x = SiteName, y = median_diff_heatgain)) +
  geom_point(size = 3) +
  labs(x = "SiteName", y = "Median Difference") +
  ggtitle("Median Difference in heat gain (Smoke day y - Smoke day n)")



median_diff_heatgain_2020=median_diff_heatgain
mean_diff_heatgain_2020=median_diff_heatgain

median_diff_heatgain_2020$year=2020
mean_diff_heatgain_2020$year=2020














######################### same for 2022########################################################

#load 22 data

wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data/ponds"
setwd(wd)

#Choose year and lake
year <- "S22" #desired open water period, including season
lake <- 'EMLPOND1' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
# bookkeep.emlpond1.22 <- outputs[[1]]
# kalman.emlpond1.22 <- outputs[[2]]
# bayesian.emlpond1.22 <- outputs[[3]]
ts.data.emlpond1.22 <- outputs[[4]]
# DO_depth.emlpond1.22 <- outputs[[5]]
# 
# 
# bookkeep.emlpond1.22 $ SiteName = lake
# kalman.emlpond1.22 $ SiteName = lake
# bayesian.emlpond1.22 $ SiteName = lake
ts.data.emlpond1.22 $ SiteName = lake



#Choose year and lake
year <- "S22" #desired open water period, including season
lake <- 'TOK11' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
# bookkeep.TOK11.22 <- outputs[[1]]
# kalman.TOK11.22 <- outputs[[2]]
# bayesian.TOK11.22 <- outputs[[3]]
ts.data.TOK11.22 <- outputs[[4]]
# DO_depth.TOK11.22 <- outputs[[5]]
# 
# bookkeep.TOK11.22 $ SiteName = lake
# kalman.TOK11.22 $ SiteName = lake
# bayesian.TOK11.22 $ SiteName = lake
ts.data.TOK11.22 $ SiteName = lake



#Choose year and lake
year <- "S22" #desired open water period, including season
lake <- 'TOPAZPOND' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
# bookkeep.TOPAZPOND.22 <- outputs[[1]]
# kalman.TOPAZPOND.22 <- outputs[[2]]
# bayesian.TOPAZPOND.22 <- outputs[[3]]
ts.data.TOPAZPOND.22 <- outputs[[4]]
# DO_depth.TOPAZPOND.22 <- outputs[[5]]

# bookkeep.TOPAZPOND.22 $ SiteName = lake
# kalman.TOPAZPOND.22 $ SiteName = lake
# bayesian.TOPAZPOND.22 $ SiteName = lake
ts.data.TOPAZPOND.22 $ SiteName = lake




#Choose year and lake
year <- "S22" #desired open water period, including season
lake <- 'TOK30' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
# bookkeep.TOK30.22 <- outputs[[1]]
# kalman.TOK30.22 <- outputs[[2]]
# bayesian.TOK30.22 <- outputs[[3]]
ts.data.TOK30.22 <- outputs[[4]]
# DO_depth.TOK30.22 <- outputs[[5]]
# 
# bookkeep.TOK30.22 $ SiteName = lake
# kalman.TOK30.22 $ SiteName = lake
# bayesian.TOK30.22 $ SiteName = lake
ts.data.TOK30.22 $ SiteName = lake



#Choose year 
year <- "2023" #desired open water period, including season

#choose lake
lake <- 'Emerald' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
# bookkeep.emerald.22 <- outputs[[1]]
# kalman.emerald.22 <- outputs[[2]]
# bayesian.emerald.22 <- outputs[[3]]
ts.data.emerald.22 <- outputs[[4]]
# DO_depth.emerald.22 <- outputs[[5]]

ts.data.emerald.22$SiteName="Emerald"

#Choose year 
year <- "2023" #desired open water period, including season

#choose lake
lake <- 'Topaz' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
# bookkeep.topaz.22 <- outputs[[1]]
# kalman.topaz.22 <- outputs[[2]]
# bayesian.topaz.22 <- outputs[[3]]
ts.data.topaz.22 <- outputs[[4]]
# DO_depth.topaz.22 <- outputs[[5]]

ts.data.topaz.22$SiteName="Topaz"
# kalman.topaz.22$SiteName="Topaz"



############ emerald lake 2022 heat content ###############################
library(rLakeAnalyzer)
#get lake bathymetry
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd)

hypso <- read.csv("Emerald_hypsography.csv",header=TRUE)
#format bathymetry dataframe for use with LakeAnalyzer functions
bathy <- data.frame(depths=hypso$Depth_m,areas=hypso$SA_m2)
bathy <- bathy[bathy$areas>0,]

#load lake data
head(ts.data.emerald.22)

# # Create a sequence of times from "00:00:00" to "23:00:00" with 1-hour increments
# time_sequence <- seq(from = as.POSIXct("00:00:00", format = "%H:%M:%S"),
#                      to = as.POSIXct("23:00:00", format = "%H:%M:%S"),
#                      by = "hour")
# 
# # Extract only the time portion
# time_sequence <- format(time_sequence, format = "%H:%M:%S")
# 
# # Repeat the time sequence for the number of rows in the dataframe
# repeated_times <- rep(time_sequence, length.out = nrow(emerald20))
# 
# # Add the repeated time sequence as a new column to the dataframe
# emerald20$time <- repeated_times

# emerald22$datetime <- as.POSIXct(paste(emerald22$datetime, emerald22$tim), format = "%Y-%m-%d %H:%M:%S")


#keep only datetime and wtr_
temp.data <- ts.data.emerald.22 %>%
  select(datetime, starts_with("wtr_"))

#Calculate whole lake heat content (internal energy in Joules); wtr is a dataframe of water , with columns 'datetime', 'wtr_1', 'wtr_2' etc.. corresponding to the depth (m) of the measurement
WL.heat <- ts.internal.energy(wtr=temp.data,bathy=bathy,na.rm = FALSE)#[,2]

##########WL.heat is hourly lake heat content; calculate daily gain directly from this ############
# Extract the date and hour from the datetime column
WL.heat <- WL.heat %>%
  mutate(date = as.Date(datetime),
         hour = hour(datetime))

# Find the minimum heat content for each day, considering only the morning hours (before noon)
daily_min_heat <- WL.heat %>%
  filter(hour <= 12) %>%
  group_by(date) %>%
  summarize(min_heat = min(internal.energy))

#find the maximum heat for each day
daily_max_heat <- WL.heat %>%
  group_by(date) %>%
  summarize(max_heat = max(internal.energy))

#average daily heat content of entire day
daily_heat_content <- WL.heat %>%
  group_by(date) %>%
  summarize(mean_heat = mean(internal.energy))

daily_heat = merge(daily_max_heat, daily_min_heat, all=T)
daily_heat$heatgain = daily_heat$max_heat - daily_heat$min_heat

daily_heat = merge(daily_heat, daily_heat_content, all=T)

heat_EMERALD_22= daily_heat
heat_EMERALD_22$SiteName="EMERALD"

# Create the ggplot object
heat_gain_plot <- ggplot(daily_heat, aes(x = date, y = heatgain)) +
  geom_line() +  # Add a line plot
  geom_point() +  # Add points
  labs(x = "Date", y = "Heat Gain") +  # Label axes
  ggtitle("Daily Heat Gain") +  # Add a title
  theme_bw() +  # Apply a theme
  theme(panel.grid = element_blank())  # Remove gridlines
heat_gain_plot


# Convert Date column to POSIXct class by adding a dummy time component
daily_heat$date <- as.POSIXct(paste(daily_heat$date, "00:00:00"))



############ topaz lake 2020 heat content ###############################
library(rLakeAnalyzer)
#get lake bathymetry
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd)

hypso <- read.csv("Topaz_hypsography.csv",header=TRUE)
#format bathymetry dataframe for use with LakeAnalyzer functions
bathy <- data.frame(depths=hypso$Depth_m,areas=hypso$SA_m2)
bathy <- bathy[bathy$areas>0,]

#load lake data
head(ts.data.topaz.22)

#keep only datetime and wtr_
temp.data <- ts.data.topaz.22 %>%
  select(datetime, starts_with("wtr_"))

#Calculate whole lake heat content (internal energy in Joules); wtr is a dataframe of water , with columns 'datetime', 'wtr_1', 'wtr_2' etc.. corresponding to the depth (m) of the measurement
WL.heat <- ts.internal.energy(wtr=temp.data,bathy=bathy,na.rm = FALSE)#[,2]

##########WL.heat is hourly lake heat content; calculate daily gain directly from this ############
# Extract the date and hour from the datetime column
WL.heat <- WL.heat %>%
  mutate(date = as.Date(datetime),
         hour = hour(datetime))

# Find the minimum heat content for each day, considering only the morning hours (before noon)
daily_min_heat <- WL.heat %>%
  filter(hour <= 12) %>%
  group_by(date) %>%
  summarize(min_heat = min(internal.energy))

#find the maximum heat for each day
daily_max_heat <- WL.heat %>%
  group_by(date) %>%
  summarize(max_heat = max(internal.energy))

#average daily heat content of entire day
daily_heat_content <- WL.heat %>%
  group_by(date) %>%
  summarize(mean_heat = mean(internal.energy))

daily_heat = merge(daily_max_heat, daily_min_heat, all=T)
daily_heat$heatgain = daily_heat$max_heat - daily_heat$min_heat

daily_heat = merge(daily_heat, daily_heat_content, all=T)

heat_TOPAZ_22= daily_heat
heat_TOPAZ_22$SiteName="TOPAZ"




######################## EMLPOND1 ######################################

#No bathymetry for ponds, so use inverted cone method
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd)

# hypso <- read.csv("Emerald_hypsography.csv",header=TRUE)
# #format bathymetry dataframe for use with LakeAnalyzer functions
# bathy <- data.frame(depths=hypso$Depth_m,areas=hypso$SA_m2)
# bathy <- bathy[bathy$areas>0,]
# 

# #For sites lacking bathymetry, estimate using surface area and max depth
#site attributes
setwd("C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data")
site_attributes <- read.csv('All_PondCharacteristics_2020_2022.csv',header=TRUE)
site_attributes <- filter(site_attributes, Lake == "EMLPOND1" & Year == 2022)
LakeArea <- site_attributes$Lake_Area_m2_NG[site_attributes$Lake=="EMLPOND1"] #m^2
MaxDepth <- site_attributes$Max_Depth_m_NG[site_attributes$Lake=="EMLPOND1"] #m^2


bathy <-approx.bathy(Zmax=MaxDepth,lkeArea=LakeArea,zinterval=0.1)#use cone method if no Zmean is known
colnames(bathy)[colnames(bathy) == "Area.at.z"] <- "areas"


#load lake data
head(ts.data.emlpond1.22)


#keep only datetime and wtr_
temp.data <- ts.data.emlpond1.22 %>%
  select(datetime, starts_with("wtr_"))

head(temp.data)

#Calculate whole lake heat content (internal energy in Joules); wtr is a dataframe of water , with columns 'datetime', 'wtr_1', 'wtr_2' etc.. corresponding to the depth (m) of the measurement
WL.heat <- ts.internal.energy(wtr=temp.data,bathy=bathy, na.rm=F)



##########WL.heat is hourly lake heat content; calculate daily gain directly from this ############
# Extract the date and hour from the datetime column
WL.heat <- WL.heat %>%
  mutate(date = as.Date(datetime),
         hour = hour(datetime))

# Find the minimum heat content for each day, considering only the morning hours (before noon)
daily_min_heat <- WL.heat %>%
  filter(hour <= 12) %>%
  group_by(date) %>%
  summarize(min_heat = min(internal.energy))

#find the maximum heat for each day
daily_max_heat <- WL.heat %>%
  group_by(date) %>%
  summarize(max_heat = max(internal.energy))

#average daily heat content of entire day
daily_heat_content <- WL.heat %>%
  group_by(date) %>%
  summarize(mean_heat = mean(internal.energy))

daily_heat = merge(daily_max_heat, daily_min_heat, all=T)
daily_heat$heatgain = daily_heat$max_heat - daily_heat$min_heat

daily_heat = merge(daily_heat, daily_heat_content, all=T)

heat_EMLPOND1_22=daily_heat
heat_EMLPOND1_22$SiteName="EMLPOND1"

# Create the ggplot object
heat_gain_plot <- ggplot(daily_heat, aes(x = date, y = heatgain)) +
  geom_line() +  # Add a line plot
  geom_point() +  # Add points
  labs(x = "Date", y = "Heat Gain") +  # Label axes
  ggtitle("Daily Heat Gain") +  # Add a title
  theme_bw() +  # Apply a theme
  theme(panel.grid = element_blank())  # Remove gridlines
heat_gain_plot



######################## TOK11 ######################################

#No bathymetry for ponds, so use inverted cone method
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd)


# #For sites lacking bathymetry, estimate using surface area and max depth
#site attributes
setwd("C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data")
site_attributes <- read.csv('All_PondCharacteristics_2020_2022.csv',header=TRUE)
site_attributes <- filter(site_attributes, Lake == "TOK11" & Year == 2022)
LakeArea <- site_attributes$Lake_Area_m2_NG[site_attributes$Lake=="TOK11"] #m^2
MaxDepth <- site_attributes$Max_Depth_m_NG[site_attributes$Lake=="TOK11"] #m^2


bathy <-approx.bathy(Zmax=MaxDepth,lkeArea=LakeArea,zinterval=0.1)#use cone method if no Zmean is known
colnames(bathy)[colnames(bathy) == "Area.at.z"] <- "areas"


#load lake data
head(ts.data.TOK11.22)


#keep only datetime and wtr_
temp.data <- ts.data.TOK11.22 %>%
  select(datetime, starts_with("wtr_"))

head(temp.data)

#Calculate whole lake heat content (internal energy in Joules); wtr is a dataframe of water , with columns 'datetime', 'wtr_1', 'wtr_2' etc.. corresponding to the depth (m) of the measurement
WL.heat <- ts.internal.energy(wtr=temp.data,bathy=bathy, na.rm=F)



##########WL.heat is hourly lake heat content; calculate daily gain directly from this ############
# Extract the date and hour from the datetime column
WL.heat <- WL.heat %>%
  mutate(date = as.Date(datetime),
         hour = hour(datetime))

# Find the minimum heat content for each day, considering only the morning hours (before noon)
daily_min_heat <- WL.heat %>%
  filter(hour <= 12) %>%
  group_by(date) %>%
  summarize(min_heat = min(internal.energy))

#find the maximum heat for each day
daily_max_heat <- WL.heat %>%
  group_by(date) %>%
  summarize(max_heat = max(internal.energy))

#average daily heat content of entire day
daily_heat_content <- WL.heat %>%
  group_by(date) %>%
  summarize(mean_heat = mean(internal.energy))

daily_heat = merge(daily_max_heat, daily_min_heat, all=T)
daily_heat$heatgain = daily_heat$max_heat - daily_heat$min_heat

daily_heat = merge(daily_heat, daily_heat_content, all=T)

heat_TOK11_22=daily_heat
heat_TOK11_22$SiteName="TOK11"





######################## TOPAZPOND ######################################

#No bathymetry for ponds, so use inverted cone method
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd)


# #For sites lacking bathymetry, estimate using surface area and max depth
#site attributes
setwd("C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data")
site_attributes <- read.csv('All_PondCharacteristics_2020_2022.csv',header=TRUE)
site_attributes <- filter(site_attributes, Lake == "TOPAZPOND" & Year == 2022)
LakeArea <- site_attributes$Lake_Area_m2_NG[site_attributes$Lake=="TOPAZPOND"] #m^2
MaxDepth <- site_attributes$Max_Depth_m_NG[site_attributes$Lake=="TOPAZPOND"] #m^2


bathy <-approx.bathy(Zmax=MaxDepth,lkeArea=LakeArea,zinterval=0.1)#use cone method if no Zmean is known
colnames(bathy)[colnames(bathy) == "Area.at.z"] <- "areas"


#load lake data
head(ts.data.TOPAZPOND.22)


#keep only datetime and wtr_
temp.data <- ts.data.TOPAZPOND.22 %>%
  select(datetime, starts_with("wtr_"))

head(temp.data)

#Calculate whole lake heat content (internal energy in Joules); wtr is a dataframe of water , with columns 'datetime', 'wtr_1', 'wtr_2' etc.. corresponding to the depth (m) of the measurement
WL.heat <- ts.internal.energy(wtr=temp.data,bathy=bathy, na.rm=F)



##########WL.heat is hourly lake heat content; calculate daily gain directly from this ############
# Extract the date and hour from the datetime column
WL.heat <- WL.heat %>%
  mutate(date = as.Date(datetime),
         hour = hour(datetime))

# Find the minimum heat content for each day, considering only the morning hours (before noon)
daily_min_heat <- WL.heat %>%
  filter(hour <= 12) %>%
  group_by(date) %>%
  summarize(min_heat = min(internal.energy))

#find the maximum heat for each day
daily_max_heat <- WL.heat %>%
  group_by(date) %>%
  summarize(max_heat = max(internal.energy))

#average daily heat content of entire day
daily_heat_content <- WL.heat %>%
  group_by(date) %>%
  summarize(mean_heat = mean(internal.energy))

daily_heat = merge(daily_max_heat, daily_min_heat, all=T)
daily_heat$heatgain = daily_heat$max_heat - daily_heat$min_heat

daily_heat = merge(daily_heat, daily_heat_content, all=T)

heat_TOPAZPOND_22=daily_heat
heat_TOPAZPOND_22$SiteName="TOPAZPOND"






######################## TOK30 ######################################

#No bathymetry for ponds, so use inverted cone method
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd)


# #For sites lacking bathymetry, estimate using surface area and max depth
#site attributes
setwd("C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data")
site_attributes <- read.csv('All_PondCharacteristics_2020_2022.csv',header=TRUE)
site_attributes <- filter(site_attributes, Lake == "TOK30" & Year == 2022)
LakeArea <- site_attributes$Lake_Area_m2_NG[site_attributes$Lake=="TOK30"] #m^2
MaxDepth <- site_attributes$Max_Depth_m_NG[site_attributes$Lake=="TOK30"] #m^2


bathy <-approx.bathy(Zmax=MaxDepth,lkeArea=LakeArea,zinterval=0.1)#use cone method if no Zmean is known
colnames(bathy)[colnames(bathy) == "Area.at.z"] <- "areas"


#load lake data
head(ts.data.TOK30.22)

#keep only datetime and wtr_
temp.data <- ts.data.TOK30.22 %>%
  select(datetime, starts_with("wtr_"))

head(temp.data)

#Calculate whole lake heat content (internal energy in Joules); wtr is a dataframe of water , with columns 'datetime', 'wtr_1', 'wtr_2' etc.. corresponding to the depth (m) of the measurement
WL.heat <- ts.internal.energy(wtr=temp.data,bathy=bathy, na.rm=F)



##########WL.heat is hourly lake heat content; calculate daily gain directly from this ############
# Extract the date and hour from the datetime column
WL.heat <- WL.heat %>%
  mutate(date = as.Date(datetime),
         hour = hour(datetime))

# Find the minimum heat content for each day, considering only the morning hours (before noon)
daily_min_heat <- WL.heat %>%
  filter(hour <= 12) %>%
  group_by(date) %>%
  summarize(min_heat = min(internal.energy))

#find the maximum heat for each day
daily_max_heat <- WL.heat %>%
  group_by(date) %>%
  summarize(max_heat = max(internal.energy))

#average daily heat content of entire day
daily_heat_content <- WL.heat %>%
  group_by(date) %>%
  summarize(mean_heat = mean(internal.energy))

daily_heat = merge(daily_max_heat, daily_min_heat, all=T)
daily_heat$heatgain = daily_heat$max_heat - daily_heat$min_heat

daily_heat = merge(daily_heat, daily_heat_content, all=T)

heat_TOK30_22=daily_heat
heat_TOK30_22$SiteName="TOK30"




##################################### merge 2020 ##############################################
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd) 
files <- list.files()#all the files in wd

heat_2022=list(heat_EMLPOND1_22, heat_TOK11_22, heat_TOPAZPOND_22, heat_TOK30_22, heat_EMERALD_22, heat_TOPAZ_22)

# Merge all dataframes in the list by 'id' column
heat_2022 <- Reduce(function(x, y) merge(x, y, all = TRUE), heat_2022)


##################### plot all together ################################
# Convert Smoke.day to a factor 


#density plot
ggplot(heat_2022, aes(x = mean_heat)) +
  geom_density(alpha = 0.5) +
  labs(x = "Mean Heat", y = "Density") +
  ggtitle("Density Plot of Mean Heat ")

#by site
ggplot(heat_2022, aes(x = mean_heat)) +
  geom_density(alpha = 0.5) +
  labs(x = "Mean Heat", y = "Density") +
  ggtitle("Density Plot of Mean Heat by Smoke Day") +
  facet_wrap(~ SiteName, ncol = 2) +
  theme_minimal()
###


#density plot
ggplot(heat_2022, aes(x = heatgain)) +
  geom_density(alpha = 0.5) +
  labs(x = "heatgain", y = "Density") +
  ggtitle("Density Plot of heatgain")

#by site
ggplot(heat_2022, aes(x = heatgain)) +
  geom_density(alpha = 0.5) +
  labs(x = "heatgain", y = "Density") +
  ggtitle("Density Plot of heatgain") +
  facet_wrap(~ SiteName, ncol = 2) +
  theme_minimal()

heat_2022=na.omit(heat_2022)

# Calculate median for each SiteName and Smoke.day combination
median_heat <- heat_2022 %>%
  group_by(SiteName) %>%
  summarize(median_mean_heat = median(mean_heat), na.rm=T)


# Calculate mean for each SiteName and Smoke.day combination
mean_heat <- heat_2022 %>%
  group_by(SiteName) %>%
  summarize(overall_mean_heat = mean(mean_heat))


########same but with heat gain###############
# Calculate median for each SiteName and Smoke.day combination,  heat gain
median_heatgain <- heat_2022 %>%
  group_by(SiteName) %>%
  summarize(median_heatgain = median(heatgain))
mean_heatgain <- heat_2022 %>%
  group_by(SiteName) %>%
  summarize(mean_heatgain = mean(heatgain))

median_heatgain_2022=median_heatgain
mean_heatgain_2022=mean_heatgain

median_heatgain_2022$year=2022
mean_heatgain_2022$year=2022



#################### combine 2020 and 2021 ######################################

median_diff_all=merge(median_diff_heatgain_2020, median_diff_heatgain_2021, all=T)

median_diff_all$year=as.factor(median_diff_all$year)

ggplot(median_diff_all, aes(x = SiteName, y = median_diff_heatgain, color=year)) +
  geom_point(size = 3) +
  labs(x = "SiteName", y = "Median Difference") +
  ggtitle("Median Difference in heatgain (Smoke day y - Smoke day n)")+
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +  # Adding dotted line at y=0
    theme_classic( )+
  theme(panel.background = element_rect(fill='transparent'))


# ggplot(mean_diff_all, aes(x = SiteName, y = mean_diff, color=year)) +
#   geom_point(size = 3) +
#   labs(x = "SiteName", y = "Mean Difference") +
#   ggtitle("Mean Difference in mean Heat (Smoke day y - Smoke day n)")+
#   geom_hline(yintercept = 0, linetype = "dotted", color = "black") +  # Adding dotted line at y=0
#     theme_classic( )+
#   theme(panel.background = element_rect(fill='transparent'))





median_diff_gain_all=merge(median_diff_heatgain_2020, median_diff_heatgain_2021, all=T)

median_diff_gain_all$year=as.factor(median_diff_gain_all$year)

# ggplot(median_diff_gain_all, aes(x = SiteName, y = median_diff, color=year)) +
#   geom_point(size = 3) +
#   labs(x = "SiteName", y = "Median Difference") +
#   ggtitle("Median Difference in heatgain (Smoke day y - Smoke day n)")+
#   geom_hline(yintercept = 0, linetype = "dotted", color = "black") +  # Adding dotted line at y=0
#   theme_classic( )+
#   theme(panel.background = element_rect(fill='transparent'))


#density plot
heat_2020 <- subset(heat_2020, date >= as.Date("2020-07-02") & date <= as.Date("2020-10-05"))
heat_2021 <- subset(heat_2021, date >= as.Date("2021-07-02") & date <= as.Date("2021-10-05"))
heat_2022 <- subset(heat_2022, date >= as.Date("2022-07-02") & date <= as.Date("2022-10-05"))

# Calculate medians for Smoke Days
median_heat2020 <- heat_2020 %>%
  group_by(Smoke.day) %>%
  summarise(median_heatgain = median(heatgain))

hg20=ggplot(heat_2020, aes(x = heatgain, fill = Smoke.day)) +
  geom_density(alpha = 0.5) +
  geom_vline(data = median_heat2020, aes(xintercept = median_heatgain, linetype = Smoke.day, color = Smoke.day),
             size=1, show.legend = FALSE) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "lightskyblue","y" = "#fdbb84"), labels = c("No", "Yes")) +
    labs(x = " ", y = "2020", fill = "Smoke Day") +
  ggtitle("Density Plot of heat gain by Smoke Day, 7/2-10/5")  +
  scale_x_continuous(limits=c(3000000, 15000000))+
  scale_color_manual(values = c("n" = "lightskyblue", "y" = "#fdbb84")) +
  scale_linetype_manual(values = c("n" = "dashed", "y" = "dashed")) +
  theme_bw()+
  theme(panel.grid = element_blank())
hg20

# Calculate medians for Smoke Days
median_heat2021 <- heat_2021 %>%
  group_by(Smoke.day) %>%
  summarise(median_heatgain = median(heatgain))

hg21=ggplot(heat_2021, aes(x = heatgain, fill = Smoke.day)) +
  geom_density(alpha = 0.5) +
  geom_vline(data = median_heat2021, aes(xintercept = median_heatgain, linetype = Smoke.day, color = Smoke.day),
             size=1, show.legend = FALSE) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "lightskyblue","y" = "#fdbb84"), labels = c("No", "Yes")) +
    labs(x = " ", y = "2021", fill = "Smoke Day") +
  #ggtitle("Density Plot of heatgain by Smoke Day")+
  scale_x_continuous(limits=c(3000000, 15000000))+
  scale_color_manual(values = c("n" = "lightskyblue", "y" = "#fdbb84")) +
  scale_linetype_manual(values = c("n" = "dashed", "y" = "dashed")) +
  theme_bw()+
  theme(panel.grid = element_blank())
hg21

# Calculate medians for Smoke Days
heat_2022$Smoke.day="n"
median_heat2022 <- heat_2022 %>%
  group_by(Smoke.day) %>%
  summarise(median_heatgain = median(heatgain))

heat_2022$Smoke.day="n"
hg22=ggplot(heat_2022, aes(x = heatgain, fill = Smoke.day)) +
  geom_density(alpha = 0.5) +
  geom_vline(data = median_heat2022, aes(xintercept = median_heatgain, linetype = Smoke.day, color = Smoke.day),
             size=1, show.legend = FALSE) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "lightskyblue","y" = "#fdbb84"), labels = c("No", "Yes")) +
  labs(x = "Heat Gain", y = "2022", fill = "Smoke Day") +
  #ggtitle("Density Plot of heatgain by Smoke Day")+
  scale_x_continuous(limits=c(3000000, 15000000))+
  scale_color_manual(values = c("n" = "lightskyblue", "y" = "#fdbb84")) +
  scale_linetype_manual(values = c("n" = "dashed", "y" = "dashed")) +
  theme_bw()+
  theme(panel.grid = element_blank())
hg22
plot_grid(hg20, hg21, hg22,ncol=1)


##############################################################################
#density plots by site

hg20_all=ggplot(heat_2020, aes(x = heatgain, fill = Smoke.day)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "lightskyblue","y" = "#fdbb84"), labels = c("No", "Yes")) +
  labs(x = " ", y = "2020", fill = "Smoke Day") +
  ggtitle("Density Plot of heat gain by Smoke Day, 7/2-10/5")  +
  facet_wrap(~ SiteName, ncol = 2) +
    scale_x_continuous(limits=c(3000000, 15000000))+
  theme_bw()+
  theme(panel.grid = element_blank())
hg20_all

hg21_all=ggplot(heat_2021, aes(x = heatgain, fill = Smoke.day)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "lightskyblue","y" = "#fdbb84"), labels = c("No", "Yes")) +
  labs(x = " ", y = "2021", fill = "Smoke Day") +
  #ggtitle("Density Plot of heatgain by Smoke Day")+
  facet_wrap(~ SiteName, ncol = 2) +
    scale_x_continuous(limits=c(3000000, 15000000))+
  theme_bw()+
  theme(panel.grid = element_blank())
hg21_all

hg22_all=ggplot(heat_2022, aes(x = heatgain, fill = Smoke.day)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "lightskyblue","y" = "#fdbb84"), labels = c("No", "Yes")) +
  labs(x = "Heat Gain", y = "2022", fill = "Smoke Day") +
  #ggtitle("Density Plot of heatgain by Smoke Day")+
  facet_wrap(~ SiteName, ncol = 2) +
    scale_x_continuous(limits=c(3000000, 15000000))+
  theme_bw()+
  theme(panel.grid = element_blank())
hg22_all

plot_grid(hg20_all, hg21_all, hg22_all,ncol=3)




#add median lines to density plots by site

library(dplyr)
library(ggplot2)
library(gridExtra)

# Function to calculate median lines
add_median_lines <- function(data) {
  median_data <- data %>%
    group_by(Smoke.day, SiteName) %>%
    summarise(median_heatgain = median(heatgain))
  
  plot <- ggplot(data, aes(x = heatgain, fill = Smoke.day)) +
    geom_density(alpha = 0.5) +
    geom_vline(data = median_data, aes(xintercept = median_heatgain, linetype = Smoke.day, color = Smoke.day),
               size = 1.5, show.legend = FALSE) +
    scale_fill_manual(name = "Smoke Day", values = c("n" = "lightskyblue", "y" = "#fdbb84"), labels = c("No", "Yes")) +
    labs(x = "Heat Gain", y = "Density", fill = "Smoke Day") +
    facet_wrap(~ SiteName, ncol = 2) +
    scale_x_continuous(limits = c(3000000, 15000000)) +
    scale_color_manual(values = c("n" = "lightskyblue", "y" = "#fdbb84")) +
    scale_linetype_manual(values = c("n" = "dashed", "y" = "dashed")) +
    theme_bw() +
    theme(panel.grid = element_blank())
  
  return(plot)
}

# Create density plots with median lines for 2020, 2021, 2022
hg20_all <- add_median_lines(heat_2020)
hg21_all <- add_median_lines(heat_2021)
hg22_all <- add_median_lines(heat_2022)

# Plot grid
plot_grid(hg20_all, hg21_all, hg22_all, ncol = 2)





##################################################################################
#plot all years and all sites as a large grid:

library(ggplot2)
library(dplyr)
library(gridExtra)

# Concatenate the data frames for the three years
heat_combined <- bind_rows(
  mutate(heat_2020, Year = 2020),
  mutate(heat_2021, Year = 2021),
  mutate(heat_2022, Year = 2022)
)

# Reorder the SiteName factor levels
heat_combined$SiteName <- factor(heat_combined$SiteName, levels = c("EMERALD", "TOPAZ", "EMLPOND1", "TOK11", "TOPAZPOND", "TOK30"))

# Function to create density plots with median lines and t-test results
add_median_lines <- function(data) {
  median_data <- data %>%
    group_by(Smoke.day, SiteName, Year) %>%
    summarise(median_heatgain = median(heatgain))
  
  # Perform t-test for each SiteName and Year combination (excluding 2022)
  t_test_results <- data %>%
    filter(Year != 2022) %>%  # Exclude 2022 from t-test
    filter(Smoke.day %in% c("y", "n")) %>%
    group_by(SiteName, Year) %>%
    summarise(p_value = t.test(heatgain ~ Smoke.day)$p.value) %>%
    ungroup() # Remove grouping so that merging doesn't cause errors
  
  # Merge t-test results with median data
  median_data <- left_join(median_data, t_test_results, by = c("SiteName", "Year"))
  
  # Create the plot
  plot <- ggplot(data, aes(x = heatgain, fill = Smoke.day)) +
    geom_density(alpha = 0.5) +
    geom_vline(data = median_data, aes(xintercept = median_heatgain, linetype = Smoke.day, color = Smoke.day),
               size = 1.5, show.legend = FALSE) +
    scale_fill_manual(name = "Smoke Day", values = c("n" = "lightskyblue", "y" = "#fdbb84"), labels = c("No", "Yes")) +
    labs(x = "Heat Gain", y = "Density", fill = "Smoke Day") +
    facet_grid(SiteName ~ Year) +
    scale_x_continuous(limits = c(3000000, 15000000)) +
    scale_color_manual(values = c("n" = "lightskyblue", "y" = "#fdbb84")) +
    scale_linetype_manual(values = c("n" = "dashed", "y" = "dashed")) +
    theme_bw() +
    theme(panel.grid = element_blank())
  
  # Add "*" to the top right corner of plots where p-value < 0.05
  plot <- plot + geom_text(data = median_data %>% filter(p_value < 0.05),
                           aes(x = Inf, y = Inf, label = "*"),
                           hjust = 1, vjust = 1, size = 8, color = "black")
  
  return(plot)
}
# Create density plots with median lines
hg_all <- add_median_lines(heat_combined)

hg_all


##################################################################################################
plotsave_file_path <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Output/heatgain_plot_gpp.png"

# Save the plot
ggsave(plotsave_file_path, hg_all, width = 8, height =8,dpi = 300)
###################################################################################################

########################################################################
names(smoke.density)[names(smoke.density) == "Date"] <- "date"
smoke.density.2=smoke.density
smoke.density.2$DOY=NULL
smoke.density.2$Year=NULL

heat_smoke_combined <- merge(heat_combined, smoke.density.2, by = "date", all.x = TRUE)

heat_smoke_combined$smoke.density <- ifelse(heat_smoke_combined$SiteName %in% c("EMERALD", "EMLPOND1", "TOK11"), heat_smoke_combined$Emerald.Lake,
                           ifelse(heat_smoke_combined$SiteName %in% c("TOPAZ", "TOPAZPOND", "TOK30"), heat_smoke_combined$Topaz.Lake, NA))
heat_smoke_combined$Topaz.Lake=NULL
heat_smoke_combined$Emerald.Lake=NULL

#boxplot of smokedensity by heatgain
# Convert smoke.density to a factor for better plotting
heat_smoke_combined$smoke.density <- factor(heat_smoke_combined$smoke.density)
heat_smoke_combined <- heat_smoke_combined[complete.cases(heat_smoke_combined$smoke.density), ]
heat_smoke_combined$Year <- factor(heat_smoke_combined$Year)

# Create the boxplot using ggplot
ggplot(heat_smoke_combined, aes(x = smoke.density, y = heatgain, fill=Year)) +
  geom_boxplot() +
  labs(title = " ",
       x = "Smoke Density", y = "Heat Gain")+
  scale_fill_manual(values = c("white", "gray")) +  
  theme_bw() +
  theme(panel.grid = element_blank())





# Boxplot for 2020
hgbp20 <- ggplot(heat_2020, aes(x = Smoke.day, y = heatgain, fill = Smoke.day)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(name = "Smoke Day", values = c("n" = "lightskyblue", "y" = "#fdbb84"), labels = c("No", "Yes")) +
  labs(x = " ", y = "2020", fill = "Smoke Day") +
  ggtitle("Boxplot of heat gain by Smoke Day, 7/2-10/5") +
  scale_y_continuous(limits = c(3000000, 15000000)) +
  theme_bw() +
  theme(panel.grid = element_blank())

# Boxplot for 2021
hgbp21 <- ggplot(heat_2021, aes(x = Smoke.day, y = heatgain, fill = Smoke.day)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(name = "Smoke Day", values = c("n" = "lightskyblue", "y" = "#fdbb84"), labels = c("No", "Yes")) +
  labs(x = " ", y = "2021", fill = "Smoke Day") +
  ggtitle("Boxplot of heat gain by Smoke Day, 7/2-10/5") +
  scale_y_continuous(limits = c(3000000, 15000000)) +
  theme_bw() +
  theme(panel.grid = element_blank())

# Boxplot for 2022
hgbp22 <- ggplot(heat_2022, aes(x = Smoke.day, y = heatgain, fill = Smoke.day)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(name = "Smoke Day", values = c("n" = "lightskyblue", "y" = "#fdbb84"), labels = c("No", "Yes")) +
  labs(x = " ", y = "2022", fill = "Smoke Day") +
  ggtitle("Boxplot of heat gain by Smoke Day, 7/2-10/5") +
  scale_y_continuous(limits = c(3000000, 15000000)) +
  theme_bw() +
  theme(panel.grid = element_blank())

# Arrange the plots in a single column
plot_grid(hgbp20, hgbp21, hgbp22, ncol = 1)




#plot all sites
# Boxplot for 2020
hgbp20_all <- ggplot(heat_2020, aes(x = Smoke.day, y = heatgain, fill = Smoke.day)) +
  geom_boxplot() +
  scale_fill_manual(name = "Smoke Day", values = c("n" = "lightskyblue", "y" = "#fdbb84"), labels = c("No", "Yes")) +
  labs(x = " ", y = "Heat Gain", fill = "Smoke Day") +
  facet_wrap(~ SiteName, ncol = 2) +
  ggtitle("Boxplot of Heat Gain by Smoke Day for 2020") +
  scale_y_continuous(limits = c(3000000, 15000000)) +
  theme_bw() +
  theme(panel.grid = element_blank())
hgbp20_all

# Boxplot for 2021
hgbp21_all <- ggplot(heat_2021, aes(x = Smoke.day, y = heatgain, fill = Smoke.day)) +
  geom_boxplot() +
  scale_fill_manual(name = "Smoke Day", values = c("n" = "lightskyblue", "y" = "#fdbb84"), labels = c("No", "Yes")) +
  labs(x = " ", y = "Heat Gain", fill = "Smoke Day") +
  facet_wrap(~ SiteName, ncol = 2) +
  ggtitle("Boxplot of Heat Gain by Smoke Day for 2021") +
  scale_y_continuous(limits = c(3000000, 15000000)) +
  theme_bw() +
  theme(panel.grid = element_blank())
hgbp21_all

# Boxplot for 2022
hgbp22_all <- ggplot(heat_2022, aes(x = Smoke.day, y = heatgain, fill = Smoke.day)) +
  geom_boxplot() +
  scale_fill_manual(name = "Smoke Day", values = c("n" = "lightskyblue", "y" = "#fdbb84"), labels = c("No", "Yes")) +
  labs(x = " ", y = "Heat Gain", fill = "Smoke Day") +
  facet_wrap(~ SiteName, ncol = 2) +
  ggtitle("Boxplot of Heat Gain by Smoke Day for 2022") +
  scale_y_continuous(limits = c(3000000, 15000000)) +
  theme_bw() +
  theme(panel.grid = element_blank())
hgbp22_all
# Arrange the plots in a single column
plot_grid(hgbp20_all, hgbp21_all, hgbp22_all, ncol = 2)
