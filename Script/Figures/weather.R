
#precip plots
###############################################################################################################################################################
library(LakeMetabolizer)
library(rLakeAnalyzer)
library(lubridate)
library(R2jags)
library(ggplot2)
library(cowplot)
library(dplyr)


################################################################################################################################################################
#load precip data
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd) 


rain = read.csv("Precip_05_21_EMLMET.csv", header=T)
rain$Datetime=as.POSIXct(rain$Datetime, format='%m/%d/%y %H:%M', tz="Etc/GMT-8")

weather.data.e.22=read.csv("Emerald_MET_data_2019_2023_hourly.csv",header=TRUE)
weather.data.e.22$Datetime=as.POSIXct(weather.data.e.22$Datetime, format='%Y-%m-%d %H:%M', tz="Etc/GMT-8")
weather.data.e.22 <- select(weather.data.e.22, Datetime, Rain_mm)

rain=merge(rain, weather.data.e.22, all=T)
rain=rain[with(rain, Datetime > "2020-01-01 00:00:00" & Datetime < "2022-12-30 00:00:00"),]
rain <- na.omit(rain)

# If Rain_mm >4, make RainIntensity "High". If Rain_mm >2 but <4, make RainIntensity "Medium".  If Rain_mm <2, make RainIntensity "Low". 
rain <- rain %>%
  mutate(RainIntensity = case_when(
    Rain_mm > 4 ~ "High",
    Rain_mm > 2 ~ "Medium",
    Rain_mm > 0 & Rain_mm <= 2 ~ "Low",
    TRUE ~ NA
  ))

#cumulative precip from Jan 2020 to Dec 2022

# Extract year from Datetime column
rain$Year <- lubridate::year(rain$Datetime)

# Calculate cumulative rain_mm totals for each year
cumulative_rain <- rain %>%
  group_by(Year) %>%
  mutate(Cumulative_Rain_mm = sum(Rain_mm)) %>%
  select(Year, Cumulative_Rain_mm) %>%
  distinct()

rain$Month <- month(rain$Datetime)

# Calculate cumulative rain_mm totals for each MONTH AND year
cumulative_rain_monthly <- rain %>%
  group_by(Year, Month) %>%
  mutate(Cumulative_Rain_mm = sum(Rain_mm)) %>%
  select(Year, Month, Cumulative_Rain_mm) %>%
  distinct()

#boxplot of rain by year
annual_rain=ggplot(cumulative_rain, aes(x = factor(Year), y = Cumulative_Rain_mm, fill = factor(Year))) +
geom_boxplot() +
  labs(x = "Year", y = "Precipitation (mm)", title = "Precip by Year", fill="Year") +
  scale_y_log10()+
  theme_bw()+
  theme(panel.grid = element_blank(), legend.position = "bottom")  


annual_rain_month=ggplot(rain, aes(x = factor(Month), y = Rain_mm, fill = factor(Year))) +
  geom_boxplot() +
  labs(x = "Year", y = "Precipitation (mm)", title = "Precip by Month (full year)", fill="Year") +
  scale_y_log10()+
  theme_bw()+
  theme(panel.grid = element_blank(), legend.position = "bottom")  

plot_grid(annual_rain, annual_rain_month, ncol=2)

#bar plot version
annual_rain_month_bar=ggplot(cumulative_rain_monthly, aes(x = factor(Month), y = Cumulative_Rain_mm, fill = factor(Year))) +
  geom_bar(stat="identity", position="dodge") +
  labs(x = "Year", y = "Precipitation (mm)", title = "Precip by Month (full year)", color="Year") +
  theme_bw()+
  theme(panel.grid = element_blank(), legend.position = "bottom") 
annual_rain_month_bar

annual_rain_bar=ggplot(cumulative_rain, aes(x = factor(Year), y = Cumulative_Rain_mm, fill = factor(Year))) +
  geom_bar(stat="identity", position="dodge") +
  labs(x = "Year", y = "Precipitation (mm)", title = "Precip by Year", fill="Year") +
  theme_bw()+
  theme(panel.grid = element_blank(), legend.position = "bottom")  
annual_rain_bar

rain_firemonths <- rain[rain$Month %in% c(7,8, 9), ]

#boxplot of rain by year, only including fire months
rain_bp=ggplot(rain_firemonths, aes(x = factor(Month), y = Rain_mm, fill = factor(Year))) +
  geom_boxplot() +
  labs(x = "Month", y = "Precipitation (mm)", title = "Precip (Jul/Aug/Sep)", fill="Year") +
  scale_y_log10()+
  theme_bw()+
  theme(panel.grid = element_blank(), legend.position = "bottom")  
rain_bp

annual_fire_rain=ggplot() +
  geom_boxplot(data=rain_firemonths, aes(x = factor(Year), y = Rain_mm, fill = factor(Year))) +
  labs(x = "Year", y = "Precipitation (mm)", title = "Annual precip (Jul/Aug/sep only)", fill="Year") +
  theme_bw()+
  scale_y_log10()+
  theme(panel.grid = element_blank(), legend.position = "bottom")  
annual_fire_rain

plot_grid(annual_rain, annual_fire_rain, ncol=2)

#bar plot version
cumulative_rain_monthly_firemonths <- cumulative_rain_monthly[cumulative_rain_monthly$Month %in% c(7, 8, 9), ]

rain_bar=ggplot(cumulative_rain_monthly_firemonths, aes(x = factor(Month), y = Cumulative_Rain_mm, fill = factor(Year))) +
  geom_bar(stat="identity", position="dodge") +
  labs(x = "Month", y = "Precipitation (mm)", title = "Precip (Jul/Aug/sep)", fill="Year") +
  theme_bw()+
  theme(panel.grid = element_blank(), legend.position = "bottom")  
rain_bar


cumulative_rain_annual_fire <- cumulative_rain_monthly_firemonths %>%
  group_by(Year) %>%
  mutate(Cumulative_Rain_mm = sum(Cumulative_Rain_mm)) %>%
  select(Year, Cumulative_Rain_mm) %>%
  distinct()

annual_fire_rain_bar=ggplot(data=cumulative_rain_annual_fire, aes(x = factor(Year), y = Cumulative_Rain_mm, fill = factor(Year))) +
  geom_bar(stat="identity", position="dodge") +
    labs(x = "Year", y = "Precipitation (mm)", title = "Annual precip (Jul/Aug/sep only)", fill="Year") +
  theme_bw()+
  theme(panel.grid = element_blank(), legend.position = "bottom")  
annual_fire_rain_bar

plot_grid(annual_rain_bar, annual_fire_rain_bar, rain_bar, ncol=3)


# Summmary stats for each year MET
# boxplots by month, for each year
# average monthly air temperature, shortwave, precipitation

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


#2020 eml met station is avg.sw.e.20
#2021 eml met station is avg.sw.e.21
#2022 eml met station is avg.sw.e.22

avg.sw.e.all=merge(avg.sw.e.20, avg.sw.e.21, all=T)
avg.sw.e.all=merge(avg.sw.e.all, avg.sw.e.22, all=T)

# Create a month and year column
avg.sw.e.all$Month <- format(avg.sw.e.all$Date, "%m")
avg.sw.e.all$Month <- as.numeric(avg.sw.e.all$Month)
avg.sw.e.all$Year <- format(avg.sw.e.all$Date, "%Y")
avg.sw.e.all$DOY <- format(avg.sw.e.all$Date, "%Y")

avg.sw.e.all <- avg.sw.e.all %>%
  filter(!(Month %in% c(4, 5, 6, 7)))

avg.sw.e.all$Month <- as.factor(avg.sw.e.all$Month)


avg.sw.e.all <- avg.sw.e.all %>%
  mutate(doy = yday(Date)) %>%
  select(-Date)
avg.sw.e.all <- avg.sw.e.all[!is.na(avg.sw.e.all$Month), ]

# Plot boxplots
airtemp_box=ggplot(avg.sw.e.all, aes(x = Month, y = airt, fill=Year)) +
  geom_boxplot(position = position_dodge()) +
  labs(x = "Month", y = "Air Temperature C", title = "Air Temperature by Month (aug/sep/oct)") +
  theme_bw()+
  theme(panel.grid = element_blank(),legend.position = "bottom")


swrad_box=ggplot(avg.sw.e.all, aes(x = Month, y = swrad, fill=Year)) +
  geom_boxplot(position = position_dodge()) +
  labs(x = "Month", y = "SW Radiation W/m2", title = "SWrad by Month (aug/sep/oct)") +
  theme_bw() +
  theme(panel.grid = element_blank(), legend.position = "bottom")  

plot_grid(airtemp_box, swrad_box, rain_bp, ncol=3)

plot_grid(airtemp_box, swrad_box,  ncol=2)






wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd) 
files <- list.files()#all the files in wd

load('SmokeData_forMJ.Rdata')
smoke_topaz_20 = SmokeData_forMJ [[1]]
smoke_eml_20 = SmokeData_forMJ [[2]]
smoke_topaz_21 = SmokeData_forMJ [[3]]
smoke_eml_21 = SmokeData_forMJ [[4]]


smoke_eml_20$Month <- month(smoke_eml_20$Date)
smoke_eml_20=smoke_eml_20[with(smoke_eml_20, Date> "2020-07-02 00:00:00" & Date < "2020-10-05 00:00:00"),]

smoke_hist=ggplot(smoke_eml_20, aes(x = month, fill = Smoke.day)) +
  geom_bar(position = "dodge", color = "black", alpha = 0.7) +
  labs(x = "Month", y = "Frequency", fill = "Smoke.day") +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "lightskyblue","y" = "#fdbb84"), labels = c("No", "Yes")) +
  
  ggtitle("Number of Smoke.day by Month 2020")+
  theme_bw()+
  theme(panel.grid = element_blank(),legend.position = "bottom")
smoke_hist

smoke_eml_21$Month <- month(smoke_eml_21$Date)
smoke_eml_21=smoke_eml_21[with(smoke_eml_21, Date> "2021-07-02 00:00:00" & date < "2021-10-05 00:00:00"),]

smoke_hist_21=ggplot(smoke_eml_21, aes(x = month, fill = Smoke.day)) +
  geom_bar(position = "dodge", color = "black", alpha = 0.7) +
  labs(x = "Month", y = "Frequency", fill = "Smoke.day") +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "lightskyblue","y" = "#fdbb84"), labels = c("No", "Yes")) +
  
  ggtitle("Number of Smoke.day by Month 2021")+
  theme_bw()+
  theme(panel.grid = element_blank(),legend.position = "bottom")
smoke_hist_21
plot_grid(smoke_hist, smoke_hist_21, ncol=1)



smoke_20_21=merge(smoke_eml_20, smoke_eml_21, all=T)
smoke_20_21$Year <- year(smoke_20_21$Date)

swdiff_boxplot <- ggplot(smoke_20_21, aes(x = factor(smoke.density), y = sw.diff, fill = as.factor(Year))) +
  geom_boxplot(alpha = 0.7, outlier.colour = "black", outlier.shape = 1) +
  labs(x = "Smoke Density", y = "Δ Shortwave W m-2", fill = "Year") +
  ggtitle("SWdiff") +
  scale_fill_manual(values = c("2020" = "white", "2021" = "gray"), labels = c("2020", "2021")) +
  theme_bw()+
  theme(panel.grid = element_blank(),legend.position = "bottom")
swdiff_boxplot

wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Output"
setwd(wd) 

ggsave("swdiff_boxplot.png", plot = swdiff_boxplot, width = 8, height = 10, units = "in", dpi = 300)

