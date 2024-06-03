#this script is for reading in calculated metabolism values, 
#plus other lake and weather parameters for plotting. 

###############################################################################################################################################################
library(LakeMetabolizer)
library(rLakeAnalyzer)
library(lubridate)
library(R2jags)
library(ggplot2)
library(cowplot)

###############################################################################################################################################################
##set working directory, load data, select desired year and lake to analyze
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data/ponds"
setwd(wd)

#Choose year and lake
year <- "S20" #desired open water period, including season
lake <- 'TOK30' #choose desired lake

###############################################################################################################################################################
#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
bookkeep <- outputs[[1]]
kalman <- outputs[[2]]
bayesian <- outputs[[3]]
ts.data <- outputs[[4]]
DO_depth <- outputs[[5]]

################################################################################################################################################################
#load water chem data and subset by pond/year
chem <- read.csv("pond_chemistry_chla_TNTP_2020_2021.csv", header=T)
chem <- subset(chem, Lake %in% lake & Year == 2020)
chem$SampleDate=as.POSIXct(chem$SampleDate, format='%m/%d/%Y',tz="Etc/GMT-8")
################################################################################################################################################################
#load lake and smoke data
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd) 
files <- list.files()#all the files in wd
load('Emerald_Topaz_sensor_data_2020_forMJ.Rdata')
emerald20= ts.2020.forMJ  [[1]]
topaz20= ts.2020.forMJ  [[2]]

load('Emerald_Topaz_sensor_data_2021_forMJ.Rdata')
emerald21= ts.2021.forMJ  [[1]]
topaz21= ts.2021.forMJ  [[2]]

load('SmokeData_forMJ.Rdata')
smoke_topaz_20 = SmokeData_forMJ [[1]]
smoke_eml_20 = SmokeData_forMJ [[2]]
smoke_topaz_21 = SmokeData_forMJ [[3]]
smoke_eml_21 = SmokeData_forMJ [[4]]

load('EML_Topaz_metab_forMJ.Rdata')
metab_topaz_20 = EML_Topaz_metab [[1]]
metab_eml_20 = EML_Topaz_metab [[2]]
metab_topaz_21 = EML_Topaz_metab [[3]]
metab_eml_21 = EML_Topaz_metab [[4]]


##plot diel sw data, water temperature data, with metabolism estimates 

#sw rad
SWR.plot=ggplot()+
  geom_line(data=ts.data, aes(x=datetime, y = swrad))+
  theme_bw()+
  labs(x="Date",  y="sw radiation")+
  ggtitle("TOK30 2020")


#water temp c #*****************needs editing based on specific depths****************#
WTEMP.plot=ggplot()+
  geom_line(data=ts.data, aes(x=datetime, y = wtr_0.77), colour="blue")+
  geom_line(data=ts.data, aes(x=datetime, y = wtr_0.28), colour="dodgerblue")+
    theme_bw()+
  labs(x="Date",  y="water temp c")


#air temp c
ATEMP.plot=ggplot()+
  geom_line(data=ts.data, aes(x=datetime, y = airt))+
  theme_bw()+
  labs(x="Date",  y="air temp c")


plot_grid(SWR.plot, WTEMP.plot, ATEMP.plot, ncol=1)


#DO.sat
DOSAT.plot=ggplot()+
  geom_line(data=ts.data, aes(x=datetime, y = do.sat))+
  theme_bw()+
  labs(x="Date",  y="DO sat")

#DO.obs
DOOBS.plot=ggplot()+
  geom_line(data=ts.data, aes(x=datetime, y = do.obs))+
  theme_bw()+
  labs(x="Date",  y="DO obs")

plot_grid(WTEMP.plot, DOSAT.plot, DOOBS.plot, ncol=1)
plot_grid(SWR.plot, ATEMP.plot, WTEMP.plot, DOSAT.plot, DOOBS.plot, ncol=1)



#GPP
 GPP.plot= ggplot() +
    geom_line(data = bookkeep, aes(x = datetime, y = GPP, color="Bookkeeping")) +
    geom_point(data = bookkeep, aes(x = datetime, y = GPP, color="Bookkeeping"), pch=17) +
    
    geom_line(data = kalman, aes(x = datetime, y = GPP, color="Kalman")) +
    geom_point(data = kalman, aes(x = datetime, y = GPP, color="Kalman"), pch = 19) +
    
    geom_line(data = bayesian, aes(x = datetime, y = GPP, color="Bayesian")) +
    geom_point(data = bayesian, aes(x = datetime, y = GPP, color="Bayesian"), pch = 15) +
    
    scale_color_manual (values=c( "orange","chartreuse4","blue"))+
    
    geom_hline(yintercept = 0, linetype = 2) +
    
    labs(x="Date",  y="GPP")+
    ggtitle("TOK30 2020")+
    
   theme_bw()
   
 #R
 ER.plot= ggplot() +
   geom_line(data = bookkeep, aes(x = datetime, y = R, color="Bookkeeping")) +
   geom_point(data = bookkeep, aes(x = datetime, y = R, color="Bookkeeping"), pch=17) +
   
   geom_line(data = kalman, aes(x = datetime, y = R, color="Kalman")) +
   geom_point(data = kalman, aes(x = datetime, y = R, color="Kalman"), pch = 19) +
   
   geom_line(data = bayesian, aes(x = datetime, y = R, color="Bayesian")) +
   geom_point(data = bayesian, aes(x = datetime, y = R, color="Bayesian"), pch = 15) +
   
   scale_color_manual (values=c( "orange","chartreuse4","blue"))+
   
   geom_hline(yintercept = 0, linetype = 2) +
   
   labs(x="Date",  y="R")+
   #ggtitle("emlpond1 2020 R")+
   
   theme_bw()
 
 
 #NEP
NEP.plot= ggplot() +
   geom_line(data = bookkeep, aes(x = datetime, y = NEP, color="Bookkeeping")) +
   geom_point(data = bookkeep, aes(x = datetime, y = NEP, color="Bookkeeping"), pch=17) +
   
   geom_line(data = kalman, aes(x = datetime, y = NEP, color="Kalman")) +
   geom_point(data = kalman, aes(x = datetime, y = NEP, color="Kalman"), pch = 19) +
   
   geom_line(data = bayesian, aes(x = datetime, y = NEP, color="Bayesian")) +
   geom_point(data = bayesian, aes(x = datetime, y = NEP, color="Bayesian"), pch = 15) +
   
   scale_color_manual (values=c( "orange","chartreuse4","blue"))+
   
   geom_hline(yintercept = 0, linetype = 2) +
   
   labs(x="Date",  y="NEP")+
   #ggtitle("emlpond1 2020 NEP")+
   
   theme_bw()
 
 
 plot_grid(GPP.plot, ER.plot, NEP.plot, ncol=1)
 

 ##############again, but restricted dates##########################################

 ts.data.filter=ts.data[with(ts.data, datetime > "2020-08-01 00:00:00" & datetime < "2020-10-31 00:00:00"),]
 
 ##plot diel sw data, water temperature data, with metabolism estimates 
 
 #sw rad
 SWR.plot=ggplot()+
   geom_line(data=ts.data.filter, aes(x=datetime, y = swrad))+
   theme_bw()+
   labs(x="Date",  y="sw radiation")+
   ggtitle("emlpond1 2020")
 
 
 #water temp c #*****************needs editing based on specific depths****************#
 WTEMP.plot=ggplot()+
   geom_line(data=ts.data.filter, aes(x=datetime, y = wtr_1.58), colour="blue")+
   geom_line(data=ts.data.filter, aes(x=datetime, y = wtr_0.2), colour="dodgerblue")+

   theme_bw()+
   labs(x="Date",  y="water temp c")
 
 
 #air temp c
 ATEMP.plot=ggplot()+
   geom_line(data=ts.data.filter, aes(x=datetime, y = airt))+
   theme_bw()+
   labs(x="Date",  y="air temp c")
 
 
 plot_grid(SWR.plot, WTEMP.plot, ATEMP.plot, ncol=1)
 
 
 #DO.sat
 DOSAT.plot=ggplot()+
   geom_line(data=ts.data.filter, aes(x=datetime, y = do.sat))+
   theme_bw()+
   labs(x="Date",  y="DO sat")
 
 #DO.obs
 DOOBS.plot=ggplot()+
   geom_line(data=ts.data.filter, aes(x=datetime, y = do.obs))+
   theme_bw()+
   labs(x="Date",  y="DO obs")
 
 
 #smoke
 
 smoke_eml_20.filter=smoke_eml_20[with(smoke_eml_20, Datetime > "2020-08-01 00:00:00" & Datetime < "2020-10-31 00:00:00"),]
 
swdiff.plot=ggplot()+
   geom_line(data=smoke_eml_20.filter, aes(x=Datetime, y = sw.diff))+
   theme_bw()+
   labs(x="Date",  y="SW diff")
swdiff.plot


pm.plot=ggplot()+
  geom_point(data=smoke_eml_20.filter, aes(x=Datetime, y = pm2.5_ug_m3))+
  theme_bw()+
  labs(x="Date",  y="pm 2.5")
pm.plot

sden.plot=ggplot()+
  geom_point(data=smoke_eml_20.filter, aes(x=Datetime, y = smoke.density))+
  theme_bw()+
  labs(x="Date",  y="pm 2.5")
sden.plot
 
 plot_grid(WTEMP.plot, DOSAT.plot, DOOBS.plot, ncol=1)
 plot_grid(SWR.plot, ATEMP.plot, swdiff.plot, WTEMP.plot, DOSAT.plot, DOOBS.plot, ncol=1)
 
 plot_grid(SWR.plot, ATEMP.plot, swdiff.plot, WTEMP.plot, ncol=1)
 
bookkeep.filter=bookkeep[with(bookkeep, datetime > "2020-08-01 00:00:00" & datetime < "2020-10-31 00:00:00"),]
kalman.filter=kalman[with(kalman, datetime > "2020-08-01 00:00:00" & datetime < "2020-10-31 00:00:00"),]
bayesian.filter=bayesian[with(bayesian, datetime > "2020-08-01 00:00:00" & datetime < "2020-10-31 00:00:00"),]

 
 #GPP
 GPP.plot= ggplot() +
   geom_line(data = bookkeep.filter, aes(x = datetime, y = GPP, color="Bookkeeping")) +
   geom_point(data = bookkeep.filter, aes(x = datetime, y = GPP, color="Bookkeeping"), pch=17) +
   
   geom_line(data = kalman.filter, aes(x = datetime, y = GPP, color="Kalman")) +
   geom_point(data = kalman.filter, aes(x = datetime, y = GPP, color="Kalman"), pch = 19) +
   
   geom_line(data = bayesian.filter, aes(x = datetime, y = GPP, color="Bayesian")) +
   geom_point(data = bayesian.filter, aes(x = datetime, y = GPP, color="Bayesian"), pch = 15) +
   
   scale_color_manual (values=c( "orange","chartreuse4","blue"))+

   geom_hline(yintercept = 0, linetype = 2) +
   
   labs(x="Date",  y="GPP")+
   ggtitle("emlpond1 2020")+
   
   theme_bw()
 
 #R
 ER.plot= ggplot() +
   geom_line(data = bookkeep.filter, aes(x = datetime, y = R, color="Bookkeeping")) +
   geom_point(data = bookkeep.filter, aes(x = datetime, y = R, color="Bookkeeping"), pch=17) +
   
   geom_line(data = kalman.filter, aes(x = datetime, y = R, color="Kalman")) +
   geom_point(data = kalman.filter, aes(x = datetime, y = R, color="Kalman"), pch = 19) +
   
   geom_line(data = bayesian.filter, aes(x = datetime, y = R, color="Bayesian")) +
   geom_point(data = bayesian.filter, aes(x = datetime, y = R, color="Bayesian"), pch = 15) +
   
   scale_color_manual (values=c( "orange","chartreuse4","blue"))+
   
   geom_hline(yintercept = 0, linetype = 2) +
   
   labs(x="Date",  y="R")+
   #ggtitle("emlpond1 2020 R")+
   
   theme_bw()
 
 
 #NEP
 NEP.plot= ggplot() +
   geom_line(data = bookkeep.filter, aes(x = datetime, y = NEP, color="Bookkeeping")) +
   geom_point(data = bookkeep.filter, aes(x = datetime, y = NEP, color="Bookkeeping"), pch=17) +
   
   geom_line(data = kalman.filter, aes(x = datetime, y = NEP, color="Kalman")) +
   geom_point(data = kalman.filter, aes(x = datetime, y = NEP, color="Kalman"), pch = 19) +
   
   geom_line(data = bayesian.filter, aes(x = datetime, y = NEP, color="Bayesian")) +
   geom_point(data = bayesian.filter, aes(x = datetime, y = NEP, color="Bayesian"), pch = 15) +
   
   scale_color_manual (values=c( "orange","chartreuse4","blue"))+

   geom_hline(yintercept = 0, linetype = 2) +
   
   labs(x="Date",  y="NEP")+
   #ggtitle("emlpond1 2020 NEP")+
   
   theme_bw()
 
 
 plot_grid(GPP.plot, ER.plot, NEP.plot, ncol=1)
 


###########################################################################################################
#water chem - chla
chem.chla = subset(chem, SampleType == "chla")

plot.chla=ggplot()+
  geom_point(data=chem.chla, aes(x=SampleDate, y = Value, color=SampleType))+
  #scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_bw()+
  labs(x="Date",  y="Chla ug/L")+
  ggtitle("emlpond1 2020 Chl-a")


#water chem - TN/TP
chem.TNTP = subset(chem, SampleType != "chla")

plot.TNTP=ggplot()+
  geom_point(data=chem.TNTP, aes(x=SampleDate, y = Value, color=SampleType))+
  #  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_bw()+
  labs(x="Date",  y="umol/L")+
  ggtitle("emlpond1 2020 TN/TP")

plot_grid(plot.chla, plot.TNTP, ncol=1)







###########################################################################################################
#rolling medians and z scores

metab_eml_20$GPP_rolling_median_10d <- rollapply(metab_eml_20$GPP, width = 10, FUN = median, fill = NA, na.rm=T, align = "right")
metab_eml_20$z_score_GPP <- scale(metab_eml_20$GPP)

metab_eml_21$GPP_rolling_median_10d <- rollapply(metab_eml_21$GPP, width = 10, FUN = median, fill = NA, na.rm=T, align = "right")
metab_eml_21$z_score_GPP <- scale(metab_eml_21$GPP)

metab_topaz_20$GPP_rolling_median_10d <- rollapply(metab_topaz_20$GPP, width = 10, FUN = median, fill = NA, na.rm=T, align = "right")
metab_topaz_20$z_score_GPP <- scale(metab_topaz_20$GPP)

metab_topaz_21$GPP_rolling_median_10d <- rollapply(metab_topaz_21$GPP, width = 10, FUN = median, fill = NA, na.rm=T, align = "right")
metab_topaz_21$z_score_GPP <- scale(metab_topaz_21$GPP)

kalman$GPP_rolling_median_10d <- rollapply(kalman$GPP, width = 10, FUN = median, fill = NA, na.rm=T, align = "right")
kalman$z_score_GPP <- scale(kalman$GPP)

#daily z score of metabolism

# Calculate the mean and standard deviation for each day
kalman$GPP_daily_mean <- ave(kalman$GPP, as.Date(kalman$datetime), FUN = mean)
kalman$GPP_daily_sd <- ave(kalman$GPP, as.Date(kalman$datetime), FUN = sd)

# Calculate the z-score for each observation based on daily mean and standard deviation
kalman$z_score_GPP_daily <- (kalman$GPP - kalman$GPP_daily_mean) / kalman$GPP_daily_sd




# Separate and plot GPP for days when Smoke.day is "y" and "n" (single value)
merged_data <- inner_join(smoke_eml_20, kalman, by = c("Date" = "date"))

result <- merged_data %>%
  group_by(Smoke.day) %>%
  summarize(median_GPP = median(GPP, na.rm = TRUE))


# GPP by smoke day 
daily_median_gpp_smokeday <- merged_data %>%
  mutate(Period = case_when(
    Smoke.day == "y" ~ "Smoke",
    Smoke.day == "n" ~ "No Smoke",
    FALSE ~ "Other"
  )) %>%
  group_by(Period, Date = as.Date(datetime)) %>%
  summarize(Median_GPP = median(GPP, na.rm = TRUE))
# Plot density plot (on 2 plots)
density_plot_smokeday <- ggplot(daily_median_gpp_smokeday, aes(x = Median_GPP, fill = Period)) +
  geom_density(alpha = 0.5) +
  labs(x = "Median GPP", y = "Density") +
  ggtitle("Density Plot of Daily GPP by smoke day - emlpond1 2020") +
  theme_minimal() +
  facet_wrap(~Period, scales = "free_y", ncol = 1)
density_plot_smokeday

#median GPP by day, by smoke day
median_gpp_by_day <- merged_data %>%
  group_by(Datetime, Smoke.day) %>%
  summarize(Median_GPP = median(GPP, na.rm = TRUE))
# Plot density plot of median GPP (on same plot)
density_plot <- ggplot(median_gpp_by_day, aes(x = Median_GPP, fill = Smoke.day)) +
  geom_density(alpha = 0.5) +
  labs(x = "Median GPP", y = "Density") +
  ggtitle("Density Plot of Daily GPP - emlpond1 2020") +
  theme_minimal() 
density_plot




#calculate median GPP for different seasons
#early summer (June 1 - July 1)
earlysummer_med_gpp <- median(kalman %>% filter(datetime >= "2020-06-01 00:00", datetime <= "2020-07-15 00:00") %>% pull(GPP), na.rm = TRUE)

#mid summer (July 1 - Aug. 1)
midsummer_med_gpp <- median(kalman %>% filter(datetime >= "2020-07-15 00:00", datetime <= "2020-08-15 00:00") %>% pull(GPP), na.rm = TRUE)

#late summer (Aug. 1 - Sep. 1)
latesummer_med_gpp <- median(kalman %>% filter(datetime >= "2020-08-15 00:00", datetime <= "2020-09-15 00:00") %>% pull(GPP), na.rm = TRUE)

#fall (Sep15-Oct30)
fall_med_gpp <- median(kalman %>% filter(datetime >= "2020-09-15 00:00", datetime <= "2020-10-30 00:00") %>% pull(GPP), na.rm = TRUE)

#entire summer (June 1 - Oct. 31)
allsummer_med_gpp <- median(kalman %>% filter(datetime >= "2020-06-01 00:00", datetime <= "2020-10-30 00:00") %>% pull(GPP), na.rm = TRUE)

#pre-fire (july & aug)
prefire_med_gpp <- median(kalman %>% filter(datetime >= "2020-07-01 00:00", datetime <= "2020-08-15 00:00") %>% pull(GPP), na.rm = TRUE)

#post-fire (oct)
postfire_med_gpp <- median(kalman %>% filter(datetime >= "2020-10-01 00:00", datetime <= "2020-10-30 00:00") %>% pull(GPP), na.rm = TRUE)



# Create a data frame with the median GPP values and corresponding categories
median_gpp_data <- data.frame(
  Category = c("a_Early Summer", "b_Mid Summer", "c_Late Summer", "e_Entire Summer", "d_Fall", "f_Pre-Fire", "g_Post-Fire"),
  Median_GPP = c(earlysummer_med_gpp, midsummer_med_gpp, latesummer_med_gpp, fall_med_gpp, allsummer_med_gpp, prefire_med_gpp, postfire_med_gpp)
)

# Plot a boxplot
ggplot(median_gpp_data, aes(x = Category, y = Median_GPP)) +
  geom_point() +
  labs(title = "Median GPP in Different Time Periods; EMLPOND1 2020",
       x = "Time Period",
       y = "Median GPP") +
  theme_minimal()








#calculate DAILY median GPP for different seasons
#early summer (June 1 - July 1)

#mid summer (July 1 - Aug. 1)

#late summer (Aug. 1 - Sep. 1)

#entire summer (June 1 - Oct. 31)

#pre-fire (july & aug)

#post-fire (oct)
daily_median_gpp <- kalman %>%
  mutate(
    Period = case_when(
      between(datetime, as.POSIXct("2020-06-01 00:00"), as.POSIXct("2020-07-15 00:00")) ~ "Early Summer",
      between(datetime, as.POSIXct("2020-07-15 00:00"), as.POSIXct("2020-08-15 00:00")) ~ "Mid Summer",
      between(datetime, as.POSIXct("2020-08-15 00:00"), as.POSIXct("2020-09-15 00:00")) ~ "Late Summer",
      between(datetime, as.POSIXct("2020-09-15 00:00"), as.POSIXct("2020-10-30 00:00")) ~ "Fall",
      between(datetime, as.POSIXct("2020-06-01 00:00"), as.POSIXct("2020-10-30 00:00")) ~ "Entire Summer",
      between(datetime, as.POSIXct("2020-07-15 00:00"), as.POSIXct("2020-08-15 00:00")) ~ "pre-fire",
      between(datetime, as.POSIXct("2020-10-01 00:00"), as.POSIXct("2020-10-30 00:00")) ~ "Post-Fire",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(Period, Date = as.Date(datetime)) %>%
  summarize(Median_GPP = median(GPP, na.rm = TRUE))
daily_median_gpp$Period <- factor(daily_median_gpp$Period, levels = c("Early Summer", "Mid Summer", "Late Summer", "Fall"))

#plot boxplot
dailymed_gpp_plot <- ggplot(daily_median_gpp, aes(x = Period, y = Median_GPP)) +
  geom_boxplot() +
  labs(x = "Period", y = "GPP") +
  ggtitle("Daily GPP for Different Summer Periods") +
  theme_minimal()
dailymed_gpp_plot

# Plot density plot
density_plot <- ggplot(daily_median_gpp, aes(x = Median_GPP, fill = Period)) +
  geom_density(alpha = 0.5) +
  labs(x = "GPP", y = "Density") +
  ggtitle("Density Plot of Daily GPP - emlpond1 2020") +
  theme_minimal() +
  facet_wrap(~Period, scales = "free_y", ncol = 1)

density_plot


daily_median_gpp_fire <- kalman %>%
  mutate(Period = case_when(
    between(datetime, as.POSIXct("2020-07-15 00:00"), as.POSIXct("2020-08-15 00:00")) ~ "Pre-fire",
    between(datetime, as.POSIXct("2020-10-01 00:00"), as.POSIXct("2020-10-30 00:00")) ~ "Post-Fire",
    FALSE ~ "Other"
  )) %>%
  group_by(Period, Date = as.Date(datetime)) %>%
  filter(!is.na(Period)) %>%
    summarize(Median_GPP = median(GPP, na.rm = TRUE))

#plot boxplot
dailymed_gpp_plot <- ggplot(daily_median_gpp_fire, aes(x = Period, y = Median_GPP)) +
  geom_boxplot() +
  labs(x = "Period", y = "GPP") +
  ggtitle("Daily GPP for Different Periods") +
  theme_minimal()
dailymed_gpp_plot

# Plot density plot
density_plot <- ggplot(daily_median_gpp_fire, aes(x = Median_GPP, fill = Period)) +
  geom_density(alpha = 0.5) +
  labs(x = "GPP", y = "Density") +
  ggtitle("30day Pre/Post Fire Density Plot of Daily GPP - emlpond1 2020") +
  theme_minimal() 

density_plot


####################################################################################
#zscored GPP
daily_median_zscore_gpp <- kalman %>%
  mutate(
    Period = case_when(
      between(datetime, as.POSIXct("2020-06-01 00:00"), as.POSIXct("2020-07-15 00:00")) ~ "Early Summer",
      between(datetime, as.POSIXct("2020-07-15 00:00"), as.POSIXct("2020-08-15 00:00")) ~ "Mid Summer",
      between(datetime, as.POSIXct("2020-08-15 00:00"), as.POSIXct("2020-09-15 00:00")) ~ "Late Summer",
      between(datetime, as.POSIXct("2020-09-15 00:00"), as.POSIXct("2020-10-30 00:00")) ~ "Fall",
      between(datetime, as.POSIXct("2020-06-01 00:00"), as.POSIXct("2020-10-30 00:00")) ~ "Entire Summer",
      between(datetime, as.POSIXct("2020-07-15 00:00"), as.POSIXct("2020-08-15 00:00")) ~ "pre-fire",
      between(datetime, as.POSIXct("2020-10-01 00:00"), as.POSIXct("2020-10-30 00:00")) ~ "Post-Fire",
      TRUE ~ "Other"
    ),
    GPP = ifelse(is.na(GPP), NA, scale(GPP))  # Calculate z-scores for GPP
  ) %>%
  group_by(Period, Date = as.Date(datetime)) %>%
  summarize(Median_ZScore_GPP = median(GPP, na.rm = TRUE))
daily_median_zscore_gpp$Period <- factor(daily_median_zscore_gpp$Period, levels = c("Early Summer", "Mid Summer", "Late Summer", "Fall", "Entire Summer"))

#plot boxplot
dailymed_zgpp_plot <- ggplot(daily_median_zscore_gpp, aes(x = Period, y = Median_ZScore_GPP)) +
  geom_boxplot() +
  labs(x = "Period", y = "z scored GPP") +
  ggtitle("Daily GPP for Different Periods") +
  theme_minimal()
dailymed_zgpp_plot


# Plot density plot
density_plot_z <- ggplot(daily_median_zscore_gpp, aes(x = Median_ZScore_GPP, fill = Period)) +
  geom_density(alpha = 0.5) +
  labs(x = "GPP", y = "Density") +
  ggtitle("Density Plot of Daily z scored GPP - emlpond1 2020") +
  theme_minimal() +
  facet_wrap(~Period, scales = "free_y", ncol = 1)

density_plot_z


#zscored GPP fire
daily_median_zscore_gpp_fire <- kalman %>%
  mutate(
    Period = case_when(
      between(datetime, as.POSIXct("2020-07-15 00:00"), as.POSIXct("2020-08-15 00:00")) ~ "Pre-fire",
      between(datetime, as.POSIXct("2020-10-01 00:00"), as.POSIXct("2020-10-30 00:00")) ~ "Post-Fire",
      TRUE ~ "Other"
    ),
    GPP = ifelse(is.na(GPP), NA, scale(GPP))  # Calculate z-scores for GPP
  ) %>%
  group_by(Period, Date = as.Date(datetime)) %>%
  summarize(Median_ZScore_GPP = median(GPP, na.rm = TRUE))

daily_median_zscore_gpp_fire<- daily_median_zscore_gpp_fire %>%
  filter(Period != "Other")

daily_median_zscore_gpp_fire$Period <- factor(daily_median_zscore_gpp_fire$Period, levels = c("Pre-fire", "Post-Fire"))


#plot boxplot
dailymed_zgpp_plot <- ggplot(daily_median_zscore_gpp_fire, aes(x = Period, y = Median_ZScore_GPP)) +
  geom_boxplot() +
  labs(x = "Period", y = "z scored GPP") +
  ggtitle("Daily GPP for Different Periods") +
  theme_minimal()
dailymed_zgpp_plot


# Plot density plot
density_plot_z <- ggplot(daily_median_zscore_gpp_fire, aes(x = Median_ZScore_GPP, fill = Period)) +
  geom_density(alpha = 0.5) +
  labs(x = "GPP", y = "Density") +
  ggtitle("Density Plot of Daily z scored GPP - emlpond1 2020") +
  theme_minimal() +
  facet_wrap(~Period, scales = "free_y", ncol = 1)

density_plot_z









