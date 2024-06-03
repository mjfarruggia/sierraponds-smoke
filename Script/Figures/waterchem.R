#water chemistry data


###############################################################################################################################################################
library(LakeMetabolizer)
library(rLakeAnalyzer)
library(lubridate)
library(R2jags)
library(cowplot)
library(tidyverse)
library(ggpubr)
###############################################################################################################################################################
##set working directory, load data, select desired year and lake to analyze
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data/ponds"
setwd(wd)


################################################################################################################################################################
#load water chem data 
#chem <- read.csv("pond_chemistry_chla_TNTP_2020_2021.csv", header=T)
#chem <- subset(chem, Lake %in% lake & Year == 2020)
#chem$SampleDate=as.POSIXct(chem$SampleDate, format='%m/%d/%Y',tz="Etc/GMT-8")

chem <- read.csv("tokopah_chemistry_2020_2022.csv", header=T)
chem$SampleDate=as.POSIXct(chem$SampleDate, format='%m/%d/%Y',tz="Etc/GMT-8")
###load lake and smoke data#############################################################################################################################################################
#load lake and smoke data
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd) 
files <- list.files()#all the files in wd
load('Emerald_Topaz_sensor_data_2020_forMJ.Rdata')
ts.emerald20= ts.2020.forMJ  [[1]]
ts.topaz20= ts.2020.forMJ  [[2]]

ts.emerald20$SiteName="Emerald"
ts.topaz20$SiteName="Topaz"

load('Emerald_Topaz_sensor_data_2021_forMJ.Rdata')
ts.emerald21= ts.2021.forMJ  [[1]]
ts.topaz21= ts.2021.forMJ  [[2]]



load('SmokeData_forMJ.Rdata')
smoke_topaz_20 = SmokeData_forMJ [[1]]
names(smoke_topaz_20)[names(smoke_topaz_20) == 'Date'] <- 'date'
smoke_eml_20 = SmokeData_forMJ [[2]]
names(smoke_eml_20)[names(smoke_eml_20) == 'Date'] <- 'date'

smoke_topaz_21 = SmokeData_forMJ [[3]]
names(smoke_topaz_21)[names(smoke_topaz_21) == 'Date'] <- 'date'
smoke_eml_21 = SmokeData_forMJ [[4]]
names(smoke_eml_21)[names(smoke_eml_21) == 'Date'] <- 'date'


load('EML_Topaz_metab_forMJ.Rdata')
metab_topaz_20 = EML_Topaz_metab [[1]]
metab_eml_20 = EML_Topaz_metab [[2]]
metab_topaz_21 = EML_Topaz_metab [[3]]
metab_eml_21 = EML_Topaz_metab [[4]]

metab_eml_20$SiteName="Emerald"
names(metab_eml_20)[names(metab_eml_20) == 'Date'] <- 'date'

metab_topaz_20$SiteName="Topaz"
names(metab_topaz_20)[names(metab_topaz_20) == 'Date'] <- 'date'

metab_eml_21$SiteName="Emerald"
names(metab_eml_21)[names(metab_eml_21) == 'Date'] <- 'date'

metab_topaz_21$SiteName="Topaz"
names(metab_topaz_21)[names(metab_topaz_21) == 'Date'] <- 'date'


###############################################################################################################################################################
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data/ponds"
setwd(wd)

#Choose year and lake
year <- "S20" #desired open water period, including season
lake <- 'EMLPOND1' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
bookkeep.emlpond1.20 <- outputs[[1]]
kalman.emlpond1.20 <- outputs[[2]]
bayesian.emlpond1.20 <- outputs[[3]]
ts.data.emlpond1.20 <- outputs[[4]]
DO_depth.emlpond1.20 <- outputs[[5]]


bookkeep.emlpond1.20 $ SiteName = lake
kalman.emlpond1.20 $ SiteName = lake
bayesian.emlpond1.20 $ SiteName = lake
ts.data.emlpond1.20 $ SiteName = lake

#Choose year and lake
year <- "W20" #desired open water period, including season
lake <- 'EMLPOND1' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
bookkeep.emlpond1.W20 <- outputs[[1]]
kalman.emlpond1.W20 <- outputs[[2]]
bayesian.emlpond1.W20 <- outputs[[3]]
ts.data.emlpond1.W20 <- outputs[[4]]
DO_depth.emlpond1.W20 <- outputs[[5]]


bookkeep.emlpond1.W20 $ SiteName = lake
kalman.emlpond1.W20 $ SiteName = lake
bayesian.emlpond1.W20 $ SiteName = lake
ts.data.emlpond1.W20 $ SiteName = lake



#Choose year and lake
year <- "S20" #desired open water period, including season
lake <- 'TOK11' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
bookkeep.TOK11.20 <- outputs[[1]]
kalman.TOK11.20 <- outputs[[2]]
bayesian.TOK11.20 <- outputs[[3]]
ts.data.TOK11.20 <- outputs[[4]]
DO_depth.TOK11.20 <- outputs[[5]]

bookkeep.TOK11.20 $ SiteName = lake
kalman.TOK11.20 $ SiteName = lake
bayesian.TOK11.20 $ SiteName = lake
ts.data.TOK11.20 $ SiteName = lake


#Choose year and lake
year <- "W20" #desired open water period, including season
lake <- 'TOK11' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
bookkeep.TOK11.W20 <- outputs[[1]]
kalman.TOK11.W20 <- outputs[[2]]
bayesian.TOK11.W20 <- outputs[[3]]
ts.data.TOK11.W20 <- outputs[[4]]
DO_depth.TOK11.W20 <- outputs[[5]]

bookkeep.TOK11.W20 $ SiteName = lake
kalman.TOK11.W20 $ SiteName = lake
bayesian.TOK11.W20 $ SiteName = lake
ts.data.TOK11.W20 $ SiteName = lake


#Choose year and lake
year <- "S20" #desired open water period, including season
lake <- 'TOPAZPOND' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
bookkeep.TOPAZPOND.20 <- outputs[[1]]
kalman.TOPAZPOND.20 <- outputs[[2]]
bayesian.TOPAZPOND.20 <- outputs[[3]]
ts.data.TOPAZPOND.20 <- outputs[[4]]
DO_depth.TOPAZPOND.20 <- outputs[[5]]

bookkeep.TOPAZPOND.20 $ SiteName = lake
kalman.TOPAZPOND.20 $ SiteName = lake
bayesian.TOPAZPOND.20 $ SiteName = lake
ts.data.TOPAZPOND.20 $ SiteName = lake


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

bookkeep.TOPAZPOND.W20 $ SiteName = lake
kalman.TOPAZPOND.W20 $ SiteName = lake
bayesian.TOPAZPOND.W20 $ SiteName = lake
ts.data.TOPAZPOND.W20 $ SiteName = lake

#Choose year and lake
year <- "S20" #desired open water period, including season
lake <- 'TOK30' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
bookkeep.TOK30.20 <- outputs[[1]]
kalman.TOK30.20 <- outputs[[2]]
bayesian.TOK30.20 <- outputs[[3]]
ts.data.TOK30.20 <- outputs[[4]]
DO_depth.TOK30.20 <- outputs[[5]]

bookkeep.TOK30.20 $ SiteName = lake
kalman.TOK30.20 $ SiteName = lake
bayesian.TOK30.20 $ SiteName = lake
ts.data.TOK30.20 $ SiteName = lake

#merge
#merge
merged_kalman_20a <- Reduce(function(x, y) merge(x, y, all=T), list(kalman.emlpond1.20,  kalman.TOK11.20,  kalman.emlpond1.W20,  kalman.TOK11.W20, metab_eml_20))
merged_kalman_20a$datetime=NULL
merged_kalman_20a_smoke <- merge(smoke_eml_20, merged_kalman_20a, all=T)

merged_kalman_20b <- Reduce(function(x, y) merge(x, y, all=T), list(kalman.TOPAZPOND.20,kalman.TOPAZPOND.W20,kalman.TOK30.20, metab_topaz_20))
merged_kalman_20b$datetime=NULL
merged_kalman_20b_smoke <- merge(smoke_eml_20, merged_kalman_20b, all=T)

merged_kalman = merge(merged_kalman_20a, merged_kalman_20b, all=T)
merged_kalman$R = merged_kalman$R * -1

merged_kalman_smoke = merge(merged_kalman_20a_smoke, merged_kalman_20b_smoke, all=T)
merged_kalman_smoke$R = merged_kalman_smoke$R * -1


merged_ts.data <- Reduce(function(x, y) merge(x, y, all=T), list(ts.data.emlpond1.20, ts.data.TOK11.20, ts.data.TOPAZPOND.20, ts.data.emlpond1.W20, ts.data.TOK11.W20, ts.data.TOPAZPOND.W20,ts.data.TOK30.20, ts.emerald20, ts.topaz20))





#2021

wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data/ponds"
setwd(wd)

#Choose year and lake
year <- "S21" #desired open water period, including season
lake <- 'EMLPOND1' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
bookkeep.emlpond1.21 <- outputs[[1]]
kalman.emlpond1.21 <- outputs[[2]]
bayesian.emlpond1.21 <- outputs[[3]]
ts.data.emlpond1.21 <- outputs[[4]]
DO_depth.emlpond1.21 <- outputs[[5]]


bookkeep.emlpond1.21 $ SiteName = lake
kalman.emlpond1.21 $ SiteName = lake
bayesian.emlpond1.21 $ SiteName = lake
ts.data.emlpond1.21 $ SiteName = lake



#Choose year and lake
year <- "S21" #desired open water period, including season
lake <- 'TOK11' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
bookkeep.TOK11.21 <- outputs[[1]]
kalman.TOK11.21 <- outputs[[2]]
bayesian.TOK11.21 <- outputs[[3]]
ts.data.TOK11.21 <- outputs[[4]]
DO_depth.TOK11.21 <- outputs[[5]]

bookkeep.TOK11.21 $ SiteName = lake
kalman.TOK11.21 $ SiteName = lake
bayesian.TOK11.21 $ SiteName = lake
ts.data.TOK11.21 $ SiteName = lake



#Choose year and lake
year <- "S21" #desired open water period, including season
lake <- 'TOPAZPOND' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
bookkeep.TOPAZPOND.21 <- outputs[[1]]
kalman.TOPAZPOND.21 <- outputs[[2]]
bayesian.TOPAZPOND.21 <- outputs[[3]]
ts.data.TOPAZPOND.21 <- outputs[[4]]
DO_depth.TOPAZPOND.21 <- outputs[[5]]

bookkeep.TOPAZPOND.21 $ SiteName = lake
kalman.TOPAZPOND.21 $ SiteName = lake
bayesian.TOPAZPOND.21 $ SiteName = lake
ts.data.TOPAZPOND.21 $ SiteName = lake




#Choose year and lake
year <- "S21" #desired open water period, including season
lake <- 'TOK30' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
bookkeep.TOK30.21 <- outputs[[1]]
kalman.TOK30.21 <- outputs[[2]]
bayesian.TOK30.21 <- outputs[[3]]
ts.data.TOK30.21 <- outputs[[4]]
DO_depth.TOK30.21 <- outputs[[5]]

bookkeep.TOK30.21 $ SiteName = lake
kalman.TOK30.21 $ SiteName = lake
bayesian.TOK30.21 $ SiteName = lake
ts.data.TOK30.21 $ SiteName = lake

#merge
merged_kalman_21a <- Reduce(function(x, y) merge(x, y, all=T), list(kalman.emlpond1.21,  kalman.TOK11.21,  metab_eml_21))
merged_kalman_21a$datetime=NULL
merged_kalman_21a_smoke <- merge(smoke_eml_21, merged_kalman_21a, all=T)

merged_kalman_21b <- Reduce(function(x, y) merge(x, y, all=T), list(kalman.TOPAZPOND.21,kalman.TOK30.21, metab_topaz_21))
merged_kalman_21b$datetime=NULL
merged_kalman_21b_smoke <- merge(smoke_eml_21, merged_kalman_21b, all=T)

merged_kalman_21 = merge(merged_kalman_21a, merged_kalman_21b, all=T)
merged_kalman_21$R = merged_kalman_21$R * -1

merged_kalman_21_smoke = merge(merged_kalman_21a_smoke, merged_kalman_21b_smoke, all=T)
merged_kalman_21_smoke$R = merged_kalman_21_smoke$R * -1


#merged_kalman_21 <- Reduce(function(x, y) merge(x, y, all=T), list(kalman.emlpond1.21,  kalman.TOK11.21, kalman.TOPAZPOND.21,kalman.TOK30.21, metab_eml_21, metab_topaz_21))

merged_ts.data_21 <- Reduce(function(x, y) merge(x, y, all=T), list(ts.data.emlpond1.21, ts.data.TOK11.21, ts.data.TOPAZPOND.21,ts.data.TOK30.21, ts.emerald21, ts.topaz21))









###### chemistry###########

chem$CollectionDepth=NULL
chem$Units=NULL
chem$Notes=NULL
chem$SampleDate=as.Date(chem$SampleDate)
chem <- chem %>%  filter(Value != "<0.08")
chem$Value=as.numeric(chem$Value)
names(chem)[names(chem) == 'Lake'] <- 'SiteName'
names(chem)[names(chem) == 'SampleDate'] <- 'date'
chem$date=as.Date(chem$date)
chem <- chem[chem$Year %in% c(2020, 2021, 2022), ]

#plot chem for each year

#TN
chem_TN=chem %>% filter(SampleType == "TN")
chem_TN$SiteName <- factor(chem_TN$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND", "TOK30"))

chem_TN <- chem_TN %>%
  group_by(SiteName, date) %>%
  summarise(mean_value = mean(Value, na.rm = TRUE))

chem_TN$doy=yday(chem_TN$date)
chem_TN$year=year(chem_TN$date)

TNplot <- ggplot(data = chem_TN, aes(x = doy, y = mean_value, color = SiteName, shape=factor(year))) +
  geom_point() +
  geom_line() +
  labs(x = "doy", y = "TN") +
  ggtitle("TN") +
  facet_wrap(~ year) +
  theme_bw()+   theme(panel.grid = element_blank())
TNplot



#TP
chem_TP=chem %>% filter(SampleType == "TP")
chem_TP$SiteName <- factor(chem_TP$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND", "TOK30"))

chem_TP <- chem_TP %>%
  group_by(SiteName, date) %>%
  summarise(mean_value = mean(Value, na.rm = TRUE))

chem_TP$doy=yday(chem_TP$date)
chem_TP$year=year(chem_TP$date)

TPplot <- ggplot(data = chem_TP, aes(x = doy, y = mean_value, color = SiteName, shape=factor(year))) +
  geom_point() +
  geom_line() +
  labs(x = "doy", y = "TP") +
  ggtitle("TP") +
  facet_wrap(~ year) +
  theme_bw()+   theme(panel.grid = element_blank())
TPplot



#CHLA
chem_chla=chem %>% filter(SampleType == "chla")
chem_chla$SiteName <- factor(chem_chla$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND", "TOK30"))

chem_chla <- chem_chla %>%
  group_by(SiteName, date) %>%
  summarise(mean_value = mean(Value, na.rm = TRUE))

chem_chla$doy=yday(chem_chla$date)
chem_chla$year=year(chem_chla$date)

chlaplot <- ggplot(data = chem_chla, aes(x = doy, y = mean_value, color = SiteName, shape=factor(year))) +
  geom_point() +
  geom_line() +
  labs(x = "doy", y = "chla") +
  ggtitle("chla") +
  facet_wrap(~ year) +
  theme_bw()+   theme(panel.grid = element_blank())
chlaplot


#CHLA + PHEO

chem_PheoChl=chem %>% filter(SampleType == "Pheo+Chl")
chem_PheoChl$SiteName <- factor(chem_PheoChl$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND", "TOK30"))

chem_PheoChl <- chem_PheoChl %>%
  group_by(SiteName, date) %>%
  summarise(mean_value = mean(Value, na.rm = TRUE))

chem_PheoChl$doy=yday(chem_PheoChl$date)
chem_PheoChl$year=year(chem_PheoChl$date)

PheoChlplot <- ggplot(data = chem_PheoChl, aes(x = doy, y = mean_value, color = SiteName, shape=factor(year))) +
  geom_point() +
  geom_line() +
  labs(x = "doy", y = "PheoChl") +
  ggtitle("Pheo + Chl") +
  facet_wrap(~ year) +
  theme_bw()+   theme(panel.grid = element_blank())
PheoChlplot


#DOC.PPM
chem_doc.ppm=chem %>% filter(SampleType == "DOC.ppm")
chem_doc.ppm$SiteName <- factor(chem_doc.ppm$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND", "TOK30"))

chem_doc.ppm <- chem_doc.ppm %>%
  group_by(SiteName, date) %>%
  summarise(mean_value = mean(Value, na.rm = TRUE))

chem_doc.ppm$doy=yday(chem_doc.ppm$date)
chem_doc.ppm$year=year(chem_doc.ppm$date)

doc.ppmplot <- ggplot(data = chem_doc.ppm, aes(x = doy, y = mean_value, color = SiteName, shape=factor(year))) +
  geom_point() +
  geom_line() +
  labs(x = "doy", y = "doc.ppm") +
  ggtitle("doc.ppm") +
  facet_wrap(~ year) +
  theme_bw()+   theme(panel.grid = element_blank())
doc.ppmplot



#DOC.uM
chem_doc.um=chem %>% filter(SampleType == "DOC.uM")
chem_doc.um$SiteName <- factor(chem_doc.um$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND", "TOK30"))

chem_doc.um <- chem_doc.um %>%
  group_by(SiteName, date) %>%
  summarise(mean_value = mean(Value, na.rm = TRUE))

chem_doc.um$doy=yday(chem_doc.um$date)
chem_doc.um$year=year(chem_doc.um$date)

doc.umplot <- ggplot(data = chem_doc.um, aes(x = doy, y = mean_value, color = SiteName, shape=factor(year))) +
  geom_point() +
  geom_line() +
  labs(x = "doy", y = "DOC.um") +
  ggtitle("DOC.uM") +
  facet_wrap(~ year) +
  theme_bw()+   theme(panel.grid = element_blank())
doc.umplot




#PCPN
chem_PCPN=chem %>% filter(SampleType == "PCPN")
chem_PCPN$SiteName <- factor(chem_PCPN$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND", "TOK30"))

chem_PCPN <- chem_PCPN %>%
  group_by(SiteName, date) %>%
  summarise(mean_value = mean(Value, na.rm = TRUE))

chem_PCPN$doy=yday(chem_PCPN$date)
chem_PCPN$year=year(chem_PCPN$date)

PCPNplot <- ggplot(data = chem_PCPN, aes(x = doy, y = mean_value, color = SiteName, shape = factor(year))) +
  geom_point() +
  geom_line() +
  labs(x = "Day of Year", y = "PCPN") +
  ggtitle("PCPN") +
  facet_wrap(~ year) +  # Create separate plots for each year
  theme_bw() + 
  theme(panel.grid = element_blank())

PCPNplot





chem2020=chem %>% filter(Year == 2020)
chem2020$CollectionDepth=NULL
chem2020$Units=NULL
chem2020$Notes=NULL
names(chem2020)[names(chem2020) == 'SampleDate'] <- 'date'

chem2020$date=as.Date(chem2020$date)
chem2020 <- chem2020 %>%  filter(Value != "<0.08")
chem2020$Value=as.numeric(chem2020$Value)
names(chem2020)[names(chem2020) == 'Lake'] <- 'SiteName'

chem2020$date=as.Date(chem2020$date)

merged_kalman_smoke <- merged_kalman_smoke %>%
  mutate(meandepth = case_when(
    SiteName == "Emerald" ~ 4,
    SiteName == "Topaz" ~ 2,
    SiteName == "EMLPOND1" ~ 1.5,
    SiteName == "TOK11" ~ 1.15,
    SiteName == "TOPAZPOND" ~ 0.95,
    SiteName == "TOK30" ~ 0.65,
    TRUE ~ NA_real_  # If none of the conditions match, you can assign NA or any other default value
  ))

merged_kalman_smoke$GPP_depthstandardized=merged_kalman_smoke$GPP * merged_kalman_smoke$meandepth

merged_kalman_smoke_chem=merge(chem2020, merged_kalman_smoke, all=T)

#TN
merged_kalman_smoke_chem_TN=merged_kalman_smoke_chem %>% filter(SampleType == "TN")
merged_kalman_smoke_chem_TN$SiteName <- factor(merged_kalman_smoke_chem_TN$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND", "TOK30"))

TNplot <- ggplot(merged_kalman_smoke_chem_TN, aes(x = Value, y = GPP, color = Smoke.day)) +
  geom_point() +
  labs(x = "TN", y = "GPP") +
  ggtitle("GPP vs TN 2020") +
  scale_x_continuous(limits=c(2, 40))+
    theme_bw()+   theme(panel.grid = element_blank())
TNplot

#TP
merged_kalman_smoke_chem_TP=merged_kalman_smoke_chem %>% filter(SampleType == "TP")
merged_kalman_smoke_chem_TP$SiteName <- factor(merged_kalman_smoke_chem_TP$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND", "TOK30"))

TPplot <- ggplot(merged_kalman_smoke_chem_TP, aes(x = Value, y = GPP, color = Smoke.day)) +
  geom_point() +
  labs(x = "TP", y = "GPP") +
  ggtitle("GPP vs TP 2020") +
  scale_x_continuous(limits=c(0, 0.6))+
    theme_bw()+   theme(panel.grid = element_blank())
TPplot

#chl-a
merged_kalman_smoke_chem_chla=merged_kalman_smoke_chem %>% filter(SampleType == "chla")
merged_kalman_smoke_chem_chla$SiteName <- factor(merged_kalman_smoke_chem_chla$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND", "TOK30"))

chlaplot <- ggplot(merged_kalman_smoke_chem_chla, aes(x = Value, y = GPP, color = Smoke.day)) +
  geom_point() +
  labs(x = "chla", y = "GPP") +
  ggtitle("GPP vs chla 2020") +
  scale_x_continuous(limits=c(0, 5))+
      theme_bw()+   theme(panel.grid = element_blank())
chlaplot

#PCPN
merged_kalman_smoke_chem_CN=merged_kalman_smoke_chem %>% filter(SampleType == "PCPN")
merged_kalman_smoke_chem_CN$SiteName <- factor(merged_kalman_smoke_chem_CN$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND", "TOK30"))

CNplot <- ggplot(merged_kalman_smoke_chem_CN, aes(x = Value, y = GPP, color = Smoke.day)) +
  geom_point() +
  labs(x = "PC/PN", y = "GPP") +
  ggtitle("GPP vs CN 2020") +
  scale_x_continuous(limits=c(0, 5))+
    theme_bw()+   theme(panel.grid = element_blank())
CNplot




###############################################
#chem 2021
chem2021=chem %>% filter(Year == 2021)
chem2021$CollectionDepth=NULL
chem2021$Units=NULL
chem2021$Notes=NULL
chem2021$date=as.Date(chem2021$date)
chem2021 <- chem2021 %>%  filter(Value != "<0.08")
chem2021$Value=as.numeric(chem2021$Value)
names(chem2021)[names(chem2021) == 'Lake'] <- 'SiteName'
names(chem2021)[names(chem2021) == 'SampleDate'] <- 'date'

chem2021$date=as.Date(chem2021$date)


merged_kalman_21_smoke <- merged_kalman_21_smoke %>%
  mutate(meandepth = case_when(
    SiteName == "Emerald" ~ 4,
    SiteName == "Topaz" ~ 2,
    SiteName == "EMLPOND1" ~ 1.5,
    SiteName == "TOK11" ~ 1.15,
    SiteName == "TOPAZPOND" ~ 0.95,
    SiteName == "TOK30" ~ 0.65,
    TRUE ~ NA_real_  # If none of the conditions match, assign NA 
  ))

#standardize GPP by depth
merged_kalman_21_smoke$GPP_depthstandardized=merged_kalman_21_smoke$GPP * merged_kalman_21_smoke$meandepth
merged_kalman_21_smoke <- merged_kalman_21_smoke[!is.na(merged_kalman_21_smoke$GPP_depthstandardized), ]


merged_kalman_21_smoke_chem=merge(chem2021, merged_kalman_21_smoke, all=T)

#TN
merged_kalman_21_smoke_chem_TN=merged_kalman_21_smoke_chem %>% filter(SampleType == "TN")
merged_kalman_21_smoke_chem_TN$SiteName <- factor(merged_kalman_21_smoke_chem_TN$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND", "TOK30"))

TNplot21 <- ggplot(merged_kalman_21_smoke_chem_TN, aes(x = Value, y = GPP, color = Smoke.day)) +
  geom_point() +
  labs(x = "TN", y = "GPP") +
  ggtitle("GPP vs TN 2021") +
  scale_x_continuous(limits=c(2, 40))+
    theme_bw()+   theme(panel.grid = element_blank())
TNplot21

#TP
merged_kalman_21_smoke_chem_TP=merged_kalman_21_smoke_chem %>% filter(SampleType == "TP")
merged_kalman_21_smoke_chem_TP$SiteName <- factor(merged_kalman_21_smoke_chem_TP$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND", "TOK30"))

TPplot21 <- ggplot(merged_kalman_21_smoke_chem_TP, aes(x = Value, y = GPP, color = Smoke.day)) +
  geom_point() +
  labs(x = "TP", y = "GPP") +
  ggtitle("GPP vs TP 2021") +
  scale_x_continuous(limits=c(0, 0.6))+
  
    theme_bw()+   theme(panel.grid = element_blank())
TPplot21

#chl-a
merged_kalman_21_smoke_chem_chla=merged_kalman_21_smoke_chem %>% filter(SampleType == "chla")
merged_kalman_21_smoke_chem_chla$SiteName <- factor(merged_kalman_21_smoke_chem_chla$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND", "TOK30"))


chlaplot21 <- ggplot(merged_kalman_21_smoke_chem_chla, aes(x = Value, y = GPP, color = Smoke.day)) +
  geom_point() +
  labs(x = "chla", y = "GPP") +
  ggtitle("GPP vs chla 2021") +
  scale_x_continuous(limits=c(0, 5))+
    theme_bw()+   theme(panel.grid = element_blank())
chlaplot21


plot_grid(TNplot, TNplot21, TPplot, TPplot21, chlaplot, chlaplot21,ncol=2)


#combine years
TNplot20.21 <- ggplot() +
  geom_point(data=merged_kalman_smoke_chem_TN, aes(x = Value, y = GPP, color = Smoke.day, shape="2020")) +
  geom_point(data=merged_kalman_21_smoke_chem_TN, aes(x = Value, y = GPP, color = Smoke.day, shape="2021")) +
  labs(x = "TN", y = "GPP") +
  ggtitle("GPP vs TN 2020 + 2021") +
  scale_x_continuous(limits=c(2, 40))+
    theme_bw()+   theme(panel.grid = element_blank())
TNplot20.21

TPplot20.21 <- ggplot() +
  geom_point(data=merged_kalman_smoke_chem_TP, aes(x = Value, y = GPP, color = Smoke.day, shape="2020")) +
  geom_point(data=merged_kalman_21_smoke_chem_TP, aes(x = Value, y = GPP, color = Smoke.day, shape="2021")) +
  labs(x = "TP", y = "GPP") +
  ggtitle("GPP vs TP 2020 + 2021") +
  scale_x_continuous(limits=c(0, 0.5))+
    theme_bw()+   theme(panel.grid = element_blank())
TPplot20.21

chlaplot20.21 <- ggplot() +
  geom_point(data=merged_kalman_smoke_chem_chla, aes(x = Value, y = GPP, color = Smoke.day, shape="2020")) +
  geom_point(data=merged_kalman_21_smoke_chem_chla, aes(x = Value, y = GPP, color = Smoke.day, shape="2021")) +
  labs(x = "chla", y = "GPP") +
  ggtitle("GPP vs chla 2020 + 2021") +
  scale_x_continuous(limits=c(0, 2))+
    theme_bw()+   theme(panel.grid = element_blank())
chlaplot20.21

plot_grid( TNplot20.21,  TPplot20.21,  chlaplot20.21,ncol=1)

#again but colored by SiteName instead of SmokeDay
#combine years
TNplot20.21_sites <- ggplot() +
  geom_point(data=merged_kalman_smoke_chem_TN, aes(x = Value, y = GPP, color = SiteName, shape="2020")) +
  geom_point(data=merged_kalman_21_smoke_chem_TN, aes(x = Value, y = GPP, color = SiteName, shape="2021")) +
  labs(x = "TN", y = "GPP") +
  ggtitle("GPP vs TN 2020 + 2021") +
  scale_x_continuous(limits=c(2, 40))+
  theme_bw()+   theme(panel.grid = element_blank())
TNplot20.21_sites

TPplot20.21_sites <- ggplot() +
  geom_point(data=merged_kalman_smoke_chem_TP, aes(x = Value, y = GPP, color = SiteName, shape="2020")) +
  geom_point(data=merged_kalman_21_smoke_chem_TP, aes(x = Value, y = GPP, color = SiteName, shape="2021")) +
  labs(x = "TP", y = "GPP") +
  ggtitle("GPP vs TP 2020 + 2021") +
  scale_x_continuous(limits=c(0, 0.5))+
  theme_bw()+   theme(panel.grid = element_blank())
TPplot20.21_sites

chlaplot20.21_sites <- ggplot() +
  geom_point(data=merged_kalman_smoke_chem_chla, aes(x = Value, y = GPP, color = SiteName, shape="2020")) +
  geom_point(data=merged_kalman_21_smoke_chem_chla, aes(x = Value, y = GPP, color = SiteName, shape="2021")) +
  labs(x = "chla", y = "GPP") +
  ggtitle("GPP vs chla 2020 + 2021") +
  scale_x_continuous(limits=c(0, 2))+
  theme_bw()+   theme(panel.grid = element_blank())
chlaplot20.21_sites

plot_grid( TNplot20.21_sites,  TPplot20.21_sites,  chlaplot20.21_sites,ncol=1)


#plot when standardized GPP by mean depth 


TNplot20.21.ds <- ggplot() +
  geom_point(data=merged_kalman_smoke_chem_TN, aes(x = Value, y = GPP_depthstandardized, color = Smoke.day, shape="2020")) +
  geom_point(data=merged_kalman_21_smoke_chem_TN, aes(x = Value, y = GPP_depthstandardized, color = Smoke.day, shape="2021")) +
  labs(x = "TN", y = "GPP (depth standardized)") +
  ggtitle("GPP vs TN 2020 + 2021") +
  scale_x_continuous(limits=c(2, 40))+
    theme_bw()+   theme(panel.grid = element_blank())
TNplot20.21.ds

TPplot20.21.ds <- ggplot() +
  geom_point(data=merged_kalman_smoke_chem_TP, aes(x = Value, y = GPP_depthstandardized, color = Smoke.day, shape="2020")) +
  geom_point(data=merged_kalman_21_smoke_chem_TP, aes(x = Value, y = GPP_depthstandardized, color = Smoke.day, shape="2021")) +
  labs(x = "TP", y = "GPP (depth standardized)") +
  ggtitle("GPP vs TP 2020 + 2021") +
  scale_x_continuous(limits=c(0, 0.5))+
    theme_bw()+   theme(panel.grid = element_blank())
TPplot20.21.ds

chlaplot20.21.ds <- ggplot() +
  geom_point(data=merged_kalman_smoke_chem_chla, aes(x = Value, y = GPP_depthstandardized, color = Smoke.day, shape="2020")) +
  geom_point(data=merged_kalman_21_smoke_chem_chla, aes(x = Value, y = GPP_depthstandardized, color = Smoke.day, shape="2021")) +
  labs(x = "chla", y = "GPP (depth standardized)") +
  ggtitle("GPP vs chla 2020 + 2021") +
  scale_x_continuous(limits=c(0, 2))+
    theme_bw()+   theme(panel.grid = element_blank())
chlaplot20.21.ds

plot_grid( TNplot20.21.ds,  TPplot20.21.ds,  chlaplot20.21.ds,ncol=1)


plot_grid( TNplot20.21,TNplot20.21.ds,   TPplot20.21,TPplot20.21.ds,  chlaplot20.21,chlaplot20.21.ds,ncol=2)


##################################################
#plot seasonal chem, date on x, chem on y
#add swrad in background, and rain

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
rain=rain[with(rain, Datetime > "2019-07-01 00:00:00" & Datetime < "2021-10-01 00:00:00"),]



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
weather.data.e.20=weather.data.e[with(weather.data.e, Datetime > "2020-06-02 00:00:00" & Datetime < "2020-10-01 00:00:00"),]
weather.data.e.21=weather.data.e[with(weather.data.e, Datetime > "2021-04-02 00:00:00" & Datetime < "2021-10-01 00:00:00"),]

weather.data.e.22=read.csv("Emerald_MET_data_2019_2023_hourly.csv",header=TRUE)
weather.data.e.22$Date = as.Date(weather.data.e.22$Datetime , format='%Y-%m-%d')
weather.data.e.22$DOY = yday(weather.data.e.22$Date)
weather.data.e.22$Datetime=as.POSIXct(weather.data.e.22$Datetime, format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
weather.data.e.22$SW_solar=weather.data.e.22$SW_solar * -1
weather.data.e.22=weather.data.e.22[with(weather.data.e.22, Date > "2022-04-02" & Date < "2022-10-01"),]


weather.data.e.20 <- weather.data.e.20[complete.cases(weather.data.e.20$Datetime), ]
avg.sw.e.20 <- weather.data.e.20 %>%
  group_by(Date) %>%
  summarize(
    swrad = mean(SW_solar, na.rm = TRUE),
    airt = mean(AirTemp, na.rm = TRUE),
    rain= max(Rain_mm, na.rm=T) ) %>%
  mutate(Date = ymd_hms(paste(Date, "12:00:00")))

weather.data.e.21 <- weather.data.e.21[complete.cases(weather.data.e.21$Datetime), ]
avg.sw.e.21= weather.data.e.21%>%
  group_by(Date) %>%
  summarize(
    swrad = mean(SW_solar, na.rm = TRUE),
    airt = mean(AirTemp, na.rm = TRUE),
    rain= max(Rain_mm, na.rm=T) ) %>%
  mutate(Date = ymd_hms(paste(Date, "12:00:00")))

weather.data.e.22 <- weather.data.e.22[complete.cases(weather.data.e.22$Datetime), ]
avg.sw.e.22= weather.data.e.22%>%
  group_by(Date) %>%
  summarize(
    swrad = mean(SW_solar, na.rm = TRUE),
    airt = mean(AirTemp, na.rm = TRUE),
    rain= max(Rain_mm, na.rm=T)
  ) %>%
  mutate(Date = ymd_hms(paste(Date, "12:00:00")))
avg.sw.e.22$swrad=avg.sw.e.22$swrad*-1

weather.data.t=merge(weather.data.t, smoke.density, all=T)
weather.data.t$Emerald.Lake=NULL
weather.data.t.20=weather.data.t[with(weather.data.t, Datetime > "2020-06-01 00:00:00" & Datetime < "2020-10-01 00:00:00"),]
weather.data.t.21=weather.data.t[with(weather.data.t, Datetime > "2021-04-01 00:00:00" & Datetime < "2021-10-01 00:00:00"),]

weather.data.t.22=read.csv("Topaz_MET_data_2019_2023_hourly.csv",header=TRUE)
weather.data.t.22$Date = as.Date(weather.data.t.22$Datetime , format='%m/%d/%Y')
weather.data.t.22$DOY = yday(weather.data.t.22$Date)
weather.data.t.22$Datetime=as.POSIXct(weather.data.t.22$Datetime, format='%m/%d/%Y %H:%M', tz="Etc/GMT-8")
weather.data.t.22$SW_solar=weather.data.t.22$SW_solar * -1
weather.data.t.22=weather.data.t.22[with(weather.data.t.22, Date > "2022-04-02" & Date < "2022-10-01"),]

weather.data.t.20 <- weather.data.t.20[complete.cases(weather.data.t.20$Datetime), ]
avg.sw.t.20= weather.data.t.20%>%
  group_by(Date) %>%
  summarize(
    swrad = mean(SW_solar, na.rm = TRUE),
    airt = mean(AirTemp, na.rm = TRUE),
    rain= max(Rain_mm, na.rm=T)
  ) %>%
  mutate(Date = ymd_hms(paste(Date, "12:00:00")))

weather.data.t.21 <- weather.data.t.21[complete.cases(weather.data.t.21$Datetime), ]
avg.sw.t.21 <- weather.data.t.21 %>%
  group_by(Date) %>%
  summarize(
    swrad = mean(SW_solar, na.rm = TRUE),
    airt = mean(AirTemp, na.rm = TRUE),
    rain= max(Rain_mm, na.rm=T)
  ) %>%
  mutate(Date = ymd_hms(paste(Date, "12:00:00", sep = " ")))


weather.data.t.22 <- weather.data.t.22[complete.cases(weather.data.t.22$Datetime), ]
avg.sw.t.22 <- weather.data.t.22 %>%
  group_by(Date) %>%
  summarize(
    swrad = mean(SW_solar, na.rm = TRUE),
    airt = mean(AirTemp, na.rm = TRUE),
    rain= max(Rain_mm, na.rm=T)
  ) %>%
  mutate(Date = ymd_hms(paste(Date, "12:00:00", sep = " ")))

# If Rain_mm >4, make RainIntensity "High". If Rain_mm >2 but <4, make RainIntensity "Medium".  If Rain_mm <2, make RainIntensity "Low". 
weather.data.e.20 <- weather.data.e.20 %>%
  mutate(RainIntensity = case_when(
    Rain_mm > 4 ~ "High",
    Rain_mm > 2 ~ "Medium",
    Rain_mm > 0 & Rain_mm <= 2 ~ "Low",
    TRUE ~ NA
  ))

weather.data.e.21 <- weather.data.e.21 %>%
  mutate(RainIntensity = case_when(
    Rain_mm > 4 ~ "High",
    Rain_mm > 2 ~ "Medium",
    Rain_mm > 0 & Rain_mm <= 2 ~ "Low",
    TRUE ~ NA
  ))

weather.data.e.21 <- weather.data.e.21 %>%
  mutate(RainIntensity = case_when(
    Rain_mm > 4 ~ "High",
    Rain_mm > 2 ~ "Medium",
    Rain_mm > 0 & Rain_mm <= 2 ~ "Low",
    TRUE ~ NA
  ))

avg.sw.e.20 <- avg.sw.e.20 %>%
  mutate(RainIntensity = case_when(
    rain > 4 ~ "High",
    rain > 2 ~ "Medium",
    rain > 0 & rain <= 2 ~ "Low",
    TRUE ~ NA
  ))

avg.sw.e.21 <- avg.sw.e.21 %>%
  mutate(RainIntensity = case_when(
    rain > 4 ~ "High",
    rain > 2 ~ "Medium",
    rain > 0 & rain <= 2 ~ "Low",
    TRUE ~ NA
  ))

avg.sw.e.22 <- avg.sw.e.22 %>%
  mutate(RainIntensity = case_when(
    rain > 4 ~ "High",
    rain > 2 ~ "Medium",
    rain > 0 & rain <= 2 ~ "Low",
    TRUE ~ NA
  ))



#CHLA
chem2020chla=chem2020 %>% filter (SampleType=="chla")
# Create a new column for the month
chem2020chla$month <- format(chem2020chla$date, "%m")

# Calculate the monthly average
chem2020chla <- chem2020chla %>%
  group_by(SiteName, month) %>%
  summarise(avg_value = mean(Value, na.rm = TRUE))

chem2020chla$SiteName=as.factor(chem2020chla$SiteName)
# Convert month column to Date format with the first day of each month
chem2020chla$datetime <- as.POSIXct(paste("2020", chem2020chla$month, "01 12:00:00"), format = "%Y %m %d %H:%M:%S")


chla20 <- ggplot() +
  geom_point(data = chem2020chla, aes(x = datetime, y = avg_value, color = SiteName, group = SiteName)) +
  geom_line(data = chem2020chla, aes(x = datetime, y = avg_value, color = SiteName, group = SiteName)) +
  geom_line(data = avg.sw.e.20, aes(x = Date, y = airt/10)) +
  geom_point(data = avg.sw.e.20[!is.na(avg.sw.e.20$RainIntensity), ],
             aes(x = Date, y = 3.5, color = RainIntensity),
             size = 3, shape = 16) +
   scale_y_continuous(
    name = "chla (monthly avg)",
    sec.axis = sec_axis(~ . * 10, name = "Air temp")) + 
  labs(x = "Month", y = "chla (monthly avg)") +
  ggtitle("chla 2020") +
  theme_bw() +
  theme(panel.grid = element_blank())

chla20


#TN
chem2020TN=chem2020 %>% filter (SampleType=="TN")
# Create a new column for the month
chem2020TN$month <- format(chem2020TN$date, "%m")

# Calculate the monthly average
chem2020TN <- chem2020TN %>%
  group_by(SiteName, month) %>%
  summarise(avg_value = mean(Value, na.rm = TRUE))

chem2020TN$SiteName=as.factor(chem2020TN$SiteName)

# Convert month column to Date format with the first day of each month
chem2020TN$datetime <- as.POSIXct(paste("2020", chem2020TN$month, "01 12:00:00"), format = "%Y %m %d %H:%M:%S")


TN20  <- ggplot() +
  geom_point(data = chem2020TN, aes(x = datetime, y = avg_value, color = SiteName, group = SiteName)) +
  geom_line(data = chem2020TN, aes(x = datetime, y = avg_value, color = SiteName, group = SiteName)) +
  geom_line(data = avg.sw.e.20, aes(x = Date, y = airt)) +
  geom_point(data = avg.sw.e.20[!is.na(avg.sw.e.20$RainIntensity), ],
             aes(x = Date, y = 60, color = RainIntensity),
             size = 3, shape = 16) +
  scale_y_continuous(
    name = "TN (monthly avg)",
    sec.axis = sec_axis(~ . * 1, name = "Air temp")) + 
  labs(x = "Month", y = "TN (monthly avg)") +
  ggtitle("TN 2020") +
  theme_bw() +
  theme(panel.grid = element_blank())

TN20


#TP
chem2020TP=chem2020 %>% filter (SampleType=="TP")
# Create a new column for the month
chem2020TP$month <- format(chem2020TP$date, "%m")

# Calculate the monthly average
chem2020TP <- chem2020TP %>%
  group_by(SiteName, month) %>%
  summarise(avg_value = mean(Value, na.rm = TRUE))

chem2020TP$SiteName=as.factor(chem2020TP$SiteName)

# Convert month column to Date format with the first day of each month
chem2020TP$datetime <- as.POSIXct(paste("2020", chem2020TP$month, "01 12:00:00"), format = "%Y %m %d %H:%M:%S")


TP20  <- ggplot() +
  geom_point(data = chem2020TP, aes(x = datetime, y = avg_value, color = SiteName, group = SiteName)) +
  geom_line(data = chem2020TP, aes(x = datetime, y = avg_value, color = SiteName, group = SiteName)) +
  geom_line(data = avg.sw.e.20, aes(x = Date, y = airt/10)) +
  geom_point(data = avg.sw.e.20[!is.na(avg.sw.e.20$RainIntensity), ],
             aes(x = Date, y = 2, color = RainIntensity),
             size = 3, shape = 16) +
  scale_y_continuous(
    name = "TP (monthly avg)",
    sec.axis = sec_axis(~ . * 10, name = "Air temp")) + 
  labs(x = "Month", y = "TP (monthly avg)") +
  ggtitle("TP 2020") +
  theme_bw() +
  theme(panel.grid = element_blank())

TP20



#PCPN
chem2020PCPN=chem2020 %>% filter (SampleType=="PCPN")
# Create a new column for the month
chem2020PCPN$month <- format(chem2020PCPN$date, "%m")

# Calculate the monthly average
chem2020PCPN <- chem2020PCPN %>%
  group_by(SiteName, month) %>%
  summarise(avg_value = mean(Value, na.rm = TRUE))

chem2020PCPN$SiteName=as.factor(chem2020PCPN$SiteName)

# Convert month column to Date format with the first day of each month
chem2020PCPN$datetime <- as.POSIXct(paste("2020", chem2020PCPN$month, "01 12:00:00"), format = "%Y %m %d %H:%M:%S")


PCPN20  <- ggplot() +
  geom_point(data = chem2020PCPN, aes(x = datetime, y = avg_value, color = SiteName, group = SiteName)) +
  geom_line(data = chem2020PCPN, aes(x = datetime, y = avg_value, color = SiteName, group = SiteName)) +
  geom_line(data = avg.sw.e.20, aes(x = Date, y = airt)) +
  geom_point(data = avg.sw.e.20[!is.na(avg.sw.e.20$RainIntensity), ],
             aes(x = Date, y = 20, color = RainIntensity),
             size = 3, shape = 16) +
  scale_y_continuous(
    name = "PCPN (monthly avg)",
    sec.axis = sec_axis(~ . * 1, name = "Air temp")) + 
  labs(x = "Month", y = "PCPN (monthly avg)") +
  ggtitle("PCPN 2020") +
  theme_bw() +
  theme(panel.grid = element_blank())

PCPN20








#CHLA
chem2021chla=chem2021 %>% filter (SampleType=="chla")
# Create a new column for the month
chem2021chla$month <- format(chem2021chla$date, "%m")

# Calculate the monthly average
chem2021chla <- chem2021chla %>%
  group_by(SiteName, month) %>%
  summarise(avg_value = mean(Value, na.rm = TRUE))

chem2021chla$SiteName=as.factor(chem2021chla$SiteName)

# Convert month column to Date format with the first day of each month
chem2021chla$datetime <- as.POSIXct(paste("2021", chem2021chla$month, "01 12:00:00"), format = "%Y %m %d %H:%M:%S")


chla21 <- ggplot() +
  geom_point(data = chem2021chla, aes(x = datetime, y = avg_value, color = SiteName, group = SiteName)) +
  geom_line(data = chem2021chla, aes(x = datetime, y = avg_value, color = SiteName, group = SiteName)) +
  geom_line(data = avg.sw.e.21, aes(x = Date, y = airt/10)) +
  geom_point(data = avg.sw.e.21[!is.na(avg.sw.e.21$RainIntensity), ],
             aes(x = Date, y = 3.5, color = RainIntensity),
             size = 3, shape = 16) +
  scale_y_continuous(
    name = "chla (monthly avg)",
    sec.axis = sec_axis(~ . * 10, name = "Air temp")) + 
  labs(x = "Month", y = "chla (monthly avg)") +
  ggtitle("chla 2021") +
  theme_bw() +
  theme(panel.grid = element_blank())

chla21


#TN
chem2021TN=chem2021 %>% filter (SampleType=="TN")
# Create a new column for the month
chem2021TN$month <- format(chem2021TN$date, "%m")

# Calculate the monthly average
chem2021TN <- chem2021TN %>%
  group_by(SiteName, month) %>%
  summarise(avg_value = mean(Value, na.rm = TRUE))

chem2021TN$SiteName=as.factor(chem2021TN$SiteName)

 # Convert month column to Date format with the first day of each month
chem2021TN$datetime <- as.POSIXct(paste("2021", chem2021TN$month, "01 12:00:00"), format = "%Y %m %d %H:%M:%S")


TN21  <- ggplot() +
  geom_point(data = chem2021TN, aes(x = datetime, y = avg_value, color = SiteName, group = SiteName)) +
  geom_line(data = chem2021TN, aes(x = datetime, y = avg_value, color = SiteName, group = SiteName)) +
  geom_line(data = avg.sw.e.21, aes(x = Date, y = airt)) +
  geom_point(data = avg.sw.e.21[!is.na(avg.sw.e.21$RainIntensity), ],
             aes(x = Date, y = 40, color = RainIntensity),
             size = 3, shape = 16) +
  scale_y_continuous(
    name = "TN (monthly avg)",
    sec.axis = sec_axis(~ . * 1, name = "Air temp")) + 
  labs(x = "Month", y = "TN (monthly avg)") +
  ggtitle("TN 2021") +
  theme_bw() +
  theme(panel.grid = element_blank())

TN21


#TP
chem2021TP=chem2021 %>% filter (SampleType=="TP")
# Create a new column for the month
chem2021TP$month <- format(chem2021TP$date, "%m")

# Calculate the monthly average
chem2021TP <- chem2021TP %>%
  group_by(SiteName, month) %>%
  summarise(avg_value = mean(Value, na.rm = TRUE))

chem2021TP$SiteName=as.factor(chem2021TP$SiteName)

# Convert month column to Date format with the first day of each month
chem2021TP$datetime <- as.POSIXct(paste("2021", chem2021TP$month, "01 12:00:00"), format = "%Y %m %d %H:%M:%S")


TP21  <- ggplot() +
  geom_point(data = chem2021TP, aes(x = datetime, y = avg_value, color = SiteName, group = SiteName)) +
  geom_line(data = chem2021TP, aes(x = datetime, y = avg_value, color = SiteName, group = SiteName)) +
  geom_line(data = avg.sw.e.21, aes(x = Date, y = airt/30)) +
  geom_point(data = avg.sw.e.21[!is.na(avg.sw.e.21$RainIntensity), ],
             aes(x = Date, y = 0.75, color = RainIntensity),
             size = 3, shape = 16) +
  scale_y_continuous(
    name = "TP (monthly avg)",
    sec.axis = sec_axis(~ . * 30, name = "Air temp")) + 
  labs(x = "Month", y = "TP (monthly avg)") +
  ggtitle("TP 2021") +
  theme_bw() +
  theme(panel.grid = element_blank())

TP21


#PCPN
chem2021PCPN=chem2021 %>% filter (SampleType=="PCPN")
# Create a new column for the month
chem2021PCPN$month <- format(chem2021PCPN$date, "%m")

# Calculate the monthly average
chem2021PCPN <- chem2021PCPN %>%
  group_by(SiteName, month) %>%
  summarise(avg_value = mean(Value, na.rm = TRUE))

chem2021PCPN$SiteName=as.factor(chem2021PCPN$SiteName)

# Convert month column to Date format with the first day of each month
chem2021PCPN$datetime <- as.POSIXct(paste("2021", chem2021PCPN$month, "01 12:00:00"), format = "%Y %m %d %H:%M:%S")


PCPN21  <- ggplot() +
  geom_point(data = chem2021PCPN, aes(x = datetime, y = avg_value, color = SiteName, group = SiteName)) +
  geom_line(data = chem2021PCPN, aes(x = datetime, y = avg_value, color = SiteName, group = SiteName)) +
  geom_line(data = avg.sw.e.21, aes(x = Date, y = airt)) +
  geom_point(data = avg.sw.e.21[!is.na(avg.sw.e.21$RainIntensity), ],
             aes(x = Date, y = 20, color = RainIntensity),
             size = 3, shape = 16) +
  scale_y_continuous(
    name = "PCPN (monthly avg)",
    sec.axis = sec_axis(~ . * 1, name = "Air temp")) + 
  labs(x = "Month", y = "PCPN (monthly avg)") +
  ggtitle("PCPN 2021") +
  theme_bw() +
  theme(panel.grid = element_blank())

PCPN21



plot_grid(TN20, TN21, TP20, TP21, chla20, chla21, ncol=2)
plot_grid(TN20, TN21, TP20, TP21, chla20, chla21, PCPN20, PCPN21, ncol=2)

library(gridExtra)

# Extract legend from one plot (let's say TN20)
legend <- cowplot::get_legend(chla21)

# Plot grid with one legend
grid <- plot_grid(TN20 + theme(legend.position = "none"),
          TN21 + theme(legend.position = "none"),
          TP20 + theme(legend.position = "none"),
          TP21 + theme(legend.position = "none"),
          chla20 + theme(legend.position = "none"),
          chla21 + theme(legend.position = "none"),
          PCPN20 + theme(legend.position = "none"),
          PCPN21 + theme(legend.position = "none"),
          ncol = 2)
final_plot <- plot_grid(grid, legend, ncol = 2)
final_plot
#############################################################
#### surface area ######
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd) 
SA=read.csv(file="Site_SurfaceArea.csv", header=T)

merged_kalman_20_smoke_SA=merge(SA, merged_kalman_smoke, all=T)
merged_kalman_21_smoke_SA=merge(SA, merged_kalman_21_smoke, all=T)

swradplot20 <- ggplot(merged_kalman_20_smoke_SA, aes(x = swrad, y = GPP, color = SiteName)) +
  geom_point() +
  labs(x = "swrad", y = "GPP") +
  ggtitle("GPP vs swrad 2020") +
  theme_bw()+
  theme(panel.grid = element_blank())
swradplot20

SAradplot20 <- ggplot(merged_kalman_20_smoke_SA, aes(x = SurfaceArea_m2, y = GPP, color = SiteName)) +
  geom_point() +
  labs(x = "SA", y = "GPP") +
  ggtitle("GPP vs SA 2020") +
  theme_bw()+
  theme(panel.grid = element_blank())
SAradplot20







############################################################################################################
chem2020=chem %>% filter(Year == 2020)
chem2020$CollectionDepth=NULL
chem2020$Units=NULL
chem2020$Notes=NULL
chem2020$SampleDate=as.Date(chem2020$SampleDate)
chem2020 <- chem2020 %>%  filter(Value != "<0.08")
chem2020$Value=as.numeric(chem2020$Value)
names(chem2020)[names(chem2020) == 'Lake'] <- 'SiteName'

mean_chem2020=chem2020 %>%
  group_by(SiteName, SampleType) %>%
  summarize(Average_Value = mean(Value, na.rm = TRUE))

smokeday_chem=merge(smokeday_delta_metab, chem2020, all=T)

smokeday_chem_mean=merge(smokeday_delta_metab, mean_chem2020, all=T)

smokeday_chem_mean=na.omit(smokeday_chem_mean)

wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd) 
SA=read.csv(file="Site_SurfaceArea.csv", header=T)
smokeday_SA=merge(smokeday_delta_metab, SA, all=T)


#plot surface area x gpp
smokeday_SA$log_SurfaceArea_m2=log(smokeday_SA$SurfaceArea_m2)
ggplot(smokeday_SA, aes(x = log_SurfaceArea_m2, y = GPP_difference, color = SiteName)) +
  geom_point(size=3) +
  labs(x = "log Surface Area m2", y = "ΔGPP") +
    theme_bw()+   theme(panel.grid = element_blank())

#plot chla x gpp
smokeday_chem_mean_chla <- smokeday_chem_mean %>%  filter(SampleType == "chla")

ggplot(smokeday_chem_mean_chla, aes(x = Average_Value, y = GPP_difference, color = SiteName)) +
  geom_point(size=3) +
  labs(x = "seasonal mean chla", y = "ΔGPP") +
    theme_bw()+   theme(panel.grid = element_blank())

#plot TN x gpp
smokeday_chem_mean_TN <- smokeday_chem_mean %>%  filter(SampleType == "TN")

ggplot(smokeday_chem_mean_TN, aes(x = Average_Value, y = GPP_difference, color = SiteName)) +
  geom_point(size=3) +
  labs(x = "seasonal mean TN", y = "ΔGPP") +
    theme_bw()+   theme(panel.grid = element_blank())

#plot TP x gpp
smokeday_chem_mean_TP <- smokeday_chem_mean %>%  filter(SampleType == "TP")

ggplot(smokeday_chem_mean_TP, aes(x = Average_Value, y = GPP_difference, color = SiteName)) +
  geom_point(size=3) +
  labs(x = "seasonal mean TP", y = "ΔGPP") +
    theme_bw()+   theme(panel.grid = element_blank())
