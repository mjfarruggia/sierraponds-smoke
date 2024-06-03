#load 22 data

wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data/ponds"
setwd(wd)

#Choose year and lake
year <- "S22" #desired open water period, including season
lake <- 'EMLPOND1' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
bookkeep.emlpond1.22 <- outputs[[1]]
kalman.emlpond1.22 <- outputs[[2]]
bayesian.emlpond1.22 <- outputs[[3]]
ts.data.emlpond1.22 <- outputs[[4]]
DO_depth.emlpond1.22 <- outputs[[5]]


bookkeep.emlpond1.22 $ SiteName = lake
kalman.emlpond1.22 $ SiteName = lake
bayesian.emlpond1.22 $ SiteName = lake
ts.data.emlpond1.22 $ SiteName = lake



#Choose year and lake
year <- "S22" #desired open water period, including season
lake <- 'TOK11' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
bookkeep.TOK11.22 <- outputs[[1]]
kalman.TOK11.22 <- outputs[[2]]
bayesian.TOK11.22 <- outputs[[3]]
ts.data.TOK11.22 <- outputs[[4]]
DO_depth.TOK11.22 <- outputs[[5]]

bookkeep.TOK11.22 $ SiteName = lake
kalman.TOK11.22 $ SiteName = lake
bayesian.TOK11.22 $ SiteName = lake
ts.data.TOK11.22 $ SiteName = lake



#Choose year and lake
year <- "S22" #desired open water period, including season
lake <- 'TOPAZPOND' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
bookkeep.TOPAZPOND.22 <- outputs[[1]]
kalman.TOPAZPOND.22 <- outputs[[2]]
bayesian.TOPAZPOND.22 <- outputs[[3]]
ts.data.TOPAZPOND.22 <- outputs[[4]]
DO_depth.TOPAZPOND.22 <- outputs[[5]]

bookkeep.TOPAZPOND.22 $ SiteName = lake
kalman.TOPAZPOND.22 $ SiteName = lake
bayesian.TOPAZPOND.22 $ SiteName = lake
ts.data.TOPAZPOND.22 $ SiteName = lake




#Choose year and lake
year <- "S22" #desired open water period, including season
lake <- 'TOK30' #choose desired lake

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
bookkeep.TOK30.22 <- outputs[[1]]
kalman.TOK30.22 <- outputs[[2]]
bayesian.TOK30.22 <- outputs[[3]]
ts.data.TOK30.22 <- outputs[[4]]
DO_depth.TOK30.22 <- outputs[[5]]

bookkeep.TOK30.22 $ SiteName = lake
kalman.TOK30.22 $ SiteName = lake
bayesian.TOK30.22 $ SiteName = lake
ts.data.TOK30.22 $ SiteName = lake



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

ts.data.emerald.22$SiteName="Emerald"
kalman.emerald.22$SiteName="Emerald"

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

ts.data.topaz.22$SiteName="Topaz"
kalman.topaz.22$SiteName="Topaz"


####################weather####################
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd)
weather.data.e <- read.csv("Emerald_MET_data_2019_2021_hourly.csv",header=TRUE)
weather.data.e$Date <- as.Date(weather.data.e$Datetime , format='%m/%d/%Y')
weather.data.e$Datetime<-as.POSIXct(weather.data.e$Datetime, format='%m/%d/%Y %H:%M', tz="Etc/GMT-8")


weather.data.t <- read.csv("Topaz_MET_data_2019_2021_hourly.csv",header=TRUE)
weather.data.t$Date <- as.Date(weather.data.t$Datetime , format='%m/%d/%Y')
weather.data.t$DOY <- yday(weather.data.t$Date)
weather.data.t$Datetime<-as.POSIXct(weather.data.t$Datetime, format='%m/%d/%Y %H:%M', tz="Etc/GMT-8")
weather.data.t$SW_solar<-weather.data.t$SW_solar * -1

smoke.density=read.csv("RAPID_sites_smoke_density.csv", header=T)
smoke.density$Date <- as.Date(with(smoke.density, paste(Year, Month, Day, sep = "-")))
smoke.density <-subset(smoke.density, select = c(Date, Year, Topaz.Lake, Emerald.Lake))
smoke.density$DOY<-yday(smoke.density$Date)

weather.data.e<-merge(weather.data.e, smoke.density, all=T)
weather.data.e$Topaz.Lake=NULL
weather.data.e.20<-weather.data.e[with(weather.data.e, Datetime > "2020-08-02 00:00:00" & Datetime < "2020-10-30 00:00:00"),]
weather.data.e.21<-weather.data.e[with(weather.data.e, Datetime > "2021-08-02 00:00:00" & Datetime < "2021-10-30 00:00:00"),]

weather.data.e.22<-read.csv("Emerald_MET_data_2019_2023_hourly.csv",header=TRUE)
weather.data.e.22$Date <- as.Date(weather.data.e.22$Datetime , format='%Y-%m-%d')
weather.data.e.22$DOY<- yday(weather.data.e.22$Date)
weather.data.e.22$Datetime<-as.POSIXct(weather.data.e.22$Datetime, format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT-8")
weather.data.e.22$SW_solar<-weather.data.e.22$SW_solar * -1
weather.data.e.22<-weather.data.e.22[with(weather.data.e.22, Date > "2022-04-02" & Date < "2022-10-30"),]

weather.data.e.20 <- weather.data.e.20[complete.cases(weather.data.e.20$Datetime), ]
avg.sw.e.20 <- weather.data.e.20 %>%
  group_by(Date) %>%
  summarize(
    swrad = mean(SW_solar, na.rm = TRUE),
    airt = mean(AirTemp, na.rm = TRUE)
  ) %>%
  mutate(Date = ymd_hms(paste(Date, "12:00:00")))

weather.data.e.21 <- weather.data.e.21[complete.cases(weather.data.e.21$Datetime), ]
avg.sw.e.21<- weather.data.e.21%>%
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

weather.data.t<-merge(weather.data.t, smoke.density, all=T)
weather.data.t$Emerald.Lake<-NULL
weather.data.t.20<-weather.data.t[with(weather.data.t, Datetime > "2020-08-02 00:00:00" & Datetime < "2020-10-30 00:00:00"),]
weather.data.t.21<-weather.data.t[with(weather.data.t, Datetime > "2021-08-02 00:00:00" & Datetime < "2021-10-30 00:00:00"),]

weather.data.t.22<-read.csv("Topaz_MET_data_2019_2023_hourly.csv",header=TRUE)
weather.data.t.22$Date <- as.Date(weather.data.t.22$Datetime , format='%m/%d/%Y')
weather.data.t.22$DOY <- yday(weather.data.t.22$Date)
weather.data.t.22$Datetime<-as.POSIXct(weather.data.t.22$Datetime, format='%m/%d/%Y %H:%M', tz="Etc/GMT-8")
weather.data.t.22$SW_solar<-weather.data.t.22$SW_solar * -1
weather.data.t.22<-weather.data.t.22[with(weather.data.t.22, Date > "2022-04-02" & Date < "2022-10-30"),]

weather.data.t.20 <- weather.data.t.20[complete.cases(weather.data.t.20$Datetime), ]
avg.sw.t.20<- weather.data.t.20%>%
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

###################################################
###################################################
#emerald 2022 

#GPP
GPP.plot<- ggplot() +
  geom_line(data = bookkeep.emerald.22, aes(x = datetime, y = GPP, color="Bookkeeping")) +
  geom_point(data = bookkeep.emerald.22, aes(x = datetime, y = GPP, color="Bookkeeping"), pch=17) +
  
  geom_line(data = kalman.emerald.22, aes(x = datetime, y = GPP, color="Kalman")) +
  geom_point(data = kalman.emerald.22, aes(x = datetime, y = GPP, color="Kalman"), pch = 19) +
  
  geom_line(data = bayesian.emerald.22, aes(x = datetime, y = GPP, color="Bayesian")) +
  geom_point(data = bayesian.emerald.22, aes(x = datetime, y = GPP, color="Bayesian"), pch = 15) +
  
  scale_color_manual (values=c( "orange","chartreuse4","blue"))+
  
  geom_hline(yintercept = 0, linetype = 2) +
  
  labs(x="Date",  y="GPP")+
  ggtitle("emerald lake 2022")+
  
  theme_bw()

#R
ER.plot<- ggplot() +
  geom_line(data = bookkeep.emerald.22, aes(x = datetime, y = R, color="Bookkeeping")) +
  geom_point(data = bookkeep.emerald.22, aes(x = datetime, y = R, color="Bookkeeping"), pch=17) +
  
  geom_line(data = kalman.emerald.22, aes(x = datetime, y = R, color="Kalman")) +
  geom_point(data = kalman.emerald.22, aes(x = datetime, y = R, color="Kalman"), pch = 19) +
  
  geom_line(data = bayesian.emerald.22, aes(x = datetime, y = R, color="Bayesian")) +
  geom_point(data = bayesian.emerald.22, aes(x = datetime, y = R, color="Bayesian"), pch = 15) +
  
  scale_color_manual (values=c( "orange","chartreuse4","blue"))+
  
  geom_hline(yintercept = 0, linetype = 2) +
  
  labs(x="Date",  y="R")+

  theme_bw()


#NEP
NEP.plot<- ggplot() +

  geom_line(data = bookkeep.emerald.22, aes(x = datetime, y = NEP, color="Bookkeeping")) +
  geom_point(data = bookkeep.emerald.22, aes(x = datetime, y = NEP, color="Bookkeeping"), pch=17) +
  
  geom_line(data = kalman.emerald.22, aes(x = datetime, y = NEP, color="Kalman")) +
  geom_point(data = kalman.emerald.22, aes(x = datetime, y = NEP, color="Kalman"), pch = 19) +
  
  geom_line(data = bayesian.emerald.22, aes(x = datetime, y = NEP, color="Bayesian")) +
  geom_point(data = bayesian.emerald.22, aes(x = datetime, y = NEP, color="Bayesian"), pch = 15) +
  
  scale_color_manual (values=c( "orange","chartreuse4","blue"))+
  
  geom_hline(yintercept = 0, linetype = 2) +
  
  labs(x="Date",  y="NEP")+

  theme_bw()

library(cowplot)
plot_grid(GPP.plot, ER.plot, NEP.plot, ncol=1)





#TOPAZ LAKE 2022

#GPP
GPP.plot<- ggplot() +
  geom_line(data = bookkeep.topaz.22, aes(x = datetime, y = GPP, color="Bookkeeping")) +
  geom_point(data = bookkeep.topaz.22, aes(x = datetime, y = GPP, color="Bookkeeping"), pch=17) +
  
  geom_line(data = kalman.topaz.22, aes(x = datetime, y = GPP, color="Kalman")) +
  geom_point(data = kalman.topaz.22, aes(x = datetime, y = GPP, color="Kalman"), pch = 19) +
  
  geom_line(data = bayesian.topaz.22, aes(x = datetime, y = GPP, color="Bayesian")) +
  geom_point(data = bayesian.topaz.22, aes(x = datetime, y = GPP, color="Bayesian"), pch = 15) +
  
  scale_color_manual (values=c( "orange","chartreuse4","blue"))+
  
  geom_hline(yintercept = 0, linetype = 2) +
  
  labs(x="Date",  y="GPP")+
  ggtitle("topaz lake 2022")+
  
  theme_bw()

#R
ER.plot<- ggplot() +
  geom_line(data = bookkeep.topaz.22, aes(x = datetime, y = R, color="Bookkeeping")) +
  geom_point(data = bookkeep.topaz.22, aes(x = datetime, y = R, color="Bookkeeping"), pch=17) +
  
  geom_line(data = kalman.topaz.22, aes(x = datetime, y = R, color="Kalman")) +
  geom_point(data = kalman.topaz.22, aes(x = datetime, y = R, color="Kalman"), pch = 19) +
  
  geom_line(data = bayesian.topaz.22, aes(x = datetime, y = R, color="Bayesian")) +
  geom_point(data = bayesian.topaz.22, aes(x = datetime, y = R, color="Bayesian"), pch = 15) +
  
  scale_color_manual (values=c( "orange","chartreuse4","blue"))+
  
  geom_hline(yintercept = 0, linetype = 2) +
  
  labs(x="Date",  y="R")+
  
  theme_bw()


#NEP
NEP.plot<- ggplot() +
  
  geom_line(data = bookkeep.topaz.22, aes(x = datetime, y = NEP, color="Bookkeeping")) +
  geom_point(data = bookkeep.topaz.22, aes(x = datetime, y = NEP, color="Bookkeeping"), pch=17) +
  
  geom_line(data = kalman.topaz.22, aes(x = datetime, y = NEP, color="Kalman")) +
  geom_point(data = kalman.topaz.22, aes(x = datetime, y = NEP, color="Kalman"), pch = 19) +
  
  geom_line(data = bayesian.topaz.22, aes(x = datetime, y = NEP, color="Bayesian")) +
  geom_point(data = bayesian.topaz.22, aes(x = datetime, y = NEP, color="Bayesian"), pch = 15) +
  
  scale_color_manual (values=c( "orange","chartreuse4","blue"))+
  
  geom_hline(yintercept = 0, linetype = 2) +
  
  labs(x="Date",  y="NEP")+
  
  theme_bw()


plot_grid(GPP.plot, ER.plot, NEP.plot, ncol=1)







#emlpond1 2022

#GPP
GPP.plot<- ggplot() +
  geom_line(data = bookkeep.emlpond1.22, aes(x = datetime, y = GPP, color="Bookkeeping")) +
  geom_point(data = bookkeep.emlpond1.22, aes(x = datetime, y = GPP, color="Bookkeeping"), pch=17) +
  
  geom_line(data = kalman.emlpond1.22, aes(x = datetime, y = GPP, color="Kalman")) +
  geom_point(data = kalman.emlpond1.22, aes(x = datetime, y = GPP, color="Kalman"), pch = 19) +
  
  geom_line(data = bayesian.emlpond1.22, aes(x = datetime, y = GPP, color="Bayesian")) +
  geom_point(data = bayesian.emlpond1.22, aes(x = datetime, y = GPP, color="Bayesian"), pch = 15) +
  
  scale_color_manual (values=c( "orange","chartreuse4","blue"))+
  
  geom_hline(yintercept = 0, linetype = 2) +
  
  labs(x="Date",  y="GPP")+
  ggtitle("emlpond1  2022")+
  
  theme_bw()

#R
ER.plot<- ggplot() +
  geom_line(data = bookkeep.emlpond1.22, aes(x = datetime, y = R, color="Bookkeeping")) +
  geom_point(data = bookkeep.emlpond1.22, aes(x = datetime, y = R, color="Bookkeeping"), pch=17) +
  
  geom_line(data = kalman.emlpond1.22, aes(x = datetime, y = R, color="Kalman")) +
  geom_point(data = kalman.emlpond1.22, aes(x = datetime, y = R, color="Kalman"), pch = 19) +
  
  geom_line(data = bayesian.emlpond1.22, aes(x = datetime, y = R, color="Bayesian")) +
  geom_point(data = bayesian.emlpond1.22, aes(x = datetime, y = R, color="Bayesian"), pch = 15) +
  
  scale_color_manual (values=c( "orange","chartreuse4","blue"))+
  
  geom_hline(yintercept = 0, linetype = 2) +
  
  labs(x="Date",  y="R")+
  
  theme_bw()


#NEP
NEP.plot<- ggplot() +
  
  geom_line(data = bookkeep.emlpond1.22, aes(x = datetime, y = NEP, color="Bookkeeping")) +
  geom_point(data = bookkeep.emlpond1.22, aes(x = datetime, y = NEP, color="Bookkeeping"), pch=17) +
  
  geom_line(data = kalman.emlpond1.22, aes(x = datetime, y = NEP, color="Kalman")) +
  geom_point(data = kalman.emlpond1.22, aes(x = datetime, y = NEP, color="Kalman"), pch = 19) +
  
  geom_line(data = bayesian.emlpond1.22, aes(x = datetime, y = NEP, color="Bayesian")) +
  geom_point(data = bayesian.emlpond1.22, aes(x = datetime, y = NEP, color="Bayesian"), pch = 15) +
  
  scale_color_manual (values=c( "orange","chartreuse4","blue"))+
  
  geom_hline(yintercept = 0, linetype = 2) +
  
  labs(x="Date",  y="NEP")+
  
  theme_bw()


plot_grid(GPP.plot, ER.plot, NEP.plot, ncol=1)








#tok11 2022

#GPP
GPP.plot<- ggplot() +
  geom_line(data = bookkeep.TOK11.22, aes(x = datetime, y = GPP, color="Bookkeeping")) +
  geom_point(data = bookkeep.TOK11.22, aes(x = datetime, y = GPP, color="Bookkeeping"), pch=17) +
  
  geom_line(data = kalman.TOK11.22, aes(x = datetime, y = GPP, color="Kalman")) +
  geom_point(data = kalman.TOK11.22, aes(x = datetime, y = GPP, color="Kalman"), pch = 19) +
  
  geom_line(data = bayesian.TOK11.22, aes(x = datetime, y = GPP, color="Bayesian")) +
  geom_point(data = bayesian.TOK11.22, aes(x = datetime, y = GPP, color="Bayesian"), pch = 15) +
  
  scale_color_manual (values=c( "orange","chartreuse4","blue"))+
  
  geom_hline(yintercept = 0, linetype = 2) +
  
  labs(x="Date",  y="GPP")+
  ggtitle("TOK11  2022")+
  
  theme_bw()

#R
ER.plot<- ggplot() +
  geom_line(data = bookkeep.TOK11.22, aes(x = datetime, y = R, color="Bookkeeping")) +
  geom_point(data = bookkeep.TOK11.22, aes(x = datetime, y = R, color="Bookkeeping"), pch=17) +
  
  geom_line(data = kalman.TOK11.22, aes(x = datetime, y = R, color="Kalman")) +
  geom_point(data = kalman.TOK11.22, aes(x = datetime, y = R, color="Kalman"), pch = 19) +
  
  geom_line(data = bayesian.TOK11.22, aes(x = datetime, y = R, color="Bayesian")) +
  geom_point(data = bayesian.TOK11.22, aes(x = datetime, y = R, color="Bayesian"), pch = 15) +
  
  scale_color_manual (values=c( "orange","chartreuse4","blue"))+
  
  geom_hline(yintercept = 0, linetype = 2) +
  
  labs(x="Date",  y="R")+
  
  theme_bw()


#NEP
NEP.plot<- ggplot() +
  
  geom_line(data = bookkeep.TOK11.22, aes(x = datetime, y = NEP, color="Bookkeeping")) +
  geom_point(data = bookkeep.TOK11.22, aes(x = datetime, y = NEP, color="Bookkeeping"), pch=17) +
  
  geom_line(data = kalman.TOK11.22, aes(x = datetime, y = NEP, color="Kalman")) +
  geom_point(data = kalman.TOK11.22, aes(x = datetime, y = NEP, color="Kalman"), pch = 19) +
  
  geom_line(data = bayesian.TOK11.22, aes(x = datetime, y = NEP, color="Bayesian")) +
  geom_point(data = bayesian.TOK11.22, aes(x = datetime, y = NEP, color="Bayesian"), pch = 15) +
  
  scale_color_manual (values=c( "orange","chartreuse4","blue"))+
  
  geom_hline(yintercept = 0, linetype = 2) +
  
  labs(x="Date",  y="NEP")+
  
  theme_bw()


plot_grid(GPP.plot, ER.plot, NEP.plot, ncol=1)







#TOPAZPOND 2022

#GPP
GPP.plot<- ggplot() +
  geom_line(data = bookkeep.TOPAZPOND.22, aes(x = datetime, y = GPP, color="Bookkeeping")) +
  geom_point(data = bookkeep.TOPAZPOND.22, aes(x = datetime, y = GPP, color="Bookkeeping"), pch=17) +
  
  geom_line(data = kalman.TOPAZPOND.22, aes(x = datetime, y = GPP, color="Kalman")) +
  geom_point(data = kalman.TOPAZPOND.22, aes(x = datetime, y = GPP, color="Kalman"), pch = 19) +
  
  geom_line(data = bayesian.TOPAZPOND.22, aes(x = datetime, y = GPP, color="Bayesian")) +
  geom_point(data = bayesian.TOPAZPOND.22, aes(x = datetime, y = GPP, color="Bayesian"), pch = 15) +
  
  scale_color_manual (values=c( "orange","chartreuse4","blue"))+
  
  geom_hline(yintercept = 0, linetype = 2) +
  
  labs(x="Date",  y="GPP")+
  ggtitle("TOPAZPOND  2022")+
  
  theme_bw()

#R
ER.plot<- ggplot() +
  geom_line(data = bookkeep.TOPAZPOND.22, aes(x = datetime, y = R, color="Bookkeeping")) +
  geom_point(data = bookkeep.TOPAZPOND.22, aes(x = datetime, y = R, color="Bookkeeping"), pch=17) +
  
  geom_line(data = kalman.TOPAZPOND.22, aes(x = datetime, y = R, color="Kalman")) +
  geom_point(data = kalman.TOPAZPOND.22, aes(x = datetime, y = R, color="Kalman"), pch = 19) +
  
  geom_line(data = bayesian.TOPAZPOND.22, aes(x = datetime, y = R, color="Bayesian")) +
  geom_point(data = bayesian.TOPAZPOND.22, aes(x = datetime, y = R, color="Bayesian"), pch = 15) +
  
  scale_color_manual (values=c( "orange","chartreuse4","blue"))+
  
  geom_hline(yintercept = 0, linetype = 2) +
  
  labs(x="Date",  y="R")+
  
  theme_bw()


#NEP
NEP.plot<- ggplot() +
  
  geom_line(data = bookkeep.TOPAZPOND.22, aes(x = datetime, y = NEP, color="Bookkeeping")) +
  geom_point(data = bookkeep.TOPAZPOND.22, aes(x = datetime, y = NEP, color="Bookkeeping"), pch=17) +
  
  geom_line(data = kalman.TOPAZPOND.22, aes(x = datetime, y = NEP, color="Kalman")) +
  geom_point(data = kalman.TOPAZPOND.22, aes(x = datetime, y = NEP, color="Kalman"), pch = 19) +
  
  geom_line(data = bayesian.TOPAZPOND.22, aes(x = datetime, y = NEP, color="Bayesian")) +
  geom_point(data = bayesian.TOPAZPOND.22, aes(x = datetime, y = NEP, color="Bayesian"), pch = 15) +
  
  scale_color_manual (values=c( "orange","chartreuse4","blue"))+
  
  geom_hline(yintercept = 0, linetype = 2) +
  
  labs(x="Date",  y="NEP")+
  
  theme_bw()


plot_grid(GPP.plot, ER.plot, NEP.plot, ncol=1)







#TOK30 2022

#GPP
GPP.plot<- ggplot() +
  geom_line(data = bookkeep.TOK30.22, aes(x = datetime, y = GPP, color="Bookkeeping")) +
  geom_point(data = bookkeep.TOK30.22, aes(x = datetime, y = GPP, color="Bookkeeping"), pch=17) +
  
  geom_line(data = kalman.TOK30.22, aes(x = datetime, y = GPP, color="Kalman")) +
  geom_point(data = kalman.TOK30.22, aes(x = datetime, y = GPP, color="Kalman"), pch = 19) +
  
  geom_line(data = bayesian.TOK30.22, aes(x = datetime, y = GPP, color="Bayesian")) +
  geom_point(data = bayesian.TOK30.22, aes(x = datetime, y = GPP, color="Bayesian"), pch = 15) +
  
  scale_color_manual (values=c( "orange","chartreuse4","blue"))+
  
  geom_hline(yintercept = 0, linetype = 2) +
  
  labs(x="Date",  y="GPP")+
  ggtitle("TOK30  2022")+
  
  theme_bw()

#R
ER.plot<- ggplot() +
  geom_line(data = bookkeep.TOK30.22, aes(x = datetime, y = R, color="Bookkeeping")) +
  geom_point(data = bookkeep.TOK30.22, aes(x = datetime, y = R, color="Bookkeeping"), pch=17) +
  
  geom_line(data = kalman.TOK30.22, aes(x = datetime, y = R, color="Kalman")) +
  geom_point(data = kalman.TOK30.22, aes(x = datetime, y = R, color="Kalman"), pch = 19) +
  
  geom_line(data = bayesian.TOK30.22, aes(x = datetime, y = R, color="Bayesian")) +
  geom_point(data = bayesian.TOK30.22, aes(x = datetime, y = R, color="Bayesian"), pch = 15) +
  
  scale_color_manual (values=c( "orange","chartreuse4","blue"))+
  
  geom_hline(yintercept = 0, linetype = 2) +
  
  labs(x="Date",  y="R")+
  
  theme_bw()


#NEP
NEP.plot<- ggplot() +
  
  geom_line(data = bookkeep.TOK30.22, aes(x = datetime, y = NEP, color="Bookkeeping")) +
  geom_point(data = bookkeep.TOK30.22, aes(x = datetime, y = NEP, color="Bookkeeping"), pch=17) +
  
  geom_line(data = kalman.TOK30.22, aes(x = datetime, y = NEP, color="Kalman")) +
  geom_point(data = kalman.TOK30.22, aes(x = datetime, y = NEP, color="Kalman"), pch = 19) +
  
  geom_line(data = bayesian.TOK30.22, aes(x = datetime, y = NEP, color="Bayesian")) +
  geom_point(data = bayesian.TOK30.22, aes(x = datetime, y = NEP, color="Bayesian"), pch = 15) +
  
  scale_color_manual (values=c( "orange","chartreuse4","blue"))+
  
  geom_hline(yintercept = 0, linetype = 2) +
  
  labs(x="Date",  y="NEP")+
  
  theme_bw()


plot_grid(GPP.plot, ER.plot, NEP.plot, ncol=1)

