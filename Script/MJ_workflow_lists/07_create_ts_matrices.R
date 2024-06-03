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

#change temp_2.17 to wtr_2.55 so it merges into the same column
colnames(topaz21_early)[colnames(topaz21_early) == "temp_2.17"] <- "wtr_2.55"
#keep only date and temp column
topaz21_early = topaz21_early [, c("datetime", "wtr_2.55"), drop=F]

#merge the two topaz datasets to get full 2021 data
topaz21=merge(topaz21_early, topaz21_late, all=T)

########################################################################################################################
############## temperature###########################
library(tidyverse)
# Summarize hourly data to daily mean temperature
ts.data.EMLPOND1.21$date=as.Date(ts.data.EMLPOND1.21$datetime)
EMLPOND1.21.TEMP<- ts.data.EMLPOND1.21 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_0.1, na.rm = TRUE)) #shallowest temp
#summarize(daily_mean_temp = mean(wtr_1.58, na.rm = TRUE)) #minidot temp
EMLPOND1.21.TEMP$Site="EMLPOND1"

ts.data.TOK11.21$date=as.Date(ts.data.TOK11.21$datetime)
TOK11.21.TEMP<- ts.data.TOK11.21 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_0.22, na.rm = TRUE)) #shallowest temp
#summarize(daily_mean_temp = mean(wtr_1.24, na.rm = TRUE)) #minidot temp
TOK11.21.TEMP$Site="TOK11"

ts.data.TOPAZPOND.21$date=as.Date(ts.data.TOPAZPOND.21$datetime)
TOPAZPOND.21.TEMP<- ts.data.TOPAZPOND.21 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_0.8, na.rm = TRUE)) #shallowest temp (typo - actual depth 0.08)
#summarize(daily_mean_temp = mean(wtr_0.98, na.rm = TRUE))  #minidot temp
TOPAZPOND.21.TEMP$Site="TOPAZPOND"

ts.data.TOK30.21$date=as.Date(ts.data.TOK30.21$datetime)
TOK30.21.TEMP<- ts.data.TOK30.21 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_0.28, na.rm = TRUE)) #shallowest temp
#summarize(daily_mean_temp = mean(wtr_0.77, na.rm = TRUE))  #minidot temp
TOK30.21.TEMP$Site="TOK30"

emerald21$date=as.Date(emerald21$datetime)
EMERALD.21.TEMP<- emerald21 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_3.58, na.rm = TRUE)) #shallowest temp
EMERALD.21.TEMP$Site="EMERALD"

topaz21$date=as.Date(topaz21$datetime)
TOPAZ.21.TEMP<- topaz21 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_2.55, na.rm = TRUE)) #shallowest temp
TOPAZ.21.TEMP$Site="TOPAZ"


#merge all datasets
combined_temp <- do.call(rbind, list(EMLPOND1.21.TEMP, TOK11.21.TEMP, TOPAZPOND.21.TEMP, TOK30.21.TEMP, EMERALD.21.TEMP, TOPAZ.21.TEMP))
combined_temp <- combined_temp[with(combined_temp, date >= "2021-07-02" & date <= "2021-10-02"),]

#restrict dates


# Pivot the combined data frame to wide format
temperature_matrix_a <- pivot_wider(data = combined_temp, 
                                  id_cols = Site, 
                                  names_from = date, 
                                  values_from = daily_mean_temp)

# Remove row names if not needed
rownames(temperature_matrix_a) <- NULL

# Convert to matrix
temperature_matrix_a <- as.matrix(temperature_matrix_a)

# Check if there are any NA values in the matrix
if (any(is.na(temperature_matrix_a))) {
  print("There are NA values in the matrix.")
} else {
  print("There are no NA values in the matrix.")
}

#remove site column
temperature_matrix_a <- temperature_matrix_a[, -1, drop = FALSE]

#make sure matrix is as.numeric
temperature_matrix_a <- apply(temperature_matrix_a, 2, as.numeric)



############################### temp, but of logger closest to EML's shallowest#############################
library(tidyverse)
# Summarize hourly data to daily mean temperature
EMLPOND1.21.TEMP.b<- ts.data.EMLPOND1.21 %>%
  group_by(date) %>%
 summarize(daily_mean_temp = mean(wtr_1.58, na.rm = TRUE)) #minidot temp
EMLPOND1.21.TEMP.b$Site="EMLPOND1"

TOK11.21.TEMP.b<- ts.data.TOK11.21 %>%
  group_by(date) %>%
summarize(daily_mean_temp = mean(wtr_1.24, na.rm = TRUE)) #minidot temp
TOK11.21.TEMP.b$Site="TOK11"

TOPAZPOND.21.TEMP.b<- ts.data.TOPAZPOND.21 %>%
  group_by(date) %>%
summarize(daily_mean_temp = mean(wtr_0.98, na.rm = TRUE))  #minidot temp
TOPAZPOND.21.TEMP.b$Site="TOPAZPOND"

TOK30.21.TEMP.b<- ts.data.TOK30.21 %>%
  group_by(date) %>%
summarize(daily_mean_temp = mean(wtr_0.77, na.rm = TRUE))  #minidot temp
TOK30.21.TEMP.b$Site="TOK30"

EMERALD.21.TEMP.b<- emerald21 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_3.58, na.rm = TRUE)) #shallowest temp
EMERALD.21.TEMP.b$Site="EMERALD"

TOPAZ.21.TEMP.b<- topaz21 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_2.55, na.rm = TRUE)) #shallowest temp
TOPAZ.21.TEMP.b$Site="TOPAZ"


#merge all datasets
combined_temp.b <- do.call(rbind, list(EMLPOND1.21.TEMP.b, TOK11.21.TEMP.b, TOPAZPOND.21.TEMP.b, TOK30.21.TEMP.b, EMERALD.21.TEMP.b, TOPAZ.21.TEMP.b))
combined_temp.b <- combined_temp.b[with(combined_temp.b, date >= "2021-07-02" & date <= "2021-10-02"),]

#restrict dates


# Pivot the combined data frame to wide format
temperature_matrix_b <- pivot_wider(data = combined_temp.b, 
                                  id_cols = Site, 
                                  names_from = date, 
                                  values_from = daily_mean_temp)

# Remove row names if not needed
rownames(temperature_matrix_b) <- NULL

# Convert to matrix
temperature_matrix_b <- as.matrix(temperature_matrix_b)

# Check if there are any NA values in the matrix
if (any(is.na(temperature_matrix_b))) {
  print("There are NA values in the matrix.")
} else {
  print("There are no NA values in the matrix.")
}

#remove site column
temperature_matrix_b <- temperature_matrix_b[, -1, drop = FALSE]

#make sure matrix is as.numeric
temperature_matrix_b <- apply(temperature_matrix_b, 2, as.numeric)



######################################

#matrix of daily SW (row 1 EML MET, row 2 Topaz MET)
#can use pond data with sw values already tied to df
#TOK11 = EML MET
#TOPAZPOND = TOPAZ MET


sw_eml<- ts.data.TOK11.21 %>%
  group_by(date) %>%
  summarize(daily_mean_sw = mean(swrad, na.rm = TRUE)) #shallowest temp
sw_eml$Site="EMLMET"


sw_topaz<- ts.data.TOPAZPOND.21 %>%
  group_by(date) %>%
  summarize(daily_mean_sw = mean(swrad, na.rm = TRUE)) #shallowest temp
sw_topaz$Site="TOPAZMET"

sw_topaz$daily_mean_sw = sw_topaz$daily_mean_sw*-1


#merge all datasets
combined_sw <- do.call(rbind, list(sw_eml, sw_topaz))
combined_sw <- combined_sw[with(combined_sw, date >= "2021-07-02" & date <= "2021-10-02"),]

#restrict dates


# Pivot the combined data frame to wide format
sw_matrix <- pivot_wider(data = combined_sw, 
                                  id_cols = Site, 
                                  names_from = date, 
                                  values_from = daily_mean_sw)

# Remove row names if not needed
rownames(sw_matrix) <- NULL

# Convert to matrix
sw_matrix <- as.matrix(sw_matrix)

# Check if there are any NA values in the matrix
if (any(is.na(sw_matrix))) {
  print("There are NA values in the matrix.")
} else {
  print("There are no NA values in the matrix.")
}

#remove site column
sw_matrix <- sw_matrix[, -1, drop = FALSE]


#make sure matrix is as.numeric
sw_matrix <- apply(sw_matrix, 2, as.numeric)








######################################
#matrix of smoke density
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd) 
files <- list.files()#all the files in wd

load('SmokeData_forMJ.Rdata')
smoke_topaz_20 = SmokeData_forMJ [[1]]
smoke_eml_20 = SmokeData_forMJ [[2]]
smoke_topaz_21 = SmokeData_forMJ [[3]]
smoke_eml_21 = SmokeData_forMJ [[4]]

smokedensity=read.csv(file="RAPID_sites_smoke_density.csv")
smokedensity$Date <- as.Date(paste(smokedensity$Year, smokedensity$Month, smokedensity$Day, sep = "-"))

smokedensity = smokedensity [, c("Date", "Emerald.Lake", "Topaz.Lake"), drop=F]

smokedensity.eml = smokedensity [, c("Date", "Emerald.Lake"), drop=F]
smokedensity.eml$Site="EMERALD"
names(smokedensity.eml)[names(smokedensity.eml) == "Emerald.Lake"] <- "SmokeDensity"

smokedensity.topaz = smokedensity [, c("Date", "Topaz.Lake"), drop=F]
smokedensity.topaz$Site="TOPAZ"
names(smokedensity.topaz)[names(smokedensity.topaz) == "Topaz.Lake"] <- "SmokeDensity"


#merge all datasets
combined_smokedensity=merge(smokedensity.eml, smokedensity.topaz, all=T)
combined_smokedensity <- combined_smokedensity[with(combined_smokedensity, Date >= "2021-07-02" & Date <= "2021-10-02"),]



# Pivot the combined data frame to wide format
smokedensity_matrix <- pivot_wider(data = combined_smokedensity, 
                         id_cols = Site, 
                         names_from = Date, 
                         values_from = SmokeDensity)

# Remove row names if not needed
rownames(smokedensity_matrix) <- NULL

# Convert to matrix
smokedensity_matrix <- as.matrix(smokedensity_matrix)

# Check if there are any NA values in the matrix
if (any(is.na(smokedensity_matrix))) {
  print("There are NA values in the matrix.")
} else {
  print("There are no NA values in the matrix.")
}

#remove site column
smokedensity_matrix <- smokedensity_matrix[, -1, drop = FALSE]

#make sure matrix is as.numeric
smokedensity_matrix <- apply(smokedensity_matrix, 2, as.numeric)




################ pm2.5 matrix ######################
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data/timeseries"
setwd(wd) 
pm25=read.csv(file="PM2.5_Tokopah_2021.csv", header=T)
pm25$Date=as.Date(pm25$Date, format = "%m/%d/%Y")
pm25 <- pm25[with(pm25, Date >= "2021-07-02" & Date <= "2021-10-02"),]

pm25_long <- pivot_longer(pm25, 
                          cols = -Date,  # Specify the columns to pivot
                          names_to = "Site",  # Name of the new column for column names
                          values_to = "PM25")  # Name of the new column for values





########################### A (by site); results in NAs in df ##############
# Pivot the combined data frame to wide format
pm25_matrix_a <- pivot_wider(data = pm25_long, 
                        id_cols = Site, 
                      names_from = Date, 
                      values_from = PM25)

# Remove row names if not needed
rownames(pm25_matrix_a) <- NULL

# Convert to matrix
pm25_matrix_a <- as.matrix(pm25_matrix_a)

# Check if there are any NA values in the matrix
if (any(is.na(pm25_matrix_a))) {
  print("There are NA values in the matrix.")
} else {
  print("There are no NA values in the matrix.")
}

#remove site column
pm25_matrix_a <- pm25_matrix_a[, -1, drop = FALSE]

#make sure matrix is as.numeric
pm25_matrix_a <- apply(pm25_matrix_a, 2, as.numeric)



################### b (means)################
pm25_mean=pm25
pm25_mean$meanpm25 = rowMeans(pm25_mean[,c("RiverHouse_PM2.5_ug.m3", "Lodgepole_PM2.5_ug.m3" , "Hammond_PM2.5_ug.m3" )], na.rm=T)
pm25_mean <- pm25_mean[complete.cases(pm25_mean$meanpm25), ]
pm25_mean$RiverHouse_PM2.5_ug.m3=NULL
pm25_mean$Lodgepole_PM2.5_ug.m3=NULL
pm25_mean$Hammond_PM2.5_ug.m3=NULL

# Convert from long to wide format
pm25_matrix_b <- pivot_wider(pm25_mean, names_from = Date, values_from = meanpm25)


# Convert to matrix
pm25_matrix_b <- as.matrix(pm25_matrix_b)

# Check if there are any NA values in the matrix
if (any(is.na(pm25_matrix_b))) {
  print("There are NA values in the matrix.")
} else {
  print("There are no NA values in the matrix.")
}


################ c (just riverhouse site) ####################
pm25_river=pm25
pm25_river$Lodgepole_PM2.5_ug.m3=NULL
pm25_river$Hammond_PM2.5_ug.m3=NULL

# Convert from long to wide format
pm25_matrix_c <- pivot_wider(pm25_river, names_from = Date, values_from = RiverHouse_PM2.5_ug.m3)


# Convert to matrix
pm25_matrix_c <- as.matrix(pm25_matrix_c)

# Check if there are any NA values in the matrix
if (any(is.na(pm25_matrix_c))) {
  print("There are NA values in the matrix.")
} else {
  print("There are no NA values in the matrix.")
}


#save all matrices in a single .Rdata file
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data/timeseries"
setwd(wd)

save(temperature_matrix_a, temperature_matrix_b, sw_matrix, smokedensity_matrix, pm25_matrix_a, pm25_matrix_b, pm25_matrix_c, file = "ts_matrices.Rdata")









#########################################################################################################################
#########################################################################################################################
#load pond temperature time series data 2020

wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data/ponds"
setwd(wd)

#Choose year 
year <- "S20" #desired open water period, including season


#EMLPOND1

lake <- 'EMLPOND1' 

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
# bookkeep.EMLPOND1.20 <- outputs[[1]]
# kalman.EMLPOND1.20 <- outputs[[2]]
# bayesian.EMLPOND1.20 <- outputs[[3]]
ts.data.EMLPOND1.20 <- outputs[[4]]
# DO_depth.EMLPOND1.20 <- outputs[[5]]



#load pond data - TOK11
lake <- 'TOK11' 

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
# bookkeep.TOK11.20 <- outputs[[1]]
# kalman.TOK11.20 <- outputs[[2]]
# bayesian.TOK11.20 <- outputs[[3]]
ts.data.TOK11.20 <- outputs[[4]]
# DO_depth.TOK11.20 <- outputs[[5]]



#load pond data - TOPAZPOND
lake <- 'TOPAZPOND'

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
# bookkeep.TOPAZPOND.20 <- outputs[[1]]
# kalman.TOPAZPOND.20 <- outputs[[2]]
# bayesian.TOPAZPOND.20 <- outputs[[3]]
ts.data.TOPAZPOND.20 <- outputs[[4]]
# DO_depth.TOPAZPOND.20 <- outputs[[5]]



#load pond data - TOK30
lake <- 'TOK30' 

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
# bookkeep.TOK30.20 <- outputs[[1]]
# kalman.TOK30.20 <- outputs[[2]]
# bayesian.TOK30.20 <- outputs[[3]]
ts.data.TOK30.20 <- outputs[[4]]
# DO_depth.TOK30.20 <- outputs[[5]]


#load lake data
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd) 
files <- list.files()

load('Emerald_Topaz_sensor_data_2020_forMJ.Rdata')
emerald20= ts.2020.forMJ  [[1]]
topaz20 = ts.2020.forMJ  [[2]]

files <- list.files()
load('Emerald_2020_winter_data.Rdata')
emerald1920 = output.list [[1]]


#emerald 20 shallowest temp is 3.58; adjust colnames in emerald1920 to match so it merges into same column
colnames(emerald1920)[colnames(emerald1920) == "temp_3.47"] <- "wtr_3.58"
# Keep only datetime and wtr_3.58 columns
emerald1920 <- emerald1920[, c("datetime", "wtr_3.58")]

emerald20=merge(emerald20, emerald1920, all=T)


files <- list.files()
load('Topaz_2020_winter_data.Rdata')
topaz1920 = output.list [[1]]


#Topaz 20 shallowest temp is 1.91; closest to 3 is 2.78; adjust colnames in Topaz1920 to match so it merges into same column
colnames(topaz1920)[colnames(topaz1920) == "temp_2.8"] <- "wtr_2.78"
colnames(topaz1920)[colnames(topaz1920) == "temp_2.19"] <- "wtr_1.91"

# Keep only datetime and wtr_2.78 and wtr_1.91 columns
topaz1920 <- topaz1920[, c("datetime", "wtr_2.78","wtr_1.91")]

topaz20=merge(topaz20, topaz1920, all=T)
########################################################################################################################
############## temperature###########################
library(tidyverse)
# Summarize hourly data to daily mean temperature
ts.data.EMLPOND1.20$date=as.Date(ts.data.EMLPOND1.20$datetime)
EMLPOND1.20.TEMP<- ts.data.EMLPOND1.20 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_0.2, na.rm = TRUE)) #shallowest temp
#summarize(daily_mean_temp = mean(wtr_1.58, na.rm = TRUE)) #minidot temp
EMLPOND1.20.TEMP$Site="EMLPOND1"

ts.data.TOK11.20$date=as.Date(ts.data.TOK11.20$datetime)
TOK11.20.TEMP<- ts.data.TOK11.20 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_0.22, na.rm = TRUE)) #shallowest temp
#summarize(daily_mean_temp = mean(wtr_1.24, na.rm = TRUE)) #minidot temp
TOK11.20.TEMP$Site="TOK11"

ts.data.TOPAZPOND.20$date=as.Date(ts.data.TOPAZPOND.20$datetime)
TOPAZPOND.20.TEMP<- ts.data.TOPAZPOND.20 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_0.08, na.rm = TRUE)) #shallowest temp 
#summarize(daily_mean_temp = mean(wtr_0.98, na.rm = TRUE))  #minidot temp
TOPAZPOND.20.TEMP$Site="TOPAZPOND"

ts.data.TOK30.20$date=as.Date(ts.data.TOK30.20$datetime)
TOK30.20.TEMP<- ts.data.TOK30.20 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_0.28, na.rm = TRUE)) #shallowest temp
#summarize(daily_mean_temp = mean(wtr_0.77, na.rm = TRUE))  #minidot temp
TOK30.20.TEMP$Site="TOK30"

emerald20$date=as.Date(emerald20$datetime)
EMERALD.20.TEMP<- emerald20 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_3.58, na.rm = TRUE)) #shallowest temp
EMERALD.20.TEMP$Site="EMERALD"

topaz20$date=as.Date(topaz20$datetime)
TOPAZ.20.TEMP<- topaz20 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_1.91, na.rm = TRUE))  #shallowest temp
  #  summarize(daily_mean_temp = mean(wtr_2.78, na.rm = TRUE)) #closest to 3m

TOPAZ.20.TEMP$Site="TOPAZ"


#merge all datasets
combined_temp <- do.call(rbind, list(EMLPOND1.20.TEMP, TOK11.20.TEMP, TOPAZPOND.20.TEMP, TOK30.20.TEMP, EMERALD.20.TEMP, TOPAZ.20.TEMP))
combined_temp <- combined_temp[with(combined_temp, date >= "2020-07-02" & date <= "2020-10-02"),]

#restrict dates


# Pivot the combined data frame to wide format
temperature_matrix_a <- pivot_wider(data = combined_temp, 
                                    id_cols = Site, 
                                    names_from = date, 
                                    values_from = daily_mean_temp)

# Remove row names if not needed
rownames(temperature_matrix_a) <- NULL

# Convert to matrix
temperature_matrix_a <- as.matrix(temperature_matrix_a)

# Check if there are any NA values in the matrix
if (any(is.na(temperature_matrix_a))) {
  print("There are NA values in the matrix.")
} else {
  print("There are no NA values in the matrix.")
}

#remove site column
temperature_matrix_a <- temperature_matrix_a[, -1, drop = FALSE]

#make sure matrix is as.numeric
temperature_matrix_a <- apply(temperature_matrix_a, 2, as.numeric)



############################### temp, but of logger closest to EML's shallowest#############################
library(tidyverse)
# Summarize hourly data to daily mean temperature
EMLPOND1.20.TEMP.b<- ts.data.EMLPOND1.20 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_1.58, na.rm = TRUE)) #minidot temp
EMLPOND1.20.TEMP.b$Site="EMLPOND1"

TOK11.20.TEMP.b<- ts.data.TOK11.20 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_1.24, na.rm = TRUE)) #minidot temp
TOK11.20.TEMP.b$Site="TOK11"

TOPAZPOND.20.TEMP.b<- ts.data.TOPAZPOND.20 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_0.98, na.rm = TRUE))  #minidot temp
TOPAZPOND.20.TEMP.b$Site="TOPAZPOND"

TOK30.20.TEMP.b<- ts.data.TOK30.20 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_0.77, na.rm = TRUE))  #minidot temp
TOK30.20.TEMP.b$Site="TOK30"

EMERALD.20.TEMP.b<- emerald20 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_3.58, na.rm = TRUE)) #shallowest temp
EMERALD.20.TEMP.b$Site="EMERALD"

TOPAZ.20.TEMP.b<- topaz20 %>%
  group_by(date) %>%
    summarize(daily_mean_temp = mean(wtr_2.78, na.rm = TRUE)) #closest to 3m
  TOPAZ.20.TEMP.b$Site="TOPAZ"


#merge all datasets
combined_temp.b <- do.call(rbind, list(EMLPOND1.20.TEMP.b, TOK11.20.TEMP.b, TOPAZPOND.20.TEMP.b, TOK30.20.TEMP.b, EMERALD.20.TEMP.b, TOPAZ.20.TEMP.b))
combined_temp.b <- combined_temp.b[with(combined_temp.b, date >= "2020-07-02" & date <= "2020-10-02"),]

#restrict dates


# Pivot the combined data frame to wide format
temperature_matrix_b <- pivot_wider(data = combined_temp.b, 
                                    id_cols = Site, 
                                    names_from = date, 
                                    values_from = daily_mean_temp)

# Remove row names if not needed
rownames(temperature_matrix_b) <- NULL

# Convert to matrix
temperature_matrix_b <- as.matrix(temperature_matrix_b)

# Check if there are any NA values in the matrix
if (any(is.na(temperature_matrix_b))) {
  print("There are NA values in the matrix.")
} else {
  print("There are no NA values in the matrix.")
}

#remove site column
temperature_matrix_b <- temperature_matrix_b[, -1, drop = FALSE]

#make sure matrix is as.numeric
temperature_matrix_b <- apply(temperature_matrix_b, 2, as.numeric)



######################################

#matrix of daily SW (row 1 EML MET, row 2 Topaz MET)
#can use pond data with sw values already tied to df
#TOK11 = EML MET
#TOPAZPOND = TOPAZ MET


sw_eml<- ts.data.TOK11.20 %>%
  group_by(date) %>%
  summarize(daily_mean_sw = mean(swrad, na.rm = TRUE)) #shallowest temp
sw_eml$Site="EMLMET"


sw_topaz<- ts.data.TOPAZPOND.20 %>%
  group_by(date) %>%
  summarize(daily_mean_sw = mean(swrad, na.rm = TRUE)) #shallowest temp
sw_topaz$Site="TOPAZMET"

sw_topaz$daily_mean_sw = sw_topaz$daily_mean_sw*-1


#merge all datasets
combined_sw <- do.call(rbind, list(sw_eml, sw_topaz))
combined_sw <- combined_sw[with(combined_sw, date >= "2020-07-02" & date <= "2020-10-02"),]

#restrict dates


# Pivot the combined data frame to wide format
sw_matrix <- pivot_wider(data = combined_sw, 
                         id_cols = Site, 
                         names_from = date, 
                         values_from = daily_mean_sw)

# Remove row names if not needed
rownames(sw_matrix) <- NULL

# Convert to matrix
sw_matrix <- as.matrix(sw_matrix)

# Check if there are any NA values in the matrix
if (any(is.na(sw_matrix))) {
  print("There are NA values in the matrix.")
} else {
  print("There are no NA values in the matrix.")
}

#remove site column
sw_matrix <- sw_matrix[, -1, drop = FALSE]


#make sure matrix is as.numeric
sw_matrix <- apply(sw_matrix, 2, as.numeric)








######################################
#matrix of smoke density
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd) 
files <- list.files()#all the files in wd

load('SmokeData_forMJ.Rdata')
smoke_topaz_20 = SmokeData_forMJ [[1]]
smoke_eml_20 = SmokeData_forMJ [[2]]


smokedensity=read.csv(file="RAPID_sites_smoke_density.csv")
smokedensity$Date <- as.Date(paste(smokedensity$Year, smokedensity$Month, smokedensity$Day, sep = "-"))

smokedensity = smokedensity [, c("Date", "Emerald.Lake", "Topaz.Lake"), drop=F]

smokedensity.eml = smokedensity [, c("Date", "Emerald.Lake"), drop=F]
smokedensity.eml$Site="EMERALD"
names(smokedensity.eml)[names(smokedensity.eml) == "Emerald.Lake"] <- "SmokeDensity"

smokedensity.topaz = smokedensity [, c("Date", "Topaz.Lake"), drop=F]
smokedensity.topaz$Site="TOPAZ"
names(smokedensity.topaz)[names(smokedensity.topaz) == "Topaz.Lake"] <- "SmokeDensity"


#merge all datasets
combined_smokedensity=merge(smokedensity.eml, smokedensity.topaz, all=T)
combined_smokedensity <- combined_smokedensity[with(combined_smokedensity, Date >= "2020-07-02" & Date <= "2020-10-02"),]

#data is missing 7/8 - add this in manually
missing_rows <- data.frame(
  Date = c("2020-07-08", "2020-07-08"),
  SmokeDensity = c(0, 0),
  Site = c("EMERALD", "TOPAZ")
)
missing_rows$Date=as.Date(missing_rows$Date)

# Add the new rows to the original dataframe using rbind
combined_smokedensity <- merge(combined_smokedensity, missing_rows, all=T)


# Pivot the combined data frame to wide format
smokedensity_matrix <- pivot_wider(data = combined_smokedensity, 
                                   id_cols = Site, 
                                   names_from = Date, 
                                   values_from = SmokeDensity)

# Remove row names if not needed
rownames(smokedensity_matrix) <- NULL

# Convert to matrix
smokedensity_matrix <- as.matrix(smokedensity_matrix)

# Check if there are any NA values in the matrix
if (any(is.na(smokedensity_matrix))) {
  print("There are NA values in the matrix.")
} else {
  print("There are no NA values in the matrix.")
}

#remove site column
smokedensity_matrix <- smokedensity_matrix[, -1, drop = FALSE]

#make sure matrix is as.numeric
smokedensity_matrix <- apply(smokedensity_matrix, 2, as.numeric)






#save all matrices in a single .Rdata file
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data/timeseries"
setwd(wd)

save(temperature_matrix_a, temperature_matrix_b, sw_matrix, smokedensity_matrix, file = "ts_matrices_2020.Rdata")




























##2020 with dummy datasets for PM2.5
#########################################################################################################################
#########################################################################################################################
#load pond temperature time series data 2020

wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data/ponds"
setwd(wd)

#Choose year 
year <- "S20" #desired open water period, including season


#EMLPOND1

lake <- 'EMLPOND1' 

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
# bookkeep.EMLPOND1.20 <- outputs[[1]]
# kalman.EMLPOND1.20 <- outputs[[2]]
# bayesian.EMLPOND1.20 <- outputs[[3]]
ts.data.EMLPOND1.20 <- outputs[[4]]
# DO_depth.EMLPOND1.20 <- outputs[[5]]



#load pond data - TOK11
lake <- 'TOK11' 

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
# bookkeep.TOK11.20 <- outputs[[1]]
# kalman.TOK11.20 <- outputs[[2]]
# bayesian.TOK11.20 <- outputs[[3]]
ts.data.TOK11.20 <- outputs[[4]]
# DO_depth.TOK11.20 <- outputs[[5]]



#load pond data - TOPAZPOND
lake <- 'TOPAZPOND'

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
# bookkeep.TOPAZPOND.20 <- outputs[[1]]
# kalman.TOPAZPOND.20 <- outputs[[2]]
# bayesian.TOPAZPOND.20 <- outputs[[3]]
ts.data.TOPAZPOND.20 <- outputs[[4]]
# DO_depth.TOPAZPOND.20 <- outputs[[5]]



#load pond data - TOK30
lake <- 'TOK30' 

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
# bookkeep.TOK30.20 <- outputs[[1]]
# kalman.TOK30.20 <- outputs[[2]]
# bayesian.TOK30.20 <- outputs[[3]]
ts.data.TOK30.20 <- outputs[[4]]
# DO_depth.TOK30.20 <- outputs[[5]]


#load lake data
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd) 
files <- list.files()

load('Emerald_Topaz_sensor_data_2020_forMJ.Rdata')
emerald20= ts.2020.forMJ  [[1]]
topaz20 = ts.2020.forMJ  [[2]]

files <- list.files()
load('Emerald_2020_winter_data.Rdata')
emerald1920 = output.list [[1]]


#emerald 20 shallowest temp is 3.58; adjust colnames in emerald1920 to match so it merges into same column
colnames(emerald1920)[colnames(emerald1920) == "temp_3.47"] <- "wtr_3.58"
# Keep only datetime and wtr_3.58 columns
emerald1920 <- emerald1920[, c("datetime", "wtr_3.58")]

emerald20=merge(emerald20, emerald1920, all=T)


files <- list.files()
load('Topaz_2020_winter_data.Rdata')
topaz1920 = output.list [[1]]


#Topaz 20 shallowest temp is 1.91; closest to 3 is 2.78; adjust colnames in Topaz1920 to match so it merges into same column
colnames(topaz1920)[colnames(topaz1920) == "temp_2.8"] <- "wtr_2.78"
colnames(topaz1920)[colnames(topaz1920) == "temp_2.19"] <- "wtr_1.91"

# Keep only datetime and wtr_2.78 and wtr_1.91 columns
topaz1920 <- topaz1920[, c("datetime", "wtr_2.78","wtr_1.91")]

topaz20=merge(topaz20, topaz1920, all=T)
########################################################################################################################
############## temperature###########################
library(tidyverse)
# Summarize hourly data to daily mean temperature
ts.data.EMLPOND1.20$date=as.Date(ts.data.EMLPOND1.20$datetime)
EMLPOND1.20.TEMP<- ts.data.EMLPOND1.20 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_0.2, na.rm = TRUE)) #shallowest temp
#summarize(daily_mean_temp = mean(wtr_1.58, na.rm = TRUE)) #minidot temp
EMLPOND1.20.TEMP$Site="EMLPOND1"

ts.data.TOK11.20$date=as.Date(ts.data.TOK11.20$datetime)
TOK11.20.TEMP<- ts.data.TOK11.20 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_0.22, na.rm = TRUE)) #shallowest temp
#summarize(daily_mean_temp = mean(wtr_1.24, na.rm = TRUE)) #minidot temp
TOK11.20.TEMP$Site="TOK11"

ts.data.TOPAZPOND.20$date=as.Date(ts.data.TOPAZPOND.20$datetime)
TOPAZPOND.20.TEMP<- ts.data.TOPAZPOND.20 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_0.08, na.rm = TRUE)) #shallowest temp 
#summarize(daily_mean_temp = mean(wtr_0.98, na.rm = TRUE))  #minidot temp
TOPAZPOND.20.TEMP$Site="TOPAZPOND"

ts.data.TOK30.20$date=as.Date(ts.data.TOK30.20$datetime)
TOK30.20.TEMP<- ts.data.TOK30.20 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_0.28, na.rm = TRUE)) #shallowest temp
#summarize(daily_mean_temp = mean(wtr_0.77, na.rm = TRUE))  #minidot temp
TOK30.20.TEMP$Site="TOK30"

emerald20$date=as.Date(emerald20$datetime)
EMERALD.20.TEMP<- emerald20 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_3.58, na.rm = TRUE)) #shallowest temp
EMERALD.20.TEMP$Site="EMERALD"

topaz20$date=as.Date(topaz20$datetime)
TOPAZ.20.TEMP<- topaz20 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_1.91, na.rm = TRUE))  #shallowest temp
#  summarize(daily_mean_temp = mean(wtr_2.78, na.rm = TRUE)) #closest to 3m

TOPAZ.20.TEMP$Site="TOPAZ"


#merge all datasets
combined_temp <- do.call(rbind, list(EMLPOND1.20.TEMP, TOK11.20.TEMP, TOPAZPOND.20.TEMP, TOK30.20.TEMP, EMERALD.20.TEMP, TOPAZ.20.TEMP))
combined_temp <- combined_temp[with(combined_temp, date >= "2020-07-02" & date <= "2020-10-02"),]

#restrict dates


# Pivot the combined data frame to wide format
temperature_matrix_a <- pivot_wider(data = combined_temp, 
                                    id_cols = Site, 
                                    names_from = date, 
                                    values_from = daily_mean_temp)

# Remove row names if not needed
rownames(temperature_matrix_a) <- NULL

# Convert to matrix
temperature_matrix_a <- as.matrix(temperature_matrix_a)

# Check if there are any NA values in the matrix
if (any(is.na(temperature_matrix_a))) {
  print("There are NA values in the matrix.")
} else {
  print("There are no NA values in the matrix.")
}

#remove site column
temperature_matrix_a <- temperature_matrix_a[, -1, drop = FALSE]

#make sure matrix is as.numeric
temperature_matrix_a <- apply(temperature_matrix_a, 2, as.numeric)



############################### temp, but of logger closest to EML's shallowest#############################
library(tidyverse)
# Summarize hourly data to daily mean temperature
EMLPOND1.20.TEMP.b<- ts.data.EMLPOND1.20 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_1.58, na.rm = TRUE)) #minidot temp
EMLPOND1.20.TEMP.b$Site="EMLPOND1"

TOK11.20.TEMP.b<- ts.data.TOK11.20 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_1.24, na.rm = TRUE)) #minidot temp
TOK11.20.TEMP.b$Site="TOK11"

TOPAZPOND.20.TEMP.b<- ts.data.TOPAZPOND.20 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_0.98, na.rm = TRUE))  #minidot temp
TOPAZPOND.20.TEMP.b$Site="TOPAZPOND"

TOK30.20.TEMP.b<- ts.data.TOK30.20 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_0.77, na.rm = TRUE))  #minidot temp
TOK30.20.TEMP.b$Site="TOK30"

EMERALD.20.TEMP.b<- emerald20 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_3.58, na.rm = TRUE)) #shallowest temp
EMERALD.20.TEMP.b$Site="EMERALD"

TOPAZ.20.TEMP.b<- topaz20 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_2.78, na.rm = TRUE)) #closest to 3m
TOPAZ.20.TEMP.b$Site="TOPAZ"


#merge all datasets
combined_temp.b <- do.call(rbind, list(EMLPOND1.20.TEMP.b, TOK11.20.TEMP.b, TOPAZPOND.20.TEMP.b, TOK30.20.TEMP.b, EMERALD.20.TEMP.b, TOPAZ.20.TEMP.b))
combined_temp.b <- combined_temp.b[with(combined_temp.b, date >= "2020-07-02" & date <= "2020-10-02"),]

#restrict dates


# Pivot the combined data frame to wide format
temperature_matrix_b <- pivot_wider(data = combined_temp.b, 
                                    id_cols = Site, 
                                    names_from = date, 
                                    values_from = daily_mean_temp)

# Remove row names if not needed
rownames(temperature_matrix_b) <- NULL

# Convert to matrix
temperature_matrix_b <- as.matrix(temperature_matrix_b)

# Check if there are any NA values in the matrix
if (any(is.na(temperature_matrix_b))) {
  print("There are NA values in the matrix.")
} else {
  print("There are no NA values in the matrix.")
}

#remove site column
temperature_matrix_b <- temperature_matrix_b[, -1, drop = FALSE]

#make sure matrix is as.numeric
temperature_matrix_b <- apply(temperature_matrix_b, 2, as.numeric)



######################################

#matrix of daily SW (row 1 EML MET, row 2 Topaz MET)
#can use pond data with sw values already tied to df
#TOK11 = EML MET
#TOPAZPOND = TOPAZ MET


sw_eml<- ts.data.TOK11.20 %>%
  group_by(date) %>%
  summarize(daily_mean_sw = mean(swrad, na.rm = TRUE)) #shallowest temp
sw_eml$Site="EMLMET"


sw_topaz<- ts.data.TOPAZPOND.20 %>%
  group_by(date) %>%
  summarize(daily_mean_sw = mean(swrad, na.rm = TRUE)) #shallowest temp
sw_topaz$Site="TOPAZMET"

sw_topaz$daily_mean_sw = sw_topaz$daily_mean_sw*-1


#merge all datasets
combined_sw <- do.call(rbind, list(sw_eml, sw_topaz))
combined_sw <- combined_sw[with(combined_sw, date >= "2020-07-02" & date <= "2020-10-02"),]

#restrict dates


# Pivot the combined data frame to wide format
sw_matrix <- pivot_wider(data = combined_sw, 
                         id_cols = Site, 
                         names_from = date, 
                         values_from = daily_mean_sw)

# Remove row names if not needed
rownames(sw_matrix) <- NULL

# Convert to matrix
sw_matrix <- as.matrix(sw_matrix)

# Check if there are any NA values in the matrix
if (any(is.na(sw_matrix))) {
  print("There are NA values in the matrix.")
} else {
  print("There are no NA values in the matrix.")
}

#remove site column
sw_matrix <- sw_matrix[, -1, drop = FALSE]


#make sure matrix is as.numeric
sw_matrix <- apply(sw_matrix, 2, as.numeric)








######################################
#matrix of smoke density
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd) 
files <- list.files()#all the files in wd

load('SmokeData_forMJ.Rdata')
smoke_topaz_20 = SmokeData_forMJ [[1]]
smoke_eml_20 = SmokeData_forMJ [[2]]


smokedensity=read.csv(file="RAPID_sites_smoke_density.csv")
smokedensity$Date <- as.Date(paste(smokedensity$Year, smokedensity$Month, smokedensity$Day, sep = "-"))

smokedensity = smokedensity [, c("Date", "Emerald.Lake", "Topaz.Lake"), drop=F]

smokedensity.eml = smokedensity [, c("Date", "Emerald.Lake"), drop=F]
smokedensity.eml$Site="EMERALD"
names(smokedensity.eml)[names(smokedensity.eml) == "Emerald.Lake"] <- "SmokeDensity"

smokedensity.topaz = smokedensity [, c("Date", "Topaz.Lake"), drop=F]
smokedensity.topaz$Site="TOPAZ"
names(smokedensity.topaz)[names(smokedensity.topaz) == "Topaz.Lake"] <- "SmokeDensity"


#merge all datasets
combined_smokedensity=merge(smokedensity.eml, smokedensity.topaz, all=T)
combined_smokedensity <- combined_smokedensity[with(combined_smokedensity, Date >= "2020-07-02" & Date <= "2020-10-02"),]

#data is missing 7/8 - add this in manually
missing_rows <- data.frame(
  Date = c("2020-07-08", "2020-07-08"),
  SmokeDensity = c(0, 0),
  Site = c("EMERALD", "TOPAZ")
)
missing_rows$Date=as.Date(missing_rows$Date)

# Add the new rows to the original dataframe using rbind
combined_smokedensity <- merge(combined_smokedensity, missing_rows, all=T)

# Pivot the combined data frame to wide format
smokedensity_matrix <- pivot_wider(data = combined_smokedensity, 
                                   id_cols = Site, 
                                   names_from = Date, 
                                   values_from = SmokeDensity)

# Remove row names if not needed
rownames(smokedensity_matrix) <- NULL

# Convert to matrix
smokedensity_matrix <- as.matrix(smokedensity_matrix)

# Check if there are any NA values in the matrix
if (any(is.na(smokedensity_matrix))) {
  print("There are NA values in the matrix.")
} else {
  print("There are no NA values in the matrix.")
}

#remove site column
smokedensity_matrix <- smokedensity_matrix[, -1, drop = FALSE]

#make sure matrix is as.numeric
smokedensity_matrix <- apply(smokedensity_matrix, 2, as.numeric)




################ pm2.5 matrix ######################
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data/timeseries"
setwd(wd) 
pm25=read.csv(file="PM2.5_Tokopah_2021.csv", header=T)
pm25$Date=as.Date(pm25$Date, format = "%m/%d/%Y")
pm25 <- pm25[with(pm25, Date >= "2021-07-02" & Date <= "2021-10-02"),]

pm25_long <- pivot_longer(pm25, 
                          cols = -Date,  # Specify the columns to pivot
                          names_to = "Site",  # Name of the new column for column names
                          values_to = "PM25")  # Name of the new column for values




#######load up PM2.5 data as a placeholder but don't run################
########################### A (by site); results in NAs in df ##############
# Pivot the combined data frame to wide format
pm25_matrix_a <- pivot_wider(data = pm25_long, 
                             id_cols = Site, 
                             names_from = Date, 
                             values_from = PM25)

# Remove row names if not needed
rownames(pm25_matrix_a) <- NULL

# Convert to matrix
pm25_matrix_a <- as.matrix(pm25_matrix_a)

# Check if there are any NA values in the matrix
if (any(is.na(pm25_matrix_a))) {
  print("There are NA values in the matrix.")
} else {
  print("There are no NA values in the matrix.")
}

#remove site column
pm25_matrix_a <- pm25_matrix_a[, -1, drop = FALSE]

#make sure matrix is as.numeric
pm25_matrix_a <- apply(pm25_matrix_a, 2, as.numeric)



################### b (means)################
pm25_mean=pm25
pm25_mean$meanpm25 = rowMeans(pm25_mean[,c("RiverHouse_PM2.5_ug.m3", "Lodgepole_PM2.5_ug.m3" , "Hammond_PM2.5_ug.m3" )], na.rm=T)
pm25_mean <- pm25_mean[complete.cases(pm25_mean$meanpm25), ]
pm25_mean$RiverHouse_PM2.5_ug.m3=NULL
pm25_mean$Lodgepole_PM2.5_ug.m3=NULL
pm25_mean$Hammond_PM2.5_ug.m3=NULL

# Convert from long to wide format
pm25_matrix_b <- pivot_wider(pm25_mean, names_from = Date, values_from = meanpm25)


# Convert to matrix
pm25_matrix_b <- as.matrix(pm25_matrix_b)

# Check if there are any NA values in the matrix
if (any(is.na(pm25_matrix_b))) {
  print("There are NA values in the matrix.")
} else {
  print("There are no NA values in the matrix.")
}


################ c (just riverhouse site) ####################
pm25_river=pm25
pm25_river$Lodgepole_PM2.5_ug.m3=NULL
pm25_river$Hammond_PM2.5_ug.m3=NULL

# Convert from long to wide format
pm25_matrix_c <- pivot_wider(pm25_river, names_from = Date, values_from = RiverHouse_PM2.5_ug.m3)


# Convert to matrix
pm25_matrix_c <- as.matrix(pm25_matrix_c)

# Check if there are any NA values in the matrix
if (any(is.na(pm25_matrix_c))) {
  print("There are NA values in the matrix.")
} else {
  print("There are no NA values in the matrix.")
}



#save all matrices in a single .Rdata file
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data/timeseries"
setwd(wd)

save(temperature_matrix_a, temperature_matrix_b, sw_matrix, smokedensity_matrix, pm25_matrix_a, pm25_matrix_b, pm25_matrix_c, file = "ts_matrices_2020_b.Rdata")















#2022
##########################################################################################################################
#load pond temperature time series data 2021 

wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data/ponds"
setwd(wd)

#Choose year 
year <- "S22" #desired open water period, including season


#EMLPOND1

lake <- 'EMLPOND1' 

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
# bookkeep.EMLPOND1.22 <- outputs[[1]]
# kalman.EMLPOND1.22 <- outputs[[2]]
# bayesian.EMLPOND1.22 <- outputs[[3]]
ts.data.EMLPOND1.22 <- outputs[[4]]
# DO_depth.EMLPOND1.22 <- outputs[[5]]



#load pond data - TOK11
lake <- 'TOK11' 

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
# bookkeep.TOK11.22 <- outputs[[1]]
# kalman.TOK11.22 <- outputs[[2]]
# bayesian.TOK11.22 <- outputs[[3]]
ts.data.TOK11.22 <- outputs[[4]]
# DO_depth.TOK11.22 <- outputs[[5]]



#load pond data - TOPAZPOND
lake <- 'TOPAZPOND'

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
# bookkeep.TOPAZPOND.22 <- outputs[[1]]
# kalman.TOPAZPOND.22 <- outputs[[2]]
# bayesian.TOPAZPOND.22 <- outputs[[3]]
ts.data.TOPAZPOND.22 <- outputs[[4]]
# DO_depth.TOPAZPOND.22 <- outputs[[5]]



#load pond data - TOK30
lake <- 'TOK30' 

#Load LakeMetabolizer inputs for desired year and lake (ts.data):
files <- list.files()#all the files in wd
load(files[grep(paste(lake,year,'LakeMetabolizer_outputs.Rdata',sep="_"),files)])
# bookkeep.TOK30.22 <- outputs[[1]]
# kalman.TOK30.22 <- outputs[[2]]
# bayesian.TOK30.22 <- outputs[[3]]
ts.data.TOK30.22 <- outputs[[4]]
# DO_depth.TOK30.22 <- outputs[[5]]


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


########################################################################################################################
############## temperature###########################
library(tidyverse)
# Summarize hourly data to daily mean temperature
ts.data.EMLPOND1.22$date=as.Date(ts.data.EMLPOND1.22$datetime)
EMLPOND1.22.TEMP<- ts.data.EMLPOND1.22 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_1.58, na.rm = TRUE)) #shallowest temp
#summarize(daily_mean_temp = mean(wtr_2.8, na.rm = TRUE)) #minidot temp
EMLPOND1.22.TEMP$Site="EMLPOND1"

ts.data.TOK11.22$date=as.Date(ts.data.TOK11.22$datetime)
TOK11.22.TEMP<- ts.data.TOK11.22 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_2, na.rm = TRUE)) #shallowest temp
#summarize(daily_mean_temp = mean(wtr_1.24, na.rm = TRUE)) #minidot temp
TOK11.22.TEMP$Site="TOK11"

ts.data.TOPAZPOND.22$date=as.Date(ts.data.TOPAZPOND.22$datetime)
TOPAZPOND.22.TEMP<- ts.data.TOPAZPOND.22 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_0.98, na.rm = TRUE)) #shallowest temp (typo - actual depth 0.08)
#summarize(daily_mean_temp = mean(wtr_1.66, na.rm = TRUE))  #minidot temp
TOPAZPOND.22.TEMP$Site="TOPAZPOND"

ts.data.TOK30.22$date=as.Date(ts.data.TOK30.22$datetime)
TOK30.22.TEMP<- ts.data.TOK30.22 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_0.77, na.rm = TRUE)) #shallowest temp
#summarize(daily_mean_temp = mean(wtr_1.34, na.rm = TRUE))  #minidot temp
TOK30.22.TEMP$Site="TOK30"

ts.data.emerald.22$date=as.Date(ts.data.emerald.22$datetime)
EMERALD.22.TEMP<- ts.data.emerald.22 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_3.47, na.rm = TRUE)) #shallowest temp
EMERALD.22.TEMP$Site="EMERALD"

ts.data.topaz.22$date=as.Date(ts.data.topaz.22$datetime)
TOPAZ.22.TEMP<- ts.data.topaz.22 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_3.19, na.rm = TRUE)) #shallowest temp
TOPAZ.22.TEMP$Site="TOPAZ"


#merge all datasets
combined_temp <- do.call(rbind, list(EMLPOND1.22.TEMP, TOK11.22.TEMP, TOPAZPOND.22.TEMP, TOK30.22.TEMP, EMERALD.22.TEMP, TOPAZ.22.TEMP))
combined_temp <- combined_temp[with(combined_temp, date >= "2022-07-02" & date <= "2022-10-02"),]

#restrict dates


# Pivot the combined data frame to wide format
temperature_matrix_a <- pivot_wider(data = combined_temp, 
                                    id_cols = Site, 
                                    names_from = date, 
                                    values_from = daily_mean_temp)

# Remove row names if not needed
rownames(temperature_matrix_a) <- NULL

# Convert to matrix
temperature_matrix_a <- as.matrix(temperature_matrix_a)

# Check if there are any NA values in the matrix
if (any(is.na(temperature_matrix_a))) {
  print("There are NA values in the matrix.")
} else {
  print("There are no NA values in the matrix.")
}

#remove site column
temperature_matrix_a <- temperature_matrix_a[, -1, drop = FALSE]

#make sure matrix is as.numeric
temperature_matrix_a <- apply(temperature_matrix_a, 2, as.numeric)



############################### temp, but of logger closest to EML's shallowest#############################
library(tidyverse)
# Summarize hourly data to daily mean temperature
EMLPOND1.22.TEMP.b<- ts.data.EMLPOND1.22 %>%
  group_by(date) %>%
#  summarize(daily_mean_temp = mean(wtr_1.58, na.rm = TRUE)) #shallowest temp
summarize(daily_mean_temp = mean(wtr_2.8, na.rm = TRUE)) #minidot temp
EMLPOND1.22.TEMP.b$Site="EMLPOND1"

TOK11.22.TEMP.b<- ts.data.TOK11.22 %>%
  group_by(date) %>%
 # summarize(daily_mean_temp = mean(wtr_2, na.rm = TRUE)) #shallowest temp
summarize(daily_mean_temp = mean(wtr_1.24, na.rm = TRUE)) #minidot temp
TOK11.22.TEMP.b$Site="TOK11"

TOPAZPOND.22.TEMP.b<- ts.data.TOPAZPOND.22 %>%
  group_by(date) %>%
 # summarize(daily_mean_temp = mean(wtr_0.98, na.rm = TRUE)) #shallowest temp (typo - actual depth 0.08)
summarize(daily_mean_temp = mean(wtr_1.66, na.rm = TRUE))  #minidot temp
TOPAZPOND.22.TEMP.b$Site="TOPAZPOND"

TOK30.22.TEMP.b<- ts.data.TOK30.22 %>%
  group_by(date) %>%
  #summarize(daily_mean_temp = mean(wtr_0.77, na.rm = TRUE)) #shallowest temp
summarize(daily_mean_temp = mean(wtr_1.34, na.rm = TRUE))  #minidot temp
TOK30.22.TEMP.b$Site="TOK30"

EMERALD.22.TEMP.b<- ts.data.emerald.22 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_3.47, na.rm = TRUE)) #shallowest temp
EMERALD.22.TEMP.b$Site="EMERALD"

TOPAZ.22.TEMP.b<- ts.data.topaz.22 %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(wtr_3.19, na.rm = TRUE)) #shallowest temp
TOPAZ.22.TEMP.b$Site="TOPAZ"


#merge all datasets
combined_temp.b <- do.call(rbind, list(EMLPOND1.22.TEMP.b, TOK11.22.TEMP.b, TOPAZPOND.22.TEMP.b, TOK30.22.TEMP.b, EMERALD.22.TEMP.b, TOPAZ.22.TEMP.b))
combined_temp.b <- combined_temp.b[with(combined_temp.b, date >= "2022-07-02" & date <= "2022-10-02"),]

#restrict dates


# Pivot the combined data frame to wide format
temperature_matrix_b <- pivot_wider(data = combined_temp.b, 
                                    id_cols = Site, 
                                    names_from = date, 
                                    values_from = daily_mean_temp)

# Remove row names if not needed
rownames(temperature_matrix_b) <- NULL

# Convert to matrix
temperature_matrix_b <- as.matrix(temperature_matrix_b)

# Check if there are any NA values in the matrix
if (any(is.na(temperature_matrix_b))) {
  print("There are NA values in the matrix.")
} else {
  print("There are no NA values in the matrix.")
}

#remove site column
temperature_matrix_b <- temperature_matrix_b[, -1, drop = FALSE]

#make sure matrix is as.numeric
temperature_matrix_b <- apply(temperature_matrix_b, 2, as.numeric)



######################################

#matrix of daily SW (row 1 EML MET, row 2 Topaz MET)
#can use pond data with sw values already tied to df
#TOK11 = EML MET
#TOPAZPOND = TOPAZ MET


sw_eml<- ts.data.TOK11.22 %>%
  group_by(date) %>%
  summarize(daily_mean_sw = mean(swrad, na.rm = TRUE)) #shallowest temp
sw_eml$Site="EMLMET"


sw_topaz<- ts.data.TOPAZPOND.22 %>%
  group_by(date) %>%
  summarize(daily_mean_sw = mean(swrad, na.rm = TRUE)) #shallowest temp
sw_topaz$Site="TOPAZMET"

# sw_topaz$daily_mean_sw = sw_topaz$daily_mean_sw*-1


#merge all datasets
combined_sw <- do.call(rbind, list(sw_eml, sw_topaz))
combined_sw <- combined_sw[with(combined_sw, date >= "2022-07-02" & date <= "2022-10-02"),]

#restrict dates


# Pivot the combined data frame to wide format
sw_matrix <- pivot_wider(data = combined_sw, 
                         id_cols = Site, 
                         names_from = date, 
                         values_from = daily_mean_sw)

# Remove row names if not needed
rownames(sw_matrix) <- NULL

# Convert to matrix
sw_matrix <- as.matrix(sw_matrix)

# Check if there are any NA values in the matrix
if (any(is.na(sw_matrix))) {
  print("There are NA values in the matrix.")
} else {
  print("There are no NA values in the matrix.")
}

#remove site column
sw_matrix <- sw_matrix[, -1, drop = FALSE]


#make sure matrix is as.numeric
sw_matrix <- apply(sw_matrix, 2, as.numeric)


#save all matrices in a single .Rdata file
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data/timeseries"
setwd(wd)

save(temperature_matrix_a, temperature_matrix_b, sw_matrix, file = "ts_matrices_2022.Rdata")


