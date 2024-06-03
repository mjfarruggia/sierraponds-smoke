###############################################################################################################################################################
# This script corrects DO concentration and saturation time series for sensor drift
#Author: Adrianne Smits
#Date: 01/13/2022; modified 9/12/2022 for MJ
###############################################################################################################################################################
library(LakeMetabolizer)
###############################################################################################################################################################

##set working directory, load data, select desired year and lake to analyze
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data/ponds"
setwd(wd)
#SPECIFY DATA BASED ON LAKE AND YEAR
season.year <- 'S21' #desired period
lake.name <- 'TOK30' #choose desired lake
year=2021
logger.pos="Middle"
short.year= year%%100


###############################################################################################################################################################
#Load list of lake sensor dataframes (clipped to open water period in chosen year and corrected for DO sensor drift)

files <- list.files()#all the files in wd
sensor_data <- list(0)
  load(files[grep(paste(lake.name,season.year,sep="_"),files)])


# Find the .rdata file that matches the keywords using grep
  matching_file <- grep(paste(lake.name, season.year, sep = "_"), files, value = TRUE)
  
# Load the matching .rdata file and assign its contents to a variable
  sensor_data1 <- if (length(matching_file) > 0) {
    load(matching_file)
    get(grep(paste(lake.name, season.year, sep = "_"), ls(), value = TRUE))
  } else {
    NULL
  }

sensor_data=sensor_data1
sensor_data$Temperature=NULL
sensor_data$Dissolved.Oxygen=NULL
sensor_data$Dissolved.Oxygen.Saturation=NULL
sensor_data$Conductivity=NULL

sensor_data <- sensor_data[sensor_data$Position == logger.pos, ]

# Find columns that are entirely NA
na_columns <- which(colSums(is.na(sensor_data)) == nrow(sensor_data))
# Remove columns that are entirely NA
sensor_data <- sensor_data[, -na_columns]



setwd("C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data")
site_attributes <- read.csv('All_PondCharacteristics_2020_2022.csv',header=TRUE)


#Load instrument DO sat offsets from calibrations in water baths
setwd("C:/Users/maryj/Documents/R/SierraPonds/Calibrations/Output")
offsets <- read.csv('DO_offsets_all_20-22.csv',header=TRUE)
offset_pattern <- paste(lake.name, short.year, sep = "_") #for summer
#offset_pattern <- paste(lake.name, season.year, sep = "_") #for winter
subset_offset <- subset(offsets, grepl(offset_pattern, Site_Year))
offsets<-subset_offset

#Minidot sensor number matching your lake/pond data, should match how sensors are named in offsets dataframe
serial<-subset(sensor_data, InstrumentType == "MINIDOT" & Position == logger.pos)$LoggerSerial
sensor<- serial[1]

#Lake elevation (meters above sea level)
elevation<-subset(site_attributes, Lake == lake.name, Year= year)$Elevation_m
elevation <-elevation[1]

###############################################################################################################################################################
#Find sensor number for DO data, match to correct DO offset, and apply offset
#Note: if offset is negative, that means the sensor is reading values that are undersaturated relative to the 'true' value

#get correct offset (in % saturation units)
offset <- offsets$Sat_correction_percent[offsets$Sensor_num==sensor]

#apply offset to DO saturation column
correctSat <- sensor_data[grep('Sat',names(sensor_data))] - offset
names(correctSat) <- paste('corrected',names(correctSat),sep="_")

#Now apply offset to DO concentration column
#Calculate theoretical 100% saturation concentration for each data point (based on water temperature and elevation)
DO_sat_mgL <-o2.at.sat.base(temp=sensor_data[grep('temp',names(sensor_data))], altitude=elevation )

#convert offset units to mg/L
DO_offset_mgL <- DO_sat_mgL*offset/100

#subtract offset (in mg/L) from DO concentration data
correctDO <- sensor_data[grep('DO',names(sensor_data))] - DO_offset_mgL
names(correctDO) <-  paste('corrected',names(correctDO),sep="_")


#add correctSat and correctDO to sensor_data
sensor_data <- cbind(sensor_data,correctSat)
sensor_data <- cbind(sensor_data,correctDO)
head(sensor_data)#check correct values to see if they make sense

###################################################################################################################################
#remove short term dips in DO by removing any measurement outside 2SD of the mean, then replacing the removed values with linearly interpolated values

library(zoo)


# Find the column name that matches the pattern using grep
column_name1 <- grep("^corrected_Sat", names(sensor_data), value = TRUE)

# Extract the column values
column_values1 <- sensor_data[, column_name1]

# Step 1: get mean and SD of the column
mean_val1 <- mean(column_values1, na.rm = TRUE)
sd_val1 <- sd(column_values1, na.rm = TRUE)

# Step 2: Calculate +/- 2SD
lower_threshold1 <- mean_val1 - 2 * sd_val1
upper_threshold1 <- mean_val1 + 2 * sd_val1

# Step 3: Create logical vector for values outside the threshold
outlier_indices1 <- column_values1 < lower_threshold1 | column_values1 > upper_threshold1

# Step 4: Replace outliers with NA
column_values1[outlier_indices1] <- NA

# Step 5: Perform linear interpolation using na.approx()
column_values1 <- na.approx(column_values1, na.rm = FALSE)

# Assign the updated column back to the data frame
sensor_data[, column_name1] <- column_values1

#################
#DO mgL


# Find the column name that matches the pattern using grep
column_name2 <- grep("^corrected_DO", names(sensor_data), value = TRUE)

# Extract the column values
column_values2 <- sensor_data[, column_name2]

# Step 1: get mean and SD of the column
mean_val2 <- mean(column_values2, na.rm = TRUE)
sd_val2 <- sd(column_values2, na.rm = TRUE)

# Step 2: Calculate +/- 2SD
lower_threshold2 <- mean_val2 - 2 * sd_val2
upper_threshold2 <- mean_val2 + 2 * sd_val2

# Step 3: Create logical vector for values outside the threshold
outlier_indices2 <- column_values2 < lower_threshold2 | column_values2 > upper_threshold2

# Step 4: Replace outliers with NA
column_values2[outlier_indices2] <- NA

# Step 5: Perform linear interpolation using na.approx()
column_values2 <- na.approx(column_values2, na.rm = FALSE)

# Assign the updated column back to the data frame
sensor_data[, column_name2] <- column_values2

###############################################################################################################################################################

#Merge the new corrected values with the full data frame
merged_df <-merge(sensor_data1, sensor_data, all=T)

head(merged_df)
###############################################################################################################################################################
#Save corrected data

setwd("C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data/ponds/corrected")

save(merged_df, file=paste(lake.name,season.year,'temp_DO_corr.Rdata',sep="_"))
