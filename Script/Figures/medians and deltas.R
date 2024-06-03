#this script is for reading in calculated metabolism values, 
#plus other lake and weather parameters for plotting. 

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
chem <- read.csv("pond_chemistry_chla_TNTP_2020_2021.csv", header=T)
#chem <- subset(chem, Lake %in% lake & Year == 2020)
chem$SampleDate=as.POSIXct(chem$SampleDate, format='%m/%d/%Y',tz="Etc/GMT-8")
################################################################################################################################################################
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

###############################################################################################################################################################
##############################################################################################################################
################# 2020 density plots ###################################################################################################
######################################################################################################################
###############################################################################################################################################################

# Separate and plot GPP for days when Smoke.day is "y" and "n" (single value)
merged_kalman$datetime=NULL
merged_kalman_smoke <- merge(smoke_eml_20, merged_kalman, all=T)


#daily GPP, by smoke day, by lake
gpp_by_day <- merged_kalman_smoke %>%
  group_by(date, Smoke.day, SiteName) %>%
  summarize(Daily_GPP = median(GPP, na.rm = TRUE))

gpp_by_day <- gpp_by_day %>% filter(!is.na(Smoke.day))

# Plot density plot of daily GPP 
density_plot_gpp <- ggplot(gpp_by_day, aes(x = Daily_GPP, fill = Smoke.day)) +
  geom_density(alpha = 0.5) +
  labs(x = "GPP", y = "Density") +
  ggtitle("Density Plot of Daily GPP") +
  theme_minimal() 
density_plot_gpp



ggplot(gpp_by_day, aes(x = Daily_GPP, fill = Smoke.day)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~SiteName, ncol=2) +
  labs(x = "Daily GPP", y = "Density") +
  ggtitle("Density Plot of Daily GPP by SiteName") +
  theme_minimal()


# Convert Smoke.day to a factor for t-test
gpp_by_day$Smoke.day <- as.factor(gpp_by_day$Smoke.day)

# Split the data by SiteName
site_split <- split(gpp_by_day, gpp_by_day$SiteName)

# Create a list to store t-test results
t_test_results <- list()

# Reorder SiteNames as per your preference
site_order <- c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30")  # Add your desired order

# List to store plots
plot_list <- list()

# Loop through each SiteName in the specified order
for (site in site_order) {
  # Perform t-test
  t_test_result <- t.test(Daily_GPP ~ Smoke.day, data = site_split[[site]])
  
  # Determine text color based on significance
  text_color <- ifelse(t_test_result$p.value < 0.05, "darkorange3", "black")
  
  # Plot density
  density_plot <- ggplot(site_split[[site]], aes(x = Daily_GPP, fill = Smoke.day)) +
    geom_density(alpha = 0.5) +
    labs(x = "Daily GPP", y = "Density") +
    ggtitle(paste("Density Plot of Daily GPP 2020 for", site)) +
    scale_x_continuous(limits=c(-0.5, 1.5))+
        theme_minimal() +
    # Add t-test results to the plot
    annotate("text", x = Inf, y = Inf,
             label = paste("p =", format(t_test_result$p.value, scientific = TRUE)),
             color=text_color,hjust = 1, vjust = 1)
  
  # Add the plot to the list
  plot_list[[site]] <- density_plot
}

# Arrange plots in a grid
plot_grid(plotlist = plot_list, ncol = 2)
  
############################################################################################################
#daily GPP, by smoke density, by lake
#do anova based on each smoke density category

gpp_by_day_density <- merged_kalman_smoke %>%
  group_by(date, smoke.density, SiteName) %>%
  summarize(Daily_GPP = median(GPP, na.rm = TRUE))

gpp_by_day_density <- gpp_by_day_density %>% filter(!is.na(smoke.density))

# Convert smoke.density to a factor for ANOVA
gpp_by_day_density$smoke.density <- as.factor(gpp_by_day_density$smoke.density)

# Split the data by SiteName
site_split <- split(gpp_by_day_density, gpp_by_day_density$SiteName)

# Create a list to store ANOVA results
anova_results <- list()

# Reorder SiteNames as per your preference
site_order <- c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30")  # Add your desired order

# Loop through each SiteName
for (site in names(site_split)) {
  # Perform ANOVA
  anova_result <- aov(Daily_GPP ~ smoke.density, data = site_split[[site]])
  
  # Store the ANOVA result
  anova_results[[site]] <- anova_result
  
  # Print ANOVA summary
  print(summary(anova_result))
  
  # Plot density
  density_plot <- ggplot(site_split[[site]], aes(x = Daily_GPP, fill = smoke.density)) +
    geom_density(alpha = 0.5) +
    labs(x = "Daily GPP", y = "Density") +
    ggtitle(paste("Density Plot of Daily GPP 2020 by smoke density,", site)) +
   # scale_x_continuous(limits=c(-0.5, 1.5))+
    
    theme_minimal()
  
  # Add ANOVA results to the plot
  density_plot <- density_plot +
    annotate("text", x = Inf, y = Inf,
             label = paste("p =", format(summary(anova_result)[["Pr(>F)"]][1], scientific = TRUE)),
             hjust = 1, vjust = 1)
  
  # Add the plot to the list
  plot_list[[site]] <- density_plot
}

# Arrange plots in a grid
plot_grid(plotlist = plot_list, ncol = 2)
####################################################################################################################

#daily GPP, by smoke density, by lake
#only include med/high smoke density, then do t  test

merged_kalman_smoke <- merged_kalman_smoke %>%
  mutate(smoke.density.categories = ifelse(smoke.density %in% c(16, 27), "med/high smoke", "low/no smoke"))

gpp_by_day_density <- merged_kalman_smoke %>%
  group_by(date, smoke.density.categories, SiteName) %>%
  summarize(Daily_GPP = median(GPP, na.rm = TRUE))

gpp_by_day_density <- gpp_by_day_density %>% filter(!is.na(smoke.density.categories))

# Convert Smoke.day to a factor for t-test
gpp_by_day_density$smoke.density.categories <- as.factor(gpp_by_day_density$smoke.density.categories)

# Split the data by SiteName
site_split <- split(gpp_by_day_density, gpp_by_day_density$SiteName)

# Create a list to store t-test results
t_test_results <- list()

# Reorder SiteNames as per your preference
site_order <- c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30")  # Add your desired order

# List to store plots
plot_list <- list()

# Loop through each SiteName in the specified order
for (site in site_order) {
  # Perform t-test
  t_test_result <- t.test(Daily_GPP ~ smoke.density.categories, data = site_split[[site]])
  
  # Determine text color based on significance
  text_color <- ifelse(t_test_result$p.value < 0.05, "darkorange3", "black")
  
  # Plot density
  density_plot <- ggplot(site_split[[site]], aes(x = Daily_GPP, fill = smoke.density.categories)) +
    geom_density(alpha = 0.5) +
    labs(x = "Daily GPP", y = "Density") +
    ggtitle(paste("Density Plot of Daily GPP 2020 for", site)) +
    scale_x_continuous(limits=c(-0.5, 1.5))+
    theme_minimal() +
    # Add t-test results to the plot
    annotate("text", x = Inf, y = Inf,
             label = paste("p =", format(t_test_result$p.value, scientific = TRUE)),
             color=text_color, hjust = 1, vjust = 1)
  
  # Add the plot to the list
  plot_list[[site]] <- density_plot
}

# Arrange plots in a grid
plot_grid(plotlist = plot_list, ncol = 2)
####################################################################################################################
#daily GPP, by smoke density, by lake
#only include HIGH smoke density, then do t  test

merged_kalman_smoke <- merged_kalman_smoke %>%
  mutate(smoke.density.categories = ifelse(smoke.density %in% c(27), "high smoke", "med/low/no smoke"))

gpp_by_day_density <- merged_kalman_smoke %>%
  group_by(date, smoke.density.categories, SiteName) %>%
  summarize(Daily_GPP = median(GPP, na.rm = TRUE))

gpp_by_day_density <- gpp_by_day_density %>% filter(!is.na(smoke.density.categories))

# Convert Smoke.day to a factor for t-test
gpp_by_day_density$smoke.density.categories <- as.factor(gpp_by_day_density$smoke.density.categories)

# Split the data by SiteName
site_split <- split(gpp_by_day_density, gpp_by_day_density$SiteName)

# Create a list to store t-test results
t_test_results <- list()

# Reorder SiteNames as per your preference
site_order <- c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30")  # Add your desired order

# List to store plots
plot_list <- list()

# Loop through each SiteName in the specified order
for (site in site_order) {
  # Perform t-test
  t_test_result <- t.test(Daily_GPP ~ smoke.density.categories, data = site_split[[site]])
  
  # Determine text color based on significance
  text_color <- ifelse(t_test_result$p.value < 0.05, "darkorange3", "black")
  
  # Plot density
  density_plot <- ggplot(site_split[[site]], aes(x = Daily_GPP, fill = smoke.density.categories)) +
    geom_density(alpha = 0.5) +
    labs(x = "Daily GPP", y = "Density") +
    ggtitle(paste("Density Plot of Daily 2020 GPP for", site)) +
    scale_x_continuous(limits=c(-0.5, 2))+
    theme_minimal() +
    # Add t-test results to the plot
    annotate("text", x = Inf, y = Inf,
             label = paste("p =", format(t_test_result$p.value, scientific = TRUE)),
             color=text_color,hjust = 1, vjust = 1)
  
  # Add the plot to the list
  plot_list[[site]] <- density_plot
}

# Arrange plots in a grid
plot_grid(plotlist = plot_list, ncol = 2)
####################################################################################################################


#daily NEP, by smoke day, by lake
nep_by_day <- merged_kalman_smoke %>%
  group_by(date, Smoke.day, SiteName) %>%
  summarize(Daily_NEP = median(NEP, na.rm = TRUE))

nep_by_day <- nep_by_day %>% filter(!is.na(Smoke.day))
density_plot_nep <- ggplot(nep_by_day, aes(x = Daily_NEP, fill = Smoke.day)) +
  geom_density(alpha = 0.5) +
  labs(x = "NEP", y = "Density") +
  ggtitle("Density Plot of Daily NEP 2020") +
  theme_minimal() 
density_plot_nep

ggplot(nep_by_day, aes(x = Daily_NEP, fill = Smoke.day)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~SiteName, ncol=2) +
  labs(x = "Daily NEP", y = "Density") +
  ggtitle("Density Plot of Daily NEP 2020 by SiteName") +
  theme_minimal()


# Convert Smoke.day to a factor for t-test
nep_by_day$Smoke.day <- as.factor(nep_by_day$Smoke.day)

# Split the data by SiteName
site_split <- split(nep_by_day, nep_by_day$SiteName)

# Create a list to store t-test results
t_test_results <- list()

# Reorder SiteNames as per your preference
site_order <- c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30")  # Add your desired order

# List to store plots
plot_list <- list()

# Loop through each SiteName in the specified order
for (site in site_order) {
  # Perform t-test
  t_test_result <- t.test(Daily_NEP ~ Smoke.day, data = site_split[[site]])
  
  # Determine text color based on significance
  text_color <- ifelse(t_test_result$p.value < 0.05, "darkorange3", "black")
  
  # Plot density
  density_plot <- ggplot(site_split[[site]], aes(x = Daily_NEP, fill = Smoke.day)) +
    geom_density(alpha = 0.5) +
    labs(x = "Daily NEP", y = "Density") +
    ggtitle(paste("Density Plot of Daily NEP 2020 for", site)) +    
    scale_x_continuous(limits=c(-1, 1))+
    theme_minimal() +
    # Add t-test results to the plot
    annotate("text", x = Inf, y = Inf,
             label = paste("p =", format(t_test_result$p.value, scientific = TRUE)),
             color=text_color, hjust = 1, vjust = 1)
  
  # Add the plot to the list
  plot_list[[site]] <- density_plot
}

# Arrange plots in a grid
plot_grid(plotlist = plot_list, ncol = 2)



############################################################################################################
#daily NEP, by smoke density, by lake
#only include med/high smoke density, then do t  test

merged_kalman_smoke <- merged_kalman_smoke %>%
  mutate(smoke.density.categories = ifelse(smoke.density %in% c(16, 27), "med/high smoke", "low/no smoke"))

NEP_by_day_density <- merged_kalman_smoke %>%
  group_by(date, smoke.density.categories, SiteName) %>%
  summarize(Daily_NEP = median(NEP, na.rm = TRUE))

NEP_by_day_density <- NEP_by_day_density %>% filter(!is.na(smoke.density.categories))

# Convert Smoke.day to a factor for t-test
NEP_by_day_density$smoke.density.categories <- as.factor(NEP_by_day_density$smoke.density.categories)

# Split the data by SiteName
site_split <- split(NEP_by_day_density, NEP_by_day_density$SiteName)

# Create a list to store t-test results
t_test_results <- list()

# Reorder SiteNames as per your preference
site_order <- c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30")  # Add your desired order

# List to store plots
plot_list <- list()

# Loop through each SiteName in the specified order
for (site in site_order) {
  # Perform t-test
  t_test_result <- t.test(Daily_NEP ~ smoke.density.categories, data = site_split[[site]])
  
  # Determine text color based on significance
  text_color <- ifelse(t_test_result$p.value < 0.05, "darkorange3", "black")
  
  # Plot density
  density_plot <- ggplot(site_split[[site]], aes(x = Daily_NEP, fill = smoke.density.categories)) +
    geom_density(alpha = 0.5) +
    labs(x = "Daily NEP", y = "Density") +
    ggtitle(paste("Density Plot of Daily NEP 2020 for", site)) +
    scale_x_continuous(limits=c(-1, 1))+
    theme_minimal() +
    # Add t-test results to the plot
    annotate("text", x = Inf, y = Inf,
             label = paste("p =", format(t_test_result$p.value, scientific = TRUE)),
             color=text_color, hjust = 1, vjust = 1)
  
  # Add the plot to the list
  plot_list[[site]] <- density_plot
}

# Arrange plots in a grid
plot_grid(plotlist = plot_list, ncol = 2)
####################################################################################################################

#daily NEP, by smoke density, by lake
#only include HIGH smoke density, then do t  test

merged_kalman_smoke <- merged_kalman_smoke %>%
  mutate(smoke.density.categories = ifelse(smoke.density %in% c(27), "high smoke", "med/low/no smoke"))

NEP_by_day_density <- merged_kalman_smoke %>%
  group_by(date, smoke.density.categories, SiteName) %>%
  summarize(Daily_NEP = median(NEP, na.rm = TRUE))

NEP_by_day_density <- NEP_by_day_density %>% filter(!is.na(smoke.density.categories))

# Convert Smoke.day to a factor for t-test
NEP_by_day_density$smoke.density.categories <- as.factor(NEP_by_day_density$smoke.density.categories)

# Split the data by SiteName
site_split <- split(NEP_by_day_density, NEP_by_day_density$SiteName)

# Create a list to store t-test results
t_test_results <- list()

# Reorder SiteNames as per your preference
site_order <- c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30")  # Add your desired order

# List to store plots
plot_list <- list()

# Loop through each SiteName in the specified order
for (site in site_order) {
  # Perform t-test
  t_test_result <- t.test(Daily_NEP ~ smoke.density.categories, data = site_split[[site]])
  
  # Determine text color based on significance
  text_color <- ifelse(t_test_result$p.value < 0.05, "darkorange3", "black")
  
  # Plot density
  density_plot <- ggplot(site_split[[site]], aes(x = Daily_NEP, fill = smoke.density.categories)) +
    geom_density(alpha = 0.5) +
    labs(x = "Daily NEP", y = "Density") +
    ggtitle(paste("Density Plot of Daily 2020 NEP for", site)) +
    scale_x_continuous(limits=c(-1, 1))+
    theme_minimal() +
    # Add t-test results to the plot
    annotate("text", x = Inf, y = Inf,
             label = paste("p =", format(t_test_result$p.value, scientific = TRUE)),
            color=text_color, hjust = 1, vjust = 1)
  
  # Add the plot to the list
  plot_list[[site]] <- density_plot
}

# Arrange plots in a grid
plot_grid(plotlist = plot_list, ncol = 2)
####################################################################################################################


#daily R, by smoke day, by lake
R_by_day <- merged_kalman_smoke %>%
  group_by(date, Smoke.day, SiteName) %>%
  summarize(Daily_R = median(R, na.rm = TRUE))

R_by_day <-R_by_day %>% filter(!is.na(Smoke.day))
density_plot_r <- ggplot(R_by_day, aes(x = Daily_R, fill = Smoke.day)) +
  geom_density(alpha = 0.5) +
  labs(x = "R", y = "Density") +
  ggtitle("Density Plot of Daily R 2020") +
  theme_minimal() 
density_plot_r

ggplot(R_by_day, aes(x = Daily_R, fill = Smoke.day)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~SiteName, ncol=2) +
  labs(x = "Daily R", y = "Density") +
  ggtitle("Density Plot of Daily R 2020 by SiteName") +
  theme_minimal()



plot_grid(density_plot_gpp, density_plot_nep, density_plot_r, ncol=1)

# Convert Smoke.day to a factor for t-test
R_by_day$Smoke.day <- as.factor(R_by_day$Smoke.day)

# Split the data by SiteName
site_split <- split(R_by_day, R_by_day$SiteName)

# Create a list to store t-test results
t_test_results <- list()

# Reorder SiteNames as per your preference
site_order <- c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30")  # Add your desired order




# List to store plots
plot_list <- list()

# Loop through each SiteName in the specified order
for (site in site_order) {
  # Perform t-test
  t_test_result <- t.test(Daily_R ~ Smoke.day, data = site_split[[site]])
  
  # Determine text color based on significance
  text_color <- ifelse(t_test_result$p.value < 0.05, "darkorange3", "black")
  
  # Plot density
  density_plot <- ggplot(site_split[[site]], aes(x = Daily_R, fill = Smoke.day)) +
    geom_density(alpha = 0.5) +
    labs(x = "Daily R", y = "Density") +
    ggtitle(paste("Density Plot of Daily R 2020 for", site)) + 
    scale_x_continuous(limits=c(-0.5, 3))+
    theme_minimal() +
    # Add t-test results to the plot
    annotate("text", x = Inf, y = Inf,
             label = paste("p =", format(t_test_result$p.value, scientific = TRUE)),
             color=text_color,hjust = 1, vjust = 1)
  
  # Add the plot to the list
  plot_list[[site]] <- density_plot
}

# Arrange plots in a grid
plot_grid(plotlist = plot_list, ncol = 2)
############################################################################################################
#daily R, by smoke density, by lake
#only include med/high smoke density, then do t  test

merged_kalman_smoke <- merged_kalman_smoke %>%
  mutate(smoke.density.categories = ifelse(smoke.density %in% c(16, 27), "med/high smoke", "low/no smoke"))

R_by_day_density <- merged_kalman_smoke %>%
  group_by(date, smoke.density.categories, SiteName) %>%
  summarize(Daily_R = median(R, na.rm = TRUE))

R_by_day_density <- R_by_day_density %>% filter(!is.na(smoke.density.categories))

# Convert Smoke.day to a factor for t-test
R_by_day_density$smoke.density.categories <- as.factor(R_by_day_density$smoke.density.categories)

# Split the data by SiteName
site_split <- split(R_by_day_density, R_by_day_density$SiteName)

# Create a list to store t-test results
t_test_results <- list()

# Reorder SiteNames as per your preference
site_order <- c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30")  # Add your desired order

# List to store plots
plot_list <- list()

# Loop through each SiteName in the specified order
for (site in site_order) {
  # Perform t-test
  t_test_result <- t.test(Daily_R ~ smoke.density.categories, data = site_split[[site]])
  
  # Determine text color based on significance
  text_color <- ifelse(t_test_result$p.value < 0.05, "darkorange3", "black")
  
    # Plot density
  density_plot <- ggplot(site_split[[site]], aes(x = Daily_R, fill = smoke.density.categories)) +
    geom_density(alpha = 0.5) +
    labs(x = "Daily R", y = "Density") +
    ggtitle(paste("Density Plot of Daily R 2020 for", site)) +
    scale_x_continuous(limits=c(-0.5, 3))+
    theme_minimal() +
    # Add t-test results to the plot
    annotate("text", x = Inf, y = Inf,
             label = paste("p =", format(t_test_result$p.value, scientific = TRUE)),
             color=text_color, hjust = 1, vjust = 1)
  
  # Add the plot to the list
  plot_list[[site]] <- density_plot
}

# Arrange plots in a grid
plot_grid(plotlist = plot_list, ncol = 2)
####################################################################################################################

#daily R, by smoke density, by lake
#only include HIGH smoke density, then do t  test

merged_kalman_smoke <- merged_kalman_smoke %>%
  mutate(smoke.density.categories = ifelse(smoke.density %in% c(27), "high smoke", "med/low/no smoke"))

R_by_day_density <- merged_kalman_smoke %>%
  group_by(date, smoke.density.categories, SiteName) %>%
  summarize(Daily_R = median(R, na.rm = TRUE))

R_by_day_density <- R_by_day_density %>% filter(!is.na(smoke.density.categories))

# Convert Smoke.day to a factor for t-test
R_by_day_density$smoke.density.categories <- as.factor(R_by_day_density$smoke.density.categories)

# Split the data by SiteName
site_split <- split(R_by_day_density, R_by_day_density$SiteName)

# Create a list to store t-test results
t_test_results <- list()

# Reorder SiteNames as per your preference
site_order <- c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30")  # Add your desired order

# List to store plots
plot_list <- list()

# Loop through each SiteName in the specified order
for (site in site_order) {
  # Perform t-test
  t_test_result <- t.test(Daily_R ~ smoke.density.categories, data = site_split[[site]])
  
  # Determine text color based on significance
  text_color <- ifelse(t_test_result$p.value < 0.05, "darkorange3", "black")
  
  # Plot density
  density_plot <- ggplot(site_split[[site]], aes(x = Daily_R, fill = smoke.density.categories)) +
    geom_density(alpha = 0.5) +
    labs(x = "Daily R", y = "Density") +
    ggtitle(paste("Density Plot of Daily 2020 R for", site)) +
    scale_x_continuous(limits=c(-0.5, 3))+
    theme_minimal() +
    # Add t-test results to the plot
    annotate("text", x = Inf, y = Inf,
             label = paste("p =", format(t_test_result$p.value, scientific = TRUE)),
             color = text_color,hjust = 1, vjust = 1)
  
  # Add the plot to the list
  plot_list[[site]] <- density_plot
}

# Arrange plots in a grid
plot_grid(plotlist = plot_list, ncol = 2)



##############################################################################################################################
################ 2021 density plots ###################################################################################################
######################################################################################################################
###############################################################################################################################################################


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

###############################################################################################################################################################

#
# Separate and plot GPP for days when Smoke.day is "y" and "n" (single value)



#daily GPP, by smoke day, by lake
gpp_by_day <- merged_kalman_21_smoke %>%
  group_by(date, Smoke.day, SiteName) %>%
  summarize(Daily_GPP = median(GPP, na.rm = TRUE))

gpp_by_day <- gpp_by_day %>% filter(!is.na(Smoke.day))

# Plot density plot of daily GPP 
density_plot_gpp <- ggplot(gpp_by_day, aes(x = Daily_GPP, fill = Smoke.day)) +
  geom_density(alpha = 0.5) +
  labs(x = "GPP", y = "Density") +
  ggtitle("Density Plot of Daily GPP") +
  theme_minimal() 
density_plot_gpp



ggplot(gpp_by_day, aes(x = Daily_GPP, fill = Smoke.day)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~SiteName, ncol=2) +
  labs(x = "Daily GPP", y = "Density") +
  ggtitle("Density Plot of Daily GPP by SiteName") +
  theme_minimal()


# Convert Smoke.day to a factor for t-test
gpp_by_day$Smoke.day <- as.factor(gpp_by_day$Smoke.day)

# Split the data by SiteName
site_split <- split(gpp_by_day, gpp_by_day$SiteName)

# Create a list to store t-test results
t_test_results <- list()

# Reorder SiteNames as per your preference
site_order <- c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30")  # Add your desired order

# List to store plots
plot_list <- list()

# Loop through each SiteName in the specified order
for (site in site_order) {
  # Perform t-test
  t_test_result <- t.test(Daily_GPP ~ Smoke.day, data = site_split[[site]])
  
  # Determine text color based on significance
  text_color <- ifelse(t_test_result$p.value < 0.05, "darkorange3", "black")
  
  # Plot density
  density_plot <- ggplot(site_split[[site]], aes(x = Daily_GPP, fill = Smoke.day)) +
    geom_density(alpha = 0.5) +
    labs(x = "Daily GPP", y = "Density") +
    ggtitle(paste("Density Plot of Daily GPP 2021 for", site)) +
    scale_x_continuous(limits=c(-0.5, 2))+
    theme_minimal() +
    # Add t-test results to the plot
    annotate("text", x = Inf, y = Inf,
             label = paste("p =", format(t_test_result$p.value, scientific = TRUE)),
             color=text_color, hjust = 1, vjust = 1)
  
  # Add the plot to the list
  plot_list[[site]] <- density_plot
}

# Arrange plots in a grid
plot_grid(plotlist = plot_list, ncol = 2)

############################################################################################################
#daily GPP, by smoke density, by lake
#do anova based on each smoke density category

gpp_by_day_density <- merged_kalman_21_smoke %>%
  group_by(date, smoke.density, SiteName) %>%
  summarize(Daily_GPP = median(GPP, na.rm = TRUE))

gpp_by_day_density <- gpp_by_day_density %>% filter(!is.na(smoke.density))

# Convert smoke.density to a factor for ANOVA
gpp_by_day_density$smoke.density <- as.factor(gpp_by_day_density$smoke.density)

# Split the data by SiteName
site_split <- split(gpp_by_day_density, gpp_by_day_density$SiteName)

# Create a list to store ANOVA results
anova_results <- list()

# Reorder SiteNames as per your preference
site_order <- c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30")  # Add your desired order

# Loop through each SiteName
for (site in names(site_split)) {
  # Perform ANOVA
  anova_result <- aov(Daily_GPP ~ smoke.density, data = site_split[[site]])
  
  # Store the ANOVA result
  anova_results[[site]] <- anova_result
  
  # Print ANOVA summary
  print(summary(anova_result))
  
  # Plot density
  density_plot <- ggplot(site_split[[site]], aes(x = Daily_GPP, fill = smoke.density)) +
    geom_density(alpha = 0.5) +
    labs(x = "Daily GPP", y = "Density") +
    ggtitle(paste("Density Plot of Daily GPP 2021 by smoke density,", site)) +
    # scale_x_continuous(limits=c(-0.5, 1.5))+
    
    theme_minimal()
  
  # Add ANOVA results to the plot
  density_plot <- density_plot +
    annotate("text", x = Inf, y = Inf,
             label = paste("p =", format(summary(anova_result)[["Pr(>F)"]][1], scientific = TRUE)),
             hjust = 1, vjust = 1)
  
  # Add the plot to the list
  plot_list[[site]] <- density_plot
}

# Arrange plots in a grid
plot_grid(plotlist = plot_list, ncol = 2)
####################################################################################################################

#daily GPP, by smoke density, by lake
#only include MED + HIGH smoke density, then do t  test

merged_kalman_21_smoke <- merged_kalman_21_smoke %>%
  mutate(smoke.density.categories = ifelse(smoke.density %in% c(16, 27), "med/high smoke", "low/no smoke"))

gpp_by_day_density <- merged_kalman_21_smoke %>%
  group_by(date, smoke.density.categories, SiteName) %>%
  summarize(Daily_GPP = median(GPP, na.rm = TRUE))

gpp_by_day_density <- gpp_by_day_density %>% filter(!is.na(smoke.density.categories))

# Convert Smoke.day to a factor for t-test
gpp_by_day_density$smoke.density.categories <- as.factor(gpp_by_day_density$smoke.density.categories)

# Split the data by SiteName
site_split <- split(gpp_by_day_density, gpp_by_day_density$SiteName)

# Create a list to store t-test results
t_test_results <- list()

# Reorder SiteNames as per your preference
site_order <- c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30")  # Add your desired order

# List to store plots
plot_list <- list()

# Loop through each SiteName in the specified order
for (site in site_order) {
  # Perform t-test
  t_test_result <- t.test(Daily_GPP ~ smoke.density.categories, data = site_split[[site]])
  
  # Determine text color based on significance
  text_color <- ifelse(t_test_result$p.value < 0.05, "darkorange3", "black")
  
  # Plot density
  density_plot <- ggplot(site_split[[site]], aes(x = Daily_GPP, fill = smoke.density.categories)) +
    geom_density(alpha = 0.5) +
    labs(x = "Daily GPP", y = "Density") +
    ggtitle(paste("Density Plot of Daily 2021 GPP for", site)) +
    scale_x_continuous(limits=c(-0.5, 2))+
    theme_minimal() +
    # Add t-test results to the plot
    annotate("text", x = Inf, y = Inf,
             label = paste("p =", format(t_test_result$p.value, scientific = TRUE)),
             color=text_color, hjust = 1, vjust = 1)
  
  # Add the plot to the list
  plot_list[[site]] <- density_plot
}

# Arrange plots in a grid
plot_grid(plotlist = plot_list, ncol = 2)
####################################################################################################################
#daily GPP, by smoke density, by lake
#only include HIGH smoke density, then do t  test

merged_kalman_21_smoke <- merged_kalman_21_smoke %>%
  mutate(smoke.density.categories = ifelse(smoke.density %in% c(27), "high smoke", "med/low/no smoke"))

gpp_by_day_density <- merged_kalman_21_smoke %>%
  group_by(date, smoke.density.categories, SiteName) %>%
  summarize(Daily_GPP = median(GPP, na.rm = TRUE))

gpp_by_day_density <- gpp_by_day_density %>% filter(!is.na(smoke.density.categories))

# Convert Smoke.day to a factor for t-test
gpp_by_day_density$smoke.density.categories <- as.factor(gpp_by_day_density$smoke.density.categories)

# Split the data by SiteName
site_split <- split(gpp_by_day_density, gpp_by_day_density$SiteName)

# Create a list to store t-test results
t_test_results <- list()

# Reorder SiteNames as per your preference
site_order <- c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30")  # Add your desired order

# List to store plots
plot_list <- list()

# Loop through each SiteName in the specified order
for (site in site_order) {
  # Perform t-test
  t_test_result <- t.test(Daily_GPP ~ smoke.density.categories, data = site_split[[site]])
  
  # Determine text color based on significance
  text_color <- ifelse(t_test_result$p.value < 0.05, "darkorange3", "black")
  
  # Plot density
  density_plot <- ggplot(site_split[[site]], aes(x = Daily_GPP, fill = smoke.density.categories)) +
    geom_density(alpha = 0.5) +
    labs(x = "Daily GPP", y = "Density") +
    ggtitle(paste("Density Plot of Daily 2021 GPP for", site)) +
    scale_x_continuous(limits=c(-0.5, 2))+
    theme_minimal() +
    # Add t-test results to the plot
    annotate("text", x = Inf, y = Inf,
             label = paste("p =", format(t_test_result$p.value, scientific = TRUE)),
             color=text_color, hjust = 1, vjust = 1)
  
  # Add the plot to the list
  plot_list[[site]] <- density_plot
}

# Arrange plots in a grid
plot_grid(plotlist = plot_list, ncol = 2)
####################################################################################################################


#daily NEP, by smoke day, by lake
nep_by_day <- merged_kalman_21_smoke %>%
  group_by(date, Smoke.day, SiteName) %>%
  summarize(Daily_NEP = median(NEP, na.rm = TRUE))

nep_by_day <- nep_by_day %>% filter(!is.na(Smoke.day))
density_plot_nep <- ggplot(nep_by_day, aes(x = Daily_NEP, fill = Smoke.day)) +
  geom_density(alpha = 0.5) +
  labs(x = "NEP", y = "Density") +
  ggtitle("Density Plot of Daily NEP") +
  theme_minimal() 
density_plot_nep

ggplot(nep_by_day, aes(x = Daily_NEP, fill = Smoke.day)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~SiteName, ncol=2) +
  labs(x = "Daily NEP", y = "Density") +
  ggtitle("Density Plot of Daily NEP 2021 by SiteName") +
  theme_minimal()


# Convert Smoke.day to a factor for t-test
nep_by_day$Smoke.day <- as.factor(nep_by_day$Smoke.day)

# Split the data by SiteName
site_split <- split(nep_by_day, nep_by_day$SiteName)

# Create a list to store t-test results
t_test_results <- list()

# Reorder SiteNames as per your preference
site_order <- c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30")  # Add your desired order

# List to store plots
plot_list <- list()

# Loop through each SiteName in the specified order
for (site in site_order) {
  # Perform t-test
  t_test_result <- t.test(Daily_NEP ~ Smoke.day, data = site_split[[site]])
  
  # Determine text color based on significance
  text_color <- ifelse(t_test_result$p.value < 0.05, "darkorange3", "black")
  
  # Plot density
  density_plot <- ggplot(site_split[[site]], aes(x = Daily_NEP, fill = Smoke.day)) +
    geom_density(alpha = 0.5) +
    labs(x = "Daily NEP", y = "Density") +
    ggtitle(paste("Density Plot of Daily NEP 2021 for", site)) +    
    scale_x_continuous(limits=c(-1, 1))+
    theme_minimal() +
    # Add t-test results to the plot
    annotate("text", x = Inf, y = Inf,
             label = paste("p =", format(t_test_result$p.value, scientific = TRUE)),
             color=text_color, hjust = 1, vjust = 1)
  
  # Add the plot to the list
  plot_list[[site]] <- density_plot
}

# Arrange plots in a grid
plot_grid(plotlist = plot_list, ncol = 2)


############################################################################################################
#daily NEP, by smoke density, by lake
#only include MED + HIGH smoke density, then do t  test

merged_kalman_21_smoke <- merged_kalman_21_smoke %>%
  mutate(smoke.density.categories = ifelse(smoke.density %in% c(16, 27), "med/high smoke", "low/no smoke"))

NEP_by_day_density <- merged_kalman_21_smoke %>%
  group_by(date, smoke.density.categories, SiteName) %>%
  summarize(Daily_NEP = median(NEP, na.rm = TRUE))

NEP_by_day_density <- NEP_by_day_density %>% filter(!is.na(smoke.density.categories))

# Convert Smoke.day to a factor for t-test
NEP_by_day_density$smoke.density.categories <- as.factor(NEP_by_day_density$smoke.density.categories)

# Split the data by SiteName
site_split <- split(NEP_by_day_density, NEP_by_day_density$SiteName)

# Create a list to store t-test results
t_test_results <- list()

# Reorder SiteNames as per your preference
site_order <- c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30")  # Add your desired order

# List to store plots
plot_list <- list()

# Loop through each SiteName in the specified order
for (site in site_order) {
  # Perform t-test
  t_test_result <- t.test(Daily_NEP ~ smoke.density.categories, data = site_split[[site]])
  
  # Determine text color based on significance
  text_color <- ifelse(t_test_result$p.value < 0.05, "darkorange3", "black")
  
  # Plot density
  density_plot <- ggplot(site_split[[site]], aes(x = Daily_NEP, fill = smoke.density.categories)) +
    geom_density(alpha = 0.5) +
    labs(x = "Daily NEP", y = "Density") +
    ggtitle(paste("Density Plot of Daily 2021 NEP for", site)) +
    scale_x_continuous(limits=c(-1, 1))+
    theme_minimal() +
    # Add t-test results to the plot
    annotate("text", x = Inf, y = Inf,
             label = paste("p =", format(t_test_result$p.value, scientific = TRUE)),
             color=text_color, hjust = 1, vjust = 1)
  
  # Add the plot to the list
  plot_list[[site]] <- density_plot
}

# Arrange plots in a grid
plot_grid(plotlist = plot_list, ncol = 2)
####################################################################################################################
#daily NEP, by smoke density, by lake
#only include HIGH smoke density, then do t  test

merged_kalman_21_smoke <- merged_kalman_21_smoke %>%
  mutate(smoke.density.categories = ifelse(smoke.density %in% c(27), "high smoke", "med/low/no smoke"))

NEP_by_day_density <- merged_kalman_21_smoke %>%
  group_by(date, smoke.density.categories, SiteName) %>%
  summarize(Daily_NEP = median(NEP, na.rm = TRUE))

NEP_by_day_density <- NEP_by_day_density %>% filter(!is.na(smoke.density.categories))

# Convert Smoke.day to a factor for t-test
NEP_by_day_density$smoke.density.categories <- as.factor(NEP_by_day_density$smoke.density.categories)

# Split the data by SiteName
site_split <- split(NEP_by_day_density, NEP_by_day_density$SiteName)

# Create a list to store t-test results
t_test_results <- list()

# Reorder SiteNames as per your preference
site_order <- c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30")  # Add your desired order

# List to store plots
plot_list <- list()

# Loop through each SiteName in the specified order
for (site in site_order) {
  # Perform t-test
  t_test_result <- t.test(Daily_NEP ~ smoke.density.categories, data = site_split[[site]])
  
  # Determine text color based on significance
  text_color <- ifelse(t_test_result$p.value < 0.05, "darkorange3", "black")
  
  # Plot density
  density_plot <- ggplot(site_split[[site]], aes(x = Daily_NEP, fill = smoke.density.categories)) +
    geom_density(alpha = 0.5) +
    labs(x = "Daily NEP", y = "Density") +
    ggtitle(paste("Density Plot of Daily 2021 NEP for", site)) +
    scale_x_continuous(limits=c(-1, 1))+
    theme_minimal() +
    # Add t-test results to the plot
    annotate("text", x = Inf, y = Inf,
             label = paste("p =", format(t_test_result$p.value, scientific = TRUE)),
             color=text_color, hjust = 1, vjust = 1)
  
  # Add the plot to the list
  plot_list[[site]] <- density_plot
}

# Arrange plots in a grid
plot_grid(plotlist = plot_list, ncol = 2)
####################################################################################################################

#daily R, by smoke day, by lake
R_by_day <- merged_kalman_21_smoke %>%
  group_by(date, Smoke.day, SiteName) %>%
  summarize(Daily_R = median(R, na.rm = TRUE))

R_by_day <-R_by_day %>% filter(!is.na(Smoke.day))
density_plot_r <- ggplot(R_by_day, aes(x = Daily_R, fill = Smoke.day)) +
  geom_density(alpha = 0.5) +
  labs(x = "R", y = "Density") +
  ggtitle("Density Plot of Daily R") +
  theme_minimal() 
density_plot_r

ggplot(R_by_day, aes(x = Daily_R, fill = Smoke.day)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~SiteName, ncol=2) +
  labs(x = "Daily R", y = "Density") +
  ggtitle("Density Plot of Daily R by SiteName") +
  theme_minimal()



plot_grid(density_plot_gpp, density_plot_nep, density_plot_r, ncol=1)

# Convert Smoke.day to a factor for t-test
R_by_day$Smoke.day <- as.factor(R_by_day$Smoke.day)

# Split the data by SiteName
site_split <- split(R_by_day, R_by_day$SiteName)

# Create a list to store t-test results
t_test_results <- list()

# Reorder SiteNames as per your preference
site_order <- c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30")  # Add your desired order


# Find the overall range of Daily_R for setting xlim
overall_range <- range(unlist(lapply(site_split, function(x) x$Daily_R)))


# List to store plots
plot_list <- list()

# Loop through each SiteName in the specified order
for (site in site_order) {
  # Perform t-test
  t_test_result <- t.test(Daily_R ~ Smoke.day, data = site_split[[site]])
  
  # Determine text color based on significance
  text_color <- ifelse(t_test_result$p.value < 0.05, "darkorange3", "black")
  
  # Plot density
  density_plot <- ggplot(site_split[[site]], aes(x = Daily_R, fill = Smoke.day)) +
    geom_density(alpha = 0.5) +
    labs(x = "Daily R", y = "Density") +
    ggtitle(paste("Density Plot of Daily R 2021 for", site)) + 
    scale_x_continuous(limits=c(-0.5, 2.5))+
    theme_minimal() +
    # Add t-test results to the plot
    annotate("text", x = Inf, y = Inf,
             label = paste("p =", format(t_test_result$p.value, scientific = TRUE)),
             color=text_color, hjust = 1, vjust = 1)
  
  # Add the plot to the list
  plot_list[[site]] <- density_plot
}

# Arrange plots in a grid
plot_grid(plotlist = plot_list, ncol = 2)

############################################################################################################
#daily R, by smoke density, by lake
#only include MED + HIGH smoke density, then do t  test

merged_kalman_21_smoke <- merged_kalman_21_smoke %>%
  mutate(smoke.density.categories = ifelse(smoke.density %in% c(16, 27), "med/high smoke", "low/no smoke"))

R_by_day_density <- merged_kalman_21_smoke %>%
  group_by(date, smoke.density.categories, SiteName) %>%
  summarize(Daily_R = median(R, na.rm = TRUE))

R_by_day_density <- R_by_day_density %>% filter(!is.na(smoke.density.categories))

# Convert Smoke.day to a factor for t-test
R_by_day_density$smoke.density.categories <- as.factor(R_by_day_density$smoke.density.categories)

# Split the data by SiteName
site_split <- split(R_by_day_density, R_by_day_density$SiteName)

# Create a list to store t-test results
t_test_results <- list()

# Reorder SiteNames as per your preference
site_order <- c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30")  # Add your desired order

# List to store plots
plot_list <- list()

# Loop through each SiteName in the specified order
for (site in site_order) {
  # Perform t-test
  t_test_result <- t.test(Daily_R ~ smoke.density.categories, data = site_split[[site]])
  
  # Determine text color based on significance
  text_color <- ifelse(t_test_result$p.value < 0.05, "darkorange3", "black")
  
  # Plot density
  density_plot <- ggplot(site_split[[site]], aes(x = Daily_R, fill = smoke.density.categories)) +
    geom_density(alpha = 0.5) +
    labs(x = "Daily R", y = "Density") +
    ggtitle(paste("Density Plot of Daily 2021 R for", site)) +
    scale_x_continuous(limits=c(-0.5, 2.5))+
    theme_minimal() +
    # Add t-test results to the plot
    annotate("text", x = Inf, y = Inf,
             label = paste("p =", format(t_test_result$p.value, scientific = TRUE)),
             color=text_color, hjust = 1, vjust = 1)
  
  # Add the plot to the list
  plot_list[[site]] <- density_plot
}

# Arrange plots in a grid
plot_grid(plotlist = plot_list, ncol = 2)
####################################################################################################################
#daily R, by smoke density, by lake
#only include HIGH smoke density, then do t  test

merged_kalman_21_smoke <- merged_kalman_21_smoke %>%
  mutate(smoke.density.categories = ifelse(smoke.density %in% c(27), "high smoke", "med/low/no smoke"))

R_by_day_density <- merged_kalman_21_smoke %>%
  group_by(date, smoke.density.categories, SiteName) %>%
  summarize(Daily_R = median(R, na.rm = TRUE))

R_by_day_density <- R_by_day_density %>% filter(!is.na(smoke.density.categories))

# Convert Smoke.day to a factor for t-test
R_by_day_density$smoke.density.categories <- as.factor(R_by_day_density$smoke.density.categories)

# Split the data by SiteName
site_split <- split(R_by_day_density, R_by_day_density$SiteName)

# Create a list to store t-test results
t_test_results <- list()

# Reorder SiteNames as per your preference
site_order <- c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30")  # Add your desired order

# List to store plots
plot_list <- list()

# Loop through each SiteName in the specified order
for (site in site_order) {
  # Perform t-test
  t_test_result <- t.test(Daily_R ~ smoke.density.categories, data = site_split[[site]])
  
  # Determine text color based on significance
  text_color <- ifelse(t_test_result$p.value < 0.05, "darkorange3", "black")
  
  # Plot density
  density_plot <- ggplot(site_split[[site]], aes(x = Daily_R, fill = smoke.density.categories)) +
    geom_density(alpha = 0.5) +
    labs(x = "Daily R", y = "Density") +
    ggtitle(paste("Density Plot of Daily 2021 R for", site)) +
    scale_x_continuous(limits=c(-0.5, 2.5))+
    theme_minimal() +
    # Add t-test results to the plot
    annotate("text", x = Inf, y = Inf,
             label = paste("p =", format(t_test_result$p.value, scientific = TRUE)),
             color=text_color, hjust = 1, vjust = 1)
  
  # Add the plot to the list
  plot_list[[site]] <- density_plot
}

# Arrange plots in a grid
plot_grid(plotlist = plot_list, ncol = 2)



####################################################################################################################
############### scatter plots #############################################################################################################
#####################################################################################################################
####################################################################################################################
########################################################################################################################

# 2020  data: merged_kalman; merged_ts.data
# 2021 data: merged_kalman 21; merged_ts.data_21

# Change the order of SiteName
merged_kalman$SiteName <- factor(merged_kalman$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30"))

RvGPP <- ggplot(merged_kalman, aes(x = R, y = GPP, color=SiteName)) +
  geom_point() +
  labs(x = "R", y = "GPP") +
  ggtitle("R vs GPP 2020")+
  theme_bw()
RvGPP

#remove TOK30
kalman_no30=merged_kalman %>% filter(SiteName != "TOK30")
# Change the order of SiteName
kalman_no30$SiteName <- factor(kalman_no30$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND"))

RvGPP <- ggplot(kalman_no30, aes(x = R, y = GPP, color=SiteName)) +
  geom_point() +
  labs(x = "R", y = "GPP") +
  ggtitle("R vs GPP 2020")+
  theme_bw()
RvGPP



#subset by smoke.day
subset_kalman_smoke <- merged_kalman_smoke[merged_kalman_smoke$Smoke.day == "n", ]
subset_kalman_smoke$SiteName <- factor(subset_kalman_smoke$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30"))

scatter_plot <- ggplot(subset_kalman_smoke, aes(x = R, y = GPP, color = SiteName)) +
  geom_point() +
  labs(x = "R", y = "GPP") +
  ggtitle("R vs GPP on smoke days 2020") +
  theme_bw()
scatter_plot

#remove TOK30
subset_kalman_smoke_no30=subset_kalman_smoke %>% filter(SiteName != "TOK30")
# Change the order of SiteName
subset_kalman_smoke_no30$SiteName <- factor(subset_kalman_smoke_no30$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND"))

smokeday_20_no30 <- ggplot(subset_kalman_smoke_no30, aes(x = R, y = GPP, color = SiteName)) +
  geom_point() +
  labs(x = "R", y = "GPP") +
  ggtitle("R vs GPP on no smoke days 2020") +
  scale_x_continuous(limits=c(-0.5, 2))+
  scale_y_continuous(limits=c(-0.5, 2))+
  theme_bw()
smokeday_20_no30




#subset by smoke density; only keep "high density" days 

merged_kalman_smoke_subset <- merged_kalman_smoke[merged_kalman_smoke$smoke.density.categories == "high smoke", ]

merged_kalman_smoke_subset$SiteName <- factor(merged_kalman_smoke_subset$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30"))

scatter_plot <- ggplot(merged_kalman_smoke_subset, aes(x = R, y = GPP, color = SiteName)) +
  geom_point() +
  labs(x = "R", y = "GPP") +
  ggtitle("R vs GPP on high density smoke days 2020") +
  theme_bw()
scatter_plot

#remove TOK30
subset_kalman_smoke_no30=merged_kalman_smoke_subset %>% filter(SiteName != "TOK30")
# Change the order of SiteName
subset_kalman_smoke_no30$SiteName <- factor(subset_kalman_smoke_no30$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND"))

hismoke_20_no30 <- ggplot(subset_kalman_smoke_no30, aes(x = R, y = GPP, color = SiteName)) +
  geom_point() +
  labs(x = "R", y = "GPP") +
  ggtitle("R vs GPP on high density smoke days 2020") +
  scale_x_continuous(limits=c(-0.5, 2))+
  scale_y_continuous(limits=c(-0.5, 2))+
    theme_bw()
hismoke_20_no30



#subset by smoke density; only keep "high density" days 

merged_kalman_smoke_subset_low <- merged_kalman_smoke[merged_kalman_smoke$smoke.density.categories == "med/low/no smoke", ]

merged_kalman_smoke_subset_low$SiteName <- factor(merged_kalman_smoke_subset_low$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30"))

scatter_plot <- ggplot(merged_kalman_smoke_subset_low, aes(x = R, y = GPP, color = SiteName)) +
  geom_point() +
  labs(x = "R", y = "GPP") +
  ggtitle("R vs GPP on med/low/no density smoke days 2020") +
  theme_bw()
scatter_plot

#remove TOK30
subset_kalman_smoke_no30=merged_kalman_smoke_subset_low %>% filter(SiteName != "TOK30")
# Change the order of SiteName
subset_kalman_smoke_no30$SiteName <- factor(subset_kalman_smoke_no30$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND"))

losmoke_20_no30 <- ggplot(subset_kalman_smoke_no30, aes(x = R, y = GPP, color = SiteName)) +
  geom_point() +
  labs(x = "R", y = "GPP") +
  ggtitle("R vs GPP on med/low/no density smoke days 2020") +
  scale_x_continuous(limits=c(-0.5, 2))+
  scale_y_continuous(limits=c(-0.5, 2))+
  theme_bw()
losmoke_20_no30




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


########################################################################################################################
#######same for 2021###############################################################################################################################
# Change the order of SiteName
merged_kalman_21$SiteName <- factor(merged_kalman_21$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30"))

RvGPP <- ggplot(merged_kalman_21, aes(x = R, y = GPP, color=SiteName)) +
  geom_point() +
  labs(x = "R", y = "GPP") +
  ggtitle("R vs GPP 2021")+
  theme_bw()
RvGPP

#remove TOK30
kalman_no30_21=merged_kalman_21 %>% filter(SiteName != "TOK30")
# Change the order of SiteName
kalman_no30_21$SiteName <- factor(kalman_no30_21$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND"))

RvGPP <- ggplot(kalman_no30_21, aes(x = R, y = GPP, color=SiteName)) +
  geom_point() +
  labs(x = "R", y = "GPP") +
  ggtitle("R vs GPP 2021")+
  theme_bw()
RvGPP

#subset by smoke.day
subset_kalman_21_smoke <- merged_kalman_21_smoke[merged_kalman_21_smoke$Smoke.day == "n", ]

subset_kalman_21_smoke$SiteName <- factor(subset_kalman_21_smoke$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30"))

hismoke_20 <- ggplot(subset_kalman_21_smoke, aes(x = R, y = GPP, color = SiteName)) +
  geom_point() +
  labs(x = "R", y = "GPP") +
  ggtitle("R vs GPP on smoke days 2021") +
  theme_bw()
hismoke_20

#remove TOK30
subset_kalman_21_smoke_no30=subset_kalman_21_smoke %>% filter(SiteName != "TOK30")
# Change the order of SiteName
subset_kalman_21_smoke_no30$SiteName <- factor(subset_kalman_21_smoke_no30$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND"))

smokeday_21_no30 <- ggplot(subset_kalman_21_smoke_no30, aes(x = R, y = GPP, color = SiteName)) +
  geom_point() +
  labs(x = "R", y = "GPP") +
  ggtitle("R vs GPP on no smoke days 2021") +
  scale_x_continuous(limits=c(-0.5, 2))+
  scale_y_continuous(limits=c(-0.5, 2))+
    theme_bw()
smokeday_21_no30




#subset by smoke density; only keep "high density" days as y

merged_kalman_smoke_subset_21 <- merged_kalman_21_smoke[merged_kalman_21_smoke$smoke.density.categories == "high smoke", ]

merged_kalman_smoke_subset_21$SiteName <- factor(merged_kalman_smoke_subset_21$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30"))

hismoke_21 <- ggplot(merged_kalman_smoke_subset_21, aes(x = R, y = GPP, color = SiteName)) +
  geom_point() +
  labs(x = "R", y = "GPP") +
  ggtitle("R vs GPP on high density smoke days 2021") +
  theme_bw()
hismoke_21

#remove TOK30
subset_kalman_smoke_no30=merged_kalman_smoke_subset_21 %>% filter(SiteName != "TOK30")
# Change the order of SiteName
subset_kalman_smoke_no30$SiteName <- factor(subset_kalman_smoke_no30$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND"))

hismoke_21_no30 <- ggplot(subset_kalman_smoke_no30, aes(x = R, y = GPP, color = SiteName)) +
  geom_point() +
  labs(x = "R", y = "GPP") +
  ggtitle("R vs GPP on high density smoke days 2021") +
  scale_x_continuous(limits=c(-0.5, 2))+
  scale_y_continuous(limits=c(-0.5, 2))+
    theme_bw()
hismoke_21_no30

plot_grid(hismoke_20_no30, hismoke_21_no30)
plot_grid(smokeday_20_no30, smokeday_21_no30)




#subset by smoke density; only keep "low density" days 

merged_kalman_21_smoke_subset_low <- merged_kalman_21_smoke[merged_kalman_21_smoke$smoke.density.categories == "med/low/no smoke", ]

merged_kalman_21_smoke_subset_low$SiteName <- factor(merged_kalman_21_smoke_subset_low$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30"))

scatter_plot <- ggplot(merged_kalman_21_smoke_subset_low, aes(x = R, y = GPP, color = SiteName)) +
  geom_point() +
  labs(x = "R", y = "GPP") +
  ggtitle("R vs GPP on med/low/no density smoke days 2021") +
  theme_bw()
scatter_plot

#remove TOK30
subset_kalman_smoke_no30=merged_kalman_21_smoke_subset_low %>% filter(SiteName != "TOK30")
# Change the order of SiteName
subset_kalman_smoke_no30$SiteName <- factor(subset_kalman_smoke_no30$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND"))

losmoke_21_no30 <- ggplot(subset_kalman_smoke_no30, aes(x = R, y = GPP, color = SiteName)) +
  geom_point() +
  labs(x = "R", y = "GPP") +
  ggtitle("R vs GPP on med/low/no density smoke days 2021") +
  scale_x_continuous(limits=c(-0.5, 2))+
  scale_y_continuous(limits=c(-0.5, 2))+
  theme_bw()
losmoke_21_no30

plot_grid(losmoke_20_no30, losmoke_21_no30)





###### chemistry###########
chem2020=chem %>% filter(Year == 2020)
chem2020$CollectionDepth=NULL
chem2020$Units=NULL
chem2020$Notes=NULL
chem2020$SampleDate=as.Date(chem2020$SampleDate)
chem2020 <- chem2020 %>%  filter(Value != "<0.08")
chem2020$Value=as.numeric(chem2020$Value)
names(chem2020)[names(chem2020) == 'Lake'] <- 'SiteName'
names(chem2020)[names(chem2020) == 'SampleDate'] <- 'date'

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
    theme_bw()
TNplot

#TP
merged_kalman_smoke_chem_TP=merged_kalman_smoke_chem %>% filter(SampleType == "TP")
merged_kalman_smoke_chem_TP$SiteName <- factor(merged_kalman_smoke_chem_TP$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND", "TOK30"))

TPplot <- ggplot(merged_kalman_smoke_chem_TP, aes(x = Value, y = GPP, color = Smoke.day)) +
  geom_point() +
  labs(x = "TP", y = "GPP") +
  ggtitle("GPP vs TP 2020") +
  scale_x_continuous(limits=c(0, 0.6))+
  theme_bw()
TPplot

#chl-a
merged_kalman_smoke_chem_chla=merged_kalman_smoke_chem %>% filter(SampleType == "chla")
merged_kalman_smoke_chem_chla$SiteName <- factor(merged_kalman_smoke_chem_chla$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND", "TOK30"))

chlaplot <- ggplot(merged_kalman_smoke_chem_chla, aes(x = Value, y = GPP, color = Smoke.day)) +
  geom_point() +
  labs(x = "chla", y = "GPP") +
  ggtitle("GPP vs chla 2020") +
  scale_x_continuous(limits=c(0, 5))+
  
  theme_bw()
chlaplot




###############################################
#chem 2021
chem2021=chem %>% filter(Year == 2021)
chem2021$CollectionDepth=NULL
chem2021$Units=NULL
chem2021$Notes=NULL
chem2021$SampleDate=as.Date(chem2021$SampleDate)
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
    TRUE ~ NA_real_  # If none of the conditions match, you can assign NA or any other default value
  ))

merged_kalman_21_smoke$GPP_depthstandardized=merged_kalman_21_smoke$GPP * merged_kalman_21_smoke$meandepth


merged_kalman_21_smoke_chem=merge(chem2021, merged_kalman_21_smoke, all=T)

#TN
merged_kalman_21_smoke_chem_TN=merged_kalman_21_smoke_chem %>% filter(SampleType == "TN")
merged_kalman_21_smoke_chem_TN$SiteName <- factor(merged_kalman_21_smoke_chem_TN$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND", "TOK30"))

TNplot21 <- ggplot(merged_kalman_21_smoke_chem_TN, aes(x = Value, y = GPP, color = Smoke.day)) +
  geom_point() +
  labs(x = "TN", y = "GPP") +
  ggtitle("GPP vs TN 2021") +
  scale_x_continuous(limits=c(2, 40))+
    theme_bw()
TNplot21

#TP
merged_kalman_21_smoke_chem_TP=merged_kalman_21_smoke_chem %>% filter(SampleType == "TP")
merged_kalman_21_smoke_chem_TP$SiteName <- factor(merged_kalman_21_smoke_chem_TP$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND", "TOK30"))

TPplot21 <- ggplot(merged_kalman_21_smoke_chem_TP, aes(x = Value, y = GPP, color = Smoke.day)) +
  geom_point() +
  labs(x = "TP", y = "GPP") +
  ggtitle("GPP vs TP 2021") +
  scale_x_continuous(limits=c(0, 0.6))+
  
  theme_bw()
TPplot21

#chl-a
merged_kalman_21_smoke_chem_chla=merged_kalman_21_smoke_chem %>% filter(SampleType == "chla")
merged_kalman_21_smoke_chem_chla$SiteName <- factor(merged_kalman_21_smoke_chem_chla$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND", "TOK30"))


chlaplot21 <- ggplot(merged_kalman_21_smoke_chem_chla, aes(x = Value, y = GPP, color = Smoke.day)) +
  geom_point() +
  labs(x = "chla", y = "GPP") +
  ggtitle("GPP vs chla 2021") +
  scale_x_continuous(limits=c(0, 5))+
    theme_bw()
chlaplot21


plot_grid(TNplot, TNplot21, TPplot, TPplot21, chlaplot, chlaplot21,ncol=2)


#combine years
TNplot20.21 <- ggplot() +
  geom_point(data=merged_kalman_smoke_chem_TN, aes(x = Value, y = GPP, color = Smoke.day, shape="2020")) +
  geom_point(data=merged_kalman_21_smoke_chem_TN, aes(x = Value, y = GPP, color = Smoke.day, shape="2021")) +
  labs(x = "TN", y = "GPP") +
  ggtitle("GPP vs TN 2020 + 2021") +
  scale_x_continuous(limits=c(2, 40))+
  theme_bw()
TNplot20.21

TPplot20.21 <- ggplot() +
  geom_point(data=merged_kalman_smoke_chem_TP, aes(x = Value, y = GPP, color = Smoke.day, shape="2020")) +
  geom_point(data=merged_kalman_21_smoke_chem_TP, aes(x = Value, y = GPP, color = Smoke.day, shape="2021")) +
  labs(x = "TP", y = "GPP") +
  ggtitle("GPP vs TP 2020 + 2021") +
  scale_x_continuous(limits=c(0, 0.5))+
  theme_bw()
TPplot20.21

chlaplot20.21 <- ggplot() +
  geom_point(data=merged_kalman_smoke_chem_chla, aes(x = Value, y = GPP, color = Smoke.day, shape="2020")) +
  geom_point(data=merged_kalman_21_smoke_chem_chla, aes(x = Value, y = GPP, color = Smoke.day, shape="2021")) +
  labs(x = "chla", y = "GPP") +
  ggtitle("GPP vs chla 2020 + 2021") +
  scale_x_continuous(limits=c(0, 2))+
  theme_bw()
chlaplot20.21

plot_grid( TNplot20.21,  TPplot20.21,  chlaplot20.21,ncol=1)



#plot when standardized GPP by mean depth 


TNplot20.21.ds <- ggplot() +
  geom_point(data=merged_kalman_smoke_chem_TN, aes(x = Value, y = GPP_depthstandardized, color = Smoke.day, shape="2020")) +
  geom_point(data=merged_kalman_21_smoke_chem_TN, aes(x = Value, y = GPP_depthstandardized, color = Smoke.day, shape="2021")) +
  labs(x = "TN", y = "GPP (depth standardized)") +
  ggtitle("GPP vs TN 2020 + 2021") +
  scale_x_continuous(limits=c(2, 40))+
  theme_bw()
TNplot20.21.ds

TPplot20.21.ds <- ggplot() +
  geom_point(data=merged_kalman_smoke_chem_TP, aes(x = Value, y = GPP_depthstandardized, color = Smoke.day, shape="2020")) +
  geom_point(data=merged_kalman_21_smoke_chem_TP, aes(x = Value, y = GPP_depthstandardized, color = Smoke.day, shape="2021")) +
  labs(x = "TP", y = "GPP (depth standardized)") +
  ggtitle("GPP vs TP 2020 + 2021") +
  scale_x_continuous(limits=c(0, 0.5))+
  theme_bw()
TPplot20.21.ds

chlaplot20.21.ds <- ggplot() +
  geom_point(data=merged_kalman_smoke_chem_chla, aes(x = Value, y = GPP_depthstandardized, color = Smoke.day, shape="2020")) +
  geom_point(data=merged_kalman_21_smoke_chem_chla, aes(x = Value, y = GPP_depthstandardized, color = Smoke.day, shape="2021")) +
  labs(x = "chla", y = "GPP (depth standardized)") +
  ggtitle("GPP vs chla 2020 + 2021") +
  scale_x_continuous(limits=c(0, 2))+
  theme_bw()
chlaplot20.21.ds

plot_grid( TNplot20.21.ds,  TPplot20.21.ds,  chlaplot20.21.ds,ncol=1)


plot_grid( TNplot20.21,TNplot20.21.ds,   TPplot20.21,TPplot20.21.ds,  chlaplot20.21,chlaplot20.21.ds,ncol=2)
   

##################################################
#plot seasonal chem, date on x, chem on y
#add swrad in background, and rain

#CHLA
chem2020chla=chem2020 %>% filter (SampleType=="chla")
# Create a new column for the month
chem2020chla$month <- format(chem2020chla$date, "%m")

# Calculate the monthly average
chem2020chla <- chem2020chla %>%
  group_by(SiteName, month) %>%
  summarise(avg_value = mean(Value, na.rm = TRUE))

chem2020chla$SiteName=as.factor(chem2020chla$SiteName)

chla20 <- ggplot(data=chem2020chla, aes(x = as.character(month), y = avg_value, color=as.character(SiteName), group=as.character(SiteName))) +
  geom_point() +
  geom_line()+
  labs(x = "Month", y = "chla (monthly avg)") +
  ggtitle("chla 2020") +
  scale_y_continuous(limits = c(0, 2.5))+
    theme_bw()
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

TN20 <- ggplot(data=chem2020TN, aes(x = as.character(month), y = avg_value, color=as.character(SiteName), group=as.character(SiteName))) +
  geom_point() +
  geom_line()+
  labs(x = "Month", y = "TN (monthly avg)") +
  ggtitle("TN 2020") +
  scale_y_continuous(limits = c(0, 40))+
    theme_bw()
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

TP20 <- ggplot(data=chem2020TP, aes(x = as.character(month), y = avg_value, color=as.character(SiteName), group=as.character(SiteName))) +
  geom_point() +
  geom_line()+
  labs(x = "Month", y = "TP (monthly avg)") +
  ggtitle("TP 2020") +  
  scale_y_continuous(limits = c(0, 0.6))+
  theme_bw()
TP20




#CHLA
chem2021chla=chem2021 %>% filter (SampleType=="chla")
# Create a new column for the month
chem2021chla$month <- format(chem2021chla$date, "%m")

# Calculate the monthly average
chem2021chla <- chem2021chla %>%
  group_by(SiteName, month) %>%
  summarise(avg_value = mean(Value, na.rm = TRUE))

chem2021chla$SiteName=as.factor(chem2021chla$SiteName)

chla21 <- ggplot(data=chem2021chla, aes(x = as.character(month), y = avg_value, color=as.character(SiteName), group=as.character(SiteName))) +
  geom_point() +
  geom_line()+
  labs(x = "Month", y = "chla (monthly avg)") +
  ggtitle("chla 2021") +
  scale_y_continuous(limits = c(0, 2.5))+
  theme_bw()
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

TN21 <- ggplot(data=chem2021TN, aes(x = as.character(month), y = avg_value, color=as.character(SiteName), group=as.character(SiteName))) +
  geom_point() +
  geom_line()+
  labs(x = "Month", y = "TN (monthly avg)") +
  ggtitle("TN 2021") +
  scale_y_continuous(limits = c(0, 40))+
  
  theme_bw()
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

TP21 <- ggplot(data=chem2021TP, aes(x = as.character(month), y = avg_value, color=as.character(SiteName), group=as.character(SiteName))) +
  geom_point() +
  geom_line()+
  labs(x = "Month", y = "TP (monthly avg)") +
  ggtitle("TP 2021") +
  scale_y_continuous(limits = c(0, 0.6))+
    theme_bw()
TP21


plot_grid(TN20, TN21, TP20, TP21, chla20, chla21, ncol=2)



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
  theme_bw()
swradplot20

SAradplot20 <- ggplot(merged_kalman_20_smoke_SA, aes(x = SurfaceArea_m2, y = GPP, color = SiteName)) +
  geom_point() +
  labs(x = "SA", y = "GPP") +
  ggtitle("GPP vs SA 2020") +
  theme_bw()
SAradplot20


####################################################################################################################
############################################################################################################################
########## deltas ###########################################################################################################
####################################################################################################################
########################################################################################################################


#calculate deltas for each metabolic rate based on different time periods, then plot
#use the dataframes named x_by_day

#calculate median rates for different seasons
#early summer (month of july)
earlysummer_med_gpp <- median(gpp_by_day %>% filter(date >= "2020-07-16", date <= "2020-07-30") %>% pull(Daily_GPP), na.rm = TRUE)
earlysummer_median_gpp <-gpp_by_day %>%  group_by(date, Smoke.day,SiteName) %>%  summarize(Median_GPP = median(Daily_GPP, na.rm = TRUE))
earlysummer_delta_gpp=earlysummer_median_gpp %>%  filter(date %in% as.Date(c("2020-07-16", "2020-07-30"))) %>%  group_by(SiteName) %>%
  summarize(GPP_difference = diff(Median_GPP))

earlysummer_med_nep <- median(nep_by_day %>% filter(date >= "2020-07-16", date <= "2020-07-30") %>% pull(Daily_NEP), na.rm = TRUE)
earlysummer_median_nep <-nep_by_day %>%  group_by(date, Smoke.day,SiteName) %>%  summarize(Median_NEP = median(Daily_NEP, na.rm = TRUE))
earlysummer_delta_nep=earlysummer_median_nep %>%  filter(date %in% as.Date(c("2020-07-16", "2020-07-30"))) %>%  group_by(SiteName) %>%
  summarize(NEP_difference = diff(Median_NEP))

earlysummer_med_r <- median(R_by_day %>% filter(date >= "2020-07-16", date <= "2020-07-30") %>% pull(Daily_R), na.rm = TRUE)
earlysummer_median_r <-R_by_day %>%  group_by(date, Smoke.day,SiteName) %>%  summarize(Median_R = median(Daily_R, na.rm = TRUE))
earlysummer_delta_r=earlysummer_median_r %>%  filter(date %in% as.Date(c("2020-07-16", "2020-07-30"))) %>%  group_by(SiteName) %>%
  summarize(R_difference = diff(Median_R))

earlysummer_delta_metab <- Reduce(function(x, y) merge(x, y, all=T), list(earlysummer_delta_gpp,earlysummer_delta_nep, earlysummer_delta_r))


#mid summer (month of august)
midsummer_med_gpp <- median(gpp_by_day %>% filter(date >= "2020-08-03", date <= "2020-08-30") %>% pull(Daily_GPP), na.rm = TRUE)
midsummer_median_gpp <-gpp_by_day %>%  group_by(date, Smoke.day,SiteName) %>%  summarize(Median_GPP = median(Daily_GPP, na.rm = TRUE))
midsummer_delta_gpp=midsummer_median_gpp %>%  filter(date %in% as.Date(c("2020-08-03", "2020-08-30"))) %>%  group_by(SiteName) %>%
  summarize(GPP_difference = diff(Median_GPP))

midsummer_med_nep <- median(nep_by_day %>% filter(date >= "2020-08-03", date <= "2020-08-30") %>% pull(Daily_NEP), na.rm = TRUE)
midsummer_median_nep <-nep_by_day %>%  group_by(date, Smoke.day,SiteName) %>%  summarize(Median_NEP = median(Daily_NEP, na.rm = TRUE))
midsummer_delta_nep=midsummer_median_nep %>%  filter(date %in% as.Date(c("2020-08-03", "2020-08-30"))) %>%  group_by(SiteName) %>%
  summarize(NEP_difference = diff(Median_NEP))

midsummer_med_r <- median(R_by_day %>% filter(date >= "2020-08-03", date <= "2020-08-30") %>% pull(Daily_R), na.rm = TRUE)
midsummer_median_r <-R_by_day %>%  group_by(date, Smoke.day,SiteName) %>%  summarize(Median_R = median(Daily_R, na.rm = TRUE))
midsummer_delta_r=midsummer_median_r %>%  filter(date %in% as.Date(c("2020-08-03", "2020-08-30"))) %>%  group_by(SiteName) %>%
  summarize(R_difference = diff(Median_R))

midsummer_delta_metab <- Reduce(function(x, y) merge(x, y, all=T), list(midsummer_delta_gpp,midsummer_delta_nep, midsummer_delta_r))


#late summer (month of september)
latesummer_med_gpp <- median(gpp_by_day %>% filter(date >= "2020-09-01", date <= "2020-09-30") %>% pull(Daily_GPP), na.rm = TRUE)
latesummer_median_gpp <-gpp_by_day %>%  group_by(date, Smoke.day,SiteName) %>%  summarize(Median_GPP = median(Daily_GPP, na.rm = TRUE))
latesummer_delta_gpp=latesummer_median_gpp %>%  filter(date %in% as.Date(c("2020-09-01", "2020-09-30"))) %>%  group_by(SiteName) %>%
  summarize(GPP_difference = diff(Median_GPP))

latesummer_med_nep <- median(nep_by_day %>% filter(date >= "2020-09-01", date <= "2020-09-30") %>% pull(Daily_NEP), na.rm = TRUE)
latesummer_median_nep <-nep_by_day %>%  group_by(date, Smoke.day,SiteName) %>%  summarize(Median_NEP = median(Daily_NEP, na.rm = TRUE))
latesummer_delta_nep=latesummer_median_nep %>%  filter(date %in% as.Date(c("2020-09-01", "2020-09-30"))) %>%  group_by(SiteName) %>%
  summarize(NEP_difference = diff(Median_NEP))

latesummer_med_r <- median(R_by_day %>% filter(date >= "2020-09-01", date <= "2020-09-30") %>% pull(Daily_R), na.rm = TRUE)
latesummer_median_r <-R_by_day %>%  group_by(date, Smoke.day,SiteName) %>%  summarize(Median_R = median(Daily_R, na.rm = TRUE))
latesummer_delta_r=latesummer_median_r %>%  filter(date %in% as.Date(c("2020-09-01", "2020-09-30"))) %>%  group_by(SiteName) %>%
  summarize(R_difference = diff(Median_R))

latesummer_delta_metab <- Reduce(function(x, y) merge(x, y, all=T), list(latesummer_delta_gpp,latesummer_delta_nep, latesummer_delta_r))


#fall (Sep15-Oct30)
fall_med_gpp <- median(gpp_by_day %>% filter(date >= "2020-09-15", date <= "2020-10-07") %>% pull(Daily_GPP), na.rm = TRUE)
fall_median_gpp <-gpp_by_day %>%  group_by(date, Smoke.day,SiteName) %>%  summarize(Median_GPP = median(Daily_GPP, na.rm = TRUE))
fall_delta_gpp=fall_median_gpp %>%  filter(date %in% as.Date(c("2020-09-15", "2020-10-07"))) %>%  group_by(SiteName) %>%
  summarize(GPP_difference = diff(Median_GPP))

fall_med_nep <- median(nep_by_day %>% filter(date >= "2020-09-15", date <= "2020-10-07") %>% pull(Daily_NEP), na.rm = TRUE)
fall_median_nep <-nep_by_day %>%  group_by(date, Smoke.day,SiteName) %>%  summarize(Median_NEP = median(Daily_NEP, na.rm = TRUE))
fall_delta_nep=fall_median_nep %>%  filter(date %in% as.Date(c("2020-09-15", "2020-10-07"))) %>%  group_by(SiteName) %>%
  summarize(NEP_difference = diff(Median_NEP))

fall_med_r <- median(R_by_day %>% filter(date >= "2020-09-15", date <= "2020-10-07") %>% pull(Daily_R), na.rm = TRUE)
fall_median_r <-R_by_day %>%  group_by(date, Smoke.day,SiteName) %>%  summarize(Median_R = median(Daily_R, na.rm = TRUE))
fall_delta_r=fall_median_r %>%  filter(date %in% as.Date(c("2020-09-15", "2020-10-07"))) %>%  group_by(SiteName) %>%
  summarize(R_difference = diff(Median_R))

fall_delta_metab <- Reduce(function(x, y) merge(x, y, all=T), list(fall_delta_gpp,fall_delta_nep, fall_delta_r))


#entire summer (June 1 - Oct. 31)
allsummer_med_gpp <- median(gpp_by_day %>% filter(date >= "2020-07-16", date <= "2020-10-07") %>% pull(Daily_GPP), na.rm = TRUE)
allsummer_median_gpp <-gpp_by_day %>%  group_by(date, Smoke.day,SiteName) %>%  summarize(Median_GPP = median(Daily_GPP, na.rm = TRUE))
allsummer_delta_gpp=allsummer_median_gpp %>%  filter(date %in% as.Date(c("2020-07-16", "2020-10-07"))) %>%  group_by(SiteName) %>%
  summarize(GPP_difference = diff(Median_GPP))

allsummer_med_nep <- median(nep_by_day %>% filter(date >= "2020-07-16", date <= "2020-10-07") %>% pull(Daily_NEP), na.rm = TRUE)
allsummer_median_nep <-nep_by_day %>%  group_by(date, Smoke.day,SiteName) %>%  summarize(Median_NEP = median(Daily_NEP, na.rm = TRUE))
allsummer_delta_nep=allsummer_median_nep %>%  filter(date %in% as.Date(c("2020-07-16", "2020-10-30"))) %>%  group_by(SiteName) %>%
  summarize(NEP_difference = diff(Median_NEP))

allsummer_med_r <- median(R_by_day %>% filter(date >= "2020-07-16", date <= "2020-10-07") %>% pull(Daily_R), na.rm = TRUE)
allsummer_median_r <-R_by_day %>%  group_by(date, Smoke.day,SiteName) %>%  summarize(Median_R = median(Daily_R, na.rm = TRUE))
allsummer_delta_r=allsummer_median_r %>%  filter(date %in% as.Date(c("2020-07-16", "2020-10-07"))) %>%  group_by(SiteName) %>%
  summarize(R_difference = diff(Median_R))

allsummer_delta_metab <- Reduce(function(x, y) merge(x, y, all=T), list(allsummer_delta_gpp,allsummer_delta_nep, allsummer_delta_r))


#pre-smoke (mid july to mid aug)
presmoke_med_gpp <- median(gpp_by_day %>% filter(date >= "2020-08-03", date <= "2020-08-16") %>% pull(Daily_GPP), na.rm = TRUE)
presmoke_median_gpp <-gpp_by_day %>%  group_by(date, Smoke.day,SiteName) %>%  summarize(Median_GPP = median(Daily_GPP, na.rm = TRUE))
presmoke_delta_gpp=presmoke_median_gpp %>%  filter(date %in% as.Date(c("2020-08-03", "2020-08-16"))) %>%  group_by(SiteName) %>%
  summarize(GPP_difference = diff(Median_GPP))

presmoke_med_nep <- median(nep_by_day %>% filter(date >= "2020-08-03", date <= "2020-08-16") %>% pull(Daily_NEP), na.rm = TRUE)
presmoke_median_nep <-nep_by_day %>%  group_by(date, Smoke.day,SiteName) %>%  summarize(Median_NEP = median(Daily_NEP, na.rm = TRUE))
presmoke_delta_nep=presmoke_median_nep %>%  filter(date %in% as.Date(c("2020-08-03", "2020-08-16"))) %>%  group_by(SiteName) %>%
  summarize(NEP_difference = diff(Median_NEP))

presmoke_med_r <- median(R_by_day %>% filter(date >= "2020-08-03", date <= "2020-08-16") %>% pull(Daily_R), na.rm = TRUE)
presmoke_median_r <-R_by_day %>%  group_by(date, Smoke.day,SiteName) %>%  summarize(Median_R = median(Daily_R, na.rm = TRUE))
presmoke_delta_r=presmoke_median_r %>%  filter(date %in% as.Date(c("2020-08-03", "2020-08-16"))) %>%  group_by(SiteName) %>%
  summarize(R_difference = diff(Median_R))

presmoke_delta_metab <- Reduce(function(x, y) merge(x, y, all=T), list(presmoke_delta_gpp, presmoke_delta_nep, presmoke_delta_r))


#during-smoke (oct)
smoke_med_gpp <- median(gpp_by_day %>% filter(date >= "2020-10-01", date <= "2020-10-07") %>% pull(Daily_GPP), na.rm = TRUE)
smoke_median_gpp <-gpp_by_day %>%  group_by(date, Smoke.day,SiteName) %>%  summarize(Median_GPP = median(Daily_GPP, na.rm = TRUE))
smoke_delta_gpp=smoke_median_gpp %>%  filter(date %in% as.Date(c("2020-10-01", "2020-10-07"))) %>%  group_by(SiteName) %>%
  summarize(GPP_difference = diff(Median_GPP))

smoke_med_nep <- median(nep_by_day %>% filter(date >= "2020-10-01", date <= "2020-10-07") %>% pull(Daily_NEP), na.rm = TRUE)
smoke_median_nep <-nep_by_day %>%  group_by(date, Smoke.day,SiteName) %>%  summarize(Median_NEP = median(Daily_NEP, na.rm = TRUE))
smoke_delta_nep=smoke_median_nep %>%  filter(date %in% as.Date(c("2020-10-01", "2020-10-07"))) %>%  group_by(SiteName) %>%
  summarize(NEP_difference = diff(Median_NEP))

smoke_med_r <- median(R_by_day %>% filter(date >= "2020-10-01", date <= "2020-10-07") %>% pull(Daily_R), na.rm = TRUE)
smoke_median_r <-R_by_day %>%  group_by(date, Smoke.day,SiteName) %>%  summarize(Median_R = median(Daily_R, na.rm = TRUE))
smoke_delta_r=smoke_median_r %>%  filter(date %in% as.Date(c("2020-10-01", "2020-10-07"))) %>%  group_by(SiteName) %>%
  summarize(R_difference = diff(Median_R))

smoke_delta_metab <- Reduce(function(x, y) merge(x, y, all=T), list(smoke_delta_gpp, smoke_delta_nep, smoke_delta_r))


#smoke-day

smokeday_delta_gpp <- gpp_by_day %>%
  group_by(SiteName, Smoke.day) %>%
  summarize(Median_Daily_GPP = median(Daily_GPP, na.rm = TRUE))%>%
  pivot_wider(names_from = Smoke.day, values_from = Median_Daily_GPP) %>%
  mutate(GPP_difference = y - n) %>%
  select(SiteName, GPP_difference)

smokeday_delta_nep <- nep_by_day %>%
  group_by(SiteName, Smoke.day) %>%
  summarize(Median_Daily_NEP = median(Daily_NEP, na.rm = TRUE)) %>%
  pivot_wider(names_from = Smoke.day, values_from = Median_Daily_NEP) %>%
  mutate(NEP_difference = y - n) %>%
  select(SiteName, NEP_difference)

smokeday_delta_r <- R_by_day %>%
  group_by(SiteName, Smoke.day) %>%
  summarize(Median_Daily_R = median(Daily_R, na.rm = TRUE)) %>%
  pivot_wider(names_from = Smoke.day, values_from = Median_Daily_R) %>%
  mutate(R_difference = y - n) %>%
  select(SiteName, R_difference)

smokeday_delta_metab <- Reduce(function(x, y) merge(x, y, all=T), list(smokeday_delta_gpp, smokeday_delta_nep, smokeday_delta_r))


#now plot the deltas

#early summer
ggplot(earlysummer_delta_metab, aes(x = R_difference, y = GPP_difference, color = SiteName)) +
  geom_point(size=3) +
  labs(x = "ΔR", y = "ΔGPP") +
  ggtitle("jul16-jul30 2020 - early summer?") +
  theme_minimal()

#mid summer
ggplot(midsummer_delta_metab, aes(x = R_difference, y = GPP_difference, color = SiteName)) +
  geom_point(size=3) +
  labs(x = "ΔR", y = "ΔGPP") +
  ggtitle("aug 3-aug30 2020 - mid summer?") +
  theme_minimal()


#late summer
ggplot(latesummer_delta_metab, aes(x = R_difference, y = GPP_difference, color = SiteName)) +
  geom_point(size=3) +
  labs(x = "ΔR", y = "ΔGPP") +
  ggtitle("sep1-sep30 2020 - latesummer?") +
  theme_minimal()

latesummer_delta_metab_no30=latesummer_delta_metab %>%
  filter(SiteName != "TOK30")

ggplot(latesummer_delta_metab_no30, aes(x = R_difference, y = GPP_difference, color = SiteName)) +
  geom_point(size=3) +
  labs(x = "ΔR", y = "ΔGPP") +
  ggtitle("sep1-sep30 2020 - latesummer? no tok30") +
  theme_minimal()

#fall
ggplot(fall_delta_metab, aes(x = R_difference, y = GPP_difference, color = SiteName)) +
  geom_point(size=3) +
  labs(x = "ΔR", y = "ΔGPP") +
  ggtitle("sep17-oct7 2020 - fall?") +
  theme_minimal()

fall_delta_metab_no30=fall_delta_metab %>%
  filter(SiteName != "TOK30")

ggplot(fall_delta_metab_no30, aes(x = R_difference, y = GPP_difference, color = SiteName)) +
  geom_point(size=3) +
  labs(x = "ΔR", y = "ΔGPP") +
  ggtitle("sep17-oct7 2020 - fall? no tok30") +
  theme_minimal()


#entire summer
ggplot(allsummer_delta_metab, aes(x = R_difference, y = GPP_difference, color = SiteName)) +
  geom_point(size=3) +
  labs(x = "ΔR", y = "ΔGPP") +
  ggtitle("july16-oct7 2020 - full summer") +
  theme_minimal()

allsummer_delta_metab_no30=allsummer_delta_metab %>%
  filter(SiteName != "TOK30")

ggplot(allsummer_delta_metab_no30, aes(x = R_difference, y = GPP_difference, color = SiteName)) +
  geom_point(size=3) +
  labs(x = "ΔR", y = "ΔGPP") +
  ggtitle("july16-oct7 2020 - allsummer? no tok30") +
  theme_minimal()

#pre-smoke
presmoke=ggplot(presmoke_delta_metab, aes(x = R_difference, y = GPP_difference, color = SiteName)) +
  geom_point(size=3) +
  labs(x = "ΔR", y = "ΔGPP") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +  # Add 1:1 line
    ggtitle("pre-smoke") +
  theme_minimal()

#during-smoke
duringsmoke=ggplot(smoke_delta_metab, aes(x = R_difference, y = GPP_difference, color = SiteName)) +
  geom_point(size=3) +
  labs(x = "ΔR", y = "ΔGPP") +
  ggtitle("during-smoke") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +  # Add 1:1 line
  theme_minimal()

plot_grid(presmoke, duringsmoke, ncol=1)


#smokeday 
ggplot(smokeday_delta_metab, aes(x = R_difference, y = GPP_difference, color = SiteName)) +
  geom_point(size=3) +
  labs(x = "ΔR", y = "ΔGPP") +
  ggtitle("smoke-day") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +  # Add 1:1 line
  theme_minimal()

#smokeday no TOK30
smokeday_delta_metab_no30=smokeday_delta_metab %>%
  filter(SiteName != "TOK30")
  
ggplot(smokeday_delta_metab_no30, aes(x = R_difference, y = GPP_difference, color = SiteName)) +
  geom_point(size=3) +
  labs(x = "ΔR", y = "ΔGPP") +
  ggtitle("smoke-day, no tok30") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +  # Add 1:1 line
  theme_minimal()




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
  theme_bw()

#plot chla x gpp
smokeday_chem_mean_chla <- smokeday_chem_mean %>%  filter(SampleType == "chla")

ggplot(smokeday_chem_mean_chla, aes(x = Average_Value, y = GPP_difference, color = SiteName)) +
  geom_point(size=3) +
  labs(x = "seasonal mean chla", y = "ΔGPP") +
  theme_bw()

#plot TN x gpp
smokeday_chem_mean_TN <- smokeday_chem_mean %>%  filter(SampleType == "TN")

ggplot(smokeday_chem_mean_TN, aes(x = Average_Value, y = GPP_difference, color = SiteName)) +
  geom_point(size=3) +
  labs(x = "seasonal mean TN", y = "ΔGPP") +
  theme_bw()

#plot TP x gpp
smokeday_chem_mean_TP <- smokeday_chem_mean %>%  filter(SampleType == "TP")

ggplot(smokeday_chem_mean_TP, aes(x = Average_Value, y = GPP_difference, color = SiteName)) +
  geom_point(size=3) +
  labs(x = "seasonal mean TP", y = "ΔGPP") +
  theme_bw()
