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
# Remove rows where "flag" column contains "y"
merged_kalman <- merged_kalman[merged_kalman$flag != "y", ]

merged_kalman_smoke <- merge(smoke_eml_20, merged_kalman, all=T)

merged_kalman_smoke <- merged_kalman_smoke[with(merged_kalman_smoke, date >= "2020-07-02" & date <= "2020-10-05"),]
smoke_eml_20 <- smoke_eml_20[with(smoke_eml_20, date >= "2020-07-02" & date <= "2020-10-05"),]
smoke_topaz_20 <- smoke_topaz_20[with(smoke_topaz_20, date >= "2020-07-02" & date <= "2020-10-05"),]

#daily GPP, by smoke day, by lake
gpp_by_day <- merged_kalman_smoke %>%
  group_by(date, Smoke.day, SiteName) %>%
  summarize(Daily_GPP = median(GPP, na.rm = TRUE))

gpp_by_day <- gpp_by_day %>% filter(!is.na(Smoke.day))

# Plot density plot of daily GPP 
density_plot_gpp <- ggplot(gpp_by_day, aes(x = Daily_GPP, fill = Smoke.day)) +
  geom_density(alpha = 0.5, show.legend = T) +
  scale_fill_manual(values = c( "lightskyblue","#fdbb84")) +
 # scale_fill_manual(values = c( "#af8dc3","#fdbb84")) +
      labs(x = "GPP", y = "Density") +
  ggtitle("Density Plot of Daily GPP") +
  theme_minimal()  +
  theme(panel.background = element_rect(fill = "white"),plot.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"), legend.box = "none")
density_plot_gpp

##################################################################################################
#plotsave_file_path <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Output/density_plot_gpp.png"

# Save the plot
#ggsave(plotsave_file_path, density_plot_gpp, dpi = 300)
###################################################################################################



# Plot density plot of swrad 
density_plot_swrad_20 <- ggplot(smoke_eml_20, aes(x = swrad, fill = Smoke.day)) +
  geom_density(alpha = 0.5, show.legend = T) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "lightskyblue","y" = "#fdbb84"), labels = c("No", "Yes")) +
  # scale_fill_manual(values = c( "#af8dc3","#fdbb84")) +
    labs(x = "swrad", y = "Density") +
  scale_x_continuous(limits=c(50, 400))+
  geom_text(x = -Inf, y = Inf, label = "eml met 2020", hjust = 0, vjust = 1, color = "black", size = 3) +
  theme_bw() +
  theme(panel.grid = element_blank()) 
  density_plot_swrad_20

density_plot_swrad_20_topaz <- ggplot(smoke_topaz_20, aes(x = swrad, fill = Smoke.day)) +
  geom_density(alpha = 0.5, show.legend = T) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "lightskyblue","y" = "#fdbb84"), labels = c("No", "Yes")) +
  # scale_fill_manual(values = c( "#af8dc3","#fdbb84")) +
  labs(x = "swrad", y = "Density") +
  scale_x_continuous(limits=c(50, 400))+
  geom_text(x = -Inf, y = Inf, label = "topaz met 2020", hjust = 0, vjust = 1, color = "black", size = 3) +
  theme_bw()+
  theme(panel.grid = element_blank())
density_plot_swrad_20_topaz

density_plot_swrad_21 <- ggplot(smoke_eml_21, aes(x = swrad, fill = Smoke.day)) +
  geom_density(alpha = 0.5, show.legend = T) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "lightskyblue","y" = "#fdbb84"), labels = c("No", "Yes")) +
  # scale_fill_manual(values = c( "#af8dc3","#fdbb84")) +
  labs(x = "swrad", y = "Density") +
  scale_x_continuous(limits=c(50, 400))+
  geom_text(x = -Inf, y = Inf, label = "eml met 2021", hjust = 0, vjust = 1, color = "black", size = 3) +
  theme_bw()+
  theme(panel.grid = element_blank()) 
density_plot_swrad_21

density_plot_swrad_21_topaz <- ggplot(smoke_topaz_21, aes(x = swrad, fill = Smoke.day)) +
  geom_density(alpha = 0.5, show.legend = T) +
  scale_fill_manual(name = "Smoke Day", values = c( "n" = "lightskyblue","y" = "#fdbb84"), labels = c("No", "Yes")) +
  # scale_fill_manual(values = c( "#af8dc3","#fdbb84")) +
  labs(x = "swrad", y = "Density") +
  scale_x_continuous(limits=c(50, 400))+
  geom_text(x = -Inf, y = Inf, label = "topaz met 2021", hjust = 0, vjust = 1, color = "black", size = 3) +
  theme_bw()+
  theme(panel.grid = element_blank())
density_plot_swrad_21_topaz

plot_grid(density_plot_swrad_20, density_plot_swrad_20_topaz, density_plot_swrad_21, density_plot_swrad_21_topaz, ncol=2)

plot_grid(density_plot_swrad_20, density_plot_swrad_21, ncol=1)



#####################################################################################################



ggplot(gpp_by_day, aes(x = Daily_GPP, fill = Smoke.day)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~SiteName, ncol=2) +
  labs(x = "Daily GPP", y = "Density") +
  ggtitle("Density Plot of Daily GPP by SiteName") +
  theme_minimal()


# Convert Smoke.day to a factor for t-test
gpp_by_day$Smoke.day <- as.factor(gpp_by_day$Smoke.day)
gpp_by_day$year = year(gpp_by_day$date)

# Split the data by SiteName
site_split <- split(gpp_by_day, gpp_by_day$SiteName)

# Create a list to store t-test results
t_test_results <- list()

# Reorder SiteNames by size
site_order <- c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30")  

# List to store plots
plot_list_GPP_20 <- list()

# Dataframe to store median GPP by Smoke.day and SiteName
median_gpp_df <- data.frame()

# Loop through each SiteName in the specified order
for (site in site_order) {
  # Perform t-test
  t_test_result <- t.test(Daily_GPP ~ Smoke.day, data = site_split[[site]])
  
  # Label if significant
  significance_label <- ifelse(t_test_result$p.value < 0.05, "*", "")
  
  # Extract year from the data
  year_label <- unique(site_split[[site]]$year)
  
  # Calculate median Daily_GPP for each Smoke.day group
  median_data <- site_split[[site]] %>%
    group_by(Smoke.day) %>%
    summarise(median_gpp = median(Daily_GPP, na.rm = TRUE))
  
  # Plot density
  density_plot <- ggplot(site_split[[site]], aes(x = Daily_GPP, fill = Smoke.day)) +
    geom_density(alpha = 0.5, show.legend = T) +
    scale_fill_manual(name = "Smoke Day", values = c( "n" = "lightskyblue","y" = "#fdbb84"), labels = c("No", "Yes")) +
    labs(x = "Daily GPP", y = "Density") +
    scale_x_continuous(limits=c(-0.5, 1.5))+
    geom_vline(data = median_data, aes(xintercept = median_gpp, color = Smoke.day), linetype = "longdash", size=1, show.legend = FALSE) +  # Add median line
    scale_color_manual(name = "Smoke Day", values = c( "n" = "lightskyblue","y" = "#fdbb84"), guide="none") + # Define colors for dotted lines
    
   # geom_text(x = -Inf, y = Inf, label = paste(site, year_label), hjust = 0, vjust = 1, color = "black", size = 3) +
    # Add t-test results to the plot
    annotate("text", x = Inf, y = Inf,
             label = significance_label,
             color = ifelse(t_test_result$p.value < 0.05, "black", "black"), hjust = 1, vjust = 1, size=8)+  # Color the asterisk darkorange if p < 0.05
  theme_bw()+
    theme(panel.grid = element_blank())

    
  # Add the plot to the list
  plot_list_GPP_20[[site]] <- density_plot
  
  # Add SiteName to the median data
  median_data$SiteName <- site
  
  # Combine with existing median_gpp_df
  median_gpp_df <- bind_rows(median_gpp_df, median_data)
}

# Arrange plots in a grid
plot_grid(plotlist = plot_list_GPP_20, ncol = 2)



# Convert Smoke.day to factor for correct ordering
median_gpp_df$Smoke.day <- factor(median_gpp_df$Smoke.day, levels = c("n", "y"))

# Pivot the data to have Smoke.day values as columns
deltagpp <- median_gpp_df %>%
  pivot_wider(names_from = Smoke.day, values_from = median_gpp)

# Calculate the difference
deltagpp$diff_n_y <- deltagpp$y - deltagpp$n

deltagpp$SiteName <- factor(deltagpp$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1", "TOK11", "TOPAZPOND", "TOK30"))

deltagpp_plot=ggplot(deltagpp, aes(x = SiteName, y = diff_n_y)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Site (ordered largest to smallest)", y = "ΔGPP", title="2020") +
  theme_bw()+
  theme(panel.grid = element_blank())
deltagpp_plot

deltagpp_plot=ggplot(deltagpp, aes(x = SiteName, y = diff_n_y)) +
  geom_point(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Site (ordered largest to smallest)", y = "ΔGPP", title="2020") +
  theme_bw()+
  theme(panel.grid = element_blank())
deltagpp_plot

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
nep_by_day$year = year(nep_by_day$date)

# Split the data by SiteName
site_split <- split(nep_by_day, nep_by_day$SiteName)

# Create a list to store t-test results
t_test_results <- list()

# Reorder SiteNames by size
site_order <- c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30")  

# List to store plots
plot_list_NEP_20 <- list()

# Dataframe to store median NEP by Smoke.day and SiteName
median_nep_df <- data.frame()

# Loop through each SiteName in the specified order
for (site in site_order) {
  # Perform t-test
  t_test_result <- t.test(Daily_NEP ~ Smoke.day, data = site_split[[site]])
  
  # Label if significant
  significance_label <- ifelse(t_test_result$p.value < 0.05, "*", "")
  
  # Extract year from the data
  year_label <- unique(site_split[[site]]$year)
  
  # Calculate median Daily_NEP for each Smoke.day group
  median_data <- site_split[[site]] %>%
    group_by(Smoke.day) %>%
    summarise(median_nep = median(Daily_NEP, na.rm = TRUE))
  
  
  # Plot density
  density_plot <- ggplot(site_split[[site]], aes(x = Daily_NEP, fill = Smoke.day)) +
    geom_density(alpha = 0.5, show.legend = T) +
    scale_fill_manual(name = "Smoke Day", values = c( "n" = "lightskyblue","y" = "#fdbb84"), labels = c("No", "Yes")) +
    labs(x = "Daily NEP", y = "Density") +
    #geom_text(x = -Inf, y = Inf, label = paste(site, year_label), hjust = 0, vjust = 1, color = "black", size = 3) +
    scale_x_continuous(limits=c(-1, 1))+
    geom_vline(data = median_data, aes(xintercept = median_nep, color = Smoke.day), linetype = "longdash", size=1, show.legend = FALSE) +  # Add median line
    scale_color_manual(name = "Smoke Day", values = c( "n" = "lightskyblue","y" = "#fdbb84"), guide="none") + # Define colors for dotted lines
    
    theme_bw()+
    theme(panel.grid = element_blank())+
    # Add t-test results to the plot
    annotate("text", x = Inf, y = Inf,
             label = significance_label,
             color = ifelse(t_test_result$p.value < 0.05, "black", "black"), hjust = 1, vjust = 1, size=8)  # Color the asterisk darkorange if p < 0.05
  
  # Add the plot to the list
  plot_list_NEP_20[[site]] <- density_plot
  
  # Add SiteName to the median data
  median_data$SiteName <- site
  
  # Combine with existing median_gpp_df
  median_nep_df <- bind_rows(median_nep_df, median_data)
}

# Arrange plots in a grid
plot_grid(plotlist = plot_list_NEP_20, ncol = 2)








# Convert Smoke.day to factor for correct ordering
median_nep_df$Smoke.day <- factor(median_nep_df$Smoke.day, levels = c("n", "y"))

# Pivot the data to have Smoke.day values as columns
deltanep <- median_nep_df %>%
  pivot_wider(names_from = Smoke.day, values_from = median_nep)

# Calculate the difference
deltanep$diff_n_y <- deltanep$n - deltanep$y

deltanep$SiteName <- factor(deltanep$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1", "TOK11", "TOPAZPOND", "TOK30"))

deltanep_plot=ggplot(deltanep, aes(x = SiteName, y = diff_n_y)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Site (ordered largest to smallest)", y = "ΔNEP", title="2020") +
  theme_bw()+
  theme(panel.grid = element_blank())
deltanep_plot

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
R_by_day$year = year(R_by_day$date)

# Split the data by SiteName
site_split <- split(R_by_day, R_by_day$SiteName)

# Create a list to store t-test results
t_test_results <- list()

# Reorder SiteNames by size
site_order <- c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30")  



# List to store plots
plot_list_R_20 <- list()

# Dataframe to store median NEP by Smoke.day and SiteName
median_r_df <- data.frame()

# Loop through each SiteName in the specified order
for (site in site_order) {
  # Perform t-test
  t_test_result <- t.test(Daily_R ~ Smoke.day, data = site_split[[site]])
  
  # Label if significant
  significance_label <- ifelse(t_test_result$p.value < 0.05, "*", "")
  
  # Extract year from the data
  year_label <- unique(site_split[[site]]$year)
  
  # Calculate median Daily_R for each Smoke.day group
  median_data <- site_split[[site]] %>%
    group_by(Smoke.day) %>%
    summarise(median_r = median(Daily_R, na.rm = TRUE))
  
  # Plot density
  density_plot <- ggplot(site_split[[site]], aes(x = Daily_R, fill = Smoke.day)) +
    geom_density(alpha = 0.5, show.legend = T) +
    scale_fill_manual(name = "Smoke Day", values = c( "n" = "lightskyblue","y" = "#fdbb84"), labels = c("No", "Yes")) +
    labs(x = "Daily R", y = "Density") +
   # geom_text(x = -Inf, y = Inf, label = paste(site, year_label), hjust = 0, vjust = 1, color = "black", size = 3) +
    scale_x_continuous(limits=c(-0.5, 3))+
    geom_vline(data = median_data, aes(xintercept = median_r, color = Smoke.day), linetype = "longdash", size=1, show.legend = FALSE) +  # Add median line
    scale_color_manual(name = "Smoke Day", values = c( "n" = "lightskyblue","y" = "#fdbb84"), guide="none") + # Define colors for dotted lines
    
    theme_bw()+
    theme(panel.grid = element_blank())+
    # Add t-test results to the plot
    annotate("text", x = Inf, y = Inf,
             label = significance_label,
             color = ifelse(t_test_result$p.value < 0.05, "black", "black"), hjust = 1, vjust = 1, size=8)  # Color the asterisk darkorange if p < 0.05
  
  # Add the plot to the list
  plot_list_R_20[[site]] <- density_plot
  
  # Add SiteName to the median data
  median_data$SiteName <- site
  
  # Combine with existing median_gpp_df
  median_r_df <- bind_rows(median_r_df, median_data)
}

# Arrange plots in a grid
plot_grid(plotlist = plot_list_R_20, ncol = 2)


# Convert Smoke.day to factor for correct ordering
median_r_df$Smoke.day <- factor(median_r_df$Smoke.day, levels = c("n", "y"))

# Pivot the data to have Smoke.day values as columns
deltar <- median_r_df %>%
  pivot_wider(names_from = Smoke.day, values_from = median_r)

# Calculate the difference
deltar$diff_y_n <- deltar$y - deltar$n

deltar$SiteName <- factor(deltar$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1", "TOK11", "TOPAZPOND", "TOK30"))

deltar_plot=ggplot(deltar, aes(x = SiteName, y = diff_y_n)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Site (ordered largest to smallest)", y = "ΔR", title="2020") +
  theme_bw()+
  theme(panel.grid = element_blank())
deltar_plot



##############################################################################################################################
################ 2021 density plots ###################################################################################################
######################################################################################################################
###############################################################################################################################################################

#load 2021 data

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


merged_kalman_21 <- Reduce(function(x, y) merge(x, y, all=T), list(kalman.emlpond1.21,  kalman.TOK11.21, kalman.TOPAZPOND.21,kalman.TOK30.21, metab_eml_21, metab_topaz_21))

merged_ts.data_21 <- Reduce(function(x, y) merge(x, y, all=T), list(ts.data.emlpond1.21, ts.data.TOK11.21, ts.data.TOPAZPOND.21,ts.data.TOK30.21, ts.emerald21, ts.topaz21))

###############################################################################################################################################################
merged_kalman_21_smoke <- merged_kalman_21_smoke[with(merged_kalman_21_smoke, date >= "2021-07-02" & date <= "2021-10-05"),]
smoke_eml_21 <- smoke_eml_21[with(smoke_eml_21, date >= "2021-07-02" & date <= "2021-10-05"),]
smoke_topaz_21 <- smoke_topaz_21[with(smoke_topaz_21, date >= "2021-07-02" & date <= "2021-10-05"),]

#
# Separate and plot GPP for days when Smoke.day is "y" and "n" (single value)

# Remove rows where "flag" column contains "y"
merged_kalman_21_smoke <- merged_kalman_21_smoke[merged_kalman_21_smoke$flag != "y", ]

#daily GPP, by smoke day, by lake
gpp_by_day <- merged_kalman_21_smoke %>%
  group_by(date, Smoke.day, SiteName) %>%
  summarize(Daily_GPP = median(GPP, na.rm = TRUE))

gpp_by_day <- gpp_by_day %>% filter(!is.na(Smoke.day))

# Plot density plot of daily GPP 
density_plot_gpp <- ggplot(gpp_by_day, aes(x = Daily_GPP, fill = Smoke.day)) +
  geom_density(alpha = 0.5, show.legend = T) +
  scale_fill_manual(values = c( "#af8dc3","#fdbb84")) +
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
gpp_by_day$year = year(gpp_by_day$date)

# Split the data by SiteName
site_split <- split(gpp_by_day, gpp_by_day$SiteName)

# Create a list to store t-test results
t_test_results <- list()

# Reorder SiteNames by size
site_order <- c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30")  

# List to store plots
plot_list_GPP_21 <- list()

# Dataframe to store median GPP by Smoke.day and SiteName
median_gpp_df <- data.frame()

# Loop through each SiteName in the specified order
for (site in site_order) {
  # Perform t-test
  t_test_result <- t.test(Daily_GPP ~ Smoke.day, data = site_split[[site]])
  
  # Label if significant
  significance_label <- ifelse(t_test_result$p.value < 0.05, "*", "")
  
  # Extract year from the data
  year_label <- unique(site_split[[site]]$year)
  
  # Calculate median Daily_GPP for each Smoke.day group
  median_data <- site_split[[site]] %>%
    group_by(Smoke.day) %>%
    summarise(median_gpp = median(Daily_GPP, na.rm = TRUE))
  
  
  # Plot density
  density_plot <- ggplot(site_split[[site]], aes(x = Daily_GPP, fill = Smoke.day)) +
    geom_density(alpha = 0.5, show.legend = T) +
    scale_fill_manual(name = "Smoke Day", values = c( "n" = "lightskyblue","y" = "#fdbb84"), labels = c("No", "Yes")) +
    labs(x = "Daily GPP", y = "Density") +
    #geom_text(x = -Inf, y = Inf, label = paste(site, year_label), hjust = 0, vjust = 1, color = "black", size = 3) +
    scale_x_continuous(limits=c(-0.5, 2))+
    geom_vline(data = median_data, aes(xintercept = median_gpp, color = Smoke.day), linetype = "longdash", size=1, show.legend = FALSE) +  # Add median line
    scale_color_manual(name = "Smoke Day", values = c( "n" = "lightskyblue","y" = "#fdbb84"), guide="none") + # Define colors for dotted lines
    
    theme_bw()+
    theme(panel.grid = element_blank())+
    # Add t-test results to the plot
    annotate("text", x = Inf, y = Inf,
             label = significance_label,
             color = ifelse(t_test_result$p.value < 0.05, "black", "black"), hjust = 1, vjust = 1, size=8)  # Color the asterisk darkorange if p < 0.05
  

  # Add the plot to the list
  plot_list_GPP_21[[site]] <- density_plot
  
  # Add SiteName to the median data
  median_data$SiteName <- site
  
  # Combine with existing median_gpp_df
  median_gpp_df <- bind_rows(median_gpp_df, median_data)
}

# Arrange plots in a grid
plot_grid(plotlist = plot_list_GPP_20, ncol = 2)



# Convert Smoke.day to factor for correct ordering
median_gpp_df$Smoke.day <- factor(median_gpp_df$Smoke.day, levels = c("n", "y"))

# Pivot the data to have Smoke.day values as columns
deltagpp_21 <- median_gpp_df %>%
  pivot_wider(names_from = Smoke.day, values_from = median_gpp)

# Calculate the difference
deltagpp_21$diff_n_y <- deltagpp_21$y - deltagpp_21$n

deltagpp_21$SiteName <- factor(deltagpp_21$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1", "TOK11", "TOPAZPOND", "TOK30"))

deltagpp_21_plot=ggplot(deltagpp_21, aes(x = SiteName, y = diff_n_y)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Site (ordered largest to smallest)", y = "ΔGPP", title="2021") +
  theme_bw()+
  theme(panel.grid = element_blank())
deltagpp_21_plot

deltagpp$year=2020
deltagpp_21$year=2021
merge=merge(deltagpp, deltagpp_21, all=T)

wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data"
setwd(wd)
maxdepth=read.csv("Tokopahmaxdepth.csv", header=T)
merge2=merge(maxdepth, merge, all=T)

ggplot(merge, aes(x = SiteName, y = diff_n_y, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Site (ordered largest to smallest)", y = "ΔGPP", fill="Year") +
  scale_fill_manual(values=c("#d8b365","#5ab4ac"))+
  theme_bw()+
  theme(panel.grid = element_blank())


ggplot(merge, aes(x = SiteName, y = diff_n_y, color = factor(year))) +
  geom_point(stat = "identity", position = "dodge", size=4) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +  # Adding dotted line at y=0
    labs(x = "Site (ordered largest to smallest)", y = "ΔGPP", color="Year") +
  scale_color_manual(values=c("#d8b365","#5ab4ac"))+
  guides(color = guide_legend(title.theme = element_text(size = 14)))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 12),  # Adjust font size of axis text
        axis.title = element_text(size = 14),
        legend.text = element_text(size=14))

ggplot(merge2, aes(x = MaxDepth, y = diff_n_y, color = factor(year), fill = factor(year))) +
  geom_point(stat = "identity", position = "dodge", size = 4, shape = 21, stroke = 1) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  labs(x = "Maximum depth (m)", y = "ΔGPP", color = "Year", fill = "Year") +
  scale_color_manual(values = c("black", "black"), guide = "none") +
  scale_fill_manual(values = c("white", "darkgray")) +
  guides(fill = guide_legend(title.theme = element_text(size = 14))) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14))


library(ggplot2)

deltagpp=ggplot(merge2, aes(x = MaxDepth, y = diff_n_y)) +
  geom_point(aes(color = factor(year), fill = factor(year)), stat = "identity", position = "dodge", size = 4, shape = 21, stroke = 1) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  labs(x = "Maximum depth (m)", y = "ΔGPP", color = "Year", fill = "Year") +
  scale_color_manual(values = c("black", "black"), guide = "none") +
  scale_fill_manual(values = c("white", "darkgray")) +
  guides(fill = guide_legend(title.theme = element_text(size = 14))) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14))
deltagpp

#get the r2
model <- lm(diff_n_y ~ MaxDepth, data = merge2)
summary_model <- summary(model)
r_squared <- summary_model$r.squared

wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Output"
setwd(wd) 

ggsave("ΔGPP.png", plot = deltagpp, width = 7, height = 4.5, units = "in", dpi = 300)
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
nep_by_day$year = year(nep_by_day$date)

# Split the data by SiteName
site_split <- split(nep_by_day, nep_by_day$SiteName)

# Create a list to store t-test results
t_test_results <- list()

# Reorder SiteNames by size
site_order <- c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30")  

# List to store plots
plot_list_NEP_21 <- list()

# Loop through each SiteName in the specified order
for (site in site_order) {
  # Perform t-test
  t_test_result <- t.test(Daily_NEP ~ Smoke.day, data = site_split[[site]])
  
  # Label if significant
  significance_label <- ifelse(t_test_result$p.value < 0.05, "*", "")
  
  # Extract year from the data
  year_label <- unique(site_split[[site]]$year)
  
  # Calculate median Daily_GPP for each Smoke.day group
  median_data <- site_split[[site]] %>%
    group_by(Smoke.day) %>%
    summarise(median_nep = median(Daily_NEP, na.rm = TRUE))
  
  # Plot density
  density_plot <- ggplot(site_split[[site]], aes(x = Daily_NEP, fill = Smoke.day)) +
    geom_density(alpha = 0.5, show.legend = T) +
    scale_fill_manual(name = "Smoke Day", values = c( "n" = "lightskyblue","y" = "#fdbb84"), labels = c("No", "Yes")) +
    labs(x = "Daily NEP", y = "Density") +
   # geom_text(x = -Inf, y = Inf, label = paste(site, year_label), hjust = 0, vjust = 1, color = "black", size = 3) +
    scale_x_continuous(limits=c(-1, 1))+
    geom_vline(data = median_data, aes(xintercept = median_nep, color = Smoke.day), linetype = "longdash", size=1, show.legend = FALSE) +  # Add median line
    scale_color_manual(name = "Smoke Day", values = c( "n" = "lightskyblue","y" = "#fdbb84"), guide="none") + # Define colors for dotted lines
    
    theme_bw()+
    theme(panel.grid = element_blank())+
    # Add t-test results to the plot
    annotate("text", x = Inf, y = Inf,
             label = significance_label,
             color = ifelse(t_test_result$p.value < 0.05, "black", "black"), hjust = 1, vjust = 1, size=8)  # Color the asterisk darkorange if p < 0.05
  
  # Add the plot to the list
  plot_list_NEP_21[[site]] <- density_plot
}

# Arrange plots in a grid
plot_grid(plotlist = plot_list_NEP_21, ncol = 2)



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
R_by_day$year = year(R_by_day$date)

# Split the data by SiteName
site_split <- split(R_by_day, R_by_day$SiteName)

# Create a list to store t-test results
t_test_results <- list()

# Reorder SiteNames by size
site_order <- c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30")  

# Find the overall range of Daily_R for setting xlim
overall_range <- range(unlist(lapply(site_split, function(x) x$Daily_R)))


# List to store plots
plot_list_R_21 <- list()

median_r_df <- data.frame()

# Loop through each SiteName in the specified order
for (site in site_order) {
  # Perform t-test
  t_test_result <- t.test(Daily_R ~ Smoke.day, data = site_split[[site]])
  
  # Label if significant
  significance_label <- ifelse(t_test_result$p.value < 0.05, "*", "")
  
  # Extract year from the data
  year_label <- unique(site_split[[site]]$year)
  
  # Calculate median Daily_GPP for each Smoke.day group
  median_data <- site_split[[site]] %>%
    group_by(Smoke.day) %>%
    summarise(median_r = median(Daily_R, na.rm = TRUE))
  
  # Plot density
  density_plot <- ggplot(site_split[[site]], aes(x = Daily_R, fill = Smoke.day)) +
    geom_density(alpha = 0.5, show.legend = T) +
    scale_fill_manual(name = "Smoke Day", values = c( "n" = "lightskyblue","y" = "#fdbb84"), labels = c("No", "Yes")) +
    labs(x = "Daily R", y = "Density") +
   # geom_text(x = -Inf, y = Inf, label = paste(site, year_label), hjust = 0, vjust = 1, color = "black", size = 3) +
    scale_x_continuous(limits=c(-0.5, 2.5))+
    geom_vline(data = median_data, aes(xintercept = median_r, color = Smoke.day), linetype = "longdash", size=1, show.legend = FALSE) +  # Add median line
    scale_color_manual(name = "Smoke Day", values = c( "n" = "lightskyblue","y" = "#fdbb84"), guide="none") + # Define colors for dotted lines
    
    theme_bw()+
    theme(panel.grid = element_blank())+
    # Add t-test results to the plot
    annotate("text", x = Inf, y = Inf,
             label = significance_label,
             color = ifelse(t_test_result$p.value < 0.05, "black", "black"), hjust = 1, vjust = 1, size=8)  # Color the asterisk darkorange if p < 0.05
  # Add SiteName to the median data
  median_data$SiteName <- site
  
  # Combine with existing median_gpp_df
  median_r_df <- bind_rows(median_r_df, median_data)
  
  # Add the plot to the list
  plot_list_R_21[[site]] <- density_plot
}

# Arrange plots in a grid
plot_grid(plotlist = plot_list_R_21, ncol = 2)





# Convert Smoke.day to factor for correct ordering
median_r_df$Smoke.day <- factor(median_r_df$Smoke.day, levels = c("n", "y"))

# Pivot the data to have Smoke.day values as columns
deltar_21 <- median_r_df %>%
  pivot_wider(names_from = Smoke.day, values_from = median_r)

# Calculate the difference
deltar_21$diff_y_n <- deltar_21$y - deltar_21$n

deltar_21$SiteName <- factor(deltar_21$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1", "TOK11", "TOPAZPOND", "TOK30"))

deltar_plot=ggplot(deltar_21, aes(x = SiteName, y = diff_y_n)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Site (ordered largest to smallest)", y = "ΔR", title="2020") +
  theme_bw()+
  theme(panel.grid = element_blank())
deltar_plot




deltar$year=2020
deltar_21$year=2021
merge=merge(deltar, deltar_21, all=T)

ggplot(merge, aes(x = SiteName, y = diff_y_n, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Site (ordered largest to smallest)", y = "ΔR", fill="Year") +
  scale_fill_manual(values=c("#d8b365","#5ab4ac"))+
  theme_bw()+
  theme(panel.grid = element_blank())


ggplot(merge, aes(x = SiteName, y = diff_y_n, color = factor(year))) +
  geom_point(stat = "identity", position = "dodge", size=4) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +  # Adding dotted line at y=0
  labs(x = "Site (ordered largest to smallest)", y = "ΔR", color="Year") +
  scale_color_manual(values=c("#d8b365","#5ab4ac"))+
  guides(color = guide_legend(title.theme = element_text(size = 14)))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 12),  # Adjust font size of axis text
        axis.title = element_text(size = 14),
        legend.text = element_text(size=14))
############################################################################################################

##############################################################################################################################
################ 2022 density plots ###################################################################################################
######################################################################################################################
###############################################################################################################################################################

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


#merge
merged_kalman_22a <- Reduce(function(x, y) merge(x, y, all=T), list(kalman.emlpond1.22,  kalman.TOK11.22,  kalman.emerald.22))
merged_kalman_22a$datetime=NULL
merged_kalman_22a_smoke <- merged_kalman_22a

merged_kalman_22b <- Reduce(function(x, y) merge(x, y, all=T), list(kalman.TOPAZPOND.22,kalman.TOK30.22, kalman.topaz.22))
merged_kalman_22b$datetime=NULL
merged_kalman_22b_smoke <- merged_kalman_22b

merged_kalman_22 = merge(merged_kalman_22a, merged_kalman_22b, all=T)
merged_kalman_22$R = merged_kalman_22$R * -1

merged_kalman_22_smoke = merge(merged_kalman_22a_smoke, merged_kalman_22b_smoke, all=T)
merged_kalman_22_smoke$R = merged_kalman_22_smoke$R * -1

#remove flagged columns
merged_kalman_22_smoke = merged_kalman_22_smoke [merged_kalman_22_smoke$flag !="y",]

#add dummy smoke column
merged_kalman_22_smoke$Smoke.day="n"

#merged_kalman_22 <- Reduce(function(x, y) merge(x, y, all=T), list(kalman.emlpond1.22,  kalman.TOK11.22, kalman.TOPAZPOND.22,kalman.TOK30.22, metab_eml_22, metab_topaz_22))

merged_ts.data_22 <- Reduce(function(x, y) merge(x, y, all=T), list(ts.data.emlpond1.22, ts.data.TOK11.22, ts.data.TOPAZPOND.22,ts.data.TOK30.22, ts.data.emerald.22, ts.data.topaz.22))

###############################################################################################################################################################

#
# Separate and plot GPP for days when Smoke.day is "y" and "n" (single value)
# Remove rows where "flag" column contains "y"
merged_kalman_22_smoke <- merged_kalman_22_smoke[merged_kalman_22_smoke$flag != "y", ]
merged_kalman_22_smoke <- merged_kalman_22_smoke[with(merged_kalman_22_smoke, date >= "2022-07-02" & date <= "2022-10-05"),]



#daily GPP,  by lake
gpp_by_day <- merged_kalman_22_smoke %>%
  group_by(date, Smoke.day, SiteName) %>%
  summarize(Daily_GPP = median(GPP, na.rm = TRUE))


# Plot density plot of daily GPP 
density_plot_gpp <- ggplot(gpp_by_day, aes(x = Daily_GPP,fill = Smoke.day)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#af8dc3","#fdbb84" )) +
  scale_x_continuous(limits=c(-0.5, 1.5))+
    labs(x = "GPP", y = "Density") +
  ggtitle("Density Plot of Daily GPP") +
  theme_minimal() 
density_plot_gpp



ggplot(gpp_by_day, aes(x = Daily_GPP,fill = Smoke.day)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#af8dc3","#fdbb84" )) +
  scale_x_continuous(limits=c(-0.5, 1.5))+
      facet_wrap(~SiteName, scales="free_y", ncol=2) +
  labs(x = "Daily GPP", y = "Density") +
  ggtitle("Density Plot of Daily GPP 2022 by SiteName") +
  theme_minimal()

#store plots in a list
plot_list_GPP_22 <- list()

site_order <- c("Emerald", "Topaz", "EMLPOND1", "TOK11", "TOPAZPOND", "TOK30")


# Loop through each SiteName
for (site in site_order) {
  # Filter data for the current SiteName
  subset_data <- subset(gpp_by_day, SiteName == site)
  
  # Extract year from the Date column
  library(lubridate)
  subset_data$year <- year(subset_data$date)
  
  # Calculate median for the entire data
  median_GPP <- median(subset_data$Daily_GPP)
  
  # Create the plot for the current SiteName
  plot <- ggplot(subset_data, aes(x = Daily_GPP, fill = Smoke.day)) +
    geom_density(alpha = 0.5, show.legend = F) +
    scale_fill_manual(values = c("lightskyblue", "#fdbb84")) +
    scale_x_continuous(limits = c(-0.5, 1.5)) +
    labs(x = "Daily GPP", y = "Density") +
   # geom_text(x = -Inf, y = Inf, label = paste(site, subset_data$year[1]), hjust = 0, vjust = 1, color = "black", size = 3) +
    geom_vline(xintercept = median_GPP, linetype = "longdash", color="lightskyblue", size=1)+  # Add dotted line at the median
  
    theme_bw()+
    theme(panel.grid = element_blank())  
  # Add the plot to the list
  plot_list_GPP_22[[site]] <- plot
}
# Arrange plots in a grid
plot_grid(plotlist = plot_list_GPP_22, ncol = 2)

####################################################################################################################


#daily NEP, by smoke day, by lake
nep_by_day <- merged_kalman_22_smoke %>%
  group_by(date, Smoke.day, SiteName) %>%
  summarize(Daily_NEP = median(NEP, na.rm = TRUE))

density_plot_nep <- ggplot(nep_by_day, aes(x = Daily_NEP,fill = Smoke.day)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#af8dc3","#fdbb84" )) +
  scale_x_continuous(limits=c(-1, 1))+
      labs(x = "NEP", y = "Density") +
  ggtitle("Density Plot of Daily NEP") +
  theme_minimal() 
density_plot_nep

ggplot(nep_by_day, aes(x = Daily_NEP,fill = Smoke.day)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#af8dc3","#fdbb84" )) +
  scale_x_continuous(limits=c(-1, 1))+
      facet_wrap(~SiteName, ncol=2) +
  labs(x = "Daily NEP", y = "Density") +
  ggtitle("Density Plot of Daily NEP 2022 by SiteName") +
  theme_bw()+
  theme(panel.grid = element_blank())

#store plots in a list
plot_list_NEP_22 <- list()

site_order <- c("Emerald", "Topaz", "EMLPOND1", "TOK11", "TOPAZPOND", "TOK30")


# Loop through each SiteName
for (site in site_order) {
  # Filter data for the current SiteName
  subset_data <- subset(nep_by_day, SiteName == site)
  
  # Extract year from the Date column
  library(lubridate)
  subset_data$year <- year(subset_data$date)
  
  # Calculate median for the entire data
  median_NEP <- median(subset_data$Daily_NEP)
  
  # Create the plot for the current SiteName
  plot <- ggplot(subset_data, aes(x = Daily_NEP, fill = Smoke.day)) +
    geom_density(alpha = 0.5, show.legend = F) +
    scale_fill_manual(values = c("lightskyblue", "#fdbb84")) +
   scale_x_continuous(limits = c(-0.5, 1.5)) +
    labs(x = "Daily NEP", y = "Density") +
   # geom_text(x = -Inf, y = Inf, label = paste(site, subset_data$year[1]), hjust = 0, vjust = 1, color = "black", size = 3) +
    geom_vline(xintercept = median_NEP, linetype = "longdash", color="lightskyblue", size=1)+  # Add dotted line at the median
  
    theme_bw()+
    theme(panel.grid = element_blank())  
  # Add the plot to the list
  plot_list_NEP_22[[site]] <- plot
}
# Arrange plots in a grid
plot_grid(plotlist = plot_list_NEP_22, ncol = 2)


####################################################################################################################

#daily R, by smoke day, by lake
R_by_day <- merged_kalman_22_smoke %>%
  group_by(date,  Smoke.day, SiteName) %>%
  summarize(Daily_R = median(R, na.rm = TRUE))

density_plot_r <- ggplot(R_by_day, aes(x = Daily_R,fill = Smoke.day)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#af8dc3","#fdbb84" )) +
    scale_x_continuous(limits=c(-0.5, 2.5))+
      labs(x = "R", y = "Density") +
  ggtitle("Density Plot of Daily R") +
  theme_minimal() 
density_plot_r

ggplot(R_by_day, aes(x = Daily_R,fill = Smoke.day)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#af8dc3","#fdbb84" )) +
  scale_x_continuous(limits=c(-0.5, 2.5))+
    facet_wrap(~SiteName, ncol=2) +
  labs(x = "Daily R", y = "Density") +
  ggtitle("Density Plot of Daily R 2022 by SiteName") +
  theme_bw()+
  theme(panel.grid = element_blank())

#store plots in a list
plot_list_R_22 <- list()

site_order <- c("Emerald", "Topaz", "EMLPOND1", "TOK11", "TOPAZPOND", "TOK30")


# Loop through each SiteName
for (site in site_order) {
  # Filter data for the current SiteName
  subset_data <- subset(R_by_day, SiteName == site)
  
  # Extract year from the Date column
  library(lubridate)
  subset_data$year <- year(subset_data$date)
  
  # Calculate median Daily_GPP for each Smoke.day group
  # Calculate median for the entire data
  median_R <- median(subset_data$Daily_R)
  
  # Create the plot for the current SiteName
  plot <- ggplot(subset_data, aes(x = Daily_R, fill = Smoke.day)) +
    geom_density(alpha = 0.5, show.legend = F) +
    scale_fill_manual(values = c("lightskyblue", "#fdbb84")) +
    scale_x_continuous(limits = c(-0.5, 1.5)) +
    labs(x = "Daily R", y = "Density") +
   # geom_text(x = -Inf, y = Inf, label = paste(site, subset_data$year[1]), hjust = 0, vjust = 1, color = "black", size = 3) +
    geom_vline(xintercept = median_R, linetype = "longdash", color="lightskyblue", size=1)+  # Add dotted line at the median
  
    theme_bw()+
    theme(panel.grid = element_blank())  
  # Add the plot to the list
  plot_list_R_22[[site]] <- plot
}
# Arrange plots in a grid
plot_grid(plotlist = plot_list_R_22, ncol = 2)

##################################################################################################
############################### ARRANGE PLOTS  ###################################################################
# plot_list_GPP_20    plot_list_GPP_21   plot_list_GPP_22
# plot_list_NEP_20   plot_list_NEP_21   plot_list_NEP_22
# plot_list_R_20    plot_list_R_21     plot_list_R_22

#SITE ORDER OF EACH LIST: Emerald, Topaz, EMLPOND1, TOK11, TOPAZPOND, TOK30

plot_grid(plotlist = plot_list_GPP_20, ncol = 2)
plot_grid(plotlist = plot_list_GPP_21, ncol = 2)
plot_grid(plotlist = plot_list_GPP_22, ncol = 2)

######################################################################################################
#GPP
library(ggplot2)
library(patchwork)
library(grid)
# Create a list of lists
plot_lists <- list(plot_list_GPP_20, plot_list_GPP_21, plot_list_GPP_22)

# Create a data frame to represent the combination of plots and years
years <- c(2020, 2021, 2022)
sites <- c("Emerald", "Topaz", "EMLPOND1", "TOK11", "TOPAZPOND", "TOK30")
df <- expand.grid(year = years, site = sites)

# Function to extract ggplots from the combined list
get_plot <- function(year, site) {
  plot <- plot_lists[[match(year, years)]][[site]]
  
  # Define y-axis range for each site
  if (site == "Emerald") {
    plot <- plot +
      scale_y_continuous( limits = c(0, 6.5))
  } else {

  }
  
  if (site == "Topaz") {
    plot <- plot +
      #scale_y_continuous( limits = c(0, 4.5))
    scale_y_continuous( limits = c(0, 4.5))
    
  } else {
    
  }
  
  if (site == "EMLPOND1") {
    plot <- plot +
      #scale_y_continuous( limits = c(0, 2.5))
      scale_y_continuous( limits = c(0, 4.5))
    
  } else {
    
  }
  
  if (site == "TOK11") {
    plot <- plot +
     # scale_y_continuous( limits = c(0, 3.5))
      scale_y_continuous( limits = c(0, 4.5))
    
  } else {
    
  }
  
  if (site == "TOPAZPOND") {
    plot <- plot +
      #scale_y_continuous( limits = c(0, 2.5))
      scale_y_continuous( limits = c(0, 4.5))
    
  } else {
    
  }
  
  if (site == "TOK30") {
    plot <- plot +
      #scale_y_continuous( limits = c(0, 1.5))
      scale_y_continuous( limits = c(0, 4.5))
    
  } else {
    
  }
  
  # Modify x-axis labels, title, limits, and ticks for the specified site
  if (site == "TOK30") {
    plot <- plot +
      theme(axis.title.x = element_text(hjust = 0.5)) +
      scale_x_continuous(labels = scales::number_format(), limits = c(-0.5, 2.5), breaks = seq(-0.5, 2.5, 0.5))
  } else {
    plot <- plot +
      theme(axis.text.x = element_blank(),
            axis.title.x = element_blank())
  }
  
  # Separate y-axis manipulations
  if (year != 2020) {
    plot <- plot +
      theme(axis.title.y = element_blank(),
            axis.text.y = element_blank())
  }
  # Add year to the top of the first plot in each column
  if (year == 2020 && site == "Emerald") {
    plot <- plot +
      facet_grid(.~ "2020")
  }

  
  if (year == 2021 && site == "Emerald") {
    plot <- plot +
      facet_grid(. ~ "2021")
  }
  if (year == 2022 && site == "Emerald") {
    plot <- plot +
      facet_grid(.~ "2022")
  }
  
  #add site name to right side of plot in third column
  if (year == 2022 && site == "Emerald") {
    plot <- plot +
      facet_grid("Emerald" ~ "2022") +  # Specify both facets in a single call
      theme(strip.text.y.right = element_text(angle = -90))  # Customize the appearance of the facet label
  }
  
   if (year == 2022 && site == "Topaz") {
    plot <- plot +
      facet_grid("Topaz" ~ .) +
            theme(strip.text.y.right  = element_text(angle = -90))
   }
  
  if (year == 2022 && site == "EMLPOND1") {
    plot <- plot +
      facet_grid("EMLPOND1" ~ .) +
      theme(strip.text.y.right  = element_text(angle = -90))
  }
  
  if (year == 2022 && site == "TOK11") {
    plot <- plot +
      facet_grid("TOK11" ~ .) +
      theme(strip.text.y.right  = element_text(angle = -90))
  }
  
  if (year == 2022 && site == "TOPAZPOND") {
    plot <- plot +
      facet_grid("TOPAZPOND" ~ .) +
      theme(strip.text.y.right  = element_text(angle = -90))
  }
  
  if (year == 2022 && site == "TOK30") {
    plot <- plot +
      facet_grid("TOK30" ~ .) +
      theme(strip.text.y.right  = element_text(angle = -90))
  }
  
  #remove y axis labels for all but 1 site
  if (site != "EMLPOND1" && year == 2020 ) {
    plot <- plot +
      theme(axis.title.y = element_blank())
  }
  
  # Remove x-axis labels for all but 1 site
  if (site == "TOK30" && year == 2021) {
    plot <- plot +
      scale_x_continuous(labels = scales::number_format(), limits = c(-0.5, 2.5), breaks = seq(-0.5, 2.5, 0.5),
                         name = "Daily GPP")  
  } else {
    plot <- plot +
      theme(axis.title.x = element_blank())
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
combined_plot <- wrap_plots(plots, ncol = length(years))


# Add title and subtitle
combined_plot <- combined_plot +
  plot_layout(guides = "collect") 


# Show the combined plot
print(combined_plot)


# Save the plot
plotsave_file_path <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Output/combined_density_plot_gpp.png"
ggsave(plotsave_file_path, combined_plot, width = 10, height = 10, units = "in",dpi = 300)


##############################################################################################



library(ggplot2)
library(patchwork)

# Create a list of lists
plot_lists <- list(plot_list_GPP_20, plot_list_GPP_21, plot_list_GPP_22)

# Create a data frame to represent the combination of plots and years
years <- c(2020, 2021, 2022)
sites <- c("Emerald", "Topaz", "EMLPOND1", "TOK11", "TOPAZPOND", "TOK30")
df <- expand.grid(year = years, site = sites)

# Function to extract ggplots from the combined list
get_plot <- function(year, site) {
  plot_index <- match(year, years)
  plot <- plot_lists[[plot_index]][[site]]
  
  # Customize plot appearance if needed
  
  return(plot)
}

# Create a list to store individual plots
plots <- list()

# Add individual plots to the list
for (year in years) {
  for (site in sites) {
    plots[[length(plots) + 1]] <- get_plot(year, site)
  }
}

# Use patchwork to arrange the plots
combined_plot <- wrap_plots(plots, ncol = length(sites), byrow = TRUE)

# Add legend to the bottom of the combined plot
combined_plot <- combined_plot +
  plot_layout(guides = "collect")

# Show the combined plot
print(combined_plot)


######################################


#NEP
library(ggplot2)
library(patchwork)

# Create a list of lists
plot_lists <- list(plot_list_NEP_20, plot_list_NEP_21, plot_list_NEP_22)


# Create a data frame to represent the combination of plots and years
years <- c(2020, 2021, 2022)
sites <- c("Emerald", "Topaz", "EMLPOND1", "TOK11", "TOPAZPOND", "TOK30")
df <- expand.grid(year = years, site = sites)

# Function to extract ggplots from the combined list
get_plot <- function(year, site) {
  plot <- plot_lists[[match(year, years)]][[site]]
  
  # Define y-axis range for each site
  if (site == "Emerald") {
    plot <- plot +
      scale_y_continuous( limits = c(0, 15))
  } else {
    
  }
  
  if (site == "Topaz") {
    plot <- plot +
     # scale_y_continuous( limits = c(0, 4.5))
    scale_y_continuous( limits = c(0, 7))
    
  } else {
    
  }
  
  if (site == "EMLPOND1") {
    plot <- plot +
    #  scale_y_continuous( limits = c(0, 4.5))
      scale_y_continuous( limits = c(0, 7))
    
  } else {
    
  }
  
  if (site == "TOK11") {
    plot <- plot +
      scale_y_continuous( limits = c(0, 7))
  } else {
    
  }
  
  if (site == "TOPAZPOND") {
    plot <- plot +
   #   scale_y_continuous( limits = c(0, 3.5))
      scale_y_continuous( limits = c(0, 7))
    
  } else {
    
  }
  
  if (site == "TOK30") {
    plot <- plot +
   #   scale_y_continuous( limits = c(0, 4))
      scale_y_continuous( limits = c(0, 7))
    
  } else {
    
  }
  
  # Modify x-axis labels, title, limits, and ticks for the specified site
  if (site == "TOK30") {
    plot <- plot +
      theme(axis.title.x = element_text(hjust = 0.5)) +
      scale_x_continuous(labels = scales::number_format(), limits = c(-1, 1), breaks = seq(-1, 1,0.5))
  } else {
    plot <- plot +
      theme(axis.text.x = element_blank(),
            axis.title.x = element_blank())
  }
  
  # Separate y-axis manipulations
  if (year != 2020) {
    plot <- plot +
      theme(axis.title.y = element_blank(),
            axis.text.y = element_blank())
  }
  
  # Add year to the top of the first plot in each column
  if (year == 2020 && site == "Emerald") {
    plot <- plot +
      facet_grid(.~ "2020")
  }
  
  
  if (year == 2021 && site == "Emerald") {
    plot <- plot +
      facet_grid(. ~ "2021")
  }
  if (year == 2022 && site == "Emerald") {
    plot <- plot +
      facet_grid(.~ "2022")
  }
  
  #add site name to right side of plot in third column
  if (year == 2022 && site == "Emerald") {
    plot <- plot +
      facet_grid("Emerald" ~ "2022") +  # Specify both facets in a single call
      theme(strip.text.y.right = element_text(angle = -90))  # Customize the appearance of the facet label
  }
  
  if (year == 2022 && site == "Topaz") {
    plot <- plot +
      facet_grid("Topaz" ~ .) +
      theme(strip.text.y.right  = element_text(angle = -90))
  }
  
  if (year == 2022 && site == "EMLPOND1") {
    plot <- plot +
      facet_grid("EMLPOND1" ~ .) +
      theme(strip.text.y.right  = element_text(angle = -90))
  }
  
  if (year == 2022 && site == "TOK11") {
    plot <- plot +
      facet_grid("TOK11" ~ .) +
      theme(strip.text.y.right  = element_text(angle = -90))
  }
  
  if (year == 2022 && site == "TOPAZPOND") {
    plot <- plot +
      facet_grid("TOPAZPOND" ~ .) +
      theme(strip.text.y.right  = element_text(angle = -90))
  }
  
  if (year == 2022 && site == "TOK30") {
    plot <- plot +
      facet_grid("TOK30" ~ .) +
      theme(strip.text.y.right  = element_text(angle = -90))
  }
  
  #remove y axis labels for all but 1 site
  if (site != "EMLPOND1" && year == 2020 ) {
    plot <- plot +
      theme(axis.title.y = element_blank())
  }
  
  # Remove x-axis labels for all but 1 site
  if (site == "TOK30" && year == 2021) {
    plot <- plot +
      scale_x_continuous(labels = scales::number_format(), limits = c(-1, 1), breaks = seq(-1, 1, 0.5),
                         name = "Daily NEP")  
  } else {
    plot <- plot +
      theme(axis.title.x = element_blank())
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
combined_plot <- wrap_plots(plots, ncol = length(years))


# Add title and subtitle
combined_plot <- combined_plot +
  plot_layout(guides = "collect") 


# Show the combined plot
print(combined_plot)


# Save the plot
plotsave_file_path <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Output/combined_density_plot_nep.png"
ggsave(plotsave_file_path, combined_plot, width = 10, height = 10, units = "in",dpi = 300)




######################################
#R
library(ggplot2)
library(patchwork)

# Create a list of lists
plot_lists <- list(plot_list_R_20, plot_list_R_21, plot_list_R_22)


# Create a data frame to represent the combination of plots and years
years <- c(2020, 2021, 2022)
sites <- c("Emerald", "Topaz", "EMLPOND1", "TOK11", "TOPAZPOND", "TOK30")
df <- expand.grid(year = years, site = sites)

# Function to extract ggplots from the combined list
get_plot <- function(year, site) {
  plot <- plot_lists[[match(year, years)]][[site]]
  
  # Define y-axis range for each site
  if (site == "Emerald") {
    plot <- plot +
      scale_y_continuous( limits = c(0, 9))
  } else {
    
  }
  
  if (site == "Topaz") {
    plot <- plot +
      scale_y_continuous( limits = c(0, 4.5))
  } else {
    
  }
  
  if (site == "EMLPOND1") {
    plot <- plot +
      #scale_y_continuous( limits = c(0, 2.5))
      scale_y_continuous( limits = c(0, 4.5))
    
  } else {
    
  }
  
  if (site == "TOK11") {
    plot <- plot +
     # scale_y_continuous( limits = c(0, 3.5))
      scale_y_continuous( limits = c(0, 4.5))
    
  } else {
    
  }
  
  if (site == "TOPAZPOND") {
    plot <- plot +
     # scale_y_continuous( limits = c(0, 2.5))
      scale_y_continuous( limits = c(0, 4.5))
    
  } else {
    
  }
  
  if (site == "TOK30") {
    plot <- plot +
     # scale_y_continuous( limits = c(0, 1.5))
      scale_y_continuous( limits = c(0, 4.5))
    
  } else {
    
  }
  
  # Modify x-axis labels, title, limits, and ticks for the specified site
  if (site == "TOK30") {
    plot <- plot +
      theme(axis.title.x = element_text(hjust = 0.5)) +
      scale_x_continuous(labels = scales::number_format(), limits = c(-0.5, 2.5), breaks = seq(-0.5, 2.5, 0.5))
  } else {
    plot <- plot +
      theme(axis.text.x = element_blank(),
            axis.title.x = element_blank())
  }
  
  # Separate y-axis manipulations
  if (year != 2020) {
    plot <- plot +
      theme(axis.title.y = element_blank(),
            axis.text.y = element_blank())
  }
  # Add year to the top of the first plot in each column
  if (year == 2020 && site == "Emerald") {
    plot <- plot +
      facet_grid(.~ "2020")
  }
  
  
  if (year == 2021 && site == "Emerald") {
    plot <- plot +
      facet_grid(. ~ "2021")
  }
  if (year == 2022 && site == "Emerald") {
    plot <- plot +
      facet_grid(.~ "2022")
  }
  
  #add site name to right side of plot in third column
  if (year == 2022 && site == "Emerald") {
    plot <- plot +
      facet_grid("Emerald" ~ "2022") +  # Specify both facets in a single call
      theme(strip.text.y.right = element_text(angle = -90))  # Customize the appearance of the facet label
  }
  
  if (year == 2022 && site == "Topaz") {
    plot <- plot +
      facet_grid("Topaz" ~ .) +
      theme(strip.text.y.right  = element_text(angle = -90))
  }
  
  if (year == 2022 && site == "EMLPOND1") {
    plot <- plot +
      facet_grid("EMLPOND1" ~ .) +
      theme(strip.text.y.right  = element_text(angle = -90))
  }
  
  if (year == 2022 && site == "TOK11") {
    plot <- plot +
      facet_grid("TOK11" ~ .) +
      theme(strip.text.y.right  = element_text(angle = -90))
  }
  
  if (year == 2022 && site == "TOPAZPOND") {
    plot <- plot +
      facet_grid("TOPAZPOND" ~ .) +
      theme(strip.text.y.right  = element_text(angle = -90))
  }
  
  if (year == 2022 && site == "TOK30") {
    plot <- plot +
      facet_grid("TOK30" ~ .) +
      theme(strip.text.y.right  = element_text(angle = -90))
  }
  
  #remove y axis labels for all but 1 site
  if (site != "EMLPOND1" && year == 2020 ) {
    plot <- plot +
      theme(axis.title.y = element_blank())
  }
  
  # Remove x-axis labels for all but 1 site
  if (site == "TOK30" && year == 2021) {
    plot <- plot +
      scale_x_continuous(labels = scales::number_format(), limits = c(-.5, 2.5), breaks = seq(-.5, 2.5, 0.5),
                         name = "Daily R")  
  } else {
    plot <- plot +
      theme(axis.title.x = element_blank())
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
combined_plot <- wrap_plots(plots, ncol = length(years))


# Add title and subtitle
combined_plot <- combined_plot +
  plot_layout(guides = "collect") 


# Show the combined plot
print(combined_plot)



# Save the plot
plotsave_file_path <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Output/combined_density_plot_r.png"
ggsave(plotsave_file_path, combined_plot, width = 10, height = 10, units = "in",dpi = 300)

















##################################################################################################
nep_by_day$SiteName <- factor(nep_by_day$SiteName, levels = c("Emerald", "TOK11", "Topaz", "TOPAZPOND","EMLPOND1","TOK30"))
gpp_by_day$SiteName <- factor(gpp_by_day$SiteName, levels =  c("Emerald", "TOK11", "Topaz", "TOPAZPOND","EMLPOND1","TOK30"))
R_by_day$SiteName <- factor(R_by_day$SiteName, levels =  c("Emerald", "TOK11", "Topaz", "TOPAZPOND","EMLPOND1","TOK30"))

combined_plot <- ggplot() +
  geom_density(data = nep_by_day, aes(x = Daily_NEP, color = "Daily NEP"), alpha = 0.5) +
  geom_density(data = R_by_day, aes(x = Daily_R, color = "Daily R"), alpha = 0.5) +
  geom_density(data = gpp_by_day, aes(x = Daily_GPP, color = "Daily GPP"), alpha = 0.5) +
  facet_wrap(~SiteName, ncol = 2) +
  labs(x = "Daily rate", y = "Density") +
  ggtitle("Density Plot of Daily NEP, R, and GPP (2022) by SiteName") +
  scale_color_manual(values = c("Daily NEP" = "#F8766D", "Daily R" = "#619CFF", "Daily GPP" = "#00BA38")) +
  theme_minimal()
combined_plot

############################################################################################################

# all years combined 
merged_kalman_21$R = (merged_kalman_21$R) *-1
kalman_all_a=merge(merged_kalman_smoke, merged_kalman_21, all=T)
kalman_all=merge(kalman_all_a, merged_kalman_22_smoke, all=T)
kalman_all_filtered <- kalman_all[month(kalman_all$date) %in% c(8, 9, 10), ]


#daily GPP, by smoke day, by lake
gpp_by_day <- kalman_all_filtered %>%
  group_by(date,  SiteName, year) %>%
  summarize(Daily_GPP = median(GPP, na.rm = TRUE))

gpp_by_day$month=month(gpp_by_day$date)


# Convert year to a factor for plots
gpp_by_day$year <- as.factor(gpp_by_day$year)
gpp_by_day$SiteName <- factor(gpp_by_day$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1", "TOK11", "TOPAZPOND", "TOK30"))

#only keep days where every lake/pond has a GPP estimate
gpp_by_day_filtered <- gpp_by_day %>%
  group_by(date) %>%
  filter(all(!is.na(Daily_GPP))) %>%
  ungroup()

# Density Plot
density_plot_gpp_anova <- ggplot(gpp_by_day_filtered, aes(x = Daily_GPP, fill = year)) +
  geom_density(alpha = 0.5, show.legend = TRUE) +
  labs(x = "GPP", y = "Density") +
  ggtitle("Daily median GPP") +
  theme_bw() +
   theme(panel.grid = element_blank()) +
  facet_wrap(~SiteName, scales = "free_y")


print(density_plot_gpp_anova)

# Boxplot
boxplot_gpp_anova <- ggplot(gpp_by_day_filtered, aes(x = year, y = Daily_GPP, fill = year)) +
  geom_boxplot(alpha = 0.5) +
  labs(x = "Year", y = "Daily GPP") +
  ggtitle("Daily median GPP (includes aug-oct data only)") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  facet_wrap(~SiteName, scales = "free_y")

print(boxplot_gpp_anova)


boxplot_gpp_month <- ggplot(gpp_by_day_filtered, aes(x = factor(month), y = Daily_GPP, fill = year)) +
  geom_boxplot(alpha = 0.5) +
  labs(x = "month", y = "Daily GPP") +
  ggtitle("Daily median GPP (Aug-Oct)") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  facet_wrap(~SiteName, scales = "free_y")
boxplot_gpp_month









#daily R, by smoke day, by lake
R_by_day <- kalman_all_filtered %>%
  group_by(date,  SiteName, year) %>%
  summarize(Daily_R = median(R, na.rm = TRUE))

R_by_day$month=month(R_by_day$date)


# Convert year to a factor for plots
R_by_day$year <- as.factor(R_by_day$year)
R_by_day$SiteName <- factor(R_by_day$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1", "TOK11", "TOPAZPOND", "TOK30"))

#only keep days where every lake/pond has a R estimate
R_by_day_filtered <- R_by_day %>%
  group_by(date) %>%
  filter(all(!is.na(Daily_R))) %>%
  ungroup()

# Density Plot
density_plot_R_anova <- ggplot(R_by_day_filtered, aes(x = Daily_R, fill = year)) +
  geom_density(alpha = 0.5, show.legend = TRUE) +
  labs(x = "R", y = "Density") +
  ggtitle("Daily median R") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  facet_wrap(~SiteName, scales = "free_y")
density_plot_R_anova

# Boxplot
boxplot_R_anova <- ggplot(R_by_day_filtered, aes(x = year, y = Daily_R, fill = year)) +
  geom_boxplot(alpha = 0.5) +
  labs(x = "Year", y = "Daily R") +
  ggtitle("Daily median R") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  facet_wrap(~SiteName, scales = "free_y")
boxplot_R_anova


boxplot_R_month <- ggplot(R_by_day_filtered, aes(x = factor(month), y = Daily_R, fill = year)) +
  geom_boxplot(alpha = 0.5) +
  labs(x = "month", y = "Daily R") +
  ggtitle("Daily median R") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  facet_wrap(~SiteName, scales = "free_y")
boxplot_R_month


















####################################################################################################################
############### scatter plots #############################################################################################################
#####################################################################################################################
####################################################################################################################
########################################################################################################################

# 2020  data: merged_kalman; merged_ts.data
# 2021 data: merged_kalman 21; merged_ts.data_21
# 2022 data: merged_kalman_22; merged_ts.data_22

# Change the order of SiteName
merged_kalman$SiteName <- factor(merged_kalman$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30"))
merged_kalman <- merged_kalman[merged_kalman$flag != "y", ]



RvGPP_20 <- ggplot(merged_kalman, aes(x = R, y = GPP, color=SiteName), na.rm=T) +
  geom_point() +
  labs(x = "R", y = "GPP") +
  ggtitle("R vs GPP 2020")+
  theme_bw()+
  theme(panel.grid = element_blank())
RvGPP_20

#remove TOK30
kalman_no30=merged_kalman %>% filter(SiteName != "TOK30")
# Change the order of SiteName
kalman_no30$SiteName <- factor(kalman_no30$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND"))

RvGPP_20_no30 <- ggplot(kalman_no30, aes(x = R, y = GPP, color=SiteName), na.rm=T) +
  geom_point() +
  labs(x = "R", y = "GPP") +
  ggtitle("R vs GPP 2020, no TOK 30")+
  theme_bw()+
  theme(panel.grid = element_blank())
RvGPP_20_no30



#subset by smoke.day - no smoke
subset_kalman_smoke <- merged_kalman_smoke[merged_kalman_smoke$Smoke.day == "n", ]
subset_kalman_smoke$SiteName <- factor(subset_kalman_smoke$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30"))

nosmokeday_20 <- ggplot(subset_kalman_smoke, aes(x = R, y = GPP, color = SiteName), na.rm=T) +
  geom_point() +
  labs(x = "R", y = "GPP") +
  ggtitle("R vs GPP on no smoke days 2020") +
  theme_bw()+
  theme(panel.grid = element_blank())
nosmokeday_20

#remove TOK30
subset_kalman_smoke_no30=subset_kalman_smoke %>% filter(SiteName != "TOK30")
# Change the order of SiteName
subset_kalman_smoke_no30$SiteName <- factor(subset_kalman_smoke_no30$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND"))

nosmokeday_20_no30 <- ggplot(subset_kalman_smoke_no30, aes(x = R, y = GPP, color = SiteName), na.rm=T) +
  geom_point() +
  labs(x = "R", y = "GPP") +
  ggtitle("R vs GPP on no smoke days 2020, no TOK30") +
  scale_x_continuous(limits=c(-0.5, 2))+
  scale_y_continuous(limits=c(-0.5, 2))+
  theme_bw()+
  theme(panel.grid = element_blank())
nosmokeday_20_no30


#subset by smoke.day - yes smoke
subset_kalman_smoke <- merged_kalman_smoke[merged_kalman_smoke$Smoke.day == "y", ]
subset_kalman_smoke$SiteName <- factor(subset_kalman_smoke$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30"))

smokeday_20 <- ggplot(subset_kalman_smoke, aes(x = R, y = GPP, color = SiteName), na.rm=T) +
  geom_point() +
  labs(x = "R", y = "GPP") +
  ggtitle("R vs GPP on smoke days 2020") +
  theme_bw()+
  theme(panel.grid = element_blank())
smokeday_20

#remove TOK30
subset_kalman_smoke_no30=subset_kalman_smoke %>% filter(SiteName != "TOK30")
# Change the order of SiteName
subset_kalman_smoke_no30$SiteName <- factor(subset_kalman_smoke_no30$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND"))

smokeday_20_no30 <- ggplot(subset_kalman_smoke_no30, aes(x = R, y = GPP, color = SiteName), na.rm=T) +
  geom_point() +
  labs(x = "R", y = "GPP") +
  ggtitle("R vs GPP on smoke days 2020, no TOK30") +
  scale_x_continuous(limits=c(-0.5, 2))+
  scale_y_continuous(limits=c(-0.5, 2))+
  theme_bw()+
  theme(panel.grid = element_blank())
smokeday_20_no30

plot_grid(nosmokeday_20_no30, smokeday_20_no30, ncol=1)

plot_grid(nosmokeday_20, smokeday_20, ncol=1)

########################################################################################################################
#######same for 2021###############################################################################################################################
# Change the order of SiteName

merged_kalman_21$SiteName <- factor(merged_kalman_21$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30"))

RvGPP_21 <- ggplot(merged_kalman_21, aes(x = R, y = GPP, color=SiteName)) +
  geom_point() +
  labs(x = "R", y = "GPP") +
  ggtitle("R vs GPP 2021")+
  theme_bw()+
  theme(panel.grid = element_blank())
RvGPP_21

#remove TOK30
kalman_no30_21=merged_kalman_21 %>% filter(SiteName != "TOK30")
# Change the order of SiteName
kalman_no30_21$SiteName <- factor(kalman_no30_21$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND"))

RvGPP_21_no30 <- ggplot(kalman_no30_21, aes(x = R, y = GPP, color=SiteName)) +
  geom_point() +
  labs(x = "R", y = "GPP") +
  ggtitle("R vs GPP 2021, no tok30")+
  theme_bw()+
  theme(panel.grid = element_blank())
RvGPP_21_no30

#subset by smoke.day - no smoke
subset_kalman_21_smoke <- merged_kalman_21_smoke[merged_kalman_21_smoke$Smoke.day == "n", ]

subset_kalman_21_smoke$SiteName <- factor(subset_kalman_21_smoke$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30"))

nosmoke_21 <- ggplot(subset_kalman_21_smoke, aes(x = R, y = GPP, color = SiteName)) +
  geom_point() +
  labs(x = "R", y = "GPP") +
  ggtitle("R vs GPP on no smoke days 2021") +
  theme_bw()+
  theme(panel.grid = element_blank())
nosmoke_21

#remove TOK30
subset_kalman_21_smoke_no30=subset_kalman_21_smoke %>% filter(SiteName != "TOK30")
# Change the order of SiteName
subset_kalman_21_smoke_no30$SiteName <- factor(subset_kalman_21_smoke_no30$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND"))

nosmokeday_21_no30 <- ggplot(subset_kalman_21_smoke_no30, aes(x = R, y = GPP, color = SiteName)) +
  geom_point() +
  labs(x = "R", y = "GPP") +
  ggtitle("R vs GPP on no smoke days 2021, no TOK 30") +
  scale_x_continuous(limits=c(-0.5, 2))+
  scale_y_continuous(limits=c(-0.5, 2))+
  theme_bw()+
  theme(panel.grid = element_blank())
nosmokeday_21_no30


#subset by smoke.day - yes smoke
subset_kalman_21_smoke <- merged_kalman_smoke[merged_kalman_smoke$Smoke.day == "y", ]
subset_kalman_21_smoke$SiteName <- factor(subset_kalman_21_smoke$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30"))

smokeday_21 <- ggplot(subset_kalman_21_smoke, aes(x = R, y = GPP, color = SiteName)) +
  geom_point() +
  labs(x = "R", y = "GPP") +
  ggtitle("R vs GPP on smoke days 2021") +
   theme_bw()+
  theme(panel.grid = element_blank())
smokeday_21

#remove TOK30
subset_kalman_21_smoke_no30=subset_kalman_21_smoke %>% filter(SiteName != "TOK30")
# Change the order of SiteName
subset_kalman_21_smoke_no30$SiteName <- factor(subset_kalman_21_smoke_no30$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND"))

smokeday_21_no30 <- ggplot(subset_kalman_21_smoke_no30, aes(x = R, y = GPP, color = SiteName)) +
  geom_point() +
  labs(x = "R", y = "GPP") +
  ggtitle("R vs GPP on smoke days 2021, no TOK30") +
  scale_x_continuous(limits=c(-0.5, 2))+
  scale_y_continuous(limits=c(-0.5, 2))+
  theme_bw()+
  theme(panel.grid = element_blank())
smokeday_21_no30

plot_grid(nosmokeday_21_no30, smokeday_21_no30, ncol=1)

plot_grid(nosmokeday_20_no30, nosmokeday_21_no30, smokeday_20_no30,  smokeday_21_no30, ncol=2)



########################################################################################################################
#######same for 2022###############################################################################################################################
# Change the order of SiteName
merged_kalman_22$SiteName <- factor(merged_kalman_22$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND","TOK30"))

RvGPP_22 <- ggplot(merged_kalman_22, aes(x = R, y = GPP, color=SiteName)) +
  geom_point() +
  labs(x = "R", y = "GPP") +
  ggtitle("R vs GPP 2022")+
  theme_bw()+
  theme(panel.grid = element_blank())
RvGPP_22

#remove TOK30
kalman_no30_22=merged_kalman_22 %>% filter(SiteName != "TOK30")
# Change the order of SiteName
kalman_no30_22$SiteName <- factor(kalman_no30_22$SiteName, levels = c("Emerald", "Topaz", "EMLPOND1","TOK11","TOPAZPOND"))

RvGPP_22_no30 <- ggplot(kalman_no30_22, aes(x = R, y = GPP, color=SiteName)) +
  geom_point() +
  labs(x = "R", y = "GPP") +
  ggtitle("R vs GPP 2022, no TOK 30")+
  scale_x_continuous(limits = c(-0.5, 2), breaks = seq(-0.5, 2, 0.5)) +
  theme_bw()+
  theme(panel.grid = element_blank())
RvGPP_22_no30



plot_grid(nosmokeday_20_no30, nosmokeday_21_no30,smokeday_20_no30,  smokeday_21_no30, RvGPP_no30, ncol=2)



plot_grid(nosmokeday_20, nosmoke_21, smokeday_20,  smokeday_21, RvGPP, ncol=2) 

plot_grid(RvGPP_20, RvGPP_21, RvGPP_22, ncol=1) 

plot_grid(RvGPP_20_no30, RvGPP_21_no30, RvGPP_22_no30, ncol=1) 










####################################################################################################
#compare median daily GPP


# Calculate median GPP for each combination of SiteName and Smoke.day
result <- df %>%
  group_by(SiteName, Smoke.day) %>%
  summarise(median_GPP = median(GPP, na.rm = TRUE))

# Pivot the data to have Smoke.day values as columns
result_pivot <- result %>%
  pivot_wider(names_from = Smoke.day, values_from = median_GPP)

# Optionally, you can add a column for the difference between y and n
result_pivot <- result_pivot %>%
  mutate(diff_y_n = y - n)

# Print the result
print(result_pivot)
