############################################################################################################################################################################################
##Set working directory
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data/timeseries"
setwd(wd)

CI=read.csv("paramCI_df_040224.csv", header=T)

library(ggplot2)
library(cowplot)
library(tidyverse)


# Subset the data for Model = mod6.2de_sep and Year = 2020
subset_data_a <- CI[CI$Model == "mod6.2de_sep" & CI$year == 2020 & grepl("^C", CI$Parameter),]

# Reorder the Parameter column so sites are displayed largest to smallest
subset_data_a$Parameter <- factor(subset_data_a$Parameter,
                                  levels = rev(c("C.sw.EmeraldLake", "C.sw.TopazLake",
                                             "C.sw.EMLPond1", "C.sw.TOK11",
                                             "C.sw.TopazPond", "C.sw.TOK30")))
# Create the plot
plot_mod6.2de_sep_2020 <- ggplot(subset_data_a, aes(x = ML.Estimate, y = Parameter)) +
  geom_point(color="blue") +
  geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI), color="blue", height = 0) +
  labs(
    x = "ML.Estimate",
    y = "Parameter",
    title = "Model: mod6.2de_sep, Year: 2020"
  ) +
  theme_bw() +
  xlim(-1.0, 1.0) +
  theme(panel.grid = element_blank())
plot_mod6.2de_sep_2020


#####
# Subset the data for Model = mod6.2de_sep and Year = 2021
subset_data_b <- CI[CI$Model == "mod6.2de_sep" & CI$year == 2021 & grepl("^C", CI$Parameter),]

# Reorder the Parameter column so sites are displayed largest to smallest
subset_data_b$Parameter <- factor(subset_data_b$Parameter,
                                  levels = rev(c("C.sw.EmeraldLake", "C.sw.TopazLake",
                                                 "C.sw.EMLPond1", "C.sw.TOK11",
                                                 "C.sw.TopazPond", "C.sw.TOK30")))
# Create the plot
plot_mod6.2de_sep_2021 <- ggplot(subset_data_b, aes(x = ML.Estimate, y = Parameter)) +
  geom_point(color="blue") +
  geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI), color="blue", height = 0) +
  labs(
    x = "ML.Estimate",
    y = "Parameter",
    title = "Model: mod6.2de_sep, Year: 2021"
  ) +
  theme_bw() +
  xlim(-1.0, 1.0) +
  theme(panel.grid = element_blank())
plot_mod6.2de_sep_2021



plot_grid(plot_mod6.2de_sep_2020, plot_mod6.2de_sep_2021, ncol=1)




#####
# Subset the data for Model = mod6.2de_sh and mod6.3de_sh Year = 2020
# Subset the data for mod6.2de_sh
subset_data_c1 <- CI[CI$Model == "mod6.2de_sh" & CI$year == 2020 & grepl("^C", CI$Parameter),]

# Subset the data for mod6.3de_sh
subset_data_c2 <- CI[CI$Model == "mod6.3de_sh" & CI$year == 2020 & grepl("^C", CI$Parameter),]

# Create the combined plot
plot_combined <- ggplot() +
  geom_point(data = subset_data_c1, aes(x = ML.Estimate, y = Parameter, color = "mod6.2de_sh"), size = 2) +
  geom_errorbarh(data = subset_data_c1, aes(xmin = Lower_CI, xmax = Upper_CI, y = Parameter, color = "mod6.2de_sh"), height = 0) +
  geom_point(data = subset_data_c2, aes(x = ML.Estimate, y = Parameter, color = "mod6.3de_sh"),  size = 2) +
  geom_errorbarh(data = subset_data_c2, aes(xmin = Lower_CI, xmax = Upper_CI, y = Parameter, color = "mod6.3de_sh"), height = 0) +
  scale_color_manual(name = "Model",
                     values = c("mod6.2de_sh" = "blue", "mod6.3de_sh" = "red"),
                     labels = c("Shortwave, shared", "Smoke, shared")) +
  labs(
    x = "ML.Estimate",
    y = "Parameter",
    title = "mod6.2de_sh, mod6.3de_sh (Year: 2020)"
  )  +
  theme_bw() +    xlim(-1.0, 1.0)+

  theme(panel.grid = element_blank())
plot_combined




# Subset the data for Model = mod6.2de_sh and mod6.3de_sh and mod6.4de_sh Year = 2021
# Subset the data for mod6.2de_sh
subset_data_d1 <- CI[CI$Model == "mod6.2de_sh" & CI$year == 2021 & grepl("^C", CI$Parameter),]

# Subset the data for mod6.3de_sh
subset_data_d2 <- CI[CI$Model == "mod6.3de_sh" & CI$year == 2021 & grepl("^C", CI$Parameter),]


subset_data_d3 <- CI[CI$Model == "mod6.4de_sh" & CI$year == 2021 & grepl("^C", CI$Parameter),]

# Create the combined plot
plot_combined_21 <- ggplot() +
  geom_point(data = subset_data_d1, aes(x = ML.Estimate, y = Parameter, color = "mod6.2de_sh"), size = 2) +
  geom_errorbarh(data = subset_data_d1, aes(xmin = Lower_CI, xmax = Upper_CI, y = Parameter, color = "mod6.2de_sh"), height = 0) +
  geom_point(data = subset_data_d2, aes(x = ML.Estimate, y = Parameter, color = "mod6.3de_sh"),  size = 2) +
  geom_errorbarh(data = subset_data_d2, aes(xmin = Lower_CI, xmax = Upper_CI, y = Parameter, color = "mod6.3de_sh"), height = 0) +
  geom_point(data = subset_data_d3, aes(x = ML.Estimate, y = Parameter, color = "mod6.4de_sh"),  size = 2) +
  geom_errorbarh(data = subset_data_d3, aes(xmin = Lower_CI, xmax = Upper_CI, y = Parameter, color = "mod6.4de_sh"), height = 0) +
  scale_color_manual(name = "Model",
                     values = c("mod6.2de_sh" = "blue", "mod6.3de_sh" = "red", "mod6.4de_sh"= "purple"),
                     labels = c("Shortwave, shared", "Smoke, shared","PM2.5, shared")) +
  labs(
    x = "ML.Estimate",
    y = "Parameter",
    title = "mod6.2de_sh, mod6.3de_sh, mod6.4de_sh(Year: 2021)"
  )  +  xlim(-1.0, 1.0)+

  theme_bw() +  
  theme(panel.grid = element_blank())
plot_combined_21




# Subset the data for Model = mod6.2de_sh  Year = 2022
subset_data_e <- CI[CI$Model == "mod6.2de_sh" & CI$year == 2022 & grepl("^C", CI$Parameter),]

subset_data_e$Parameter <- gsub("C.sw", "     C.sw", subset_data_e$Parameter)


# Create the combined plot
plot_mod6.2de_sh_22 <- ggplot() +
  geom_point(data = subset_data_e, aes(x = ML.Estimate, y = Parameter, color = "mod6.2de_sh"), size = 2) +
  geom_errorbarh(data = subset_data_e, aes(xmin = Lower_CI, xmax = Upper_CI, y = Parameter, color = "mod6.2de_sh"), height = 0) +
  scale_color_manual(name = "Model",
                     values = c("mod6.2de_sh" = "blue"),
                     labels = c("Shortwave, shared")) +
  labs(
    x = "ML.Estimate",
    y = "Parameter",
    title = "mod6.2de_sh (Year: 2022)"
  )  +
  xlim(-1.0, 1.0)+
  theme_bw() +  
  theme(panel.grid = element_blank())
plot_mod6.2de_sh_22



library(cowplot)
plot_grid(plot_combined, plot_combined_21, plot_mod6.2de_sh_22, ncol=1, rel_heights = c(1, 1, 1), rel_widths = c(1, 1, 1))



# Subset the data for all models/years
subset_data <- CI %>%
  filter(Model %in% c("mod6.2de_sep", "mod6.2de_sh", "mod6.3de_sh", "mod6.4de_sh"),
         grepl("^C", Parameter)) %>%
  mutate(Parameter = factor(Parameter,
                            levels = rev(c("C.sw.EmeraldLake", "C.sw.TopazLake",
                                           "C.sw.EMLPond1", "C.sw.TOK11",
                                           "C.sw.TopazPond", "C.sw.TOK30", "C.sw"))))
subset_data <- subset_data[!is.na(subset_data$Parameter), , drop = FALSE]

# Create the plot
plot_combined <- ggplot(subset_data, aes(x = ML.Estimate, y = Parameter, color = factor(year))) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI), height = 0) +
  scale_color_manual(name = "Year", values = c("blue", "red", "purple", "green"), labels = c(2020, 2021, 2021, 2022)) +
  labs(
    x = "ML.Estimate",
    y = "Parameter",
    title = "Model: mod6.2de_sep, mod6.2de_sh, mod6.3de_sh, mod6.4de_sh"
  ) +
  theme_bw() +
  xlim(-1.0, 1.0) +
  theme(panel.grid = element_blank())

plot_combined









# Subset the data for sw and smoke den
subset_data_f <- CI %>%
  filter(Model %in% c("mod6.2de_sh", "mod6.3de_sh"),
         grepl("^C", Parameter)) %>%
  mutate(Parameter = factor(Parameter,
                            levels = rev(c("C.sw","C.smoke"))))
subset_data_f <- subset_data_f[!is.na(subset_data_f$Parameter), , drop = FALSE]

# Create the plot
plot_combined <- ggplot(subset_data_f, aes(x = ML.Estimate, y = Parameter, color = factor(year))) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI), height = 0) +
  scale_color_manual(name = "Year", values = c("blue", "red", "purple"), labels = c(2020, 2021, 2022)) +
  labs(
    x = "ML.Estimate",
    y = "Parameter",
    title = "Model:  mod6.2de_sh, mod6.3de_sh"
  ) +
  theme_bw() +
  xlim(-1.0, 1.0) +
  theme(panel.grid = element_blank())

plot_combined

plot_combined <- ggplot(subset_data_f, aes(x = Parameter, y = ML.Estimate, color = factor(year))) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = ML.Estimate - Lower_CI, ymax = ML.Estimate + Upper_CI), width = 0.1) +
  scale_color_manual(name = "Year", values = c("blue", "red", "purple"), labels = c(2020, 2021, 2022)) +
  labs(
    x = "Parameter",
    y = "ML.Estimate",
    title = "Model:  mod6.2de_sh, mod6.3de_sh"
  ) +
  theme_bw() +
  ylim(-1.0, 1.0) +
  theme(panel.grid = element_blank())

plot_combined


