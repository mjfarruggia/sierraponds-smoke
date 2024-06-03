
############################################################################################################################################################################################
##Set working directory
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data/timeseries"
setwd(wd)

#######################################################################################################################################################################################################
##Load time series data, formatted into matrices for use in MARSS model
#load('Tokopah_2020_MARSS_output_03122024.Rdata')
 #load('Tokopah_2021_MARSS_output_02272024.Rdata')
load('Tokopah_2022_MARSS_output_03262024.Rdata')

names(mod.output)


# Create empty vectors to store results
model_names <- character(length(mod.output))
AIC_values <- numeric(length(mod.output))
AICc_values <- numeric(length(mod.output))
estimates <- list(length = length(mod.output))

# Iterate through each model
for (i in seq_along(mod.output)) {
  # Extract model name
  model_names[i] <- names(mod.output)[i]
  
  # Extract AIC and AICc
  AIC_values[i] <- mod.output[[i]]$AIC
  AICc_values[i] <- mod.output[[i]]$AICc
  
  # Extract parameter estimates
  estimates[[i]] <- mod.output[[i]]$par
  
  # Print parameter estimates for each model
  cat("Model:", model_names[i], "\n")
  cat("Estimates:\n")
  print(estimates[[i]])
  cat("\n")
}

# Create a data frame to store the results
results <- data.frame(
  Model = model_names,
  AIC = AIC_values,
  AICc = AICc_values
)


##write.csv(results, "modelAIC_2020.csv", row.names = FALSE)
#write.csv(results, "modelAIC_2021.csv", row.names = FALSE)
write.csv(results, "modelAIC_2022.csv", row.names = FALSE)

#plot AIC
library(ggplot2)

# Create a data frame with model names and AIC values
aic_data <- data.frame(
  Model = model_names,
  AIC = AIC_values
)

# Sort the data by AIC values for better visualization
aic_data <- aic_data[order(aic_data$AIC), ]

# Create the bar plot
aic_plot <- ggplot(aic_data, aes(x = Model, y = AIC)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "AIC Comparison",
       x = "Model",
       y = "AIC") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

aic_plot




