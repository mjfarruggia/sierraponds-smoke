############################################################################################################################################################################################
##Set working directory
wd <- "C:/Users/maryj/Documents/R/SierraPonds/PondMetabolism/Data/timeseries"
setwd(wd)

#######################################################################################################################################################################################################
library(MARSS)

##Load time series data, formatted into matrices for use in MARSS model
#load('Tokopah_2020_MARSS_output_03122024.Rdata')
load('Tokopah_2021_MARSS_output_02272024.Rdata')
#load('Tokopah_2022_MARSS_output_03262024.Rdata')

 names(mod.output)
 
 mod.to.analyze = mod.output[16]
 
 marss_mle_obj = mod.to.analyze$mod

# output with error estimates around model parameters
 paramCI <- MARSSparamCIs(marss_mle_obj, method = 'parametric')
 
 print(paramCI)


  # Extract information from paramCI 
 param_names <- names(paramCI$coef)
 ML_estimate <- unname(paramCI$coef)
 order <- c("R", "Q", "U")
 
 par.se.unlisted <- unlist(paramCI$par.se)
 par.se_reordered <- par.se.unlisted[order]
 Std_Err <- unname(par.se_reordered)
 
 par.lowCI.unlisted <- unlist(paramCI$par.lowCI)
 par.lowCI_reordered <- par.lowCI.unlisted[order]
 Lower_CI <- unname(par.lowCI_reordered)
 
 par.upCI.unlisted <- unlist(paramCI$par.upCI)
 par.upCI_reordered <- par.upCI.unlisted[order]
 Upper_CI <- unname(par.upCI_reordered)
 
 par.bias.unlisted <- unlist(paramCI$par.bias)
 par.bias_reordered <- par.bias.unlisted[order]
 Est_Bias <- unname(par.bias_reordered)
 
 Unbias_Est <- unname(unlist(paramCI$coef - Est_Bias))
 AIC_value <- paramCI$AIC
 AICc_value <- paramCI$AICc
 
 # Store in dataframe
 paramCI_df_16_21 <- data.frame(
   Parameter = param_names,
   ML.Estimate = ML_estimate,
   Std.Err = Std_Err,
   Lower_CI = Lower_CI,
   Upper_CI = Upper_CI,
   Est.Bias = Est_Bias,
   Unbias.Est = Unbias_Est,
   AIC = AIC_value,
   AICc = AICc_value
 )
 
 paramCI_df_16_21
 paramCI_df_16_21$Model= names(mod.output[16])
 paramCI_df_16_21$year=2021
 
 
 
 
 paramCI_df <- paramCI_df_16_21
 
 
 
 
 ##Load time series data, formatted into matrices for use in MARSS model
 #load('Tokopah_2020_MARSS_output_03122024.Rdata')
 load('Tokopah_2021_MARSS_output_02272024.Rdata')
 #load('Tokopah_2022_MARSS_output_03262024.Rdata')
 
 names(mod.output)
 
 mod.to.analyze = mod.output[7]
 
 marss_mle_obj = mod.to.analyze$mod
 
 # output with error estimates around model parameters
 paramCI <- MARSSparamCIs(marss_mle_obj, method = 'parametric')
 
 print(paramCI)
 

 # Extract information from paramCI 
 param_names <- names(paramCI$coef)
 ML_estimate <- unname(paramCI$coef)

order <- c("R", "Q", "U1", "U2", "U3", "U4", "U5", "U6")
    par.se.unlisted <- unlist(paramCI$par.se)
    par.se_reordered <- par.se.unlisted[order]
Std_Err <- unname(par.se_reordered)

    par.lowCI.unlisted <- unlist(paramCI$par.lowCI)
    par.lowCI_reordered <- par.lowCI.unlisted[order]
 Lower_CI <- unname(par.lowCI_reordered)
 
   par.upCI.unlisted <- unlist(paramCI$par.upCI)
   par.upCI_reordered <- par.upCI.unlisted[order]
 Upper_CI <- unname(par.upCI_reordered)
 
   par.bias.unlisted <- unlist(paramCI$par.bias)
   par.bias_reordered <- par.bias.unlisted[order]
 Est_Bias <- unname(par.bias_reordered)
 
 Unbias_Est <- unname(unlist(paramCI$coef - Est_Bias))
 AIC_value <- paramCI$AIC
 AICc_value <- paramCI$AICc
 
 # Store in dataframe
 paramCI_df_7_21 <- data.frame(
   Parameter = param_names,
   ML.Estimate = ML_estimate,
   Std.Err = Std_Err,
   Lower_CI = Lower_CI,
   Upper_CI = Upper_CI,
   Est.Bias = Est_Bias,
   Unbias.Est = Unbias_Est,
   AIC = AIC_value,
   AICc = AICc_value
 )
 
 paramCI_df_7_21
 paramCI_df_7_21$Model= names(mod.output[7])
 paramCI_df_7_21$year=2021
 
 paramCI_df <- merge(paramCI_df, paramCI_df_7_21, all = T)
 
 
 
 
 
 ##Load time series data, formatted into matrices for use in MARSS model
 #load('Tokopah_2020_MARSS_output_03122024.Rdata')
 load('Tokopah_2021_MARSS_output_02272024.Rdata')
 #load('Tokopah_2022_MARSS_output_03262024.Rdata')
 
 names(mod.output)
 
 mod.to.analyze = mod.output[4]
 
 marss_mle_obj = mod.to.analyze$mod
 
 # output with error estimates around model parameters
 paramCI <- MARSSparamCIs(marss_mle_obj, method = 'parametric')
 
 print(paramCI)
 
 
 # Extract information from paramCI 
 param_names <- names(paramCI$coef)
 ML_estimate <- unname(paramCI$coef)
 order <- c("R", "Q", "U")
 
 par.se.unlisted <- unlist(paramCI$par.se)
 par.se_reordered <- par.se.unlisted[order]
 Std_Err <- unname(par.se_reordered)
 
 par.lowCI.unlisted <- unlist(paramCI$par.lowCI)
 par.lowCI_reordered <- par.lowCI.unlisted[order]
 Lower_CI <- unname(par.lowCI_reordered)
 
 par.upCI.unlisted <- unlist(paramCI$par.upCI)
 par.upCI_reordered <- par.upCI.unlisted[order]
 Upper_CI <- unname(par.upCI_reordered)
 
 par.bias.unlisted <- unlist(paramCI$par.bias)
 par.bias_reordered <- par.bias.unlisted[order]
 Est_Bias <- unname(par.bias_reordered)
 
 Unbias_Est <- unname(unlist(paramCI$coef - Est_Bias))
 AIC_value <- paramCI$AIC
 AICc_value <- paramCI$AICc
 
 # Store in dataframe
 paramCI_df_4_21 <- data.frame(
   Parameter = param_names,
   ML.Estimate = ML_estimate,
   Std.Err = Std_Err,
   Lower_CI = Lower_CI,
   Upper_CI = Upper_CI,
   Est.Bias = Est_Bias,
   Unbias.Est = Unbias_Est,
   AIC = AIC_value,
   AICc = AICc_value
 )
 
 paramCI_df_4_21
 paramCI_df_4_21$Model= names(mod.output[4])
 paramCI_df_4_21$year=2021
 
 paramCI_df <- merge(paramCI_df, paramCI_df_4_21, all = T)
 
 
 
 
 ##Load time series data, formatted into matrices for use in MARSS model
 #load('Tokopah_2020_MARSS_output_03122024.Rdata')
 load('Tokopah_2021_MARSS_output_02272024.Rdata')
 #load('Tokopah_2022_MARSS_output_03262024.Rdata')
 
 names(mod.output)
 
 mod.to.analyze = mod.output[10]
 
 marss_mle_obj = mod.to.analyze$mod
 
 # output with error estimates around model parameters
 paramCI <- MARSSparamCIs(marss_mle_obj, method = 'parametric')
 
 print(paramCI)
 
 
 # Extract information from paramCI 
 param_names <- names(paramCI$coef)
 ML_estimate <- unname(paramCI$coef)
 order <- c("R", "Q", "U")
 
 par.se.unlisted <- unlist(paramCI$par.se)
 par.se_reordered <- par.se.unlisted[order]
 Std_Err <- unname(par.se_reordered)
 
 par.lowCI.unlisted <- unlist(paramCI$par.lowCI)
 par.lowCI_reordered <- par.lowCI.unlisted[order]
 Lower_CI <- unname(par.lowCI_reordered)
 
 par.upCI.unlisted <- unlist(paramCI$par.upCI)
 par.upCI_reordered <- par.upCI.unlisted[order]
 Upper_CI <- unname(par.upCI_reordered)
 
 par.bias.unlisted <- unlist(paramCI$par.bias)
 par.bias_reordered <- par.bias.unlisted[order]
 Est_Bias <- unname(par.bias_reordered)
 
 Unbias_Est <- unname(unlist(paramCI$coef - Est_Bias))
 AIC_value <- paramCI$AIC
 AICc_value <- paramCI$AICc
 
 # Store in dataframe
 paramCI_df_10_21 <- data.frame(
   Parameter = param_names,
   ML.Estimate = ML_estimate,
   Std.Err = Std_Err,
   Lower_CI = Lower_CI,
   Upper_CI = Upper_CI,
   Est.Bias = Est_Bias,
   Unbias.Est = Unbias_Est,
   AIC = AIC_value,
   AICc = AICc_value
 )
 
 paramCI_df_10_21
 paramCI_df_10_21$Model= names(mod.output[10])
 paramCI_df_10_21$year=2021
 
 paramCI_df <- merge(paramCI_df, paramCI_df_10_21, all = T)
 
 
 
 
 
 
#2020
 ##Load time series data, formatted into matrices for use in MARSS model
 load('Tokopah_2020_MARSS_output_03122024.Rdata')
 #load('Tokopah_2021_MARSS_output_02272024.Rdata')
 #load('Tokopah_2022_MARSS_output_03262024.Rdata')
 
 names(mod.output)
 
 mod.to.analyze = mod.output[4]
 
 marss_mle_obj = mod.to.analyze$mod
 
 # output with error estimates around model parameters
 paramCI <- MARSSparamCIs(marss_mle_obj, method = 'parametric')
 
 print(paramCI)
 
 
 # Extract information from paramCI 
 param_names <- names(paramCI$coef)
 ML_estimate <- unname(paramCI$coef)
 order <- c("R", "Q", "U")
 
 par.se.unlisted <- unlist(paramCI$par.se)
 par.se_reordered <- par.se.unlisted[order]
 Std_Err <- unname(par.se_reordered)
 
 par.lowCI.unlisted <- unlist(paramCI$par.lowCI)
 par.lowCI_reordered <- par.lowCI.unlisted[order]
 Lower_CI <- unname(par.lowCI_reordered)
 
 par.upCI.unlisted <- unlist(paramCI$par.upCI)
 par.upCI_reordered <- par.upCI.unlisted[order]
 Upper_CI <- unname(par.upCI_reordered)
 
 par.bias.unlisted <- unlist(paramCI$par.bias)
 par.bias_reordered <- par.bias.unlisted[order]
 Est_Bias <- unname(par.bias_reordered)
 
 Unbias_Est <- unname(unlist(paramCI$coef - Est_Bias))
 AIC_value <- paramCI$AIC
 AICc_value <- paramCI$AICc
 
 # Store in dataframe
 paramCI_df_4_20 <- data.frame(
   Parameter = param_names,
   ML.Estimate = ML_estimate,
   Std.Err = Std_Err,
   Lower_CI = Lower_CI,
   Upper_CI = Upper_CI,
   Est.Bias = Est_Bias,
   Unbias.Est = Unbias_Est,
   AIC = AIC_value,
   AICc = AICc_value
 )
 
 paramCI_df_4_20
 paramCI_df_4_20$Model= names(mod.output[4])
 paramCI_df_4_20$year=2020
 
 paramCI_df <- merge(paramCI_df, paramCI_df_4_20, all = T)
 
 ###
 
 names(mod.output)
 
 mod.to.analyze = mod.output[7]
 
 marss_mle_obj = mod.to.analyze$mod
 
 # output with error estimates around model parameters
 paramCI <- MARSSparamCIs(marss_mle_obj, method = 'parametric')
 
 print(paramCI)
 
 
 # Extract information from paramCI 
 param_names <- names(paramCI$coef)
 ML_estimate <- unname(paramCI$coef)
 
 order <- c("R", "Q", "U1", "U2", "U3", "U4", "U5", "U6")
 par.se.unlisted <- unlist(paramCI$par.se)
 par.se_reordered <- par.se.unlisted[order]
 Std_Err <- unname(par.se_reordered)
 
 par.lowCI.unlisted <- unlist(paramCI$par.lowCI)
 par.lowCI_reordered <- par.lowCI.unlisted[order]
 Lower_CI <- unname(par.lowCI_reordered)
 
 par.upCI.unlisted <- unlist(paramCI$par.upCI)
 par.upCI_reordered <- par.upCI.unlisted[order]
 Upper_CI <- unname(par.upCI_reordered)
 
 par.bias.unlisted <- unlist(paramCI$par.bias)
 par.bias_reordered <- par.bias.unlisted[order]
 Est_Bias <- unname(par.bias_reordered)
 
 Unbias_Est <- unname(unlist(paramCI$coef - Est_Bias))
 AIC_value <- paramCI$AIC
 AICc_value <- paramCI$AICc
 
 # Store in dataframe
 paramCI_df_7_20 <- data.frame(
   Parameter = param_names,
   ML.Estimate = ML_estimate,
   Std.Err = Std_Err,
   Lower_CI = Lower_CI,
   Upper_CI = Upper_CI,
   Est.Bias = Est_Bias,
   Unbias.Est = Unbias_Est,
   AIC = AIC_value,
   AICc = AICc_value
 )
 
 paramCI_df_7_20
 paramCI_df_7_20$Model= names(mod.output[7])
 paramCI_df_7_20$year=2020
 
 paramCI_df <- merge(paramCI_df, paramCI_df_7_20, all = T)
 
 
 
 ###
 
 names(mod.output)
 
 mod.to.analyze = mod.output[10]
 
 marss_mle_obj = mod.to.analyze$mod
 
 # output with error estimates around model parameters
 paramCI <- MARSSparamCIs(marss_mle_obj, method = 'parametric')
 
 print(paramCI)
 
 
 # Extract information from paramCI 
 param_names <- names(paramCI$coef)
 ML_estimate <- unname(paramCI$coef)
 order <- c("R", "Q", "U")
 
 par.se.unlisted <- unlist(paramCI$par.se)
 par.se_reordered <- par.se.unlisted[order]
 Std_Err <- unname(par.se_reordered)
 
 par.lowCI.unlisted <- unlist(paramCI$par.lowCI)
 par.lowCI_reordered <- par.lowCI.unlisted[order]
 Lower_CI <- unname(par.lowCI_reordered)
 
 par.upCI.unlisted <- unlist(paramCI$par.upCI)
 par.upCI_reordered <- par.upCI.unlisted[order]
 Upper_CI <- unname(par.upCI_reordered)
 
 par.bias.unlisted <- unlist(paramCI$par.bias)
 par.bias_reordered <- par.bias.unlisted[order]
 Est_Bias <- unname(par.bias_reordered)
 
 Unbias_Est <- unname(unlist(paramCI$coef - Est_Bias))
 AIC_value <- paramCI$AIC
 AICc_value <- paramCI$AICc
 
 # Store in dataframe
 paramCI_df_10_20 <- data.frame(
   Parameter = param_names,
   ML.Estimate = ML_estimate,
   Std.Err = Std_Err,
   Lower_CI = Lower_CI,
   Upper_CI = Upper_CI,
   Est.Bias = Est_Bias,
   Unbias.Est = Unbias_Est,
   AIC = AIC_value,
   AICc = AICc_value
 )
 
 paramCI_df_10_20
 paramCI_df_10_20$Model= names(mod.output[10])
 paramCI_df_10_20$year=2020
 
 paramCI_df <- merge(paramCI_df, paramCI_df_10_20, all = T)
 
 
 
 
 
 
 
 #2022
 ##Load time series data, formatted into matrices for use in MARSS model
 #load('Tokopah_2020_MARSS_output_03122024.Rdata')
 #load('Tokopah_2021_MARSS_output_02272024.Rdata')
 load('Tokopah_2022_MARSS_output_03262024.Rdata')
 
 names(mod.output)
 
 mod.to.analyze = mod.output[4]
 
 marss_mle_obj = mod.to.analyze$mod
 
 # output with error estimates around model parameters
 paramCI <- MARSSparamCIs(marss_mle_obj, method = 'parametric')
 
 print(paramCI)
 
 
 # Extract information from paramCI 
 param_names <- names(paramCI$coef)
 ML_estimate <- unname(paramCI$coef)
 order <- c("R", "Q", "U")
 
 par.se.unlisted <- unlist(paramCI$par.se)
 par.se_reordered <- par.se.unlisted[order]
 Std_Err <- unname(par.se_reordered)
 
 par.lowCI.unlisted <- unlist(paramCI$par.lowCI)
 par.lowCI_reordered <- par.lowCI.unlisted[order]
 Lower_CI <- unname(par.lowCI_reordered)
 
 par.upCI.unlisted <- unlist(paramCI$par.upCI)
 par.upCI_reordered <- par.upCI.unlisted[order]
 Upper_CI <- unname(par.upCI_reordered)
 
 par.bias.unlisted <- unlist(paramCI$par.bias)
 par.bias_reordered <- par.bias.unlisted[order]
 Est_Bias <- unname(par.bias_reordered)
 
 Unbias_Est <- unname(unlist(paramCI$coef - Est_Bias))
 AIC_value <- paramCI$AIC
 AICc_value <- paramCI$AICc
 
 # Store in dataframe
 paramCI_df_4_22 <- data.frame(
   Parameter = param_names,
   ML.Estimate = ML_estimate,
   Std.Err = Std_Err,
   Lower_CI = Lower_CI,
   Upper_CI = Upper_CI,
   Est.Bias = Est_Bias,
   Unbias.Est = Unbias_Est,
   AIC = AIC_value,
   AICc = AICc_value
 )
 
 paramCI_df_4_22
 paramCI_df_4_22$Model= names(mod.output[4])
 paramCI_df_4_22$year=2022
 
 paramCI_df <- merge(paramCI_df, paramCI_df_4_22, all = T)
 
 ###
 
 names(mod.output)
 
 mod.to.analyze = mod.output[5]
 
 marss_mle_obj = mod.to.analyze$mod
 
 # output with error estimates around model parameters
 paramCI <- MARSSparamCIs(marss_mle_obj, method = 'parametric')
 
 print(paramCI)
 
 
 # Extract information from paramCI 
 param_names <- names(paramCI$coef)
 ML_estimate <- unname(paramCI$coef)
 order <- c("R", "Q1", "Q2","U")
 
 par.se.unlisted <- unlist(paramCI$par.se)
 par.se_reordered <- par.se.unlisted[order]
 Std_Err <- unname(par.se_reordered)
 
 par.lowCI.unlisted <- unlist(paramCI$par.lowCI)
 par.lowCI_reordered <- par.lowCI.unlisted[order]
 Lower_CI <- unname(par.lowCI_reordered)
 
 par.upCI.unlisted <- unlist(paramCI$par.upCI)
 par.upCI_reordered <- par.upCI.unlisted[order]
 Upper_CI <- unname(par.upCI_reordered)
 
 par.bias.unlisted <- unlist(paramCI$par.bias)
 par.bias_reordered <- par.bias.unlisted[order]
 Est_Bias <- unname(par.bias_reordered)
 
 Unbias_Est <- unname(unlist(paramCI$coef - Est_Bias))
 AIC_value <- paramCI$AIC
 AICc_value <- paramCI$AICc
 
 # Store in dataframe
 paramCI_df_5_22 <- data.frame(
   Parameter = param_names,
   ML.Estimate = ML_estimate,
   Std.Err = Std_Err,
   Lower_CI = Lower_CI,
   Upper_CI = Upper_CI,
   Est.Bias = Est_Bias,
   Unbias.Est = Unbias_Est,
   AIC = AIC_value,
   AICc = AICc_value
 )
 
 paramCI_df_5_22
 paramCI_df_5_22$Model= names(mod.output[5])
 paramCI_df_5_22$year=2022
 
 paramCI_df <- merge(paramCI_df, paramCI_df_5_22, all = T)
 
 
 
 
 
 
 
 
 
 
 
 write.csv(paramCI_df, "paramCI_df_040224.csv", row.names = FALSE)
 
 