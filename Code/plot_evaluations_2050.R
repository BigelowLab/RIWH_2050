# Camille Ross
# May 2020
# Purpose: Plot model evaluations
# GIVE DATAFRAMES MORE MEANINGFUL NAMES

# Load libraries
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)

# Set working directory
DIR <- '~/Desktop/Thesis/Bigelow/right_whale'
setwd(dir = DIR)
#Create reference variable
fp_model <- file.path(DIR, "paper_2050_models_final_run_final")
#Create variable with reference to evals subdirectory
fp_evals <- file.path(fp_model, 'evals')
#Create variable with reference to evals subdirectory
fp_var <- file.path(fp_model, 'variable_importance')

#Color blind friendly pallette. Source: https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/#use-a-colorblind-friendly-palette
cbp <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
         "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Read in initial evaluations
full_df_init <- read_csv(file.path(fp_evals, '1_evals.csv')) %>%
  filter(Eval.metric == "ROC", grepl('RUN', Model.name )) %>%
  mutate(month = 1, model_id = rep(seq(1:3),10), EM = FALSE) %>%
  dplyr::group_by(model_id) %>%
  mutate(min = min(Testing.data), max = max(Testing.data)) 

# Take mean of intitial evaluations
full_df <- aggregate(full_df_init[, 4], list(full_df_init$model_id), mean) %>%
  mutate(month = 1, model_id = seq(1:3), Model.name = c("GAM", "GBM", "ANN"), EM = FALSE,
         min = unique(full_df_init$min), max = unique(full_df_init$max)) %>%
  dplyr::select(Testing.data, month, model_id, Model.name, EM, min, max)

# Read in initial ensemble evaluations
EM <- read_csv(file.path(fp_evals, '1_ensemble_evals.csv')) %>%
  filter(Eval.metric == "TSS", grepl('EMmean', Model.name )) %>%
  mutate(month = 1, model_id = 4, EM = TRUE, min = Testing.data, max = Testing.data) %>%
  dplyr::select(Testing.data, month, model_id, Model.name, EM, min, max)

# Bind to model evaluations
full_df <- rbind(full_df, EM)

# Initialize months
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Loop through months
for (i in 2:12) {
  
  # Read in month evaluations
  df_init <- read_csv(file.path(fp_evals, paste0(i, '_evals.csv'))) %>%
    filter(Eval.metric == "ROC", grepl('RUN', Model.name )) %>%
    mutate(month = i, model_id = rep(seq(1:3),10), EM = FALSE) %>%
    dplyr::group_by(model_id) %>%
    mutate(min = min(Testing.data), max = max(Testing.data)) 
  
  # Take mean
  # Issue with April and December required hard coding
  #   Min and max were incorrect otherwise

  df <- aggregate(df_init[, 4], list(df_init$model_id), mean) %>%
    mutate(month = i, model_id = seq(1:3), Model.name = c("GAM", "GBM", "ANN"), EM = FALSE,
           min = unique(df_init$min), max = unique(df_init$max)) %>%
    dplyr::select(Testing.data, month, model_id, Model.name, EM, min, max)
    
  # Bind dataframe to initial evaluation dataframe
  full_df <- rbind(full_df, df)
  
  # Read in ensemble evaluations for given month
  EM <- read_csv(file.path(fp_evals, paste(i, '_ensemble_evals.csv', sep = ""))) %>%
    filter(Eval.metric == "ROC", grepl('EMmean', Model.name )) %>%
    mutate(month = i, model_id = 4, EM = TRUE, min = Testing.data, max = Testing.data) %>%
    dplyr::select(Testing.data, month, model_id, Model.name, EM, min, max)

  # Bind to full dataframe object
  full_df <- rbind(full_df, EM)

}

# Shorten model names based on model ID
full_df$Model.name[full_df$model_id == 1] <- "BRT"
full_df$Model.name[full_df$model_id == 2] <- "GAM"
full_df$Model.name[full_df$model_id == 3] <- "ANN"
full_df$Model.name[full_df$model_id == 4] <- "Ensemble"

# Plot ensemble AUC
plot(x = full_df$month[full_df$Model.name == "Ensemble"], 
     y = full_df$Testing.data[full_df$Model.name == "Ensemble"], type = "l", col = cbp[3], lty = 2,
     xlim = c(1,12), ylim = c(0.5,1.0), ylab = "ROC", xlab = "Month", axes = FALSE)
# Add x axis
axis(1, at = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = months)
# Add y axis
axis(2, at = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))
# Plot mean ANN AUC
lines(x = full_df$month[full_df$Model.name == "ANN"], 
     y = full_df$Testing.data[full_df$Model.name == "ANN"], type = "l", col = cbp[2])
# Plot ANN range
polygon(c(full_df$month[full_df$Model.name == "ANN"],rev(full_df$month[full_df$Model.name == "ANN"])),
        c(full_df$min[full_df$Model.name == "ANN"],rev(full_df$max[full_df$Model.name == "ANN"])),
        col = alpha(cbp[2], 0.2), border = FALSE)
# Plot mean BRT AUC
lines(x = full_df$month[full_df$Model.name == "BRT"], 
     y = full_df$Testing.data[full_df$Model.name == "BRT"], type = "l", col = cbp[8])
# Plot BRT range
polygon(c(full_df$month[full_df$Model.name == "BRT"],rev(full_df$month[full_df$Model.name == "BRT"])),
        c(full_df$min[full_df$Model.name == "BRT"],rev(full_df$max[full_df$Model.name == "BRT"])),
        col = alpha(cbp[8], 0.2), border = FALSE)
# Plot mean GAM AUC
lines(x = full_df$month[full_df$Model.name == "GAM"], 
     y = full_df$Testing.data[full_df$Model.name == "GAM"], type = "l", col = cbp[6])
# Plot GAM range
polygon(c(full_df$month[full_df$Model.name == "GAM"],rev(full_df$month[full_df$Model.name == "GAM"])),
        c(full_df$min[full_df$Model.name == "GAM"],rev(full_df$max[full_df$Model.name == "GAM"])),
        col = alpha(cbp[6], 0.2), border = FALSE)
# Add legend
legend(x = 6.8, y = 0.45, legend = c("Ensemble", "ANN", "BRT", "GAM"),
       col = cbp[c(3,2,8,6)], lty = c(2, 1, 1, 1), cex = 0.8, box.col = "white")

# ------- Plot variable importance --------
# Read in intial variable importance dataframe
df_var <- read_csv(file.path(fp_var, '1_var_importance.csv')) %>%
  dplyr::select(contains('RUN'))

# Save backup
new_df <- df_var

# dplyr::select BRT models (GBM)
new_df_brt <- df_var %>% dplyr::select(contains("GBM"))
# Reformat
new_df_brt <- new_df_brt %>% gather() %>% mutate(Var = rep(c("sst", "btm_temp", "bat", "cal", "chlor"),10),
                                                 Month = 1)
# Rename columns
names(new_df_brt) <- c("Model", "Perc.Cont", "Var", "Month") 
# Compute normalized percent contribution
new_df_brt <- new_df_brt %>% mutate(Model = "BRT", 
                                    Perc.Cont.Temp = rep(rowMeans(df_var %>% dplyr::select(contains("GBM")), na.rm = TRUE), 10)) %>%
                   mutate(Perc.Cont.Norm = Perc.Cont/sum(unique(Perc.Cont.Temp)))                  

# dplyr::select GAMs
new_df_gam <- df_var %>% dplyr::select(contains("GAM"))
# Reformat
new_df_gam <- new_df_gam %>% gather() %>% mutate(Var = rep(c("sst", "btm_temp", "bat", "cal", "chlor"),10),
                                                 Month = 1)
# Rename columns
names(new_df_gam) <- c("Model", "Perc.Cont", "Var", "Month") 
# Compute normalized percent contribution
new_df_gam <- new_df_gam %>% mutate(Model = "GAM", 
                                    Perc.Cont.Temp = rep(rowMeans(df_var %>% dplyr::select(contains("GAM")), na.rm = TRUE), 10)) %>%
  mutate(Perc.Cont.Norm = Perc.Cont/sum(unique(Perc.Cont.Temp))) 

# dplyr::select ANNs
new_df_ann <- df_var %>% dplyr::select(contains("ANN"))
# Reformat
new_df_ann <- new_df_ann %>% gather() %>% mutate(Var = rep(c("sst", "btm_temp", "bat", "cal", "chlor"),10),
                                                 Month = 1)
# Rename columns
names(new_df_ann) <- c("Model", "Perc.Cont", "Var", "Month") 
# Compute normalized percent contribution
new_df_ann <- new_df_ann %>% mutate(Model = "ANN", 
                                    Perc.Cont.Temp = rep(rowMeans(df_var %>% dplyr::select(contains("ANN")), na.rm = TRUE), 10)) %>%
  mutate(Perc.Cont.Norm = Perc.Cont/sum(unique(Perc.Cont.Temp))) 

# Bind individual model dataframes together
new_df <- rbind(new_df_brt, new_df_gam, new_df_ann)

# Loop through months
for (i in 2:12) {
  # Load variable importance for month
  df_var <- read_csv(file.path(fp_var, paste0(i,'_var_importance.csv'))) %>%
    dplyr::select(contains('RUN'))
  
  # Select BRTs (GBMs)
  new_df_brt <- df_var %>% dplyr::select(contains("GBM"))
  # Reformat
  new_df_brt <- new_df_brt %>% gather() %>% mutate(Var = rep(c("sst", "btm_temp", "bat", "cal", "chlor"),10),
                                                   Month = i)
  # Rename columns
  names(new_df_brt) <- c("Model", "Perc.Cont", "Var", "Month") 
  # Compute normalized percent contribution
  new_df_brt <- new_df_brt %>% mutate(Model = "BRT", 
                                      Perc.Cont.Temp = rep(rowMeans(df_var %>% dplyr::select(contains("GBM")), na.rm = TRUE), 10)) %>%
    mutate(Perc.Cont.Norm = Perc.Cont/sum(unique(Perc.Cont.Temp)))
  
  # Select GAMs
  new_df_gam <- df_var %>% dplyr::select(contains("GAM"))
  # Reformat
  new_df_gam <- new_df_gam %>% gather() %>% mutate(Var = rep(c("sst", "btm_temp", "bat", "cal", "chlor"),10),
                                                   Month = i)
  # Rename columns
  names(new_df_gam) <- c("Model", "Perc.Cont", "Var", "Month") 
  # Compute normalized percent contribution
  new_df_gam <- new_df_gam %>% mutate(Model = "GAM", 
                                      Perc.Cont.Temp = rep(rowMeans(df_var %>% dplyr::select(contains("GAM")), na.rm = TRUE), 10)) %>%
    mutate(Perc.Cont.Norm = Perc.Cont/sum(unique(Perc.Cont.Temp)))
  
  # Select ANNs
  new_df_ann <- df_var %>% dplyr::select(contains("ANN"))
  # Reformat
  new_df_ann <- new_df_ann %>% gather() %>% mutate(Var = rep(c("sst", "btm_temp", "bat", "cal", "chlor"),10),
                                                   Month = i)
  # Rename columns
  names(new_df_ann) <- c("Model", "Perc.Cont", "Var", "Month") 
  # Compute normalized percent contribution
  new_df_ann <- new_df_ann %>% mutate(Model = "ANN", 
                                      Perc.Cont.Temp = rep(rowMeans(df_var %>% dplyr::select(contains("ANN")), na.rm = TRUE), 10)) %>%
    mutate(Perc.Cont.Norm = Perc.Cont/sum(unique(Perc.Cont.Temp)))  
  
  # Bind dataframes togther
  new_df_temp <- rbind(new_df_brt, new_df_gam, new_df_ann)
  
  # Bind to initial variable importance dataframe
  new_df <- rbind(new_df, new_df_temp)
}

# Plot BRT variable contributions
ggplot(data = (new_df %>% filter(Model == "BRT")), aes(x = Month, fill = factor(Var, c("sst","btm_temp","bat", "cal", "chlor")))) +
  geom_col(aes(y = Perc.Cont.Norm)) +
  scale_x_continuous(breaks = seq(1,12,2), labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov")) + 
  scale_fill_manual(labels = c("Surface Temp", "Bottom Temp", "Bathymetry", "Calanus", "Chlorophyll"), values = cbp[c(3,5,6,8,4)]) +
  labs(y = "Contribution", fill = "Covariate") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Plot GAM variable contributions
ggplot(data = (new_df %>% filter(Model == "GAM")), aes(x = Month, fill = factor(Var, c("sst","btm_temp","bat", "cal", "chlor")))) +
  geom_col(aes(y = Perc.Cont.Norm)) +
  scale_x_continuous(breaks = seq(1,12,2), labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov")) + 
  scale_fill_manual(labels = c("Surface Temp", "Bottom Temp", "Bathymetry", "Calanus", "Chlorophyll"), values = cbp[c(3,5,6,8,4)]) +
  labs(y = "Contribution", fill = "Covariate") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Plot ANN variable contributions
ggplot(data = (new_df %>% filter(Model == "ANN")), aes(x = Month, fill = factor(Var, c("sst","btm_temp","bat", "cal", "chlor")))) +
  geom_col(aes(y = Perc.Cont.Norm)) +
  scale_x_continuous(breaks = seq(1,12,2), labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov")) + 
  scale_fill_manual(labels = c("Surface Temp", "Bottom Temp", "Bathymetry", "Calanus", "Chlorophyll"), values = cbp[c(3,5,6,8,4)]) +
  labs(x = "Month", y = "Contribution", fill = "Covariate") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# ---- Average variable contriubtion across models ----

# Compute normalized percent contribution
avg_var <- data.frame("Perc.Cont" = rowMeans(df_var, na.rm = TRUE),
                     "Month" = 1,
                     "Var" = c("sst", "btm_temp", "bat", "cal", "chlor")) %>% 
  mutate(Perc.Cont.Norm = Perc.Cont/sum(Perc.Cont))

# Loop through months
for (i in 2:12) {
  # Read in variable contribution for given month
  df <- read_csv(file.path(fp_var, paste0(i,'_var_importance.csv'))) %>%
    dplyr::select(contains('RUN'))
  
  # Compute normalized percent contribution
  temp <- data.frame("Perc.Cont" = rowMeans(df, na.rm = TRUE), 
                     "Month" = i,
                     "Var" = c("sst", "btm_temp", "bat", "cal", "chlor")) %>%
    mutate(Perc.Cont.Norm = Perc.Cont/sum(Perc.Cont))
  
  # Bind to original dataframe
  avg_var <- rbind(avg_var, temp)
}

# Plot average variable contribution
ggplot(data = avg_var, aes(x = Month, fill = factor(Var, c("sst","btm_temp","bat", "cal", "chlor")))) +
  geom_col(aes(y = Perc.Cont.Norm)) +
  scale_x_continuous(breaks = seq(1,12,2), labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov")) + 
  scale_fill_manual(labels = c("Surface Temp", "Bottom Temp", "Bathymetry", "Calanus", "Chlorophyll"), values = cbp[c(3,5,6,8,4)]) +
  labs(x = "Month", y = "Contribution", fill = "Covariate", title = "Average") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Plot scatter plot of variable contribution vs. AUC for BRTs
par(mfrow = c(2,3), cex.lab = 1.3)
plot(x = (new_df %>% filter(Var == "sst", Model == "BRT"))$Perc.Cont.Norm*100, 
     y = rep(full_df$Testing.data[full_df$Model.name == "BRT"],10),
     ylab = "AUC", xlab = "SST contribution (%)", main = "BRT", xlim = c(0,100), ylim = c(0.4,1), pch = 19,
     col = "darkgrey")
plot(x = (new_df %>% filter(Var == "btm_temp", Model == "BRT"))$Perc.Cont.Norm*100, 
     y = rep(full_df$Testing.data[full_df$Model.name == "BRT"],10),
     ylab = "AUC", xlab = "Bottom temp contribution (%)", xlim = c(0,100), ylim = c(0.4,1), pch = 19,
     col = "darkgrey")
plot(x = (new_df %>% filter(Var == "bat", Model == "BRT"))$Perc.Cont.Norm*100, 
     y = rep(full_df$Testing.data[full_df$Model.name == "BRT"],10),
     ylab = "AUC", xlab = "Bathymetry contribution (%)", xlim = c(0,100), ylim = c(0.4,1), pch = 19,
     col = "darkgrey")
plot(x = (new_df %>% filter(Var == "cal", Model == "BRT"))$Perc.Cont.Norm*100, 
     y = rep(full_df$Testing.data[full_df$Model.name == "BRT"],10),
     ylab = "AUC", xlab = "Calanus contribution (%)", xlim = c(0,100), ylim = c(0.4,1), pch = 19,
     col = "darkgrey")
plot(x = (new_df %>% filter(Var == "chlor", Model == "BRT"))$Perc.Cont.Norm*100, 
     y = rep(full_df$Testing.data[full_df$Model.name == "BRT"],10),
     ylab = "AUC", xlab = "Chlorophyll contribution (%)", xlim = c(0,100), ylim = c(0.4,1), pch = 19,
     col = "darkgrey")

# Plot scatter plot of variable contribution vs. AUC for GAMs
par(mfrow = c(2,3), cex.lab = 1.3)
plot(x = (new_df %>% filter(Var == "sst", Model == "GAM"))$Perc.Cont.Norm*100, 
     y = rep(full_df$Testing.data[full_df$Model.name == "GAM"], 10),
     ylab = "AUC", xlab = "SST contribution (%)", main = "GAM", xlim = c(0,100), ylim = c(0.4,1), pch = 19,
     col = "darkgrey")
plot(x = (new_df %>% filter(Var == "btm_temp", Model == "GAM"))$Perc.Cont.Norm*100, 
     y = rep(full_df$Testing.data[full_df$Model.name == "GAM"], 10),
     ylab = "AUC", xlab = "Bottom temp contribution (%)", xlim = c(0,100), ylim = c(0.4,1), pch = 19,
     col = "darkgrey")
plot(x = (new_df %>% filter(Var == "bat", Model == "GAM"))$Perc.Cont.Norm*100, 
     y = rep(full_df$Testing.data[full_df$Model.name == "GAM"], 10),
     ylab = "AUC", xlab = "Bathymetry contribution (%)", xlim = c(0,100), ylim = c(0.4,1), pch = 19,
     col = "darkgrey")
plot(x = (new_df %>% filter(Var == "cal", Model == "GAM"))$Perc.Cont.Norm*100, 
     y = rep(full_df$Testing.data[full_df$Model.name == "GAM"], 10),
     ylab = "AUC", xlab = "Calanus contribution (%)", xlim = c(0,100), ylim = c(0.4,1), pch = 19,
     col = "darkgrey")
plot(x = (new_df %>% filter(Var == "chlor", Model == "GAM"))$Perc.Cont.Norm*100, 
     y = rep(full_df$Testing.data[full_df$Model.name == "GAM"], 10),
     ylab = "AUC", xlab = "Chlorophyll contribution (%)", xlim = c(0,100), ylim = c(0.4,1), pch = 19,
     col = "darkgrey")

# Plot scatter plot of variable contribution vs. AUC for ANNs
par(mfrow = c(2,3), cex.lab = 1.3)
plot(x = (new_df %>% filter(Var == "sst", Model == "ANN"))$Perc.Cont.Norm*100, 
     y = rep(full_df$Testing.data[full_df$Model.name == "BRT"],10),
     ylab = "AUC", xlab = "SST contribution (%)", main = "ANN", xlim = c(0,100), ylim = c(0.4,1), pch = 19,
     col = "darkgrey")
plot(x = (new_df %>% filter(Var == "btm_temp", Model == "ANN"))$Perc.Cont.Norm*100, 
     y = rep(full_df$Testing.data[full_df$Model.name == "ANN"],10),
     ylab = "AUC", xlab = "Bottom temp contribution (%)", xlim = c(0,100), ylim = c(0.4,1), pch = 19,
     col = "darkgrey")
plot(x = (new_df %>% filter(Var == "bat", Model == "ANN"))$Perc.Cont.Norm*100, 
     y = rep(full_df$Testing.data[full_df$Model.name == "ANN"],10),
     ylab = "AUC", xlab = "Bathymetry contribution (%)", xlim = c(0,100), ylim = c(0.4,1), pch = 19,
     col = "darkgrey")
plot(x = (new_df %>% filter(Var == "cal", Model == "ANN"))$Perc.Cont.Norm*100, 
     y = rep(full_df$Testing.data[full_df$Model.name == "ANN"],10),
     ylab = "AUC", xlab = "Calanus contribution (%)", xlim = c(0,100), ylim = c(0.4,1), pch = 19,
     col = "darkgrey")
plot(x = (new_df %>% filter(Var == "chlor", Model == "ANN"))$Perc.Cont.Norm*100, 
     y = rep(full_df$Testing.data[full_df$Model.name == "ANN"],10),
     ylab = "AUC", xlab = "Chlorophyll contribution (%)", xlim = c(0,100), ylim = c(0.4,1), pch = 19,
     col = "darkgrey")

# new_df <- new_df %>% aggregate(by = list(new_df$Month, new_df$Var, new_df$Model), mean) %>%
#   dplyr::select(Group.1, Group.2, Group.3, Perc.Cont.Norm) %>%
#   dplyr::rename(Month = Group.1, Var = Group.2, Model = Group.3)

# Plot correlation between variable contribution and mode evaluation for BRTs
print(paste("SST BRT coefficient:",
            cor.test((new_df %>% filter(Var == "sst", Model == "BRT"))$Perc.Cont.Norm, rep(full_df$Testing.data[full_df$Model.name == "BRT"], each = 10))$estimate))
print(paste("SST BRT p value:",
            cor.test((new_df %>% filter(Var == "sst", Model == "BRT"))$Perc.Cont.Norm, rep(full_df$Testing.data[full_df$Model.name == "BRT"], each = 10))$p.value))

print(paste("BTM TEMP BRT coefficient:",
            cor.test((new_df %>% filter(Var == "btm_temp", Model == "BRT"))$Perc.Cont.Norm, rep(full_df$Testing.data[full_df$Model.name == "BRT"], each = 10))$estimate))
print(paste("BTM TEMP BRT p value:",
            cor.test((new_df %>% filter(Var == "btm_temp", Model == "BRT"))$Perc.Cont.Norm, rep(full_df$Testing.data[full_df$Model.name == "BRT"], each = 10))$p.value))

print(paste("BAT BRT coefficient:",
            cor.test((new_df %>% filter(Var == "bat", Model == "BRT"))$Perc.Cont.Norm, rep(full_df$Testing.data[full_df$Model.name == "BRT"], each = 10))$estimate))
print(paste("BAT BRT p value:",
            cor.test((new_df %>% filter(Var == "bat", Model == "BRT"))$Perc.Cont.Norm, rep(full_df$Testing.data[full_df$Model.name == "BRT"], each = 10))$p.value))

print(paste("CAL BRT coefficient:",
            cor.test((new_df %>% filter(Var == "cal", Model == "BRT"))$Perc.Cont.Norm, rep(full_df$Testing.data[full_df$Model.name == "BRT"], each = 10))$estimate))
print(paste("CAL BRT p value:",
            cor.test((new_df %>% filter(Var == "cal", Model == "BRT"))$Perc.Cont.Norm, rep(full_df$Testing.data[full_df$Model.name == "BRT"], each = 10))$p.value))

print(paste("CHLOR BRT coefficient:",
            cor.test((new_df %>% filter(Var == "chlor", Model == "BRT"))$Perc.Cont.Norm, rep(full_df$Testing.data[full_df$Model.name == "BRT"], each = 10))$estimate))
print(paste("CHLOR BRT p value:",
            cor.test((new_df %>% filter(Var == "chlor", Model == "BRT"))$Perc.Cont.Norm, rep(full_df$Testing.data[full_df$Model.name == "BRT"], each = 10))$p.value))

# Plot correlation between variable contribution and mode evaluation for GAMs
print(paste("SST GAM coefficient:",
            cor.test((new_df %>% filter(Var == "sst", Model == "GAM"))$Perc.Cont.Norm, rep(full_df$Testing.data[full_df$Model.name == "GAM"], each = 10))$estimate))
print(paste("SST GAM p value:",
            cor.test((new_df %>% filter(Var == "sst", Model == "GAM"))$Perc.Cont.Norm, rep(full_df$Testing.data[full_df$Model.name == "GAM"], each = 10))$p.value))

print(paste("BTM TEMP GAM coefficient:",
            cor.test((new_df %>% filter(Var == "btm_temp", Model == "GAM"))$Perc.Cont.Norm, rep(full_df$Testing.data[full_df$Model.name == "GAM"], each = 10))$estimate))
print(paste("BTM TEMP GAM p value:",
            cor.test((new_df %>% filter(Var == "btm_temp", Model == "GAM"))$Perc.Cont.Norm, rep(full_df$Testing.data[full_df$Model.name == "GAM"], each = 10))$p.value))

print(paste("BAT GAM coefficient:",
            cor.test((new_df %>% filter(Var == "bat", Model == "GAM"))$Perc.Cont.Norm, rep(full_df$Testing.data[full_df$Model.name == "GAM"], each = 10))$estimate))
print(paste("BAT GAM p value:",
            cor.test((new_df %>% filter(Var == "bat", Model == "GAM"))$Perc.Cont.Norm, rep(full_df$Testing.data[full_df$Model.name == "GAM"], each = 10))$p.value))

print(paste("CAL GAM coefficient:",
            cor.test((new_df %>% filter(Var == "cal", Model == "GAM"))$Perc.Cont.Norm, rep(full_df$Testing.data[full_df$Model.name == "GAM"], each = 10))$estimate))
print(paste("CAL GAM p value:",
            cor.test((new_df %>% filter(Var == "cal", Model == "GAM"))$Perc.Cont.Norm, rep(full_df$Testing.data[full_df$Model.name == "GAM"], each = 10))$p.value))

print(paste("CHLOR GAM coefficient:",
            cor.test((new_df %>% filter(Var == "chlor", Model == "GAM"))$Perc.Cont.Norm, rep(full_df$Testing.data[full_df$Model.name == "GAM"], each = 10))$estimate))
print(paste("CHLOR GAM p value:",
            cor.test((new_df %>% filter(Var == "chlor", Model == "GAM"))$Perc.Cont.Norm, rep(full_df$Testing.data[full_df$Model.name == "GAM"], each = 10))$p.value))

# Plot correlation between variable contribution and mode evaluation for ANNs
print(paste("SST ANN coefficient:",
            cor.test((new_df %>% filter(Var == "sst", Model == "ANN"))$Perc.Cont.Norm, rep(full_df$Testing.data[full_df$Model.name == "ANN"], each = 10))$estimate))
print(paste("SST ANN p value:",
            cor.test((new_df %>% filter(Var == "sst", Model == "ANN"))$Perc.Cont.Norm, rep(full_df$Testing.data[full_df$Model.name == "ANN"], each = 10))$p.value))

print(paste("BTM TEMP ANN coefficient:",
            cor.test((new_df %>% filter(Var == "btm_temp", Model == "ANN"))$Perc.Cont.Norm, rep(full_df$Testing.data[full_df$Model.name == "ANN"], each = 10))$estimate))
print(paste("BTM TEMP ANN p value:",
            cor.test((new_df %>% filter(Var == "btm_temp", Model == "ANN"))$Perc.Cont.Norm, rep(full_df$Testing.data[full_df$Model.name == "ANN"], each = 10))$p.value))

print(paste("BAT ANN coefficient:",
            cor.test((new_df %>% filter(Var == "bat", Model == "ANN"))$Perc.Cont.Norm, rep(full_df$Testing.data[full_df$Model.name == "ANN"], each = 10))$estimate))
print(paste("BAT ANN p value:",
            cor.test((new_df %>% filter(Var == "bat", Model == "ANN"))$Perc.Cont.Norm, rep(full_df$Testing.data[full_df$Model.name == "ANN"], each = 10))$p.value))

print(paste("CAL ANN coefficient:",
            cor.test((new_df %>% filter(Var == "cal", Model == "ANN"))$Perc.Cont.Norm, rep(full_df$Testing.data[full_df$Model.name == "ANN"], each = 10))$estimate))
print(paste("CAL ANN p value:",
            cor.test((new_df %>% filter(Var == "cal", Model == "ANN"))$Perc.Cont.Norm, rep(full_df$Testing.data[full_df$Model.name == "ANN"], each = 10))$p.value))

print(paste("CHLOR ANN coefficient:",
            cor.test((new_df %>% filter(Var == "chlor", Model == "ANN"))$Perc.Cont.Norm, rep(full_df$Testing.data[full_df$Model.name == "ANN"], each = 10))$estimate))
print(paste("CHLOR ANN p value:",
            cor.test((new_df %>% filter(Var == "chlor", Model == "ANN"))$Perc.Cont.Norm, rep(full_df$Testing.data[full_df$Model.name == "ANN"], each = 10))$p.value))







