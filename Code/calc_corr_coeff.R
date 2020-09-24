
annual <- brick(proj_covars)

for (month in 2:12){
#Load sea surface temperature
sst <- raster(file.path(fp_R45_sfce_temp_pres, paste('tsfce_pres_R45_', month, '.grd', sep = '')))
#Load bottom temperature
#Resample to same extent as sea surface temperature
# Likely unnecessary, but avoids mismatch errors
sst_btm <- raster(file.path(fp_R45_btm_temp_pres, paste('tbtm_pres_R45_', month, '.grd', sep = ''))) %>%
  raster::resample(sst)
#Load surface salinity
#Resample to same extent as sea surface temperature
sal <- raster(file.path(fp_R45_sfce_sal_pres, paste('ssfce_pres_R45_', month, '.grd', sep = ''))) %>%
  raster::resample(sst)
#Load bottom salinity
#Resample to same extent as sea surface temperature
sal_btm <- raster(file.path(fp_R45_btm_sal_pres, paste('sbtm_pres_R45_', month, '.grd', sep = ''))) %>%
  raster::resample(sst)

#Replace NA values for deep water temperature and salinity with surface values
#Otherwise data near the coasts and around shallow bathymetric features is missing
sst_btm[is.nan(sst_btm)] <- sst[is.nan(sst_btm)]
sal_btm[is.nan(sal_btm)] <- sal[is.nan(sal_btm)]

#Load bathymetry
#Resample to same extent as sea surface temperature
bat <- raster(file.path(fp_bat, 'BathymetryGoMFlipped.grd')) %>%
  raster::resample(sst)

#Load the present calanus climatology
#Ji et al. 2011 life history model
cal <- raster(file.path(fp_cal_pres, paste('calanus_', month, '.grd', sep = ''))) %>%
  raster::resample(sst)
#Zero out values at depths less than 80m
cal[bat >= -80] <- 0
# #Change NA values to 0
cal[is.na(cal)] <- 0

sst[is.na(sst)] <- 0
sst_btm[is.na(sst_btm)] <- 0

#Chlorophyll
chlor <- raster(file.path(fp_chlor, paste('chlor_a_', month, '.grd', sep = ''))) %>%
  raster::resample(sst)

# Zero out below 1000m
sst[bat <= -1000] <- 0
sst_btm[bat <= -1000] <- 0
cal[bat <= -1000] <- 0

# bat[bat <= -1000] <- 0
bat_orig <- bat
#Take the log of the absolute value
bat <- log(abs(bat))
chlor <- log(abs(chlor))

bat[is.na(bat)] <- 0
chlor[is.na(chlor)] <- 0

bat[bat_orig <= -1000] <- 0
chlor[bat_orig <= -1000] <- 0

#Stack covariates
proj_covars <- stack(sst,
                     sst_btm,
                     # sal,
                     # sal_btm,
                     bat,
                     cal,
                     chlor)

#Assign layer names 
names(proj_covars) <- c('sst', 'sst_btm', 'bat', 'cal', 'chlor')

annual <- merge(annual, proj_covars)

print(paste("Month:", month))
#print(layerStats(proj_covars,'pearson')$`pearson correlation coefficient`)
}

annual <- mean(annual)
print(layerStats(annual,'pearson')$`pearson correlation coefficient`)
# 
# for (i in 1:12) {md <- full_data %>% filter(MONTH == i); print(NROW(md[md$pa == 0,]))}
# 
# 

month = 1
#Load sea surface temperature
sst_annual <- raster(file.path(fp_R45_sfce_temp_pres, paste('tsfce_pres_R45_', month, '.grd', sep = '')))
#Load bottom temperature
#Resample to same extent as sea surface temperature
# Likely unnecessary, but avoids mismatch errors
btm_temp_annual <- raster(file.path(fp_R45_btm_temp_pres, paste('tbtm_pres_R45_', month, '.grd', sep = ''))) %>%
  raster::resample(sst)
#Load surface salinity
#Resample to same extent as sea surface temperature
sal_annual <- raster(file.path(fp_R45_sfce_sal_pres, paste('ssfce_pres_R45_', month, '.grd', sep = ''))) %>%
  raster::resample(sst)
#Load bottom salinity
#Resample to same extent as sea surface temperature
sal_btm_annual <- raster(file.path(fp_R45_btm_sal_pres, paste('sbtm_pres_R45_', month, '.grd', sep = ''))) %>%
  raster::resample(sst)

#Replace NA values for deep water temperature and salinity with surface values
#Otherwise data near the coasts and around shallow bathymetric features is missing
btm_temp_annual[is.nan(btm_temp_annual)] <- sst_annual[is.nan(btm_temp_annual)]
#sal_btm[is.nan(sal_btm)] <- sal[is.nan(sal_btm)]

#Load bathymetry
#Resample to same extent as sea surface temperature
bat <- raster(file.path(fp_bat, 'BathymetryGoMFlipped.grd')) %>%
  raster::resample(sst)

#Load the present calanus climatology
#Ji et al. 2011 life history model
cal_annual <- raster(file.path(fp_cal_pres, paste('calanus_', month, '.grd', sep = ''))) %>%
  raster::resample(sst_annual)
#Zero out values at depths less than 80m
cal_annual[bat >= -80] <- 0
# #Change NA values to 0
cal_annual[is.na(cal_annual)] <- 0

sst_annual[is.na(sst_annual)] <- 0
btm_temp_annual[is.na(btm_temp_annual)] <- 0

#Chlorophyll
chlor_annual <- raster(file.path(fp_chlor, paste('chlor_a_', month, '.grd', sep = ''))) %>%
  raster::resample(sst_annual)

# Zero out below 1000m
sst_annual[bat <= -1000] <- 0
btm_temp_annual[bat <= -1000] <- 0
cal_annual[bat <= -1000] <- 0

# bat[bat <= -1000] <- 0
bat_orig <- bat
#Take the log of the absolute value
bat <- log(abs(bat))
chlor_annual <- log(abs(chlor_annual))

bat[is.na(bat)] <- 0
chlor_annual[is.na(chlor_annual)] <- 0

bat[bat_orig <= -1000] <- 0
chlor_annual[bat_orig <= -1000] <- 0

for (month in 2:12){
  #Load sea surface temperature
  sst <- raster(file.path(fp_R45_sfce_temp_pres, paste('tsfce_pres_R45_', month, '.grd', sep = '')))
  #Load bottom temperature
  #Resample to same extent as sea surface temperature
  # Likely unnecessary, but avoids mismatch errors
  sst_btm <- raster(file.path(fp_R45_btm_temp_pres, paste('tbtm_pres_R45_', month, '.grd', sep = ''))) %>%
    raster::resample(sst)
  #Load surface salinity
  #Resample to same extent as sea surface temperature
  sal <- raster(file.path(fp_R45_sfce_sal_pres, paste('ssfce_pres_R45_', month, '.grd', sep = ''))) %>%
    raster::resample(sst)
  #Load bottom salinity
  #Resample to same extent as sea surface temperature
  sal_btm <- raster(file.path(fp_R45_btm_sal_pres, paste('sbtm_pres_R45_', month, '.grd', sep = ''))) %>%
    raster::resample(sst)
  
  #Replace NA values for deep water temperature and salinity with surface values
  #Otherwise data near the coasts and around shallow bathymetric features is missing
  sst_btm[is.nan(sst_btm)] <- sst[is.nan(sst_btm)]
  sal_btm[is.nan(sal_btm)] <- sal[is.nan(sal_btm)]
  
  #Load bathymetry
  #Resample to same extent as sea surface temperature
  bat <- raster(file.path(fp_bat, 'BathymetryGoMFlipped.grd')) %>%
    raster::resample(sst)
  
  #Load the present calanus climatology
  #Ji et al. 2011 life history model
  cal <- raster(file.path(fp_cal_pres, paste('calanus_', month, '.grd', sep = ''))) %>%
    raster::resample(sst)
  #Zero out values at depths less than 80m
  cal[bat >= -80] <- 0
  # #Change NA values to 0
  cal[is.na(cal)] <- 0
  
  sst[is.na(sst)] <- 0
  sst_btm[is.na(sst_btm)] <- 0
  
  #Chlorophyll
  chlor <- raster(file.path(fp_chlor, paste('chlor_a_', month, '.grd', sep = ''))) %>%
    raster::resample(sst)
  
  # Zero out below 1000m
  sst[bat <= -1000] <- 0
  sst_btm[bat <= -1000] <- 0
  cal[bat <= -1000] <- 0
  
  # bat[bat <= -1000] <- 0
  bat_orig <- bat
  #Take the log of the absolute value
  bat <- log(abs(bat))
  chlor <- log(abs(chlor))
  
  bat[is.na(bat)] <- 0
  chlor[is.na(chlor)] <- 0
  
  bat[bat_orig <= -1000] <- 0
  chlor[bat_orig <= -1000] <- 0
  
  sst_annual = sst_annual + sst
  btm_temp_annual = sst_btm + btm_temp_annual
  cal_annual = cal_annual + cal
  chlor_annual = chlor_annual + chlor

  
  #Stack covariates
  proj_covars <- stack(sst,
                       sst_btm,
                       # sal,
                       # sal_btm,
                       bat,
                       cal,
                       chlor)
  
  #Assign layer names 
  names(proj_covars) <- c('sst', 'sst_btm', 'bat', 'cal', 'chlor')
  
  annual <- merge(annual, proj_covars)
  
  print(paste("Month:", month))
  #print(layerStats(proj_covars,'pearson')$`pearson correlation coefficient`)
}

proj_covars <- stack(sst_annual/12,
                     btm_temp_annual/12,
                     # sal,
                     # sal_btm,
                     bat,
                     cal_annual/12,
                     chlor_annual/12)
names(proj_covars) <- c('sst', 'btm_temp', 'bat', 'cal', 'chlor')

print(layerStats(proj_covars,'pearson')$`pearson correlation coefficient`)
# 
# for (i in 1:12) {md <- full_data %>% filter(MONTH == i); print(NROW(md[md$pa == 0,]))}
# 

#'@
