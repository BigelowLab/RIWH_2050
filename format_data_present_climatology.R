#Load libraries
library(dplyr) #Useful functions for use with data frames (i.e. mutate, filter, etc.) & piping (%>%)
library(readr) #allows reading and writing of CSV files
library(raster) #Read in & create .grd raster files

#floor values if using second approach for pixels
latlon2cell <- function(lat,lon,cellsize_y, cellsize_x) {
  # %convert (lat lon) coordinates to (row column) coordinates
  # %lat=column vector containing latitude
  # %lon=column vector containing longitude
  # %ll_lat=lower left latitude
  # %ll_lon=lower left longitude
  # %cellsize=cell size in degrees
  # 
  # % yurcorner=46;
  # % xllcorner=-72;
  
  ROW=floor(((38.49874-lat)/cellsize_y))+1
  COL=floor(((lon-(-72.46014))/cellsize_x))+1
  
  return(c(COL,ROW))
}

DIR = '~/Desktop/Thesis/Bigelow/right_whale'
setwd(dir = DIR)

#Create filepath to presence/absense data
fp_pa <- file.path(DIR, 'pa_data')
#Calanus filepath
fp_cal <- file.path(DIR, 'trn_lyrs/JiCalanus')
#Bathymetry filepath
fp_bat <- file.path(DIR, 'trn_lyrs/bathy/gom')
#DFO layers filepath
fp_R45_btm_temp_pres <- file.path(DIR, 'trn_lyrs/DFO_2050/R45_btm/T_pres')
fp_R85_btm_temp_pres <- file.path(DIR, 'trn_lyrs/DFO_2050/R85_btm/T_pres')

fp_R45_btm_sal_pres <- file.path(DIR, 'trn_lyrs/DFO_2050/R45_btm/S_pres')
fp_R85_btm_sal_pres <- file.path(DIR, 'trn_lyrs/DFO_2050/R85_btm/S_pres')

fp_R45_sfce_temp_pres <- file.path(DIR, 'trn_lyrs/DFO_2050/R45_sfce/T_pres')
fp_R85_sfce_temp_pres <- file.path(DIR, 'trn_lyrs/DFO_2050/R85_sfce/T_pres')

fp_R45_sfce_sal_pres <- file.path(DIR, 'trn_lyrs/DFO_2050/R45_sfce/S_pres')
fp_R85_sfce_sal_pres <- file.path(DIR, 'trn_lyrs/DFO_2050/R85_sfce/S_pres')

pa_data <- readr::read_csv(file.path(fp_pa, 'RW_GOM2050_FORECAST_PAdata_Nov182019.csv')) %>%
  dplyr::select(YEAR, MONTH, DAY, latgrd, longrd, SPECCODE)

pa_data[c('lonpixel', 'latpixel')] <- latlon2cell(pa_data$latgrd, pa_data$longrd, 0.05785365, 0.08867679)

for (month in sort(unique(pa_data$MONTH))) {
  pa <- pa_data %>% filter(MONTH == month)
    
  print(paste(month))
    
  #load in initial environmental data layers for first sightings data point (1/2/2006)
  #SST from DFO climatology
  sst_raster <- raster(file.path(fp_R45_sfce_temp_pres, paste('tsfce_pres_R45_', month, '.grd', sep = '')))

  sst <- as.data.frame(sst_raster, xy = TRUE)
      
  sst[c('lonpixel', 'latpixel')] <- latlon2cell(sst$y, sst$x, 0.05785365, 0.08867679)
    
  #SST at bottom from DFO climatology
  sst_btm <- raster(file.path(fp_R45_btm_temp_pres, paste('tbtm_pres_R45_', month, '.grd', sep = '')))
      
  sst_btm[is.nan(sst_btm)] <- sst_raster[is.nan(sst_btm)]
      
  sst_btm <- as.data.frame(sst_btm, xy = TRUE)
      
  sst_btm[c('lonpixel', 'latpixel')] <- latlon2cell(sst_btm$y, sst_btm$x, 0.05785365, 0.08867679)
      
  #Salinity from DFO climatology
  sal_raster <- raster(file.path(fp_R45_sfce_sal_pres, paste('ssfce_pres_R45_', month, '.grd', sep = '')))
    
  sal <- as.data.frame(sal_raster, xy = TRUE) 
      
  sal[c('lonpixel', 'latpixel')] <- latlon2cell(sal$y, sal$x, 0.05785365, 0.08867679)
      
  #Salinity at bottom from DFO climatology
  sal_btm <- raster(file.path(fp_R45_btm_sal_pres, paste('sbtm_pres_R45_', month, '.grd', sep = '')))
      
  sal_btm[is.nan(sal_btm)] <- sal_raster[is.nan(sal_btm)]
      
  sal_btm <- as.data.frame(sal_btm, xy = TRUE)
      
  sal_btm[c('lonpixel', 'latpixel')] <- latlon2cell(sal_btm$y, sal_btm$x, 0.05785365, 0.08867679)
      
  #Bathymetry for Gulf of Maine
  bat_raster <- raster(file.path(fp_bat, 'BathymetryGoMFlipped.grd')) %>%
    raster::resample(sst_raster)
    
  bat <- as.data.frame(bat_raster, xy = TRUE) 
      
  bat[c('lonpixel', 'latpixel')] <- latlon2cell(bat$y, bat$x, 0.05785365, 0.08867679)
      
  #Calanus from Ji's diapause model
  cal_raster <- raster(file.path(fp_cal, paste('calanus_yearly_', month, '.grd', sep = ''))) %>%
      raster::resample(sst_raster)
  #Reclassify of points at less than 80m depth
  cal_raster[bat_raster >= -80] <- 0
  cal_raster[is.na(cal_raster)] <- 0
      
  cal <- as.data.frame(cal_raster, xy = TRUE) 
      
  cal[c('lonpixel', 'latpixel')] <- latlon2cell(cal$y, cal$x, 0.05785365, 0.08867679)
      
  names(sst) <- c('x', 'y', 'layer', 'lonpixel', 'latpixel')
  names(sst_btm) <- c('x', 'y', 'layer', 'lonpixel', 'latpixel')
  names(sal) <- c('x', 'y', 'layer', 'lonpixel', 'latpixel')
  names(sal_btm) <- c('x', 'y', 'layer', 'lonpixel', 'latpixel')
      
  covars <- data.frame('lonpixel' = sst$lonpixel, 'latpixel' = sst$latpixel,
                       'sst' = sst$layer, 'sst_btm' = sst_btm$layer, 
                       'sal' = sal$layer, 'sal_btm' = sal_btm$layer, 'bat' = bat$layer, 'cal' = cal$layer)
      
  temp_full_data <- left_join(pa, covars, by = c('lonpixel', 'latpixel'))
      
  if (month == 1) {
    full_data <- temp_full_data
  }else{
    full_data <- rbind(full_data, temp_full_data)
  }
}

write_csv(full_data, file.path(fp_pa,'RIWH_NOV_2019_PRES_CLIMATOLOGY.csv'))
