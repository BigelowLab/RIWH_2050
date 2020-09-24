#Camille Ross
#February 2020
#Honors thesis, Colby College Environmental Studies Department
#Purpose: Build monthly GAM, GBM, and ANN models, save the evaluations and variable contributions of each model, 
#         create an ensemble of the individual models, and project individual models and the ensemble back onto 
#         the present climatology and forecast into 2050 for RCP 4.5 and RCP 8.5.

#---------------------- LOAD LIBRARIES ----------------------
library(biomod2) #Modelling package
library(dplyr) #Useful functions for use with data frames (i.e. mutate, filter, etc.) & piping (%>%)
library(RColorBrewer) #Color pallette creation 
library(viridis) #Pre-made color pallettes
library(readr) #allows reading and writing of CSV files
library(ggplot2) #Plots
library(lubridate) #Allows for datetime manipulation
library(Hmisc) #Allows monthday conversions

#---------------------- SETUP DIRECTORIES ----------------------

#Set working directory
#Create directories and save filepaths for presence/absence data, covariate data, and results
DIR <- '~/Desktop/Thesis/Bigelow/right_whale'
#Create filepath to presence/absense data
fp_pa <- file.path(DIR, 'pa_data')

#DFO climatology filepaths
fp_R45_btm_temp_pres <- file.path(DIR, 'trn_lyrs/DFO_2050/R45_btm/T_pres')
fp_R85_btm_temp_pres <- file.path(DIR, 'trn_lyrs/DFO_2050/R85_btm/T_pres')

fp_R45_btm_sal_pres <- file.path(DIR, 'trn_lyrs/DFO_2050/R45_btm/S_pres')
fp_R85_btm_sal_pres <- file.path(DIR, 'trn_lyrs/DFO_2050/R85_btm/S_pres')

fp_R45_sfce_temp_pres <- file.path(DIR, 'trn_lyrs/DFO_2050/R45_sfce/T_pres')
fp_R85_sfce_temp_pres <- file.path(DIR, 'trn_lyrs/DFO_2050/R85_sfce/T_pres')

fp_R45_sfce_sal_pres <- file.path(DIR, 'trn_lyrs/DFO_2050/R45_sfce/S_pres')
fp_R85_sfce_sal_pres <- file.path(DIR, 'trn_lyrs/DFO_2050/R85_sfce/S_pres')

fp_R45_btm_temp_2050 <- file.path(DIR, 'trn_lyrs/DFO_2050/R45_btm/T_2050')
fp_R85_btm_temp_2050 <- file.path(DIR, 'trn_lyrs/DFO_2050/R85_btm/T_2050')

fp_R45_btm_sal_2050 <- file.path(DIR, 'trn_lyrs/DFO_2050/R45_btm/S_2050')
fp_R85_btm_sal_2050 <- file.path(DIR, 'trn_lyrs/DFO_2050/R85_btm/S_2050')

fp_R45_sfce_temp_2050 <- file.path(DIR, 'trn_lyrs/DFO_2050/R45_sfce/T_2050')
fp_R85_sfce_temp_2050 <- file.path(DIR, 'trn_lyrs/DFO_2050/R85_sfce/T_2050')

fp_R45_sfce_sal_2050 <- file.path(DIR, 'trn_lyrs/DFO_2050/R45_sfce/S_2050')
fp_R85_sfce_sal_2050 <- file.path(DIR, 'trn_lyrs/DFO_2050/R85_sfce/S_2050')

#Calanus filepaths
fp_cal_pres <- file.path(DIR, 'trn_lyrs/JiCalanus')
fp_cal_2050 <- file.path(DIR, 'trn_lyrs/JiCalanus2050')

#Chlorophyll filepath
fp_chlor <- file.path(DIR, 'trn_lyrs/chlor_climatology')

#Bathymetry filepath
fp_bat <- file.path(DIR, 'trn_lyrs/bathy/gom')

#Formatted presence/absence & covariate data
full_data <- read_csv(file.path(fp_pa,'RIWH_MAR_2020_PRES_CLIMATOLOGY.csv'))

#Take log of bathymetry
#Convert presence/absence to binary
full_data <- full_data %>%
  filter(YEAR < 2010) %>%
  mutate(sst = ifelse(bat <= -1000, NA, sst),
         sst_btm = ifelse(bat <= -1000, NA, sst_btm),
         sal = ifelse(bat <= -1000, NA, sal),
         sal_btm = ifelse(bat <= -1000, NA, sal_btm),
         cal = ifelse(bat <= -1000, NA, cal),
         chlor = ifelse(bat <= -1000, NA, log(abs(as.numeric(chlor)))),
         bat = ifelse(bat <= -1000, NA, log(abs(as.numeric(bat)))),
         #Add binary presence/absence column (p = 1, a = 0)
         pa = if_else(is.na(SPECCODE), 0, 1))


#Create directory for model
dir.create(file.path(DIR, 'paper_2050_models_final_run_final'), showWarnings = FALSE)
#Create reference variable
fp_model <- file.path(DIR, 'paper_2050_models_final_run_final')

#Set directory to model filepath
setwd(dir = fp_model)

#Create directory to evals
dir.create(file.path(fp_model, 'evals'), showWarnings = FALSE)
#Create variable with reference to evals subdirectory
fp_evals <- file.path(fp_model, 'evals')

#Create directory to evals
dir.create(file.path(fp_model, 'variable_importance'), showWarnings = FALSE)
#Create variable with reference to evals subdirectory
fp_var <- file.path(fp_model, 'variable_importance')

#Create directory to response curves
dir.create(file.path(fp_model, 'response_curves'), showWarnings = FALSE)
#Create variable with reference to response curve subdirectory
fp_rc <- file.path(fp_model, 'response_curves')

#Create directory to habitat maps
dir.create(file.path(fp_model, 'habitat_maps'), showWarnings = FALSE)
#Create variable with reference to habitat maps subdirectory
fp_maps <- file.path(fp_model, 'habitat_maps')

#List of months for plotting
month_labs <- c('January', 'February', 'March',
                'April', 'May', 'June',
                'July', 'August', 'September',
                'October', 'November', 'December')

species_name <- 'RIWH.2050.Paper.Final'

#Initialize models, list holding names of models to run
#Run ?BIOMOD_ModelingOptions for names of possible models
models <- c("GAM", "GBM", "ANN")

#Number of cross-validation folds (i.e. number of model runs)
cv_folds <- 10
#Training/evaluation data split (i.e. percent of data of use for training, rest will be saved for evaluation)
data_split <- 70

#---------------------- BUILD, EVALUATE, & PROJECT MONTHLY MODELS ---------------------

for (month in c(1:12)) {
  
  #---------------------- FORMAT DATA & TUNE MODELS ----------------------
  #Configure individual model parameters
  #GAM -- specify k parameter
  #GBM -- Specify number of trees, interaction depth (tree complexity), 
  # shrinkage (learning rate), bagging fraction, and number of cross-validation folds (other parameters avalable for tuning)
  #ANN -- specify number of cross validatations and maximum iterations
  modelOptions <- BIOMOD_ModelingOptions(GAM = list(algo = "GAM_mgcv",
                                                    k = 4),
                                         GBM = list(n.trees = 1000,
                                                    interaction.depth = 9,
                                                    shrinkage = 0.01,
                                                    bag.fraction = 0.5,
                                                    cv.folds = 10),
                                         ANN = list(NbCV = 10,
                                                    maxit = 1000))
                                         # RF = list(do.classif = FALSE,
                                         #           ntree= 1000),
                                         # MAXENT.Phillips = list(path_to_maxent.jar = DIR,
                                         #                        maximumiterations = 1000,
                                         #                        defaultprevalence = 0.7))
  
  #Filter training data for the current month
  trainingData <- full_data %>% filter(MONTH == month)
  
  #Isolate binary presence/absence data
  trainingPA <- as.data.frame(trainingData[,'pa'])
  #Isolate presence/absence coordiantes
  trainingXY <- trainingData[,c('longrd', 'latgrd')]
  #Isolate environmental covariates
  #Select variables based on month
  trainingCovars <- as.data.frame(trainingData[,c('sst', 'sst_btm', 'bat', 'cal', 'chlor')])
  
  #Format data for use in Biomod2 modelling function
  #' @param resp.var <dataframe or vector> training binary presence & absence data, i.e. response variable
  #' @param resp.xy <matrix> two-column, X and Y coordinates of resp.var
  #' @param resp.name <chr> species name
  #' @param expl.var <matrix, RasterStack, or dataframe> contains training explanatory variable data (environmental covariates)
  #' @param eval.resp.var <dataframe or vector> testing binary presence & absence data, i.e. response variable
  #' @param eval.expl.var <matrix, RasterStack, or dataframe> contains testing explanatory variable data (environmental covariates)
  #' @param eval.resp.xy <matrix> two-column, X and Y coordinates of eval.resp.var 
  biomodData <- BIOMOD_FormatingData(resp.var = trainingPA,
                                     expl.var = trainingCovars,
                                     resp.xy = trainingXY,
                                     resp.name = species_name)
  
  #---------------------- BUILD MODELS ----------------------
  #Random data split determined by data_split variable assigned in SETUP DIRECTORIES section
  #Number of cross-validation folds determined by cv_folds variable assigned in SETUP DIRECTORIES section
  
  #Build models
  #' @param data <BIOMOD.formated.data object> the data returned by BIOMOD_FormatingData function
  #' @param models <vector> names of models chosen
  #' @param models.options <BIOMOD.models.options object> the model options returned by BIOMOD_ModelingOptions function
  #' @param NbRunEval <numeric> number of evaluations to run i.e. the number of folds for cross-validation
  #' @param DataSplit <numeric> percentage of data that is training data; the remainder is used for evaluation
  #' @param Yweights <vector> Y weights for model; default is NULL
  #' @param Prevalence <double> deouble between 0 and 1 used to build response weights; prevalence of 0.5 gives
  #' equal weight to presences and absences; prevalence > 0.5 gives more weight to presences; prevalence < 0.5 gives
  #' more weight to absences
  #' @param VarImport <numeric> number of permutations used to estimate variable importance
  #' @param models.eval.meth <vector> names of evaluation metrics to use
  #' @param SaveObj <logical> TRUE saves results to hard drive; useful for loading models later without having to re-run
  #' @param rescal.all.models <logical> scales model predictions with a binomical GLM; FALSE here
  #' @param do.full.models <logical> if true, a model run is done using all of the data; i.e. trains and evaluates on same 
  #' data in that model run; useful to rare species cases where presences are low
  #' @param modeling.id <character> name of model
  #' @param DataSplitTable <matrix, data.frame, or 3Darray> contains TRUE/FALSE values specifying which data points
  #' to use for training (TRUE) and which to use for evaluation (FALSE); each column corresponds to a model run i.e.
  #' number of columns is equal to desired number of cross validation folds; not used here, but if used NbRunEval, DataSplit,
  #' & do.full.models arguments are ignored
  modelOut <- BIOMOD_Modeling(data = biomodData,
                              models = models,
                              models.options = modelOptions,
                              NbRunEval = cv_folds,
                              DataSplit = data_split,
                              Prevalence = 0.7,
                              VarImport = 5,
                              models.eval.meth = c('ROC', 'TSS', 'KAPPA'),
                              SaveObj = TRUE,
                              rescal.all.models = FALSE,
                              do.full.models = TRUE,
                              modeling.id = paste0(species_name, "_", month))
  
  #---------------------- SAVE EVALUATIONS & VARIABLE CONTRIBUTION ----------------------
  #Retrieves model evaluations
  #'@param obj <BIOMOD.models.out> model produced using BIOMOD_Modeling
  #'@param as.data.frame <boolean> if TRUE, function returns evaluations as a dataframe
  modelEvals <- get_evaluations(obj = modelOut, as.data.frame = TRUE)
  #Saves model evaluations to a csv file in the results directory
  #Allows for easy analysis
  write.csv(modelEvals, file = file.path(fp_evals, paste(month, '_evals.csv', sep = '')), row.names = TRUE)
  
  #Retrieves variable contribution
  #'@param obj <BIOMOD.models.out> model produced using BIOMOD_Modeling
  #'@param as.data.frame <boolean> if TRUE, function returns variable contributions as a dataframe
  varImportance <- get_variables_importance(obj = modelOut, as.data.frame = TRUE)
  #Saves model evaluations to a csv file in the results directory
  #Allows for easy analysis
  write.csv(varImportance, file = file.path(fp_var, paste(month, '_var_importance.csv', sep = '')), row.names = TRUE)
  
  #---------------------- PLOT RESPONSE CURVES ----------------------
  #Plots for individual models and a combination of all of the models
  
  #GBM response curve; check biological plausibility
  response.plot2(models  = BIOMOD_LoadModels(modelOut, models='GBM'),
                 Data = get_formal_data(modelOut,'expl.var'), 
                 show.variables= get_formal_data(modelOut,'expl.var.names'),
                 do.bivariate = FALSE,
                 fixed.var.metric = 'median',
                 col = c("blue", "red", "green"),
                 save.file = "jpeg",
                 name = file.path(fp_rc, paste("GBM_response_curve_", month, sep = "")),
                 legend = FALSE,
                 main = "GBM Response Curves",
                 data_species = get_formal_data(modelOut,'resp.var'))
  
  #GAM response curve; check biological plausibility
  response.plot2(models  = BIOMOD_LoadModels(modelOut, models='GAM'),
                 Data = get_formal_data(modelOut,'expl.var'), 
                 show.variables= get_formal_data(modelOut,'expl.var.names'),
                 do.bivariate = FALSE,
                 fixed.var.metric = 'median',
                 col = c("blue", "red", "green"),
                 save.file = "jpeg",
                 name = file.path(fp_rc, paste("GAM_response_curve_", month, sep = "")),
                 legend = FALSE,
                 main = "GAM Response Curves",
                 data_species = get_formal_data(modelOut,'resp.var'))
  
  #ANN response curve; check biological plausibility
  response.plot2(models  = BIOMOD_LoadModels(modelOut, models='ANN'),
                 Data = get_formal_data(modelOut,'expl.var'), 
                 show.variables= get_formal_data(modelOut,'expl.var.names'),
                 do.bivariate = FALSE,
                 fixed.var.metric = 'median',
                 col = c("blue", "red", "green"),
                 save.file = "jpeg",
                 name = file.path(fp_rc, paste("ANN_response_curve_", month, sep = "")),
                 legend = FALSE,
                 main = "ANN Response Curves",
                 data_species = get_formal_data(modelOut,'resp.var'))
  
  #Overall response curve; check biological plausibility
  response.plot2(models  = BIOMOD_LoadModels(modelOut),
                 Data = get_formal_data(modelOut,'expl.var'), 
                 show.variables= get_formal_data(modelOut,'expl.var.names'),
                 do.bivariate = FALSE,
                 fixed.var.metric = 'median',
                 col = c("blue", "red", "green"),
                 save.file = "jpeg",
                 name = file.path(fp_rc, paste("all_models_response_curve_", month, sep = "")),
                 legend = FALSE,
                 main = "Response Curves for All Models",
                 data_species = get_formal_data(modelOut,'resp.var'))
  
  
  #---------------------- BUILD ENSEMBLE MODEL ----------------------
  
  biomodEM <- BIOMOD_EnsembleModeling(
                modeling.output = modelOut,
                chosen.models = 'all',
                em.by = 'all',
                eval.metric = c('ROC'),
                eval.metric.quality.threshold = c(0.7),
                prob.mean = TRUE,
                prob.cv = TRUE,
                prob.ci = TRUE,
                prob.ci.alpha = 0.05,
                prob.median = TRUE,
                committee.averaging = TRUE,
                prob.mean.weight = TRUE,
                prob.mean.weight.decay = 'proportional')
  
  #---------------------- SAVE ENSEMBLE EVALUATIONS ----------------------
  
  #Retrieves ensemble model evaluations
  modelEvals <- get_evaluations(obj = biomodEM, as.data.frame = TRUE)
  #Saves ensemble model evaluations to a csv file in the results directory
  #Allows for easy analysis
  write.csv(modelEvals, file = file.path(fp_evals, paste(month, '_ensemble_evals.csv', sep = '')), row.names = TRUE)
  
  #---------------------- LOAD ENVIRONMENTAL COVARIATES FOR PROJECTION ----------------------
  #Same covariates used to build the models
  #From the DFO present climatology
  
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
  bat_raw <- bat
  
  #Load the present calanus climatology
  #Ji et al. 2011 life history model
  cal <- raster(file.path(fp_cal_pres, paste('calanus_', month, '.grd', sep = ''))) %>%
    raster::resample(sst)
  #Zero out values at depths less than 80m
  cal[bat >= -80] <- 0
  #Change NA values to 0
  cal[is.na(cal)] <- 0
  
  #Load chlorophyll
  chlor <- raster(file.path(fp_chlor, paste('chlor_a_', month, '.grd', sep = ''))) %>%
    raster::resample(sst)
  
  #Take the log of the absolute value of bathymetry and chlorophyll
  bat <- log(abs(bat))
  chlor <- log(abs(chlor))
  
  # Zero out below 1000m
  sst[bat_raw <= -1000] <- NA
  sst_btm[bat_raw <= -1000] <- NA
  cal[bat_raw <= -1000] <- NA
  bat[bat_raw <= -1000] <- NA
  chlor[bat_raw <= -1000] <- NA
  
  #Stack covariates
  proj_covars <- stack(sst,
                       sst_btm,
                       bat,
                       cal,
                       chlor)
   
  #Assign layer names 
  names(proj_covars) <- c('sst', 'sst_btm', 'bat', 'cal', 'chlor')
  
  #Filter presence data by season
  #Select lat and long of each sighting
  presences <- trainingData %>%
    filter(pa == 1) %>%
    dplyr::select(latgrd, longrd)
  
  #---------------------- PROJECT ENSEMBLE MODEL ----------------------
  
  #Project all models
  #Neccessary parameter for ensemble forecasting
  biomodProjFuture <- BIOMOD_Projection(modeling.output = modelOut,
                                        new.env = proj_covars,
                                        proj.name = 'all',
                                        selected.models = 'all',
                                        binary.meth = 'ROC',
                                        compress = 'xz',
                                        build.clamping.mask = FALSE,
                                        output.format = '.grd')
  
  #Build ensemble forecast
  myBiomodEF <- BIOMOD_EnsembleForecasting(EM.output = biomodEM,
                                           projection.output = biomodProjFuture,
                                           proj.name = paste(month, '_Ensemble', sep = ''))
  
  #Load ensemble forecast as raster
  #Divide by 1000 to convert probabilities to percentages
  ensemble_proj_raster <- raster(file.path(species_name, paste('proj_', month, '_Ensemble', sep = ''), paste('proj_', month, '_Ensemble_', species_name, '_ensemble.grd', sep = ''))) %>%
    `/`(1000)

  crs(ensemble_proj_raster) <- '+init=epsg:4121 +proj=longlat +ellps=GRS80 +datum=GGRS87 +no_defs +towgs84=-199.87,74.79,246.62'
  
  #Save projection raster as data frame with xy coords and no NAs
  ensemble_proj_df <- as.data.frame(ensemble_proj_raster, xy = TRUE, na.rm = TRUE)
  
  #Assign column names
  names(ensemble_proj_df) <- c('x', 'y', 'prob')
  
  #Latitude breaks
  lat_breaks <- seq(min(ensemble_proj_df$y), max(ensemble_proj_df$y) + 1, 2)
  #Latitude label text
  lat_text <- paste(abs(round(lat_breaks, 1)), '°N')
  
  #Longitude breaks
  lon_breaks <- seq(min(ensemble_proj_df$x)-1, max(ensemble_proj_df$x)+1, 4)
  #Longitude label text
  lon_text <- paste(abs(round(lon_breaks, 1)), '°W')
  
  #Plot map, probabilities, and sightings data
  ggplot() + 
    #Add probability data
    geom_tile(data = ensemble_proj_df, aes(x, y, fill = prob)) +
    #Add probabilitiy color gradient and label
    scale_fill_gradientn(colours = inferno(500), limits = c(0,1), name = 'Probability') +
    labs(x = "Longitude", 
         y = "Latitude", 
         title = paste(month_labs[[month]], "Ensemble")) +
    #Add world map data
    # geom_polygon(data = worldmap, aes(long, lat, group = group), fill = NA, colour = "gray43") +
    coord_quickmap(xlim = c(round(min(ensemble_proj_df$x)), round(max(ensemble_proj_df$x))), 
                   ylim = c(round(min(ensemble_proj_df$y)), round(max(ensemble_proj_df$y))),
                   expand = TRUE) +
    #Add latitude and longitude labels
    scale_y_continuous(breaks = lat_breaks, labels = lat_text, limits = NA) +
    scale_x_continuous(breaks = lon_breaks, labels = lon_text, limits = NA) +
    #Remove grid lines
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    #Add sightings data
    geom_point(data = presences, aes(longrd, latgrd), pch = 21, cex = 1, color = 'black', fill = 'white') +
    #Save plot to hard drive
    ggsave(filename = file.path(fp_maps, paste(month, '_ensemble_pres.jpeg', sep = '')), width = 7, height = 7)
  
  #Plot map, probabilities without sightings
  ggplot() + 
    #Add probability data
    geom_tile(data = ensemble_proj_df, aes(x, y, fill = prob)) +
    #Add probabilitiy color gradient and label
    scale_fill_gradientn(colours = inferno(500), limits = c(0,1), name = 'Probability') +
    labs(x = "Longitude", 
         y = "Latitude", 
         title = paste(month_labs[[month]], "Ensemble")) +
    #Add world map data
    # geom_polygon(data = worldmap, aes(long, lat, group = group), fill = NA, colour = "gray43") +
    coord_quickmap(xlim = c(round(min(ensemble_proj_df$x)), round(max(ensemble_proj_df$x))), 
                   ylim = c(round(min(ensemble_proj_df$y)), round(max(ensemble_proj_df$y))),
                   expand = TRUE) +
    #Add latitude and longitude labels
    scale_y_continuous(breaks = lat_breaks, labels = lat_text, limits = NA) +
    scale_x_continuous(breaks = lon_breaks, labels = lon_text, limits = NA) +
    #Remove grid lines
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    #Save plot to hard drive
    ggsave(filename = file.path(fp_maps, paste(month, '_ensemble_pres_no_sightings.jpeg', sep = '')), width = 7, height = 7)
  
  #---------------------- PROJECT INDIVIDUAL MODELS ----------------------
  for (model in models) {
    
    #Create vector all runs of model algorithm for projection
    select_models <- c()
    for (i in 1:cv_folds) {
      select_models[i] <- paste0(species_name, "_AllData_RUN", i, "_", model)
    }
    
  
    #Create projection of single model
    #'@param modeling.output <BIOMOD.models.out> biomod models produced by BIOMOD_Modeling
    #'@param new.env <matrix, RasterStack, or dataframe> environemntal covariate layers upon which to project the models
    #'@param proj.name <char> name of projection
    #'@param selected.models <char or vector> models to project
    #'@param binary.meth <char or vector> evaluation method or methods used to compute models initially, same as input to BIOMOD_Modeling
    #'@param compress <boolean or char> format to compress projection when stored on hard disk
    #'@param build.clamping.mask <boolean> saves clamping mask on hard disk
    #'@param output.format <chr> specify format of output saved on hard disk, i.e. .grd is good because projection can be loaded back as Raster object
    biomodProj <- BIOMOD_Projection(modeling.output = modelOut,
                                          new.env = proj_covars,
                                          proj.name = paste(month, '_', model, '_onto_pres', sep = ''),
                                          selected.models = select_models,
                                          binary.meth = 'ROC',
                                          compress = 'xz',
                                          build.clamping.mask = TRUE,
                                          output.format = '.grd')

    #Load projection as a raster from .grd output saved on hard disk
    #Divide all values by 1000 to conver to 0-1.0 probabilities
    #Probabilities are computed in range 1-1000 to save memory
    #Modify extent to range of PA data
    #DOES THIS MESS ANYTHING UP?
    proj_raster <- raster(file.path(species_name, paste('proj_', month, '_', model, '_onto_pres',
                                                            '/proj_', month, '_', model, '_onto_pres', '_', species_name, '.grd', sep = ''))) %>%
      `/`(1000)
    
    crs(proj_raster) <- '+init=epsg:4121 +proj=longlat +ellps=GRS80 +datum=GGRS87 +no_defs +towgs84=-199.87,74.79,246.62'
    
    #Save projection raster as data frame with xy coords and no NAs
    proj_df <- as.data.frame(proj_raster, xy = TRUE, na.rm = TRUE)
    
    #Assign column names
    names(proj_df) <- c('x', 'y', 'prob')
    
    #Plot map, probabilities, and sightings data
    ggplot() + 
      #Add probability data
      geom_tile(data = proj_df, aes(x, y, fill = prob)) +
      #Add probabilitiy color gradient and label
      scale_fill_gradientn(colours = inferno(500), limits = c(0,1), name = 'Probability') +
      labs(x = "Longitude", 
           y = "Latitude", 
           title = paste(month_labs[[month]], model, 'Present')) +
      #Add world map data
      # geom_polygon(data = worldmap, aes(long, lat, group = group), fill = "gray43", colour = "gray43") +
      coord_quickmap(xlim = c(round(min(proj_df$x)), round(max(proj_df$x))), 
                     ylim = c(round(min(proj_df$y)), round(max(proj_df$y))),
                     expand = TRUE) +
      #Add latitude and longitude labels
      scale_y_continuous(breaks = lat_breaks, labels = lat_text, limits = NA) +
      scale_x_continuous(breaks = lon_breaks, labels = lon_text, limits = NA) +
      #Remove grid lines
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      #Add sightings data
      geom_point(data = presences, aes(longrd, latgrd), pch = 21, cex = 1, color = 'black', fill = 'white') +
      #Save plot to hard drive
      ggsave(filename = file.path(fp_maps, paste(month, '_', model, '_pres.jpeg', sep = '')), width = 7, height = 7)
    
    #Plot map, probabilities without sightings data
    ggplot() + 
      #Add probability data
      geom_tile(data = proj_df, aes(x, y, fill = prob)) +
      #Add probabilitiy color gradient and label
      scale_fill_gradientn(colours = inferno(500), limits = c(0,1), name = 'Probability') +
      labs(x = "Longitude", 
           y = "Latitude", 
           title = paste(month_labs[[month]], model, 'Present')) +
      # Add world map data
      # geom_polygon(data = worldmap, aes(long, lat, group = group), fill = "gray43", colour = "gray43") +
      coord_quickmap(xlim = c(round(min(proj_df$x)), round(max(proj_df$x))), 
                     ylim = c(round(min(proj_df$y)), round(max(proj_df$y))),
                     expand = TRUE) +
      #Add latitude and longitude labels
      scale_y_continuous(breaks = lat_breaks, labels = lat_text, limits = NA) +
      scale_x_continuous(breaks = lon_breaks, labels = lon_text, limits = NA) +
      #Remove grid lines
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      #Save plot to hard drive
      ggsave(filename = file.path(fp_maps, paste(month, '_', model, '_pres_no_sightings.jpeg', sep = '')), width = 7, height = 7)
    
  }
  
  #---------------------- LOAD ENVIRONMENTAL COVARIATES FOR 2050 FORECAST ----------------------
  #Same covariates used to build the models
  #From the DFO 2050 climatology
  
  #Load sea surface temperature for R45
  sst_2050_R45 <- raster(file.path(fp_R45_sfce_temp_2050, paste('tsfce_2050_R45_', month, '.grd', sep = '')))
  #Load sea surface temperature for R85
  sst_2050_R85 <- raster(file.path(fp_R85_sfce_temp_2050, paste('tsfce_2050_R85_', month, '.grd', sep = '')))
  #Load bottom temperature for R45
  #Resample to same extent as sea surface temperature
  #Likely unnecessary, but avoids mismatch errors
  sst_btm_2050_R45 <- raster(file.path(fp_R45_btm_temp_2050, paste('tbtm_2050_R45_', month, '.grd', sep = ''))) %>%
    raster::resample(sst)
  #Load bottom tempearture for R85
  sst_btm_2050_R85 <- raster(file.path(fp_R85_btm_temp_2050, paste('tbtm_2050_R85_', month, '.grd', sep = ''))) %>%
    raster::resample(sst)
  #Load surface salinity for R45
  #Resample to same extent as sea surface temperature
  sal_2050_R45 <- raster(file.path(fp_R45_sfce_sal_2050, paste('ssfce_2050_R45_', month, '.grd', sep = ''))) %>%
    raster::resample(sst)
  #Load surface salinity for R85
  sal_2050_R85 <- raster(file.path(fp_R85_sfce_sal_2050, paste('ssfce_2050_R85_', month, '.grd', sep = ''))) %>%
    raster::resample(sst)
  #Load bottom salinity for R45
  #Resample to same extent as sea surface temperature
  sal_btm_2050_R45 <- raster(file.path(fp_R45_btm_sal_2050, paste('sbtm_2050_R45_', month, '.grd', sep = ''))) %>%
    raster::resample(sst)
  #Load bottom salinity for R85
  sal_btm_2050_R85 <- raster(file.path(fp_R85_btm_sal_2050, paste('sbtm_2050_R85_', month, '.grd', sep = ''))) %>%
    raster::resample(sst)
  
  #Replace NA values for deep water temperature and salinity with surface values for R45
  #Otherwise data near the coasts and around shallow bathymetric features is missing
  sst_btm_2050_R45[is.nan(sst_btm_2050_R45)] <- sst_2050_R45[is.nan(sst_btm_2050_R45)]
  sal_btm_2050_R45[is.nan(sal_btm_2050_R45)] <- sal_2050_R45[is.nan(sal_btm_2050_R45)]
  #Repeat for R85
  sst_btm_2050_R85[is.nan(sst_btm_2050_R85)] <- sst_2050_R85[is.nan(sst_btm_2050_R85)]
  sal_btm_2050_R85[is.nan(sal_btm_2050_R85)] <- sal_2050_R85[is.nan(sal_btm_2050_R85)]
  
  #Load the 2050 calanus climatology
  #Ji et al. 2011 life history model
  #R45 calanus
  cal_2050_R45 <- raster(file.path(fp_cal_2050, paste('calanus_R45_', month, '.grd', sep = ''))) %>%
    raster::resample(sst)
  #Zero out values at depths less than 80m
  cal_2050_R45[bat_raw >= -80] <- 0
  #Change NA values to 0
  cal_2050_R45[is.na(cal_2050_R45)] <- 0
  #R85 calanus
  cal_2050_R85 <- raster(file.path(fp_cal_2050, paste('calanus_R85_', month, '.grd', sep = ''))) %>%
    raster::resample(sst)
  #Zero out values at depths less than 80m
  cal_2050_R85[bat_raw >= -80] <- 0
  #Change NA values to 0
  cal_2050_R85[is.na(cal_2050_R85)] <- 0
  
  #Load chlorophyll
  chlor <- raster(file.path(fp_chlor, paste('chlor_a_', month, '.grd', sep = ''))) %>%
    raster::resample(sst)
  #Double chlorophyll
  chlor_2x <- log(abs(chlor*2))
  #Halve chlorophyll
  chlor_half <- log(abs(chlor/2))
  #Take log
  chlor <- log(abs(chlor))
  
  #Zero below 1000m
  sst_2050_R45[bat_raw <= -1000] <- NA
  sst_2050_R85[bat_raw <= -1000] <- NA
  sst_btm_2050_R45[bat_raw <= -1000] <- NA
  sst_btm_2050_R85[bat_raw <= -1000] <- NA
  cal_2050_R45[bat_raw <= -1000] <- NA
  cal_2050_R85[bat_raw <= -1000] <- NA
  chlor[bat_raw <= -1000] <- NA
  chlor_2x[bat_raw <= -1000] <- NA
  chlor_half[bat_raw <= -1000] <- NA
  
  #Stack covariates for R45
  proj_covars_2050_R45 <- stack(sst_2050_R45,
                                       sst_btm_2050_R45,
                                       bat,
                                       cal_2050_R45,
                                       chlor)
  #Stack covariates for R85
  proj_covars_2050_R85 <- stack(sst_2050_R85,
                                       sst_btm_2050_R85,
                                       bat,
                                       cal_2050_R85,
                                       chlor)
  
  #Stack covariates for R45 for double chlorophyll
  proj_covars_2050_R45_2x_chl <- stack(sst_2050_R45,
                                       sst_btm_2050_R45,
                                       bat,
                                       cal_2050_R45,
                                       chlor_2x)
  #Stack covariates for R85 for double chlorophyll
  proj_covars_2050_R85_2x_chl <- stack(sst_2050_R85,
                                       sst_btm_2050_R85,
                                       bat,
                                       cal_2050_R85,
                                       chlor_2x)
  
  #Stack covariates for R45 for half chlorophyll
  proj_covars_2050_R45_half_chl <- stack(sst_2050_R45,
                                         sst_btm_2050_R45,
                                         bat,
                                         cal_2050_R45,
                                         chlor_half)
  #Stack covariates for R85 for half chlorophyll
  proj_covars_2050_R85_half_chl <- stack(sst_2050_R85,
                                         sst_btm_2050_R85,
                                         bat,
                                         cal_2050_R85,
                                         chlor_half)
  
  #Assign layer names for R45
  names(proj_covars_2050_R45) <- c('sst', 'sst_btm', 'bat', 'cal', 'chlor')
  #Assign layer names for R85
  names(proj_covars_2050_R85) <- c('sst', 'sst_btm', 'bat', 'cal', 'chlor')
  
  #Assign layer names for R45 for double chlorophyll
  names(proj_covars_2050_R45_2x_chl) <- c('sst', 'sst_btm', 'bat', 'cal', 'chlor')
  #Assign layer names for R85 for double chlorophyll
  names(proj_covars_2050_R85_2x_chl) <- c('sst', 'sst_btm', 'bat', 'cal', 'chlor')
  
  #Assign layer names for R45 for half chlorophyll
  names(proj_covars_2050_R45_half_chl) <- c('sst', 'sst_btm', 'bat', 'cal', 'chlor')
  #Assign layer names for R85 for half chlorophyll
  names(proj_covars_2050_R85_half_chl) <- c('sst', 'sst_btm', 'bat', 'cal', 'chlor')
  
  #---------------------- FORECAST ENSEMBLE MODEL FOR R45 ----------------------
  
  #Project all models
  biomodProj2050_R45 <- BIOMOD_Projection(modeling.output = modelOut,
                                                 new.env = proj_covars_2050_R45,
                                                 proj.name = 'all',
                                                 selected.models = 'all',
                                                 binary.meth = 'ROC',
                                                 compress = 'xz',
                                                 build.clamping.mask = FALSE,
                                                 output.format = '.grd')
  
  #Build ensemble forecast
  biomodEF2050_R45 <- BIOMOD_EnsembleForecasting(EM.output = biomodEM,
                                                        projection.output = biomodProj2050_R45,
                                                        proj.name = paste(month, '_Ensemble_2050_R45', sep = ''))
  
  #Load ensemble forecast as raster
  #Divide by 1000 to convert probabilities to percentages
  ensemble_proj_raster_2050_R45 <- raster(file.path(species_name, paste('proj_', month, '_Ensemble_2050_R45', sep = ''), paste('proj_', month, '_Ensemble_2050_R45_', species_name, '_ensemble.grd', sep = ''))) %>%
    `/`(1000)
  
  crs(ensemble_proj_raster_2050_R45) <- '+init=epsg:4121 +proj=longlat +ellps=GRS80 +datum=GGRS87 +no_defs +towgs84=-199.87,74.79,246.62'
  
  #Save projection raster as data frame with xy coords and no NAs
  ensemble_proj_df_2050_R45 <- as.data.frame(ensemble_proj_raster_2050_R45, xy = TRUE, na.rm = TRUE)
  
  #Assign column names
  names(ensemble_proj_df_2050_R45) <- c('x', 'y', 'prob')
  
  #Plot map, probabilities, and sightings data
  ggplot() + 
    #Add probability data
    geom_tile(data = ensemble_proj_df_2050_R45, aes(x, y, fill = prob)) +
    #Add probabilitiy color gradient and label
    scale_fill_gradientn(colours = inferno(500), limits = c(0,1), name = 'Probability') +
    labs(x = "Longitude", 
         y = "Latitude", 
         title = paste(month_labs[[month]], "2050 R45 Ensemble")) +
    #Add world map data
    # geom_polygon(data = worldmap, aes(long, lat, group = group), fill = NA, colour = "gray43") +
    coord_quickmap(xlim = c(round(min(ensemble_proj_df_2050_R45$x)), round(max(ensemble_proj_df_2050_R45$x))), 
                   ylim = c(round(min(ensemble_proj_df_2050_R45$y)), round(max(ensemble_proj_df_2050_R45$y))),
                   expand = TRUE) +
    #Add latitude and longitude labels
    scale_y_continuous(breaks = lat_breaks, labels = lat_text, limits = NA) +
    scale_x_continuous(breaks = lon_breaks, labels = lon_text, limits = NA) +
    #Remove grid lines
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    #Save plot to hard drive
    ggsave(filename = file.path(fp_maps, paste(month, '_ensemble_2050_R45.jpeg', sep = '')), width = 7, height = 7)
  
  #---------------------- FORECAST ENSEMBLE MODEL FOR R45 FOR 2X CHLOROPHYLL ----------------------
  
  #Project all models
  biomodProj2050_R45_2x_chl <- BIOMOD_Projection(modeling.output = modelOut,
                                        new.env = proj_covars_2050_R45_2x_chl,
                                        proj.name = 'all',
                                        selected.models = 'all',
                                        binary.meth = 'ROC',
                                        compress = 'xz',
                                        build.clamping.mask = FALSE,
                                        output.format = '.grd')
  
  #Build ensemble forecast
  biomodEF2050_R45_2x_chl <- BIOMOD_EnsembleForecasting(EM.output = biomodEM,
                                             projection.output = biomodProj2050_R45_2x_chl,
                                             proj.name = paste(month, '_Ensemble_2050_R45_2x_chl', sep = ''))
  
  #Load ensemble forecast as raster
  #Divide by 1000 to convert probabilities to percentages
  ensemble_proj_raster_2050_R45_2x_chl <- raster(file.path(species_name, paste('proj_', month, '_Ensemble_2050_R45_2x_chl', sep = ''), 
                                                           paste('proj_', month, '_Ensemble_2050_R45_2x_chl_', species_name, '_ensemble.grd', sep = ''))) %>%
    `/`(1000)
  
  crs(ensemble_proj_raster_2050_R45_2x_chl) <- '+init=epsg:4121 +proj=longlat +ellps=GRS80 +datum=GGRS87 +no_defs +towgs84=-199.87,74.79,246.62'
  
  #Save projection raster as data frame with xy coords and no NAs
  ensemble_proj_df_2050_R45_2x_chl <- as.data.frame(ensemble_proj_raster_2050_R45_2x_chl, xy = TRUE, na.rm = TRUE)
  
  #Assign column names
  names(ensemble_proj_df_2050_R45_2x_chl) <- c('x', 'y', 'prob')
  
  #Plot map, probabilities, and sightings data
  ggplot() + 
    #Add probability data
    geom_tile(data = ensemble_proj_df_2050_R45_2x_chl, aes(x, y, fill = prob)) +
    #Add probabilitiy color gradient and label
    scale_fill_gradientn(colours = inferno(500), limits = c(0,1), name = 'Probability') +
    labs(x = "Longitude", 
         y = "Latitude", 
         title = paste(month_labs[[month]], "2050 R45 Ensemble 2x chlorophyll")) +
    #Add world map data
    # geom_polygon(data = worldmap, aes(long, lat, group = group), fill = NA, colour = "gray43") +
    coord_quickmap(xlim = c(round(min(ensemble_proj_df_2050_R45_2x_chl$x)), round(max(ensemble_proj_df_2050_R45_2x_chl$x))), 
                   ylim = c(round(min(ensemble_proj_df_2050_R45_2x_chl$y)), round(max(ensemble_proj_df_2050_R45_2x_chl$y))),
                   expand = TRUE) +
    #Add latitude and longitude labels
    scale_y_continuous(breaks = lat_breaks, labels = lat_text, limits = NA) +
    scale_x_continuous(breaks = lon_breaks, labels = lon_text, limits = NA) +
    #Remove grid lines
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    #Save plot to hard drive
    ggsave(filename = file.path(fp_maps, paste(month, '_ensemble_2050_R45_2x_chl.jpeg', sep = '')), width = 7, height = 7)
  
  #---------------------- FORECAST ENSEMBLE MODEL FOR R45 FOR HALF CHLOROPHYLL ----------------------
  
  #Project all models
  biomodProj2050_R45_half_chl <- BIOMOD_Projection(modeling.output = modelOut,
                                                 new.env = proj_covars_2050_R45_half_chl,
                                                 proj.name = 'all',
                                                 selected.models = 'all',
                                                 binary.meth = 'ROC',
                                                 compress = 'xz',
                                                 build.clamping.mask = FALSE,
                                                 output.format = '.grd')
  
  #Build ensemble forecast
  biomodEF2050_R45_half_chl <- BIOMOD_EnsembleForecasting(EM.output = biomodEM,
                                                        projection.output = biomodProj2050_R45_half_chl,
                                                        proj.name = paste(month, '_Ensemble_2050_R45_half_chl', sep = ''))
  
  #Load ensemble forecast as raster
  #Divide by 1000 to convert probabilities to percentages
  ensemble_proj_raster_2050_R45_half_chl <- raster(file.path(species_name, paste('proj_', month, '_Ensemble_2050_R45_half_chl', sep = ''), 
                                                             paste('proj_', month, '_Ensemble_2050_R45_half_chl_', species_name, '_ensemble.grd', sep = ''))) %>%
    `/`(1000)
  
  crs(ensemble_proj_raster_2050_R45_half_chl) <- '+init=epsg:4121 +proj=longlat +ellps=GRS80 +datum=GGRS87 +no_defs +towgs84=-199.87,74.79,246.62'
  
  #Save projection raster as data frame with xy coords and no NAs
  ensemble_proj_df_2050_R45_half_chl <- as.data.frame(ensemble_proj_raster_2050_R45_half_chl, xy = TRUE, na.rm = TRUE)
  
  #Assign column names
  names(ensemble_proj_df_2050_R45_half_chl) <- c('x', 'y', 'prob')
  
  #Plot map, probabilities, and sightings data
  ggplot() + 
    #Add probability data
    geom_tile(data = ensemble_proj_df_2050_R45_half_chl, aes(x, y, fill = prob)) +
    #Add probabilitiy color gradient and label
    scale_fill_gradientn(colours = inferno(500), limits = c(0,1), name = 'Probability') +
    labs(x = "Longitude", 
         y = "Latitude", 
         title = paste(month_labs[[month]], "2050 R45 Ensemble half chl")) +
    #Add world map data
    # geom_polygon(data = worldmap, aes(long, lat, group = group), fill = NA, colour = "gray43") +
    coord_quickmap(xlim = c(round(min(ensemble_proj_df_2050_R45_half_chl$x)), round(max(ensemble_proj_df_2050_R45_half_chl$x))), 
                   ylim = c(round(min(ensemble_proj_df_2050_R45_half_chl$y)), round(max(ensemble_proj_df_2050_R45_half_chl$y))),
                   expand = TRUE) +
    #Add latitude and longitude labels
    scale_y_continuous(breaks = lat_breaks, labels = lat_text, limits = NA) +
    scale_x_continuous(breaks = lon_breaks, labels = lon_text, limits = NA) +
    #Remove grid lines
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    #Save plot to hard drive
    ggsave(filename = file.path(fp_maps, paste(month, '_ensemble_2050_R45_half_chl.jpeg', sep = '')), width = 7, height = 7)
  
  #---------------------- FORECAST ENSEMBLE MODEL FOR R85 ----------------------
  
  #Project all models
  biomodProj2050_R85 <- BIOMOD_Projection(modeling.output = modelOut,
                                                 new.env = proj_covars_2050_R85,
                                                 proj.name = 'all',
                                                 selected.models = 'all',
                                                 binary.meth = 'ROC',
                                                 compress = 'xz',
                                                 build.clamping.mask = FALSE,
                                                 output.format = '.grd')
  
  #Build ensemble forecast
  biomodEF2050_R85 <- BIOMOD_EnsembleForecasting(EM.output = biomodEM,
                                                        projection.output = biomodProj2050_R85,
                                                        proj.name = paste(month, '_Ensemble_2050_R85', sep = ''))
  
  #Load ensemble forecast as raster
  #Divide by 1000 to convert probabilities to percentages
  ensemble_proj_raster_2050_R85 <- raster(file.path(species_name, paste('proj_', month, '_Ensemble_2050_R85', sep = ''), paste('proj_', month, '_Ensemble_2050_R85_', species_name, '_ensemble.grd', sep = ''))) %>%
    `/`(1000)
  
  crs(ensemble_proj_raster_2050_R85) <- '+init=epsg:4121 +proj=longlat +ellps=GRS80 +datum=GGRS87 +no_defs +towgs84=-199.87,74.79,246.62'
  
  #Save projection raster as data frame with xy coords and no NAs
  ensemble_proj_df_2050_R85 <- as.data.frame(ensemble_proj_raster_2050_R85, xy = TRUE, na.rm = TRUE)
  
  #Assign column names
  names(ensemble_proj_df_2050_R85) <- c('x', 'y', 'prob')
  
  #Plot map, probabilities, and sightings data
  ggplot() + 
    #Add probability data
    geom_tile(data = ensemble_proj_df_2050_R85, aes(x, y, fill = prob)) +
    #Add probabilitiy color gradient and label
    scale_fill_gradientn(colours = inferno(500), limits = c(0,1), name = 'Probability') +
    labs(x = "Longitude", 
         y = "Latitude", 
         title = paste(month_labs[[month]], "2050 R85 Ensemble")) +
    #Add world map data
    # geom_polygon(data = worldmap, aes(long, lat, group = group), fill = NA, colour = "gray43") +
    coord_quickmap(xlim = c(round(min(ensemble_proj_df_2050_R85$x)), round(max(ensemble_proj_df_2050_R85$x))), 
                   ylim = c(round(min(ensemble_proj_df_2050_R85$y)), round(max(ensemble_proj_df_2050_R85$y))),
                   expand = TRUE) +
    #Add latitude and longitude labels
    scale_y_continuous(breaks = lat_breaks, labels = lat_text, limits = NA) +
    scale_x_continuous(breaks = lon_breaks, labels = lon_text, limits = NA) +
    #Remove grid lines
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    #Save plot to hard drive
    ggsave(filename = file.path(fp_maps, paste(month, '_ensemble_2050_R85.jpeg', sep = '')), width = 7, height = 7)
  
  #---------------------- FORECAST ENSEMBLE MODEL FOR R85 FOR 2X CHLOROPHYLL ----------------------
  
  #Project all models
  biomodProj2050_R85_2x_chl <- BIOMOD_Projection(modeling.output = modelOut,
                                          new.env = proj_covars_2050_R85_2x_chl,
                                          proj.name = 'all',
                                          selected.models = 'all',
                                          binary.meth = 'ROC',
                                          compress = 'xz',
                                          build.clamping.mask = FALSE,
                                          output.format = '.grd')
  
  #Build ensemble forecast
  biomodEF2050_R85_2x_chl <- BIOMOD_EnsembleForecasting(EM.output = biomodEM,
                                                 projection.output = biomodProj2050_R85_2x_chl,
                                                 proj.name = paste(month, '_Ensemble_2050_R85_2x_chl', sep = ''))
  
  #Load ensemble forecast as raster
  #Divide by 1000 to convert probabilities to percentages
  ensemble_proj_raster_2050_R85_2x_chl <- raster(file.path(species_name, paste('proj_', month, '_Ensemble_2050_R85_2x_chl', sep = ''), 
                                                           paste('proj_', month, '_Ensemble_2050_R85_2x_chl_', species_name, '_ensemble.grd', sep = ''))) %>%
    `/`(1000)
  
  crs(ensemble_proj_raster_2050_R85_2x_chl) <- '+init=epsg:4121 +proj=longlat +ellps=GRS80 +datum=GGRS87 +no_defs +towgs84=-199.87,74.79,246.62'
  
  #Save projection raster as data frame with xy coords and no NAs
  ensemble_proj_df_2050_R85_2x_chl <- as.data.frame(ensemble_proj_raster_2050_R85_2x_chl, xy = TRUE, na.rm = TRUE)
  
  #Assign column names
  names(ensemble_proj_df_2050_R85_2x_chl) <- c('x', 'y', 'prob')
  
  #Plot map, probabilities, and sightings data
  ggplot() + 
    #Add probability data
    geom_tile(data = ensemble_proj_df_2050_R85_2x_chl, aes(x, y, fill = prob)) +
    #Add probabilitiy color gradient and label
    scale_fill_gradientn(colours = inferno(500), limits = c(0,1), name = 'Probability') +
    labs(x = "Longitude", 
         y = "Latitude", 
         title = paste(month_labs[[month]], "2050 R85 Ensemble 2x chl")) +
    #Add world map data
    # geom_polygon(data = worldmap, aes(long, lat, group = group), fill = NA, colour = "gray43") +
    coord_quickmap(xlim = c(round(min(ensemble_proj_df_2050_R85_2x_chl$x)), round(max(ensemble_proj_df_2050_R85_2x_chl$x))), 
                   ylim = c(round(min(ensemble_proj_df_2050_R85_2x_chl$y)), round(max(ensemble_proj_df_2050_R85_2x_chl$y))),
                   expand = TRUE) +
    #Add latitude and longitude labels
    scale_y_continuous(breaks = lat_breaks, labels = lat_text, limits = NA) +
    scale_x_continuous(breaks = lon_breaks, labels = lon_text, limits = NA) +
    #Remove grid lines
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    #Save plot to hard drive
    ggsave(filename = file.path(fp_maps, paste(month, '_ensemble_2050_R85_2x_chl.jpeg', sep = '')), width = 7, height = 7)
  
  #---------------------- FORECAST ENSEMBLE MODEL FOR R85 FOR HALF CHLOROPHYLL ----------------------
  
  #Project all models
  biomodProj2050_R85_half_chl <- BIOMOD_Projection(modeling.output = modelOut,
                                                 new.env = proj_covars_2050_R85_half_chl,
                                                 proj.name = 'all',
                                                 selected.models = 'all',
                                                 binary.meth = 'ROC',
                                                 compress = 'xz',
                                                 build.clamping.mask = FALSE,
                                                 output.format = '.grd')
  
  #Build ensemble forecast
  biomodEF2050_R85_half_chl <- BIOMOD_EnsembleForecasting(EM.output = biomodEM,
                                                        projection.output = biomodProj2050_R85_half_chl,
                                                        proj.name = paste(month, '_Ensemble_2050_R85_half_chl', sep = ''))
  
  #Load ensemble forecast as raster
  #Divide by 1000 to convert probabilities to percentages
  ensemble_proj_raster_2050_R85_half_chl <- raster(file.path(species_name, paste('proj_', month, '_Ensemble_2050_R85_half_chl', sep = ''), 
                                                             paste('proj_', month, '_Ensemble_2050_R85_half_chl_', species_name, '_ensemble.grd', sep = ''))) %>%
    `/`(1000)
  
  crs(ensemble_proj_raster_2050_R85_half_chl) <- '+init=epsg:4121 +proj=longlat +ellps=GRS80 +datum=GGRS87 +no_defs +towgs84=-199.87,74.79,246.62'
  
  #Save projection raster as data frame with xy coords and no NAs
  ensemble_proj_df_2050_R85_half_chl <- as.data.frame(ensemble_proj_raster_2050_R85_half_chl, xy = TRUE, na.rm = TRUE)
  
  #Assign column names
  names(ensemble_proj_df_2050_R85_half_chl) <- c('x', 'y', 'prob')
  
  #Plot map, probabilities, and sightings data
  ggplot() + 
    #Add probability data
    geom_tile(data = ensemble_proj_df_2050_R85_half_chl, aes(x, y, fill = prob)) +
    #Add probabilitiy color gradient and label
    scale_fill_gradientn(colours = inferno(500), limits = c(0,1), name = 'Probability') +
    labs(x = "Longitude", 
         y = "Latitude", 
         title = paste(month_labs[[month]], "2050 R85 Ensemble half chl")) +
    #Add world map data
    # geom_polygon(data = worldmap, aes(long, lat, group = group), fill = NA, colour = "gray43") +
    coord_quickmap(xlim = c(round(min(ensemble_proj_df_2050_R85_half_chl$x)), round(max(ensemble_proj_df_2050_R85_half_chl$x))), 
                   ylim = c(round(min(ensemble_proj_df_2050_R85_half_chl$y)), round(max(ensemble_proj_df_2050_R85_half_chl$y))),
                   expand = TRUE) +
    #Add latitude and longitude labels
    scale_y_continuous(breaks = lat_breaks, labels = lat_text, limits = NA) +
    scale_x_continuous(breaks = lon_breaks, labels = lon_text, limits = NA) +
    #Remove grid lines
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    #Save plot to hard drive
    ggsave(filename = file.path(fp_maps, paste(month, '_ensemble_2050_R85_half_chl.jpeg', sep = '')), width = 7, height = 7)
  
  #---------------------- FORECAST INDIVIDUAL MODELS FOR R45 ------------
  for (model in models) {
    
    #Create vector all runs of model algorithm for projection
    select_models <- c()
    for (i in 1:cv_folds) {
      select_models[i] <- paste0(species_name, "_AllData_RUN", i, "_", model)
    }
    
    #Create projection of single model
    #'@param modeling.output <BIOMOD.models.out> biomod models produced by BIOMOD_Modeling
    #'@param new.env <matrix, RasterStack, or dataframe> environemntal covariate layers upon which to project the models
    #'@param proj.name <char> name of projection
    #'@param selected.models <char or vector> models to project
    #'@param binary.meth <char or vector> evaluation method or methods used to compute models initially, same as input to BIOMOD_Modeling
    #'@param compress <boolean or char> format to compress projection when stored on hard disk
    #'@param build.clamping.mask <boolean> saves clamping mask on hard disk
    #'@param output.format <chr> specify format of output saved on hard disk, i.e. .grd is good because projection can be loaded back as Raster object
    biomodProj2050_R45 <- BIOMOD_Projection(modeling.output = modelOut,
                                                       new.env = proj_covars_2050_R45,
                                                       proj.name = paste(month, '_', model, '_onto_2050_R45', sep = ''),
                                                       selected.models = select_models,
                                                       binary.meth = 'ROC',
                                                       compress = 'xz',
                                                       build.clamping.mask = TRUE,
                                                       output.format = '.grd')
    
    #Load projection as a raster from .grd output saved on hard disk
    #Divide all values by 1000 to conver to 0-1.0 probabilities
    #Probabilities are computed in range 1-1000 to save memory
    #Modify extent to range of PA data
    #DOES THIS MESS ANYTHING UP?
    proj_raster_2050_R45 <- raster(file.path(species_name, paste('proj_', month, '_', model, '_onto_2050_R45',
                                                                 '/proj_', month, '_', model, '_onto_2050_R45_', species_name, '.grd', sep = ''))) %>%
      `/`(1000)
    
    crs(proj_raster_2050_R45) <- '+init=epsg:4121 +proj=longlat +ellps=GRS80 +datum=GGRS87 +no_defs +towgs84=-199.87,74.79,246.62'
    
    #Save projection raster as data frame with xy coords and no NAs
    proj_df_2050_R45 <- as.data.frame(proj_raster_2050_R45, xy = TRUE, na.rm = TRUE)
    
    #Assign column names
    names(proj_df_2050_R45) <- c('x', 'y', 'prob')
    
    #Latitude breaks
    lat_breaks <- seq(min(proj_df_2050_R45$y), max(proj_df_2050_R45$y) + 1, 2)
    #Latitude label text
    lat_text <- paste(abs(round(lat_breaks, 1)), '°N')
    
    #Longitude breaks
    lon_breaks <- seq(min(proj_df_2050_R45$x)-1, max(proj_df_2050_R45$x)+1, 4)
    #Longitude label text
    lon_text <- paste(abs(round(lon_breaks, 1)), '°W')
    
    #Plot map, probabilities, and sightings data
    ggplot() + 
      #Add probability data
      geom_tile(data = proj_df_2050_R45, aes(x, y, fill = prob)) +
      #Add probabilitiy color gradient and label
      scale_fill_gradientn(colours = inferno(500), limits = c(0,1), name = 'Probability') +
      labs(x = "Longitude", 
           y = "Latitude", 
           title = paste(month_labs[[month]], model, '2050 R45')) +
      #Set extent
      coord_quickmap(xlim = c(round(min(proj_df_2050_R45$x)), round(max(proj_df_2050_R45$x))), 
                     ylim = c(round(min(proj_df_2050_R45$y)), round(max(proj_df_2050_R45$y))),
                     expand = TRUE) +
      #Add latitude and longitude labels
      scale_y_continuous(breaks = lat_breaks, labels = lat_text, limits = NA) +
      scale_x_continuous(breaks = lon_breaks, labels = lon_text, limits = NA) +
      #Remove grid lines
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      #Save plot to hard drive
      ggsave(filename = file.path(fp_maps, paste(month, '_', model, '_2050_R45.jpeg', sep = '')), width = 7, height = 7)
    
  }
  #---------------------- FORECAST INDIVIDUAL MODELS FOR R45 FOR 2X CHLOROPHYLL-----------------
  for (model in models) {
    
    #Create vector all runs of model algorithm for projection
    select_models <- c()
    for (i in 1:cv_folds) {
      select_models[i] <- paste0(species_name, "_AllData_RUN", i, "_", model)
    }
    
    #Create projection of single model
    #'@param modeling.output <BIOMOD.models.out> biomod models produced by BIOMOD_Modeling
    #'@param new.env <matrix, RasterStack, or dataframe> environemntal covariate layers upon which to project the models
    #'@param proj.name <char> name of projection
    #'@param selected.models <char or vector> models to project
    #'@param binary.meth <char or vector> evaluation method or methods used to compute models initially, same as input to BIOMOD_Modeling
    #'@param compress <boolean or char> format to compress projection when stored on hard disk
    #'@param build.clamping.mask <boolean> saves clamping mask on hard disk
    #'@param output.format <chr> specify format of output saved on hard disk, i.e. .grd is good because projection can be loaded back as Raster object
    biomodProj2050_R45_2x_chl <- BIOMOD_Projection(modeling.output = modelOut,
                                          new.env = proj_covars_2050_R45_2x_chl,
                                          proj.name = paste(month, '_', model, '_onto_2050_R45_2x_chl', sep = ''),
                                          selected.models = select_models,
                                          binary.meth = 'ROC',
                                          compress = 'xz',
                                          build.clamping.mask = TRUE,
                                          output.format = '.grd')
    
    #Load projection as a raster from .grd output saved on hard disk
    #Divide all values by 1000 to conver to 0-1.0 probabilities
    #Probabilities are computed in range 1-1000 to save memory
    #Modify extent to range of PA data
    #DOES THIS MESS ANYTHING UP?
    proj_raster_2050_R45_2x_chl <- raster(file.path(species_name, paste('proj_', month, '_', model, '_onto_2050_R45_2x_chl',
                                                       '/proj_', month, '_', model, '_onto_2050_R45_2x_chl_', species_name, '.grd', sep = ''))) %>%
      `/`(1000)
    
    crs(proj_raster_2050_R45_2x_chl) <- '+init=epsg:4121 +proj=longlat +ellps=GRS80 +datum=GGRS87 +no_defs +towgs84=-199.87,74.79,246.62'
    
    #Save projection raster as data frame with xy coords and no NAs
    proj_df_2050_R45_2x_chl <- as.data.frame(proj_raster_2050_R45_2x_chl, xy = TRUE, na.rm = TRUE)
    
    #Assign column names
    names(proj_df_2050_R45_2x_chl) <- c('x', 'y', 'prob')
    
    #Latitude breaks
    lat_breaks <- seq(min(proj_df_2050_R45_2x_chl$y), max(proj_df_2050_R45_2x_chl$y) + 1, 2)
    #Latitude label text
    lat_text <- paste(abs(round(lat_breaks, 1)), '°N')
    
    #Longitude breaks
    lon_breaks <- seq(min(proj_df_2050_R45_2x_chl$x)-1, max(proj_df_2050_R45_2x_chl$x)+1, 4)
    #Longitude label text
    lon_text <- paste(abs(round(lon_breaks, 1)), '°W')
    
    #Plot map, probabilities, and sightings data
    ggplot() + 
      #Add probability data
      geom_tile(data = proj_df_2050_R45_2x_chl, aes(x, y, fill = prob)) +
      #Add probabilitiy color gradient and label
      scale_fill_gradientn(colours = inferno(500), limits = c(0,1), name = 'Probability') +
      labs(x = "Longitude", 
           y = "Latitude", 
           title = paste(month_labs[[month]], model, '2050 R45 2x chl')) +
      #Set extent
      coord_quickmap(xlim = c(round(min(proj_df_2050_R45_2x_chl$x)), round(max(proj_df_2050_R45_2x_chl$x))), 
                     ylim = c(round(min(proj_df_2050_R45_2x_chl$y)), round(max(proj_df_2050_R45_2x_chl$y))),
                     expand = TRUE) +
      #Add latitude and longitude labels
      scale_y_continuous(breaks = lat_breaks, labels = lat_text, limits = NA) +
      scale_x_continuous(breaks = lon_breaks, labels = lon_text, limits = NA) +
      #Remove grid lines
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      #Save plot to hard drive
      ggsave(filename = file.path(fp_maps, paste(month, '_', model, '_2050_R45_2x_chl.jpeg', sep = '')), width = 7, height = 7)
    
  }
  #---------------------- FORECAST INDIVIDUAL MODELS FOR R45 FOR HALF CHLOROPHYLL-----------------
  for (model in models) {
    
    #Create vector all runs of model algorithm for projection
    select_models <- c()
    for (i in 1:cv_folds) {
      select_models[i] <- paste0(species_name, "_AllData_RUN", i, "_", model)
    }
    
    #Create projection of single model
    #'@param modeling.output <BIOMOD.models.out> biomod models produced by BIOMOD_Modeling
    #'@param new.env <matrix, RasterStack, or dataframe> environemntal covariate layers upon which to project the models
    #'@param proj.name <char> name of projection
    #'@param selected.models <char or vector> models to project
    #'@param binary.meth <char or vector> evaluation method or methods used to compute models initially, same as input to BIOMOD_Modeling
    #'@param compress <boolean or char> format to compress projection when stored on hard disk
    #'@param build.clamping.mask <boolean> saves clamping mask on hard disk
    #'@param output.format <chr> specify format of output saved on hard disk, i.e. .grd is good because projection can be loaded back as Raster object
    biomodProj2050_R45_half_chl <- BIOMOD_Projection(modeling.output = modelOut,
                                            new.env = proj_covars_2050_R45_half_chl,
                                            proj.name = paste(month, '_', model, '_onto_2050_R45_half_chl', sep = ''),
                                            selected.models = select_models,
                                            binary.meth = 'ROC',
                                            compress = 'xz',
                                            build.clamping.mask = TRUE,
                                            output.format = '.grd')
    
    #Load projection as a raster from .grd output saved on hard disk
    #Divide all values by 1000 to conver to 0-1.0 probabilities
    #Probabilities are computed in range 1-1000 to save memory
    #Modify extent to range of PA data
    #DOES THIS MESS ANYTHING UP?
    proj_raster_2050_R45_half_chl <- raster(file.path(species_name, paste('proj_', month, '_', model, '_onto_2050_R45_half_chl',
                                                                 '/proj_', month, '_', model, '_onto_2050_R45_half_chl_', species_name, '.grd', sep = ''))) %>%
      `/`(1000)
    
    crs(proj_raster_2050_R45_half_chl) <- '+init=epsg:4121 +proj=longlat +ellps=GRS80 +datum=GGRS87 +no_defs +towgs84=-199.87,74.79,246.62'
    
    #Save projection raster as data frame with xy coords and no NAs
    proj_df_2050_R45_half_chl <- as.data.frame(proj_raster_2050_R45_half_chl, xy = TRUE, na.rm = TRUE)
    
    #Assign column names
    names(proj_df_2050_R45_half_chl) <- c('x', 'y', 'prob')
    
    #Latitude breaks
    lat_breaks <- seq(min(proj_df_2050_R45_half_chl$y), max(proj_df_2050_R45_half_chl$y) + 1, 2)
    #Latitude label text
    lat_text <- paste(abs(round(lat_breaks, 1)), '°N')
    
    #Longitude breaks
    lon_breaks <- seq(min(proj_df_2050_R45_half_chl$x)-1, max(proj_df_2050_R45_half_chl$x)+1, 4)
    #Longitude label text
    lon_text <- paste(abs(round(lon_breaks, 1)), '°W')
    
    #Plot map, probabilities, and sightings data
    ggplot() + 
      #Add probability data
      geom_tile(data = proj_df_2050_R45_half_chl, aes(x, y, fill = prob)) +
      #Add probabilitiy color gradient and label
      scale_fill_gradientn(colours = inferno(500), limits = c(0,1), name = 'Probability') +
      labs(x = "Longitude", 
           y = "Latitude", 
           title = paste(month_labs[[month]], model, '2050 R45 half chl')) +
      #Set extent
      coord_quickmap(xlim = c(round(min(proj_df_2050_R45_half_chl$x)), round(max(proj_df_2050_R45_half_chl$x))), 
                     ylim = c(round(min(proj_df_2050_R45_half_chl$y)), round(max(proj_df_2050_R45_half_chl$y))),
                     expand = TRUE) +
      #Add latitude and longitude labels
      scale_y_continuous(breaks = lat_breaks, labels = lat_text, limits = NA) +
      scale_x_continuous(breaks = lon_breaks, labels = lon_text, limits = NA) +
      #Remove grid lines
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      #Save plot to hard drive
      ggsave(filename = file.path(fp_maps, paste(month, '_', model, '_2050_R45_half_chl.jpeg', sep = '')), width = 7, height = 7)
    
  }
  #---------------------- FORECAST INDIVIDUAL MODELS FOR R85 ----------------------
  for (model in models) {
    
    #Create vector all runs of model algorithm for projection
    select_models <- c()
    for (i in 1:cv_folds) {
      select_models[i] <- paste0(species_name, "_AllData_RUN", i, "_", model)
    }
    
    #Create projection of single model
    #'@param modeling.output <BIOMOD.models.out> biomod models produced by BIOMOD_Modeling
    #'@param new.env <matrix, RasterStack, or dataframe> environemntal covariate layers upon which to project the models
    #'@param proj.name <char> name of projection
    #'@param selected.models <char or vector> models to project
    #'@param binary.meth <char or vector> evaluation method or methods used to compute models initially, same as input to BIOMOD_Modeling
    #'@param compress <boolean or char> format to compress projection when stored on hard disk
    #'@param build.clamping.mask <boolean> saves clamping mask on hard disk
    #'@param output.format <chr> specify format of output saved on hard disk, i.e. .grd is good because projection can be loaded back as Raster object
    biomodProj2050_R85 <- BIOMOD_Projection(modeling.output = modelOut,
                                            new.env = proj_covars_2050_R85,
                                            proj.name = paste(month, '_', model, '_onto_2050_R85', sep = ''),
                                            selected.models = select_models,
                                            binary.meth = 'ROC',
                                            compress = 'xz',
                                            build.clamping.mask = TRUE,
                                            output.format = '.grd')
    
    #Load projection as a raster from .grd output saved on hard disk
    #Divide all values by 1000 to conver to 0-1.0 probabilities
    #Probabilities are computed in range 1-1000 to save memory
    #Modify extent to range of PA data
    #DOES THIS MESS ANYTHING UP?
    proj_raster_2050_R85 <- raster(file.path(species_name, paste('proj_', month, '_', model, '_onto_2050_R85',
                                                                 '/proj_', month, '_', model, '_onto_2050_R85_', species_name, '.grd', sep = ''))) %>%
      `/`(1000)
    
    crs(proj_raster_2050_R85) <- '+init=epsg:4121 +proj=longlat +ellps=GRS80 +datum=GGRS87 +no_defs +towgs84=-199.87,74.79,246.62'
    
    #Save projection raster as data frame with xy coords and no NAs
    proj_df_2050_R85 <- as.data.frame(proj_raster_2050_R85, xy = TRUE, na.rm = TRUE)
    
    #Assign column names
    names(proj_df_2050_R85) <- c('x', 'y', 'prob')
    
    #Latitude breaks
    lat_breaks <- seq(min(proj_df_2050_R85$y), max(proj_df_2050_R85$y) + 1, 2)
    #Latitude label text
    lat_text <- paste(abs(round(lat_breaks, 1)), '°N')
    
    #Longitude breaks
    lon_breaks <- seq(min(proj_df_2050_R85$x)-1, max(proj_df_2050_R85$x)+1, 4)
    #Longitude label text
    lon_text <- paste(abs(round(lon_breaks, 1)), '°W')
    
    #Plot map, probabilities, and sightings data
    ggplot() + 
      #Add probability data
      geom_tile(data = proj_df_2050_R85, aes(x, y, fill = prob)) +
      #Add probabilitiy color gradient and label
      scale_fill_gradientn(colours = inferno(500), limits = c(0,1), name = 'Probability') +
      labs(x = "Longitude", 
           y = "Latitude", 
           title = paste(month_labs[[month]], model, '2050 R85')) +
      #Set extent
      coord_quickmap(xlim = c(round(min(proj_df_2050_R85$x)), round(max(proj_df_2050_R85$x))), 
                     ylim = c(round(min(proj_df_2050_R85$y)), round(max(proj_df_2050_R85$y))),
                     expand = TRUE) +
      #Add latitude and longitude labels
      scale_y_continuous(breaks = lat_breaks, labels = lat_text, limits = NA) +
      scale_x_continuous(breaks = lon_breaks, labels = lon_text, limits = NA) +
      #Remove grid lines
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      #Save plot to hard drive
      ggsave(filename = file.path(fp_maps, paste(month, '_', model, '_2050_R85.jpeg', sep = '')), width = 7, height = 7)
    
  }
  #---------------------- FORECAST INDIVIDUAL MODELS FOR R85 FOR 2X CHLOROPHYLL ----------------------
  for (model in models) {
    
    #Create vector all runs of model algorithm for projection
    select_models <- c()
    for (i in 1:cv_folds) {
      select_models[i] <- paste0(species_name, "_AllData_RUN", i, "_", model)
    }
    
    #Create projection of single model
    #'@param modeling.output <BIOMOD.models.out> biomod models produced by BIOMOD_Modeling
    #'@param new.env <matrix, RasterStack, or dataframe> environemntal covariate layers upon which to project the models
    #'@param proj.name <char> name of projection
    #'@param selected.models <char or vector> models to project
    #'@param binary.meth <char or vector> evaluation method or methods used to compute models initially, same as input to BIOMOD_Modeling
    #'@param compress <boolean or char> format to compress projection when stored on hard disk
    #'@param build.clamping.mask <boolean> saves clamping mask on hard disk
    #'@param output.format <chr> specify format of output saved on hard disk, i.e. .grd is good because projection can be loaded back as Raster object
    biomodProj2050_R85_2x_chl <- BIOMOD_Projection(modeling.output = modelOut,
                                            new.env = proj_covars_2050_R85_2x_chl,
                                            proj.name = paste(month, '_', model, '_onto_2050_R85_2x_chl', sep = ''),
                                            selected.models = select_models,
                                            binary.meth = 'ROC',
                                            compress = 'xz',
                                            build.clamping.mask = TRUE,
                                            output.format = '.grd')
    
    #Load projection as a raster from .grd output saved on hard disk
    #Divide all values by 1000 to conver to 0-1.0 probabilities
    #Probabilities are computed in range 1-1000 to save memory
    #Modify extent to range of PA data
    #DOES THIS MESS ANYTHING UP?
    proj_raster_2050_R85_2x_chl <- raster(file.path(species_name, paste('proj_', month, '_', model, '_onto_2050_R85_2x_chl',
                                                                 '/proj_', month, '_', model, '_onto_2050_R85_2x_chl_', species_name, '.grd', sep = ''))) %>%
      `/`(1000)
    
    crs(proj_raster_2050_R85_2x_chl) <- '+init=epsg:4121 +proj=longlat +ellps=GRS80 +datum=GGRS87 +no_defs +towgs84=-199.87,74.79,246.62'
    
    #Save projection raster as data frame with xy coords and no NAs
    proj_df_2050_R85_2x_chl <- as.data.frame(proj_raster_2050_R85_2x_chl, xy = TRUE, na.rm = TRUE)
    
    #Assign column names
    names(proj_df_2050_R85_2x_chl) <- c('x', 'y', 'prob')
    
    #Latitude breaks
    lat_breaks <- seq(min(proj_df_2050_R85_2x_chl$y), max(proj_df_2050_R85_2x_chl$y) + 1, 2)
    #Latitude label text
    lat_text <- paste(abs(round(lat_breaks, 1)), '°N')
    
    #Longitude breaks
    lon_breaks <- seq(min(proj_df_2050_R85_2x_chl$x)-1, max(proj_df_2050_R85_2x_chl$x)+1, 4)
    #Longitude label text
    lon_text <- paste(abs(round(lon_breaks, 1)), '°W')
    
    #Plot map, probabilities, and sightings data
    ggplot() + 
      #Add probability data
      geom_tile(data = proj_df_2050_R85_2x_chl, aes(x, y, fill = prob)) +
      #Add probabilitiy color gradient and label
      scale_fill_gradientn(colours = inferno(500), limits = c(0,1), name = 'Probability') +
      labs(x = "Longitude", 
           y = "Latitude", 
           title = paste(month_labs[[month]], model, '2050 R85')) +
      #Set extent
      coord_quickmap(xlim = c(round(min(proj_df_2050_R85_2x_chl$x)), round(max(proj_df_2050_R85_2x_chl$x))), 
                     ylim = c(round(min(proj_df_2050_R85_2x_chl$y)), round(max(proj_df_2050_R85_2x_chl$y))),
                     expand = TRUE) +
      #Add latitude and longitude labels
      scale_y_continuous(breaks = lat_breaks, labels = lat_text, limits = NA) +
      scale_x_continuous(breaks = lon_breaks, labels = lon_text, limits = NA) +
      #Remove grid lines
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      #Save plot to hard drive
      ggsave(filename = file.path(fp_maps, paste(month, '_', model, '_2050_R85_2x_chl.jpeg', sep = '')), width = 7, height = 7)
    
  }
  #---------------------- FORECAST INDIVIDUAL MODELS FOR R85 FOR HALF CHLOROPHYLL ----------------------
  for (model in models) {
    
    #Create vector all runs of model algorithm for projection
    select_models <- c()
    for (i in 1:cv_folds) {
      select_models[i] <- paste0(species_name, "_AllData_RUN", i, "_", model)
    }
    
    #Create projection of single model
    #'@param modeling.output <BIOMOD.models.out> biomod models produced by BIOMOD_Modeling
    #'@param new.env <matrix, RasterStack, or dataframe> environemntal covariate layers upon which to project the models
    #'@param proj.name <char> name of projection
    #'@param selected.models <char or vector> models to project
    #'@param binary.meth <char or vector> evaluation method or methods used to compute models initially, same as input to BIOMOD_Modeling
    #'@param compress <boolean or char> format to compress projection when stored on hard disk
    #'@param build.clamping.mask <boolean> saves clamping mask on hard disk
    #'@param output.format <chr> specify format of output saved on hard disk, i.e. .grd is good because projection can be loaded back as Raster object
    biomodProj2050_R85_half_chl <- BIOMOD_Projection(modeling.output = modelOut,
                                            new.env = proj_covars_2050_R85_half_chl,
                                            proj.name = paste(month, '_', model, '_onto_2050_R85_half_chl', sep = ''),
                                            selected.models = select_models,
                                            binary.meth = 'ROC',
                                            compress = 'xz',
                                            build.clamping.mask = TRUE,
                                            output.format = '.grd')
    
    #Load projection as a raster from .grd output saved on hard disk
    #Divide all values by 1000 to conver to 0-1.0 probabilities
    #Probabilities are computed in range 1-1000 to save memory
    #Modify extent to range of PA data
    proj_raster_2050_R85_half_chl <- raster(file.path(species_name, paste('proj_', month, '_', model, '_onto_2050_R85_half_chl',
                                                                 '/proj_', month, '_', model, '_onto_2050_R85_half_chl_', species_name, '.grd', sep = ''))) %>%
      `/`(1000)
    
    crs(proj_raster_2050_R85_half_chl) <- '+init=epsg:4121 +proj=longlat +ellps=GRS80 +datum=GGRS87 +no_defs +towgs84=-199.87,74.79,246.62'
    
    #Save projection raster as data frame with xy coords and no NAs
    proj_df_2050_R85_half_chl <- as.data.frame(proj_raster_2050_R85_half_chl, xy = TRUE, na.rm = TRUE)
    
    #Assign column names
    names(proj_df_2050_R85_half_chl) <- c('x', 'y', 'prob')
    
    #Latitude breaks
    lat_breaks <- seq(min(proj_df_2050_R85_half_chl$y), max(proj_df_2050_R85_half_chl$y) + 1, 2)
    #Latitude label text
    lat_text <- paste(abs(round(lat_breaks, 1)), '°N')
    
    #Longitude breaks
    lon_breaks <- seq(min(proj_df_2050_R85_half_chl$x)-1, max(proj_df_2050_R85_half_chl$x)+1, 4)
    #Longitude label text
    lon_text <- paste(abs(round(lon_breaks, 1)), '°W')
    
    #Plot map, probabilities, and sightings data
    ggplot() + 
      #Add probability data
      geom_tile(data = proj_df_2050_R85_half_chl, aes(x, y, fill = prob)) +
      #Add probabilitiy color gradient and label
      scale_fill_gradientn(colours = inferno(500), limits = c(0,1), name = 'Probability') +
      labs(x = "Longitude", 
           y = "Latitude", 
           title = paste(month_labs[[month]], model, '2050 R85')) +
      #Set extent
      coord_quickmap(xlim = c(round(min(proj_df_2050_R85_half_chl$x)), round(max(proj_df_2050_R85_half_chl$x))), 
                     ylim = c(round(min(proj_df_2050_R85_half_chl$y)), round(max(proj_df_2050_R85_half_chl$y))),
                     expand = TRUE) +
      #Add latitude and longitude labels
      scale_y_continuous(breaks = lat_breaks, labels = lat_text, limits = NA) +
      scale_x_continuous(breaks = lon_breaks, labels = lon_text, limits = NA) +
      #Remove grid lines
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      #Save plot to hard drive
      ggsave(filename = file.path(fp_maps, paste(month, '_', model, '_2050_R85_half_chl.jpeg', sep = '')), width = 7, height = 7)
    
  }
}

#---------------------- END OF FILE ----------------------

