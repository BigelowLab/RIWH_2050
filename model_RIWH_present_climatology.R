#Camille Ross
#October 2019
#Colby College
#Purpose: Function to create monthly forecasts of right whales for 2050

#Load libraries
library(biomod2) #Modelling package
library(dplyr) #Useful functions for use with data frames (i.e. mutate, filter, etc.) & piping (%>%)
library(RColorBrewer) #Color pallette creation 
library(viridis) #Pre-made color pallettes
library(readr) #allows reading and writing of CSV files
library(ggplot2) #Plots
library(lubridate) #Allows for datetime manipulation
library(Hmisc) #Allows monthday conversions

#Set working directory
DIR = '~/Desktop/Thesis/Bigelow/right_whale'
setwd(dir = DIR)

#Create model filepath
dir.create(file.path(DIR, 'RIWH_pres_proj_climatology_all'), showWarnings = FALSE)
#Create and set model working directory
fp_model <- file.path(DIR, 'RIWH_pres_proj_climatology_all')

file.copy(from = file.path(DIR, 'maxent.jar'), to = fp_model, overwrite = TRUE)

#Create filepath to presence/absense data
fp_pa <- file.path(DIR, 'pa_data')

fp_R45_btm_temp_pres <- file.path(DIR, 'trn_lyrs/DFO_2050/R45_btm/T_pres')
fp_R85_btm_temp_pres <- file.path(DIR, 'trn_lyrs/DFO_2050/R85_btm/T_pres')
fp_R45_btm_temp_2050 <- file.path(DIR, 'trn_lyrs/DFO_2050/R45_btm/T_2050')

fp_R45_btm_sal_pres <- file.path(DIR, 'trn_lyrs/DFO_2050/R45_btm/S_pres')
fp_R85_btm_sal_pres <- file.path(DIR, 'trn_lyrs/DFO_2050/R85_btm/S_pres')

fp_R45_sfce_temp_pres <- file.path(DIR, 'trn_lyrs/DFO_2050/R45_sfce/T_pres')
fp_R85_sfce_temp_pres <- file.path(DIR, 'trn_lyrs/DFO_2050/R85_sfce/T_pres')

fp_R45_sfce_sal_pres <- file.path(DIR, 'trn_lyrs/DFO_2050/R45_sfce/S_pres')
fp_R85_sfce_sal_pres <- file.path(DIR, 'trn_lyrs/DFO_2050/R85_sfce/S_pres')

fp_cal_pres <- file.path(DIR, 'trn_lyrs/JiCalanus')
fp_cal_2050 <- file.path(DIR, 'trn_lyrs/JiCalanus2050')

#Reset working directory to store output in model filepath
setwd(fp_model)

#Load in map data centered over the Atlantic Ocean
#Used to plot habitat maps
worldmap <- ggplot2::map_data('world')

#Bathymetry
fp_bat <- file.path(DIR, 'trn_lyrs/bathy/gom')
#DFO layers
fp_dfo <- file.path(DIR, 'trn_lyrs/DFO_DAILY')

#Create directory to store results (evals, variable importance, plots)
dir.create(file.path(fp_model, 'results'), showWarnings = FALSE)
#Create filepath to results
fp_results <- file.path(fp_model, 'results')

#Load sightings data
pa_data <- readr::read_csv(file.path(fp_pa, 'RIWH_present_more_background.csv')) %>%
  #Log transform bathymetry
  mutate(bat = log(abs(as.numeric(bat))),
         #Add binary presence/absence column (p = 1, a = 0)
         pa = if_else(is.na(SPECCODE), 0, 1))

#Initialize species_name, character string containing name of species (no spaces)
species_name <- 'RIWH'

#Initialize models, list holding names of models to run
#Run ?BIOMOD_ModelingOptions for names of possible models
models <- c('GAM', 'GBM', 'MAXENT.Phillips', 'ANN')

#Configure individual model parameters
#Specify number of trees, interaction depth (tree complexity), 
#shrinkage (learning rate), and bagging fraction for GBM (BRT) model
modelOptions <- BIOMOD_ModelingOptions(GAM = list(k = -1),
                                       GBM = list(n.trees = 1000,
                                                  interaction.depth = 9,
                                                  shrinkage = 0.01,
                                                  bag.fraction = 0.5))



for (month in 1:12) {
  #Specify year
  #Loop starts with 2004 and increase to 2013
  
  #Read in training data
  #Filter data to include every year except testing year
  #Add binary presence/absence variable
  trainingData <- pa_data %>%
    filter(MONTH == month) %>%
    mutate(pa = if_else(is.na(SPECCODE), 0, 1))
  
  if (nrow(trainingData) == 0) {next}
  if (length(unique(trainingData$pa)) != 2) {print('STOP'); next}
  
  print(paste('Training data:', unique(trainingData$MONTH)))
  
  #Isolate binary presence/absence data
  trainingPA <- as.data.frame(trainingData[,'pa'])
  #Isolate presence/absence coordiantes
  trainingXY <- trainingData[,c('longrd', 'latgrd')]
  #Isolate environmental covariates
  trainingCovars <- as.data.frame(trainingData[,c('sst', 'sst_50', 'sal', 'sal_50', 'cal', 'bat')])
  
  
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
  
  #Run models using formatted data, specified models, and model options
  #' @param data <BIOMOD.formated.data> formatted data for biomod model returned by BIOMOD_FormatingData
  #' @param models <vector> contains model names, see ?BIOMOD_Modeling for choices
  #' @param models.options <BIOMOD.models.options> modeling options returned by BIOMOD_ModelingOptions
  #' @param NbRunEval <int> number of evaluations to run
  #' @param DataSplit <int> percentage of data to for model calibration*
  #' @param VarImport <int> number of permutations to use in estimating variable importance
  #' @param models.eval.meth <vector> evaluation methods to use, see ?BIOMOD_Modeling for choices
  #' @param SaveObj <boolean> save modeling object to hard drive
  #' @param do.full.models <boolean> calibrates and evaluates models with whole dataset
  #' @param modeling.id <char> name of model
  #' *Note on DataSplit: biomod2 splits the training data into training and testing, and then using the additional testing data provided for evaluation
  #' We decided not to split the training data, i.e. DataSplit = 100, and use the provided testing data instead
  modelOut <- BIOMOD_Modeling(biomodData,
                              models = models,
                              models.options = modelOptions,
                              NbRunEval = 3,
                              DataSplit = 80,
                              Prevalence = 0.5,
                              VarImport = 3,
                              models.eval.meth = c('ROC', 'TSS'),
                              SaveObj = TRUE,
                              rescal.all.models = FALSE,
                              do.full.models = FALSE,
                              modeling.id = paste(species_name, '_', month, sep = ''))
  
  #Create directory to evals
  dir.create(file.path(fp_results, 'evals'), showWarnings = FALSE)
  #Create variable with reference to plots subdirectory
  fp_evals <- file.path(fp_results, 'evals')
  
  #Retrieves model evaluations
  #'@param obj <BIOMOD.models.out> model produced using BIOMOD_Modeling
  #'@param as.data.frame <boolean> if TRUE, function returns evaluations as a dataframe
  modelEvals <- get_evaluations(obj = modelOut, as.data.frame = TRUE)
  #Saves model evaluations to a csv file in the results directory
  #Allows for easy analysis
  write.csv(modelEvals, file = file.path(fp_evals, paste('evals_', month, '.csv', sep = '')), row.names = TRUE)
  
  #Create directory to variable importance
  dir.create(file.path(fp_results, 'var_importance'), showWarnings = FALSE)
  #Create variable with reference to plots subdirectory
  fp_var <- file.path(fp_results, 'var_importance')
  
  #Retrieves variable contribution
  #'@param obj <BIOMOD.models.out> model produced using BIOMOD_Modeling
  #'@param as.data.frame <boolean> if TRUE, function returns variable contributions as a dataframe
  varImportance <- get_variables_importance(obj = modelOut, as.data.frame = TRUE)
  #Saves model evaluations to a csv file in the results directory
  #Allows for easy analysis
  write.csv(varImportance, file = file.path(fp_var, paste('var_importance_', month, '.csv', sep = '')), row.names = TRUE)
  
  
  #Create an ensemble model using the models produced by BIOMOD_Modeling
  #'@param modeling.output <BIOMOD.models.out> models produced using BIOMOD_Modeling
  #'@param chosen.models <char or vector> specifies which models to use in the ensemble; 'all' or subselection of model names, i.e. c('GBM', 'GAM')
  #'@param em.by <char> defines how the models will be combined, see ?BIOMOD_EnsembleModeling for choices
  #'@param eval.metric <vector> evaluation methods to use when building ensemble model
  #'@param eval.metric.quality.threshold <vector> evaluation metric score below which models will be discarded, must be same length as eval.metric
  #'@param prob.mean <boolean> estimate mean probabilities across predictions
  #'@param prob.cv <boolean> estimate coefficient of variation across predictions
  #'@param prob.ci <boolean> estimate confidence interval around probability mean
  #'@param prob.median <boolean> estimate median of probabilities across predictions
  #'@param committee.averaging <boolean> estimate the committee averaging across predictions
  #'@param prob.mean.weight <boolean> estimate weighted sum of probabilities
  #'@param prob.mean.weight.decay <char> defines the relative importance of the weights, ex. proportional means weights are proportional to the evaluation scores
  # ensembleModel <- BIOMOD_EnsembleModeling(modeling.output = modelOut,
  #                                          chosen.models = 'all',
  #                                          em.by = 'all',
  #                                          eval.metric = c('ROC'),
  #                                          eval.metric.quality.threshold = c(0.4),
  #                                          prob.mean = TRUE,
  #                                          prob.cv = FALSE,
  #                                          prob.ci = TRUE,
  #                                          prob.ci.alpha = 0.05,
  #                                          prob.median = TRUE,
  #                                          committee.averaging = TRUE,
  #                                          prob.mean.weight = TRUE, prob.mean.weight.decay = 'proportional')
  
  #Retrieves ensemble model evaluations
  #'@param obj <BIOMOD.EnsembleModeling.out> model produced using BIOMOD_EnsembleModeling
  #'@param as.data.frame <boolean> if TRUE, function returns evaluations as a dataframe
  #' modelEvalsEM <- get_evaluations(ensembleModel, as.data.frame = TRUE)
  #' #Saves ensemble model evaluations to a csv file in the results directory
  #' #Allows for easy analysis
  #' write.csv(modelEvalsEM, file = file.path(fp_evals, paste('evals_EM_', yr, '_', szn,'.csv', sep = '')), row.names = TRUE)
  #' 
  #' #Retrieves variable contribution
  #' #'@param obj <BIOMOD.EnsembleModeling.out> model produced using BIOMOD_EnsembleModeling
  #' #'@param as.data.frame <boolean> if TRUE, function returns variable contributions as a dataframe
  #' varImportanceEM <- get_variables_importance(ensembleModel, as.data.frame = TRUE)
  #' #Saves variable importance to a csv file in the results directory
  #' #Allows for easy analysis
  #' write.csv(varImportanceEM, file = file.path(fp_var, paste('var_importance_EM_', yr, '_', szn, '.csv', sep = '')), row.names = TRUE)
  #' 
  #Create directory to store response curves
  dir.create(file.path(fp_results, 'response_curves'), showWarnings = FALSE)
  #Create variable with reference to plots subdirectory
  fp_response <- file.path(fp_results, 'response_curves')
  
  #Plot and save response curves for each model to hard disk
  for (model in models) {
    #Open .png file, specifying filepath, filename, and dimensions in inches
    png(file.path(fp_response, paste('response_curve_', month, '_', model, '.png', sep = '')), width = 4, height = 4, units = "in", res = 480)
    #Plot response curve of selected model
    #'@param models <char vector> list of model names loaded in by BIOMOD_LoadModels, specifying the selected model
    #'@param Data <dataframe> containe explanatory variables for which response curve should be plotted, retrieve expl.var using get_formal_data
    #'@param show.variables <vector> column names for explanatory variables, retrieve using get_formal_data
    #'@param do.bivariate <boolean> if FALSE, 2D individual variable response curves are plotted, instead of 3D response curves with all variables at once
    #'@param col <vector> colors for variables, must be same length as Data and show.variables
    #'@param legend <boolean> displays color legend
    #'@param main <chr> title of plot
    #'@param data_species <chr> name of species, retrieve using get_formal_data
    response.plot2(models = BIOMOD_LoadModels(modelOut, models = model),
                   Data = get_formal_data(modelOut, 'expl.var'), 
                   show.variables= get_formal_data(modelOut, 'expl.var.names'),
                   do.bivariate = FALSE,
                   fixed.var.metric = 'median',
                   col = c("blue", "green", "black"),
                   legend = TRUE,
                   main = paste(model, "Response Curves"),
                   save.file = '.jpeg',
                   name = paste('response_curve_', month, '_', model, sep = ''),
                   data_species = get_formal_data(modelOut,'resp.var'))
    #Turn off the device, i.e. close the .png
    dev.off()
  }
  
  #Create directory to store plots
  dir.create(file.path(fp_results, 'habitat_maps'), showWarnings = FALSE)
  #Create variable with reference to plots subdirectory
  fp_plots <- file.path(fp_results, 'habitat_maps')
  #Create directory to store plots
  dir.create(file.path(fp_results, 'habitat_maps_with_sightings'), showWarnings = FALSE)
  #Create variable with reference to plots subdirectory
  fp_plots_sightings <- file.path(fp_results, 'habitat_maps_with_sightings')
  
  
  #Vector holding labels for months
  month_labs <- c('January', 'February', 'March',
                  'April', 'May', 'June',
                  'July', 'August', 'September',
                  'October', 'November', 'December')
  
  #Latitude breaks
  lat_breaks <- seq(min(pa_data$latgrd), max(pa_data$latgrd), 1)
  #Latitude label text
  lat_text <- paste(abs(round(lat_breaks, 1)), '°N')
  
  #Longitude breaks
  lon_breaks <- seq(min(pa_data$longrd), max(pa_data$longrd), 2)
  #Longitude label text
  lon_text <- paste(abs(round(lon_breaks, 1)), '°W')
  
  sst <- raster(file.path(fp_R45_sfce_temp_pres, paste('tsfce_pres_R45_', month, '.grd', sep = '')))
  sst_50 <- raster(file.path(fp_R45_btm_temp_pres, paste('tbtm_pres_R45_', month, '.grd', sep = ''))) %>%
    raster::resample(sst)
  sal <- raster(file.path(fp_R45_sfce_sal_pres, paste('ssfce_pres_R45_', month, '.grd', sep = ''))) %>%
    raster::resample(sst)
  sal_50 <- raster(file.path(fp_R45_btm_sal_pres, paste('sbtm_pres_R45_', month, '.grd', sep = ''))) %>%
    raster::resample(sst)
  bat <- raster(file.path(fp_bat, 'BathymetryGoMFlipped.grd')) %>%
    raster::resample(sst)
  cal <- raster(file.path(fp_cal_pres, paste('calanus_', month, '.grd', sep = ''))) %>%
    raster::resample(sst)
  
  proj_covars <- stack(sst, sst_50, sal, sal_50, cal, bat)
  
  
  #Assign variable names
  #par <- photosynthetically active radiation
  #sst <- sea surface temperature
  #cal <- calanus abundance
  names(proj_covars) <- c('sst', 'sst_50', 'sal', 'sal_50', 'cal', 'bat')
  
  #Take the log of bathymetry
  #proj_covars$bat <- log(abs(proj_covars$bat))
  
  #Filter presence data by season
  #Select lat and long of each sighting
  
  presences <- trainingData %>%
    filter(pa == 1) %>%
    dplyr::select(latgrd, longrd)
  
  #Project single models
  for (model in models) {
    
    #Create projection of single model
    #'@param modeling.output <BIOMOD.models.out> biomod models produced by BIOMOD_Modeling
    #'@param new.env <matrix, RasterStack, or dataframe> environemntal covariate layers upon which to project the models
    #'@param proj.name <char> name of projection
    #'@param selected.models <char or vector> models to project
    #'@param binary.meth <char or vector> evaluation method or methods used to compute models initially, same as input to BIOMOD_Modeling
    #'@param compress <boolean or char> format to compress projection when stored on hard disk
    #'@param build.clamping.mask <boolean> saves clamping mask on hard disk
    #'@param output.format <chr> specify format of output saved on hard disk, i.e. .grd is good because projection can be loaded back as Raster object
    biomodProjFuture <- BIOMOD_Projection(modeling.output = modelOut,
                                          new.env = proj_covars,
                                          proj.name = paste(month, '_', model, '_onto_pres', sep = ''),
                                          selected.models = paste(species_name, '_AllData_RUN3_', model, sep = ''),
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
      `/`(1000) #%>%
    # setExtent(extent(min(pa_data$longrd),
    #                  max(pa_data$longrd),
    #                  min(pa_data$latgrd),
    #                  max(pa_data$latgrd)))
    
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
           title = paste(month_labs[[month]], model)) +
      #Add world map data
      geom_polygon(data = worldmap, aes(long, lat, group = group), fill = "gray43", colour = "gray43") +
      coord_quickmap(xlim = c(round(min(proj_df$x)),round(max(proj_df$x))), 
                     ylim = c(round(min(proj_df$y)),round(max(proj_df$y))),
                     expand = TRUE) +
      #Add latitude and longitude labels
      scale_y_continuous(breaks = lat_breaks, labels = lat_text, limits = NA) +
      scale_x_continuous(breaks = lon_breaks, labels = lon_text, limits = NA) +
      #Add sightings data
      geom_point(data = presences, aes(longrd, latgrd), pch = 21, cex = 1, color = 'black', fill = 'white')
    #Save plot to hard drive
    ggsave(filename = file.path(fp_plots_sightings, paste(month, '_', model, '_habitat_map_onto_pres.jpeg', sep = '')), width = 7, height = 7)
    
    ggplot() + 
      #Add probability data
      geom_tile(data = proj_df, aes(x, y, fill = prob)) +
      #Add probabilitiy color gradient and label
      scale_fill_gradientn(colours = inferno(500), limits = c(0,1), name = 'Probability') +
      labs(x = "Longitude", 
           y = "Latitude", 
           title = paste(month_labs[[month]], model)) +
      #Add world map data
      geom_polygon(data = worldmap, aes(long, lat, group = group), fill = "gray43", colour = "gray43") +
      coord_quickmap(xlim = c(round(min(proj_df$x)),round(max(proj_df$x))), 
                     ylim = c(round(min(proj_df$y)),round(max(proj_df$y))),
                     expand = TRUE) +
      #Add latitude and longitude labels
      scale_y_continuous(breaks = lat_breaks, labels = lat_text, limits = NA) +
      scale_x_continuous(breaks = lon_breaks, labels = lon_text, limits = NA)
    #Save plot to hard drive
    ggsave(filename = file.path(fp_plots, paste(month, '_', model, '_habitat_map_onto_pres.jpeg', sep = '')), width = 7, height = 7)
  }
}
