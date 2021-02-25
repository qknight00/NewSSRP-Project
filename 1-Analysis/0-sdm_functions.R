# These functions are sourced in order to build SDMs in other files. 
# We put them here so that they don't clog up the script where more serious
# decisions are made

# Here's a function I'm going to specify that preps occurrence data for feeding
# sdm. Let's get this out of the way here:
# This function takes occurrence lonlat dataframe: data.frame(x,y) where x = lon
# and y = lat and a stack of the predictor rasters and spits out the sdm stack 
# and the results from the vifstep function which weeds out collinear variables
sdmdataPrep <- function(pres_lonlat.df,
                        abs_lonlat.df,
                        predictor.stack, 
                        removeCollinearity = T){
  # These are the predictor values at locations of species presence
  presvals <- raster::extract(predictor.stack, pres_lonlat.df)
  
  # predictor values at random locations
  absvals <- raster::extract(predictor.stack, abs_lonlat.df)
  
  # We know that probability of presence is 1 for areas where the species
  # was found, and we assume it's 0 for the random background points
  Y <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
  sdmdata.df <- data.frame(cbind(Y, rbind(presvals, absvals)))
  
  
  if(removeCollinearity == T){
    # There are more quantitative assessments of collinearity (like using the Variance Inlation Factor)
    # vifstep() from the usdm package uses VIF to identify the most collinear predictors
    # From the vifstep() documentation:
    # "vifstep calculate VIF for all variables, exclude one with highest VIF (greater than threshold), 
    #repeat the procedure until no variables with VIF greater than th remains."
    vif.out <- vifstep(dplyr::select(sdmdata.df, -Y), th=10)
    
    # Make new stack of uncorrelated predictors and extract presence and absence values again
    predictor_uncorr.stack <- dropLayer(predictor.stack, vif.out@excluded)
    
    presvals <- raster::extract(predictor_uncorr.stack, pres_lonlat.df)
    absvals <- raster::extract(predictor_uncorr.stack, abs_lonlat.df)
    Y <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
    sdmdata.df <- data.frame(cbind(Y, rbind(presvals, absvals)))
    
    sdmdata <- sdmData(Y ~ ., train = sdmdata.df)
    
    return(list(sdmdata = sdmdata, vif.results = vif.out, uncorr.stack = predictor_uncorr.stack))
    
  } else {
    
    sdmdata <- sdmData(Y ~ ., train = sdmdata.df)
    return(list(sdmdata = sdmdata))
  }
}

# This is for plotting the predictions. We make it here so we can make changes
# and the changes will be conistent between each plot
prediction_plot <- function(sdm_prediction.grid, plotTitle){
  # Data prep for geom_raster() and geom_sf()
  names(sdm_prediction.grid) <- "layer"
  sdm_prediction.df <- sdm_prediction.grid %>%
    as("SpatialPixelsDataFrame") %>%
    as.data.frame() %>% rename(value = 'layer')
  
  ca.sf <- st_as_sf(ca.shp_wgs84)
  
  prediction.plot <- ggplot() + geom_raster(data = sdm_prediction.df, 
                                            aes(x,y, fill = value, alpha = 1)) +
    scale_fill_gradient2(name = paste("Habitat Suitability"), 
                         limits = c(0,1),
                         low = 'white', mid = 'navyblue', high = 'yellow', midpoint = .50) +
    coord_equal() +
    geom_sf(data = ca.sf, aes(), size = .1, fill = NA) +
    xlim(c(min(sdm_prediction.df$x),max(sdm_prediction.df$x))) +
    ylim(c(min(sdm_prediction.df$y),max(sdm_prediction.df$y))) +
    guides(alpha=FALSE) + theme_minimal() + labs(x = "", y = "")
  return(prediction.plot)
}
