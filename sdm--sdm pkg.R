# Here we build an ensemble SDM based on weislander sagebrush and old climate data.
# using the sdm package

# This loads a ton of data into memory so it we have to delete all the variables
# between parts. Each Part is independent and can be run separately!

# We're not using true absences here, fix this!

#install.packages("dismo")
#install.packages("raster")
library("raster")
library("sdm")
library("sp")
library("rgdal")
library(dismo)
library(usdm)
library(dplyr)



# This is a CRS where the coordinates are in longitude and latitude
wgs84.crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

ca.shp <- readOGR("Data/CA_Counties/")
ca.shp_wgs84 <- spTransform(ca.shp, wgs84.crs) 

## First we'll build an SDM on historic climate and weislander dist and then 
# project it forward

# Here's a function I'm going to specify that preps occurrence data for feeding
# sdm. Let's get this out of the way here:
# This function takes occurrence lonlat dataframe: data.frame(x,y) where x = lon
# and y = lat and a stack of the predictor rasters and spits out the sdm stack 
# and the results from the vifstep function which weeds out collinear variables
sdmdataPrep <- function(pres_lonlat.df,
                        abs_lonlat.df,
                        predictor.stack, 
                        test_pres.df = NA,
                        test_abs.df = NA,
                        removeCollinearity = T){
  # These are the predictor values at locations of species presence
  presvals <- raster::extract(predictor.stack, pres_lonlat.df)
  
  # predictor values at random locations
  absvals <- raster::extract(predictor.stack, abs_lonlat.df)
  
  # We know that probability of presence is 1 for areas where the species
  # was found, and we assume it's 0 for the random background points
  Y <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
  sdmdata.df <- data.frame(cbind(Y, rbind(presvals, absvals)))
  
  test_presvals <- raster::extract(predictor.stack, test_pres.df)
  test_absvals <- raster::extract(predictor.stack, test_abs.df)
  Y <- c(rep(1, nrow(test_presvals)), rep(0, nrow(test_absvals)))
  test_sdmdata.df <- data.frame(cbind(Y, rbind(test_presvals, test_absvals)))
  
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
    
    test_presvals <- raster::extract(predictor_uncorr.stack, test_pres.df)
    test_absvals <- raster::extract(predictor_uncorr.stack, test_abs.df)
    Y <- c(rep(1, nrow(test_presvals)), rep(0, nrow(test_absvals)))
    test_sdmdata.df <- data.frame(cbind(Y, rbind(test_presvals, test_absvals)))
    
    if(!is.na(test_pres.df)) {
      sdmdata <- sdmData(Y ~ ., train = sdmdata.df, test = test_sdmdata.df)
    } else{
      sdmdata <- sdmData(Y ~ ., train = sdmdata.df)
    }
    
    return(list(sdmdata = sdmdata, vif.results = vif.out, uncorr.stack = predictor_uncorr.stack))
    
  } else {
    
    if(!is.na(test_pres.df)) {
      sdmdata <- sdmData(Y ~ ., train = sdmdata.df, test = test_sdmdata.df)
    } else{
      sdmdata <- sdmData(Y ~ ., train = sdmdata.df)
    }
    return(sdmdata)
  }
}


#################### PART I ###########################################
# Make SDM from historical data (weislander) with historical climate (prism)
# and project them to current time using current climate (worldclim)

#pulling and plotting worldclim data 
#worldclim is the current climate data 
# we put it in a brick so that R sees this as multiple rasters, instead of 
# 1 raster with multiple bands. i'm not sure if it makes a difference but I know 
# how to work with raster stacks/bricks better than raster bands

# historical PRISM Data
prism.stack <- brick("Data/PRISM/1920-1940/Bioclimatic19 (1km)/prism2040_bioclim19.grd")

# uhhh let's crop climate data to all of CA
prism.stack <- crop(prism.stack, extent(ca.shp_wgs84))

weislander.shp <- readOGR("Data/Weislander_Sage/")
weis.shp_wgs84 <- spTransform(weislander.shp, wgs84.crs) 
totalweislander.shp <- readOGR("Data/WeislanderVeg")
total_weis.shp_wgs84 <- spTransform(totalweislander.shp, wgs84.crs) 


#pulling presence and absence lat/lon data for weislander dataset
N = 400

pres_weis.pts <- spsample(weis.shp_wgs84, N, type = 'random')
pres_weis.df <- data.frame(pres_weis.pts@coords)

# Now to make absence points we sample everything sampled by weislander and then
# remove stuff that is found within the sagebrush subset
abs_weis.pts_backr <- spsample(total_weis.shp_wgs84, N, type = 'random')
abs_weis.df <- dplyr::setdiff(data.frame(abs_weis.pts_backr@coords), 
                               data.frame(abs_weis.pts_backr[weis.shp_wgs84]@coords))


# Let's make some test data
test_N = 300
pres_weis_test.pts <- spsample(weis.shp_wgs84, test_N, type = 'random')
pres_weis_test.df <- data.frame(pres_weis_test.pts@coords)

abs_weis_test.pts_backr <- spsample(total_weis.shp_wgs84, N, type = 'random')
abs_weis_test.df <- dplyr::setdiff(data.frame(abs_weis_test.pts_backr@coords), 
                              data.frame(abs_weis_test.pts_backr[weis.shp_wgs84]@coords))


histo_dataprep.results <- sdmdataPrep(pres_lonlat.df = pres_weis.df, 
                                     abs_lonlat.df = abs_weis.df,
                                     predictor.stack = prism.stack, 
                                     test_pres.df = pres_weis_test.df,
                                     test_abs.df = abs_weis_test.df,
                                     removeCollinearity = T)
# view output of collinearity analysis:
histo_dataprep.results$vif.results

histo_sdmdata <- histo_dataprep.results$sdmdata
prism_uncorr.stack <- histo_dataprep.results$uncorr.stack



# ENSEMBLE let's make an ensemble model of all of them
# a number of methods can be used, see documentation, but they include 
# weighted mean, unweighted mean, median, entropy, etc

# We can't run sdm with x and y in sdmdata so we need to remove that from the formula
predictor.names <- histo_sdmdata@features.name
histo_sdmFormula = reformulate(predictor.names, response="Y")

histo_sdm <- sdm::sdm(histo_sdmFormula, data = histo_sdmdata, methods=c("glm","gam","rf"))

# This ensemble is a prediction, it is no longer an SDM anymore
histo_ensemble <- sdm::ensemble(histo_sdm, prism_uncorr.stack, 
                              setting=list(method="weighted", stat="TSS"))

# Let's plot the SDM ensemble prediction
sp::plot(histo_ensemble, main = "Historic Ensemble SDM")
sp::plot(ca.shp_wgs84, add = T)
#sp::plot(weis.shp_wgs84, add =T)
#legend("bottomright", legend = "Weislander SageBrush Occ.", pch = 16, cex=.6)

getVarImp(histo_sdm)


## PROJECT TO PRESENT DAY ###
## Now let's read in worldclim data and project the ensemble to present day, 
# and compare it with CALVEG occ

prism_1995_2015.stack <- raster::stack("Data/PRISM/1995-2015/Bioclimatic19 (1km)/prism9515_bioclim19.grd")
prism_1995_2015.stack <- crop(prism_1995_2015.stack, extent(ca.shp_wgs84))
calveg.shp <- readOGR("Data/CALVEG_Sage/Total_Sage.shp")
calveg.shp_wgs84 <- spTransform(calveg.shp, wgs84.crs)

sdm_current.prediction <- sdm::ensemble(histo_sdm, prism_1995_2015.stack, 
                              setting=list(method="weighted", stat="TSS"))

# Let's plot the SDM ensemble prediction
sp::plot(sdm_current.prediction, main = "Historic --> Present Ensemble SDM")
sp::plot(ca.shp_wgs84, add = T)
#sp::plot(calveg.shp_wgs84, add =T)
#legend("bottomright", legend = "CALVEG SageBrush Occ.", pch = 16, cex=.6)


# Let's put the two previous figures into one:
par(mfrow=c(1,2))
sp::plot(histo_ensemble, main = "Historic Ensemble SDM")
sp::plot(sdm_current.prediction, main = "Historic --> Present Ensemble SDM")

par(mfrow=c(1,1))
prediction_dif <- sdm_current.prediction - histo_ensemble
sp::plot(prediction_dif, main = "Change in Habitat Suitability (1930s-2010s")
sp::plot(ca.shp_wgs84, add = T)


## PROJECT TO FUTURE ###

# Layer's for Future Projections
cmip5_2050.stack <- raster::stack(list.files("Data/CMIP5/cmip5/2_5m", full.names = T))
cmip5_2050.stack <- crop(cmip5_2050.stack, ca.shp_wgs84)

sdm_future.prediction <- sdm::ensemble(histo_sdm, cmip5_2050.stack, 
                                       setting=list(method="weighted", stat="TSS"))

# Let's plot the SDM ensemble prediction into the future
sp::plot(sdm_future.prediction, main = "Historic --> Future Ensemble SDM")
sp::plot(ca.shp_wgs84, add = T)


# compare the future to the historic
par(mfrow=c(1,2))
sp::plot(sdm_current.prediction, main = "Historic --> Present Ensemble SDM")
sp::plot(sdm_future.prediction, main = "Historic --> Future Ensemble SDM")

# huh


# Cool, I think the work here is to change the plot settings to make the sdm prediction
# and the occurrence both pretty visible


######## END PART I ########################



# These delete all the unneeded variables to free up space in memory
dont_delete_these <- c("wgs84.crs", "ca.shp_wgs84", "sdmdataPrep")
delete_these <- ls()[!(ls() %in% dont_delete_these)]
rm(list = delete_these)
.rs.restartR()


#################### PART II ###########################################
## Look at role of human impacts on calveg in present time
# We'll make an SDM using current occurrences (calveg), current climate (worldclim),
# and current human impact

worldclim.stack <- raster::stack(
  list.files("Data/WorldClim2-5/", pattern = "*.bil", full.names = T))
worldclim.stack <- crop(worldclim.stack, extent(ca.shp_wgs84))

calveg.shp <- readOGR("Data/CALVEG_Sage/Total_Sage.shp")
calveg.shp_wgs84 <- spTransform(calveg.shp, wgs84.crs)

#pulling presence and absence lat/lon data for calveglander dataset
N = 400
pres_calveg.pts <- spsample(calveg.shp_wgs84, N, type = 'random')
pres_calveg.df <- data.frame(pres_calveg.pts@coords)

# Now to make absence points we sample everything sampled by calveglander and then
# remove stuff that is found within the sagebrush subset
# going to assume here ( i think it's pretty close) that all of CA is sampled
# by CALVEG
abs_calveg.pts_backr <- spsample(ca.shp_wgs84, N, type = 'random')
abs_calveg.df <- dplyr::setdiff(data.frame(abs_calveg.pts_backr@coords), 
                              data.frame(abs_calveg.pts_backr[calveg.shp_wgs84]@coords))

# Let's make some test data
test_N = 400
pres_calveg_test.pts <- spsample(calveg.shp_wgs84, test_N, type = 'random')
pres_calveg_test.df <- data.frame(pres_calveg_test.pts@coords)

abs_calveg_test.pts_backr <- spsample(ca.shp_wgs84, N, type = 'random')
abs_calveg_test.df <- dplyr::setdiff(data.frame(abs_calveg_test.pts_backr@coords), 
                                   data.frame(abs_calveg_test.pts_backr[calveg.shp_wgs84]@coords))

# Let's read in human impact raster and add it it to stack of predictors
hf.stack_raw <- raster("Data/HumanFootprint/2009/HFP2009.tif")
hf.stack_smaller <- crop(hf.stack_raw, c(-1.1e7, -.7e7, 2e6, 6e6))
hf.stack_wgs84 <- projectRaster(hf.stack_smaller, crs = wgs84.crs)

# Need to resample because the raster's have different origins
hf.stack <- resample(hf.stack_wgs84, worldclim.stack)

worldclim_hf.stack <- addLayer(worldclim.stack, hf.stack)

curr.dataprep.results <- sdmdataPrep(pres_lonlat.df = pres_calveg.df, 
                                     abs_lonlat.df = abs_calveg.df,
                                     predictor.stack = worldclim_hf.stack, 
                                     test_pres.df = pres_calveg_test.df,
                                     test_abs.df = abs_calveg_test.df,
                                     removeCollinearity = T)
# view output of collinearity analysis:
curr.dataprep.results$vif.results

curr_sdmdata <- curr.dataprep.results$sdmdata
worldclim_hf_uncorr.stack <- curr.dataprep.results$uncorr.stack


# ENSEMBLE let's make an ensemble model of all of them
# a number of methods can be used, see documentation, but they include 
# weighted mean, unweighted mean, median, entropy, etc

# We can't run sdm with x and y in sdmdata so we need to remove that from the formula
curr_predictor.names <- curr_sdmdata@features.name
curr_sdmFormula = reformulate(curr_predictor.names, response="Y")

curr_sdm <- sdm::sdm(curr_sdmFormula, data = curr_sdmdata, methods=c("glm","gam","rf"))

# This ensemble is a prediction, it is no longer an SDM anymore
curr_ensemble <- sdm::ensemble(curr_sdm, worldclim_hf_uncorr.stack, 
                              setting=list(method="weighted", stat="TSS"))

# Let's plot the SDM ensemble prediction
sp::plot(curr_ensemble, main = "Current Ensemble SDM")
sp::plot(ca.shp_wgs84, add = T)

getVarImp(curr_sdm)

# Huh, through this method it kinda looks like human impacts don't define the
# the current distribution very much



######### END PART II ###################