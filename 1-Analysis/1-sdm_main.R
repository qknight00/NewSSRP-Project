# Here we build an ensemble SDM based on weislander sagebrush and old climate data.
# using the sdm package

# This loads a ton of data into memory so it we have to delete all the variables
# between parts. Each Part is independent and can be run separately!

# In Part 1 we build the SDM on historic occurrences and historic climate data
# we then predict the sdm onto the historic landscape 
# Then we predict it to the current time period by feeding the sdm current climate
# data. 
# Lastly we try to predict the sdm into the future using future climate data

# In Part 2 we add human impacts to the SDM and find out if the model is improved
# and to what extent human impacts affect CSS distribution

# You /should/ be able to run each subsection independently as long as everything 
# before "PART 1" is run. If a variable is missing it should be pretty easy to find
# Where the variable is declared earlier on and to go back and read it in


library("raster")
library("sdm")
library("sp")
library("rgdal")
library(dismo)
library(usdm)
library(tidyverse)
library(RColorBrewer)
library(patchwork)
# Avery needs the following lines to load the sf package, but don't worry about them
# udunits_dir <- file.path(Sys.getenv("HOME"), "udunits")
# dyn.load(paste0(udunits_dir, "/local/lib/libudunits2.so.0"))
library(sf)



# This is a CRS where the coordinates are in longitude and latitude
wgs84.crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

ca.shp <- readOGR("Data/CA_Counties")
ca.shp_wgs84 <- spTransform(ca.shp, wgs84.crs) 

# This is where all the functions live that we'll be using to do SDM
# Shouldn't have to edit these
source("Scripts/1-Analysis/0-sdm_functions.R")

## First we'll build an SDM on historic climate and weislander dist and then 
# project it forward

#################### PART I ###########################################
# Make SDM from historical data (weislander) with historical climate (prism)
# and project them to current time using current climate (prism)


# prism is the current climate data 
# we put it in a brick so that R sees this as multiple rasters, instead of 
# 1 raster with multiple bands. i'm not sure if it makes a difference but I know 
# how to work with raster stacks/bricks better than raster bands


#  Read in and manipulate istorical PRISM Data
prism.stack <- brick("Data/PRISM/1920-1940/Bioclimatic19 (1km)/prism2040_bioclim19.grd")

# Here we convert layer names to names we're familiar with:
p.names <- gsub("layer.", "bio", names(prism.stack))
bioclim_dic <- read_csv("Data/bioclim_dictionary.csv")
proper.names <- tibble(BIO_ID=p.names) %>% 
  left_join(bioclim_dic, by = "BIO_ID") %>% 
  pull(FULL_NAME)
  
names(prism.stack) <- proper.names
prism.stack <- projectRaster(prism.stack, crs = wgs84.crs)
# uhhh let's crop climate data to all of CA
prism.stack <- crop(prism.stack, extent(ca.shp_wgs84))

# Read in the historical occurrences
totalweislander.shp <- readOGR("Data/WeislanderVeg")
total_weis.shp_wgs84 <- spTransform(totalweislander.shp, wgs84.crs) 


# Pull presence and absence lat/lon data from weislander dataset
N = 1000 # Sample size

# Make a shapefile of all Sagebrush and then sample N points from it
pres.shp <- total_weis.shp_wgs84[total_weis.shp_wgs84$WHR1 == "CSC",]
pres_weis.pts <- spsample(pres.shp, N, type = 'random')
pres_weis.df <- data.frame(pres_weis.pts@coords)

# Now to make absence points we subset a new shapefile that doesn't have
# Barren areas, lacustrine areas, or sagebrush. These are known, 
# biologically relevant absences
abs.shp <- total_weis.shp_wgs84[!(total_weis.shp_wgs84$WHR1 %in% c("BAR", "LAC", "CSC")),]

# Sample N amount of points from the "absence" shapefile we just made
abs_weis.pts <- spsample(abs.shp, N, type = 'random')
abs_weis.df <- data.frame(abs_weis.pts)

# This homemade function makes organizes the data to feed it to the sdm algorithm
histo_dataprep.results <- sdmdataPrep(pres_lonlat.df = pres_weis.df, 
                                      abs_lonlat.df = abs_weis.df,
                                      predictor.stack = prism.stack,
                                      removeCollinearity = T)


# View output of collinearity analysis:
histo_dataprep.results$vif.results

histo_sdmdata <- histo_dataprep.results$sdmdata

# These are all the climate variables that are not Collinear-- the ones which we will
# use the rest of the time
prism_uncorr.stack <- histo_dataprep.results$uncorr.stack


## PREDICT TO HISTORIC TIME PERIOD (SAME AS WHEN SDM BUILT) ###
# ENSEMBLE let's make an ensemble model of all of them
# a number of methods can be used, see documentation, but they include 
# weighted mean, unweighted mean, median, entropy, etc

# We can't run sdm with x and y in sdmdata so we need to remove that from the formula
predictor.names <- histo_sdmdata@features.name
histo_sdmFormula <- reformulate(predictor.names, response="Y")

# to overcome rf error use:
# sdm::installAll()

# Build the SDM (this will take a long time)
histo_sdm <- sdm::sdm(histo_sdmFormula, data = histo_sdmdata, methods=c("glm","gam","rf"),
                      replication='cv',cv.folds = 10)
histo_testResults <- getEvaluation(histo_sdm)

# This ensemble is a prediction, it is no longer an SDM anymore
histo_ensemble <- sdm::ensemble(histo_sdm, prism_uncorr.stack,
                                filename = "Results/historic_predictionN1000Ens.tif",
                                setting=list(method="weighted", stat="TSS"),
                                overwrite =TRUE)

# You can read it from the disk with the following line so that you don't have to 
# run the ensemble stuff above:
histo_ensemble <- raster("Results/historic_predictionN1000Ens.tif")

# Let's plot the SDM ensemble prediction
historic_pred.plot <- prediction_plot(sdm_prediction.grid = histo_ensemble) +
  theme(legend.position="bottom") + 
  labs(title = "Historic CSS SDM",
          subtitle = "GAM, GLM, RF Ensemble, 10-fold Cross-Validation",
          caption = paste0("AUC = ", histo_testResults$AUC %>% mean() %>% round(2),
                           ", TSS = ", histo_testResults$TSS %>% mean() %>% round(2),
                           ", Deviance = ", histo_testResults$Deviance %>% mean() %>% round(2)))


# Let's plot the SDM ensemble prediction with the weislander occurences overlaid
weis_sage.sf <- st_as_sf(pres.shp)
hist_w.occ_pred.plot <- historic_pred.plot + 
  geom_sf(data = weis_sage.sf, color = "black", size = .05, alpha = .5) +
  labs(color = "Weislander Occurrences", title = "with Weislander Occurrences")

# Barplot of the variable importance
hist_varimp.plot <- plot(getVarImp(histo_sdm))

# We use the patchwork package to put all 3 plots into one plot like so
hist_composite.plot <- historic_pred.plot + (hist_w.occ_pred.plot / hist_varimp.plot)

# Save the plot
ggsave("Figures/hist_2-8.png", plot = hist_composite.plot, 
       width = 20, height = 20, limitsize = F)






## PREDICT TO PRESENT DAY ###
## Now let's read in prism data and project the ensemble to present day, 
# and compare it with CALVEG occ

prism_1995_2015.stack <- raster::stack("Data/PRISM/1995-2015/Bioclimatic19 (1km)/prism9515_bioclim19.grd")

# Here we convert layer names to names we're familiar with:
p.names <- gsub("layer.", "bio", names(prism_1995_2015.stack))
bioclim_dic <- read_csv("Data/bioclim_dictionary.csv")
proper.names <- tibble(BIO_ID=p.names) %>% 
  left_join(bioclim_dic, by = "BIO_ID") %>% 
  pull(FULL_NAME)
names(prism_1995_2015.stack) <- proper.names

prism_1995_2015.stack <- projectRaster(prism_1995_2015.stack, crs = wgs84.crs)
prism_1995_2015.stack <- crop(prism_1995_2015.stack, extent(ca.shp_wgs84))

sdm_current.prediction <- sdm::ensemble(histo_sdm, prism_1995_2015.stack,
                                        filename = "Results/current_predictionN1000Ens.tif",
                                        setting=list(method="weighted", stat="TSS"))

sdm_current.prediction <- raster("Results/current_predictionN1000Ens.tif")

# Let's plot the SDM ensemble prediction

# Read in current occurences of sagebrush for plotting (CALVEG)
calveg.sf <- st_read("Data/CALVEG_Sage/Total_Sage.shp")

current_pred.plot <- prediction_plot(sdm_prediction.grid = sdm_current.prediction) +
  labs(title = "Current Ensemble SDM",
       subtitle = "GAM, GLM, RF Ensemble, 10-fold Cross-Validation") + 
  theme(legend.position="bottom", guides = F)
  

current_pred_wOcc.plot <- current_pred.plot + 
  labs(title = "with CALVEG Occurences") +
  geom_sf(data = calveg.sf, color=ggplot2::alpha("black",0.1), fill=ggplot2::alpha("black",0.1))

current_composite.plot <- current_pred.plot + current_pred_wOcc.plot
ggsave("Figures/current_2-8.png", plot = current_composite.plot,
       width = 20, height = 20, limitsize = F)



## PROJECT TO FUTURE ### !!!!! THIS SECTION NEEDS WORK !!!!!!!!
# Future Climate Data comes from https://www.dropbox.com/sh/xli9t0hqdyqjgu9/AACOPQnTDYTskR9PGDP816C_a?dl=0
# We use Representative Concentration Pathway 6, which is a conservative business as 
# usual CO2 emission

# Layer's for Future Projections
# We have to list all files in the directory, pull out the files that only end in .bil, 
# and the read those in as a raster stack
cmip_files <- list.files("Data/CMIP5_Future/Future (RCPs - 2080-2100)/bio_baseline_Modern(1950-1999)CCSM_rcp60(2080-)/", full.names = T)
cmip_files <- cmip_files[grep("bil$", cmip_files)]

cmip5_2050.stack <- raster::stack(cmip_files)

# Crop to size
cmip5_2050.stack <- crop(cmip5_2050.stack, ca.shp_wgs84)
proper.names <- tibble(BIO_ID=names(cmip5_2050.stack)) %>% 
  left_join(bioclim_dic, by = "BIO_ID") %>% 
  pull(FULL_NAME)


names(cmip5_2050.stack) <- proper.names
cmip5_2050Resample.stack <- resample(cmip5_2050.stack, prism_uncorr.stack)

# Take a look a some of the layers between prism and cmip....they look pretty 
# frickin different.....
par(mfrow=c(2,1))
sp::plot(prism_uncorr.stack$Precipitation.of.Warmest.Quarter)
sp::plot(cmip5_2050.stack$Precipitation.of.Warmest.Quarter)

# Have to remove file if we want to add a new one (cause I think this package is 
# kinda broken)
prediction.file <- "Results/future_predictionN1000Ens.tif"
if(file.exists(prediction.file)) file.remove(prediction.file)
sdm_future.prediction <- sdm::ensemble(histo_sdm, cmip5_2050Resample.stack,
                                       filename = "Results/future_predictionN1000Ens.tif",
                                       setting=list(method="weighted", stat="TSS"))


future_pred.plot <- prediction_plot(sdm_prediction.grid = sdm_future.prediction) + 
  labs(title = "Historic --> Future Ensemble SDM")

# Just running the following line plots it
future_pred.plot

all_times.plot <- historic_pred.plot + current_pred.plot + future_pred.plot

# This saves it
ggsave("Figures/future_2-8.png", plot = all_times.plot,
       width = 20, height = 20, limitsize = F)


######## END PART I ########################







#################### PART II ###########################################
## Look at role of human impacts on calveg in present time
# We'll make an SDM using current occurrences (calveg), current climate (PRISM),
# and current human impact.
# This will look very similar to the beginning of part 1, except we include human
# Impact
prism_1995_2015.stack <- raster::stack("Data/PRISM/1995-2015/Bioclimatic19 (1km)/prism9515_bioclim19.grd")
prism_1995_2015.stack <- crop(prism_1995_2015.stack, extent(ca.shp_wgs84))
names(prism_1995_2015.stack) <- tibble(BIO_ID=names(prism_1995_2015.stack)) %>% 
  left_join(bioclim_dic, by = "BIO_ID") %>% 
  pull(FULL_NAME)


calveg.shp <- readOGR("Data/CALVEG/SouthCentral/")
calveg.shp_wgs84 <- spTransform(calveg.shp, wgs84.crs)

# pulling presence and absence lat/lon data for calveg dataset
N = 1000

pres.shp <- calveg.shp_wgs84[calveg.shp_wgs84$CWHR_TYPE == "CSC",]
pres_calveg.pts <- spsample(pres.shp, N, type = 'random')
pres_calveg.df <- data.frame(pres_calveg.pts@coords)

# Now to make absence points we subset a new shapefile that doesn't have
# Barren areas, lacustrine areas, or sagebrush. These are known, 
# biologically relevant absences
abs.shp <- calveg.shp_wgs84[!(calveg.shp_wgs84$CWHR_TYPE %in% c("BAR", "LAC", "CSC")),]

abs_calveg.pts <- spsample(abs.shp, N, type = 'random')
abs_calveg.df <- data.frame(abs_calveg.pts)


# Let's read in human impact raster and add it it to stack of predictors

## !!!!! THIS SECTION NEEDS WORK !!!!!!!!
# What needs to happen here is a good human footprint layer must be read in
# the CRS must be changed to the same as the PRISM data we use
# The human footprint must be Resampled so that it's the same resolution as the 
# other layers (using raster::resample() function)
# And then it needs to be added to the stack of prism layers
# using addLayer()

# Most of the following code will ahve to be changed, but it's left here
# to demonstrate some of the functions that need to be used (all are used in
# completion in PART 1:

hf.stack_raw <- raster("Data/HumanFootprint/2009/HFP2009.tif")

hf.stack <- raster::resample(hf.stack_wgs84, prism_1995_2015.stack)

prism_hf.stack <- addLayer(prism_1995_2015.stack, hf.stack)

# Feed in the new human footprint + climate stack
# (just change the predictor.stack I think)
curr.dataprep.results <- sdmdataPrep(pres_lonlat.df = pres_calveg.df, 
                                     abs_lonlat.df = abs_calveg.df,
                                     predictor.stack = prism_hf.stack, 
                                     removeCollinearity = T)

# Now build an SDM with these SDM prepped data and compare it to the 
# Original SDM we built above (by looking at the figures that have already been made)

# You should pretty much be able to reuse all code that was written in the first section 
# of PART 1