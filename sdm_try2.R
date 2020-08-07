# In this file we read in historic PRISM (bioclim) variables and current 
# bioclim variables and project past distributions on current distributions
library(raster)
library(dismo)

wgs84.crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

# prism bioclim data produced in makingBioclimVariables.R
prism_var <- raster::stack("Data/PRISM/bioclim/")
ca.data <- spTransform(readOGR("Data/CA_Counties"), wgs84.crs) # for extent

# !!! NEED TO READ IN CONTEMPORARY CLIMATE HERE !!!



#some code for making the models 
#model with all 19 var and weislander data 

weislander.data <- readOGR("Data/Weislander_Sage")
summary(weislander.data)
#pulling lat/lon data for weislander dataset
# looked at the difference that increasing number of points would make 
N = 300
M = 500
O = 1000
weislander.data_wgs84 <- spTransform(weislander.data, wgs84.crs) 
weislander.pts <- spsample(weislander.data_wgs84, N, type = 'random')
weislander.pts@coords
weislander.lon <- weislander.pts@coords[,'x']
weislander.lat <- weislander.pts@coords[,'y']
summary(weislander.pts)

model_weislander_prism <- bioclim(prism_var,
                                  p = weislander.pts)
predict_weislander_prism <- predict(object = model_weislander_prism,
                                    x = prism_var,
                                    ext = extent(ca.data))
sp::plot(predict_weislander_prism, main = "Wieslander and 19 Prism Var")

#model with the 11 variables chosen 

prism11 <- dropLayer(prism_var, c(4,5,6,7,13,16,17,19))
nlayers(prism11)
model_weis_11 <- bioclim(prism11,
                         p = weislander.pts)
predict_weis_11 <- predict(object = model_weis_11,
                           x = prism11,
                           ext = extent(ca.data))
sp::plot(predict_weis_11,
         xlab = "Longtiude",
         ylab = "Latitude")
?predict

#projecting into the current with bioclim data 

predict_current <- predict(object = model_weis_11,
                           x = bioclim.data,
                           ext = extent(ca.data))
plot(predict_current)

predict_future <- predict(object = model_weis_11,
                          x = cmip5_11.data,
                          ext = extent(ca.data))
model_weis_11 <- bioclim(prism11,
                         p = weislander.pts)
predict_weis_11 <- predict(object = model_weis_11,
                           x = prism11,
                           ext = extent(ca.data))
plot(predict_weis_11,
     xlab = "Longtiude",
     ylab = "Latitude")
?predict

#projecting into the current with bioclim data 

predict_current <- predict(object = model_weis_11,
                           x = bioclim.data,
                           ext = extent(ca.data))
plot(predict_current)

predict_future <- predict(object = model_weis_11,
                          x = cmip5_11.data,
                          ext = extent(ca.data))
# go and make sure cmip / bio variable names are the same 
plot(predict_future)