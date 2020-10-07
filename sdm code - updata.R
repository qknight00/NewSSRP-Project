#practice modeling the distribution using online source 
#install.packages("dismo")
#install.packages("raster")
library("raster")
library("dismo")
library("maptools")
library("sp")
library("rgdal")


# This is a CRS where the coordinates are in longitude and latitude
wgs84.crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

dir.create(path = "input")
dir.create(path = "output")

#pulling historical climate data : prism data 




#pulling and plotting bioclim data 
#bioclim is the current climate data 
bioclim.data <- getData(name = "worldclim",
                        var = 'bio', 
                        res = 2.5,
                        path = "input/")
bioclim.data <- dropLayer(bioclim.data, c(4,5,6,7,13,16,17,19))
plot(bioclim.data)

#loading future cmip data 
cmip5_11.data <- getData('CMIP5',
                         var = 'bio',
                         res = 2.5,
                         rcp = 45,
                         model = 'AC',
                         year = 50,
                         path = "input_11/")
cmip5_11.data <- dropLayer(cmip5_11.data, c(4,5,6,7,13,16,17,19))
nlayers(cmip5_11.data)

#loading county map 
ca.data <- readOGR("Data/CA_Counties")
plot(ca.data,axes = TRUE, fill = NA)
summary(ca.data)
plot(calveg.data,add = TRUE)

#changing crs of county data
crs(ca.data)
crs(bioclim.data)
ca.data <- spTransform(ca.data,crs(bioclim.data))

weislander.data <- readOGR("Data/Weislander_Sage")
summary(weislander.data)


#pulling lat/lon data for weislander dataset
N = 300
weislander.data_wgs84 <- spTransform(weislander.data, wgs84.crs) 
weislander.pts <- spsample(weislander.data_wgs84, N, type = 'random')
weislander.pts@coords
weislander.lon <- weislander.pts@coords[,'x']
weislander.lat <- weislander.pts@coords[,'y']
summary(weislander.pts)


#for plotting multiple graphs at a time 
par(mfrow = c(2,1))
par(mfrow = c(1,1))

#overlay the california shapefile 


#pushing weislander projection into the future 
#using all 19 variables 
#setting names equal 
names(cmip5.data) <- names(bioclim.data)
names(cmip5.data)


predict_current_weislander_19 <- predict(object = model_weislander_cmip,
                                         x = bioclim.data,
                                         ext = extent(ca.data))
plot(predict_current_weislander_19,
     main = "Current Sage Scrub Distribution with 19 Var",
     xlab = "Longitude",
     ylab = "Latitude")

current_prediction_19 <- plot(predict_current_weislander_19,
                              main = "Current Sage Scrub Distribution with 19 Var",
                              xlab = "Longitude",
                              ylab = "Latitude")

predict_current_weislander_11 <- predict(object = model_weislander_cmip11,
                                         x = bioclim_11.data,
                                         ext = extent(ca.data))
plot(predict_current_weislander_11,
     main = "Current Sage Scrub Distribution with 11 Var",
     xlab = "Longitude",
     ylab = "Latitude")


#load the calveg data 
calveg.data <- readOGR("Data/CALVEG_Sage")
summary(calveg.data)

#pulling lat/lon data for the entire calveg dataset
N = 300
all_calveg.data_wgs84 <- spTransform(calveg.data, wgs84.crs) 
all_calveg.pts <- spsample(all_calveg.data_wgs84, N, type = 'random')
all_calveg.pts@coords
all_calveg.lon <- all_calveg.pts@coords[,'x']
all_calveg.lat <- all_calveg.pts@coords[,'y']
summary(all_calveg.pts)

#code to build sdm with 11 variables 
summary(bioclim.data)

model_all <- bioclim(bioclim.data,
                     p = all_calveg.pts)

plot(model_all, add = TRUE)


#running the sdm / prediction
predict_calveg_19 <- dismo::predict(object = model_all,
                                    x = bioclim.data,
                                    ext = extent(ca.data))

plot(predict_calveg_19,
     main = "19 Bioclim Var and Calveg", 
     xlab = "Longtiude",
     ylab = "Latitude",)




model_calveg_future <- bioclim(x = cmip5_11.data,
                               p = all_calveg.pts)
predict_calvef_future <- predict(object = model_calveg_future,
                                 x = cmip5_11.data,
                                 ext = extent(ca.data))
plot(predict_calvef_future,
     xlab = "Longtiude",
     ylab = "Latitude")


##plotting side by side and by itself

par(mfrow = c(2,1))
par(mfrow = c(1,1))

------------------------------------------------------------
  
  #loading Weislander data and creating sdm with bioclim data
  #understanding that bioclim is not the best for this dataset? 
  #moved weislander up earlier 
  
  #removed code for map using weislander with bioclim data 
  
  #now try to load CMIP Data to predict Weislander distribution 
  
  #pulling the 11 variables from the cmip5 data 
  


#weislander prediction using the 11 variables in cmip5 
model_weislander_cmip11 <- bioclim(cmip5_11.data,
                                   p = weislander.pts)
predict_weislander_cmip11 <- predict(object = model_weislander_cmip11,
                                     x = cmip5_11.data,
                                     ext = extent(ca.data))
plot(predict_weislander_cmip11, main = "Future / Wieslander and 11 Cmip Variables")

model_calveg_future <- bioclim(cmip5_11.data, p = all_calveg.pts)
predict_cal_future <- predict(object = model_calveg_future,
                              x = cmip5_11.data,
                              ext = extent(ca.data))
plot(predict_cal_future, main = "calveg / future projection")

------------------------------------------------------------
  #creating a file to move maps to qgis for manipulation :) 
  ?writeRaster()

writeRaster(predict_weislander_cmip11, filename = file.path("weislander_cmip"))
writeRaster(predict_calveg,filename = file.path("calveg_bioclim"))

dir.create(path = "For QGIS")
writeRaster(predict_current_weislander_19, 
            filename = "Current_Prediction_19")
writeRaster(predict_calveg_19,
            filename = "Calveg_19")