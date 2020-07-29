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

# pulling and plotting bioclim data 
bioclim.data <- getData(name = "worldclim",
                         var = 'bio', 
                        res = 2.5,
                        path = "input/")
 
plot(bioclim.data)


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

#longtitude = x lat = y 


#loading county map 
ca.data <- readOGR("Data/CA_Counties")
plot(ca.data,axes = TRUE, col = "grey95")
summary(ca.data)
plot(calveg.data,add = TRUE)

#changing crs of county data
crs(ca.data)
crs(bioclim.data)
ca.data <- spTransform(ca.data,crs(bioclim.data))


#stopped here 12:13am

#code to build sdm with 19 variables 
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


#predict bioclim with 11 bioclim variables 
#droplayers from the rasterstack
dir.create(path = "input_11")
bioclim_11.data <- getData(name = "worldclim",
                        var = 'bio', 
                        res = 2.5,
                        path = "input_11/")

bioclim_11.data <- dropLayer(bioclim_11.data, c(4,5,6,7,13,16,17,19))

#now equals 11  
nlayers(bioclim_11.data)
plot(bioclim_11.data)

#creating model for calveg plus 11 variables from bioclim 


model_all_11 <- bioclim(bioclim_11.data,
                            p = all_calveg.pts)

predict_calveg_11 <- dismo::predict(model_all_11,
                                         x = bioclim_11.data,
                                         ext = extent(ca.data))
plot(predict_calveg_11,
     main = "Calveg + 11 Bioclim Variables",
     xlab = "Longtiude",
     ylab = "Latitude",)


--------------------------------------------------
#notes and extra code 
#pulling central and south calveg sage shapefile removed  

#code for writing a csv file 
#central_calveg.csv <- write.csv(central_calveg.data) 

#code to get rid of NAs removed / deemed unneccesary 
#skipped determining geographic extent 

#reading spatial data 
class(calveg.data)
extent(calveg.data)
crs(calveg.data)


#searching for long/lat in spatial data
# ?spsample
# plot(spsample(calveg.data,n=100,type = "random"))

##plotting side by side and by itself

par(mfrow = c(2,1))
par(mfrow = c(1,1))

------------------------------------------------------------

#loading Weislander data and creating sdm with bioclim data
#understanding that bioclim is not the best for this dataset? 

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

#removed code for map using weislander with bioclim data 

#now try to load CMIP Data to predict Weislander distribution 

?getData
cmip5.data <- getData('CMIP5',
                      var = 'bio',
                      res = 2.5,
                      rcp = 45,
                      model = 'AC',
                      year = 50,
                      path = "input/")

plot(cmip5.data)
model_weislander_cmip <- bioclim(cmip5.data,
                                 p = weislander.pts)
plot(model_weislander_cmip)
predict_weislander_cmip <- dismo::predict(object = model_weislander_cmip,
                                          x = cmip5.data,
                                          ext = extent(ca.data))
plot(predict_weislander_cmip, main = "Weislander and 19 CMIP Data")


#pulling the 11 variables from the cmip5 data 

cmip5_11.data <- getData('CMIP5',
                      var = 'bio',
                      res = 2.5,
                      rcp = 45,
                      model = 'AC',
                      year = 50,
                      path = "input_11/")
cmip5_11.data <- dropLayer(cmip5_11.data, c(4,5,6,7,13,16,17,19))
nlayers(cmip5_11.data)

#weislander prediction using the 11 variables in cmip5 
model_weislander_cmip11 <- bioclim(cmip5_11.data,
                                   p = weislander.pts)
predict_weislander_cmip11 <- predict(object = model_weislander_cmip11,
                                     x = cmip5_11.data,
                                     ext = extent(ca.data))
plot(predict_weislander_cmip11, main = "Wieslander and 11 Cmip Variables")

par(mfrow = c(2,1))
par(mfrow = c(1,1))



#pushing weislander projection into the future 
#using all 19 variables 
#setting names equal 
names(bioclim.data) <- names(cmip5.data)
names(bioclim_11.data) <- names(cmip5_11.data)


predict_current_weislander_19 <- predict(object = model_weislander_cmip,
                                         x = bioclim.data,
                                         ext = extent(ca.data))
plot(predict_current_weislander_19,
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

?maxent

#code to push weislander to the present 
------------------------------------------------------------
#creating a file to move maps to qgis for manipulation :) 
?writeRaster()

writeRaster(predict_weislander_cmip11, filename = file.path("weislander_cmip"))
writeRaster(predict_calveg,filename = file.path("calveg_bioclim"))