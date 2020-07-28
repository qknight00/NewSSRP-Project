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

#loading county map 
ca.data <- readOGR("Data/CA_Counties")
plot(ca.data,axes = TRUE, col = "grey95")
summary(ca.data)
plot(calveg.data,add = TRUE)

#changing crs of county data
crs(ca.data)
crs(bioclim.data)
ca.data <- spTransform(ca.data,crs(bioclim.data))




#code to build sdm with 19 variables 
summary(bioclim.data)

model_all <- bioclim(bioclim.data,
                     p = all_calveg.pts)

plot(model_all, add = TRUE)
plot(ca.data)

#running the sdm / prediction
predict_calveg <- dismo::predict(object = model_all,
                                 x = bioclim.data,
                                 ext = extent(ca.data))
plot(ca.data)
plot(predict_calveg, main = "19 Bioclim Var and Calveg")


#predict bioclim with 11 bioclim variables 
#droplayers from the rasterstack
?dropLayer
bioclim.data <- dropLayer(bioclim.data, c(4,5,6,7,13,16,17,19))

#now equals 11  
nlayers(bioclim.data)
plot(bioclim.data)

#creating model for calveg plus 11 variables from bioclim 


model_all_11 <- bioclim(bioclim.data,
                            p = all_calveg.pts)

predict_calveg_11 <- dismo::predict(model_all_11,
                                         x = bioclim.data,
                                         ext = extent(ca.data))
plot(predict_calveg_11, main = "Calveg + 11 Bioclim Variables")


--------------------------------------------------
#notes and extra code 
#pulling central and south calveg sage shapefile
central_calveg.data <- readOGR("Data/CALVEG_Sage","Central_Sage")

south_calveg.data <- readOGR("Data/CALVEG_Sage","South_Sage")

# Turn it into SpatialDataPoints

#code for writing a csv file 
#central_calveg.csv <- write.csv(central_calveg.data) 

#code to get rid of NAs removed / deemed unneccesary 
#skipped determining geographic extent 
#load data for base map ? 

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

#pulling lat/lon data for weislander dataset
N = 300
weislander.data_wgs84 <- spTransform(weislander.data, wgs84.crs) 
weislander.pts <- spsample(weislander.data_wgs84, N, type = 'random')
weislander.pts@coords
weislander.lon <- weislander.pts@coords[,'x']
weislander.lat <- weislander.pts@coords[,'y']
summary(weislander.pts)

#code for sdm 

model_weislander <- bioclim(bioclim.data,
                            p = weislander.pts)
plot(model_weislander)
plot(ca.data)
predict_weislander <- dismo::predict(object = model_weislander,
                                     x = bioclim.data,
                                     ext = extent(ca.data))
plot(predict_weislander, main = "weislander and BIOCLIM Data")


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
plot(predict_weislander_cmip, main = "Weislander and CMIP Data")


#pulling the 11 variables from the cmip5 data 
summary(cmip5.data)
nlayers(cmip5.data)
cmip5_11.data <- dropLayer(cmip5.data, c(4,5,6,7,13,16,17,19))
nlayers(cmip5_11.data)

#weislander prediction using the 11 variables in cmip5 
model_weislander_cmip11 <- bioclim(cmip5_11.data,
                                   p = weislander.pts)
predict_weislander_cmip11 <- predict(object = model_weislander_cmip11,
                                     x = cmip5_11.data,
                                     ext = extent(ca.data))
plot(predict_weislander_cmip11, main = "Wieslander and 11 Cmip Variables")

par(mfrow = c(2,1))

#pushing weislander projection into the future 
summary(predict_weislander_cmip11)
?predict

model_weislander_cmip11 <- bioclim(cmip5_11.data,
                                   p = weislander.pts)
plot(model_weislander_cmip11)
predict_future_weislander <- predict(object = model_weislander_cmip11,
                                      x = bioclim11.data,
                                      ext = extent(ca.data))

?maxent
------------------------------------------------------------
#creating a file to move maps to qgis for manipulation :) 
?writeRaster()

writeRaster(predict_weislander_cmip11, filename = file.path("weislander_cmip"))
writeRaster(predict_calveg,filename = file.path("calveg_bioclim"))