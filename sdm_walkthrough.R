#going through avery SDM walkthrough 

#Data Preperation 
#load species occurance data 

#loading wieslander 
weislander.data <- readOGR("Data/Weislander_Sage")
summary(weislander.data)

#turn into lon/lat data
wgs84.crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
N = 300
#can we run spsample without the N? using all the occurance points instead?
?spsample
weislander.data_wgs84 <- spTransform(weislander.data, wgs84.crs) 
weislander.pts <- spsample(weislander.data_wgs84,N, type = 'random')
weislander.pts@coords
weislander.lon <- weislander.pts@coords[,'x']
weislander.lat <- weislander.pts@coords[,'y']
summary(weislander.pts)

#read in california boundaries 

ca.data <- readOGR("Data/CA_Counties")
#plot(ca.data,axes = TRUE, fill = NA)
summary(ca.data)
ca.data <- spTransform(ca.data,crs(weislander.pts))

ca.outline <- readOGR("Data/Ca State")
summary(ca.outline)
ca.outline <- spTransform(ca.outline,crs(weislander.pts))
plot(ca.outline)


#look at raw occurance data 
#with county map
sp::plot(ca.data,main = "Weislander CSS")
points(weislander.pts)

#with Ca outline 
sp::plot(ca.outline,main = "Weislander CSS")
points(weislander.pts)


#Environmental Predictor Data 

#using bioclim for now 
bioclim.data <- getData(name = "worldclim",
                        var = 'bio', 
                        res = 2.5,
                        path = "input/")
bioclim.data <- dropLayer(bioclim.data, c(4,5,6,7,13,16,17,19))
summary(bioclim.data)

#name replacement 
names(bioclim.data)[names(bioclim.data) 
                    %in% c("bio1", "bio2","bio3","bio8","bio9","bio10",
                           "bio11", "bio12", "bio14", "bio15", "bio18")] <- 
  c("MAT", "MDR", "I", "MTWQ","MTDQ","MTWaQ", "MTCQ", "AP","PDM","PS","PWQ")
print(names(bioclim.data))
#bio1 Annual Mean Temperature (AMT)
#bio2 Mean Durnial Range (MDR)
#bio3 Isothermality (I)
#bio8 Mean Temperature of Wettest Quarter (MTWQ)
#bio9 Mean Temperature of Driest Quarter (MTDQ)
#bio10 Mean Temperature of Warmest Quarter (MTWaQ)
#bio11 Mean Temperature of Coldest Quarter (MTCQ)
#bio12 Annual Precipitation (AP)
#bio14 Precipitation of Driest Month (PDM)
#bio15 Precipitation Seasonality (PS)
#bio18 Precipitation of Warmest Quarter (PWQ)


#crop bioclim to the extent of california 
bioclim.data <- crop(bioclim.data,extent(ca.outline))

#looking at 11 vars 
predictors <- raster::subset(bioclim.data,c("MAT", "MDR", "I", 
                                            "MTWQ","MTDQ","MTWaQ", 
                                            "MTCQ", "AP","PDM",
                                            "PS","PWQ"))
#look at raster for annual precipitation
sp::plot(predictors$AP, main = "Annual Precipitation")


#SDM - r5eady df of predictor values and species presence/absence

#predictor values at location of species presence 
presvals <- raster::extract(predictors,weislander.pts)

#predictor values at loc of absence using 500 random locations
##ask if this should be 300? since there are 300 weislander pts?
backgr <- randomPoints(predictors,500)
absvals <- raster::extract(predictors,500)

Y <- c(rep(1,nrow(presvals)),rep(0,nrow(absvals)))

#dataframe of predictor values

sdmdata <- data.frame(cbind(Y, rbind(presvals,absvals)))
head(sdmdata)



#collinearity of predictors 

pairs(sdmdata[,2:length(sdmdata)], cex = 0.1, fig=TRUE)
