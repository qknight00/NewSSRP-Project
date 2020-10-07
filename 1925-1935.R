#loading prism data for the years 1925-1935
# and some sdm for the years 1930 , and 1925/26 on the bottom 

library(raster)
library(dismo)
library(rgdal)


#Load for SDM 
wgs84.crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

#loading county map 
ca.data <- readOGR("Data/CA_Counties")
plot(ca.data,axes = TRUE, col = "grey95")

#changing crs of county data
crs(ca.data)
crs(wgs84.crs)
ca.data <- spTransform(ca.data,crs(wgs84.crs))

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

---------------------------------------------------------------------
  
# So we're going to list all the files in the ppt1930s directory and put them in an array
# we want to make sure we only get the files that match the .bil file extension, we do this
# with the 'pattern' variable, which follows a regex pattern recognition protocol. The * means
# to match any character or set of characters, the $ means that the file name ends at that point
# so the pattern says "match anything that has anything before .bil, but ends with .bil

?list.files
#looking at crs 
crs_25 <- crs("PRISM 1925-1935/PPT/PRISM_ppt_stable_4kmM2_1925_all_bil/PRISM_ppt_stable_4kmM2_192501_bil.bil")
print(ppt1925.files)


ppt1925.files <- list.files("PRISM 1925-1935/PPT/PRISM_ppt_stable_4kmM2_1925_all_bil/", pattern = "*.bil$", full.names = T)
ppt1926.files <- list.files("PRISM 1925-1935/PPT/PRISM_ppt_stable_4kmM2_1926_all_bil/", pattern = "*.bil$", full.names = T)
ppt1927.files <- list.files("PRISM 1925-1935/PPT/PRISM_ppt_stable_4kmM2_1927_all_bil/", pattern = "*.bil$", full.names = T)
ppt1928.files <- list.files("PRISM 1925-1935/PPT/PRISM_ppt_stable_4kmM2_1928_all_bil/", pattern = "*.bil$", full.names = T)
ppt1929.files <- list.files("PRISM 1925-1935/PPT/PRISM_ppt_stable_4kmM2_1929_all_bil/", pattern = "*.bil$", full.names = T)
ppt1930.files <- list.files("PRISM 1925-1935/PPT/PRISM_ppt_stable_4kmM2_1930_all_bil (1)/", pattern = "*.bil$", full.names = T)
ppt1931.files <- list.files("PRISM 1925-1935/PPT/PRISM_ppt_stable_4kmM2_1931_all_bil/", pattern = "*.bil$", full.names = T)
ppt1932.files <- list.files("PRISM 1925-1935/PPT/PRISM_ppt_stable_4kmM2_1932_all_bil/", pattern = "*.bil$", full.names = T)
ppt1933.files <- list.files("PRISM 1925-1935/PPT/PRISM_ppt_stable_4kmM2_1933_all_bil/", pattern = "*.bil$", full.names = T)
ppt1934.files <- list.files("PRISM 1925-1935/PPT/PRISM_ppt_stable_4kmM2_1934_all_bil/", pattern = "*.bil$", full.names = T)
ppt1935.files <- list.files("PRISM 1925-1935/PPT/PRISM_ppt_stable_4kmM2_1935_all_bil/", pattern = "*.bil$", full.names = T)

#stack and drop the layers so that there are 12 of them 

ppt1925.stack <- raster::stack(ppt1925.files)
ppt1926.stack <- raster::stack(ppt1926.files)
ppt1927.stack <- raster::stack(ppt1927.files)
ppt1928.stack <- raster::stack(ppt1928.files)
ppt1929.stack <- raster::stack(ppt1929.files)
ppt1930.stack <- raster::stack(ppt1930.files)
ppt1931.stack <- raster::stack(ppt1931.files)
ppt1932.stack <- raster::stack(ppt1932.files)
ppt1933.stack <- raster::stack(ppt1933.files)
ppt1934.stack <- raster::stack(ppt1934.files)
ppt1935.stack <- raster::stack(ppt1935.files)

#go from 13 dimensions to 12 dimensions
ppt1925.stack <- dropLayer(ppt1925.stack,1)
ppt1926.stack <- dropLayer(ppt1926.stack,1)
ppt1927.stack <- dropLayer(ppt1927.stack,1)
ppt1928.stack <- dropLayer(ppt1928.stack,1)
ppt1929.stack <- dropLayer(ppt1929.stack,1)
ppt1930.stack <- dropLayer(ppt1930.stack,1)
ppt1931.stack <- dropLayer(ppt1931.stack,1)
ppt1932.stack <- dropLayer(ppt1932.stack,1)
ppt1933.stack <- dropLayer(ppt1933.stack,1)
ppt1934.stack <- dropLayer(ppt1934.stack,1)
ppt1935.stack <- dropLayer(ppt1935.stack,1)

class(ppt1925.stack)
class(ppt1925.files[2])
#transforming crs of raster stack
?spTransform
ppt1925_crs <- projectRaster(ppt1925.stack,wgs84.crs)

crs(ppt1925.stack)
crs(bioclim.data)
Nad83.crs <- "+proj=longlat +datum=NAD83 +no_defs"
bioclim_crs <- projectRaster(bioclim.data, Nad83.crs)
crs(x = "Data/PRISM/prism_variables.grd")
?projectRaster

#go through and change the year 1925 crs data and then run a model to see if this works  

newproj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
proj_2 <- "+proj=longlat +datum=NAD83 +no_defs "

projectRaster(bioclim.data, crs = proj_2)
crs(biocli.data)

projectRaster(ppt1925.stack, crs = newproj)
projectRaster(tmin1925.stack, crs = newproj)
projectRaster(tmax1925.stack, crs = newproj)

biovar_1925 <- biovars(ppt1925.stack, tmin1925.stack, tmax1925.stack) 
model_1925 <- bioclim(biovar_1925,p = weislander.pts)
predict_1925 <- predict(object = model_1925,
                        x = biovar_1925,
                        ext = extent(ca.data))
plot(predict_1925)

predict_1925_present <- predict(object = model_1925,
                                x = bioclim.data,
                                ext = extent(ca.data))
plot(predict_1925_present)

crs(biovar_1925)
crs(ppt1925.stack)
crs(tmax1925.stack)


?biovars
# now we read the array of file names in as a stack of rasters (just a bunch) with the 'stack function

#first try without stacking the years first
ppt1925_1935.stack <- raster::stack(c(ppt1925.files,ppt1926.files,ppt1927.files,ppt1928.files,
                                      ppt1929.files,ppt1930.files,ppt1931.files,ppt1932.files,
                                      ppt1933.files,ppt1934.files,ppt1935.files))

#try again after stacking years and removing first layer
ppt1925_1935.stack <- raster::stack(c(ppt1925.stack,ppt1926.stack,ppt1927.stack,
                                      ppt1928.stack,ppt1929.stack,ppt1930.stack,
                                      ppt1931.stack,ppt1932.stack,ppt1933.stack,
                                      ppt1934.stack,ppt1935.stack))

sp::plot(ppt1925_1935.stack)
print(ppt1925_1935.stack)


#now lets try to do the same thing with tmin / tmax 

tmin1925.files <- list.files("PRISM 1925-1935/TMIN/PRISM_tmin_stable_4kmM3_1925_all_bil/", pattern = "*.bil$", full.names = T)
tmin1926.files <- list.files("PRISM 1925-1935/TMIN/PRISM_tmin_stable_4kmM3_1926_all_bil/", pattern = "*.bil$", full.names = T)
tmin1927.files <- list.files("PRISM 1925-1935/TMIN/PRISM_tmin_stable_4kmM3_1927_all_bil/", pattern = "*.bil$", full.names = T)
tmin1928.files <- list.files("PRISM 1925-1935/TMIN/PRISM_tmin_stable_4kmM3_1928_all_bil/", pattern = "*.bil$", full.names = T)
tmin1929.files <- list.files("PRISM 1925-1935/TMIN/PRISM_tmin_stable_4kmM3_1929_all_bil/", pattern = "*.bil$", full.names = T)
tmin1930.files <- list.files("PRISM 1925-1935/TMIN/PRISM_tmin_stable_4kmM3_1930_all_bil/", pattern = "*.bil$", full.names = T)
tmin1931.files <- list.files("PRISM 1925-1935/TMIN/PRISM_tmin_stable_4kmM3_1931_all_bil/", pattern = "*.bil$", full.names = T)
tmin1932.files <- list.files("PRISM 1925-1935/TMIN/PRISM_tmin_stable_4kmM3_1932_all_bil/", pattern = "*.bil$", full.names = T)
tmin1933.files <- list.files("PRISM 1925-1935/TMIN/PRISM_tmin_stable_4kmM3_1933_all_bil/", pattern = "*.bil$", full.names = T)
tmin1934.files <- list.files("PRISM 1925-1935/TMIN/PRISM_tmin_stable_4kmM3_1934_all_bil/", pattern = "*.bil$", full.names = T)
tmin1935.files <- list.files("PRISM 1925-1935/TMIN/PRISM_tmin_stable_4kmM3_1935_all_bil/", pattern = "*.bil$", full.names = T)

#stack and drop the layers so that there are 12 of them 

tmin1925.stack <- raster::stack(tmin1925.files)
tmin1926.stack <- raster::stack(tmin1926.files)
tmin1927.stack <- raster::stack(tmin1927.files)
tmin1928.stack <- raster::stack(tmin1928.files)
tmin1929.stack <- raster::stack(tmin1929.files)
tmin1930.stack <- raster::stack(tmin1930.files)
tmin1931.stack <- raster::stack(tmin1931.files)
tmin1932.stack <- raster::stack(tmin1932.files)
tmin1933.stack <- raster::stack(tmin1933.files)
tmin1934.stack <- raster::stack(tmin1934.files)
tmin1935.stack <- raster::stack(tmin1935.files)

tmin1925.stack <- dropLayer(tmin1925.stack,1)
tmin1926.stack <- dropLayer(tmin1926.stack,1)
tmin1927.stack <- dropLayer(tmin1927.stack,1)
tmin1928.stack <- dropLayer(tmin1928.stack,1)
tmin1929.stack <- dropLayer(tmin1929.stack,1)
tmin1930.stack <- dropLayer(tmin1930.stack,1)
tmin1931.stack <- dropLayer(tmin1931.stack,1)
tmin1932.stack <- dropLayer(tmin1932.stack,1)
tmin1933.stack <- dropLayer(tmin1933.stack,1)
tmin1934.stack <- dropLayer(tmin1934.stack,1)
tmin1935.stack <- dropLayer(tmin1935.stack,1)

# now we read the array of file names in as a stack of rasters (just a bunch) with the 'stack function

#first try without stacking the years first
tmin1925_1935.stack <- raster::stack(c(tmin1925.files,tmin1926.files,tmin1927.files,tmin1928.files,
                                       tmin1929.files,tmin1930.files,tmin1931.files,tmin1932.files,
                                       tmin1933.files,tmin1934.files,tmin1935.files))

#try again after stacking years and removing first layer
tmin1925_1935.stack <- raster::stack(c(tmin1925.stack,tmin1926.stack,tmin1927.stack,
                                      tmin1928.stack,tmin1929.stack,tmin1930.stack,
                                      tmin1931.stack,tmin1932.stack,tmin1933.stack,
                                      tmin1934.stack,tmin1935.stack))

sp::plot(tmin1925_1935.stack)


#now tmax

tmax1925.files <- list.files("PRISM 1925-1935/TMAX/PRISM_tmax_stable_4kmM3_1925_all_bil/", pattern = "*.bil$", full.names = T)
tmax1926.files <- list.files("PRISM 1925-1935/TMAX/PRISM_tmax_stable_4kmM3_1926_all_bil/", pattern = "*.bil$", full.names = T)
tmax1927.files <- list.files("PRISM 1925-1935/TMAX/PRISM_tmax_stable_4kmM3_1927_all_bil/", pattern = "*.bil$", full.names = T)
tmax1928.files <- list.files("PRISM 1925-1935/TMAX/PRISM_tmax_stable_4kmM3_1928_all_bil/", pattern = "*.bil$", full.names = T)
tmax1929.files <- list.files("PRISM 1925-1935/TMAX/PRISM_tmax_stable_4kmM3_1929_all_bil/", pattern = "*.bil$", full.names = T)
tmax1930.files <- list.files("PRISM 1925-1935/TMAX/PRISM_tmax_stable_4kmM3_1930_all_bil/", pattern = "*.bil$", full.names = T)
tmax1931.files <- list.files("PRISM 1925-1935/TMAX/PRISM_tmax_stable_4kmM3_1931_all_bil/", pattern = "*.bil$", full.names = T)
tmax1932.files <- list.files("PRISM 1925-1935/TMAX/PRISM_tmax_stable_4kmM3_1932_all_bil/", pattern = "*.bil$", full.names = T)
tmax1933.files <- list.files("PRISM 1925-1935/TMAX/PRISM_tmax_stable_4kmM3_1933_all_bil/", pattern = "*.bil$", full.names = T)
tmax1934.files <- list.files("PRISM 1925-1935/TMAX/PRISM_tmax_stable_4kmM3_1934_all_bil/", pattern = "*.bil$", full.names = T)
tmax1935.files <- list.files("PRISM 1925-1935/TMAX/PRISM_tmax_stable_4kmM3_1935_all_bil/", pattern = "*.bil$", full.names = T)

#stack and drop the layers so that there are 12 of them 

tmax1925.stack <- raster::stack(tmax1925.files)
tmax1926.stack <- raster::stack(tmax1926.files)
tmax1927.stack <- raster::stack(tmax1927.files)
tmax1928.stack <- raster::stack(tmax1928.files)
tmax1929.stack <- raster::stack(tmax1929.files)
tmax1930.stack <- raster::stack(tmax1930.files)
tmax1931.stack <- raster::stack(tmax1931.files)
tmax1932.stack <- raster::stack(tmax1932.files)
tmax1933.stack <- raster::stack(tmax1933.files)
tmax1934.stack <- raster::stack(tmax1934.files)
tmax1935.stack <- raster::stack(tmax1935.files)

tmax1925.stack <- dropLayer(tmax1925.stack,1)
tmax1926.stack <- dropLayer(tmax1926.stack,1)
tmax1927.stack <- dropLayer(tmax1927.stack,1)
tmax1928.stack <- dropLayer(tmax1928.stack,1)
tmax1929.stack <- dropLayer(tmax1929.stack,1)
tmax1930.stack <- dropLayer(tmax1930.stack,1)
tmax1931.stack <- dropLayer(tmax1931.stack,1)
tmax1932.stack <- dropLayer(tmax1932.stack,1)
tmax1933.stack <- dropLayer(tmax1933.stack,1)
tmax1934.stack <- dropLayer(tmax1934.stack,1)
tmax1935.stack <- dropLayer(tmax1935.stack,1)
names(tmax1925.stack)
# now we read the array of file names in as a stack of rasters (just a bunch) with the 'stack function

#first try without stacking the years first
tmax1925_1935.stack <- raster::stack(c(tmax1925.files,tmax1926.files,tmax1927.files,tmax1928.files,
                                       tmax1929.files,tmax1930.files,tmax1931.files,tmax1932.files,
                                       tmax1933.files,tmax1934.files,tmax1935.files))

#try again after stacking years and removing first layer
tmax1925_1935.stack <- raster::stack(c(tmax1925.stack,tmax1926.stack,tmax1927.stack,
                                       tmax1928.stack,tmax1929.stack,tmax1930.stack,
                                       tmax1931.stack,tmax1932.stack,tmax1933.stack,
                                       tmax1934.stack,tmax1935.stack))

sp::plot(tmax1925_1935.stack)
nlayers(ppt1925_1935.stack)
vars_1925_1935 <- biovars(ppt1925_1935.stack,tmin1925_1935.stack,tmax1925_1935.stack)

vars_1925 <- biovars(ppt1925.stack,tmin1925.stack,tmax1925.stack)
vars1926 <- biovars(ppt1926.stack,tmin1926.stack,tmax1926.stack)
vars1927 <- biovars(ppt1927.stack,tmin1927.stack,tmax1927.stack)
vars1928 <- biovars(ppt1928.stack,tmin1928.stack,tmax1928.stack)
vars1929 <- biovars(ppt1929.stack,tmin1929.stack,tmax1929.stack)
vars1930 <- biovars(ppt1930.stack,tmin1930.stack,tmax1930.stack)
vars1931 <- biovars(ppt1931.stack,tmin1931.stack,tmax1931.stack)
vars1932 <- biovars(ppt1932.stack,tmin1932.stack,tmax1932.stack)
vars1933 <- biovars(ppt1933.stack,tmin1933.stack,tmax1933.stack)
vars1934 <- biovars(ppt1934.stack,tmin1934.stack,tmax1934.stack)
vars1935 <- biovars(ppt1935.stack,tmin1935.stack,tmax1935.stack)

nlayers(vars_1925)
#cutting the number of bioclim variables in each year

vars_1925 <- dropLayer(vars_1925, c(4,5,6,7,13,16,17,19))
vars1926 <- dropLayer(vars1926,c(4,5,6,7,13,16,17,19))
vars1927 <- dropLayer(vars1927,c(4,5,6,7,13,16,17,19))
vars1928 <- dropLayer(vars1928,c(4,5,6,7,13,16,17,19))
vars1929 <- dropLayer(vars1929,c(4,5,6,7,13,16,17,19))
vars1930 <- dropLayer(vars1930,c(4,5,6,7,13,16,17,19))
vars1931 <- dropLayer(vars1931,c(4,5,6,7,13,16,17,19))
vars1932 <- dropLayer(vars1932,c(4,5,6,7,13,16,17,19))
vars1933 <- dropLayer(vars1933,c(4,5,6,7,13,16,17,19))
vars1934 <- dropLayer(vars1934,c(4,5,6,7,13,16,17,19))
vars1935 <- dropLayer(vars1935,c(4,5,6,7,13,16,17,19))



#stacking 2 years 
stack_vars_2526 <- raster::stack(c(vars_1925,vars1926))
plot(stack_vars_2526)

#stacking all years 

stack_all <- raster::stack(c(vars_1925,vars1926,vars1927,vars1928,
                             vars1929,vars1930,vars1931,vars1932,
                             vars1933,vars1934,vars1935))

nlayers(stack_all)
#now stack all contains 123 layers , 11 years each year containing the 
#11 variables for each year

nlayers(stack_vars_2526)  #produces 38 layers
#think about cropping extent to the california state line 


?crop
#crop raster to the extent of the ca.data file 
stack_crop <- raster::crop(stack_all, ca.data)
plot(stack_crop)
names(stack_crop)

#take the mean of all the bioclim layers/ variables 


?`subset,RasterStack-method`
#subsetting biovar 1 , and averaging it
subset1 <- subset(stack_crop,c(1,12,23,34,45,56,67,78,89,100,111))
plot(subset1)
mean1 <- calc(subset1,fun = mean)
plot(mean1)

#now do the rest of them the same way 
subset2 <- subset(stack_crop,c(2,13,24,35,46,57,68,79,90,101,112))
subset3 <- subset(stack_crop,c(3,14,25,36,47,58,69,80,91,102,113))
subset8 <- subset(stack_crop,c(4,15,26,37,48,59,70,81,92,103,114))
subset9 <- subset(stack_crop,c(5,16,27,38,49,60,71,82,93,104,115))
subset10 <- subset(stack_crop,c(6,17,28,39,50,61,72,83,94,105,116))
subset11 <- subset(stack_crop,c(7,18,29,40,51,62,73,84,95,106,117))
subset12 <- subset(stack_crop,c(8,19,30,41,52,63,74,85,96,107,118))
subset14 <- subset(stack_crop,c(9,20,31,42,53,64,75,86,97,108,119))
subset15 <- subset(stack_crop,c(10,21,32,43,54,65,76,87,98,109,120))
subset18 <- subset(stack_crop,c(11,22,33,44,55,66,77,88,99,110,121))

mean2 <- calc(subset2,fun = mean)
mean3 <- calc(subset3,fun = mean)
mean8 <- calc(subset8,fun = mean)
mean9 <- calc(subset9,fun = mean)
mean10 <- calc(subset10,fun = mean)
mean11 <- calc(subset11,fun = mean)
mean12 <- calc(subset12,fun = mean)
mean14 <- calc(subset14,fun = mean)
mean15 <- calc(subset15,fun = mean)
mean18 <- calc(subset18,fun = mean)

plot(mean2)
stack_meanvars <- raster::stack(c(mean1,mean2,mean3,mean8,mean9,mean10,
                                  mean11,mean12,mean14,mean15,mean18))

plot(stack_meanvars)

#make a model with the stacked means / and 11 variables 
#if it works go back in and change the names to the same as bioclim names 
model_mean <- bioclim(x = stack_meanvars,
                      p = weislander.pts)
plot(model_mean)
predict_mean <- predict(object = model_mean,
                        x = stack_meanvars,
                        ext = extent(ca.data))
plot(predict_mean, main = "11 prism / weis - for 25-35" )

#try to project into the current 
#first make the names the same 
names(stack_meanvars) <- names(bioclim_11.data)

current_mean_prediction <- predict(object = model_mean,
                                   x = bioclim_11.data,
                                   ext = extent(ca.data))
plot(current_mean_prediction)
df_1925 <- as.data.frame(stack_vars_2526)
class(df_1925)
?rbind

bind <- rbind(as.matrix(vars_1925),
              as.matrix(vars1926))

plot(bind)

#model with a grd file / did not work 
prism_grd <- system.file("Data/PRISM/prism_variables.grd",package = "raster")
model_1 <- bioclim(x =prism_grd,p=weislander.pts)


model_df <- bioclim(bind,p = weislander.pts)

#see if this helps with error in overlaying county map 
# and shapefiles
install.packages("arulesViz")

#load Ca state Map 
state.data <- readOGR("Data/Ca State")
plot(state.data,add = TRUE)
state.data <- spTransform(state.data,crs(ca.data))
#historical plots 

#model with just one year, 1925 
model_25 <- bioclim(vars_1925, p = weislander.pts)
predict_25 <- predict(object = model_25,
                      x = vars_1925,
                      ext = extent(ca.data))
plot(predict_25, main = "prism 1925 only / weis")

#model with all years 1925 - 1935 
model_all <- bioclim(stack_all, p = weislander.pts)
predict_all <- predict(object = model_all,
                       x = stack_all,
                       ext = extent(ca.data))
plot(predict_all)


#predicting into the present with bioclim / weislander
#present calveg map 
model_25_present <- bioclim(bioclim_11.data, p = weislander.pts)
predict_model_25_present <- predict(object = model_25_present,
                                    x = bioclim_11.data,
                                    ext = extent(ca.data))
class(ca.data)
plot(predict_model_25_present,
     main = "current weis / with bioclim",
     xlab = "Longtiude",
     ylab = "Latitude")

crs(ca.data)
crs(state.data)



par(new = TRUE)
plot(ca.data)
plot(state.data)
plot(state.data, add = TRUE)
plot(ca.data, add = TRUE)
plot(calveg.data,add = TRUE)

plot(predict_calveg_11,
     main = "Calveg + 11 Bioclim Variables",
     xlab = "Longtiude",
     ylab = "Latitude",)


#future weis and calveg 
plot(predict_weislander_cmip11, main = "Future / Wieslander and 11 Cmip Variables")

plot(predict_calvef_future, main = "calveg / future projection")

