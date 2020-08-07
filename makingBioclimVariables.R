# In this file we read in the monthly raster data as a raster "stack", then produce bioclim
# variables using the biovars() function
library(raster)
library(dismo)

# So we're going to list all the files in the ppt1930s directory and put them in an array
# we want to make sure we only get the files that match the .bil file extension, we do this
# with the 'pattern' variable, which follows a regex pattern recognition protocol. The * means
# to match any character or set of characters, the $ means that the file name ends at that point
# so the pattern says "match anything that has anything before .bil, but ends with .bil
ppt.files <- list.files("Data/PRISM/Prism Data 1930/PRISM ppt1930s", pattern = "*.bil$", full.names = T)
# now we read the array of file names in as a stack of rasters (just a bunch) with the 'stack function
ppt.stack <- raster::stack(ppt.files)
sp::plot(ppt.stack)

# Let's do the same for the others
tmax.files <- list.files("Data/PRISM/Prism Data 1930/PRISM tmax1930s", pattern = "*.bil$", full.names = T)
tmax.stack <- raster::stack(tmax.files)

tmin.files <- list.files("Data/PRISM/Prism Data 1930/PRISM tmin 1930s", pattern = "*.bil$", full.names = T)
tmin.stack <- raster::stack(tmin.files)

# Now let's see if biovars works
biovars(ppt.stack, tmin.stack, tmax.stack)

# ok it doesn't because there are 13 layers. but if we look at the filenames using
print(ppt.files)
# it looks like the first file is the average of all the 12 months, so let's jsut remove it

# We'll go thru the raster stacks and remove the first layer of each
ppt.stack_12 <- dropLayer(ppt.stack, 1)
tmax.stack_12 <- dropLayer(tmax.stack, 1)
tmin.stack_12 <- dropLayer(tmin.stack, 1)

biovars(ppt.stack_12, tmin.stack_12, tmax.stack_12)
# yay!
prism_var <- biovars(ppt.stack_12, tmin.stack_12, tmax.stack_12)
nlayers(create_var)

?writeRaster
prism_var <- writeRaster(create_var,"Data/PRISM/prism_variables",format = "raster")
readOGR()

print(prism_var)
plot(prism_var)
# I'll let you figure out how to write it (so you can pick where to write it to)
# but I would use this link to pick a solution probably https://stackoverflow.com/questions/26763013/r-write-rasterstack-and-preserve-layer-names




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
plot(predict_weislander_prism, main = "Wieslander and 19 Prism Var")

#model with the 11 variables chosen 

prism11 <- dropLayer(prism_var, c(4,5,6,7,13,16,17,19))
nlayers(prism11)
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
