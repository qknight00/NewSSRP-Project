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

stack_vars_2526 <- raster::stack(c(vars_1925,vars1926))
plot(stack_vars_2526)
?biovars
nlayers(stack_vars_2526)  #produces 38 layers
#think about cropping extent to the california state line 
?crop
?merge
?as.data.frame

df_1925 <- as.data.frame(stack_vars_2526)
class(df_1925)
?rbind

bind <- rbind(as.matrix(vars_1925),
              as.matrix(vars1926))

plot(bind)

#running sdm using bind (the dataframe with 1925/1926 climate data )

?bioclim
prism_grd <- system.file("Data/PRISM/prism_variables.grd",package = "raster")
model_1 <- bioclim(x =prism_grd,p=weislander.pts)

#df to matrix 
class(bind)
bind<- as.matrix(bind)
model_df <- bioclim(bind,p = weislander.pts)


#model Production using stack_vars_2526 with all 38 variables 
model <- bioclim(stack_vars_2526, p= weislander.pts)
plot(model)
predict_stack <- dismo::predict(object = model,
                                          x = stack_vars_2526,
                                          ext = extent(ca.data))
plot(predict_stack)
