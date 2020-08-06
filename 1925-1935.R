library(raster)
library(dismo)
library(rgdal)

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