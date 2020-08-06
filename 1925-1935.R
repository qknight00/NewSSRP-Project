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


# now we read the array of file names in as a stack of rasters (just a bunch) with the 'stack function
?stack
ppt1925_1935.stack <- raster::stack(c(ppt1925.files,ppt1926.files,ppt1927.files,ppt1928.files,
                                      ppt1929.files,ppt1930.files,ppt1931.files,ppt1932.files,
                                      ppt1933.files,ppt1934.files,ppt1935.files))
sp::plot(ppt1925_1935.stack)
#not able to plot : margins to large 
#should I be trying to merge these file