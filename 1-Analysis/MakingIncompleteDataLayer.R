# In this file we make a shapefile that shows where we have inclomplete data
library(parallel)
wgs84.crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

# Here, check if there's a file that is the combo of south and central
# If not, combine south and central shapefiles
# if so, read the combo in

calveg_c.shp <- spTransform(readOGR("Data/CALVEG/CentralCoast"), wgs84.crs)
calveg_s.shp <- spTransform(readOGR("Data/CALVEG/SouthCoast"), wgs84.crs)

# Ok this next line takes the min and max extents of each shapefile, and finds
# the smallest extent that encompasses both of the shapefiles
ext <- extent(c(min(c(extent(calveg_c.shp)@xmin, extent(calveg_s.shp)@xmin)),
                max(c(extent(calveg_c.shp)@xmax, extent(calveg_s.shp)@xmax)),
                min(c(extent(calveg_c.shp)@ymin, extent(calveg_s.shp)@ymin)),
                max(c(extent(calveg_c.shp)@ymax, extent(calveg_s.shp)@ymax))
              ))

# This is a background grid at 30m resolution that we'll digitize stuff to
dummy.grid <- raster(ext, resolution = 1/60, crs = wgs84.crs)
writeRaster(dummy.grid, "Data/Rasters/background.tif")


# Now this dummy.grid raster was used to burn the vectors into the raster using a 
# supercomputer and the following function
# gdal_rasterize -burn 1 -l CentralCALVEG CentralCALVEG/CentralCALVEG.shp background.tif

# now let's read in the rasters produced by this method
weis.grid <- raster("Data/Rasters/Weislander.tif")
calveg_c.grid <- raster("Data/Rasters/CentralCALVEG.tif")
calveg_s.grid <- raster("Data/Rasters/SouthCALVEG.tif")
values(calveg_c.grid)[is.na(values(calveg_c.grid))] <- 0
values(calveg_s.grid)[is.na(values(calveg_s.grid))] <- 0
calveg.grid <- calveg_c.grid + calveg_s.grid

values(weis.grid)[is.na(values(weis.grid))] <- 0
