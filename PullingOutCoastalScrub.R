# In this Script we reduce large Vegetation to the Coastal Scrub Vegetation Type
# and Save the files

library(rgdal)

# Read in CA-- we'll use the CRS of this as the standard for the other shapefiles
ca.shp <- readOGR("Data/CA_Counties") 


# Let's start with Weislander

shp.1930s <- readOGR("Data/WeislanderVeg","Wieslander_Statewide_CANAD83")
coastalscrub.shp <- shp.1930s[shp.1930s$WHR1_TYPE == "Coastal Scrub",]
coastalscrub.shp <- spTransform(coastalscrub.shp, crs(ca.shp))

# Create a directory to hold the shapefiles
dir.create("Data/Weislander_Sage")
# Write the shapefile to that directory. dsn = the destination directory. layer = the name of the shapefile
# (you can write multiple layers to the same directory). driver = how you want to save the spatial information
writeOGR(coastalscrub.shp, dsn = "Data/Weislander_Sage/", layer = "Weislander_Sage", driver = "ESRI Shapefile")

# Calveg is broken up into pieces, so we'll pull out coastal scrub from the southcoast region and centralcoast region
# and write them as separate layers into the same directory. We can combine them later down the line

south_calveg_sage.shp <- south_calveg.shp[south_calveg.shp$CWHR_TYPE == "CSC",] # CSC is the shorthand code that Calveg uses for Coastal Scrub
south_calveg_sage.shp <- spTransform(south_calveg_sage.shp, crs(ca.shp))
dir.create("Data/CALVEG_Sage") 
writeOGR(south_calveg_sage.shp, "Data/CALVEG_Sage/", "South_Sage", driver = "ESRI Shapefile")

central_calveg.shp <- readOGR("Data/CALVEG/CentralCoast/")
central_calveg_sage.shp <- central_calveg.shp[central_calveg.shp$CWHR_TYPE == "CSC",]
central_calveg_sage.shp <- spTransform(central_calveg_sage.shp, crs(ca.shp))
writeOGR(central_calveg_sage.shp, "Data/CALVEG_Sage/", "Central_Sage", driver = "ESRI Shapefile")
