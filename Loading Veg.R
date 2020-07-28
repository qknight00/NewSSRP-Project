# In this script we load in vegetation shapefiles, pull out particular 
# vegetation types, then practice writing one to a new folder

library(rgdal)

# Load in 1930s Data
?readOGR
shp.1930s <- readOGR("Data/WeislanderVeg", "Wieslander_Statewide_CANAD83")

# Load in the CA boundary
ca.shp <- readOGR("Data/CA_Counties") 
  
# List all different Vegetation Types
sort(unique(shp.1930s$WHR1_TYPE))

# Make a shapefile that's only the Low Sage types and plot it
lowsage.shp <- shp.1930s[shp.1930s$WHR1_TYPE == "Low Sage",]
sp::plot(lowsage.shp)

# Do the same for the other types


coastalscrub.shp <- shp.1930s[shp.1930s$WHR1_TYPE == "Coastal Scrub",]
# Convert the CRS of shapefile to the same as the CA one so that you can plot them together
coastalscrub.shp <- spTransform(coastalscrub.shp, crs(ca.shp))

coastalscrub.shp <- spTransform("coastalscrub.shp", CRS("+ca.shp"))

sp::plot(ca.shp)
sp::plot(coastalscrub.shp, add = T)


sagebrush.shp <- shp.1930s[shp.1930s$WHR1_TYPE == "Sagebrush",]
sagebrush.shp <- spTransform(sagebrush.shp, crs(ca.shp))
sp::plot(ca.shp)
sp::plot(sagebrush.shp, add = T)



# We can write a new shapefile after we subset the vegetation we want!
# Run the following to pull up the 'help' page to see exactly what you need to enter

#coastalscrub.csv <- write.csv(coastalscrub.shp) # I Fucked up and gave you the wrong function
# This is the proper Function
?writeOGR()


# Create a directory to hold the shapefiles
dir.create("Data/Weislander_Sage") 
# Write the shapefile to that directory
writeOGR(coastalscrub.shp, dsn = "Data/Weislander_Sage/", layer = "Weislander_Sage", driver = "ESRI Shapefile")


# Now try and read in the CALVEG Data! It should be the same


<<<<<<< HEAD
north.shp <- readOGR("CALVEG/NorthCoast")
#still having issues with this, will run again for ~ 1 hr, if no resolve will ask about using offline cp thing
#worked in approx 10 min when first thing ran

sort(unique(north.shp$WHR1_TYPE)) #this didnt work. probs bc its spatial data?
sp::plot(north.shp)

central.shp <- readOGR("CALVEG/CentralCoast")

south.shp <- readOGR('CALVEG/SouthCoast')
=======
south_calveg.shp <- readOGR("Data/CALVEG/SouthCoast/")
south_calveg_sage.shp <- south_calveg.shp[south_calveg.shp$CWHR_TYPE == "CSC",] # CSC is the code that CALVEG uses for Coastal Scrub
south_calveg_sage.shp <- spTransform(south_calveg_sage.shp, crs(coastalscrub.shp))
sp::plot(ca.shp)
sp::plot(south_calveg_sage.shp, col = "blue", add = T)
>>>>>>> 7844bf9cdbb2e43ec1910f41dd0a67b260ac306a

