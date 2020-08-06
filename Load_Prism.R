#lets looks at PRISM Data :) 
install.packages("devtools")
library(devtools)
library(reshape2)
library(dplyr)
library(raster)
library(sp)
library(rgdal)
#installing prism 
install.packages("prism")
library(prism)

??prism
prism.path = "Prism1"
prism::get_prism_normals(type = "ppt",
                         resolution = "4km",
                         mon = NULL,
                         annual = TRUE,
                         keepZip = TRUE)
#get prism data for the year 1930 for ppt, tmean, tmin, tmax
prism::get_prism_annual(type = "ppt",
                        years = 1930,
                        keepZip = TRUE)

prism::get_prism_annual(type = "tmin",
                        years = 1930,
                        keepZip = TRUE)

prism::get_prism_annual(type = "tmax",
                        years = 1930,
                        keepZip = TRUE)

prism::get_prism_annual(type = "ppt",
                        years = 1930,
                        keepZip = TRUE)


#confirming we have 1930 data for all 4 variables 
head(prism::ls_prism_data(name = TRUE))
class(prism::ls_prism_data(name = TRUE))

#reading the data - dataframes? - and putting it into variable 
ppt_1930 <- raster::raster("prism.path = Prism1/PRISM_ppt_stable_4kmM2_1930_bil/PRISM_ppt_stable_4kmM2_1930_bil.bil")
plot(ppt_1930)

#loads specific info when all files for month 01 are placed into a folder 
ppt_1930_01 <- raster::raster("Prism Data 1930/PRISM ppt1930s/PRISM_ppt_stable_4kmM2_193001_bil/PRISM_ppt_stable_4kmM2_193001_bil.bil")
plot(ppt_1930_01)
