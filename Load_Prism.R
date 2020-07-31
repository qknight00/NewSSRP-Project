#lets looks at PRISM Data :) 
install.packages("devtools")
library(devtools)
library(reshape2)
library(dplyr)
library(raster)
library(sp)

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



head(ls_prism_data(name = TRUE))
prism_image(ls_prism_data()[1,1])

#1930 - 1940 climate data 

#downloaded prism 1930s recip data straight from website
summary(file.path("Prism1"))

