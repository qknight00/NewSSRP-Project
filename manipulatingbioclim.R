#reducing the layers in bioclim to 11 instead of 19 and running models using 
#those 11 
#there is code to model calveg and bioclim , and weislander and bioclim ,
#may be able to delete modeling done here if found in other scripts .

library("raster")
library("dismo")
library("maptools")
library("sp")
library("rgdal")

#loading specific bioclim data

bioclim.data <- getData(name = "worldclim",
                        var = 'bio', 
                        res = 2.5,
                        path = "input/")

#droplayers from the rasterstack
?dropLayer
bioclim.data <- dropLayer(bioclim.data, c(4,5,6,7,13,16,17,19))

#now equals 11  
nlayers(bioclim.data)
plot(bioclim.data)

#creating model for calveg plus 11 variables from bioclim 


m_calveg_bioclim <- bioclim(bioclim.data,
                            p = all_calveg.pts)

predict_calveg_bioclim <- dismo::predict(m_calveg_bioclim,
                                         x = bioclim.data,
                                         ext = extent(ca.data))
plot(predict_calveg_bioclim, main = "Calveg + 11 Bioclim Variables")


#weislander map using 11 bioclim variables 

model_weislander_llbioclim <- bioclim(bioclim_spec.data,
                                      p = weislander.pts)
predict_weislander_11bioclim <- dismo::predict(model_weislander_llbioclim,
                                               x = bioclim_spec.data,
                                               ext = extent(ca.data))

plot(predict_weislander_11bioclim, 
     main = "Weislander and 11 Bioclim Variables")

#manipulation of variables next 
# changing N = 300 to higher amount 
# understanding the cmip5 variables and things 

#clearning the global enviornment 
ls()
remove(list = ls())


