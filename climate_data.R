#all climate data


#pulling bioclim data 
bioclim.data <- getData(name = "worldclim",
                        var = 'bio', 
                        res = 2.5,
                        path = "input/")
bioclim.data <- dropLayer(bioclim.data, c(4,5,6,7,13,16,17,19))
plot(bioclim.data)