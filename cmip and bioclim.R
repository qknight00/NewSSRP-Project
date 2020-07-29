#cleaning and looking at bioclim data 

head(bioclim.data)
str(list.len(bioclim.data)
summary(bioclim.data)
plot(bioclim.data)
names(bioclim.data)
nlayers(bioclim.data)
names(bioclim.data) <- 1:19
names(bioclim.data)
#cmip data 

head(cmip5.data)
summary(cmip5.data)
plot(cmip5.data)
names(cmip5.data)
nlayers(cmip5.data)
names(cmip5.data) <- 1:19


#setting them equal 
names(bioclim.data) <- names(cmip5.data)
names(bioclim_11.data) <- names(cmip5_11.data)


?names
