library(raster)

#this removes the boundaries from around continuous drought areas to 
#account for random pixels that were singular

files <- list.files("D:/NASA/SBG Drought/TerraClim/6_snow_droughts/northern/",pattern = ".tif", full.names = TRUE)

for (file in files) {
  
  test <- raster(file)
  test2 <-boundaries(test, type = "outer", classes = TRUE, directions = 8) #this is to remove transition areas between drought types
  test[test2==1] <- NA
  
  test3 <- boundaries(test,type = "inner",directions = 8, classes = FALSE) #this is to remove random pixels that were by themselves but it also creates some but they used to be continuous so okay
  test[test3==1] <- NA  
  writeRaster(test,paste("D:/NASA/SBG Drought/TerraClim/6_snow_droughts/northern/modified_boundaries/",strsplit(file,"/")[[1]][7],sep=""))

}

