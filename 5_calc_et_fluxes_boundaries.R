library(raster)
library(dplyr)
library(ggplot2)

#extracts AET, soil moisture, etc data by snow drought type using the modified boundaries maps

swe <- list.files("D:/NASA/SBG Drought/TerraClim/6_snow_droughts/northern/modified_boundaries/",full.names = TRUE)
classes <- raster("D:/NASA/SBG Drought/Snow Classifications/snowClass_resample_terraclim_resolution.tif")
sd <- raster("D:/NASA/SBG Drought/TerraClim/6_snow_droughts/northern/modified_boundaries/1959.tif")
snow_class_cropped <- crop(classes,sd)


for (file in swe) {
  year <- strsplit(strsplit(file,"/")[[1]][8],".tif")[[1]][1]
  tmp <- raster(file)
  
  aet <- raster(paste("D:/NASA/SBG Drought/TerraClim/4_average_yearly/northern/SM/",year,".tif",sep="")) #changed here to SM
  aet_cropped <- crop(aet,tmp)
  
  rast_brick <- brick(tmp,aet_cropped,snow_class_cropped)
  brick.df <- as.data.frame(rast_brick, xy=TRUE)
  names(brick.df) <- c("x","y","SnowDrought","SM","SnowClassification")
  aet_swe_class2 <- brick.df %>% filter(SnowClassification < 8) #removing 8 because dont want to look at ice
  final <- aet_swe_class2[which(is.na(aet_swe_class2$SnowDrought)==FALSE),]
  write.csv(final, paste0("D:/NASA/SBG Drought/TerraClim/7b_sm_snow_drought/",year,".csv",sep=""))
  
}
