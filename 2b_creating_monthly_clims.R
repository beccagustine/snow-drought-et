library(tidyr)
library(dplyr)
library(raster)

patterns <- c("_01","_02","_03","_04","_05","_06","_07","_08","_09","_10","_11","_12")

#below is for snow only becuase it removes any cells that are < 10 mm of SWE
for (pattern in patterns) {
  files <- list.files("F:/NASA/SBG Drought/TerraClim/Monthly SWE/",pattern = pattern,full.names = TRUE)
  avg <- calc(stack(files[1:35]),fun = mean, na.rm = TRUE)
  avg[avg < 10] <- NA #masks out anything that on average has less than 10 mm of SWE
  writeRaster(avg,paste0("F:/NASA/SBG Drought/TerraClim/SWE_avg_clim/",pattern,".tif"))
  
}

#below is for precipitation and temperature
for (pattern in patterns) {
  pat <- strsplit(pattern,"_")[[1]][2]
  files <- list.files("F:/NASA/SBG Drought/TerraClim/1_individual_files/Monthly Tavg/",pattern = pattern,full.names = TRUE) #change here for P or Tavg
  avg <- calc(stack(files[1:35]),fun = mean, na.rm = TRUE)
  mask <- raster(paste("F:/NASA/SBG Drought/TerraClim/2_average_clims/SWE_avg_clim//",pat,".tif",sep=""))
  masked <- mask(avg,mask,maskvalue=NA)
  writeRaster(masked,paste0("F:/NASA/SBG Drought/TerraClim/2_average_clims/Tavg_avg_clim/",pat,".tif")) #Change here for P_avg_clim or Tavg_avg_clim
  
}


#averaging the tmin and tmax
files <- list.files("F:/NASA/SBG Drought/TerraClim/1_individual_files/Monthly Tmax/",full.names=TRUE)

for (file in files) {
  name <- gsub("max","avg",strsplit(file,"/")[[1]][7])
  max <- raster(file)
  tmp_min <- gsub("max","min", file)
  min <- raster(tmp_min)
  avg <- (max + min)/2
  writeRaster(avg,paste("F:/NASA/SBG Drought/TerraClim/1_individual_files/Monthly Tavg/",name,sep=""))
  
} 


###################################################################################################################
##Budyko analysis specific stuff
##below is to create one grid for the Budyko curve--taking all the area from all months to create one grid area for testing
#different budyko formulations
full <- list.files("D:/NASA/SBG Drought/TerraClim/2_average_clims/SWE_avg_clim/",full.names=TRUE)
sums <- calc(stack(full),fun=sum, na.rm=TRUE)
sums[sums[]>0] <- 1
plot(sums)
writeRaster(sums,"D:/NASA/SBG Drought/TerraClim/aggregate_snow_area.tif")
#################################################################################

