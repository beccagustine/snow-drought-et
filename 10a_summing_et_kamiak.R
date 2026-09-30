library(raster)
library(dplyr)

#sums up ET anomalies for each gridcell according to drought type
#Run on HPC

#only need to run this once
#et_files <- list.files("./Monthly_AET", pattern = c("_11.tif|_12.tif|_01.tif|_02.tif|_03.tif|_04.tif"),full.names=TRUE)
#print(head(et_files))
#et_norm <-calc(stack(et_files),fun=mean,na.rm=TRUE)
#writeRaster(et_norm, "./aet_winter_average.tif")

###########################################################
et_norm <- raster("./aet_winter_average.tif")

et_yearly <- list.files("./4_average_yearly_northern/AET",full.names = TRUE)
dry <- as.data.frame(matrix(nrow = 1,ncol=3))
names(dry) <- c("x","y","et")
warm <- as.data.frame(matrix(nrow = 1,ncol=3))
names(warm) <- c("x","y","et")
wd <- as.data.frame(matrix(nrow = 1,ncol=3))
names(wd) <- c("x","y","et")
none <- as.data.frame(matrix(nrow = 1,ncol=3))
names(none) <- c("x","y","et")

for (file in et_yearly) {
  
  tmp <- raster(file)
  anom <- tmp - et_norm
  year <- strsplit(strsplit(file,"/")[[1]][4],".tif")[[1]][1]
  drought <- raster(paste("./6_snow_droughts_northern/modified_boundaries/",year,".tif",sep=""))
  anom_cropped <- crop(anom,drought)
  anom_sd <- stack(anom_cropped,drought)
  anom.df <- as.data.frame(anom_sd, xy=TRUE)
  names(anom.df) <- c("x","y","et","drought")

  tmp_dry <- anom.df %>% filter(drought == 1)
  tmp_warm <- anom.df %>% filter(drought == 3)
  tmp_warmdry <- anom.df %>% filter(drought == 4)
  tmp_na <- anom.df %>% filter(drought == 2)
  
  dry <- merge(dry, tmp_dry, by = c("x","y"), all=TRUE)
  names(dry) <- c("x","y","et1","et2")
  dry$et_final <- rowSums(dry[, c("et1", "et2")],na.rm=TRUE)
  dry <- dry %>% select(x,y,et_final)
  print("Dry:")
  print(head(dry))
  
  warm <- merge(warm, tmp_warm, by = c("x","y"), all=TRUE)
  names(warm) <- c("x","y","et1","et2")
  warm$et_final <- rowSums(warm[, c("et1", "et2")],na.rm=TRUE)
  warm <- warm %>% select(x,y,et_final)
  print("Warm:")
  print(head(warm))
  
  wd <- merge(wd, tmp_warmdry, by = c("x","y"), all=TRUE)
  names(wd) <- c("x","y","et1","et2")
  wd$et_final <- rowSums(wd[, c("et1", "et2")],na.rm=TRUE)
  wd <- wd %>% select(x,y,et_final)
  print("WD:")
  print(head(wd))
  
  none <- merge(none, tmp_na, by = c("x","y"), all=TRUE)
  names(none) <- c("x","y","et1","et2")
  none$et_final <- rowSums(none[, c("et1", "et2")],na.rm=TRUE)
  none <- none %>% select(x,y,et_final)
  print("None:")
  print(head(none))
  
}


write.csv(dry,"./dry_cell_et_sums.csv")
write.csv(warm,"./warm_cell_et_sums.csv")
write.csv(wd,"./warmdry_cell_et_sums.csv")
write.csv(none,"./nodrought_cell_et_sums.csv")














