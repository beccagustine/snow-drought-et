library(raster)
library(dplyr)

#run on HPC
#sums SM with the same approach as ET in previous step

##ONLY NEED TO RUN ONCE
#sm_files <- list.files("./Monthly SM/", pattern = c("_11.tif|_12.tif|_01.tif|_02.tif|_03.tif|_04.tif"),full.names=TRUE)
#print(head(sm_files))

#sm_norm <- mean(stack(sm_files)) 
#writeRaster(sm_norm, "./sm_winter_average.tif")
#####################################################################

sm_yearly <- list.files("./4_average_yearly/northern/SM/",full.names = TRUE)

dry <- as.data.frame(matrix(nrow = 1,ncol=3))
names(dry) <- c("x","y","sm")
warm <- as.data.frame(matrix(nrow = 1,ncol=3))
names(warm) <- c("x","y","sm")
wd <- as.data.frame(matrix(nrow = 1,ncol=3))
names(wd) <- c("x","y","sm")
none <- as.data.frame(matrix(nrow = 1,ncol=3))
names(none) <- c("x","y","sm")
sm_norm <- raster("./sm_winter_average.tif")

for (file in sm_yearly) {
  
  tmp <- raster(file)
  anom <- tmp - sm_norm
  year <- strsplit(strsplit(file,"/")[[1]][6],".tif")[[1]][1]
  print(year)
  drought <- raster(paste("./6_snow_droughts/northern_boundaries/",year,".tif",sep=""))
  print(drought)
  anom_cropped <- crop(anom,drought)
  anom_sd <- stack(anom_cropped,drought)
  anom.df <- as.data.frame(anom_sd, xy=TRUE)
  names(anom.df) <- c("x","y","sm","drought")
  print(head(anom.df))
  
  tmp_dry <- anom.df %>% filter(drought == 1)
  tmp_warm <- anom.df %>% filter(drought == 3)
  tmp_warmdry <- anom.df %>% filter(drought == 4)
  tmp_na <- anom.df %>% filter(drought == 2)
  
  dry <- merge(dry, tmp_dry, by = c("x","y"), all=TRUE)
  print("head after merge on dry to check col names:")
  print(head(dry))
  names(dry) <- c("x","y","sm1","sm2") #a drought column is also here
  dry$sm_final <- rowSums(dry[, c("sm1", "sm2")],na.rm=TRUE)
  dry <- dry %>% select(x,y,sm_final)
  print("printing dry after row sums and selecting sm_final:")
  print(head(dry))
  
  warm <- merge(warm, tmp_warm, by = c("x","y"), all=TRUE)
  names(warm) <- c("x","y","sm1","sm2")
  warm$sm_final <- rowSums(warm[, c("sm1", "sm2")],na.rm=TRUE)
  warm <- warm %>% select(x,y,sm_final)
  
  wd <- merge(wd, tmp_warmdry, by = c("x","y"), all=TRUE)
  names(wd) <- c("x","y","sm1","sm2")
  wd$sm_final <- rowSums(wd[, c("sm1", "sm2")],na.rm=TRUE)
  wd <- wd %>% select(x,y,sm_final)
  
  none <- merge(none, tmp_na, by = c("x","y"), all=TRUE)
  names(none) <- c("x","y","sm1","sm2")
  none$sm_final <- rowSums(none[, c("sm1", "sm2")],na.rm=TRUE)
  none <- none %>% select(x,y,sm_final)
  
}

print("writing:")
write.csv(dry,"./dry_cell_sm_sums.csv")
write.csv(warm,"./warm_cell_sm_sums.csv")
write.csv(wd,"./warmdry_cell_sm_sums.csv")
write.csv(none,"./nodrought_cell_sm_sums.csv")














