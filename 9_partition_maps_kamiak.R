library(raster)
library(dplyr)

et_yearly <- list.files("./4_average_yearly/northern/AET/",full.names = TRUE)

dry <- as.data.frame(matrix(nrow = 1,ncol=2))
names(dry) <- c("x","y")
warm <- as.data.frame(matrix(nrow = 1,ncol=2))
names(warm) <- c("x","y")
wd <- as.data.frame(matrix(nrow = 1,ncol=2))
names(wd) <- c("x","y")
none <- as.data.frame(matrix(nrow = 1,ncol=2))
names(none) <- c("x","y")

for (file in et_yearly) {
  
  tmp <- raster(file)
  print(tmp)
  year <- strsplit(strsplit(file,"/")[[1]][6],"[.]")[[1]][1]
  print(year)
  p <- raster(paste("./4_average_yearly/northern/Precip/",year,".tif",sep=""))
  ratio <- tmp/p
  print(ratio)
  drought <- raster(paste("./6_snow_droughts/northern_boundaries/",year,".tif",sep=""))
  ratio_cropped <- crop(ratio,drought)
  ratio_sd <- stack(ratio_cropped,drought)
  ratio.df <- as.data.frame(ratio_sd, xy=TRUE)
  names(ratio.df)[3] <- paste(year)
  names(ratio.df)[4] <- "drought"
  print(head(ratio.df))
  
  tmp_dry <- ratio.df %>% filter(drought == 1)%>% select(-c(drought))
  tmp_warm <- ratio.df %>% filter(drought == 3)%>% select(-c(drought))
  tmp_warmdry <- ratio.df %>% filter(drought == 4)%>% select(-c(drought))
  tmp_na <- ratio.df %>% filter(drought == 2) %>% select(-c(drought))

  dry <- merge(dry, tmp_dry, by = c("x","y"), all=TRUE)
  print(head(dry))
  
  warm <- merge(warm, tmp_warm, by = c("x","y"), all=TRUE)
  print(head(warm))
  
  wd <- merge(wd, tmp_warmdry, by = c("x","y"), all=TRUE)
  print(head(wd))
  
  none <- merge(none, tmp_na, by = c("x","y"), all=TRUE)
  print(head(none))
  
}

write.csv(dry,"./dry_cell_partitions.csv")
write.csv(warm,"./warm_cell_partitions.csv")
write.csv(wd,"./warmdry_cell_partitions.csv")
write.csv(none,"./nodrought_cell_partitions.csv")














