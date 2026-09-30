library(raster)
library(dplyr)

#identifies which years are snow droughts
#Run on an HPC

#if (SWEi < SWEm) & (Pi < Pm) & (TDi < TDm) = DRY
#if (SWEi < SWEm) & (Pi > Pm) = WARM
#if (SWEi < SWEm) & (Pi < Pm) & (TDi > TDm) = WARM & DRY

files <- list.files("./4_average_yearly/northern/SWE/",full.names = TRUE)

for (file in files) {
  swe <- raster(file)
  swe_normal <- raster("./5_winter_normals/northern/swe.tif") #hemisphere
  swe_dif <- swe - swe_normal
  swe.df <- as.data.frame(swe_dif,xy=TRUE)
  names(swe.df)[3] <- "SWE"
  swe.df <- swe.df %>% filter(y >= 0) #hemisphere
  
  temp <- raster(gsub("SWE","Temp",file))
  temp_normal <- raster("./5_winter_normals/northern/temp.tif") #hemisphere
  t_dif <- temp - temp_normal
  t.df <- as.data.frame(t_dif, xy=TRUE)
  names(t.df)[3] <- "T"
  t.df <- t.df %>% filter(y >= 0) #hemisphere
  
  precip <- raster(gsub("SWE","Precip",file))
  precip_normal <- raster("./5_winter_normals/northern/precip.tif") #hemisphere
  p_dif <- precip - precip_normal
  p.df <- as.data.frame(p_dif, xy=TRUE)
  names(p.df)[3] <- "P"
  p.df <- p.df %>% filter(y >= 0) #hemisphere
  
  df1 <- merge(swe.df,t.df, by = c("x","y"))
  df2 <- merge(df1,p.df, by = c("x","y"))
  
  df2$Drought <- "NA"
  df2$Drought <- ifelse(df2$SWE < 0 & df2$P < 0 & df2$T < 0 ,"Dry", df2$Drought)
  df2$Drought <- ifelse(df2$SWE < 0 & df2$P > 0, "Warm", df2$Drought)
  df2$Drought <- ifelse(df2$SWE < 0 & df2$P < 0 & df2$T > 0, "WarmDry", df2$Drought)
  df3 <- df2[which(is.na(df2$SWE) == FALSE),]
  
  df4 <- df2 %>% select(x,y,Drought) #using df2 to have the whole grid
  df4$Drought <- as.factor(df4$Drought)
  df4$id <- as.numeric(df4$Drought)
  df4 <- df4 %>% select(x,y,id)
  df4_rast <- rasterFromXYZ(df4)
  
  year <- strsplit(strsplit(file,"/")[[1]][6],".tif")[[1]][1]
  print(year)
  write.csv(df3,paste0("./6_snow_droughts/northern/",year,".csv",sep=""))
  writeRaster(df4_rast,paste0("./6_snow_droughts/northern/",year,".tif",sep=""))
}


