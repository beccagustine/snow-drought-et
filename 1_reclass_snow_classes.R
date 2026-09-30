library(raster)

# snow <- raster("F:/NASA/SBG Drought/Snow Classifications/SnowClass_GL_01km_30.0arcsec_2021_v01.0.tif")
exmp <- raster("D:/NASA/SBG Drought/TerraClim/2_average_clims/P_avg_clim/01.tif")
dem <- raster("D:/NASA/SBG Drought/global_aster_DEM.tif")

reclass <- resample(dem,exmp, method = "bilinear") #"ngb" for snow classes and "bilinear" for DEM
writeRaster(reclass,"D:/NASA/SBG Drought/aster_resampled.tif", overwrite=TRUE)

north <- raster("D:/NASA/SBG Drought/Snow Classifications/snowClass_resample_terraclim_resolution_northern_hem.tif")
cropped <- crop(reclass,north)
writeRaster(cropped,"D:/NASA/SBG Drought/aster_resampled_northernhem.tif")
##if you need to crop, crop reclass and then use this write raster


##included this to just plot easily
grid <- raster("D:/NASA/SBG Drought/TerraClim/2_average_clims/SWE_avg_clim/10.tif")
grid[grid[]>0] <- 1
plot(grid)
