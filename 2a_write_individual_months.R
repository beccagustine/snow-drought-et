library(terra)
library(lubridate)

swe_files <- list.files("D:/NASA/SBG Drought/TerraClim/",
                        pattern = "vpd", full.names = TRUE)

for (file in swe_files) {
  
  tmp <- rast(file)
  name <- tools::file_path_sans_ext(basename(file))
  print(name)
  
  for (band in 1:nlyr(tmp)) {
    
    tmp2 <- tmp[[band]]
    month <- month(time(tmp2))
    month2 <- ifelse((month - 10) < 0,paste(0,month,sep=""),month)
    
    out <- paste0(
      "D:/NASA/SBG Drought/TerraClim/1_individual_files/Monthly VPD/",
      name, "_", month2, ".tif"
    )
    
    writeRaster(tmp2, out, overwrite = TRUE)
  }
}
