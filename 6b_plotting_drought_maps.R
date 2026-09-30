library(rgdal)
library(ggplot2)
library(broom)
library(sf)

#this is just to visualize the data to inspect it but not used for any paper figure creation

cont <- st_read("D:/NASA/SBG Drought/shapefiles/World Continents/World_Continents.shp",layer="World_Continents")

test <- as.data.frame(raster("D:/NASA/SBG Drought/TerraClim/6_snow_droughts/northern/modified_boundaries/1959.tif"),xy=TRUE)
names(test)[3] <- "Drought"
test$Drought <- as.factor(test$Drought)
levels(test$Drought) <- list(Dry  = "1", Warm = "3", WarmDry = "4", NoDrought = "2")
test <- test[which(is.na(test$Drought) == FALSE),]
# drought <- list.files("D:/NASA/SBG Drought/TerraClim/6_snow_droughts/northern/", pattern = ".csv", full.names = TRUE)
# test <- read.csv(drought[36])

p <- ggplot(cont) + geom_sf(fill="white", color="black") + theme_void()

p + geom_raster(data = test, aes(fill = Drought, x = x, y = y)) +
  labs(x = "", y = "", fill = "") + #change here for title
  ylim(20,90)+
  scale_fill_npg(na.value = "NA") +
  theme_light() +
  theme(legend.position = c(0.075,0.2),
        legend.text = element_text(size = 20),
        legend.background = element_blank(), 
        panel.grid = element_blank()
        )
