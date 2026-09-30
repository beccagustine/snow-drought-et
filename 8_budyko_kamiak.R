library(raster)
library(dplyr)
library(ggplot2)
library(data.table)
#library(ggpubr)

#calculates budyko anomoalies using different spatial and temporal grouping to test the sensitivity of the outcome
#Run on an HPC because very computationally expensive

options(bitmapType='cairo')
#change here for why type of Budyko curve
#if you want the full year, use /full_year data
#if you want just the winter groupings, use /northern
#path <- "./full_year/northern/" #monthly grid and whole year; if using this one, need to change the indexing below
#path <- "./northern/" #cold season and monthly grid
#path <- "./i_water_year/" #ag grid and whole year
path <- "./ii_cold_season/" #ag grid and cold season

aet <- list.files(paste(path,"AET",sep=""),full.names = TRUE)
print(head(aet))

eco_crop <- raster("./snowClass_resample_terraclim_resolution_northern_hem.tif")
dem <- raster("./aster_resampled_northernhem.tif")

list_band <- list()
list_outs <- list()
list_dem <- list()
list_all <- list()
count = 1

for (file in aet){
  print(file)
  tmp_aet <- raster(file)
  tmp_p <- raster(gsub("AET","Precip",file))
  print(tmp_p)
  tmp_pet <- raster(gsub("AET","PET",file))
  print(tmp_pet)

  drought <-  raster(paste("./modified_boundaries/",strsplit(file,"/")[[1]][4],sep="")) #change here for filepath length; 5 for full year and 4 for northern
  # drought <-  raster(paste("D:/NASA/SBG Drought/TerraClim/6_snow_droughts/northern/modified_boundaries/",strsplit(file,"/")[[1]][8],sep="")) #change here for filepath length
  x_axis <- tmp_pet/tmp_p
  x_crop <- crop(x_axis,drought) #cropping so I can mask them
  y_axis <- tmp_aet/tmp_p
  y_crop <- crop(y_axis,drought) #cropping so I can stack them
  p_crop <- crop(tmp_p,drought)
  pet_crop <- crop(tmp_pet, drought)
  exp <- ((1-exp(-x_crop))*x_crop*tanh(p_crop/pet_crop))^0.5

  maps <- stack(y_crop,x_crop,drought,eco_crop,dem,p_crop,pet_crop,exp)
  maps.df <- na.omit(as.data.frame(maps,xy=TRUE))
  names(maps.df) <- c("x","y","Y_Axis","X_Axis","DroughtType","SnowType","DEM","P","PET","Expected_Raw")
  maps.df$Year <- strsplit(strsplit(file,"/")[[1]][4],".tif")[[1]][1] #change here for filepath length; 4 for /northern/ and 5 for /full_year/norther
  maps.df$band <- as.numeric(ifelse(maps.df$y < 10, "10", paste(substr(maps.df$y,1,1),"0",sep="")))
  maps.df$Elev <- ifelse(maps.df$DEM >= 0 & maps.df$DEM <= 1000, "0-1000m",
                         ifelse(maps.df$DEM >= 1001 & maps.df$DEM <= 2000,"1001-2000m",
                                ifelse(maps.df$DEM >= 2001 & maps.df$DEM <= 3000,"2001-3000m",
                                       ifelse(maps.df$DEM >= 3001 & maps.df$DEM <= 4000,"3001-4000m",
                                              ifelse(maps.df$DEM >= 4001 & maps.df$DEM <= 5000,"4001-5000m",">5000m")))))
  
  print(head(maps.df))
  out <- maps.df[which(is.infinite(maps.df$Y_Axis) == FALSE),]
  out2 <- out %>% select(-c(x,y,band,DEM,Elev)) %>% group_by(Year,DroughtType,SnowType) %>% 
    summarize_all(mean,na.rm=TRUE)
  
  out_band <- out %>% select(-c(x,y,DEM,Elev)) %>% group_by(Year,band,DroughtType,SnowType) %>% 
    summarize_all(mean,na.rm=TRUE)

  out_elev <- out %>% select(-c(x,y,DEM,SnowType))%>% group_by(Year,band,DroughtType,Elev) %>%
    summarize_all(mean,na.rm=TRUE)
  
  out_all <- out %>% select(-c(x,y,DEM))%>% group_by(Year,band,DroughtType,Elev,SnowType) %>%
    summarize_all(mean,na.rm=TRUE)
  
  list_outs[[count]] <- out2
  list_band[[count]] <- out_band
  list_dem[[count]] <- out_elev
  list_all[[count]] <- out_all
  
  count = count +1
}

rm("drought","out","maps.df","maps","x_crop","y_crop","y_axis","x_axis","tmp_aet","tmp_p","tmp_pet","exp","pet_crop",
   "aet_crop","p_crop")

#################################
# saveRDS(list_outs, file="all.RData")
# saveRDS(list_band, file="band.RData")
# saveRDS(list_dem, file="dem.RData")

all.df <- as.data.frame(rbindlist(list_outs))
all.df <- all.df %>% filter(SnowType < 7) #removing 7 (Ice) and 8 (NA)

all.df$DroughtType <- factor(all.df$DroughtType, levels = c("1", "2", "3","4"), 
                  labels = c("Dry",
                             "NoDrought",
                             "Warm",
                             "WarmDry"))
all.df$DroughtType <- factor(all.df$DroughtType, levels=c('Dry', 'Warm', 'WarmDry', 'NoDrought'))

all.df$SnowType <- factor(all.df$SnowType, levels = c("1","2","3","4","5","6"),
                  labels = c("Tundra",
                             "Boreal\nForest",
                             "Maritime",
                             "Ephemeral\n(or no Snow)",
                             "Prairie",
                             "Montane\nForest"))

all.df$exp <- ((1-exp(-all.df$X_Axis))*all.df$X_Axis*tanh(all.df$P/all.df$PET))^0.5

#comparing if the in-loop then avg or post loop average is different
print("In-loop:")
print(summary(all.df$Expected_Raw))

print("Out of loop:")
print(summary(all.df$exp))
#######################

 # out_plot <- ggplot(all.df) + geom_point(aes(x=X_Axis,y=Y_Axis, color=SnowType),size = 2.5) +
 #   facet_grid(DroughtType~SnowType)+ 
 #   ylim(0,1) + xlim(0,6) +
 #   labs(x = "PET/P",y="ET/P") +
 #   geom_line(data=curve,aes(x=X_Axis,y=Expected_Raw))+
 #   geom_segment(aes(x = 1,y=1,xend = 6,yend=1))+
 #   geom_segment(aes(x=0,y=0,xend=1,yend=1)) +
 #   geom_segment(aes(x=1,y=0,xend=1,yend=1),linetype="dotted")+
 #   theme_classic() +
 #   theme(legend.position = "none", 
 #         strip.background = element_rect(color = "NA"),
 #         text = element_text(size = 33)) 
 # 
 # png("1_budyko_all.png",width = 1300, height = 800,units = "px")
 # print(out_plot)
 # dev.off()
 # 
 # out_plot2 <- ggplot(all.df) + geom_point(aes(x=X_Axis,y=Y_Axis, color=as.factor(DroughtType)),size = 2.5) +
 #   facet_grid(.~SnowType)+ ylim(0,1) + xlim(0,6) +
 #   labs(x = "PET/P",y="ET/P",color="Drought\nType") +
 #   geom_line(data=curve,aes(x=X_Axis,y=Expected_Raw))+
 #   geom_segment(aes(x = 1,y=1,xend = 6,yend=1))+
 #   geom_segment(aes(x=0,y=0,xend=1,yend=1)) +
 #   geom_segment(aes(x=1,y=0,xend=1,yend=1),linetype="dotted")+
 #   theme_classic() +
 #   theme(legend.position = "none", 
 #         strip.background = element_rect(color = "NA"),
 #         text = element_text(size = 33)) 
 # 
 # png("2_budyko_droughts_grouped.png",width = 1300, height = 800,units = "px")
 # print(out_plot2)
 # dev.off()
 # 
 # 
 # out_plot3 <- ggplot(all.df) + geom_point(aes(x=X_Axis,y=Y_Axis, color=as.factor(SnowType)),size = 2.5) +
 #   facet_grid(.~DroughtType)+ ylim(0,1) + xlim(0,6) +
 #   labs(x = "PET/P",y="ET/P",color="Snow\nClass") +
 #   geom_line(data=curve,aes(x=X_Axis,y=Expected_Raw))+
 #   geom_segment(aes(x = 1,y=1,xend = 6,yend=1))+
 #   geom_segment(aes(x=0,y=0,xend=1,yend=1)) +
 #   geom_segment(aes(x=1,y=0,xend=1,yend=1),linetype="dotted")+
 #   theme_classic() +
 #   theme(strip.background = element_rect(color = "NA"),
 #         text = element_text(size = 33)) 
 # 
 # png("3_budyko_ecosystems_grouped.png",width = 1300, height = 800,units = "px")
 # print(out_plot3)
 # dev.off()

# band.df <- as.data.frame(rbindlist(list_band))
# band.df <- band.df %>% filter(SnowType < 7) #removing 7 (Ice) and 8 (NA--I think as of now, will validate later 7/2/24)
# band.df$band_group <- ifelse(band.df$band >= 0 & band.df$band <= 40, "TwentytoFourty","FiftytoEighty")
# band.df <- band.df %>% filter(Y_Axis <= 1)
# band.df$DroughtType <- factor(band.df$DroughtType, levels = c("1", "2", "3","4"), 
#                              labels = c("Dry",
#                                         "NA",
#                                         "Warm",
#                                         "WarmDry"))
# band.df$DroughtType <- factor(band.df$DroughtType, levels=c('Dry', 'Warm', 'WarmDry', 'NA'))
# 
# band.df$SnowType <- factor(band.df$SnowType, levels = c("1","2","3","4","5","6"),
#                           labels = c("Tundra",
#                                      "Boreal\nForest",
#                                      "Maritime",
#                                      "Ephemeral\n(or no Snow)",
#                                      "Prairie",
#                                      "Montane\nForest"))
# band.df$exp <- ((1-exp(-band.df$X_Axis))*band.df$X_Axis*tanh(band.df$P/band.df$PET))^0.5
 
 
 # out_plot4 <- ggplot(band.df) + geom_point(aes(x=X_Axis,y=Y_Axis, color=as.factor(band)),size = 2.5) +
 #   facet_grid(band_group~DroughtType)+ ylim(0,1) + xlim(0,6) +
 #   labs(x = "PET/P",y="ET/P",color="Latitude\nBand") +
 #   geom_line(data=curve,aes(x=X_Axis,y=Expected_Raw))+
 #   geom_segment(aes(x = 1,y=1,xend = 6,yend=1))+
 #   geom_segment(aes(x=0,y=0,xend=1,yend=1)) +
 #   geom_segment(aes(x=1,y=0,xend=1,yend=1),linetype="dotted")+
 #   theme_classic() +
 #   theme(strip.background = element_rect(color = "NA"),
 #         text = element_text(size = 33))
 # 
 # png("4_budyko_bands_grouped.png",width = 1300, height = 800,units = "px")
 # print(out_plot4)
 # dev.off()
 # 
 # out_plot5 <- ggplot(band.df) + geom_point(aes(x=X_Axis,y=Y_Axis, color=as.factor(band)),size = 2.5) +
 #   facet_grid(band_group~SnowType)+ ylim(0,1) + xlim(0,6) +
 #   labs(x = "PET/P",y="ET/P",color="Latitude\nBand") +
 #   geom_line(data=curve,aes(x=X_Axis,y=Expected_Raw))+
 #   geom_segment(aes(x = 1,y=1,xend = 6,yend=1))+
 #   geom_segment(aes(x=0,y=0,xend=1,yend=1)) +
 #   geom_segment(aes(x=1,y=0,xend=1,yend=1),linetype="dotted")+
 #   theme_classic() +
 #   theme(strip.background = element_rect(color = "NA"),
 #         text = element_text(size = 33))
 # 
 # png("5_budyko_latitude_snowtype_bands_grouped.png",width = 1300, height = 800,units = "px")
 # print(out_plot5)
 # dev.off()
 # 
 
 
 
 # dem.df <- as.data.frame(rbindlist(list_dem))
 # dem.df$band_group <- ifelse(dem.df$band >= 0 & dem.df$band <= 49, "TwentytoFourty","FiftytoEighty")
 # dem.df <- dem.df %>% filter(Y_Axis <= 1)
 # dem.df$DroughtType <- factor(dem.df$DroughtType, levels = c("1", "2", "3","4"),
 #                              labels = c("Dry",
 #                                         "NA",
 #                                         "Warm",
 #                                         "WarmDry"))
 # dem.df$DroughtType <- factor(dem.df$DroughtType, levels=c('Dry', 'Warm', 'WarmDry', 'NA'))
 # dem.df$exp <- ((1-exp(-dem.df$X_Axis))*dem.df$X_Axis*tanh(dem.df$P/dem.df$PET))^0.5
 
#Year,band,DroughtType,Elev
 # out_plot8 <- ggplot(dem.df) + geom_point(aes(x=X_Axis,y=Y_Axis, color=as.factor(band)),size = 2.5) +
 #   facet_grid(Elev~DroughtType)+ ylim(0,1) + xlim(0,6) +
 #   labs(x = "PET/P",y="ET/P",color="Latitude\nBand") +
 #   geom_line(data=curve,aes(x=X_Axis,y=Expected_Raw))+
 #   geom_segment(aes(x = 1,y=1,xend = 6,yend=1))+
 #   geom_segment(aes(x=0,y=0,xend=1,yend=1)) +
 #   geom_segment(aes(x=1,y=0,xend=1,yend=1),linetype="dotted")+
 #   theme_classic() +
 #   theme(strip.background = element_rect(color = "NA"),
 #         text = element_text(size = 33))
 # 
 # png("8_budyko_elevation_droughttype_bands.png",width = 1300, height = 800,units = "px")
 # print(out_plot8)
 # dev.off()
 # 
 # 
 # out_plot6 <- ggplot(dem.df) + geom_point(aes(x=X_Axis,y=Y_Axis, color=as.factor(DroughtType)),size = 2.5) +
 #   facet_grid(Elev~band)+ ylim(0,1) + xlim(0,6) +
 #   labs(x = "PET/P",y="ET/P",color="Drought\nType") +
 #   geom_line(data=curve,aes(x=X_Axis,y=Expected_Raw))+
 #   geom_segment(aes(x = 1,y=1,xend = 6,yend=1))+
 #   geom_segment(aes(x=0,y=0,xend=1,yend=1)) +
 #   geom_segment(aes(x=1,y=0,xend=1,yend=1),linetype="dotted")+
 #   theme_classic() +
 #  theme(strip.background = element_rect(color = "NA"),
 #         text = element_text(size = 33))
 # png("6_budyko_elevation_bands_snowtype.png",width = 1300, height = 800,units = "px")
 # print(out_plot6)
 # dev.off()
 # 
 # out_plot7 <- ggplot(dem.df) + geom_point(aes(x=X_Axis,y=Y_Axis, color=as.factor(Elev)),size = 2.5) +
 #   facet_grid(DroughtType~band)+ ylim(0,1) + xlim(0,6) +
 #   labs(x = "PET/P",y="ET/P",color="Elevation\nBand") +
 #   geom_line(data=curve,aes(x=X_Axis,y=Expected_Raw))+
 #   geom_segment(aes(x = 1,y=1,xend = 6,yend=1))+
 #   geom_segment(aes(x=0,y=0,xend=1,yend=1)) +
 #   geom_segment(aes(x=1,y=0,xend=1,yend=1),linetype="dotted")+
 #   theme_classic() +
 #   theme(strip.background = element_rect(color = "NA"),
 #         text = element_text(size = 33))
 # 
 # png("7_budyko_band_SnowTypes_elevation.png",width = 1300, height = 800,units = "px")
 # print(out_plot7)
 # dev.off()

##########################################################################################
################################Anoms boxplots
# all.df$anom <- all.df$exp - all.df$Y_Axis
# out_plotb <- ggplot(all.df) + geom_boxplot(aes(y=anom, color=as.factor(SnowType))) +
#   facet_grid(DroughtType~SnowType, scales="free")+
#   geom_hline(yintercept=0)+ 
#   labs(y="Budyko Anomaly") +
#   theme_classic() +
#   theme(legend.position = "none",
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         strip.background = element_rect(color = "NA"),
#         text = element_text(size = 33)) 
# 
# png("1b_anoms_all.png",width = 1300, height = 800,units = "px")
# print(out_plotb)
# dev.off()
# 
# out_plot2b <- ggplot(all.df) + geom_boxplot(aes(y=anom,x=DroughtType, color=as.factor(DroughtType))) +
#   facet_wrap(.~SnowType, scales="free")+
#   geom_hline(yintercept=0)+
#   labs(y="Budyko Anomaly",color="Drought\nType") +
#   theme_classic() +
#   theme(axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(), 
#         strip.background = element_rect(color = "NA"),
#         text = element_text(size = 33)) 
# 
# png("2b_anoms_droughts_grouped.png",width = 1300, height = 800,units = "px")
# print(out_plot2b)
# dev.off()
# 
# 
# out_plot3b <- ggplot(all.df) + geom_boxplot(aes(y=anom,x=SnowType, color=as.factor(SnowType))) +
#   facet_wrap(.~DroughtType, scales="free")+
#   geom_hline(yintercept=0)+
#   labs(y="Budyko Anomaly",color="Snow\nClass") +
#   theme_classic() +
#   theme(axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         strip.background = element_rect(color = "NA"),
#         text = element_text(size = 33)) 
# 
# png("3b_anoms_ecosystems_grouped.png",width = 1300, height = 800,units = "px")
# print(out_plot3b)
# dev.off()
# 
# ###############################
# band.df$anom <- band.df$exp - band.df$Y_Axis
# out_plot4b <- ggplot(band.df) + geom_boxplot(aes(y=anom, color=as.factor(band)))+
#   facet_grid(band_group~DroughtType, scales="free")+
#   geom_hline(yintercept=0)+
#   labs(y="Budyko Anomaly",color="Latitude\nBand") +
#   theme_classic() +
#   theme(strip.background = element_rect(color = "NA"),
#         text = element_text(size = 33),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank())
# 
# png("4b_anoms_bands_grouped.png",width = 1300, height = 800,units = "px")
# print(out_plot4b)
# dev.off()
# 
# out_plot5b <- ggplot(band.df) + geom_boxplot(aes(y=anom, color=as.factor(band))) +
#   facet_grid(band_group~SnowType, scales="free")+
#   geom_hline(yintercept=0)+
#   labs(y="Budyko Anomaly",color="Latitude\nBand") +
#   theme_classic() +
#   theme(strip.background = element_rect(color = "NA"),
#         text = element_text(size = 33),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank())
# 
# png("5b_anoms_latitude_snowtype_bands_grouped.png",width = 1300, height = 800,units = "px")
# print(out_plot5b)
# dev.off()
# 
# ############################
# dem.df$anom <- dem.df$exp - dem.df$Y_Axis
# out_plot8b <- ggplot(dem.df) + geom_boxplot(aes(y=anom, color=as.factor(band))) +
#   facet_grid(Elev~DroughtType, scales="free")+
#   geom_hline(yintercept=0)+
#   labs(y="Budyko Anomaly",color="Latitude\nBand") +
#   theme_classic() +
#   theme(strip.background = element_rect(color = "NA"),
#         text = element_text(size = 33),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank())
# 
# png("8b_anoms_elevation_droughttype_bands.png",width = 1300, height = 800,units = "px")
# print(out_plot8b)
# dev.off()
# 
# #
# out_plot6b <- ggplot(dem.df) + geom_boxplot(aes(y=anom, color=as.factor(DroughtType))) +
#   facet_grid(Elev~band, scales="free")+
#   geom_hline(yintercept=0)+ 
#   labs(y="Budyko Anomaly",color="Drought\nType") +
#   theme_classic() +
#   theme(strip.background = element_rect(color = "NA"),
#         text = element_text(size = 33),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank())
# 
# png("6b_anoms_elevation_bands_snowtype.png",width = 1300, height = 800,units = "px")
# print(out_plot6b)
# dev.off()
# 
# out_plot7b <- ggplot(dem.df) + geom_boxplot(aes(y=anom, color=as.factor(Elev))) +
#   facet_grid(DroughtType~band, scales="free")+
#   geom_hline(yintercept=0)+ 
#   labs(y="Budyko Anomaly",color="Elevation\nBand") +
#   theme_classic() +
#   theme(strip.background = element_rect(color = "NA"),
#         text = element_text(size = 33),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank())
# 
# png("7b_anoms_band_SnowTypes_elevation.png",width = 1300, height = 800,units = "px")
# print(out_plot7b)
# dev.off()

##this is plotting all the variables and creating dry, warm, warm&dry, and no drought plots then grouping 
##them together

saveRDS(list_all,file = "all_vars-cold-ag.RData")

sd.df <- as.data.frame(rbindlist(list_all))
print("Head of sd.df:")
print(head(sd.df))
sd.df <- sd.df %>% filter(SnowType < 7) #removing 7 (Ice) and 8 (NA--I think as of now, will validate later 7/2/24)

sd.df$DroughtType <- factor(sd.df$DroughtType, levels = c("1", "2", "3","4"), 
                            labels = c("Dry",
                                       "NoDrought",
                                       "Warm",
                                       "WarmDry"))
sd.df$DroughtType <- factor(sd.df$DroughtType, levels=c('Dry', 'Warm', 'WarmDry', 'NoDrought'))

sd.df$SnowType <- factor(sd.df$SnowType, levels = c("1","2","3","4","5","6"),
                         labels = c("Tundra",
                                    "Boreal\nForest",
                                    "Maritime",
                                    "Ephemeral",
                                    "Prairie",
                                    "Montane\nForest"))

sd.df$Elev <- factor(sd.df$Elev, levels=c('0-1000m', '1001-2000m', '2001-3000m', '3001-4000m','4001-5000m','>5000m'))

sd.df$exp <- ((1-exp(-sd.df$X_Axis))*sd.df$X_Axis*tanh(sd.df$P/sd.df$PET))^0.5

sd.df$anom <- sd.df$exp - sd.df$Y_Axis

print(head(sd.df))
print(tail(sd.df))
print(dim(sd.df))
print(summary(sd.df))

sd_dry <- sd.df %>% filter(DroughtType == "Dry") %>% filter(band < 70)
sd_warm <- sd.df %>% filter(DroughtType == "Warm")%>% filter(band < 70)
sd_wd <- sd.df %>% filter(DroughtType == "WarmDry")%>% filter(band < 70)
sd_no <- sd.df %>% filter(DroughtType == "NoDrought")%>% filter(band < 70)

dry_plot <- ggplot(sd_dry) + geom_boxplot(aes(y=anom, color=as.factor(Elev))) +
  facet_grid(band~SnowType)+
  ylim(-1,1)+
  geom_hline(yintercept=0)+
  labs(y="Budyko Anomaly",color="Elevation\nBand",title="Dry") +
  theme_classic() +
  theme(strip.background = element_rect(color = "NA"),
        text = element_text(size = 33),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust=0.5,size=30))

png("dry.png",width = 1300, height = 800,units = "px")
print(dry_plot)
dev.off()


warm_plot <- ggplot(sd_warm) + geom_boxplot(aes(y=anom, color=as.factor(Elev))) +
  facet_grid(band~SnowType)+
  ylim(-1,1)+
  geom_hline(yintercept=0)+
  labs(y="Budyko Anomaly",color="Elevation\nBand",title="Warm") +
  theme_classic() +
  theme(strip.background = element_rect(color = "NA"),
        text = element_text(size = 33),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust=0.5,size=30))

png("warm.png",width = 1300, height = 800,units = "px")
print(warm_plot)
dev.off()

wd_plot <- ggplot(sd_wd) + geom_boxplot(aes(y=anom, color=as.factor(Elev))) +
  facet_grid(band~SnowType)+
  ylim(-1,1)+
  geom_hline(yintercept=0)+
  labs(y="Budyko Anomaly",color="Elevation\nBand",title="Warm & Dry") +
  theme_classic() +
  theme(strip.background = element_rect(color = "NA"),
        text = element_text(size = 33),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust=0.5,size=30))

png("wd.png",width = 1300, height = 800,units = "px")
print(wd_plot)
dev.off()

nodrought_plot <- ggplot(sd_no) + geom_boxplot(aes(y=anom, color=as.factor(Elev))) +
  facet_grid(band~SnowType)+
  ylim(-1,1)+
  geom_hline(yintercept=0)+
  labs(y="Budyko Anomaly",color="Elevation\nBand",title="No Drought") +
  theme_classic() +
  theme(strip.background = element_rect(color = "NA"),
        text = element_text(size = 33),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust=0.5,size=30))

png("nodrought.png",width = 1300, height = 800,units = "px")
print(nodrought_plot)
dev.off()

#combined <- ggarrange(nodrought_plot,dry_plot,warm_plot,wd_plot,ncol=2,common.legend=TRUE,legend = "bottom")
#png("combined.png",width = 1300, height = 800,units = "px")
#print(combined)
#dev.off()




