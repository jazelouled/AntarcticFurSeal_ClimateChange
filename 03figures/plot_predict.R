library(lubridate)
library(data.table)
library(raster)
library(gridExtra)
library(dplyr)
library(ggplot2)
library(paletteer)
library(sf)
`%notlike%` <- Negate(`%like%`)

output_data <- "C:/Users/Jazel Ouled/Dropbox/PhD/Arctocephalus-gazella/00output/"
output_data <- "~/Dropbox/PhD/Arctocephalus-gazella/00output/"
output_data <- here::here("InputOutput/00output")

sexseason <- "MW"
sp_code <- "GAZ"


# Themes ------------------------------------------------------------------
# Theme for habitat suitability plots
themeMapsCC<-theme_bw()+
  theme(axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.y=element_text(size=12),
        axis.title.x = element_text(size=12),
        legend.text = element_text(size=8),
        legend.title = element_text(face="bold", size=10),
        legend.key.size = unit(0.5, "cm"),
        # legend.background = element_rect(colour ="black", size=0.5, linetype = "dotted"),
        legend.direction = "vertical",
        legend.position = "none",
        plot.margin = ggplot2::margin(1, 1, 1, 1, "cm"), strip.background =element_rect(fill="gray59"),
        strip.text = element_blank(),
        strip.placement = "right",
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Theme for centroid maps
themeMapsCC<-theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x = element_blank(),
        legend.text = element_text(size=8),
        legend.title = element_text(face="bold", size=10),
        legend.key.size = unit(0.5, "cm"),
        # legend.background = element_rect(colour ="black", size=0.5, linetype = "dotted"),
        legend.direction = "vertical",
        legend.position = "none",
        plot.margin = margin(0, 0, 0, 0, "cm"), strip.background =element_rect(fill="gray59"),
        strip.text = element_blank(),
        strip.placement = "right",
        panel.background = element_rect(fill = "aliceblue"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())    

# Get world shapefile from naturalEarth
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
 

   
myplots_j <- list()  # new empty list
myplots <- list()  # new empty list


# Define palettes for rasters ---------------------------------------------
# https://r-charts.com/color-palettes/  
library(paletteer)
colsss <- paletteer_c("grDevices::Viridis", 12) 
viridisfake<- colsss[2:12]
dist_palette <- paletteer_c("grDevices::Blues 3", 15, direction = -1) 
dist_palette <- paletteer_c("ggthemes::Classic Green-Blue", 30, direction = -1)
dist_palette <- paletteer_c("grDevices::Greens 3", 30) 
diff_palette <- paletteer_c("ggthemes::Red-Green-Gold Diverging", 30) 
paletteer_c("ggthemes::Red-Green Diverging", 30)    
diff_palette <-paletteer_c("ggthemes::Red-Blue-White Diverging", 30) 
diff_palette <-paletteer_c("ggthemes::Classic Orange-White-Blue", 5) 
diff_palette <-paletteer_c("ggthemes::Red-Blue-White Diverging", 30) 
# colss <- paletteer_c("grDevices::Blue-Yellow", 60)
# colss <- paletteer_c("grDevices::BluYl", 30)
# colss <- paletteer_c("grDevices::Spectral", 30) 
# colss <- paletteer_c("grDevices::ag_GrnYl", 30) 
 # ------------------------------------------------------------------------
   


# Set directories where predictions are stored ----------------------------
# Prediction directories fot MW, FW and FS
predict_repoMW <- paste0(output_data, "/", "habitat-model-cc/GAZ/MW/brt/full_model/predict_boost")
predict_repoFW <- paste0(output_data, "/", "habitat-model-cc/GAZ/FW/brt/full_model/predict_boost")
predict_repoFS <- paste0(output_data, "/", "habitat-model-cc/GAZ/FS/brt/full_model/predict_boost")
predict_repoFS <- paste0(output_data, "/", "habitat-model-cc/GAZ/FScpf/brt/full_model/predict_boost")
predict_repoFS <- paste0(output_data, "/", "habitat-model-cc/GAZ/FScpf/brt/full_model/predict_boost/NoAcessColonyDistance")


predict_repoMW <- here::here("InputOutput/00output/02habitatModel/MW/BoostedRegressionTrees/full_model/predict_boost/")
predict_repoFW <- here::here("InputOutput/00output/02habitatModel/FW/BoostedRegressionTrees/full_model/predict_boost/")
predict_repoFS <- here::here("InputOutput/00output/02habitatModel/FScpf/BoostedRegressionTrees/full_model/predictions/")
predict_repoFS <- here::here("InputOutput/00output/02habitatModel/FScpf/BoostedRegressionTrees/full_model/predictions/NoAcessColonyDistance/")



# Load prediction TIFS
predict_files <- dir(predict_repoFW, recursive = T, full.names = T, pattern = ".tif")
ssp <- c("ssp126", "ssp370", "ssp585")
ssp_ <- ssp[i]

# Subset predictions by SSP
predict_files_ssp <- predict_files[predict_files %notlike% "cir" & predict_files %like% ssp_]
# predict_files_ssp <- predict_files[predict_files %like% "cir" & predict_files %like% ssp_]



# Processing PRESENT data -------------------------------------------------
# This little section will be used to get all the files with the predictions between 2019 and 2025 to average them and
# use them as "Present"
datestring <- substr(basename(predict_files_ssp), 1, 8)
dates <- ymd(substr(basename(predict_files_ssp), 1, 8))
sss <- datestring[year(ymd(datestring)) >= "2019" & year(ymd(datestring)) <="2025"]

# Loop to put all the files between 2019 and 2025 in a list
files_list <- list()
for (k in 1:length(sss)){
  aa <- subset(predict_files_ssp, predict_files_ssp %like% sss[k])
  # aa <- subset(predict_files_ssp, predict_files_ssp %like% sss[k] & predict_files_ssp %notlike% "NoAcessColonyDistance")
  files_list[[k]] <- aa
  
}

# Loop to average them monthly, so to get the average of 2019, 2020, 2021, 2022, 2023, 2024 and 2025 for every month
mean_predict_list <- list()
for (t in 1:length(unique(month(ymd(datestring))))) {
  bb <- do.call(rbind, files_list)
  bb_ <- subset(bb, month(ymd(substr(basename(bb), 1, 8))) %like% unique(month(ymd(sss)))[t])
  s <- stack(bb_[,])
  mean_predict <- stackApply(s, indices = 1, fun=mean, na.rm =T)
  mean_predict_list[[t]] <- mean_predict
  
}



# Call and average prediction layers ---------------------------------------
# PRESENT ---------------------------------------
# FW. Get mean for early, mid and late winter habitat suitability in the present
earlyWinterPresent <- mean(stack(mean_predict_list[3:5]))
midWinterPresent <-  mean(stack(mean_predict_list[6:8]))
lateWinterPresent <-  mean(stack(mean_predict_list[9:11]))

# MW. Get mean for early, mid and late winter habitat suitability in the present
earlyWinterPresent <- mean(stack(mean_predict_list[1:4]))
midWinterPresent <-  mean(stack(mean_predict_list[5:7]))
lateWinterPresent <-  mean(stack(mean_predict_list[8]))

# FW / MW. Make a list of the averaged early, mid and late winter habitat suitability layers in the present
present <- list(earlyWinterPresent, midWinterPresent, lateWinterPresent)



# Save averaged TIFFS. 
# MW
for (i in 1:length(present)){
  
  r <- present[[i]]
  
  if (i == 1){
  writeRaster(r, paste0(here::here("InputOutput/00output/02habitatModel/MW/HabitatSuitabilityTIFF/"), "00earlyWinterPresentPrediction_MalesNonBreeding.tif"))} else
    if (i == 2){
      writeRaster(r, paste0(here::here("InputOutput/00output/02habitatModel/MW/HabitatSuitabilityTIFF/"), "01midWinterPresentPrediction_MalesNonBreeding.tif"))} else
        if (i == 3){
          writeRaster(r, paste0(here::here("InputOutput/00output/02habitatModel/MW/HabitatSuitabilityTIFF/"), "02lateWinterPresentPrediction_MalesNonBreeding.tif"))} 
}


# FW
for (i in 1:length(present)){
  
  r <- present[[i]]
  
  if (i == 1){
    writeRaster(r, paste0(here::here("InputOutput/00output/02habitatModel/FW/HabitatSuitabilityTIFF/"), "00earlyWinterPresentPrediction_FemalesNonBreeding.tif"), overwrite=T)} else
      if (i == 2){
        writeRaster(r, paste0(here::here("InputOutput/00output/02habitatModel/FW/HabitatSuitabilityTIFF/"), "01midWinterPresentPrediction_FemalesNonBreeding.tif"), overwrite=T)} else
          if (i == 3){
            writeRaster(r, paste0(here::here("InputOutput/00output/02habitatModel/FW/HabitatSuitabilityTIFF/"), "02lateWinterPresentPrediction_FemalesNonBreeding.tif"), overwrite=T)} 
}

# FS strong philopatry
present <- mean(stack(mean_predict_list))
writeRaster(present, paste0(here::here("InputOutput/00output/02habitatModel/FScpf/HabitatSuitabilityTIFF/"), "SummerPresentPrediction_FemalesBreedingStrongPhilopatry.tif"))

# FS relaxed philopatry
present <- mean(stack(mean_predict_list))
writeRaster(present, paste0(here::here("InputOutput/00output/02habitatModel/FScpf/HabitatSuitabilityTIFF/"), "SummerPresentPrediction_FemalesBreedingRelaxedPhilopatry.tif"))


# SSP1-2.6 2100 ---------------------------------------
predict_files_2100_126 <- predict_files[predict_files %notlike% "cir" & predict_files %like% "ssp126" & predict_files %like% "/2100/" ]

# For FScpf strong philopatry
# predict_files_2100_126 <- predict_files[predict_files %notlike% "cir" & predict_files %like% "ssp126" & predict_files %like% "/2100/" & predict_files %notlike% "NoAcessColonyDistance"]


# FW. Get mean for early, mid and late winter habitat suitability in SSP1-2.6 2100
earlyWinter2100_126 <- mean(stack(predict_files_2100_126[3:5]))
midWinter2100_126 <-  mean(stack(predict_files_2100_126[6:8]))
lateWinter2100_126 <-  mean(stack(predict_files_2100_126[9:11]))

# MW. Get mean for early, mid and late winter habitat suitability in SSP1-2.6 2100
earlyWinter2100_126 <- mean(stack(predict_files_2100_126[1:4]))
midWinter2100_126 <-  mean(stack(predict_files_2100_126[5:7]))
lateWinter2100_126 <-  mean(stack(predict_files_2100_126[8]))

# FW / MW. Make a list of the averaged early, mid and late winter habitat suitability layers
predict126 <- list(earlyWinter2100_126, midWinter2100_126, lateWinter2100_126)


# MW
for (i in 1:length(predict126)){
  
  r <- predict126[[i]]
  
  if (i == 1){
    writeRaster(r, paste0(here::here("InputOutput/00output/02habitatModel/MW/HabitatSuitabilityTIFF/"), "00earlyWinterSSP126Prediction_MalesNonBreeding.tif"))} else
      if (i == 2){
        writeRaster(r, paste0(here::here("InputOutput/00output/02habitatModel/MW/HabitatSuitabilityTIFF/"), "01midWinterSSP126Prediction_MalesNonBreeding.tif"))} else
          if (i == 3){
            writeRaster(r, paste0(here::here("InputOutput/00output/02habitatModel/MW/HabitatSuitabilityTIFF/"), "02lateWinterSSP126Prediction_MalesNonBreeding.tif"))} 
}


# FW
for (i in 1:length(predict126)){
  
  r <- predict126[[i]]
  
  if (i == 1){
    writeRaster(r, paste0(here::here("InputOutput/00output/02habitatModel/FW/HabitatSuitabilityTIFF/"), "00earlyWinterSSP126Prediction_FemalesNonBreeding.tif"))} else
      if (i == 2){
        writeRaster(r, paste0(here::here("InputOutput/00output/02habitatModel/FW/HabitatSuitabilityTIFF/"), "01midWinterSSP126Prediction_FemalesNonBreeding.tif"))} else
          if (i == 3){
            writeRaster(r, paste0(here::here("InputOutput/00output/02habitatModel/FW/HabitatSuitabilityTIFF/"), "02lateWinterSSP126Prediction_FemalesNonBreeding.tif"))} 
}



# FS strong philopatry
predict126 <- mean(stack(predict_files_2100_126))
writeRaster(predict126, paste0(here::here("InputOutput/00output/02habitatModel/FScpf/HabitatSuitabilityTIFF/"), "SummerSSP126Prediction_FemalesBreedingStrongPhilopatry.tif"), overwrite = T)

plot(raster(paste0(here::here("InputOutput/00output/02habitatModel/FScpf/HabitatSuitabilityTIFF/"), "SummerSSP585Prediction_FemalesBreedingStrongPhilopatry.tif")))

# FS relaxed philopatry
predict126 <- mean(stack(predict_files_2100_126))
writeRaster(predict126, paste0(here::here("InputOutput/00output/02habitatModel/FScpf/HabitatSuitabilityTIFF/"), "SummerSSP126Prediction_FemalesBreedingRelaxedPhilopatry.tif"), overwrite = T)


# SSP5-8.5 2100 ---------------------------------------
predict_files_2100_585<- predict_files[predict_files %notlike% "cir" & predict_files %like% "ssp585" & predict_files %like% "/2100/" ]
# For FScpf strong philopatry
# predict_files_2100_585 <- predict_files[predict_files %notlike% "cir" & predict_files %like% "ssp585" & predict_files %like% "/2100/" & predict_files %notlike% "NoAcessColonyDistance"]

# FW (0, 0.35). Get mean for early, mid and late winter habitat suitability in SSP5-8.5 2100
earlyWinter2100_585 <- mean(stack(predict_files_2100_585[3:5]))
midWinter2100_585 <-  mean(stack(predict_files_2100_585[6:8]))
lateWinter2100_585 <-  mean(stack(predict_files_2100_585[9:11]))

# MW (0, 0.5). Get mean for early, mid and late winter habitat suitability in SSP5-8.5 2100
earlyWinter2100_585 <- mean(stack(predict_files_2100_585[1:4]))
midWinter2100_585 <-  mean(stack(predict_files_2100_585[5:7]))
lateWinter2100_585 <-  mean(stack(predict_files_2100_585[8]))

# FW / MW. Make a list of the averaged early, mid and late winter habitat suitability layers
predict585 <- list(earlyWinter2100_585, midWinter2100_585, lateWinter2100_585)


# MW
for (i in 1:length(predict585)){
  
  r <- predict585[[i]]
  
  if (i == 1){
    writeRaster(r, paste0(here::here("InputOutput/00output/02habitatModel/MW/HabitatSuitabilityTIFF/"), "00earlyWinterSSP585Prediction_MalesNonBreeding.tif"))} else
      if (i == 2){
        writeRaster(r, paste0(here::here("InputOutput/00output/02habitatModel/MW/HabitatSuitabilityTIFF/"), "01midWinterSSP585Prediction_MalesNonBreeding.tif"))} else
          if (i == 3){
            writeRaster(r, paste0(here::here("InputOutput/00output/02habitatModel/MW/HabitatSuitabilityTIFF/"), "02lateWinterSSP585Prediction_MalesNonBreeding.tif"))} 
}


# FW
for (i in 1:length(predict585)){
  
  r <- predict585[[i]]
  
  if (i == 1){
    writeRaster(r, paste0(here::here("InputOutput/00output/02habitatModel/FW/HabitatSuitabilityTIFF/"), "00earlyWinterSSP585Prediction_FemalesNonBreeding.tif"))} else
      if (i == 2){
        writeRaster(r, paste0(here::here("InputOutput/00output/02habitatModel/FW/HabitatSuitabilityTIFF/"), "01midWinterSSP585Prediction_FemalesNonBreeding.tif"))} else
          if (i == 3){
            writeRaster(r, paste0(here::here("InputOutput/00output/02habitatModel/FW/HabitatSuitabilityTIFF/"), "02lateWinterSSP585Prediction_FemalesNonBreeding.tif"))} 
}



# FS strong philopatry
predict585 <- mean(stack(predict_files_2100_585))
writeRaster(predict585, paste0(here::here("InputOutput/00output/02habitatModel/FScpf/HabitatSuitabilityTIFF/"), "SummerSSP585Prediction_FemalesBreedingStrongPhilopatry.tif"), overwrite = T)

# FS relaxed philopatry
predict585 <- mean(stack(predict_files_2100_585))
writeRaster(predict585, paste0(here::here("InputOutput/00output/02habitatModel/FScpf/HabitatSuitabilityTIFF/"), "SummerSSP585Prediction_FemalesBreedingRelaxedPhilopatry.tif"), overwrite = T)


GG <- raster(paste0(here::here("InputOutput/00output/02habitatModel/FScpf/HabitatSuitabilityTIFF/"), "SummerSSP126Prediction_FemalesBreedingStrongPhilopatry.tif"))
QQ <- raster(paste0(here::here("InputOutput/00output/02habitatModel/FScpf/HabitatSuitabilityTIFF/"), "SummerPresentPrediction_FemalesBreedingStrongPhilopatry.tif"))
plot(GG-QQ, zlim = c(-0.5, 0.5))

GG <- raster(paste0(here::here("InputOutput/00output/02habitatModel/FScpf/HabitatSuitabilityTIFF/"), "SummerSSP126Prediction_FemalesBreedingRelaxedPhilopatry.tif"))
QQ <- raster(paste0(here::here("InputOutput/00output/02habitatModel/FScpf/HabitatSuitabilityTIFF/"), "SummerPresentPrediction_FemalesBreedingRelaxedPhilopatry.tif"))
plot(GG-QQ, zlim = c(-0.5, 0.5))

GG <- raster(paste0(here::here("InputOutput/00output/02habitatModel/FScpf/HabitatSuitabilityTIFF/"), "SummerSSP585Prediction_FemalesBreedingStrongPhilopatry.tif"))
QQ <- raster(paste0(here::here("InputOutput/00output/02habitatModel/FScpf/HabitatSuitabilityTIFF/"), "SummerPresentPrediction_FemalesBreedingStrongPhilopatry.tif"))
plot(GG-QQ, zlim = c(-0.5, 0.5))

GG <- raster(paste0(here::here("InputOutput/00output/02habitatModel/FScpf/HabitatSuitabilityTIFF/"), "SummerSSP585Prediction_FemalesBreedingRelaxedPhilopatry.tif"))
QQ <- raster(paste0(here::here("InputOutput/00output/02habitatModel/FScpf/HabitatSuitabilityTIFF/"), "SummerPresentPrediction_FemalesBreedingRelaxedPhilopatry.tif"))
plot(GG-QQ, zlim = c(-0.5, 0.5))

GG <- raster(paste0(here::here("InputOutput/00output/02habitatModel/FW/HabitatSuitabilityTIFF/"), "00earlyWinterSSP126Prediction_FemalesNonBreeding.tif"))
QQ <- raster(paste0(here::here("InputOutput/00output/02habitatModel/FW/HabitatSuitabilityTIFF/"), "00earlyWinterPresentPrediction_FemalesNonBreeding.tif"))
plot(GG-QQ, zlim = c(-0.5, 0.5))

GG <- raster(paste0(here::here("InputOutput/00output/02habitatModel/FW/HabitatSuitabilityTIFF/"), "01midWinterSSP126Prediction_FemalesNonBreeding.tif"))
QQ <- raster(paste0(here::here("InputOutput/00output/02habitatModel/FW/HabitatSuitabilityTIFF/"), "01midWinterPresentPrediction_FemalesNonBreeding.tif"))
plot(GG-QQ, zlim = c(-0.5, 0.5))

GG <- raster(paste0(here::here("InputOutput/00output/02habitatModel/FW/HabitatSuitabilityTIFF/"), "02lateWinterSSP126Prediction_FemalesNonBreeding.tif"))
QQ <- raster(paste0(here::here("InputOutput/00output/02habitatModel/FW/HabitatSuitabilityTIFF/"), "02lateWinterPresentPrediction_FemalesNonBreeding.tif"))
plot(GG-QQ, zlim = c(-0.5, 0.5))

GG <- raster(paste0(here::here("InputOutput/00output/02habitatModel/FW/HabitatSuitabilityTIFF/"), "00earlyWinterSSP585Prediction_FemalesNonBreeding.tif"))
QQ <- raster(paste0(here::here("InputOutput/00output/02habitatModel/FW/HabitatSuitabilityTIFF/"), "00earlyWinterPresentPrediction_FemalesNonBreeding.tif"))
plot(GG-QQ, zlim = c(-0.5, 0.5))

GG <- raster(paste0(here::here("InputOutput/00output/02habitatModel/FW/HabitatSuitabilityTIFF/"), "01midWinterSSP585Prediction_FemalesNonBreeding.tif"))
QQ <- raster(paste0(here::here("InputOutput/00output/02habitatModel/FW/HabitatSuitabilityTIFF/"), "01midWinterPresentPrediction_FemalesNonBreeding.tif"))
plot(GG-QQ, zlim = c(-0.5, 0.5))

GG <- raster(paste0(here::here("InputOutput/00output/02habitatModel/FW/HabitatSuitabilityTIFF/"), "02lateWinterSSP585Prediction_FemalesNonBreeding.tif"))
QQ <- raster(paste0(here::here("InputOutput/00output/02habitatModel/FW/HabitatSuitabilityTIFF/"), "02lateWinterPresentPrediction_FemalesNonBreeding.tif"))
plot(GG-QQ, zlim = c(-0.5, 0.5))

GG <- raster(paste0(here::here("InputOutput/00output/02habitatModel/MW/HabitatSuitabilityTIFF/"), "00earlyWinterSSP126Prediction_MalesNonBreeding.tif"))
QQ <- raster(paste0(here::here("InputOutput/00output/02habitatModel/MW/HabitatSuitabilityTIFF/"), "00earlyWinterPresentPrediction_MalesNonBreeding.tif"))
plot(QQ-GG, zlim = c(-0.5, 0.5))


GG <- raster(paste0(here::here("InputOutput/00output/02habitatModel/MW/HabitatSuitabilityTIFF/"), "01midWinterSSP126Prediction_MalesNonBreeding.tif"))
QQ <- raster(paste0(here::here("InputOutput/00output/02habitatModel/MW/HabitatSuitabilityTIFF/"), "01midWinterPresentPrediction_MalesNonBreeding.tif"))
plot(QQ-GG, zlim = c(-0.5, 0.5))

GG <- raster(paste0(here::here("InputOutput/00output/02habitatModel/MW/HabitatSuitabilityTIFF/"), "02lateWinterSSP126Prediction_MalesNonBreeding.tif"))
QQ <- raster(paste0(here::here("InputOutput/00output/02habitatModel/MW/HabitatSuitabilityTIFF/"), "02lateWinterPresentPrediction_MalesNonBreeding.tif"))
plot(QQ-GG, zlim = c(-0.5, 0.5))


GG <- raster(paste0(here::here("InputOutput/00output/02habitatModel/MW/HabitatSuitabilityTIFF/"), "00earlyWinterSSP585Prediction_MalesNonBreeding.tif"))
QQ <- raster(paste0(here::here("InputOutput/00output/02habitatModel/MW/HabitatSuitabilityTIFF/"), "00earlyWinterPresentPrediction_MalesNonBreeding.tif"))
plot(QQ-GG, zlim = c(-0.5, 0.5))


GG <- raster(paste0(here::here("InputOutput/00output/02habitatModel/MW/HabitatSuitabilityTIFF/"), "01midWinterSSP585Prediction_MalesNonBreeding.tif"))
QQ <- raster(paste0(here::here("InputOutput/00output/02habitatModel/MW/HabitatSuitabilityTIFF/"), "01midWinterPresentPrediction_MalesNonBreeding.tif"))
plot(QQ-GG, zlim = c(-0.5, 0.5))

GG <- raster(paste0(here::here("InputOutput/00output/02habitatModel/MW/HabitatSuitabilityTIFF/"), "02lateWinterSSP585Prediction_MalesNonBreeding.tif"))
QQ <- raster(paste0(here::here("InputOutput/00output/02habitatModel/MW/HabitatSuitabilityTIFF/"), "02lateWinterPresentPrediction_MalesNonBreeding.tif"))
plot(QQ-GG, zlim = c(-0.5, 0.5))








# List with the mean data of present, SSP1-2.6 and SSP5-8.5. Each element is a list containing the mean for Early, Mid and Late winter (MW and FW), and the summer mean for FS
vv <- list(present, predict126, predict585)
vv <-  unlist(vv)


# Calculate differences 2100 - Present ------------------------------------
# EARLY -------------------------------------------------------------------
# Early winter elements are in positions 1, 4 and 7 from the list (1/3 from present -- 1/3 from mid -- (1/3 fw) and (1/1 mw) from late). This is for MW and FW
early <- list(vv[1], vv[4], vv[7])

# Unlist early list (common step for bot (MW & FW // FS))
early_ <- unlist(early)

# Calculate the difference between 2100 and present for SSP1-2.6
diff126pres_E <- early_[[2]] - early_[[1]]
plot(diff126pres_E)

# Calculate the difference between 2100 and present for SSP5-8.5
diff585pres_E <- early_[[3]] - early_[[1]]
plot(diff585pres_E)



# MID ---------------------------------------------------------------------
# Mid winter elements are in positions 2, 5 and 8 from the list (2/3 from present -- 2/3 from mid -- (2/3 from late). This is for MW and FW
mid <- list(vv[2], vv[5], vv[8])

# Unlist mid list
mid_ <- unlist(mid)

# Calculate the difference between 2100 and present for SSP1-2.6
diff126pres_M <- mid_[[2]] - mid_[[1]]
plot(diff126pres_M)

# Calculate the difference between 2100 and present for SSP5-8.5
diff585pres_M <- mid_[[3]] - mid_[[1]]
plot(diff585pres_M)


# LATE ---------------------------------------------------------------------
# Late winter elements are in positions 3, 6 and 9 from the list (3/3 from present -- 3/3 from mid -- (3/3 from late). This is for MW and FW
late <- list(vv[3], vv[6], vv[9])

# Unlist mid list
late_ <- unlist(late)

# Calculate the difference between 2100 and present for SSP1-2.6
diff126pres_L <- late_[[2]] - late_[[1]]
plot(diff126pres_L)

# Calculate the difference between 2100 and present for SSP5-8.5
diff585pres_L <- late_[[3]] - late_[[1]]
plot(diff585pres_L)


# SUMMER ------------------------------------------------------------------
# In the case of FS, the elements are 1, 2 and 3, since there is a single summer slot
FS <- list(vv[1], vv[2], vv[3])

# Unlist summer list
FS_ <- unlist(early)

# Calculate the differences between 2100 and present for SSP1-2.6
diff126pres_FS <- FS_[[2]] - FS_[[1]]
plot(diff126pres_FS)

# Calculate the differences between 2100 and present for SSP5-8.5
diff585pres_FS <- FS_[[3]] - FS_[[1]]
plot(diff585pres_FS)
# ------------------------------------------------------------------------------------------------------------------------------------------




# -------------------------------------------------------------------------
# Sea Ice data for figures. The commented lines in each beginning are the ones that we used in the first version, which was just using the 2019 data for the present. Latest
# version is the average between 2019 and 2025

# EARLY WINTER -------------------------------------------------------------------
# Early winter present
# stE3 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2019/20190301_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stE3 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2019/20190301_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stE3a<- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2020/20200301_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stE3b <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2021/20210301_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stE3c <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2022/20220301_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stE3d <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2023/20230301_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stE3e <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2024/20240301_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stE3f <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2025/20250301_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stE3SIC_ <-stack(stE3$index_SIC,stE3a$index_SIC, stE3b$index_SIC,stE3c$index_SIC, stE3d$index_SIC, stE3e$index_SIC, stE3f$index_SIC)
stE3SIC <- mean(stE3SIC_)

# stE3SIC <- stE3$index_SIC
stE3SIC[stE3SIC[] <= 15 ] = NA
stE3SIC <- stE3SIC/stE3SIC
stE3SIC_p <- rasterToPolygons(stE3SIC, dissolve = T)
stEpres_3SIC_p_s <- smoothr::smooth(stE3SIC_p, method = "ksmooth", smoothness = 3)


# stE4 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/histor/2014/20140401_monthly_ESMensemble_stack_predict_histor_ISIMIP.grd")
stE4 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2019/20190401_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stE4a<- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2020/20200401_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stE4b <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2021/20210401_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stE4c <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2022/20220401_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stE4d <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2023/20230401_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stE4e <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2024/20240401_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stE4f <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2025/20250401_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stE4SIC_ <-stack(stE4$index_SIC,stE4a$index_SIC, stE4b$index_SIC,stE4c$index_SIC, stE4d$index_SIC, stE4e$index_SIC, stE4f$index_SIC)
stE4SIC <- mean(stE4SIC_)

# stE4SIC <- stE4$index_SIC
stE4SIC[stE4SIC[] <= 15 ] = NA
stE4SIC <- stE4SIC/stE4SIC
stE4SIC_p <- rasterToPolygons(stE4SIC, dissolve = T)
stEpres_4SIC_p_s <- smoothr::smooth(stE4SIC_p, method = "ksmooth", smoothness = 3)


# stE5 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/histor/2014/20140501_monthly_ESMensemble_stack_predict_histor_ISIMIP.grd")
stE5 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2019/20190501_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stE5a<- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2020/20200501_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stE5b <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2021/20210501_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stE5c <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2022/20220501_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stE5d <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2023/20230501_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stE5e <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2024/20240501_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stE5f <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2025/20250501_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stE5SIC_ <-stack(stE5$index_SIC,stE5a$index_SIC, stE5b$index_SIC,stE5c$index_SIC, stE5d$index_SIC, stE5e$index_SIC, stE5f$index_SIC)
stE5SIC <- mean(stE5SIC_)

# stE5SIC <- stE5SIC$index_SIC
stE5SIC[stE5SIC[] <= 15 ] = NA
stE5SIC <- stE5SIC/stE5SIC
stE5SIC_p <- rasterToPolygons(stE5SIC, dissolve = T)
stEpres_5SIC_p_s <- smoothr::smooth(stE5SIC_p, method = "ksmooth", smoothness = 3)

Epres <- list(stEpres_3SIC_p_s, stEpres_4SIC_p_s, stEpres_5SIC_p_s)



# Early winter SSP1-2.6
# stE3 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2100/21000301_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stE3 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2100/21000301_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stE3SIC <- stE3$index_SIC
stE3SIC[stE3SIC[] <= 15 ] = NA
stE3SIC <- stE3SIC/stE3SIC
stE3SIC_p <- rasterToPolygons(stE3SIC, dissolve = T)
stE126_3SIC_p_s <- smoothr::smooth(stE3SIC_p, method = "ksmooth", smoothness = 3)

# stE4 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2100/21000401_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stE4 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2100/21000401_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stE4SIC <- stE4$index_SIC
stE4SIC[stE4SIC[] <= 15 ] = NA
stE4SIC <- stE4SIC/stE4SIC
stE4SIC_p <- rasterToPolygons(stE4SIC, dissolve = T)
stE126_4SIC_p_s <- smoothr::smooth(stE4SIC_p, method = "ksmooth", smoothness = 3)

# stE5 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2100/21000501_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stE5 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2100/21000501_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stE5SIC <- stE5$index_SIC
stE5SIC <- stE5SIC$index_SIC
stE5SIC[stE5SIC[] <= 15 ] = NA
stE5SIC <- stE5SIC/stE5SIC
stE5SIC_p <- rasterToPolygons(stE5SIC, dissolve = T)
stE126_5SIC_p_s <- smoothr::smooth(stE5SIC_p, method = "ksmooth", smoothness = 3)

E126 <- list(stE126_3SIC_p_s, stE126_4SIC_p_s, stE126_5SIC_p_s)



# Early winter SSP5-8.5
# stE3 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp585/2100/21000301_monthly_ESMensemble_stack_predict_ssp585_ISIMIP.grd")
stE3 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp585/2100/21000301_monthly_ESMensemble_stack_predict_ssp585_ISIMIP.grd")
stE3SIC <- stE3$index_SIC
stE3SIC[stE3SIC[] <= 15 ] = NA
stE3SIC <- stE3SIC/stE3SIC
stE3SIC_p <- rasterToPolygons(stE3SIC, dissolve = T)
stE585_3SIC_p_s <- smoothr::smooth(stE3SIC_p, method = "ksmooth", smoothness = 3)

# stE4 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp585/2100/21000401_monthly_ESMensemble_stack_predict_ssp585_ISIMIP.grd")
stE4 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp585/2100/21000401_monthly_ESMensemble_stack_predict_ssp585_ISIMIP.grd")
stE4SIC <- stE4$index_SIC
stE4SIC[stE4SIC[] <= 15 ] = NA
stE4SIC <- stE4SIC/stE4SIC
stE4SIC_p <- rasterToPolygons(stE4SIC, dissolve = T)
stE585_4SIC_p_S <- smoothr::smooth(stE4SIC_p, method = "ksmooth", smoothness = 3)

# stE5 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp585/2100/21000501_monthly_ESMensemble_stack_predict_ssp585_ISIMIP.grd")
stE5 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp585/2100/21000501_monthly_ESMensemble_stack_predict_ssp585_ISIMIP.grd")
stE5SIC <- stE5$index_SIC
stE5SIC <- stE5SIC$index_SIC
stE5SIC[stE5SIC[] <= 15 ] = NA
stE5SIC <- stE5SIC/stE5SIC
stE5SIC_p <- rasterToPolygons(stE5SIC, dissolve = T)
stE585_5SIC_p_S <- smoothr::smooth(stE5SIC_p, method = "ksmooth", smoothness = 3)

E585 <- list(stE585_3SIC_p_s, stE585_4SIC_p_S, stE585_5SIC_p_S)



# MID WINTER ---------------------------------------------------------------------
# Mid winter present
# stM6 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2019/20190601_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stM6 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2019/20190601_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stM6a<- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2020/20200601_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stM6b <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2021/20210601_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stM6c <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2022/20220601_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stM6d <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2023/20230601_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stM6e <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2024/20240601_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stM6f <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2025/20250601_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stM6SIC_ <-stack(stM6$index_SIC,stM6a$index_SIC, stM6b$index_SIC,stM6c$index_SIC, stM6d$index_SIC, stM6e$index_SIC, stM6f$index_SIC)
stM6SIC <- mean(stM6SIC_)

# stM6SIC <- stM6$index_SIC
stM6SIC[stM6SIC[] <= 15 ] = NA
stM6SIC <- stM6SIC/stM6SIC
stM6SIC_p <- rasterToPolygons(stM6SIC, dissolve = T)
stMpres_6SIC_p_s <- smoothr::smooth(stM6SIC_p, method = "ksmooth", smoothness = 3)

# stM7 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/histor/2014/20140701_monthly_ESMensemble_stack_predict_histor_ISIMIP.grd")
stM7 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2019/20190701_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stM7a<- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2020/20200701_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stM7b <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2021/20210701_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stM7c <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2022/20220701_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stM7d <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2023/20230701_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stM7e <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2024/20240701_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stM7f <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2025/20250701_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stM7SIC_ <-stack(stM7$index_SIC,stM7a$index_SIC, stM7b$index_SIC,stM7c$index_SIC, stM7d$index_SIC, stM7e$index_SIC, stM7f$index_SIC)
stM7SIC <- mean(stM7SIC_)

# stM7SIC <- stM7$index_SIC
stM7SIC[stM7SIC[] <= 15 ] = NA
stM7SIC <- stM7SIC/stM7SIC
stM7SIC_p <- rasterToPolygons(stM7SIC, dissolve = T)
stMpres_7SIC_p_s <- smoothr::smooth(stM7SIC_p, method = "ksmooth", smoothness = 3)

# stM8 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/histor/2014/20140801_monthly_ESMensemble_stack_predict_histor_ISIMIP.grd")
stM8 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2019/20190801_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stM8a<- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2020/20200801_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stM8b <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2021/20210801_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stM8c <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2022/20220801_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stM8d <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2023/20230801_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stM8e <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2024/20240801_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stM8f <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2025/20250801_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stM8SIC_ <-stack(stM8$index_SIC,stM8a$index_SIC, stM8b$index_SIC,stM8c$index_SIC, stM8d$index_SIC, stM8e$index_SIC, stM8f$index_SIC)
stM8SIC <- mean(stM8SIC_)

# stM8SIC <- stM8$index_SIC
stM8SIC[stM8SIC[] <= 15 ] = NA
stM8SIC <- stM8SIC/stM8SIC
stM8SIC_p <- rasterToPolygons(stM8SIC, dissolve = T)
stMpres_8SIC_p_s <- smoothr::smooth(stM8SIC_p, method = "ksmooth", smoothness = 3)

Mpres <- list(stMpres_6SIC_p_s, stMpres_7SIC_p_s, stMpres_8SIC_p_s)


# Mid winter SSP1-2.6
# stM6 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2100/21000601_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stM6 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2100/21000601_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stM6SIC <- stM6$index_SIC
stM6SIC[stM6SIC[] <= 15 ] = NA
stM6SIC <- stM6SIC/stM6SIC
stM6SIC_p <- rasterToPolygons(stM6SIC, dissolve = T)
stM6126_3SIC_p_s <- smoothr::smooth(stM6SIC_p, method = "ksmooth", smoothness = 3)

# stM7 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2100/21000701_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stM7 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2100/21000701_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stM7SIC <- stM7$index_SIC
stM7SIC[stM7SIC[] <= 15 ] = NA
stM7SIC <- stM7SIC/stM7SIC
stM7SIC_p <- rasterToPolygons(stM7SIC, dissolve = T)
stM126_7SIC_p_s <- smoothr::smooth(stM7SIC_p, method = "ksmooth", smoothness = 3)

# stM8 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2100/21000801_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stM8 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2100/21000801_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stM8SIC <- stM8$index_SIC
stM8SIC <- stM8SIC$index_SIC
stM8SIC[stM8SIC[] <= 15 ] = NA
stM8SIC <- stM8SIC/stM8SIC
stM8SIC_p <- rasterToPolygons(stM8SIC, dissolve = T)
stM126_8SIC_p_s <- smoothr::smooth(stM8SIC_p, method = "ksmooth", smoothness = 3)

M126 <- list(stM6126_3SIC_p_s, stM126_7SIC_p_s, stM126_8SIC_p_s)


# Mid winter SSP5-8.5
# stM6 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp585/2100/21000601_monthly_ESMensemble_stack_predict_ssp585_ISIMIP.grd")
stM6 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp585/2100/21000601_monthly_ESMensemble_stack_predict_ssp585_ISIMIP.grd")
stM6SIC <- stM6$index_SIC
stM6SIC[stM6SIC[] <= 15 ] = NA
stM6SIC <- stM6SIC/stM6SIC
stM6SIC_p <- rasterToPolygons(stM6SIC, dissolve = T)
stM6585_3SIC_p_s <- smoothr::smooth(stM6SIC_p, method = "ksmooth", smoothness = 3)

# stM7 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp585/2100/21000701_monthly_ESMensemble_stack_predict_ssp585_ISIMIP.grd")
stM7 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp585/2100/21000701_monthly_ESMensemble_stack_predict_ssp585_ISIMIP.grd")
stM7SIC <- stM7$index_SIC
stM7SIC[stM7SIC[] <= 15 ] = NA
stM7SIC <- stM7SIC/stM7SIC
stM7SIC_p <- rasterToPolygons(stM7SIC, dissolve = T)
stM585_7SIC_p_s <- smoothr::smooth(stM7SIC_p, method = "ksmooth", smoothness = 3)

# stM8 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp585/2100/21000801_monthly_ESMensemble_stack_predict_ssp585_ISIMIP.grd")
stM8 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp585/2100/21000801_monthly_ESMensemble_stack_predict_ssp585_ISIMIP.grd")
stM8SIC <- stM8$index_SIC
stM8SIC <- stM8SIC$index_SIC
stM8SIC[stM8SIC[] <= 15 ] = NA
stM8SIC <- stM8SIC/stM8SIC
stM8SIC_p <- rasterToPolygons(stM8SIC, dissolve = T)
stM585_8SIC_p_s <- smoothr::smooth(stM8SIC_p, method = "ksmooth", smoothness = 3)

M585 <- list(stM6585_3SIC_p_s, stM585_7SIC_p_s, stM585_8SIC_p_s)



# LATE WINTER --------------------------------------------------------------------
# Late winter present (only sept for MW)
# stL9 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2019/20190901_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stL9 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2019/20190901_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stL9a <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2020/20200901_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stL9b <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2021/20210901_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stL9c <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2022/20220901_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stL9d <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2023/20230901_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stL9e <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2024/20240901_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stL9f <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2025/20250901_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stL9SIC_ <-stack(stL9$index_SIC,stL9a$index_SIC, stL9b$index_SIC,stL9c$index_SIC, stL9d$index_SIC, stL9e$index_SIC, stL9f$index_SIC)
stL9SIC <- mean(stL9SIC_)

# stL9SIC <- stL9$index_SIC
stL9SIC[stL9SIC[] <= 15 ] = NA
stL9SIC <- stL9SIC/stL9SIC
stL9SIC_p <- rasterToPolygons(stL9SIC, dissolve = T)
stLpres_9SIC_p_s <- smoothr::smooth(stL9SIC_p, method = "ksmooth", smoothness = 3)

# stL10 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2019/20191001_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stL10 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2019/20191001_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stL10a <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2020/20201001_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stL10b <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2021/20211001_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stL10c <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2022/20221001_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stL10d <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2023/20231001_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stL10e <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2024/20241001_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stL10f <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2025/20251001_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stL10SIC_ <-stack(stL10$index_SIC,stL10a$index_SIC, stL10b$index_SIC, stL10c$index_SIC, stL10d$index_SIC, stL10e$index_SIC, stL10f$index_SIC)
stL10SIC <- mean(stL10SIC_)

# stL10SIC <- stL10$index_SIC
stL10SIC[stL10SIC[] <= 15 ] = NA
stL10SIC <- stL10SIC/stL10SIC
stL10SIC_p <- rasterToPolygons(stL10SIC, dissolve = T)
stLpres_10SIC_p_s <- smoothr::smooth(stL10SIC_p, method = "ksmooth", smoothness = 3)

# stL11 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2019/20191101_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stL11 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2019/20191101_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stL11a <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2020/20201101_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stL11b <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2021/20211101_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stL11c <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2022/20221101_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stL11d <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2023/20231101_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stL11e <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2024/20241101_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stL11f <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2025/20251101_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stL11SIC_ <-stack(stL11$index_SIC,stL11a$index_SIC, stL11b$index_SIC, stL11c$index_SIC, stL11d$index_SIC, stL11e$index_SIC, stL11f$index_SIC)
stL11SIC <- mean(stL11SIC_)

# stL11SIC <- stL11$index_SIC
stL11SIC[stL11SIC[] <= 15 ] = NA
stL11SIC <- stL11SIC/stL11SIC
stL11SIC_p <- rasterToPolygons(stL11SIC, dissolve = T)
stLpres_11SIC_p_s <- smoothr::smooth(stL11SIC_p, method = "ksmooth", smoothness = 3)

# December was excluded for females in the winter, since they may start having breeding-like behaviors then
# stL12 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2019/20191201_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
# stL12 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2019/20191201_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
# stL12a <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2020/20201201_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
# stL12b <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2021/20211201_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
# stL12c <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2022/20221201_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
# stL12d <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2023/20231201_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
# stL12e <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2024/20241201_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
# stL12f <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2025/20251201_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
# stL12SIC_ <-stack(stL12$index_SIC,stL12a$index_SIC, stL12b$index_SIC, stL12c$index_SIC, stL12d$index_SIC, stL12e$index_SIC, stL12f$index_SIC)
# stL12SIC <- mean(stL12SIC_)
# 
# # stL12SIC <- stL12$index_SIC
# stL12SIC[stL12SIC[] <= 15 ] = NA
# stL12SIC <- stL12SIC/stL12SIC
# stL12SIC_p <- rasterToPolygons(stL12SIC, dissolve = T)
# stLpres_12SIC_p_s <- smoothr::smooth(stL12SIC_p, method = "ksmooth", smoothness = 3)

# Lpres <- list(stLpres_9SIC_p_s, stLpres_10SIC_p_s, stLpres_11SIC_p_s, stLpres_12SIC_p_s)
Lpres <- list(stLpres_9SIC_p_s, stLpres_10SIC_p_s, stLpres_11SIC_p_s)


# Late winter SSP1-2.6
# stL9 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2100/21000901_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stL9 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2100/21000901_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stL9SIC <- stL9$index_SIC
stL9SIC[stL9SIC[] <= 15 ] = NA
stL9SIC <- stL9SIC/stL9SIC
stL9SIC_p <- rasterToPolygons(stL9SIC, dissolve = T)
stL126_9SIC_p_s <- smoothr::smooth(stL9SIC_p, method = "ksmooth", smoothness = 3)

# stL10 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2100/21001001_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stL10 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2100/21001001_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stL10SIC <- stL10$index_SIC
stL10SIC[stL10SIC[] <= 15 ] = NA
stL10SIC <- stL10SIC/stL10SIC
stL10SIC_p <- rasterToPolygons(stL10SIC, dissolve = T)
stL126_10SIC_p_s <- smoothr::smooth(stL10SIC_p, method = "ksmooth", smoothness = 3)

# stL11 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2100/21001101_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stL11 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2100/21001101_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stL11SIC <- stL11$index_SIC
stL11SIC[stL11SIC[] <= 15 ] = NA
stL11SIC <- stL11SIC/stL11SIC
stL11SIC_p <- rasterToPolygons(stL11SIC, dissolve = T)
stL126_11SIC_p_s <- smoothr::smooth(stL11SIC_p, method = "ksmooth", smoothness = 3)

# stL12 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2100/21001201_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
# stL12 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2100/21001201_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
# stL12SIC <- stL12$index_SIC
# stL12SIC[stL12SIC[] <= 15 ] = NA
# stL12SIC <- stL12SIC/stL12SIC
# stL12SIC_p <- rasterToPolygons(stL12SIC, dissolve = T)
# stL126_12SIC_p_s <- smoothr::smooth(stL12SIC_p, method = "ksmooth", smoothness = 3)

# L126 <- list(stL126_9SIC_p_s, stL126_10SIC_p_s, stL126_11SIC_p_s, stL126_12SIC_p_s)
L126 <- list(stL126_9SIC_p_s, stL126_10SIC_p_s, stL126_11SIC_p_s)


# Late winter SSP5-8.5
# stL9 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp585/2100/21000901_monthly_ESMensemble_stack_predict_ssp585_ISIMIP.grd")
stL9 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp585/2100/21000901_monthly_ESMensemble_stack_predict_ssp585_ISIMIP.grd")
stL9SIC <- stL9$index_SIC
stL9SIC[stL9SIC[] <= 15 ] = NA
stL9SIC <- stL9SIC/stL9SIC
stL9SIC_p <- rasterToPolygons(stL9SIC, dissolve = T)
stL585_9SIC_p_s <- smoothr::smooth(stL9SIC_p, method = "ksmooth", smoothness = 3)

# stL10 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp585/2100/21001001_monthly_ESMensemble_stack_predict_ssp585_ISIMIP.grd")
stL10 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp585/2100/21001001_monthly_ESMensemble_stack_predict_ssp585_ISIMIP.grd")
stL10SIC <- stL10$index_SIC
stL10SIC[stL10SIC[] <= 15 ] = NA
stL10SIC <- stL10SIC/stL10SIC
stL10SIC_p <- rasterToPolygons(stL10SIC, dissolve = T)
stL585_10SIC_p_s <- smoothr::smooth(stL10SIC_p, method = "ksmooth", smoothness = 3)

# stL11 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp585/2100/21001101_monthly_ESMensemble_stack_predict_ssp585_ISIMIP.grd")
stL11 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp585/2100/21001101_monthly_ESMensemble_stack_predict_ssp585_ISIMIP.grd")
stL11SIC <- stL11$index_SIC
stL11SIC[stL11SIC[] <= 15 ] = NA
stL11SIC <- stL11SIC/stL11SIC
stL11SIC_p <- rasterToPolygons(stL11SIC, dissolve = T)
stL585_11SIC_p_s <- smoothr::smooth(stL11SIC_p, method = "ksmooth", smoothness = 3)

# stL12 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp585/2100/21001201_monthly_ESMensemble_stack_predict_ssp585_ISIMIP.grd")
# stL12 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp585/2100/21001201_monthly_ESMensemble_stack_predict_ssp585_ISIMIP.grd")
# stL12SIC <- stL12$index_SIC
# stL12SIC[stL12SIC[] <= 15 ] = NA
# stL12SIC <- stL12SIC/stL12SIC
# stL12SIC_p <- rasterToPolygons(stL12SIC, dissolve = T)
# stL585_12SIC_p_s <- smoothr::smooth(stL12SIC_p, method = "ksmooth", smoothness = 3)

# L585 <- list(stL585_9SIC_p_s, stL585_10SIC_p_s, stL585_11SIC_p_s, stL585_12SIC_p_s)
L585 <- list(stL585_9SIC_p_s, stL585_10SIC_p_s, stL585_11SIC_p_s)


# SUMMER ------------------------------------------------------------------
# FS present
# stFS1 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2019/20190101_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
# stFS1 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2019/20190101_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS1 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2019/20190101_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS1a <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2020/20200101_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS1b <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2021/20210101_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS1c <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2022/20220101_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS1d <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2023/20230101_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS1e <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2024/20240101_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS1f <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2025/20250101_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")

stFS1SIC_ <-stack(stFS1$index_SIC,stFS1a$index_SIC, stFS1b$index_SIC,stFS1c$index_SIC, stFS1d$index_SIC, stFS1e$index_SIC, stFS1f$index_SIC)
stFS1SIC <- mean(stFS1SIC_)
# stFS1SIC <- stFS1$index_SIC
stFS1SIC[stFS1SIC[] <= 15 ] = NA
stFS1SIC <- stFS1SIC/stFS1SIC
stFS1SIC_p <- rasterToPolygons(stFS1SIC, dissolve = T)
stFSpres_1SIC_p_s <- smoothr::smooth(stFS1SIC_p, method = "ksmooth", smoothness = 3)

# stFS2 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2019/20190201_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS2 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2019/20190201_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS2a <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2020/20200201_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS2b <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2021/20210201_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS2c <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2022/20220201_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS2d <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2023/20230201_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS2e <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2024/20240201_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS2f <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2025/20250201_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")

stFS2SIC_ <-stack(stFS2$index_SIC,stFS2a$index_SIC, stFS2b$index_SIC,stFS2c$index_SIC, stFS2d$index_SIC, stFS2e$index_SIC, stFS2f$index_SIC)
stFS2SIC <- mean(stFS2SIC_)
# stFS2SIC <- stFS2$index_SIC
stFS2SIC[stFS2SIC[] <= 15 ] = NA
stFS2SIC <- stFS2SIC/stFS2SIC
stFS2SIC_p <- rasterToPolygons(stFS2SIC, dissolve = T)
stFSpres_2SIC_p_s <- smoothr::smooth(stFS2SIC_p, method = "ksmooth", smoothness = 3)

# stFS3 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2019/20190301_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS3 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2019/20190301_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS3a <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2020/20200301_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS3b <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2021/20210301_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS3c <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2022/20220301_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS3d <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2023/20230301_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS3e <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2024/20240301_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS3f <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2025/20250301_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")

stFS3SIC_ <-stack(stFS3$index_SIC,stFS3a$index_SIC, stFS3b$index_SIC,stFS3c$index_SIC, stFS3d$index_SIC, stFS3e$index_SIC, stFS3f$index_SIC)
stFS3SIC <- mean(stFS2SIC_)
# stFS3SIC <- stFS3$index_SIC
stFS3SIC[stFS3SIC[] <= 15 ] = NA
stFS3SIC <- stFS3SIC/stFS3SIC
stFS3SIC_p <- rasterToPolygons(stFS3SIC, dissolve = T)
stFSpres_3SIC_p_s <- smoothr::smooth(stFS3SIC_p, method = "ksmooth", smoothness = 3)

# stFS4 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2019/20190401_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS4 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2018/20181201_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS4a <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2019/20191201_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS4b <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2020/20201201_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS4c <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2021/20211201_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS4d <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2022/20221201_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS4e <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2023/20231201_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS4f <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2024/20241201_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")

stFS4SIC_ <-stack(stFS4$index_SIC,stFS4a$index_SIC, stFS4b$index_SIC,stFS4c$index_SIC, stFS4d$index_SIC, stFS4e$index_SIC, stFS4f$index_SIC)
stFS4SIC <- mean(stFS4SIC_)
# stFS4SIC <- stFS4$index_SIC
stFS4SIC[stFS4SIC[] <= 15 ] = NA
stFS4SIC <- stFS4SIC/stFS4SIC
stFS4SIC_p <- rasterToPolygons(stFS4SIC, dissolve = T)
stFSpres_4SIC_p_s <- smoothr::smooth(stFS4SIC_p, method = "ksmooth", smoothness = 3)

FSpres <- list(stFSpres_1SIC_p_s, stFSpres_2SIC_p_s, stFSpres_3SIC_p_s, stFSpres_4SIC_p_s)


# Summer SSP1-2.6 FS
# stFS1 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2100/21000101_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS1 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2100/21000101_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS1SIC <- stFS1$index_SIC
stFS1SIC[stFS1SIC[] <= 15 ] = NA
stFS1SIC <- stFS1SIC/stFS1SIC
stFS1SIC_p <- rasterToPolygons(stFS1SIC, dissolve = T)
stFS126_1SIC_p_s <- smoothr::smooth(stFS1SIC_p, method = "ksmooth", smoothness = 3)

# stFS2 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2100/21000201_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS2 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2100/21000201_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS2SIC <- stFS2$index_SIC
stFS2SIC[stFS2SIC[] <= 15 ] = NA
stFS2SIC <- stFS2SIC/stFS2SIC
stFS2SIC_p <- rasterToPolygons(stFS2SIC, dissolve = T)
stFS126_2SIC_p_s <- smoothr::smooth(stFS2SIC_p, method = "ksmooth", smoothness = 3)

# stFS3 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2100/21000301_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS3 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2100/21000301_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS3SIC <- stFS3$index_SIC
stFS3SIC[stFS3SIC[] <= 15 ] = NA
stFS3SIC <- stFS3SIC/stFS3SIC
stFS3SIC_p <- rasterToPolygons(stFS3SIC, dissolve = T)
stFS126_3SIC_p_s <- smoothr::smooth(stFS3SIC_p, method = "ksmooth", smoothness = 3)

# stFS4 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2100/21001201_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS4 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp126/2099/20991201_monthly_ESMensemble_stack_predict_ssp126_ISIMIP.grd")
stFS4SIC <- stFS4$index_SIC
stFS4SIC[stFS4SIC[] <= 15 ] = NA
stFS4SIC <- stFS4SIC/stFS4SIC
stFS4SIC_p <- rasterToPolygons(stFS4SIC, dissolve = T)
stFS126_4SIC_p_s <- smoothr::smooth(stFS4SIC_p, method = "ksmooth", smoothness = 3)

FSp126 <- list(stFS126_1SIC_p_s, stFS126_2SIC_p_s, stFS126_3SIC_p_s, stFS126_4SIC_p_s)


# Summer SSP5-8.5 FS

# stFS1 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp585/2100/21000101_monthly_ESMensemble_stack_predict_ssp585_ISIMIP.grd")
stFS1 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp585/2100/21000101_monthly_ESMensemble_stack_predict_ssp585_ISIMIP.grd")
stFS1SIC <- stFS1$index_SIC
stFS1SIC[stFS1SIC[] <= 15 ] = NA
stFS1SIC <- stFS1SIC/stFS1SIC
stFS1SIC_p <- rasterToPolygons(stFS1SIC, dissolve = T)
stFS585_1SIC_p_s <- smoothr::smooth(stFS1SIC_p, method = "ksmooth", smoothness = 3)

# stFS2 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp585/2100/21000201_monthly_ESMensemble_stack_predict_ssp585_ISIMIP.grd")
stFS2 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp585/2100/21000201_monthly_ESMensemble_stack_predict_ssp585_ISIMIP.grd")
stFS2SIC <- stFS2$index_SIC
stFS2SIC[stFS2SIC[] <= 15 ] = NA
stFS2SIC <- stFS2SIC/stFS2SIC
stFS2SIC_p <- rasterToPolygons(stFS2SIC, dissolve = T)
stFS585_2SIC_p_s <- smoothr::smooth(stFS2SIC_p, method = "ksmooth", smoothness = 3)

# stFS3 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp585/2100/21000301_monthly_ESMensemble_stack_predict_ssp585_ISIMIP.grd")
stFS3 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp585/2100/21000301_monthly_ESMensemble_stack_predict_ssp585_ISIMIP.grd")
stFS3SIC <- stFS3$index_SIC
stFS3SIC[stFS3SIC[] <= 15 ] = NA
stFS3SIC <- stFS3SIC/stFS3SIC
stFS3SIC_p <- rasterToPolygons(stFS3SIC, dissolve = T)
stFS585_3SIC_p_s <- smoothr::smooth(stFS3SIC_p, method = "ksmooth", smoothness = 3)

# stFS4 <- raster::stack("D:/ISIMIP_netCDF/output/monthly_stacks_predict/ssp585/2100/21001201_monthly_ESMensemble_stack_predict_ssp585_ISIMIP.grd")
stFS4 <- raster::stack("/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_predict/ssp585/2099/20991201_monthly_ESMensemble_stack_predict_ssp585_ISIMIP.grd")
stFS4SIC <- stFS4$index_SIC
stFS4SIC[stFS4SIC[] <= 15 ] = NA
stFS4SIC <- stFS4SIC/stFS4SIC
stFS4SIC_p <- rasterToPolygons(stFS4SIC, dissolve = T)
stFS585_4SIC_p_s <- smoothr::smooth(stFS4SIC_p, method = "ksmooth", smoothness = 3)

FS585 <- list(stFS585_1SIC_p_s, stFS585_2SIC_p_s, stFS585_3SIC_p_s, stFS585_4SIC_p_s)

# --------------------------------------------------------------------------------




# Get the data to add to the plots ----------------------------------------
# MW ----------------------------------------------------------------------
present <- list(Epres, Mpres, stLpres_9SIC_p_s)
ssp126 <- list(E126, M126, stL126_9SIC_p_s)
ssp585 <- list(E585, M585, stL585_9SIC_p_s)

# FW ----------------------------------------------------------------------
present <- list(Epres, Mpres, Lpres)
ssp126 <- list(E126, M126, L126)
ssp585 <- list(E585, M585, L585)

hh <- list(present, ssp126, ssp585)
hh <- present
hh <- ssp126
hh <- ssp585

# Common for MW and FW
hh <- present
aa <- list(vv[[1]], vv[[2]], vv[[3]])
myplots_j_a <- list()  # new empty list

hh <- ssp126
bb <- list(diff126pres_E, diff126pres_M, diff126pres_L)
myplots_j_b <- list()  # new empty list

hh <- ssp585
cc <- list(diff585pres_E, diff585pres_M, diff585pres_L)
myplots_j_c <- list()  # new empty list



# FS ----------------------------------------------------------------------
# Present
present <- FSpres
hh <- present
aa <- FS_[[1]]

# SSP1-2.6
ssp126 <- FSp126
hh<- ssp126
bb <- diff126pres_FS

# SSP5-8.5
ssp585 <- FS585
hh <- ssp585
cc <- diff585pres_FS



# Select sexseason combination --------------------------------------------
sexseason <- "MW"
sexseason <- "FW"
sexseason <- "FS"


# ADD cc[1] when working with FS

# for (z in 1:length(cc)){
  
# For FS, as it only has one slot (summer). You need to run first aa[[z]], then change it to bb[[z]] and finally cc[[z]]
for (z in 1:length(aa)){
    
                 r <- aa[[z]]
                 r_df<-  as.data.frame(r, xy = T)
                 colnames(r_df) <- c("x", "y", "value")
                 r_df$value[is.nan(r_df$value)]<-NA
                 r_df$value <- round(r_df$value, digits = 2)
                 
        
        # MW
                    if (z == 1){
                               poly1 <- st_as_sf(hh[[z]][[1]])
                               poly2 <- st_as_sf(hh[[z]][[2]])
                               poly3 <- st_as_sf(hh[[z]][[3]])
            
                             } else if (z == 2) {
                               poly1 <- st_as_sf(hh[[z]][[1]])
                               poly2 <- st_as_sf(hh[[z]][[2]])
                               poly3 <- st_as_sf(hh[[z]][[3]])
            
                             } else if (z == 3){
                               poly1 <- st_as_sf(hh[[z]])
                               poly2 <- NULL
                               poly3 <- NULL
                             }

                 
       # FW
       #           if (z == 1){
       #             poly1 <- st_as_sf(hh[[z]][[1]])
       #             poly2 <- st_as_sf(hh[[z]][[2]])
       #             poly3 <- NULL
       #             poly4 <- NULL
       #             polyKK <- st_as_sf(hh[[z]][[3]])
       # 
       #           } else if (z == 2) {
       #             poly1 <- st_as_sf(hh[[z]][[1]])
       #             poly2 <- st_as_sf(hh[[z]][[2]])
       #             poly3 <- NULL
       #             poly4 <- NULL
       #             polyKK <- st_as_sf(hh[[z]][[3]])
       # 
       #           } else if (z == 3){
       #             poly1 <- st_as_sf(hh[[z]][[1]])
       #             poly2 <- st_as_sf(hh[[z]][[2]])
       #             poly3 <- st_as_sf(hh[[z]][[3]])
       #             # poly4 <- st_as_sf(hh[[z]][[4]])
       #             polyKK <- NULL
       # 
       #           }




       #    if (z == 1){
       #    poly1 <- st_as_sf(hh[[z]][[1]][[1]])
       #    poly2 <- st_as_sf(hh[[z]][[1]][[2]])
       #    poly3 <- st_as_sf(hh[[z]][[1]][[3]])
       # 
       # } else if (z == 2) {
       #    poly1 <- st_as_sf(hh[[z]][[2]][[1]])
       #    poly2 <- st_as_sf(hh[[z]][[2]][[2]])
       #    poly3 <- st_as_sf(hh[[z]][[2]][[3]])
       # 
       #  } else {
       #    poly1 <- NULL
       #    poly2 <- NULL
       #    poly3 <- st_as_sf(hh[[z]][[3]])}

                                    
                 
                 diff_palette <- paletteer_c("ggthemes::Classic Orange-White-Blue", 18)
                 
        if (sexseason %like% "FS"){
                   
        # FS
                   poly1 <- st_as_sf(hh[[1]]) #jan
                   poly2 <- st_as_sf(hh[[2]]) # feb
                   poly3 <- st_as_sf(hh[[3]]) # mar
                   poly4 <- st_as_sf(hh[[4]]) #dec

                   
                   ww <-ggplot()+
                                 geom_raster(aes(x=x,y=y, fill=value),data=r_df)+
                                 geom_sf(data = poly4, fill = NA, linetype = "dotted", size = 0.3, colour = "snow")+
                                 geom_sf(data = poly3, fill = NA, linetype = "dotdash", size = 0.3, colour = "snow")+
                                 geom_sf(data = poly1, linetype = "longdash", size = 0.3, fill= NA, colour = "snow") +
                                 geom_sf(data = poly2, fill = "snow", linetype = "solid", size = 0.3)+
                                 geom_sf(fill='grey', data=world, lwd = 0.00001)+
                                 scale_x_continuous(limits = c(-97, -20.44306), expand=c(0,0)) +
                                 scale_y_continuous(limits = c(-79.05833, -48),expand=c(0,0)) +
                                 # scale_x_continuous(limits = c(-99.5, 16.5), expand=c(0,0)) +
                                 # scale_y_continuous(limits = c(-79.05833, -45),expand=c(0,0)) +
                                 # scale_x_continuous(limits = c(-180, 16.884), expand=c(0,0)) +
                                 # scale_y_continuous(limits = c(-83.606, -29.243),expand=c(0,0)) +
                                 # scale_fill_viridis_c('Habitat suitability', direction = 1, option = "viridis", na.value="white", limits = c(0,0.5))+ # slopehalf chl
                                 scale_fill_gradientn(colors = viridisfake, na.value = "gray88", limits = c(0,1))+
                                 # scale_fill_gradientn(colors = diff_palette, na.value = "gray88", limits = c(-0.55,0.55))+
                                 # scale_fill_gradientn(colors = dist_palette, na.value = "white", limits = c(0,1))+
                                 labs(x='Longitude\n',y='Latitude')+
                                 themeMapsCC

                    myplots_j_a[[z]] <- ww
                  # myplots_j_b[[z]] <- ww
                  # myplots_j_c[[z]] <- ww

                    }
                 

          if ((z == 1 | z == 2) & sexseason %notlike% "FS"){
                   
                      ww <-ggplot()+
                                    geom_raster(aes(x=x,y=y, fill=value),data=r_df)+
                              # MW present ICE polygons
                                    geom_sf(data = poly3, fill = NA, linetype = "dotdash", size = 0.3, colour = "azure1")+
                                    geom_sf(data = poly2, fill = NA, linetype = "dotted", size = 0.3, colour = "azure1")+
                                    geom_sf(data = poly1, linetype = "solid", size = 0.3, fill= "#FAF7F6") +
                   
                              # MW future ICE polygons
                                  # geom_sf(data = poly3, fill = NA, linetype = "dotdash", size = 0.3, colour = "black")+
                                  # geom_sf(data = poly2, fill = NA, linetype = "dotted", size = 0.3, colour = "black")+
                                  # geom_sf(data = poly1, linetype = "solid", size = 0.3, fill="snow")+
                 
                              # FW present ICE polygons
                                  # geom_sf(data = poly4, fill = NA, linetype = "solid", size = 0.3)+
                                  # geom_sf(data = poly3, fill = NA, linetype = "dotdash", size = 0.3, colour = "snow")+
                                  # geom_sf(data = polyKK, fill = NA, linetype = "dotdash", size = 0.3, colour = "snow")+
                                  # geom_sf(data = poly2, fill = NA, linetype = "dotted", size = 0.3, colour = "snow")+
                                  # geom_sf(data = poly1, fill= "snow", linetype = "solid", size = 0.3) +

                              # FW future ICE polygons
                                  # geom_sf(data = poly4, fill = NA, linetype = "solid", size = 0.3)+
                                  # geom_sf(data = poly3, fill = NA, linetype = "dotdash", size = 0.3, colour = "black")+
                                  # geom_sf(data = polyKK, fill = NA, linetype = "dotdash", size = 0.3, colour = "black")+
                                  # geom_sf(data = poly2, fill = NA, linetype = "dotted", size = 0.3, colour = "black")+
                                  # geom_sf(data = poly1, fill= "snow", linetype = "solid", size = 0.3) +
                   
                              # This line was not used, it creates contours, as in the case of Chambault et al., 2022
                                  # geom_contour_filled(data=r_df, aes(x,y,z=value), breaks=seq(0,1,by=0.1))+
                        
                              # World shapefile from RNaturalEarth  
                                    geom_sf(fill='grey', data=world, lwd = 0.00001)+
                        
                              # Geographical limits
                              # MW
                                    scale_x_continuous(limits = c(-99.5, 16.5), expand=c(0,0)) +
                                    scale_y_continuous(limits = c(-79.05833, -45),expand=c(0,0)) +
                        
                              # FW
                                  # scale_x_continuous(limits = c(-180, 16.884), expand=c(0,0)) +
                                  # scale_y_continuous(limits = c(-83.606, -29.243),expand=c(0,0)) +
                        
                              # FS
                                  # scale_x_continuous(limits = c(-97, -20.44306), expand=c(0,0)) +
                                  # scale_y_continuous(limits = c(-79.05833, -48),expand=c(0,0)) +
                        
                              # Palettes, labels & style (theme)
                                   # scale_fill_viridis_c('Habitat suitability', direction = 1, option = "viridis", na.value="white", limits = c(0,0.5))+ 
                                   # scale_fill_gradientn(colors = viridisfake, na.value = "gray88", limits = c(0,1))+
                                     scale_fill_gradientn(colors = diff_palette, na.value = "gray88", limits = c(-0.55,0.55))+
                                   # scale_fill_gradientn(colors = dist_palette, na.value = "white", limits = c(0,1))+
                                     labs(x='Longitude\n',y='Latitude')+
                                     themeMapsCC
                      
                 # Append to list
                   myplots_j_a[[z]] <- ww
                 # myplots_j_b[[z]] <- ww
                 # myplots_j_c[[z]] <- ww
                   
                   
                 }

                 
             else if (z == 3){
                             
                      ww <-ggplot()+
                                 geom_raster(aes(x=x,y=y, fill=value),data=r_df)+
                            # MW present 
                                 # geom_sf(data = poly3, fill = NA, linetype = "dotdash", size = 0.3, colour = "snow")+
                                 # geom_sf(data = poly2, fill = NA, linetype = "dotted", size = 0.3, colour = "snow")+
                                 # geom_sf(data = poly1, linetype = "solid", size = 0.3, fill= "#FAF7F6") +
                                 
                            # MW future
                                 # geom_sf(data = poly3, fill = NA, linetype = "dotdash", size = 0.3, colour = "black")+
                                 # geom_sf(data = poly2, fill = NA, linetype = "dotted", size = 0.3, colour = "black")+
                                 # geom_sf(data = poly1, linetype = "solid", size = 0.3, fill="snow")+
                                 
                           # FW present
                                 # geom_sf(data = poly1, fill = NA, linetype = "longdash", size = 0.3, colour = "snow")+
                                 # geom_sf(data = poly2, fill = NA, linetype = "dotted", size = 0.3, colour = "snow")+
                                 # geom_sf(data = polyKK, fill = "snow", linetype = "dotdash", size = 0.3, colour = "snow")+
                                 # geom_sf(data = poly3, fill = NA, linetype = "dotdash", size = 0.3, colour = "snow")+
                                 # geom_sf(data = poly4, fill= "snow", linetype = "solid", size = 0.3) +
                                 
                           # FW future
                                  geom_sf(data = poly2, fill = NA, linetype = "dotted", size = 0.3, colour = "snow")+
                                  geom_sf(data = polyKK, fill = "snow", linetype = "dotdash", size = 0.3, colour = "snow")+
                                  geom_sf(data = poly1, fill = NA, linetype = "longdash", size = 0.3)+
                                  geom_sf(data = poly3, fill = "snow", linetype = "solid", size = 0.3)+
                                 # geom_sf(data = poly4, fill= "snow", linetype = "solid", size = 0.3) +
                                 
                           # This line was not used, it creates contours, as in the case of Chambault et al., 2022
                                 # geom_contour_filled(data=r_df, aes(x,y,z=value), breaks=seq(0,1,by=0.1))+
                        
                           # World shapefile from RNaturalEarth  
                                 geom_sf(fill='grey', data=world, lwd = 0.00001)+
                        
                           # Geographical limits
                                 # scale_x_continuous(limits = c(-97, -20.44306), expand=c(0,0)) +
                                 # scale_y_continuous(limits = c(-79.05833, -48),expand=c(0,0)) +
                        
                                 # scale_x_continuous(limits = c(-99.5, 16.5), expand=c(0,0)) +
                                 # scale_y_continuous(limits = c(-79.05833, -45),expand=c(0,0)) +
                        
                                 scale_x_continuous(limits = c(-180, 16.884), expand=c(0,0)) +
                                 scale_y_continuous(limits = c(-83.606, -29.243),expand=c(0,0)) +
                        
                          # Palettes, labels & style (theme)
                                 # scale_fill_viridis_c('Habitat suitability', direction = 1, option = "viridis", na.value="white", limits = c(0,0.5))+ # slopehalf chl
                                 # scale_fill_gradientn(colors = viridisfake, na.value = "gray88", limits = c(0,1))+
                                 scale_fill_gradientn(colors = diff_palette, na.value = "gray88", limits = c(-0.55,0.55))+
                                 # scale_fill_gradientn(colors = dist_palette, na.value = "white", limits = c(0,1))+
                                 labs(x='Longitude\n',y='Latitude')+
                                 themeMapsCC +
                               # theme(legend.position = "right",
                               #       legend.direction = "vertical",
                               #       legend.title = element_text(size = 14, angle = 90),
                               #       legend.key.height=grid::unit(2,"cm"),
                               #       legend.key.width=grid::unit(0.4,"cm"),
                               #       legend.text = element_text(size = 12),
                               #       legend.margin = ggplot2::margin(0,0,0,0),
                               #       legend.box.margin = ggplot2::margin(0,0,0,0),
                               #       plot.margin = unit(c(10,20,10,10), "points")) +
                               # legend title
                               # guides(fill = guide_colorbar(title.position = "right",
                               #                              title.hjust = 0.5,
                               #                              frame.colour = "black",
                               #                              ticks = TRUE))
                               # 
                               
                                 # myplots_j_a[[z]] <- ww
                                 # myplots_j_b[[z]] <- ww
                                   myplots_j_c[[z]] <- ww
                                 
                                 
                                       }
                                           }
            
            
myplots_j_joined <- c(myplots_j_a, myplots_j_b, myplots_j_c)
myplots_j_joined <- c(myplots_j_a, myplots_j_b)


# Save figures in pdf format
ggsave(filename = "femalesW_aaa_testdiffaaa.pdf", plot = gridExtra::marrangeGrob(myplots_j_joined, nrow=3, ncol=3), width = 10, height = 10, limitsize = F)
ggsave(filename = "malesWww.pdf", plot = gridExtra::marrangeGrob(myplots_j_joined, nrow=3, ncol=3), width = 10, height = 10, limitsize = F)
ggsave(filename = "femalesS_aa.pdf", plot = gridExtra::marrangeGrob(myplots_j_joined, nrow=1, ncol=3), width = 10, height = 10, limitsize = F)
ggsave(filename = "FScpf_diff_meanPresent20192025.pdf", plot = gridExtra::marrangeGrob(myplots_j_joined, nrow=1, ncol=3), width = 10, height = 10, limitsize = F)
ggsave(filename = "MW_diff_meanPresent20192025.pdf", plot = gridExtra::marrangeGrob(myplots_j_joined, nrow=3, ncol=3), width = 10, height = 10, limitsize = F)
ggsave(filename = "FW_diff_meanPresent20192025.pdf", plot = gridExtra::marrangeGrob(myplots_j_joined, nrow=3, ncol=3), width = 10, height = 10, limitsize = F)
ggsave(filename = "FScpfNOphilopatry_diff_meanPresent20192025.pdf", plot = gridExtra::marrangeGrob(myplots_j_joined, nrow=1, ncol=3), width = 10, height = 10, limitsize = F)
ggsave(filename = "FScpfonlyPresent.pdf", plot = gridExtra::marrangeGrob(myplots_j_joined, nrow=1, ncol=3), width = 10, height = 10, limitsize = F)
ggsave(filename = "FWwithoutDecember.pdf", plot = gridExtra::marrangeGrob(myplots_j_joined, nrow=3, ncol=3), width = 10, height = 10, limitsize = F)




# CENTROIDS (supplementary material) ---------------------------
# Prediction directories
predict_repoMW <- paste0(output_data, "/", "habitat-model-cc/GAZ/MW/brt/full_model/predict_boost")
predict_repoFW <- paste0(output_data, "/", "habitat-model-cc/GAZ/FW/brt/full_model/predict_boost")
predict_repoFS <- paste0(output_data, "/", "habitat-model-cc/GAZ/FS/brt/full_model/predict_boost")
predict_repoFScpf <- paste0(output_data, "/", "habitat-model-cc/GAZ/FScpf/brt/full_model/predict_boost")

# Subset by sexseason combination
predict_files <- dir(predict_repoFScpf, recursive = T, full.names = T, pattern = ".tif")
predict_files_ <- predict_files[predict_files %notlike% "cir" & (predict_files %like% "126" | predict_files %like% "585")]

# Extents
MW_extent <- extent(c(-99.5, 16.5, -79.05833, -45))
FW_extent <- extent(c(-180, 16.884, -83.606, -29.243))
FS_extent <- extent(c(-97, -20.44306, -79.05833, -48))

# Create empty list
centroid_along_list <- list()
ssp <- c("ssp126", "ssp585")

# Prepare cluster
cl <- makeCluster(5)
registerDoParallel(cl)


l <- foreach(j = 1:length(predict_files_), .packages=c("raster", "dplyr", "lubridate", "dplyr", "data.table")) %dopar% {
   # for (j in 1:length(predict_files_)){
                  print(j)
                  file <- predict_files_[j]
                  r <- raster(file)
                  r_ <- crop(r, MW_extent)
                  r_df <- as.data.frame(r_, xy = T)
                  colnames(r_df)[3] <- "habitat_suitability"
                  r_df <- round(r_df, digits = 6)
                  r_df$habitat_suitability[r_df$habitat_suitability < 0.01] <- 0   
                  suitable_locs <- r_df[r_df$habitat_suitability > 0,]
                  suitable_locs_ <- suitable_locs[complete.cases(suitable_locs),]
                  suitable_locs_ <- suitable_locs_[suitable_locs_$habitat_suitability > 0.1,]
                  xy <- data.frame(cbind(suitable_locs_$x, suitable_locs_$y))
                  centroid <- xy %>% summarise(lon=mean(xy[,1]), lat=mean(xy[,2]))
                  centroid$date <- ymd(substr(basename(file), 1, 8))
      
                 if (file %like% "ssp126"){
                     centroid$ssp <- "ssp126" } else if (file %like% "ssp585") {
                     centroid$ssp <- "ssp585"}
                   # centroid_along_list[[j]] <- centroid
                     centroid
  
                       }

zz <- do.call(rbind, l)

# MW SSP1-2.6
zz_early_126 <- zz[zz$ssp == "ssp126" & (month(zz$date) == "3" | month(zz$date) == "4" | month(zz$date) == "5"),]
zz_mid_126 <- zz[zz$ssp == "ssp126" & (month(zz$date) == "6" | month(zz$date) == "7" | month(zz$date) == "8"),]
zz_late_126 <- zz[zz$ssp == "ssp126" & (month(zz$date) == "9"),]

# MW SSP5-8.5
zz_early_585 <- zz[zz$ssp == "ssp585" & (month(zz$date) == "3" | month(zz$date) == "4" | month(zz$date) == "5"),]
zz_mid_585 <- zz[zz$ssp == "ssp585" & (month(zz$date) == "6" | month(zz$date) == "7" | month(zz$date) == "8"),]
zz_late_585 <- zz[zz$ssp == "ssp585" & (month(zz$date) == "9"),]

  
# FW SSP1-2.6
# zz_early_126 <- zz[zz$ssp == "ssp126" & (month(zz$date) == "3" | month(zz$date) == "4" | month(zz$date) == "5"),]
# zz_mid_126 <- zz[zz$ssp == "ssp126" & (month(zz$date) == "6" | month(zz$date) == "7" | month(zz$date) == "8"),]
# zz_late_126 <- zz[zz$ssp == "ssp126" & (month(zz$date) == "9" | month(zz$date) == "10" | month(zz$date) == "11" | month(zz$date) == "12"),]
 
# FW SSP5-8.5
# zz_early_585 <- zz[zz$ssp == "ssp585" & (month(zz$date) == "3" | month(zz$date) == "4" | month(zz$date) == "5"),]
# zz_mid_585 <- zz[zz$ssp == "ssp585" & (month(zz$date) == "6" | month(zz$date) == "7" | month(zz$date) == "8"),]
# zz_late_585 <- zz[zz$ssp == "ssp585" & (month(zz$date) == "9" | month(zz$date) == "10" | month(zz$date) == "11" | month(zz$date) == "12"),]

list_centroids <- list(zz_early_126, zz_mid_126, zz_late_126, zz_early_585, zz_mid_585, zz_late_585)

# FS
# zz_summer_126 <- zz[zz$ssp == "ssp126",]
# zz_summer_585 <- zz[zz$ssp == "ssp585",]

# list_centroids <- list(zz_summer_126, zz_summer_585)


# WAY 1: without legend, not as pretty

colss <- paletteer_c("grDevices::Spectral", 81, direction = -1)
myplots_j <- list()  # new empty list

for (u in 1:length(list_centroids)){
                   centroid <- list_centroids[[u]] %>% group_by(year(date)) %>% summarise(lon=mean(lon), lat=mean(lat))
                   colnames(centroid)[1] <- "date"
                   map_centroids<-ggplot(centroid) + 
                                    geom_point(aes(lon, lat, color=date), size= 0.2) + 
                                    scale_color_gradientn(colors = colss)+themeMapsCC+
                                    geom_sf(fill='grey', data=world, lwd = 0.00001)+
                     
                                  scale_x_continuous(limits = c(-99.5, 16.5), expand=c(0,0)) +
                                  scale_y_continuous(limits = c(-79.05833, -45),expand=c(0,0))
                     
                                  # scale_x_continuous(limits = c(-180, 16.884), expand=c(0,0)) +
                                  # scale_y_continuous(limits = c(-83.606, -29.243),expand=c(0,0))+
                     
                                    # scale_x_continuous(limits = c(-80, -50), expand=c(0,0)) +
                                    # scale_y_continuous(limits = c(-70, -57),expand=c(0,0))
                   
                                    myplots_j[[u]] <- map_centroids
                
                
}



# WAY 2: with legend, larger, prettier
myplots_g <- list()  # new empty list

for (y in 1:length(list_centroids)){
  centroid <- list_centroids[[u]] %>% group_by(year(date)) %>% summarise(lon=mean(lon), lat=mean(lat))
  colnames(centroid)[1] <- "date"
  
  
  map_centroids<-ggplot(centroid) + 
    geom_point(aes(lon, lat, color=date), size=1) + 
    scale_color_gradientn(colors = colss)+themeMapsCC+
    geom_sf(fill='grey', data=world, lwd = 0.00001)+
    scale_x_continuous(limits = c(-99.5, 16.5), expand=c(0,0)) +
    scale_y_continuous(limits = c(-79.05833, -45),expand=c(0,0))+
    theme(legend.position = "right",
          legend.direction = "vertical",
          legend.title = element_text(size = 14, angle = 90),
          legend.key.height=grid::unit(2,"cm"),
          legend.key.width=grid::unit(0.4,"cm"),
          legend.text = element_text(size = 12),
          legend.margin = ggplot2::margin(0,0,0,0),
          legend.box.margin = ggplot2::margin(0,0,0,0),
          plot.margin = unit(c(10,20,10,10), "points")) +
    # legend title
    guides(fill = guide_colorbar(title.position = "right",
                                 title.hjust = 0.5,
                                 frame.colour = "black",
                                 ticks = TRUE))
  
  # scale_x_continuous(limits = c(-180, 16.884), expand=c(0,0)) +
  # scale_y_continuous(limits = c(-83.606, -29.243),expand=c(0,0))+
  myplots_g[[y]] <- map_centroids
}



ggsave(filename = "centroids_fscpf.pdf", plot = gridExtra::marrangeGrob(myplots_j, nrow=3, ncol=2), width = 10, height = 10, limitsize = F)




# -------------------------------------------------------------------------




# HABITAT SUITABILITY TRENDS ----------------------------------------------

# Loop to reclassify locations into 0/1 (suitable == habitat suitability > 0.1) & calculate the surface of suitable habitat by month
surface_df <- data.frame()
  system.time(
       l <- foreach(j =  1:length(predict_files_), .packages=c("raster", "dplyr", "lubridate", "dplyr", "data.table", "adehabitatHR")) %dopar% {
     # for (j in 1:length(predict_files_)){
                print(j)
                file <- predict_files_[j]
                r <- raster(file)
                r_ <- crop(r, MW_extent)
                m <- c(0, 0.1, 0,  0.1, 1, 1,  1, 1000, 0)    
                rcl <- matrix(m, ncol=3, byrow=TRUE)
                CC <-reclassify(r_, rcl, na.rm=TRUE)
                kk<- rasterToPolygons(CC, fun=function(x){x==1}, n=4, na.rm=TRUE, digits=12, dissolve=TRUE)
                
                if (is.null(kk) == TRUE) {kkk <- 0} else if (is.null(kk) == FALSE){
                kkk <- geosphere::areaPolygon(x=kk)/1000000 } # km2 
      
                mmm <- data.frame(kkk)
                mmm$date <- substr(basename(file), 1, 8)
    
               if (file %like% "ssp126"){
                   mmm$ssp <- "ssp126" } else if (file %like% "ssp585"){
                   mmm$ssp <- "ssp585" }
                   print(mmm)
                }
             ) 
  
zz <- do.call(rbind, l)

# Get the surface with the maximum suitable habitat area
colnames(zz)[1] <- "surface_km2"
zz_max <- zz %>% group_by(ssp) %>%
                 summarise(max = max(surface_km2))

# Create new column with max surface 
zz$max_surface <- ifelse(zz$ssp == "ssp126", zz_max$max[1] , ifelse(zz$ssp == "ssp585", zz_max$max[2], NA))

# Normalize surfaces by dividing by the maximum suitable habitat surface
zz$surface_lm <- zz$surface_km2/zz$max_surface

# Format dates
zz$date <-  ymd(zz$date)
zz$date_ymd <-  ymd(zz$date)

# Present
zz_present <-  zz[year(zz$date_ymd) == "2019"  | year(zz$date_ymd) == "2020"  | year(zz$date_ymd) == "2021"  |
                    year(zz$date_ymd) == "2022"  | year(zz$date_ymd) == "2023"  | year(zz$date_ymd) == "2024"  |
                    year(zz$date_ymd) == "2025",]

zz_present_early <- zz_present[(month(zz_present$date_ymd) == "3" | month(zz_present$date_ymd) == "4" | month(zz_present$date_ymd) == "5"),]
zz_present_mid <- zz_present[(month(zz_present$date_ymd) == "6" | month(zz_present$date_ymd) == "7" | month(zz_present$date_ymd) == "8"),]
zz_present_late <- zz_present[(month(zz_present$date_ymd) == "9" | month(zz_present$date_ymd) == "10" | month(zz_present$date_ymd) == "11"),]

# Calculate the mean suitable habitat area for early winter in the present
aa <- mean(zz_present_early$surface_km2)
# Calculate radius to draw the circles of the first version of the graphical abstract (final version has barplots)
aa_ <- sqrt((4*aa)/pi)

# Calculate the mean suitable habitat area for mid winter in the present
bb <- mean(zz_present_mid$surface_km2)
# Calculate radius to draw the circles of the first version of the graphical abstract (final version has barplots)
bb_ <- sqrt((4*bb)/pi)

# Calculate the mean suitable habitat area for late winter in the present
cc <- mean(zz_present_late$surface_km2)
# Calculate radius to draw the circles of the first version of the graphical abstract (final version has barplots)
cc_ <- sqrt((4*cc)/pi)

# Get the mean for the present
aa <- mean(zz_present$surface_km2)
# Calculate radius for the present
aa_ <- sqrt((4*aa)/pi)

# Get data for the whole winter
# zz_585_winter <-  zz_585[year(zz_126$date_ymd) == "2100" & zz_585$ssp == "ssp585" & (month(zz_585$date_ymd) == "3" | month(zz_585$date_ymd) == "4" | month(zz_585$date_ymd) == "5"
#                                                                                      | month(zz_585$date_ymd) == "6" | month(zz_585$date_ymd) == "7" | month(zz_585$date_ymd) == "8"
#                                                                                      | month(zz_585$date_ymd) == "9" | month(zz_585$date_ymd) == "10" | month(zz_585$date_ymd) == "11"),]

# Winter SSP1-2.6 2100 surfaces
# Subset data for SSP1-2.6
zz_126 <- zz[zz$ssp == "ssp126",]
# Format dates
zz_126$date_ymd <- ymd(zz_126$date)
# Get surface for 2100
zz_126_winter <-  zz_126[year(zz_126$date_ymd) == "2100" & zz_126$ssp == "ssp126",]

# Split winter of 2100 into early, mid and late winter
zz_126Winter_early <- zz_126_winter[(month(zz_126_winter$date_ymd) == "3" | month(zz_126_winter$date_ymd) == "4" | month(zz_126_winter$date_ymd) == "5"),]
zz_126Winter_mid <- zz_126_winter[(month(zz_126_winter$date_ymd) == "6" | month(zz_126_winter$date_ymd) == "7" | month(zz_126_winter$date_ymd) == "8"),]
zz_126Winter_late <- zz_126_winter[(month(zz_126_winter$date_ymd) == "9" | month(zz_126_winter$date_ymd) == "10" | month(zz_126_winter$date_ymd) == "11"),]

# Calculate the mean suitable habitat area for early winter of 2100 in SSP1-2.6
aa <- mean(zz_126Winter_early$surface_km2)
# Calculate radius to draw the circles of the first version of the graphical abstract (final version has barplots)
aa_ <- sqrt((4*aa)/pi)

# Calculate the mean suitable habitat area for mid winter of 2100 in SSP1-2.6
bb <- mean(zz_126Winter_mid$surface_km2)
# Calculate radius to draw the circles of the first version of the graphical abstract (final version has barplots)
bb_ <- sqrt((4*bb)/pi)

# Calculate the mean suitable habitat area for late winter of 2100 in SSP1-2.6
cc <- mean(zz_126Winter_late$surface_km2)
# Calculate radius to draw the circles of the first version of the graphical abstract (final version has barplots)
cc_ <- sqrt((4*cc)/pi)


# Winter SSP5-8.5 2100 surfaces
# Subset data for SSP5-8.5 
zz_585 <- zz[zz$ssp == "ssp585",]
# Format dates
zz_585$date_ymd <- ymd(zz_585$date)
# Get surface for 2100
zz_585_winter <-  zz_585[year(zz_585$date_ymd) == "2100" & zz_585$ssp == "ssp585",]

# Split winter of 2100 into early, mid and late winter
zz_585Winter_early <- zz_585_winter[(month(zz_585_winter$date_ymd) == "3" | month(zz_585_winter$date_ymd) == "4" | month(zz_585_winter$date_ymd) == "5"),]
zz_585Winter_mid <- zz_585_winter[(month(zz_585_winter$date_ymd) == "6" | month(zz_585_winter$date_ymd) == "7" | month(zz_585_winter$date_ymd) == "8"),]
zz_585Winter_late <- zz_585_winter[(month(zz_585_winter$date_ymd) == "9" | month(zz_585_winter$date_ymd) == "10" | month(zz_585_winter$date_ymd) == "11"),]


# Calculate the mean suitable habitat area for early winter of 2100 in SSP5-8.5
aa <- mean(zz_585Winter_early$surface_km2)
# Calculate radius to draw the circles of the first version of the graphical abstract (final version has barplots)
aa_ <- sqrt((4*aa)/pi)

# Calculate the mean suitable habitat area for mid winter of 2100 in SSP5-8.5
bb <- mean(zz_585Winter_mid$surface_km2)
# Calculate radius to draw the circles of the first version of the graphical abstract (final version has barplots)
bb_ <- sqrt((4*bb)/pi)

# Calculate the mean suitable habitat area for late winter of 2100 in SSP5-8.5
cc <- mean(zz_585Winter_late$surface_km2)
# Calculate radius to draw the circles of the first version of the graphical abstract (final version has barplots)
cc_ <- sqrt((4*cc)/pi)


# FEMALES WINTER: mean 2100 ssp 126: 9033778 km2 == 33,91 mm diameter // mean 2100 ssp 585: 7012895 km2 == 29,88 mm diameter // present: 9867647 km2  == 35,44 mm diameter
# MALES WINTER: mean 2100 ssp 126: 366783.4 km2 == 6,83 (x3 = 20,49) mm diameter // mean 2100 ssp 585: 330222.9 km2 == 6,48 (x3 = 19,44) mm diameter // present: 350598 km2  == 6,68 (x3 = 20,04) mm diameter
# FEMALES SUMMER: mean 2100 ssp 126: 84519.55 km2 == 3,28 (x5 = 16,40) mm diameter // mean 2100 ssp 585: 78494.49 km2 == 3,16 (x5 = 15,80) mm diameter // present: 56864.63 km2 == 2,69 (x5 = 13,45) mm diameter

# ---------------


zz_early_585 <- zz[zz$ssp == "ssp585" & (month(zz$date) == "3" | month(zz$date) == "4" | month(zz$date) == "5"),]
zz_mid_585 <- zz[zz$ssp == "ssp585" & (month(zz$date) == "6" | month(zz$date) == "7" | month(zz$date) == "8"),]
zz_late_585 <- zz[zz$ssp == "ssp585" & (month(zz$date) == "9" | month(zz$date) == "10" | month(zz$date) == "11" | month(zz$date) == "12"),]

dff<- zz_late_585 %>%
                  group_by((year(date))) %>%
                  dplyr::summarize(Mean = mean(surface_km2, na.rm=TRUE))

colnames(dff)[1] <- "date"


zz_early_585 <- zz[zz$ssp == "ssp585" & (month(zz$date) == "3" | month(zz$date) == "4" | month(zz$date) == "5"),]
zz_mid_585 <- zz[zz$ssp == "ssp585" & (month(zz$date) == "6" | month(zz$date) == "7" | month(zz$date) == "8"),]
zz_late_585 <- zz[zz$ssp == "ssp585" & (month(zz$date) == "9" | month(zz$date) == "10" | month(zz$date) == "11" | month(zz$date) == "12"),]




# SUMMER
colnames(zz)[1] <- "surface_km2"

zz_summer_126 <- zz[zz$ssp == "ssp126",]
zz_summer_585 <- zz[zz$ssp == "ssp585",]

dff<- zz_summer_126 %>%
  group_by((year(date))) %>%
  dplyr::summarize(Mean = mean(surface_km2, na.rm=TRUE))

colnames(dff)[1] <- "date"




list_trends <- list(zz_early_585, zz_mid_585, zz_late_585)


list_trends <- list(zz_summer_585)




lm_df <- data.frame(date_=numeric(), ssp = character(), slope=numeric(), pvalue=numeric())
lm_df_list<-list()

f=1


for (b in 1:length(ssp)){
  
  ssp_ <- ssp[b]
  
  for (f in unique(month(zz$date))){    
    # MW FW
    month <- f
    
    
    # FS
    
    # if(f <=3){
    #   month <- f
    # } else {
    #   month <- 12
    #   
    # }
    # 
    
    # MW FW
    zz_month <- zz[month(zz$date) == f & zz$ssp == ssp_,]
    
    # FS
    # zz_month <- zz[month(zz$date) == month & zz$ssp == ssp_,]
    
    
    
    # month_ <- month(zz_month$date[t])
    fit <- lm(formula = surface_km2 ~ year(date), data = zz_month)
    slope <-fit$coefficients[2]  
    pvalue <- coef(summary(fit))[8] 
    
    lm_df[f,1] <- month
    lm_df[f,2] <- ssp_
    lm_df[f,3] <- slope
    lm_df[f,4] <- pvalue
    
    
  }
  
  
  lm_df_list[[b]] <- lm_df
  
  
  
}

lm_summary <- do.call(rbind, lm_df_list)
MW_months <- subset(lm_summary, lm_summary$date_ >= 3 &  lm_summary$date_ <= 9)
lm_summary<-as.numeric(lm_summary$date_)

lm_summary_MW <- lm_summary[lm_summary$date_ >= 3 &  lm_summary$date_ <= 9,]
lm_summary_FW <- lm_summary[lm_summary$date_ >= 3 &  lm_summary$date_ <= 12,]


theme2Review<-theme_bw()+theme(axis.text.x=element_text(size=10.5),
                               axis.text.y = element_text(size=8),
                               axis.title.y=element_text(size=12),
                               axis.title.x = element_text(size=12),
                               legend.text = element_text(size=11),
                               legend.title = element_text(face="bold", size=13),
                               legend.key.size = unit(0.9, "cm"),
                               legend.background = element_rect(colour ="black", size=0.5, linetype = "dotted"),
                               legend.direction = "vertical",
                               legend.position = "none",
                               plot.margin = ggplot2::margin(2, 2, 2, 2, "cm"), strip.background =element_rect(fill="gray59"),
                               strip.text = element_blank(),
                               strip.placement = "right")



vv<-as.numeric(lm_summary$date_)


lm_summary <- read.csv(file = "slopes_trends_habitats.csv")

lm_summary$date_<- as.factor(lm_summary$date_)


trend_lm_surface_FW_ <- ggplot(lm_summary, aes(date_, slope, fill=factor(ssp)))+
                                geom_bar(stat="identity",position="dodge", width = 0.8)+
                                facet_grid(~ssp, scales="free",space = "free")+
                                xlab("\nMediterranean pelagic fish species")+
                                ylab("Slope from linear regressions\n")+
                                ylim(c(-60000, 10000))+
                                scale_fill_fish_d(option = "Taeniura_lymma", name="Decade")+theme2Review+
                                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6))



list_trend_plots <- list(trend_lm_surface_MW, trend_lm_surface_FW)
list_trend_plots <- list(trend_lm_surface_FW, trend_lm_surface_FW_)

ggsave(filename = "trends_habitat.pdf", plot = gridExtra::marrangeGrob(list_trend_plots, nrow=2, ncol=1), width = 7, height = 10, limitsize = F)


  
  
  


  






































































