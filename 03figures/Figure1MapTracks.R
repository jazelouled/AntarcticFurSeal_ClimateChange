library(data.table)
library(precrec)
library(ggplot2)
library(ggspatial)
library(paletteer)
library(ggnewscale)
library(RColorBrewer)
library(patchwork)
library(raster)
library(terra)
library(rnaturalearth)
library(sf)
library(rgdal)
# ggOceanMaps
# install.packages("ggOceanMaps")
# install.packages("ggOceanMapsData")
# devtools::install_github("MikkoVihtakari/ggOceanMapsData")
library(ggOceanMaps)
# https://github.com/AeN-R-Workshop/4-ggOceanMaps/blob/4fa3774240d441371dff369cc09f1734e5f2f95a/ggOceanMaps_workshop.Rmd

data(countriesHigh, package = "rworldxtra", envir = environment())
wm <- suppressMessages(fortify(countriesHigh))
locFiles <- dir(here::here("InputOutput/00output/01tracking/L1_locations/"), pattern = "locations.csv", full.names = T)
  
points_list <- list()
for (i in 1:length(locFiles)){
  file_locs <- read.csv(locFiles[i])
  file_locs_ <- dplyr::select(file_locs,id, date, lon, lat)
  file_locs_$date <- as.POSIXct(file_locs_$date,format= "%Y-%m-%d %H:%M")
  points_list[[i]] <- file_locs_

                    }

allLocs <- do.call("rbind", points_list)
FW_locs <- allLocs[allLocs$id %like% "FW",]
MW_locs <- allLocs[allLocs$id %like% "MW",]
FS_locs <- allLocs[allLocs$id %like% "FS",]


qq <- paletteer_c("grDevices::RdYlGn", 12)
FW_locs$month <- as.numeric(month(FW_locs$date))

FWmap <- ggOceanMaps::basemap(limits = c(-135,16.884, -83.606,-40), bathymetry = T, rotate = T, bathy.alpha = 0.6, grid.col = NA, land.border.col = NA) +
                              xlab("   Longitude") +
                              ylab("Latitude") +
                              # geom_spatial_point(data = FW_locs, aes(x = lon, y = lat), color = "black", size = 0.01)
                              geom_spatial_path(data = FW_locs, aes(x = lon, y = lat, group = id, colour = factor(month(FW_locs$date))), size  = 0.2)+
                              scale_color_manual(values =  qq, labels = month.abb[1:12])+
                              scale_x_continuous(breaks = c(0))+
                              theme(legend.position="none", plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))

MWmap <- ggOceanMaps::basemap(limits = c(-85, -30, -70, -50), bathymetry = T, rotate = T, bathy.alpha = 0.6, grid.col = NA, land.border.col = NA) +
                              xlab("   Longitude") +
                              ylab("Latitude") +
                              geom_spatial_path(data = MW_locs, aes(x = lon, y = lat, group = id, colour = factor(month(MW_locs$date))), size  = 0.2)+
                              scale_color_manual(values =  qq, labels = month.abb[1:12])+
                              theme(legend.position="none", plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))

FSmap <- ggOceanMaps::basemap(limits =  c(-65, -55.5, -64, -60), bathymetry = T, rotate = T, bathy.alpha = 0.6, grid.col = NA, land.border.col = NA) +
                              xlab("   Longitude") +
                              ylab("Latitude") +
                              # geom_spatial_point(data = FS_locs, aes(x = lon, y = lat), color = "black", size = 0.01)
                              geom_spatial_path(data = FS_locs, aes(x = lon, y = lat, group = id, colour = factor(month(FS_locs$date))), size  = 0.1)+
                              scale_color_manual(values =  qq, labels = month.abb[1:12])
                              # theme(legend.position="none", plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))

# The maps were saved separately in pdf format and edited in illustrator for better visualization. As ggOceanMaps was giving me different bathymetry color scales for FW, MW and FS, 
# I used the "select" tool in illustrator and changed the bathymetry colours there in a systematic manner, so the scale was common in all of them. 



# For circumpolar basemap:
# ggOceanMaps::basemap(limits = c(-180,16.884, -83.606,-29.243))