library(raster)

`%notlike%` <- Negate(`%like%`)


zz
output_data <- "~/Dropbox/PhD/Arctocephalus-gazella/00output/"


predict_repoMW <- paste0(output_data, "/", "habitat-model-cc/GAZ/MW/brt/full_model/predict_boost")
predict_repoFW <- paste0(output_data, "/", "habitat-model-cc/GAZ/FW/brt/full_model/predict_boost")
predict_repoFScpf <- paste0(output_data, "/", "habitat-model-cc/GAZ/FScpf/brt/full_model/predict_boost")


predict_files <- dir(predict_repoMW, recursive = T, full.names = T, pattern = ".tif")



predict_files_ <- predict_files[predict_files %notlike% "cir" & (predict_files %like% "126" | predict_files %like% "585")]


MW_extent <- extent(c(-99.5, 16.5, -79.05833, -45))
FW_extent <- extent(c(-180, 16.884, -83.606, -29.243))
FS_extent <- extent(c(-97, -20.44306, -79.05833, -48))
cl <- makeCluster(5)
registerDoParallel(cl)
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
      
      kkk <- geosphere::areaPolygon(x=kk)/1000000 # km2
      
    }
    
    mmm <- data.frame(kkk)
    mmm$date <- substr(basename(file), 1, 8)
    
    if (file %like% "ssp126"){
      
      mmm$ssp <- "ssp126"} else if (file %like% "ssp585"){
        
        mmm$ssp <- "ssp585"
      }
    
    
    
    #centroid_along_list[[j]] <- centroid
    
    print(mmm)
  }
  
) 



zz <- do.call(rbind, l)

colnames(zz)[1] <- "surface_km2"

zz_max <- zz %>% group_by(ssp) %>%
  summarise(max = max(surface_km2))


zz_present <- subset(zz, zz$date >= 20190101 & zz$date <= 20250901)

zz_present <- subset(zz, zz$date >= 20190301 & zz$date <= 20251201)

zz_present <- subset(zz, zz$date >= 20190101 & zz$date <= 20251201)


zz_mean_present <- zz %>% group_by(ssp) %>%
                      summarise(mean = mean(surface_km2))

mean_present_ssp <- mean(zz_present$kkk)



zz_2100_126 <- subset(zz, zz$date >= 21000101 & zz$date <= 21000901 & zz$ssp == "ssp126")
zz_2100_126 <- subset(zz, zz$date >= 21000301 & zz$date <= 21001201 & zz$ssp == "ssp126")
zz_2100_126 <- subset(zz, zz$date >= 21000101 & zz$date <= 21001201 & zz$ssp == "ssp126")

mean_ssp126 <- mean(zz_2100_126$kkk)


zz_2100_585 <- subset(zz, zz$date >= 21000101 & zz$date <= 21000901 & zz$ssp == "ssp585")
zz_2100_585 <- subset(zz, zz$date >= 21000301 & zz$date <= 21001201 & zz$ssp == "ssp585")
zz_2100_585 <- subset(zz, zz$date >= 21000101 & zz$date <= 21001201 & zz$ssp == "ssp585")

mean_ssp585 <- mean(zz_2100_585$kkk)




zz$max_surface <- ifelse(zz$ssp == "ssp126", zz_max$max[1] , ifelse(zz$ssp == "ssp585", zz_max$max[2], NA))

zz$surface_lm <- zz$surface_km2/zz$max_surface


zz[[1]]

class(zz)

do.call(unlist, zz)

str(zz)

dd <- as.data.frame(zz)
zz$date <-  ymd(zz$date)




# mw 
# present: 321715.7
# 126: 315562.7
# 585: 284665.2



# fw 
# present: 8843050
# 126: 8769045
# 585: 6826605


# fs 
# present: 169875
# 126: 291256.3
# 585: 251755.1

