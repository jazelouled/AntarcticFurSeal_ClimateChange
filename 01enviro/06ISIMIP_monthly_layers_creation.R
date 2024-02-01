library(ncdf4)
library(data.table)
library(lubridate)
library(raster)
library(stringr)
# nc_var <- "tos"
# # nc_var <- "chl"
# nc_var <- "o2-surf"
# nc_var <- "siconc"
# nc_var <- "so-surf"
# nc_var <- "thetao"
# nc_var <- "uo"
# nc_var <- "vo"


# CMIP6 dataset errata:

# All IPSL Chlorophyll-a data are 3 orders of magnitude (1e3) too high because related variables are in g/m3 
# instead of kg/m3 as expected in CMIP6 data request. Users are invited to apply a factor of 10^-3 while a new 
# version of the affected dataset is published. Old dataset version will be unpublished from the ESGF. 
# https://errata.es-doc.org/static/view.html?uid=1496cb33-b5ba-2583-b9f6-961e6ad94347


distToIceEdge2 <- function(sic, oceanmask, thrs=0.15){
  # Calculate distance to ice edge
  # x: ice concentration raster
  # thrs: concentration threshold used to define the edge
  # oceanmask: raster with NA values on land cells
  #
  # returns raster with distance to edge.
  # positive values from ocean, negative from ice
  
  # transform to extent based on % threshold
  sic[sic >= thrs] <- 1  # sea icea (1)
  sic[sic < thrs] <- 2  # ocean (2)
  sic[is.na(sic)] <- 2
  
  # resample to coarser resolution
  sic_coarse <- resample(sic, oceanmask, method="ngb")
  
  # calculate distance to ice extent (from the ocean, positive)
  idist <- gridDistance(sic_coarse, origin = 1) 
  idist <- raster::mask(idist, oceanmask)
  
  # calculate distance to ice extent (from the ice, negative)
  #idist2 <- gridDistance(sic_coarse, origin = 2) 
  #idist2 <- mask(idist2, oceanmask)
  #idist2 <- idist2 * (-1)
  
  # combine
  #dist <- sum(idist, idist2)/1000 # transform to km
  dist <- idist/1000
  return(dist)
}


distToIceEdge <- function(sic, oceanmask, thrs=0.15){
  # Calculate distance to ice edge
  # x: ice concentration raster
  # thrs: concentration threshold used to define the edge
  # oceanmask: raster with NA values on land cells
  #
  # returns raster with distance to edge.
  # positive values from ocean, negative from ice
  
  # transform to extent based on % threshold
  sic[sic >= thrs] <- 1  # sea icea (1)
  sic[sic < thrs] <- 2  # ocean (2)
  sic[is.na(sic)] <- 2
  
  # resample to coarser resolution
  sic_coarse <- resample(sic, oceanmask, method="ngb")
  
  # calculate distance to ice extent (from the ocean, positive)
  idist <- gridDistance(sic_coarse, origin = 1) 
  idist <- raster::mask(idist, oceanmask)
  
  # calculate distance to ice extent (from the ice, negative)
  idist2 <- gridDistance(sic_coarse, origin = 2) 
  idist2 <- raster::mask(idist2, oceanmask)
  idist2 <- idist2 * (-1)
  
  # combine
  dist <- sum(idist, idist2)/1000 # transform to km
  return(dist)
}


# Code to create monthly layers of tos, chl, o2, siconc, so-surf, thetao, uo & vo. Also added code in the 
# end of the loop to calculate tos and so-surf gradients and distance to ice (> 15% sic).
# 

ssp <- c("histor", "ssp126", "ssp370", "ssp585")

# nc_var_list <- c("tos", "chl", "o2", "siconc", "so-surf", "thetao", "uo", "vo")
nc_var_list <- c("tos","so-surf", "siconc", "chl", "o2-surf",  "thetao", "uo", "vo", "phydiat", "mlotstmax")
# nc_var_list <- c("siconc", "chl", "o2-surf",  "thetao", "uo", "vo")
# nc_var_list <- c("chl")
# nc_var_list <- c("siconc")

# nc_var_list <- c("phydiat-vint", "mlotstmax")

nc_var_list <- c("ph-surf")


ISIMIP_models <- dir("D:/ISIMIP_netCDF/models/", recursive = F, full.names = T )
ISIMIP_models <- ISIMIP_models[2]

rgdal::setCPLConfigOption("GDAL_PAM_ENABLED", "FALSE")

for (i in 1:length(nc_var_list)){
  
  
nc_var <- nc_var_list[i]

for (z in 1:length(ISIMIP_models)){
  print(z)
  model <- gsub('.*/ ?(\\w+)', '\\1', ISIMIP_models[z])
  if((model == "MRI-ESM2-0")) next
  if((nc_var == "siconc" | nc_var == "chl"|nc_var == "o2-surf"| nc_var == "so-surf" | nc_var == "uo" | nc_var == "vo" | nc_var == "phydiat-vint" | nc_var == "mlotstmax")  & model == "MRI-ESM2-0") next
  if((nc_var == "phydiat-vint")  & (model == "MPI-ESM1-2-HR"| model == "UKESM1-ESM2-0")) next
  if((nc_var == "mlotstmax")  & (model == "GFDL-ESM4")) next
  
  
  ssp_folders<- dir(ISIMIP_models[z], recursive = F, full.names = T )
  ssp <- gsub('.*/ ?(\\w+)', '\\1', ssp_folders)
  
  for (q in 1:length(ssp)){
    print(q)
    ISIMIP_files <- dir(ssp_folders[q], recursive = T, full.names = T )
    ISIMIP_files_var <- ISIMIP_files[ISIMIP_files %like% nc_var]
    filename <- gsub('.*/ ?(\\w+)', '\\1', ISIMIP_files_var)
    nc <- nc_open(ISIMIP_files_var)
    
    if (nc_var == "chl" | nc_var == "thetao" | nc_var == "uo" | nc_var == "vo"){
      var <-ncvar_get(nc, varid=nc_var, start=c(1,1,1,1), count=c(-1,-1, 1, -1))
    } else (var <-ncdf4::ncvar_get(nc, varid=nc_var))
    
    lat<-ncvar_get(nc, varid="lat")
    lon<-ncvar_get(nc, varid="lon")
    time<-ncvar_get(nc, varid="time")
    time<-floor(time)
    
    if (model == "MRI-ESM2-0"){
      date <-as.POSIXct(days(time), origin = "2015-01-01", format ="%Y-%m-%d")
    } else {date <-as.POSIXct(months(time), origin = "1601-01-01", format ="%Y-%m-%d")
    }
    
    if (ssp[q] == "histor"){
      date <- subset(date, date >= "2006-01-01")
      
    }
    
    
    var_r<-brick(var)
    var_r<-(flip(var_r, direction="x"))
    var_r<-t(flip(var_r, direction="x"))
    bb<-extent(min(lon),max(lon),min(lat),max(lat))
    var_r<-setExtent(var_r,bb,keepres = F, snap = F)
    projection(var_r)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    # antarctica <- extent(-90, -20, -80, -40) # males winter extent
    antarctica <- extent(-180, 16.884, -83.606, -29.243)
    var_r_antarctica <- crop(var_r, antarctica)
    months<-seq(1: nlayers(var_r_antarctica))
    
    for (k in 1:length(date)){
      
      YYYY <- year(date)[k]
      MM <- sprintf("%02d", month(date))[k]
      DD <- sprintf("%02d", day(date))[k]
      product_folder <- paste("D:/ISIMIP_netCDF/output", "monthly_rasters_ISIMIP_tif_allVars", model, ssp, YYYY,  sep="/")
      if (!dir.exists(product_folder[q])) dir.create(paste(product_folder[q], sep = ""), recursive = TRUE)  # create output directory if does not exist
      
      r <- var_r_antarctica[[k]]
      if (model == "IPSL-CM6A-LR" & nc_var =="chl"){
        r <- r*0.001      }
      
      filename <- paste0(YYYY, MM, DD, "_", model,"_", ssp,"_", nc_var, ".tif")
      
      writeRaster(r, filename=paste0(product_folder[q], sep="","/", filename[q]), overwrite = T)     
      
      
      
      if (nc_var == "tos" | nc_var == "so-surf"){
        p <- terrain(r, opt="slope", unit="degrees", neighbors = 4)
        setZ(p, getZ(r))
        filename <- paste0(YYYY, MM, DD, "_", model,"_", ssp,"_", nc_var, "_gr", ".tif")
        writeRaster(p, filename=paste0(product_folder[q], sep="","/", filename[q]), overwrite = T)     
        
      } else if (nc_var == "siconc") {
        sic <- r
        sid<- distToIceEdge2(sic = sic, oceanmask = sic, thrs = 0.15)
        sid <- projectRaster(sid, sic, method="bilinear")
        filename <- paste0(YYYY, MM, DD, "_", model,"_", ssp,"_", "edgedist", ".tif")
        writeRaster(sid, filename=paste0(product_folder[q], sep="","/", filename[q]), overwrite = T)     
      }
      
      
    
      
    }
  }
}

}
    









  