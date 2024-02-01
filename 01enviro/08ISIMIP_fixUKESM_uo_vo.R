ISIMIP_models <- dir("D:/ISIMIP_netCDF/models/", recursive = F, full.names = T )
UKESM1 <- ISIMIP_models[5]
nc_var_list <- c("tos","uo", "vo")



for (i in 1:length(nc_var_list)){
  nc_var <- nc_var_list[i]
  ssp_folders<- dir(UKESM1[i], recursive = F, full.names = T )
  ssp <- gsub('.*/ ?(\\w+)', '\\1', ssp_folders)
    
    for (q in 1:length(ssp)){
      ISIMIP_files <- dir(ssp_folders[q], recursive = T, full.names = T )
      ISIMIP_files_var <- ISIMIP_files[ISIMIP_files %like% nc_var]
      filename <- gsub('.*/ ?(\\w+)', '\\1', ISIMIP_files_var)
      nc <- nc_open(ISIMIP_files_var)
      var <-ncvar_get(nc, varid=nc_var, start=c(1,1,1,1), count=c(-1,-1, 1, -1))
      var <-ncdf4::ncvar_get(nc, varid=nc_var)
      
      
      lat<-ncvar_get(nc, varid="lat")
      lon<-ncvar_get(nc, varid="lon")
      time<-ncvar_get(nc, varid="time")
      time<-floor(time)
      date <-as.POSIXct(months(time), origin = "1601-01-01", format ="%Y-%m-%d")

      
      var_r<-brick(var)
      var_r<-(flip(var_r, direction="x"))
      var_r<-t(flip(var_r, direction="x"))
      bb<-extent(min(lon),max(lon),min(lat),max(lat))
      var_r<-setExtent(var_r,bb,keepres = F, snap = F)
      projection(var_r)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
      antarctica <- extent(-90, -20, -80, -40)
      var_r_antarctica <- crop(var_r, antarctica)
      months<-seq(1: nlayers(var_r_antarctica))
      
      for (k in 1:length(date)){
        
        YYYY <- year(date)[k]
        MM <- sprintf("%02d", month(date))[k]
        DD <- sprintf("%02d", day(date))[k]
        product_folder <- paste("D:/ISIMIP_netCDF/output", "monthly_rasters_ISIMIP_tif_gr", model, ssp, YYYY,  sep="/")
        if (!dir.exists(product_folder[q])) dir.create(paste(product_folder[q], sep = ""), recursive = TRUE)  # create output directory if does not exist
        
        r <- var_r_antarctica[[k]]
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
        
        
        
        
      }}}}




