library(ncdf4)
library(raster)
library(data.table)
library(dplyr)
library(lubridate)


# Functions -------------------------------------------------------------------

fun_slope <- function(y) { 
  if(all(is.na(y))) {
    NA
  } else {
    m = lm(y ~ date); summary(m)$coefficients[2] 
  }
}



fun_pvalue <- function(y) { 
  if(all(is.na(y))) {
    NA
  } else {
    m = lm(y ~ date); summary(m)$coefficients[8] 
  }
}


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")


# Loops -------------------------------------------------------------------


# nc_var <- "tos"
nc_var <- "chl"
# nc_var <- "o2"
# nc_var <- "siconc"
# nc_var <- "so-surf"
# nc_var <- "thetao"
# nc_var <- "uo"
# nc_var <- "vo"

# nc_var <- "so-surf"

antarctica <- extent(-90, -20, -80, -40)

# GFDL-ESM4
brick_126_GFDL <- raster()
brick_126_GFDL <- setExtent(brick_126_GFDL, antarctica)
res(brick_126_GFDL) <- 1 # setting raster's resolution

brick_370_GFDL <- raster()
brick_370_GFDL <- setExtent(brick_126_GFDL, antarctica)
res(brick_370_GFDL) <- 1 # setting raster's resolution

brick_585_GFDL <- raster()
brick_585_GFDL <- setExtent(brick_126_GFDL, antarctica)
res(brick_585_GFDL) <- 1 # setting raster's resolution


# IPSL-CM6A-LR
brick_126_IPSL <- raster()
brick_126_IPSL <- setExtent(brick_126_IPSL, antarctica)
res(brick_126_IPSL) <- 1 # setting raster's resolution

brick_370_IPSL <- raster()
brick_370_IPSL <- setExtent(brick_126_IPSL, antarctica)
res(brick_370_IPSL) <- 1 # setting raster's resolution

brick_585_IPSL <- raster()
brick_585_IPSL <- setExtent(brick_126_IPSL, antarctica)
res(brick_585_IPSL) <- 1 # setting raster's resolution# MPI-ESM1-2-HR



# MPI-ESM1-2-HR
brick_126_MPI <- raster()
brick_126_MPI <- setExtent(brick_126_MPI, antarctica)
res(brick_126_MPI) <- 1 # setting raster's resolution

brick_370_MPI <- raster()
brick_370_MPI <- setExtent(brick_126_MPI, antarctica)
res(brick_370_MPI) <- 1 # setting raster's resolution

brick_585_MPI <- raster()
brick_585_MPI <- setExtent(brick_126_MPI, antarctica)
res(brick_585_MPI) <- 1 # setting raster's resolution


# MRI-ESM2-0
brick_126_MRI <- raster()
brick_126_MRI <- setExtent(brick_126_MRI, antarctica)
res(brick_126_MRI) <- 1 # setting raster's resolution

brick_370_MRI <- raster()
brick_370_MRI <- setExtent(brick_126_MRI, antarctica)
res(brick_370_MRI) <- 1 # setting raster's resolution

brick_585_MRI <- raster()
brick_585_MRI <- setExtent(brick_126_MRI, antarctica)
res(brick_585_MRI) <- 1 # setting raster's resolution


# UKESM1-ESM2-0
brick_126_UKESM1 <- raster()
brick_126_UKESM1 <- setExtent(brick_126_UKESM1, antarctica)
res(brick_126_UKESM1) <- 1 # setting raster's resolution

brick_370_UKESM1 <- raster()
brick_370_UKESM1 <- setExtent(brick_126_UKESM1, antarctica)
res(brick_370_UKESM1) <- 1 # setting raster's resolution

brick_585_UKESM1 <- raster()
brick_585_UKESM1 <- setExtent(brick_126_UKESM1, antarctica)
res(brick_585_UKESM1) <- 1 # setting raster's resolution



ISIMIP_models <- dir("D:/ISIMIP_netCDF/models/", recursive = F, full.names = T )


for (z in 1:length(ISIMIP_models)){
  print(z)
  model <- gsub('.*/ ?(\\w+)', '\\1', ISIMIP_models[z])
  if((nc_var == "siconc" | nc_var == "chl"|nc_var == "o2"| nc_var == "so-surf" | nc_var == "uo" | nc_var == "vo")  & model == "MRI-ESM2-0") next
  ssp_folders<- dir(ISIMIP_models[z], recursive = F, full.names = T )
  ssp <- gsub('.*/ ?(\\w+)', '\\1', ssp_folders)
  
  for (q in 1:length(ssp)){
    ISIMIP_files <- dir(ssp_folders[q], recursive = T, full.names = T )
    ISIMIP_files_var <- ISIMIP_files[ISIMIP_files %like% nc_var]
    filename <- gsub('.*/ ?(\\w+)', '\\1', ISIMIP_files_var)
    nc <- nc_open(ISIMIP_files_var)
    if (nc_var == "chl"){
      var <-ncvar_get(nc, varid=nc_var, start=c(1,1,1,1), count=c(-1,-1, 1, -1))
    } else (var <-ncvar_get(nc, varid=nc_var))
    lat<-ncvar_get(nc, varid="lat")
    lon<-ncvar_get(nc, varid="lon")
    time<-ncvar_get(nc, varid="time")
    time<-floor(time)
    if (model == "MRI-ESM2-0"){
      date <-as.POSIXct(days(time), origin = "2015-01-01", format ="%Y-%m-%d")
      
    } else {date <-as.POSIXct(months(time), origin = "1601-01-01", format ="%Y-%m-%d")
    
    }
    
    var_r<-brick(var)
    var_r<-(flip(var_r, direction="x"))
    var_r<-t(flip(var_r, direction="x"))
    bb<-extent(min(lon),max(lon),min(lat),max(lat))
    var_r<-setExtent(var_r,bb,keepres = F, snap = F)
    projection(var_r)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    antarctica <- extent(-90, -20, -80, -40)
    var_r_antarctica <- crop(var_r, antarctica)
    months<-seq(1: nlayers(var_r_antarctica))
    
      
      if (ssp[q] == "ssp126" & model == "GFDL-ESM4"){
        brick_126_GFDL<-brick(var)
        brick_126_GFDL<-(flip(brick_126_GFDL, direction="x"))
        brick_126_GFDL<-t(flip(brick_126_GFDL, direction="x"))
        bb<-extent(min(lon),max(lon),min(lat),max(lat))
        brick_126_GFDL<-setExtent(brick_126_GFDL,bb,keepres = F, snap = F)
        projection(brick_126_GFDL)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
        antarctica <- extent(-90, -20, -80, -40)
        brick_126_GFDL <- crop(brick_126_GFDL, antarctica)
        brick_126_GFDL_slope <- calc(brick_126_GFDL, fun_slope)
        brick_126_GFDL_pvalue <- calc(brick_126_GFDL,fun_pvalue)
        
        
        
      } else if (ssp[q] == "ssp370" & model == "GFDL-ESM4"){
        brick_370_GFDL<-brick(var)
        brick_370_GFDL<-(flip(brick_370_GFDL, direction="x"))
        brick_370_GFDL<-t(flip(brick_370_GFDL, direction="x"))
        bb<-extent(min(lon),max(lon),min(lat),max(lat))
        brick_370_GFDL<-setExtent(brick_370_GFDL,bb,keepres = F, snap = F)
        projection(brick_370_GFDL)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
        antarctica <- extent(-90, -20, -80, -40)
        brick_370_GFDL <- crop(brick_370_GFDL, antarctica)
        brick_370_GFDL_slope <- calc(brick_370_GFDL, fun_slope)
        brick_370_GFDL_pvalue <- calc(brick_370_GFDL,fun_pvalue)
        
        
      } else if (ssp[q] == "ssp585" & model == "GFDL-ESM4"){
        brick_585_GFDL<-brick(var)
        brick_585_GFDL<-(flip(brick_585_GFDL, direction="x"))
        brick_585_GFDL<-t(flip(brick_585_GFDL, direction="x"))
        bb<-extent(min(lon),max(lon),min(lat),max(lat))
        brick_585_GFDL<-setExtent(brick_585_GFDL,bb,keepres = F, snap = F)
        projection(brick_585_GFDL)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
        antarctica <- extent(-90, -20, -80, -40)
        brick_585_GFDL <- crop(brick_585_GFDL, antarctica)
        brick_585_GFDL_slope <- calc(brick_585_GFDL, fun_slope)
        brick_585_GFDL_pvalue <- calc(brick_585_GFDL,fun_pvalue)
        
        
        
        # IPSL-CM6A-LR
        
      } else if (ssp[q] == "ssp126" & model == "IPSL-CM6A-LR"){
        brick_126_IPSL<-brick(var)
        brick_126_IPSL<-(flip(brick_126_IPSL, direction="x"))
        brick_126_IPSL<-t(flip(brick_126_IPSL, direction="x"))
        bb<-extent(min(lon),max(lon),min(lat),max(lat))
        brick_126_IPSL<-setExtent(brick_126_IPSL,bb,keepres = F, snap = F)
        projection(brick_126_IPSL)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
        antarctica <- extent(-90, -20, -80, -40)
        brick_126_IPSL <- crop(brick_126_IPSL, antarctica)
        brick_126_IPSL_slope <- calc(brick_126_IPSL, fun_slope)
        brick_126_IPSL_pvalue <- calc(brick_126_IPSL,fun_pvalue)
        
      } else if (ssp[q] == "ssp370" & model == "IPSL-CM6A-LR"){
        brick_370_IPSL<-brick(var)
        brick_370_IPSL<-(flip(brick_370_IPSL, direction="x"))
        brick_370_IPSL<-t(flip(brick_370_IPSL, direction="x"))
        bb<-extent(min(lon),max(lon),min(lat),max(lat))
        brick_370_IPSL<-setExtent(brick_370_IPSL,bb,keepres = F, snap = F)
        projection(brick_370_IPSL)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
        antarctica <- extent(-90, -20, -80, -40)
        brick_370_IPSL <- crop(brick_370_IPSL, antarctica)
        brick_370_IPSL_slope <- calc(brick_370_IPSL, fun_slope)
        brick_370_IPSL_pvalue <- calc(brick_370_IPSL,fun_pvalue)
        
      
      } else if (ssp[q] == "ssp585" & model == "IPSL-CM6A-LR"){
        brick_585_IPSL<-brick(var)
        brick_585_IPSL<-(flip(brick_585_IPSL, direction="x"))
        brick_585_IPSL<-t(flip(brick_585_IPSL, direction="x"))
        bb<-extent(min(lon),max(lon),min(lat),max(lat))
        brick_585_IPSL<-setExtent(brick_585_IPSL,bb,keepres = F, snap = F)
        projection(brick_585_IPSL)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
        antarctica <- extent(-90, -20, -80, -40)
        brick_585_IPSL <- crop(brick_585_IPSL, antarctica)
        brick_585_IPSL_slope <- calc(brick_585_IPSL, fun_slope)
        brick_585_IPSL_pvalue <- calc(brick_585_IPSL,fun_pvalue)
        
        
        # MPI-ESM1-2-HR
        
      } else if (ssp[q] == "ssp126" & model == "MPI-ESM1-2-HR"){
        brick_126_MPI<-brick(var)
        brick_126_MPI<-(flip(brick_126_MPI, direction="x"))
        brick_126_MPI<-t(flip(brick_126_MPI, direction="x"))
        bb<-extent(min(lon),max(lon),min(lat),max(lat))
        brick_126_MPI<-setExtent(brick_126_MPI,bb,keepres = F, snap = F)
        projection(brick_126_MPI)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
        antarctica <- extent(-90, -20, -80, -40)
        brick_126_MPI <- crop(brick_126_MPI, antarctica)
        brick_126_MPI_slope <- calc(brick_126_MPI, fun_slope)
        brick_126_MPI_pvalue <- calc(brick_126_MPI,fun_pvalue)
        
        
      } else if (ssp[q] == "ssp370" & model == "MPI-ESM1-2-HR"){
        brick_370_MPI<-brick(var)
        brick_370_MPI<-(flip(brick_370_MPI, direction="x"))
        brick_370_MPI<-t(flip(brick_370_MPI, direction="x"))
        bb<-extent(min(lon),max(lon),min(lat),max(lat))
        brick_370_MPI<-setExtent(brick_370_MPI,bb,keepres = F, snap = F)
        projection(brick_370_MPI)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
        antarctica <- extent(-90, -20, -80, -40)
        brick_370_MPI <- crop(brick_370_MPI, antarctica)
        brick_370_MPI_slope <- calc(brick_370_MPI, fun_slope)
        brick_370_MPI_pvalue <- calc(brick_370_MPI,fun_pvalue)
        
        
      } else if (ssp[q] == "ssp585" & model == "MPI-ESM1-2-HR"){
        brick_585_MPI<-brick(var)
        brick_585_MPI<-(flip(brick_585_MPI, direction="x"))
        brick_585_MPI<-t(flip(brick_585_MPI, direction="x"))
        bb<-extent(min(lon),max(lon),min(lat),max(lat))
        brick_585_MPI<-setExtent(brick_585_MPI,bb,keepres = F, snap = F)
        projection(brick_585_MPI)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
        antarctica <- extent(-90, -20, -80, -40)
        brick_585_MPI <- crop(brick_585_MPI, antarctica)
        brick_585_MPI_slope <- calc(brick_585_MPI, fun_slope)
        brick_585_MPI_pvalue <- calc(brick_585_MPI,fun_pvalue)
        
        
      #   
      #   # MRI-ESM2-0
      #   
      # } else if (ssp[q] == "ssp126" & model == "MRI-ESM2-0"){
      #   brick_126_MRI<-brick(var)
      #   brick_126_MRI<-(flip(brick_126_MRI, direction="x"))
      #   brick_126_MRI<-t(flip(brick_126_MRI, direction="x"))
      #   bb<-extent(min(lon),max(lon),min(lat),max(lat))
      #   brick_126_MRI<-setExtent(brick_126_MRI,bb,keepres = F, snap = F)
      #   projection(brick_126_MRI)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
      #   antarctica <- extent(-90, -20, -80, -40)
      #   brick_126_MRI <- crop(brick_126_MRI, antarctica)
      #   brick_126_MRI_slope <- calc(brick_126_MRI, fun_slope)
      #   brick_126_MRI_pvalue <- calc(brick_126_MRI,fun_pvalue)
      #   
      #   
      # } else if (ssp[q] == "ssp370" & model == "MRI-ESM2-0"){
      #   brick_370_MRI<-brick(var)
      #   brick_370_MRI<-(flip(brick_370_MRI, direction="x"))
      #   brick_370_MRI<-t(flip(brick_370_MRI, direction="x"))
      #   bb<-extent(min(lon),max(lon),min(lat),max(lat))
      #   brick_370_MRI<-setExtent(brick_370_MRI,bb,keepres = F, snap = F)
      #   projection(brick_370_MRI)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
      #   antarctica <- extent(-90, -20, -80, -40)
      #   brick_370_MRI <- crop(brick_370_MRI, antarctica)
      #   brick_370_MRI_slope <- calc(brick_370_MRI, fun_slope)
      #   brick_370_MRI_pvalue <- calc(brick_370_MRI,fun_pvalue)
      #   
      #   
      # } else if (ssp[q] == "ssp585" & model == "MRI-ESM2-0"){
      #   brick_585_MRI<-brick(var)
      #   brick_585_MRI<-(flip(brick_585_MRI, direction="x"))
      #   brick_585_MRI<-t(flip(brick_585_MRI, direction="x"))
      #   bb<-extent(min(lon),max(lon),min(lat),max(lat))
      #   brick_585_MRI<-setExtent(brick_585_MRI,bb,keepres = F, snap = F)
      #   projection(brick_585_MRI)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
      #   antarctica <- extent(-90, -20, -80, -40)
      #   brick_585_MRI <- crop(brick_585_MRI, antarctica)
      #   brick_585_MRI_slope <- calc(brick_585_MRI, fun_slope)
      #   brick_585_MRI_pvalue <- calc(brick_585_MRI,fun_pvalue)
        
        
        # UKESM1-ESM2-0   
        
      } else if (ssp[q] == "ssp126" & model == "UKESM1-ESM2-0"){
        brick_126_UKESM1<-brick(var)
        brick_126_UKESM1<-(flip(brick_126_UKESM1, direction="x"))
        brick_126_UKESM1<-t(flip(brick_126_UKESM1, direction="x"))
        bb<-extent(min(lon),max(lon),min(lat),max(lat))
        brick_126_UKESM1<-setExtent(brick_126_UKESM1,bb,keepres = F, snap = F)
        projection(brick_126_UKESM1)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
        antarctica <- extent(-90, -20, -80, -40)
        brick_126_UKESM1 <- crop(brick_126_UKESM1, antarctica)
        brick_126_UKESM1_slope <- calc(brick_126_UKESM1, fun_slope)
        brick_126_UKESM1_pvalue <- calc(brick_126_UKESM1,fun_pvalue)
        
        
      } else if (ssp[q] == "ssp370" & model == "UKESM1-ESM2-0"){
        brick_370_UKESM1<-brick(var)
        brick_370_UKESM1<-(flip(brick_370_UKESM1, direction="x"))
        brick_370_UKESM1<-t(flip(brick_370_UKESM1, direction="x"))
        bb<-extent(min(lon),max(lon),min(lat),max(lat))
        brick_370_UKESM1<-setExtent(brick_370_UKESM1,bb,keepres = F, snap = F)
        projection(brick_370_UKESM1)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
        antarctica <- extent(-90, -20, -80, -40)
        brick_370_UKESM1 <- crop(brick_370_UKESM1, antarctica)
        brick_370_UKESM1_slope <- calc(brick_370_UKESM1, fun_slope)
        brick_370_UKESM1_pvalue <- calc(brick_370_UKESM1,fun_pvalue)
        
        
      } else if (ssp[q] == "ssp585" & model == "UKESM1-ESM2-0"){
        brick_585_UKESM1<-brick(var)
        brick_585_UKESM1<-(flip(brick_585_UKESM1, direction="x"))
        brick_585_UKESM1<-t(flip(brick_585_UKESM1, direction="x"))
        bb<-extent(min(lon),max(lon),min(lat),max(lat))
        brick_585_UKESM1<-setExtent(brick_585_UKESM1,bb,keepres = F, snap = F)
        projection(brick_585_UKESM1)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
        antarctica <- extent(-90, -20, -80, -40)
        brick_585_UKESM1 <- crop(brick_585_UKESM1, antarctica)
        brick_585_UKESM1_slope <- calc(brick_585_UKESM1, fun_slope)
        brick_585_UKESM1_pvalue <- calc(brick_585_UKESM1,fun_pvalue)
        
        
      }
    }
  }


bricks_list <- list(brick_126_GFDL, brick_370_GFDL, brick_585_GFDL, 
                    brick_126_IPSL, brick_370_IPSL, brick_585_IPSL,
                    brick_126_MPI,brick_370_MPI, brick_585_MPI, 
                    # brick_126_MRI, brick_370_MRI, brick_585_MRI,
                    brick_126_UKESM1, brick_370_UKESM1, brick_585_UKESM1)


names <- c("brick_126_GFDL", "brick_370_GFDL", "brick_585_GFDL", 
           "brick_126_IPSL", "brick_370_IPSL", "brick_585_IPSL",
           "brick_126_MPI","brick_370_MPI", "brick_585_MPI", 
           # "brick_126_MRI", "brick_370_MRI", "brick_585_MRI",
           "brick_126_UKESM1", "brick_370_UKESM1", "brick_585_UKESM1")

bricks_list<-setNames(bricks_list, names)






slopes_list <- as.list(brick_126_GFDL_slope, brick_370_GFDL_slope, brick_585_GFDL_slope, 
                       brick_126_IPSL_slope, brick_370_IPSL_slope, brick_585_IPSL_slope,
                       brick_126_MPI_slope,brick_370_MPI_slope, brick_585_MPI_slope, 
                       brick_126_MRI_slope, brick_370_MRI_slope, brick_585_MRI_slope,
                       brick_126_UKESM1_slope, brick_370_UKESM1_slope, brick_585_UKESM1_slope)


names <- c("brick_126_GFDL_slope", "brick_370_GFDL_slope", "brick_585_GFDL_slope", 
           "brick_126_IPSL_slope", "brick_370_IPSL_slope", "brick_585_IPSL_slope",
           "brick_126_MPI_slope","brick_370_MPI_slope", "brick_585_MPI_slope", 
           "brick_126_MRI_slope", "brick_370_MRI_slope", "brick_585_MRI_slope",
           "brick_126_UKESM1_slope", "brick_370_UKESM1_slope", "brick_585_UKESM1_slope")

slopes_list <- setNames(slopes_list, names)




# Temporal pixel-based regression plots

world<-readShapePoly("C:/Users/Jazel Ouled/Dropbox/UB/CORSA/TFM/SHP/shp_world/mundo_WGS84.shp")  
world<-st_as_sf(world)
plot(world)


min_max_df <-data.frame(min = numeric(), max = numeric())


for (j in 1:length(slopes_list)){
  vals <- getValues(slopes_list[[j]])  
  min_max_df[j,1] = min(vals, na.rm = T)
  min_max_df[j,2] = max(vals, na.rm = T)
  min_max_abs <- data.frame(min(min_max_df$min), max(min_max_df$max))
  colnames(min_max_abs) <- c("min", "max")
}




theme2Review<-theme_bw()+
  theme(axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.y=element_text(size=19),
        axis.title.x = element_text(size=19),
        legend.text = element_text(size=11),
        legend.title = element_text(face="bold", size=13),
        legend.key.size = unit(0.9, "cm"),
        # legend.background = element_rect(colour ="black", size=0.5, linetype = "dotted"),
        legend.direction = "vertical",
        legend.position = "none",
        plot.margin = margin(2, 2, 2, 2, "cm"), strip.background =element_rect(fill="gray59"),
        strip.text = element_blank(),
        strip.placement = "right",
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())






for (i in 1:length(slopes_list)){
  
  
  ee<-as.data.frame(slopes_list[[i]],xy=TRUE)
  
  
  ww <- ggplot()+
    geom_raster(aes(x=x,y=y, fill=layer),data=ee)+
    geom_sf(fill='grey',data=world)+
    scale_fill_viridis_c('Slope',direction = -1, option = "viridis", na.value="white", limits = c(min_max_abs$min, min_max_abs$max))+
    coord_sf(xlim = c(-90, -20), ylim = c(-80, -40), expand=c(0,0))+
    labs(x='\nLongitude',y='Latitude\n',
         # title="Salinity",
         subtitle= if (names(slopes_list)[[i]] %like% "GFDL" & names(slopes_list)[[i]] %like% "126"){
           paste0('Linear regression slope. GFDL-ESM4. SSP126 (2015-2100)', sep=" ", nc_var)
         } else if (names(slopes_list)[[i]] %like% "GFDL" & names(slopes_list)[[i]] %like% "370"){
           paste0('Linear regression slope. GFDL-ESM4. SSP370 (2015-2100)', sep=" ", nc_var)
         } else if (names(slopes_list)[[i]] %like% "GFDL" & names(slopes_list)[[i]] %like% "585"){
           paste0('Linear regression slope. GFDL-ESM4. SSP585 (2015-2100)', sep=" ", nc_var)
           
         } else if (names(slopes_list)[[i]] %like% "IPSL" & names(slopes_list)[[i]] %like% "126"){
           paste0('Linear regression slope. IPSL-CM6A-LR. SSP126 (2015-2100)', sep = " ", nc_var)
         } else if (names(slopes_list)[[i]] %like% "IPSL" & names(slopes_list)[[i]] %like% "370"){
           paste0('Linear regression slope. IPSL-CM6A-LR. SSP370 (2015-2100)', sep = " ", nc_var)
         } else if (names(slopes_list)[[i]] %like% "IPSL" & names(slopes_list)[[i]] %like% "585"){
           paste0('Linear regression slope. IPSL-CM6A-LR. SSP585 (2015-2100)', sep = " ", nc_var)
           
         } else if (names(slopes_list)[[i]] %like% "MPI" & names(slopes_list)[[i]] %like% "126"){
           paste0('Linear regression slope. MPI-ESM1-2-HR. SSP126 (2015-2100)', sep = " ", nc_var)
         } else if (names(slopes_list)[[i]] %like% "MPI" & names(slopes_list)[[i]] %like% "370"){
           paste0('Linear regression slope. MPI-ESM1-2-HR. SSP370 (2015-2100)', sep = " ", nc_var)
         } else if (names(slopes_list)[[i]] %like% "MPI" & names(slopes_list)[[i]] %like% "585"){
           paste0('Linear regression slope. MPI-ESM1-2-HR. SSP585 (2015-2100)', sep = " ", nc_var)
           
         } else if (names(slopes_list)[[i]] %like% "MRI" & names(slopes_list)[[i]] %like% "126"){
           paste0('Linear regression slope. MRI-ESM2-0. SSP126 (2015-2100)', sep = " ", nc_var)
         } else if (names(slopes_list)[[i]] %like% "MRI" & names(slopes_list)[[i]] %like% "370"){
           paste0('Linear regression slope. MRI-ESM2-0. SSP370 (2015-2100)', sep = " ", nc_var)
         } else if (names(slopes_list)[[i]] %like% "MRI" & names(slopes_list)[[i]] %like% "585"){
           paste0('Linear regression slope. MRI-ESM2-0. SSP585 (2015-2100)', sep = " ", nc_var)
           
         } else if (names(slopes_list)[[i]] %like% "UKESM1" & names(slopes_list)[[i]] %like% "126"){
           paste0('Linear regression slope. UKESM1-ESM2-0. SSP126 (2015-2100)', sep = " ", nc_var)
         } else if (names(slopes_list)[[i]] %like% "UKESM1" & names(slopes_list)[[i]] %like% "370"){
           paste0('Linear regression slope. UKESM1-ESM2-0. SSP370 (2015-2100)', sep = " ", nc_var)
         } else if (names(slopes_list)[[i]] %like% "UKESM1" & names(slopes_list)[[i]] %like% "585"){
           paste0('Linear regression slope. UKESM1-ESM2-0. SSP585 (2015-2100)', sep = " ", nc_var)
         } else if (names(slopes_list)[[i]] %like% "UKESM1" & names(slopes_list)[[i]] %like% "aa"){
           paste0('Linear regression slope. UKESM1-ESM2-0. SSP585 (2015-2100)', sep = " ", nc_var)})+ 
    theme2Review + 
    theme(legend.position = "right")
  
  plot_name <-sprintf(paste0("%s_", nc_var, ".png", sep=""), gsub("-","", names(slopes_list)[[i]]))
  ggsave(filename = plot_name, plot = ww, width=28, height=24, units="cm", dpi=100)
  
}

























