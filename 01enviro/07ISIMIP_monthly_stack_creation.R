library(raster)
library(data.table)
library(lubridate)
library(ncdf4)
prepareGrid <- function(r, m, method, name){
  rc <- raster::crop(r, m)  # crop by extent
  rs <- raster::resample(r, m, method=method)  # resample
  rm <- raster::mask(rs, m)  # mask
  names(rm) <- name
  return(rm)
}

`%notlike%` <- Negate(`%like%`)



#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------

# set number of cores
cores <- 2

# Set raster resolution and extent
res <- 1
e <- extent(-180, 180, -90, 0)

# Set period
# date_start <- as.Date("1950-01-01")
# date_end <- as.Date("2019-09-30")

# dynamic variables to extract. same names as catalog
# env_dyn_vars <- c("SST", "SSTg", "SAL", "SALg", "SSH", "EKE", "CHL", "SIC", "SIT", "MLD", "EDGE") 
env_dyn_vars <- c("SST", "THETAO", "SAL", "CHL", "SIC", "UO", "VO", "SSTg", "SALg", "EDGE", "EKE", "MLD", "DIAT")
# path to environmental static data
static_data <-"C:/Users/Jazel Ouled/Dropbox/PhD/Arctocephalus-gazella/00output/terrain"
static_data <-"~/Dropbox/PhD/Arctocephalus-gazella/00output/terrain"

# path to output
outdir <- "D:/ISIMIP_netCDF/output/monthly_stacks_ISIMIP_allVars"



# outdir <- paste0(static_data, "stack_monthly")#"data/out/environment/stack_daily"
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)



#---------------------------------------------------------------
# 1. Create oceanmask
#---------------------------------------------------------------

# create base raster
m <- raster(e, res = res, crs = crs("+proj=longlat +datum=WGS84"))
m[] <- 1


#---------------------------------------------------------------
# 2. Import environmental data
#---------------------------------------------------------------

# import static maps
bathy <- raster(paste0(static_data, "/derived_bathy_ag.nc"))  # bathymetry
bathy <- bathy+0
slope <- raster(paste0(static_data, "/derived_slope.nc"))  # slope
slope <- slope+0
sdist <- raster(paste0(static_data, "/derived_sdist.nc"))  # distance to shore
sdist <- sdist+0
d2col_dec <- raster(paste0(static_data, "/d2col_capeShirreff.nc"))  # distance to shore
d2col_dec <- d2col_dec+0
d2col_csh <- raster(paste0(static_data, "/d2col_deception.nc"))  # distance to shore
d2col_csh <- d2col_csh+0


# 
# stack_static <- stack(bathy, slope, sdist, d2col_dec, d2col_csh)
# 
# names(stack_static) <- ifelse(names(stack_static) %like% "Elevation", "BAT",
#                        ifelse(names(stack_static) %like% "slope", "SLP",
#                        ifelse(names(stack_static) %like% "layer.1", "SDIST",
#                        ifelse(names(stack_static) %like% "layer.2", "D2COL_DEC", "D2COL_CSH"))))
# plot(stack_static)
# 


#-------------------------------------------------------
# 3. Prepare static variables
#-------------------------------------------------------

# create stack with static variables
bat <- prepareGrid(bathy, m, method="bilinear", name="BAT")
slp <- prepareGrid(slope, m, method="bilinear", name="SLP")
sdist <- prepareGrid(sdist, m, method="bilinear", name="SDIST")
d2col_dec <- prepareGrid(d2col_dec, m, method="bilinear", name="D2COL_DEC")
d2col_csh <- prepareGrid(d2col_csh, m, method="bilinear", name="D2COL_CSH")

stack_static <- stack(bat, slp, sdist, d2col_dec, d2col_csh)
plot(stack_static)



# import catalogue with oceanographic products
# catalog<-read.csv("data/output/environment/catalog_antarctica_cc.csv", sep=",") # males winter
catalog<-read.csv("C:/Users/Jazel Ouled/Dropbox/PhD/Arctocephalus-gazella/00input/enviro/catalog_antarctica_cc_.csv")

catalog <- catalog[catalog$product %like% "IPSL" & catalog$service == "histor",]

# Create dates
# dates <- seq.Date(date_start, date_end, by="month")

ISIMIP_montly_files_gr <- dir("D:/ISIMIP_netCDF/output/monthly_rasters_ISIMIP_tif_allVars", recursive = T, full.names = T, pattern = ".tif" )

ISIMIP_montly_files_gr_histor <- dir("D:/ISIMIP_netCDF/output/monthly_rasters_ISIMIP_tif_allVars/GFDL-ESM4/histor", recursive = T, full.names = T, pattern = ".tif" )

  datestring_histor <- unique(substring(basename(ISIMIP_montly_files_gr_histor), 1, 8))
  dates_histor <- ymd(datestring_histor)
  dates_histor <- unique(round_date(dates_histor, "month"))
  datestring_histor <- gsub("-", "", dates_histor)


ISIMIP_montly_files_gr_ssp <- dir("D:/ISIMIP_netCDF/output/monthly_rasters_ISIMIP_tif_allVars/GFDL-ESM4/ssp126", recursive = T, full.names = T, pattern = ".tif" )
  
  datestring_ssp <- unique(substring(basename(ISIMIP_montly_files_gr_ssp), 1, 8))
  dates_ssp <- ymd(datestring_ssp)
  dates_ssp <- unique(round_date(dates_ssp, "month"))
  datestring_ssp <- gsub("-", "", dates_ssp)
  
  
  
  
  
  

for (i in 1:length(unique(catalog$product))){
    print(i)
     model <- unique(catalog$product)[i]
     if (model == "MRI-ESM2-0") next

  for (j in seq_along(unique(catalog$service))){
    print(j)
    ssp <- catalog$service[j]
    
    
 if(ssp == "histor"){
    dates <- dates_histor
    datestring <- datestring_histor
   
   
 }   else if (ssp %like% "ssp") {
   dates <- dates_ssp
   datestring <- datestring_ssp
 }
    

    for (k in seq_along(datestring)){
      print(k)
      ISIMIP_montly_files_gr_ <- ISIMIP_montly_files_gr[which(ISIMIP_montly_files_gr %like% datestring[k] 
                                                          & ISIMIP_montly_files_gr %like% model 
                                                         & ISIMIP_montly_files_gr %like% ssp)]
   stack_dynamic <- stack()
     for(q in 1:length(ISIMIP_montly_files_gr_)){
       ISIMIP_montly_files_gr_r<- raster(ISIMIP_montly_files_gr_[q])
       ISIMIP_montly_files_gr_r<- setExtent(ISIMIP_montly_files_gr_r, e)
      stack_dynamic <- stack(stack_dynamic , ISIMIP_montly_files_gr_r)
    }
    

   stack_static_pr <- projectRaster(stack_static, stack_dynamic, method="ngb")
    # combine with static stack
    s <- stack(stack_static_pr, stack_dynamic)

    
      names(s) <- ifelse(names(s) %like% "BAT", "BAT",
                  ifelse(names(s) %like% "SLP", "SLP",
                  ifelse(names(s) %like% "SDIST", "SDIST",
                  ifelse(names(s) %like% "D2COL_DEC", "D2COL_DEC",
                  ifelse(names(s) %like% "D2COL_CSH", "D2COL_CSH",
                  ifelse(names(s) %like% "chl", "CHL",
                  ifelse(names(s) %like% "edgedist", "EDGE",
                  ifelse(names(s) %like% "eke", "EKE",
                  ifelse(names(s) %like% "mlotstmax", "MLD",
                  ifelse(names(s) %like% "o2.surf", "O2",
                  ifelse(names(s) %like% "phydiat", "DIAT",
                  ifelse(names(s) %like% "siconc", "SIC",
                  ifelse(names(s) %like% "so.surf_gr", "SALg",
                  ifelse(names(s) %like% "so.surf", "SAL",
                  ifelse(names(s) %like% "thetao", "THETAO",
                  ifelse(names(s) %like% "tos_gr", "SSTg",
                  ifelse(names(s) %like% "tos", "SST",
                  ifelse(names(s) %like% "uo", "UO", 
                  ifelse(names(s) %like% "vo", "VO", "")))))))))))))))))))


                                                                                                             

    # set/create folder
    YYYY <- year(dates[k])
    # product_folder <- paste(outdir, model, ssp, YYYY, sep ="/")  # Set folder
    product_folder <- paste0(outdir, sep="/", model,  sep="/", ssp, sep="/", YYYY)
    if (!dir.exists(product_folder)) dir.create(product_folder, recursive = TRUE)  # create output directory if does not exist
    
    # store file in GRD format
    outfile <- paste0(product_folder, sep ="/", datestring[k], "_", model, "_", ssp, "_monthly_ISIMIP_stack.grd")
    writeRaster(s, outfile, bandorder='BIL', overwrite=TRUE)

    }}}
  
  
  nc<-nc_open("D:/ISIMIP_netCDF/models/IPSL-CM6A-LR/ssp126/ipsl-cm6a-lr_r1i1p1f1_ssp126_chl_onedeg_global_monthly_2015_2100.nc")
  
  
  
  var <-ncvar_get(nc, varid="chl", start=c(1,1,1,1), count=c(-1,-1, 1, -1))
  
  
  plot(var)
  lat<-ncvar_get(nc, varid="lat")
  lon<-ncvar_get(nc, varid="lon")
  time<-ncvar_get(nc, varid="time")
  time<-floor(time)
  
  ee<-raster(var[,,1])
  
  
  plot(ee)
  
  var <-ncvar_get(nc, varid=nc_var)
  
  
  
  nc<-nc_open("D:/ISIMIP_netCDF/models/GFDL-ESM4/ssp126/gfdl-esm4_r1i1p1f1_ssp126_chl_onedeg_global_monthly_2015_2100.nc")

    tt <-nc_open("D:/ISIMIP_netCDF/models/MPI-ESM1-2-HR/ssp126/mpi-esm1-2-hr_r1i1p1f1_ssp126_chl_onedeg_global_monthly_2015_2100.nc")
   
     tt <-nc_open("D:/ISIMIP_netCDF/models/IPSL-CM6A-LR/histor/ipsl-cm6a-lr_r1i1p1f1_historical_phydiat-vint_onedeg_global_monthly_1850_2014.nc")
    
     ncvar_get(ww)
     raster(ww)
