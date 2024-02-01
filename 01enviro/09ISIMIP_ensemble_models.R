
library(data.table)
library(raster)
library(lubridate)

#

# Mean SSP and models for fitting (2006-2019) -----------------------------------------


ISIMIP_files_ <- dir("D:/ISIMIP_netCDF/output/monthly_stacks_ISIMIP_allVars", recursive = T, full.names = T )
outdir <- "D:/ISIMIP_netCDF/output/monthly_stacks_meanSSP_predict/"
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)  # create output directory if does not exist


ISIMIP_files_fit <- subset(ISIMIP_files_, substr(basename(ISIMIP_files_), 1,8) >= 20060101 & substr(basename(ISIMIP_files_), 1,8) <= 20191231)


models <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "UKESM1-ESM2-0")




for(q in 1:length(models)){
  print(q)
  
  model <- models[q]
  ISIMIP_files_fit_ <- ISIMIP_files_fit[ISIMIP_files_fit %like% "ssp" & ISIMIP_files_fit %like% model & ISIMIP_files_fit %like% ".grd"]
  datestring <- substr(basename(ISIMIP_files_fit_), 1,8)
  
  
  
for(t in 1:length(unique(substr(datestring, 1, 4)))){
  print(t)
  ISIMIP_files_fit_year <- ISIMIP_files_fit_[year(ymd(substr(basename(ISIMIP_files_fit_), 1,8))) == unique(substr(datestring, 1, 4))[t]]
  
  
  
  for(r in 1:length(unique(substr(datestring, 5, 8)))) {
  
  ISIMIP_files_fit_month <- ISIMIP_files_fit_year[ISIMIP_files_fit_year %like% unique(substr(datestring, 5, 8))[r]]
    

  aa <- stack(ISIMIP_files_fit_month)

  # GFDL
      if (model %like% "GFDL"){
        
        vars<-c("BAT", "SLP", "SDIST", "D2COL_DEC", "D2COL_CSH","CHL", "EDGE", "EKE","O2",
                "DIAT","SIC", "SAL","SALg","THETAO", "SST", "SSTg",  "UO", "VO")
 
  # IPSL
      } else if (model %like% "IPSL"){ 
        
        vars<-c("BAT", "SLP", "SDIST", "D2COL_DEC", "D2COL_CSH","CHL", "EDGE", "EKE", "MLD","O2",
                "DIAT","SIC", "SAL","SALg","THETAO", "SST", "SSTg",  "UO", "VO")
        
  # MPI
      } else if (model %like% "MPI"){ 
        vars<-c("BAT", "SLP", "SDIST", "D2COL_DEC", "D2COL_CSH","CHL", "EDGE", "EKE", "MLD","O2",
                "SIC", "SAL","SALg","THETAO", "SST", "SSTg",  "UO", "VO")
        
      } else if (model %like% "UKESM"){ 
        
  #UKESM
        
        vars<-c("BAT", "SLP", "SDIST", "D2COL_DEC", "D2COL_CSH","CHL", "EDGE", "EKE", "MLD","O2",
                "SIC", "SAL","SALg","THETAO", "SST", "SSTg",  "UO", "VO")
      }
  

  
  bb <- stackApply(aa, indices = vars, fun=mean, na.rm =T)
  
  # set/create folder
  YYYY <- unique(substr(datestring, 1, 4))[t]
  # product_folder <- paste(outdir, model, ssp, YYYY, sep ="/")  # Set folder
  product_folder <- paste0(outdir, model,  "/", YYYY, sep = "/")
  if (!dir.exists(product_folder)) dir.create(product_folder, recursive = TRUE)  # create output directory if does not exist
  
  # store file in GRD format
  outfile <- paste0(product_folder, YYYY, unique(substr(datestring, 5, 8))[r], "_",  model, "_monthly_ISIMIP_meanSSP_stack.grd")
  writeRaster(bb, outfile, bandorder='BIL', overwrite=TRUE)
  
  
  } } }
  
  

# 1. tornar a córrer ensemble stack
# 2. tornar a córrer extract
# 3. tornar a córrer models (mw, fs, fw)
# 4. treballar loop prediccions

# Average models monthly (ensemble) --------------------------------------------------
# script failed at 1/6/2006, 1/10/2010, 1/11/2011
ISIMIP_files_fit <- dir("D:/ISIMIP_netCDF/output/monthly_stacks_meanSSP", recursive = T, full.names = T )

outdir <- "D:/ISIMIP_netCDF/output/monthly_stacks_BRT/"
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)  # create output directory if does not exist



# I chose to mask by IPSL landmask
cc <- ISIMIP_files_fit

maskIPSL <- cc[cc %like% "IPSL"]
maskIPSL<-stack(maskIPSL[1])
maskIPSL <- maskIPSL$CHL
maskIPSL <- maskIPSL/maskIPSL
plot(maskIPSL)


ISIMIP_files_fit <- subset(ISIMIP_files_fit, substr(basename(ISIMIP_files_fit), 1,8) >= 20060101 & substr(basename(ISIMIP_files_fit), 1,8) <= 20191231)
# models <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "UKESM1-ESM2-0")

datestring <- substr(basename(ISIMIP_files_fit), 1,8)

ISIMIP_files_fit_ <- ISIMIP_files_fit[ISIMIP_files_fit %like% ".grd"]

  for(t in 1:length(unique(substr(datestring, 1, 4)))){
    print(t)
    ISIMIP_files_fit_year <- ISIMIP_files_fit_[year(ymd(substr(basename(ISIMIP_files_fit_), 1,8))) == unique(substr(datestring, 1, 4))[t]]
    
    for(r in 1:length(unique(substr(datestring, 5, 8)))) {
      
      ISIMIP_files_fit_month <- ISIMIP_files_fit_year[substr(basename(ISIMIP_files_fit_year), 5, 8) %like% unique(substr(datestring, 5, 8))[r]]

      aa <- stack(ISIMIP_files_fit_month)
      
      # With DIAT in UKESM. [1] is just to take one of the strings. All of them should contain histor or ssp. 
      if (ISIMIP_files_fit_month[1] %like% "histor"){
      
      
      vars<-c("BAT", "SLP", "SDIST", "D2COL_DEC", "D2COL_CSH","CHL", "EDGE", "EKE","O2",
              "DIAT","SIC", "SAL","SALg","THETAO", "SST", "SSTg",  "UO", "VO", "BAT", "SLP", 
              "SDIST", "D2COL_DEC", "D2COL_CSH","CHL", "EDGE", "EKE", "MLD","O2","DIAT","SIC", 
              "SAL","SALg","THETAO", "SST", "SSTg",  "UO", "VO", "BAT", "SLP", "SDIST", 
              "D2COL_DEC", "D2COL_CSH","CHL", "EDGE", "EKE", "MLD","O2",
              "SIC", "SAL","SALg","THETAO", "SST", "SSTg",  "UO", "VO", "BAT", "SLP", "SDIST", "D2COL_DEC", 
              "D2COL_CSH","CHL", "EDGE", "EKE", "MLD","O2", "DIAT",
              "SIC", "SAL","SALg","THETAO", "SST", "SSTg",  "UO", "VO")
      
      # Without DIAT in UKESM
      } else if (ISIMIP_files_fit_month[1] %like% "SSP"){ 
        
        vars<-c("BAT", "SLP", "SDIST", "D2COL_DEC", "D2COL_CSH","CHL", "EDGE", "EKE","O2",
                "DIAT","SIC", "SAL","SALg","THETAO", "SST", "SSTg",  "UO", "VO", "BAT", "SLP", 
                "SDIST", "D2COL_DEC", "D2COL_CSH","CHL", "EDGE", "EKE", "MLD","O2","DIAT","SIC", 
                "SAL","SALg","THETAO", "SST", "SSTg",  "UO", "VO", "BAT", "SLP", "SDIST", 
                "D2COL_DEC", "D2COL_CSH","CHL", "EDGE", "EKE", "MLD","O2",
                "SIC", "SAL","SALg","THETAO", "SST", "SSTg",  "UO", "VO", "BAT", "SLP", "SDIST", "D2COL_DEC", 
                "D2COL_CSH","CHL", "EDGE", "EKE", "MLD","O2", 
                "SIC", "SAL","SALg","THETAO", "SST", "SSTg",  "UO", "VO")
        
      }
        
   
      # aa_ <- raster::mask(aa, maskIPSL)
      
      bb <- stackApply(aa, indices = vars, fun=mean, na.rm =T)
      
      bb_ <- raster::mask(bb, maskIPSL)
      
      # set/create folder
      YYYY <- unique(substr(datestring, 1, 4))[t]
      # product_folder <- paste(outdir, model, ssp, YYYY, sep ="/")  # Set folder
      product_folder <- paste0(outdir, YYYY, sep = "/")
      if (!dir.exists(product_folder)) dir.create(product_folder, recursive = TRUE)  # create output directory if does not exist
      
      # store file in GRD format
      outfile <- paste0(product_folder, YYYY, unique(substr(datestring, 5, 8))[r], "_monthly_ESMensemble_stack_ISIMIP.grd")
      writeRaster(bb_, outfile, bandorder='BIL', overwrite=TRUE)
      
      
    } } 






# create layers for predict (ensemble of each scenario for all models. e.g. averaged 126 for gfdl, ipsl, mpi and ukesm)



ISIMIP_files_fit <- dir("D:/ISIMIP_netCDF/output/monthly_stacks_ISIMIP_allVars", recursive = T, full.names = T )


outdir <- "D:/ISIMIP_netCDF/output/monthly_stacks_predict/"
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)  # create output directory if does not exist

datestring <- substr(basename(ISIMIP_files_fit), 1,8)

datestring_hist <- datestring[datestring <= 20141201]

datestring_ssp <- datestring[datestring > 20141201]

ssp <- c("histor", "ssp126", "ssp370", "ssp585")

ISIMIP_files_fit_ <- ISIMIP_files_fit[ISIMIP_files_fit %like% ".grd"]

for(t in 1:length(ssp)){
  print(t)
  ISIMIP_files_fit_ssp <- ISIMIP_files_fit_[ISIMIP_files_fit_ %like% ssp[t]]
  
  ssp_ <- ssp[t]
  
  if (ssp_ == "histor"){
    datestring <- datestring_hist
    
  } else if (ssp_ %like% "ssp") {
    datestring <- datestring_ssp
    
  }
  
for(h in 1:length(unique(substr(datestring, 1, 4)))){
  print(h)
  ISIMIP_files_fit_year <- ISIMIP_files_fit_ssp[year(ymd(substr(basename(ISIMIP_files_fit_ssp), 1,8))) == unique(substr(datestring, 1, 4))[h]]
  
  for(r in 1:length(unique(substr(datestring, 5, 8)))) {
    
    ISIMIP_files_fit_month <- ISIMIP_files_fit_year[substr(basename(ISIMIP_files_fit_year), 5, 8) %like% unique(substr(datestring, 5, 8))[r]]
    
    aa <- stack(ISIMIP_files_fit_month)
    
    # With DIAT in UKESM. [1] is just to take one of the strings. All of them should contain histor or ssp. 
    if (ISIMIP_files_fit_month[1] %like% "histor"){
      
      
      vars<-c("BAT", "SLP", "SDIST", "D2COL_DEC", "D2COL_CSH","CHL", "EDGE", "EKE","O2",
              "DIAT","SIC", "SAL","SALg","THETAO", "SST", "SSTg",  "UO", "VO", "BAT", "SLP", 
              "SDIST", "D2COL_DEC", "D2COL_CSH","CHL", "EDGE", "EKE", "MLD","O2","DIAT","SIC", 
              "SAL","SALg","THETAO", "SST", "SSTg",  "UO", "VO", "BAT", "SLP", "SDIST", 
              "D2COL_DEC", "D2COL_CSH","CHL", "EDGE", "EKE", "MLD","O2",
              "SIC", "SAL","SALg","THETAO", "SST", "SSTg",  "UO", "VO", "BAT", "SLP", "SDIST", "D2COL_DEC", 
              "D2COL_CSH","CHL", "EDGE", "EKE", "MLD","O2", "DIAT",
              "SIC", "SAL","SALg","THETAO", "SST", "SSTg",  "UO", "VO")
      
      # Without DIAT in UKESM
    } else if (ISIMIP_files_fit_month[1] %like% "ssp"){ 
      
      vars<-c("BAT", "SLP", "SDIST", "D2COL_DEC", "D2COL_CSH","CHL", "EDGE", "EKE","O2",
              "DIAT","SIC", "SAL","SALg","THETAO", "SST", "SSTg",  "UO", "VO", "BAT", "SLP", 
              "SDIST", "D2COL_DEC", "D2COL_CSH","CHL", "EDGE", "EKE", "MLD","O2","DIAT","SIC", 
              "SAL","SALg","THETAO", "SST", "SSTg",  "UO", "VO", "BAT", "SLP", "SDIST", 
              "D2COL_DEC", "D2COL_CSH","CHL", "EDGE", "EKE", "MLD","O2",
              "SIC", "SAL","SALg","THETAO", "SST", "SSTg",  "UO", "VO", "BAT", "SLP", "SDIST", "D2COL_DEC", 
              "D2COL_CSH","CHL", "EDGE", "EKE", "MLD","O2", 
              "SIC", "SAL","SALg","THETAO", "SST", "SSTg",  "UO", "VO")
      
    }
    
    
    bb <- stackApply(aa, indices = vars, fun=mean, na.rm =T)
    
    bb_ <- raster::mask(bb, maskIPSL)
    
    # set/create folder
    YYYY <- unique(substr(datestring, 1, 4))[h]
    # product_folder <- paste(outdir, model, ssp, YYYY, sep ="/")  # Set folder
    product_folder <- paste0(outdir, ssp_, "/",  YYYY, sep = "/")
    if (!dir.exists(product_folder)) dir.create(product_folder, recursive = TRUE)  # create output directory if does not exist
    
    
    # store file in GRD format
    outfile <- paste0(product_folder, YYYY, unique(substr(datestring, 5, 8))[r], "_monthly_ESMensemble_stack_predict_", ssp_, "_ISIMIP.grd")
    writeRaster(bb_, outfile, bandorder='BIL', overwrite=TRUE)
    
    
  } } }





ww <- raster::stack("~/Dropbox/2023_JSDM_Fran/SouthernHemisphere_meanStacks_ISIMIP/1950_2014_histor_ISIMIP.grd")

plot(ww$index_SST, zlim = c(-2, 33))


www <- raster::stack("~/Dropbox/2023_JSDM_Fran/SouthernHemisphere_meanStacks_ISIMIP/2015_2050_ssp126_ISIMIP.grd")
plot(www$index_SST, zlim = c(-2, 33))


  
wwww <- raster::stack("~/Dropbox/2023_JSDM_Fran/SouthernHemisphere_meanStacks_ISIMIP/2051_2100_ssp585_ISIMIP.grd")
plot(wwww$index_SST, zlim = c(-2, 33))

  
extreme

vv <- wwww$index_SST-ww$index_SST
plot(vv)

max(wwww$index_SST)



minValue(wwww$index_SST)
maxValue(wwww$index_SST)


minValue(www$index_SST)
maxValue(www$index_SST)

minValue(ww$index_SST)
maxValue(ww$index_SST)




cc <- ISIMIP_files_

maskGFDL <- cc[cc %like% "GFDL"]
maskGFDL<-stack(maskGFDL[1])
maskGFDL <- maskGFDL$CHL
maskGFDL <- maskGFDL/maskGFDL
plot(maskGFDL)

maskIPSL <- cc[cc %like% "IPSL"]
maskIPSL<-stack(maskIPSL[1])
maskIPSL <- maskIPSL$CHL
maskIPSL <- maskIPSL/maskIPSL
plot(maskIPSL)




maskMPI <- cc[cc %like% "MPI"]
maskMPI<-stack(maskMPI[1])
maskMPI <- maskMPI$CHL
maskMPI <- maskMPI/maskMPI
plot(maskMPI)


maskUKESM <- cc[cc %like% "UKESM"]
maskUKESM<-stack(maskUKESM[1])
maskUKESM <- maskUKESM$CHL
maskUKESM <- maskUKESM/maskUKESM
plot(maskUKESM)


ssp <- c("hisor", "ssp126", "ssp370", "ssp585")

vars<-c("BAT", "SLP", "SDIST", "D2COL", "CHL", "EDGE", "EKE", "O2", 
        "SIC", "SAL","SALg","THETAO", "SST", "SSTg", "UO", "VO")


ISIMIP_files <- dir("D:/ISIMIP_netCDF/output/monthly_stacks_ISIMIP_allVars", recursive = T, full.names = T )
outdir <- "D:/ISIMIP_netCDF/output/monthly_stacks_ensemble_ISIMIP/"
datestring <- substr(basename(ISIMIP_files), 1,8)
dates<-ymd(datestring)

for (i in 1:length(ssp)) {
  ssp_ <- ssp[i]

  for (j in 1:length(datestring)) {
    ISIMIP_files_ <- ISIMIP_files[ISIMIP_files %like% datestring[j] & ISIMIP_files %like% ssp_ & ISIMIP_files %like% ".grd"]
    
    ISIMIP_files_stack <- stack(ISIMIP_files_)
    ISIMIP_files_stack_msk <- mask(ISIMIP_files_stack, maskIPSL)
    mean_stack <- stackApply(ISIMIP_files_stack_msk, indices=vars, fun=mean, na.rm =T)
    
    names(mean_stack) <- ifelse(names(mean_stack) %like% "BAT", "BAT",
                ifelse(names(mean_stack) %like% "SLP", "SLP",
                ifelse(names(mean_stack) %like% "SDIST", "SDIST",
                ifelse(names(mean_stack) %like% "D2COL", "D2COL",
                ifelse(names(mean_stack) %like% "CHL", "CHL",
                ifelse(names(mean_stack) %like% "EDGE", "EDGE",
                ifelse(names(mean_stack) %like% "EKE", "EKE",
                ifelse(names(mean_stack) %like% "O2", "O2",
                ifelse(names(mean_stack) %like% "SIC", "SIC",
                ifelse(names(mean_stack) %like% "SALg", "SALg",
                ifelse(names(mean_stack) %like% "SAL", "SAL",
                ifelse(names(mean_stack) %like% "THETAO", "THETAO",
                ifelse(names(mean_stack) %like% "SSTg", "SSTg",
                ifelse(names(mean_stack) %like% "SST", "SST",
                ifelse(names(mean_stack) %like% "MLD", "MLD",
                ifelse(names(mean_stack) %like% "UO", "UO", 
                ifelse(names(mean_stack) %like% "VO", "VO", "")))))))))))))))))

    # set/create folder
    YYYY <- year(dates[j])
    # product_folder <- paste(outdir, model, ssp, YYYY, sep ="/")  # Set folder
    product_folder <- paste0(outdir, ssp_, sep="/", YYYY)
    if (!dir.exists(product_folder)) dir.create(product_folder, recursive = TRUE)  # create output directory if does not exist
    
    # store file in GRD format
    outfile <- paste0(product_folder, sep ="/", datestring[j], "_", ssp_, "_monthly_CCensemble_stack.grd")
    writeRaster(mean_stack, outfile, bandorder='BIL', overwrite=TRUE)
    
  }
}



