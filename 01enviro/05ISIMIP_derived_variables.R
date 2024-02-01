#-------------------------------------------
# derived_gradients.R
#-------------------------------------------
# Date: 2017/11/23
# Author: David March
# Project: Tortugas oceanografas
#
# Description:
# Generate repo for gradients of SST


# Load libraries
library(raster)
library(foreach)  
library(doParallel)
library(lubridate)

#-----------------------------------------------
# Import data catalog
#-----------------------------------------------

# set repository for CMEMS products

cmems_repo <- paste0(input_data, "/cmems")

catalog<-read.csv("data/output/environment/catalog_antarctica_cc.csv", sep=";")
# Create dates



# set number of cores
cores <- 10

# Set raster resolution and extent
res <- 0.1
e <- extent(-90, -20, -80, -50)

# Set period
date_start <- as.Date("2019-03-01")
date_end <- as.Date("2019-09-30")

# dynamic variables to extract. same names as catalog
# env_dyn_vars <- c("SST", "SSTg", "SAL", "SALg", "SSH", "EKE", "CHL", "SIC", "SIT", "MLD", "EDGE") 
env_dyn_vars <- c("SST", "SAL", "CHL")
# path to environmental static data
static_data <- paste0(output_data, "/terrain/")

# path to output
outdir <- paste0(output_data, "/stack_monthly/")#"data/out/environment/stack_daily"
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)



#---------------------------------------------------------------
# 1. Create oceanmask
#---------------------------------------------------------------

# create base raster
m <- raster(e, res = res, crs = crs("+proj=longlat +datum=WGS84"))
m[] <- 1




dates <- seq.Date(date_start, date_end, by="month")


ISIMIP_montly_files <- dir("D:/ISIMIP_netCDF/output/monthly_rasters_ISIMIP_tif", recursive = T, full.names = T )




# import product catalog
catalog <- read.csv("cmems/agazella_catalog.csv")  # list with updated products for 2019

# set data formats
catalog$service <- as.character(catalog$service)
catalog$product <- as.character(catalog$product)
catalog$variable <- as.character(catalog$variable)
catalog$standard_name <- as.character(catalog$standard_name)
catalog$date_min <- dmy(catalog$date_min)
catalog$date_max <- dmy(catalog$date_max)

#-----------------------------------------------
# Set initial parameters: gradient
#-----------------------------------------------

input_product_id <- 2
output_product_id <- 11


#-----------------------------------------------
# Retrieve information
#-----------------------------------------------

# input data
input_service <- catalog$service[input_product_id]
input_product <- catalog$product[input_product_id]
input_var <- catalog$variable[input_product_id]

# output data
output_service <- catalog$service[output_product_id]
output_product <- catalog$product[output_product_id]
output_var <- catalog$variable[output_product_id]
output_name <- catalog$standard_name[output_product_id]

# set dates
start_date <- catalog$date_min[input_product_id]
end_date <- catalog$date_max[input_product_id]

# generate daily sequence
dayseq <- seq(as.Date(start_date), as.Date(end_date), by = "day")

