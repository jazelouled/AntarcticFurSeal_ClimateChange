# Script hard-coded by Dr. David March (https://zenodo.org/badge/latestdoi/235335964). Adapted by Jazel Ouled-Cheikh. 

#------------------------------------------------------------------------------------
# oceanmask.R           Create ocean mask to create simulations
#------------------------------------------------------------------------------------
# Description:
# Simulations of animal tracks requires setting a map layer to constrain their movement.
# We consider three main constrains of the movement of marine animals:
# 1.- A land mask, to avoid moving into land.
# 2.- A study area, in this case defined by the minimum convex polygon of all tracks.

# Returns:
# 1) A minimum convex polygon (shapefile format)
# 2) A raster map (resistance layer) with the following characteristics:
# - CRS is epsg:3031 (simulations seem to perform better using a projected CRS)
# - Resolution is 5 km
# - values are: 0 = ocean, 1 = land



#--------------------------------
# Import data
#--------------------------------
# Import bathymetry
bathy_nc <- here::here("InputOutput/00output/terrain/derived_bathy.nc")
bathy <- raster(bathy_nc)

# Import L2 product of simulations (i.e. simulations with environmental data)
indir <- here::here("InputOutput/00output/01tracking/L2_locations/")
loc_files <- list.files(indir, full.names = TRUE, pattern = "L2_locations.csv")
ssm <- readTrack(loc_files)
ssm$sex <- ifelse(ssm$id %like% "F", "females", "males")
ssm$season <- ifelse(ssm$id %like% "W", "winter", "summer")


#--------------------------------
# Minimum convex polygon
#--------------------------------
# All tracks --------------------------------------------------------------
ssm <- dplyr::select(ssm, lon, lat)
bbox <-ssm %>% 
  st_as_sf(coords = c("lon","lat"), crs = 4326) %>% 
  st_bbox()

# Females winter ----------------------------------------------------------
ssm_femalesW <- subset(ssm, ssm$sex == "females" & ssm$season == "winter")
coordinates(ssm_femalesW) <- ~ lon + lat
proj4string(ssm_femalesW) <- "+proj=longlat +ellps=WGS84"
cp_femalesW <- mcp(ssm_femalesW, percent = 100) # MCP use all the positions
cp.buf_femalesW <- gBuffer(cp_femalesW, width = mcp_expand) # Extend the MCP with a buffer of 5 degrees

# Females summer ----------------------------------------------------------
ssm_femalesS <- subset(ssm, ssm$sex == "females" & ssm$season == "summer")
coordinates(ssm_femalesS) <- ~ lon + lat
proj4string(ssm_femalesS) <- "+proj=longlat +ellps=WGS84"
cp_femalesS <- mcp(ssm_femalesS, percent = 100) # MCP use all the positions
cp.buf_femalesS <- gBuffer(cp_femalesS, width = mcp_expand) # Extend the MCP with a buffer of 5 degrees
plot(cp.buf_femalesS)

# Males winter ------------------------------------------------------------
ssm_malesW <- subset(ssm, ssm$sex == "males" & ssm$season == "winter")
coordinates(ssm_malesW) <- ~ lon + lat
proj4string(ssm_malesW) <- "+proj=longlat +ellps=WGS84"
cp_malesW <- mcp(ssm_malesW, percent = 100) # MCP use all the positions
cp.buf_malesW <- gBuffer(cp_malesW, width = mcp_expand) # Extend the MCP with a buffer of 5 degrees
plot(cp.buf_malesW) 

# Export MCP as shapefile
# cp.df <- data.frame(ID=1:length(cp.buf)) 
# row.names(cp.df) <- "buffer"
# p <- SpatialPolygonsDataFrame(cp.buf, cp.df) 
# writeOGR(p, paste(input_data, "mcp.gpkg", sep="/"), "mcp", driver="GPKG", overwrite_layer=TRUE)



#--------------------------------
# Combine MCP and landmask
#--------------------------------
# Reclassify bathymetry to create ocean mask
# Define resistance values (0 = ocean, 1 = land)
bathy[!is.na(bathy)] <- 0
bathy[is.na(bathy)] <- 1

# Mask with MCP extent
# oceanmask <- mask(bathy, cp.buf)
oceanmask <- bathy

# downsize
oceanmask <- aggregate(oceanmask, fact = 5, fun = median)

# Export resistance
outdir <- here::here("InputOutput/00output/00environment/terrain/")
writeRaster(oceanmask, paste0(outdir, "oceanmask.nc"), format="CDF", overwrite=TRUE)