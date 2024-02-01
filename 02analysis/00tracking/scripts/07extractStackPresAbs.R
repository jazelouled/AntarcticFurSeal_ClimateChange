# Script hard-coded by Dr. David March (https://zenodo.org/badge/latestdoi/235335964). Adapted by Jazel Ouled-Cheikh. 

#-------------------------------------------------------------------------------------
# extractStackPresAbs.R        Extract environment data from presence absence data
#-------------------------------------------------------------------------------------
# This script extracts environmental data



#---------------------------------------------------------------
# Prepare cluster
#---------------------------------------------------------------
cl <- makeCluster(cores)
registerDoParallel(cl)



#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------
# Set data paths
indir <- here::here("InputOutput/00output/tracking/L3_locations_PresAbs/")
outdir <- here::here("InputOutput/00output/tracking/L4_locations_extract/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)



#---------------------------------------------------------------
# 3. Import presence/absence data
#---------------------------------------------------------------
# import observations
obs_file <- paste0(indir, sp_code, "_L3_PresAbs.csv")
data <- read.csv(obs_file)
data$dates <- ymd(data$date)



#---------------------------------------------------------------
# 4. Extract habitat
#---------------------------------------------------------------

# filter out data after max date
# This is limited to the collected environmental repository
data <- filter(data, date < env_max_date)

# extract environmental data from stack
enviro <- rbindlist(foreach(j=1:nrow(data), .packages = c("lubridate", "raster", "stringr"))  %dopar% {
  extract_stack(date = data$date[j], lon = data$lon[j], lat = data$lat[j], buffer = env_buffer, repo = stack_repo)
})

# combine
data <- cbind(data, enviro)

# select
data <- dplyr::select(data, sp_code, id, date, lon, lat, occ, all_of(all_vars))

# remove grid cells with > 1 variable with missing data
data$na_count <- rowSums(is.na(data))
data <- data[data$na_count <= 1,]

# export
outfile <- paste0(outdir, "/", sp_code, "_observations.csv")
write.csv(data, outfile, row.names = FALSE)



#---------------------------------------------------------------
# Stop cluster
#---------------------------------------------------------------
stopCluster(cl)

print("Environmental data extracted ready") 