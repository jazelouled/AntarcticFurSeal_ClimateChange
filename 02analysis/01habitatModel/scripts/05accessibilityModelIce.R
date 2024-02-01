# Script hard-coded by Dr. David March (https://zenodo.org/badge/latestdoi/235335964). Adapted by Jazel Ouled-Cheikh. 

#---------------------------------------------------------------------
# accessibility
#---------------------------------------------------------------------
sp_code <- "GAZ"  
sexseason <- "MW"
# sexseason <- "FW"
# sexseason <- "FS"


# Define number of bootstrap models
n.boot <- 50  # number of model fits



#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------
# input data paths
indir <- here::here("InputOutput/00output", sexseason)
stack_repo <- "D:/ISIMIP_netCDF/output/monthly_stacks_BRT"
stack_repo <- "/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_BRT"


#-----------------------------------------------------------------
# Prepare data
#-----------------------------------------------------------------
obs_file <-list.files(here::here("InputOutput/00output/01tracking/L4_locations_extract/") , recursive = T, full.names = T)
# obs_file <-list.files(here::here("InputOutput/00output/01tracking/L4_locations_extract_FScpf") , recursive = T, full.names = T)
outdir <- here::here("InputOutput/00output/02habitatModel", sexseason, "BoostedRegressionTrees/full_model/accessibilityModel_SeaIce")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

set.seed(131)
# import observaions
# obs_file <- paste0(indir,"/", sp_code, "_data.csv")
data <- read.csv(obs_file)

namesCols <- names(data[,1:8])

names(data) <-  ifelse(names(data) %like% "BAT", "BAT",
                ifelse(names(data) %like% "SLP", "SLP",
                ifelse(names(data) %like% "SDIST", "SDIST",
                ifelse(names(data) %like% "D2COL_DEC", "D2COL_DEC",
                ifelse(names(data) %like% "D2COL_CSH", "D2COL_CSH",
                ifelse(names(data) %like% "CHL", "CHL",
                ifelse(names(data) %like% "EDGE", "EDGE",
                ifelse(names(data) %like% "EKE", "EKE",
                ifelse(names(data) %like% "MLD", "MLD",
                ifelse(names(data) %like% "O2", "O2",
                ifelse(names(data) %like% "DIAT", "DIAT",
                ifelse(names(data) %like% "SIC", "SIC",
                ifelse(names(data) %like% "SALg", "SALg",
                ifelse(names(data) %like% "SAL", "SAL",
                ifelse(names(data) %like% "THETAO", "THETAO",
                ifelse(names(data) %like% "SSTg", "SSTg",
                ifelse(names(data) %like% "SST", "SST",
                ifelse(names(data) %like% "UO", "UO",
                ifelse(names(data) %like% "VO", "VO", "")))))))))))))))))))

colnames(data)[1:8] <- namesCols

data <- data[data$id %like% sexseason,]

# Presences
presences <- filter(data, occ==1)
n_occ <- nrow(presences)

# Absences
absences <- filter(data, occ==0)
abs_prop <- nrow(presences)/nrow(absences)
absences <- stratified(absences, c("id"), abs_prop)

# Combine presence-absences
data <- bind_rows(presences, absences)

# Check number of occurrence per type
table(data$occ)

data$date <- ymd(data$date)

# Create dates
dates <- seq.Date(min(data$date), max(data$date), by="day")  # define sequence



#-----------------------------------------------------------------
# Get presence/absence data and link to distance to ice edge
#-----------------------------------------------------------------
dist_list <- list()

for(i in 1:length(dates)){

        print(paste("Processing date", i, "from", length(dates)))
        
      # Get time information
        date <- dates[i]
        YYYY <- year(date)
        MM <- sprintf("%02d", month(date))
        
      # Select tracking data
        idata <- dplyr::filter(data, date == dates[i])
        if(length(idata$date) == 0) next
        
      # Locate file
        pat <- format(as.Date(date), "%Y%m")
        pattern1 = c(pat,".grd")
        grdfile <- list.files(stack_repo, recursive = TRUE, full.names = TRUE, pattern = pattern1)
        # grdfile <- grdfile[grdfile %like% ".grd" & grdfile %like% ssp[j]]
        grdfile <- grdfile[grdfile %like% ".grd"]
      
      # Import environmental stack
        e <- raster::stack(grdfile)
        
        names(e) <-     ifelse(names(e) %like% "BAT", "BAT",
                        ifelse(names(e) %like% "SLP", "SLP",
                        ifelse(names(e) %like% "SDIST", "SDIST",
                        ifelse(names(e) %like% "D2COL_DEC", "D2COL_DEC",
                        ifelse(names(e) %like% "D2COL_CSH", "D2COL_CSH",
                        ifelse(names(e) %like% "CHL", "CHL",
                        ifelse(names(e) %like% "EDGE", "EDGE",
                        ifelse(names(e) %like% "EKE", "EKE",
                        ifelse(names(e) %like% "MLD", "MLD",
                        ifelse(names(e) %like% "O2", "O2",
                        ifelse(names(e) %like% "DIAT", "DIAT",
                        ifelse(names(e) %like% "SIC", "SIC",
                        ifelse(names(e) %like% "SALg", "SALg",
                        ifelse(names(e) %like% "SAL", "SAL",
                        ifelse(names(e) %like% "THETAO", "THETAO",
                        ifelse(names(e) %like% "SSTg", "SSTg",
                        ifelse(names(e) %like% "SST", "SST",
                        ifelse(names(e) %like% "UO", "UO",
                        ifelse(names(e) %like% "VO", "VO", "")))))))))))))))))))
        
        idist <- e$EDGE
        idist[idist<0] <- NA
        
      # Create an absence map
        rabs <- idist
        rabs[!is.na(rabs)] <- 0
        
      # Create a presence map with both observed and simulated trips
        rpres <- rasterize(cbind(idata$lon, idata$lat), idist)
        rpres <- rpres/rpres
        
      # Create presence/absence
        rpa <- sum(rpres, rabs, na.rm=TRUE)
        rpa <- raster::mask(rpa, rabs)
        names(rpa) <- "OCC"
        
      # ncell raster
        rcell <- e$D2COL_CSH
        rcell[] <- 1:ncell(rcell)
        names(rcell) <- "CELLID"
  
      # Prepare data.frame
        s <- stack(rpa, idist, e$D2COL_CSH/1000, rcell)  # combine
        distdf <- data.frame(na.omit(values(s)))
        distdf$date <- date
        
      # Append to list
        dist_list[[i]] <- distdf
        
      # Combine data
        dist <- rbindlist(dist_list)
        dist$month <- month(dist$date)
        
      # Select presences
        dist_pres <- filter(dist, OCC == 1)
        cell_with_pres <- dist_pres %>%
                                    distinct(CELLID)
        
      # Select absences
      # Remove absence cells with at any observed or simulate location. Also, remove duplicated absences within same month
        dist_abs <- dist %>%
          dplyr::filter(OCC == 0, !CELLID %in% cell_with_pres$CELLID) %>%
          distinct(CELLID, month, .keep_all = TRUE)
        
      # Subsample absences
        dist_abs <- sample_n(dist_abs, size = nrow(dist_pres), replace = FALSE)
        
      # Combine
        dist <- bind_rows(dist_pres, dist_abs)
        
      # Export dataset
        outfile <- paste0(outdir, "/access_ice_", sexseason, ".csv")
        write.csv(dist, outfile, row.names = FALSE)
                
  
    }



#-----------------------------------------------------------------
# Fit binomial models (bootstrap)
#-----------------------------------------------------------------
# Read data
outdir <- here::here("InputOutput/00output/02habitatModel", sexseason, "BoostedRegressionTrees/full_model/accessibilityModel_SeaIce")
outfile <- paste0(outdir, "/access_ice_", sexseason, ".csv")
dist <- read.csv(outfile)

# Prepare clusters
cores <- 5
cl <- makeCluster(cores)
registerDoParallel(cl)

foreach(i=1:n.boot, .packages=c("splitstackshape", "scam", "stringr")) %dopar% {
  
        # sampled half the data (with replacement) to fit the model (Hindell et al. 2020)
        idist <- stratified(dist, c("OCC", "month"), 0.5, replace = TRUE)
        
        # Fitted binomial models with a smooth, monotonic decreasing constraint (see Hindell 2020)
        # Check: https://github.com/SCAR/RAATD/blob/master/Code/runAvailabilityModels_04.R
        # For agazella: https://github.com/SCAR/RAATD/blob/master/Code/runAvailabilityModels_CRAS%26WESE.R
        scamMod <- scam(formula = OCC ~ s(EDGE , bs="mpd"),  # Monotone decreasing P-splines
                        family = binomial,
                        data = dist)
        
        # store model
        outfile <- paste0(outdir, "/", str_pad(i, 2, pad = "0"), "_", sp_code, "_access_ice_boot_", sexseason, ".rds")
        saveRDS(scamMod, file = outfile)  # save model

        
        }

# Stop clusters
stopCluster(cl)