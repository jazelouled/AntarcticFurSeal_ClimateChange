#---------------------------------------------------------------------
# accessibility
#---------------------------------------------------------------------

#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------
# Input data paths
indir <- paste0(output_data, "/habitat-model-cc/", sp_code, "/",  sexseason, "/")
stack_repo <- "D:/ISIMIP_netCDF/output/monthly_stacks_BRT"
stack_repo <- "/Volumes/MyPassport_/ISIMIP_netCDF/output/monthly_stacks_BRT"

# Output data paths
outdir <- paste(output_data, "habitat-model-cc", sp_code, sexseason, mod_code,  "full_model", "access_d2col_boost", sep="/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

#-----------------------------------------------------------------
# Prepare data
#-----------------------------------------------------------------
# Import observations
obs_file <- list.files("C:/Users/Jazel Ouled/Dropbox/PhD/Arctocephalus-gazella/00output/tracking/AGAZ/L3_locations/", recursive = T, full.names = T)
obs_file <-list.files("~/Dropbox/PhD/Arctocephalus-gazella/00output/tracking/AGAZ/L3_locations_FScpf/" , recursive = T, full.names = T)
data <- read.csv(obs_file)
data$date <- ymd(data$date)
data <- data[data$id %like% sexseason,]

#-----------------------------------------------------------------
# Get presence/absence data and link to distance to colony
#-----------------------------------------------------------------
# import distance to colony
grdfile <- list.files(stack_repo, recursive = TRUE, full.names = TRUE, pattern = ".grd")
e <- raster::stack(grdfile[1])

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

idist <- e$D2COL_CSH

idist <- e$SDIST*1000

plot(e$D2COL_CSH)
plot(e$SDIST)



## Create an absence map
rabs <- idist
rabs[!is.na(rabs)] <- 0

## Create a presence map with both observed and simulated trips
rpres <- rasterize(cbind(data$lon, data$lat), idist)
rpres <- rpres/rpres

## Create presence/absence
rpa <- sum(rpres, rabs, na.rm=TRUE)
rpa <- raster::mask(rpa, rabs)
names(rpa) <- "OCC"

## Prepare data.frame
s <- stack(rpa, idist)  # combine
dist <- data.frame(na.omit(values(s)))

plot(s)

#-----------------------------------------------------------------
# Fit binomial models
#-----------------------------------------------------------------

# if the dataset is too large generate a random subsample without replacement
# stratification by prevalence and date
if(nrow(dist) > 40000){
  dist_pres <- filter(dist, OCC == 1)
  dist_abs <- filter(dist, OCC == 0)
  dist_abs <- sample_n(dist_abs, size = 40000-nrow(dist_pres), replace = FALSE)
  dist <- bind_rows(dist_pres, dist_abs)
} 


# Fitted binomial models with a smooth, monotonic decreasing constraint (see Hindell 2020)
# Check: https://github.com/SCAR/RAATD/blob/master/Code/runAvailabilityModels_04.R
# For agazella: https://github.com/SCAR/RAATD/blob/master/Code/runAvailabilityModels_CRAS%26WESE.R
scamMod <- scam(formula = OCC ~ s(D2COL_CSH, bs="mpd"),  # Monotone decreasing P-splines
                family = binomial,
                data = dist)

dist$D2COL_CSH


scamMod <- scam(formula = OCC ~ s(SDIST, bs="mpd"),  # Monotone decreasing P-splines
                family = binomial,
                data = dist)


# save
saveRDS(scamMod, paste0(outdir, "/d2col_access.rds"))


# plot
dfpred <- data.frame(D2COL_CSH = seq(minValue(idist), maxValue(idist), 100))
dfpred$pre <- predict.scam(scamMod, newdata = dfpred, type="response")
plot(dfpred$D2COL_CSH, dfpred$pre, type="l")

#-----------------------------------------------------------------
# Predict model
#-----------------------------------------------------------------

## Predict model to each colony
accessibility <- predict(idist, scamMod, type="response")



#-----------------------------------------------------------------
# Export
#-----------------------------------------------------------------
# Export raster
writeRaster(accessibility, paste0(outdir, "/", sp_code, "d2col_access.grd"), bandorder='BIL', overwrite=TRUE)
