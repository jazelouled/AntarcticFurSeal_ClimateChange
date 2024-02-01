#----------------------------------------------------------------------------------
# 00mainTrackProcessing.R     Main script for processing satellite tracking data
#----------------------------------------------------------------------------------
# This script serves as a guide to run the processing of the tracking data. To obtain the tracking data, go to 
# https://doi.org/10.34810/data1043 (repository of the university of Barcelona). There you will find the data to feed
# the first script (00preprocessingTracks) with the name: completeTrackingDatasetAGAZ.xlsx. You will also find the 
# 01monthlyStacks folder, which contains the folders 00monthly_stacks_BRT and 01monthly_stacks_predict. 00monthly_stacks_BRT
# contains the stacks used to do the extractions that we will use to fit the models (step 7 here). 01monthly_stacks_predict 
# contains the stacks to do the future projections. 

# The downloaded data needs to be organised as follows:

# Create a folder in the Rproject mother folder called InputOutput. Structure:

# - InputOutput
# ---- 00input
# 
# ------ completeTrackingDatasetAGAZ.xlsx
# 
# ------ environment
# ----------- bathymetry
# -------------- GEBCO Bathymetry files 
# 
# 
# ---- 00output
# ------ 00terrain
# --------- derived static variable products (distance to shore, to colony, slope, bathymetry)
# 
# ------ 01monthlyStacks
# --------- 00monthly_stacks_BRT (used to do the extractions)
# --------- 01monthly_stacks_predict (used to do the predictions)


#---------------------------------------------------------------
# 1. Set parameters
#---------------------------------------------------------------
sp_code <- "GAZ"  # species code
tag_type <- "PTT"


# Trip definition
trip_type <- "time"  # haul: trim track by haul-out locations; time: trim track by time gaps
trip_time_gap <- 7 * 24  # (used if trip_type == time) Tracks with data gaps in excess of [seg_time_gap] hours were broken up for separate modeling

# Track selection
sel_min_loc <- 10  # minimum number of locations
sel_min_dur <- 12 # minimum durantion of track, in hours
sel_exclude <- NULL # custom selection of tags based on exploration of data
sel_min_dist <- 15 # minimum distance of tracks, in km

# Track filtering
filt_step_time <- 2/60  # time difference to consider duplicated positions, in hours
filt_step_dist <- 0/1000  # spatial distance to consider duplicated poisitions, in km
filt_land <- FALSE  # remove locations on land
filt_vmax <- 3  # value of the maximum of velocity using in sdafilter, in m/s
filt_ang <- c(15, 25) # value of the angle using in sdafilter, no spikes are removed if ang=-1
filt_distlim <- c(2500, 5000) # value of the limite distance using in sdafilter, no spikes are removed if ang=-1

# Track regularization
reg_time_step <- 6  # time step to interpolate positions, in hours

# create ocean mask
mcp_expand <- 5  # expand the minimum convex polygon, in degrees.

# Simulations for CPF FS
# sim_n <- 50  # number of simulations
# sim_fix_last <- TRUE  # fix last track location
# sim_exclude <- NULL # remove individuals from simulations
# sim_by_trip <- TRUE  # generate simulation by trip rather than full track

# Simulations for non-CPF FS, MW, FW
sim_n <- 50  # number of simulations
sim_fix_last <- FALSE  # fix last track location
sim_exclude <- NULL # remove individuals from simulations
sim_by_trip <- TRUE  # generate simulation by trip rather than full track


# Extract environment
env_buffer <- 100000  # radius of buffer to average environmental data around each location, in meters. (100000)
all_vars <- c("BAT", "SLP", "SDIST", "SST", "SSTg", "SAL", "SALg", "SSH", "EKE", "CHL", "SIC", "SIT", "MLD", "EDGE", "D2COL")
env_max_date <- as.Date("2019-09-16")


#---------------------------------------------------------------
# 2. Set data paths and import libraries
#---------------------------------------------------------------
# Load dependencies
source("setup.R")
source(here::here("00functions/functions.R"))

#---------------------------------------------------------------
# 3. Processing workflow
#---------------------------------------------------------------
# Set number of cores for parallel processing
# detectCores()-2
cores <- 10

# Step 1. Pre-process data and standardize data
# Transforms different data sources into a common format
# Output data can be found: main_dir/output/species/sp_code/L0_locations
source(here::here("02analysis/00tracking/scripts/00preprocessingTracks.R"))

# Step 2. Filter location data
# Filtering is based on selected parameters from above
source(here::here("02analysis/00tracking/scripts/01filterLocs.R"))

# Step 3. Regularize location data (PROBLEMS WITH SERVER - USE PC)
# Uses correlated random walk state-space model from Jonsen et al. 2019 doi:10.1002/ecy.2566
source(here::here("02analysis/00tracking/scripts/03regularizeSSM.R"))

# Step 4. Generate oceanmask
# Combines MCP from all L2 locations with bathymetry and ice extent
source(here::here("02analysis/00tracking/scripts/04oceanMask.R"))

# Step 5. Generate pseudo-absences using simulations for habitat model
# We select individuals that remain within the study area.
source(here::here("02analysis/00tracking/scripts/05trackSimulations.R"))

# Step 6. Generate Presence-Absence data eliminating overlap between observations
res <- 1            # size of spatial bin, in decimal degrees
temporal_thrs <- 2  # length of temporal bin, in days
sim_n <- 50         # select number of simulations to use
source(here::here("02analysis/00tracking/scripts/06presAbs.R"))

# Step 7. Extract environmental data
stack_repo <- here::here("InputOutput/00output/00environment/01MonthlyStacks/00monthly_stacks_BRT/")
source(here::here("02analysis/00tracking/scripts/07extractStackPresAbs.R"))