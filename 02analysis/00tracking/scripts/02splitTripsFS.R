#-----------------------------------------------------------------------------------------
# 02SplitTripsFS.R        Pre-process tracking data
#-----------------------------------------------------------------------------------------
# install.packages("track2KBA")
library(track2KBA)
library(dplyr)
main_dir <- "C:/Users/Jazel Ouled/Dropbox/PhD/Arctocephalus-gazella"

main_dir <- here::here("InputOutput/")

#---------------------------------------------------------------------
# readTrack     Read standardized animal track data in csv
#---------------------------------------------------------------------
readTrack <- function(csvfiles){
  # Description
  # Reads a standardized animal track data in csv.
  # Returns a data frame with parsed time
  # It allows the combination of multiple files
  # csvfiles: string with the location of 1 or more csv files
  
  library(lubridate)
  library(data.table)
  
  ## create empty list
  dt_list <- list()  
  
  ## process and append files
  for (i in 1:length(csvfiles)){
    data <- read.csv(csvfiles[i], header=TRUE)  # read csv
    # data$date <- parse_date_time(data$date, "Ymd HMS") # parse time
    dt_list[[i]] <- data  # append to list
  }
  
  dt <- rbindlist(dt_list, fill=TRUE)  # combine data.frames
  return(dt)
}
#---------------------------------------------------------------------


output_data <- paste0(main_dir, "00output/")
indir <- paste0(output_data, "tracking", "/L1_locations")
loc_files <- list.files(indir, full.names = TRUE, pattern = "L1_locations.csv")
df <- readTrack(loc_files)
str(df)


fs <- subset(df, df$season == "summer")


# fs$time <- paste(fs$UTC_Date, fs$UTC_Time)
fs$date <- as.POSIXct(fs$date, format = "%Y-%m-%d %H:%M:%S")
class(fs$date)

fs$Latitude <- as.numeric(fs$Latitude)
fs$Longitude <- as.numeric(fs$Longitude)


str(fs)


sel_min_loc <- 10  # minimum number of locations
sel_min_dur <- 12 # minimum durantion of track, in hours
sel_exclude <- NULL # custom selection of tags based on exploration of data
sel_min_dist <- 15 # minimum distance of tracks, in km


#-------------------------------#
# Split trajectories into trips #
#-------------------------------#

fs <- fs[-9]

str(fs)

# prepare dataset 
dataGroup  <- fs  %>%
  
  # rename vars for tripSplit function
  rename(DateTime = date, 
         ID = id,
         Longitude = lon ,
         Latitude = lat) 
str(dataGroup)

dataGroup$UTC_Date <- as.POSIXct(dataGroup$UTC_Date, format = "%d/%m/%Y")
dataGroup$UTC_Time <- as.POSIXct(dataGroup$UTC_Time, format = "%H:%M:%S")

class(dataGroup$DateTime)

dataGroup <- formatFields(
  dataGroup = dataGroup, 
  fieldID   = "ID", 
  fieldDate = "DateTime", 
  fieldTime = "DateTime",
  fieldLon  = "Longitude", 
  fieldLat  = "Latitude"
)


dataGroup$DateTime <- ymd_hms(dataGroup$DateTime)

class(dataGroup$DateTime)

# innerBuff: here you specify the radius around the colony that if the bird enters we consider it has "returned to the nest"
# returnBuff: trips getting closer than this distance (in km) to the colony will not considered incomplete. Further than this distance, they will
# duration: trips lasting less than this time (in hours) will not be considered foraging trips
# nests: set this to TRUE if you have a specific location for each nest

colony <- data.frame("Longitude" = -62.4, "Latitude" = -60.7)


colony <- dataGroup %>% 
  summarise(
    Longitude = first(Longitude), 
    Latitude  = first(Latitude)
  )

trips_ <- tripSplit(dataGroup, colony, innerBuff = 5, returnBuff = 10, duration = 12, nests = F)


mapTrips(trips = trips_, colony = colony)

sumTrips <- tripSummary(trips = trips_, colony = colony)

 # turn GPS to sf data
trips_sf <- st_as_sf(trips_)

# turn GPS to sf data
trips_df <- as.data.frame(trips_sf)

# reshape
trips_df <- trips_df %>%
  dplyr::select(-c(X,Y,geometry)) %>%
  rename(deploymentID = ID,
         longitude = Longitude,
         latitude = Latitude,
         time = DateTime) 


write.csv(trips_df, "C:/Users/Jazel Ouled/Dropbox/PhD/Arctocephalus-gazella/00input/tracking/trips_FS.csv")