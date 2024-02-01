# Script hard-coded by Dr. David March (https://zenodo.org/badge/latestdoi/235335964). Adapted by Jazel Ouled-Cheikh. 

#----------------------------------------------------------------------
# read_agazella       Read processed data from Cardona
#----------------------------------------------------------------------
read_agazella <- function(file){
  # Sitrack data is formatted in a Excel file by Lluis Cardona
  # Data for each tagged animal is stored in a individual sheet
  # We will get habitat classification AND deployment date from this data
  
  # Load libraries
  library(dplyr)
  library(openxlsx)
  library(data.table)
  
  # custom function embeded
  xls2degrees <- function(x){
    # Manipulating data in Excel can generate problems in coordinates because of changes of the
    # regional system.
    # This function fixes this issue. But assumes that coordinates are XX.XXXX
    
    library(stringr)
    
    
    lon_length <- str_length(x)  # get the length of the string
    lon_num <- as.numeric(x)  # convert the string to a number
    dec <- str_detect(x, pattern = "\\.", negate = FALSE)  # identify if there is a decimal separator
    minus <- str_detect(x, pattern = "-", negate = FALSE)  # identify if there is a negative coordinate
    decimals <- ifelse(minus == "TRUE", lon_length-3, lon_length-2)  # define number of decimals
    lon <- ifelse(dec == "TRUE", lon_num, lon_num/10^(decimals))
    
    return(lon)
  }
  
  # 1. Get the list of all sheets 
  sheet_names <- getSheetNames(file)
  
  
  # 2. Loop to process each sheet and combine with the others
  data_list <- list()
  
  for (i in 1:length(sheet_names)){
    print(i)
    # import data
    df <- read.xlsx(xlsxFile = file, sheet = i, detectDates = F)
    
    # process date time
    # if (unique(df$sex) == "males") {
    #   
    #   # process date time
    #   options(digits=20)
    #   df$date <- df$UTC_Date + df$UTC_Time
    #   df$date <- as.POSIXct(df$date*3600*24, tz="GMT", origin = "1900-01-01 00:00:00") -(2*3600*24)
    #   options(digits=7)  # return to default value
    
    # } else if (unique(df$sex) == "females"){
    
    options(digits=20)
    df$date <- paste(df$UTC_Date, df$UTC_Time)
    df$date <- as.POSIXct(df$date, format = "%d/%m/%Y %H:%M")
    df <- df[!is.na(df$date),]
    
    # } 
    
    # df$date <- as.POSIXct(df$date*3600*24, tz="GMT", origin = "1900-01-01 00:00:00") -(2*3600*24)
    # options(digits=7)  # return to default value
    # df$date <- format(df$date, "%d/%m/%Y %H:%M")
    # # select columns
    # df <- dplyr::select(df, id = Tag_ID, date = date, lon = Longitude, lat = Latitude, lc = Location.Quality, habitat = `Habitat(1=land;2=water;.3=.ice)`)
    df <- dplyr::select(df, id = cc_id, Tag_ID = Tag_ID, date = date, lon = Longitude, lat = Latitude, lc = Location.Quality, sex = sex, season = season)
    
    # extract ptt number
    # df$id <- as.numeric(sub('.*\\:', '', df$id))
    
    # remove row without location data
    df <- dplyr::filter(df, !is.na(lon))
    
    # fix coordinates problems
    df$lon <- as.numeric(gsub("\\s", "", df$lon))
    df$lon <- xls2degrees(df$lon)
    df$lat <- as.numeric(gsub("\\s", "", df$lat))
    df$lat <- xls2degrees(df$lat)
    
    # Create KF error ellipse information
    # Current version contains empty parameters
    df$smaj <- NA
    df$smin <- NA
    df$eor <- NA
    
    # append to data_list
    data_list[[i]] <- df
  }
  
  # combine all tags
  data <- rbindlist(data_list)
  
  # filter out locations without habitat
  # data <- dplyr::filter(data, !is.na(habitat))
  
  return(data)
}

#------------------------------------------------------------------------------------



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



#------------------------------------------------------------------------------------
# summarizeTrips        Sumarize tracking data per trip
#------------------------------------------------------------------------------------
summarizeTrips <- function(data){
  # data is a data.frame with all tracking data per species
  
  library(geosphere)
  library(dplyr)
  
  df <- data %>%
    dplyr::arrange(date) %>%  # order by date
    dplyr::group_by(id, trip) %>%  # select group info
    dplyr::summarize(date_deploy = first(date),
                     lon_deploy = first(lon),
                     lat_deploy = first(lat),
                     date_last = last(date),
                     time_interval_h = median(as.numeric(difftime(tail(date, -1), head(date, -1), units="hours"))),
                     distance_km = sum(distGeo(p1 = cbind(lon, lat)), na.rm=TRUE)/1000,  # segment distance
                     n_loc = n()) %>%  # get first and last observations
    dplyr::mutate(duration_h = round(difftime(date_last, date_deploy, units="hours")))  # calculate duration of the track
  
  return(df)
}
#------------------------------------------------------------------------------------



#-----------------------------------------------------------------
# map_argos       Map Argos locations
#-----------------------------------------------------------------
map_argos <- function (data){
  
  # Load libraries and dependencies
  library(ggplot2)
  # source("R/config.R")  # set your Google Drive data folder here
  # source("R/database_tools.R")
  
  # Import world map
  data(countriesHigh, package = "rworldxtra", envir = environment())
  wm <- suppressMessages(fortify(countriesHigh))
  
  ### Get metadata
  sdate <- data$date[1]
  edate <- data$date[nrow(data)]
  days <- round(as.numeric(difftime(edate, sdate, units="days")))
  
  ### Define extension for plot
  xl <- extendrange(data$lon, f = 0.2)
  yl <- extendrange(data$lat, f = 0.2)
  
  ### Plot
  p <- ggplot() +
    geom_polygon(data = wm, aes_string(x = "long", y = "lat", group = "group"),
                 fill = grey(0.3)) +
    coord_quickmap(xlim = xl, ylim = yl, expand = TRUE) +
    xlab("Longitude") +
    ylab("Latitude") +
    geom_point(data = data,
               aes_string(x = "lon", y = "lat", group = NULL, colour = "lc"),
               size = 2) +
    geom_path(data = data,
              aes_string(x = "lon", y = "lat", group = NULL)) +
    labs(title = paste(data$sp_code[1], "Id:", data$id[1]),
         subtitle = paste("Start:", sdate, "End:", edate, "(", days, "days)")) 
  
  
  return(p)
  
}
#-----------------------------------------------------------------


#-----------------------------------------------------------------
# mapSimTracks       Plot simulations and observed track
#-----------------------------------------------------------------
mapSimTracks  <- function (simData, obsData, title = NULL){
  
  # Load libraries and dependencies
  library(ggplot2)
  
  # Import world map
  data(countriesHigh, package = "rworldxtra", envir = environment())
  wm <- suppressMessages(fortify(countriesHigh))
  
  ### Define extension for plot
  xl <- extendrange(c(simData$lon, obsData$lon), f = 0)
  yl <- extendrange(c(simData$lat, obsData$lat), f = 0)
  
  ### Plot
  p <- ggplot() +
    geom_polygon(data = wm, aes_string(x = "long", y = "lat", group = "group"),
                 fill = grey(0.3)) +
    coord_quickmap(xlim = xl, ylim = yl, expand = TRUE) +
    xlab("Longitude") +
    ylab("Latitude") +
    geom_path(data = simData,
              aes_string(x = "lon", y = "lat", group = "simid"),
              size=0.5, alpha=0.5, color="grey70") +
    geom_path(data = obsData,
              aes_string(x = "lon", y = "lat", group = "trip"),
              size=0.5, alpha=1, color="red1") +
    geom_point(data = data.table::first(obsData),
               aes_string(x = "lon", y = "lat"),
               shape = 21, colour = "red4", fill = "white", size = 2, stroke = 2) +
    #size=3, alpha=1, color="black") + #(shape = 21, colour = "red", fill = "white", size = 4, stroke = 2)
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    ggtitle(title)
  
  return(p)
}
#-----------------------------------------------------------------



#------------------------------------------------------------------------------------
# spt_overlap     Spatio-temporal overlap of grid cells
#------------------------------------------------------------------------------------
spt_overlap <- function(abs_cell, abs_date, pres_df, temporal_thrs, grid){
  # Given a cell number and date for an absence, this function checks the overlap with presences.
  # For the temporal criteria, it first filter all presence within the temporal threshold (in days).
  # Then, for the filtered cells, the function checks if they are adhjacent to the target cell.
  #
  # absence and presence cell number have to be originated from the same raster (grid)
  
  library(dplyr)
  library(raster)
  
  # check if there are presence for a given date, considering temporal window
  ipres <- dplyr::filter(pres_df, date >= abs_date - temporal_thrs, date <= abs_date + temporal_thrs)
  if(nrow(ipres)==0) keep <- TRUE
  
  # check if there are adjacent cells
  if(nrow(ipres)>0){
    adj <- adjacent(grid, cells = as.numeric(abs_cell), directions = 8, pairs = FALSE,
                    include = TRUE, target = ipres$cell)
    
    if(length(adj)==0) keep <- TRUE
    if(length(adj)>0) keep <- FALSE
  }
  
  return(keep)
}
#------------------------------------------------------------------------------------



#------------------------------------------------------------------------------------
# density.plot     Density plots of environmental data on observed and simulated locations
#------------------------------------------------------------------------------------
density.plot <- function(title="", xlab="SST (ºC)", legend="", alpha=0.35, data=data, var=SST, group=type, cols = c("#d7191c", "#2c7bb6")){
  
  g <- ggplot(data, aes(x=var, color=group)) +
    geom_line(stat="density", size = 1, alpha = 1.0) +
    scale_color_manual(values=cols) +
    labs(title = title, x = xlab, fill="") +
    theme_light() 
  return(g)
  
}
#------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------
# plot_Missing    Plot missing data
#------------------------------------------------------------------------------------
plot_Missing <- function(data_in, title = NULL){
  # https://www.kaggle.com/notaapple/detailed-exploratory-data-analysis-using-r
  temp_df <- as.data.frame(ifelse(is.na(data_in), 0, 1))
  temp_df <- temp_df[,order(colSums(temp_df))]
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$m <- as.vector(as.matrix(temp_df))
  data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))
  ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(m))) + scale_fill_manual(values=c("grey80", "grey10"), name="Missing\n(0=Yes, 1=No)") + theme_light() + ylab("") + xlab("") + ggtitle(title)
}
#------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------
# radarPlot       Radar plot of variable contribution from models
#-----------------------------------------------------------------------------------
radarPlot <- function(var_imp, var_order, colors_border=rgb(0.2,0.5,0.5,0.9), colors_in=rgb(0.2,0.5,0.5,0.4)){
  
  # set parameters for plot
  min_val <- 0
  max_val <- ceiling(max(var_imp))#100
  nseg <- 4
  
  # prepare data.frame
  var_imp <- dplyr::select(data.frame(t(var_imp)), var_order)
  data <- rbind(rep(max_val, length(var_imp)) , rep(min_val, length(var_imp)), var_imp)
  data <- data.frame(data)
  row.names(data) <- c("max", "min", "MaxEnt")
  
  radarchart(data  , axistype=1 , 
             #custom polygon
             pcol=colors_border , pfcol=colors_in , plwd=3 , plty=1,
             # number of segments
             seg=nseg,
             #custom the grid
             cglcol="grey", cglty=1, axislabcol="grey", caxislabels=round(seq(min_val,max_val,max_val/nseg),1), cglwd=0.8,
             #custom labels
             vlcex=1)
}
#-----------------------------------------------------------------------------------