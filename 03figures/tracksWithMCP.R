# Plot tracks with minimum convex polygons --------------------------------

# Import L2 product of simulations (i.e. simulations with environmental data)
indir <- here::here("InputOutput/00output/tracking/L2_locations/")
loc_files <- list.files(indir, full.names = TRUE, pattern = "L2_locations.csv")
ssm <- readTrack(loc_files)
ssm$sex <- ifelse(ssm$id %like% "F", "females", "males")
ssm$season <- ifelse(ssm$id %like% "W", "winter", "summer")


#--------------------------------
# Minimum convex polygon
#--------------------------------


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

# Import world map
data(countriesHigh, package = "rworldxtra", envir = environment())
wm <- suppressMessages(fortify(countriesHigh))
data <- ssm

# Get metadata
sdate <- min(data$date)
edate <- max(data$date)
days <- round(as.numeric(difftime(edate, sdate, units="days")))

# Define extension for plot
xl <- extendrange(data$lon, f = 0.5)
yl <- extendrange(data$lat, f = 0.5)
blab <- c(as.Date(data$date[1]), as.Date(data$date[nrow(data)]))

polyFW <- st_as_sf(cp.buf_femalesW)
polyFS <- st_as_sf(cp.buf_femalesS)
polyMW <- st_as_sf(cp.buf_malesW)

p <- ggplot() +
  geom_sf(data = polyFW, fill = "lightcyan", alpha = 0.6)+
  geom_sf(data = polyMW, fill = "slategray2", alpha = 0.6)+
  geom_sf(data = polyFS, fill = "slategrey", alpha = 0.6)+
  coord_sf(xlim = c(-180,16.884), ylim = c(-83.606,-29.243), expand = F)+
  geom_polygon(data = wm, aes_string(x = "long", y = "lat", group = "group"), fill = grey(0.3)) +
  xlab("Longitude") +
  ylab("Latitude") +
  geom_point(data = data, aes_string(x = "lon", y = "lat",  colour ="sex", fill = "sex"), size = 0.4) +
  scale_colour_fish_d("Acanthurus_olivaceus") +
  geom_path(data = data, aes_string(x = "lon", y = "lat", group = "id"), size  = 0.2, alpha = 0.4) +
  labs(title = paste(data$sp_code[1], subtitle = paste("Start:", sdate, "End:", edate, "(", days, "days)"))) + 
  theme2Review


