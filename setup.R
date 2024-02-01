#--------------------------------------------------------------------------------
# setup.R         Setup project
#--------------------------------------------------------------------------------

# Load required packages
pacman::p_load("here", "data.table", "tidyr", "dplyr", "lubridate", "readxl", "stringr", "reshape2", "splitstackshape", "groupdata2", # data manipulation
               "ggplot2", "egg", "pals", "viridis", "gridExtra", "grid", "scales", "ggprism", "ggimage", "RColorBrewer", "fishualize", # plots
               "foreach", "doParallel",  # parallel computing
               "move", "moveVis", "SDLfilter", "adehabitatHR", "foieGras", "argosfilter",  # movement
               "availability", # https://github.com/AustralianAntarcticDataCentre/availability
               "corrplot", "dismo", "gbm", "randomForest", "Hmisc", "ggBRT", "groupdata2", "scam", # habitat model
               "rJava", "fmsb",  "mgcv", # miscellaneous
               "rnaturalearthdata", "rnaturalearth",  # spatial data
               "raster", "sf", "ncdf4", "rgeos","maptools", "gdistance",  # spatial
               "rdrop2","openxlsx", "rworldxtra", "argosfilter", "track2KBA",
               install = TRUE)  # dropbox data

`%notlike%` <- Negate(`%like%`)

# For surrogateAR function (simulations)
# install.packages("remotes")
# remotes::install_github("AustralianAntarcticDataCentre/availability")
