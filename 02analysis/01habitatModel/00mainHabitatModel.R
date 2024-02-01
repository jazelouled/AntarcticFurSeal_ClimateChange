#---------------------------------------------------------------
# main_habitat_species
#---------------------------------------------------------------
# This script controls the workflow used for habitat modeling
# To obtain the environmental data (stacks) to perform the predictions, check out the 00mainTrackProcessing.R script header.

#-----------------------------------------------------------
# 1. Set parameters
#-----------------------------------------------------------
# Load dependencies
source(here::here("setup.R"))
source(here::here("00functions/functions.R"))

# Set initial parameters
sp_code <- "GAZ"  
sp_name <- "A. gazella"
stack_repo <- here::here("InputOutput/00output/00environment/01MonthlyStacks/00monthly_stacks_BRT/")
vars <- c("BAT", "SLP", "SST", "SSTg", "SAL", "SALg", "EKE", "CHL", "SIC", "EDGE")  # list of all predictors
vars<-c("BAT", "SLP", "SDIST", "D2COL_DEC", "D2COL_CSH","CHL", "EDGE", "EKE", "MLD", "DIAT", "SIC", "SAL","THETAO", "SST")

# Computer cores
cores <- 10  # numbers of cores



#-----------------------------------------------------------
# 2. Exploratory Data Analysis
#-----------------------------------------------------------
# Reads all presence/absence data and generates several plots to explore all variables
# - Checks for missing data
# - Pearson and Spearman rank correlations
# - Density plots to compare presence/absence
source(here::here("02analysis/01habitatModel/scripts/00exploratoryDataAnalysis.R"))
# After visual inspection, keep and exclude variables



#-----------------------------------------------------------
# 3. Fit BRT
#-----------------------------------------------------------
# Fit model
source(here::here("02analysis/01habitatModel/scripts/01fitBRT.R"))



#-----------------------------------------------------------
# 5. Fit Accessibility model
#-----------------------------------------------------------
# Fit model accessibility regarding ice
source(here::here("02analysis/01habitatModel/scripts/05accessibilityModelIce.R"))

# Fit model accessibility regarding distance to the colony
source(here::here("02analysis/01habitatModel/scripts/04accessibilityModelColony.R"))



#-----------------------------------------------------------
# 6. Predict combining BRT and accessbility model
#-----------------------------------------------------------
# Set prediction period
date_start <- as.Date("2006-01-01")
date_end <- as.Date("2100-12-01")

# Select if use bootrap models or full model
source(here::here("02analysis/01habitatModel/scripts/06predictBRTBootstrap.R"))