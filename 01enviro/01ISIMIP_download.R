#---------------------------------------------------------------
# Script to download the data from each model and organize it in subfolders
#---------------------------------------------------------------

print("Reading text file with links")
links_txt<-as.data.frame(readLines("D:/ISIMIP_netCDF/ISIMIP_netCDF_ocean_list.txt"))
links_txt<-as.data.frame(readLines("/Volumes/MyPassport_/ISIMIP_netCDF/ISIMIP_netCDF_ocean_list.txt"))

links_txt<-as.data.frame(readLines("~/Dropbox/ISIMIP_netCDF/ISIMIP_netCDF_atmosphere_list.txt"))


colnames(links_txt)<-"link"
library(data.table)

options(timeout=10000)

# Models available in ISIMIP:

# 1. GFDL-ESM4
# 2. IPSL-CM6A-LR
# 3. MPI-ESM1-2-HR
# 4. MRI-ESM2-0
# 5. UKESM1-0-LL



#---------------------------------------------------------------
# 1. GFDL-ESM4
#---------------------------------------------------------------


print("Preparing GFDL-ESM4 data")

# Subset .txt links file
GFDL <-as.data.frame(links_txt[links_txt$link %like% "GFDL" & links_txt$link %like% "historical" & links_txt$link %like% "onedeg" &
                    (links_txt$link %like%  "chl" | links_txt$link %like% "siconc" | links_txt$link %like% "tos" |
                     links_txt$link %like%  "o2-surf" | links_txt$link %like% "so-surf" | links_txt$link %like% "thetao" |
                     links_txt$link %like%  "uo" | links_txt$link %like% "vo"),])


# GFDL <-as.data.frame(links_txt[links_txt$link %like% "GFDL" & (links_txt$link %like% "ssp" | links_txt$link %like% "historical") & links_txt$link %like% "onedeg"
#                                & (links_txt$link %like%  "phydiat-vint" | links_txt$link %like%  "mlotst" | links_txt$link %like%  "intppdiat"),])
                                

GFDL <-as.data.frame(links_txt[links_txt$link %like% "GFDL" & (links_txt$link %like% "ssp" | links_txt$link %like% "historical") & links_txt$link %like% "onedeg"
                               & (links_txt$link %like%  "ph-surf"),])


GFDL <-as.data.frame(links_txt[links_txt$link %like% "GFDL" & (links_txt$link %like% "ssp")
                               & (links_txt$link %like%  "sfcwind"),])



# Change column name
colnames(GFDL)<-"link"

# Get ssp from link
ssp <- substr(GFDL$link, 86,91)

ssp <- substr(GFDL$link, 132,137)


# Loop to download and save .nc files in folders. Root --> model --> SSP
for (i in 1:nrow(GFDL)){
  ssp_ <- ssp[i]
  product_folder <- paste("~/Documents/ISIMIP_netCDF/models/GFDL-ESM4", ssp_, sep="/")
  if (!dir.exists(product_folder)) dir.create(paste(product_folder, sep = ""), recursive = TRUE)  # create output directory if does not exist
  print(paste("Downloading", GFDL$link[i]))
  filename <-gsub('.*/ ?(\\w+)', '\\1', GFDL$link[i])
  download.file(url = GFDL$link[i], destfile = paste0(product_folder, sep="","/", filename),  method = "libcurl", mode = "wb")
}


print("GFDL-ESM4 downloaded")



#---------------------------------------------------------------
# 2. IPSL-CM6A-LR
#---------------------------------------------------------------

print("Preparing IPSL-CM6A-LR data")

# Subset .txt links file
IPSL <-as.data.frame(links_txt[links_txt$link %like% "IPSL" & links_txt$link %like% "historical" & links_txt$link %like% "onedeg" &
                          (links_txt$link %like%  "chl" | links_txt$link %like% "siconc" | links_txt$link %like% "tos" |
                             links_txt$link %like%  "o2-surf" | links_txt$link %like% "so-surf" | links_txt$link %like% "thetao" |
                             links_txt$link %like%  "uo" | links_txt$link %like% "vo"),])


# IPSL <-as.data.frame(links_txt[links_txt$link %like% "IPSL" & (links_txt$link %like% "ssp" | links_txt$link %like% "historical") & 
#                                links_txt$link %like% "onedeg" & (links_txt$link %like%  "phydiat-vint" | links_txt$link %like%  "mlotst" | links_txt$link %like%  "intppdiat"),])


IPSL <-as.data.frame(links_txt[links_txt$link %like% "IPSL" & (links_txt$link %like% "ssp" | links_txt$link %like% "historical") & links_txt$link %like% "onedeg"
                               & (links_txt$link %like%  "ph-surf"),])

IPSL <-as.data.frame(links_txt[links_txt$link %like% "IPSL" & (links_txt$link %like% "ssp")
                               & (links_txt$link %like%  "sfcwind" | links_txt$link %like%  "_tas_"),])



# Change column name
colnames(IPSL)<-"link"

# Get ssp from link

ssp <- substr(IPSL$link, 91,96)

ssp <- ssp[ssp %like% "ssp370" & ssp %like% "ssp126"]

# següent és SSP370

# ssp <- substr(IPSL$link, 86,91)

# Loop to download and save .nc files in folders. Root --> model --> SSP
for (i in 1:nrow(IPSL)){
  ssp_ <- ssp[i]
  product_folder <- paste("~/Documents/ISIMIP_netCDF/models/IPSL-CM6A-LR", ssp_, sep="/")
  if (!dir.exists(product_folder)) dir.create(paste(product_folder, sep = ""), recursive = TRUE)  # create output directory if does not exist
  print(paste("Downloading", IPSL$link[i]))
  filename <-gsub('.*/ ?(\\w+)', '\\1', IPSL$link[i])
  download.file(url = IPSL$link[i], destfile = paste0(product_folder, sep="","/", filename),  method = "libcurl", mode = "wb")
}


print("IPSL-CM6A-LR downloaded")


#---------------------------------------------------------------
# 3. MPI-ESM1-2-HR
#---------------------------------------------------------------

print("Preparing MPI-ESM1-2-HR data")

# Subset .txt links file
# MPI <-as.data.frame(links_txt[links_txt$link %like% "MPI" & links_txt$link %like% "historical" & links_txt$link %like% "onedeg" &
#                           (links_txt$link %like%  "chl" | links_txt$link %like% "siconc" | links_txt$link %like% "tos" |
#                              links_txt$link %like%  "o2-surf" | links_txt$link %like% "so-surf" | links_txt$link %like% "thetao" |
#                              links_txt$link %like%  "uo" | links_txt$link %like% "vo"),])
# 


# MPI <-as.data.frame(links_txt[links_txt$link %like% "MPI" & (links_txt$link %like% "ssp" | links_txt$link %like% "historical") & 
#                                 links_txt$link %like% "onedeg" & (links_txt$link %like%  "phydiat-vint" | links_txt$link %like%  "mlotst" | links_txt$link %like%  "intppdiat"),])

MPI <-as.data.frame(links_txt[links_txt$link %like% "MPI" & (links_txt$link %like% "ssp" | links_txt$link %like% "historical") & links_txt$link %like% "onedeg"
                               & (links_txt$link %like%  "ph-surf"),])


# Change column name
colnames(MPI)<-"link"

# Get ssp from link
ssp <- substr(MPI$link, 86,91) 

# Loop to download and save .nc files in folders. Root --> model --> SSP
for (i in 1:nrow(MPI)){
  ssp_ <- unique(ssp)[i]
  product_folder <- paste("D:/ISIMIP_netCDF/models/MPI-ESM1-2-HR", ssp_, sep="/")
  if (!dir.exists(product_folder)) dir.create(paste(product_folder, sep = ""), recursive = TRUE)  # create output directory if does not exist
  print(paste("Downloading", MPI$link[i]))  
  filename <-gsub('.*/ ?(\\w+)', '\\1', MPI$link[i])
  download.file(url = MPI$link[i], destfile = paste0(product_folder, sep="","/", filename),  method = "libcurl", mode = "wb")
}

print("MPI-ESM1-2-HR downloaded")


#---------------------------------------------------------------
# MRI-ESM2-0
#---------------------------------------------------------------
# 
# print("Preparing MRI-ESM2-0 data")
# 
# # Subset .txt links file
# MRI <-as.data.frame(links_txt[links_txt$link %like% "MRI" & links_txt$link %like% "ssp" & 
#                          (links_txt$link %like%  "thetao" | links_txt$link %like% "tos"),])
# 
# # Change column name
# colnames(MRI)<-"link"
# 
# # Get ssp from link
# ssp <- substr(MRI$link, 86,91) 
# 
# # Loop to download and save .nc files in folders. Root --> model --> SSP
# for (i in 1:nrow(MRI)){
#   ssp_ <- ssp[i]
#   product_folder <- paste("D:/ISIMIP_netCDF/MRI-ESM2-0", ssp_, sep="/")
#   if (!dir.exists(product_folder)) dir.create(paste(product_folder, sep = ""), recursive = TRUE)  # create output directory if does not exist
#   print(paste("Downloading", MRI$link[i])) 
#   filename <-gsub('.*/ ?(\\w+)', '\\1', MRI$link[i])
#   download.file(url = MRI$link[i], destfile = paste0(product_folder, sep="","/", filename),  method = "libcurl", mode = "wb")
# }
# 
# print("MRI-ESM2-0 downloaded")


#---------------------------------------------------------------
# UKESM1-0-LL
#---------------------------------------------------------------

print("Preparing UKESM1-0-LL data")

# Subset .txt links file
# UKESM1 <-as.data.frame(links_txt[links_txt$link %like% "UKESM1" & links_txt$link %like% "historical" & links_txt$link %like% "onedeg" &
#                          (links_txt$link %like%  "chl" | links_txt$link %like% "siconc" | links_txt$link %like% "tos" |
#                             links_txt$link %like%  "o2-surf" | links_txt$link %like% "so-surf" | links_txt$link %like% "thetao" |
#                             links_txt$link %like%  "uo" | links_txt$link %like% "vo"),])

# UKESM1 <-as.data.frame(links_txt[links_txt$link %like% "UKESM1" & (links_txt$link %like% "ssp" | links_txt$link %like% "historical") & 
#                                    links_txt$link %like% "onedeg" & (links_txt$link %like%  "phydiat-vint" | links_txt$link %like%  "mlotst" | links_txt$link %like%  "intppdiat"),])
# 

UKESM1 <-as.data.frame(links_txt[links_txt$link %like% "UKESM1" & (links_txt$link %like% "ssp" | links_txt$link %like% "historical") & links_txt$link %like% "onedeg"
                              & (links_txt$link %like%  "ph-surf"),])


# Change column name
colnames(UKESM1)<-"link"

# Get ssp from link
ssp <- substr(UKESM1$link, 86,91) 

# Loop to download and save .nc files in folders. Root --> model --> SSP
for (i in 1:nrow(UKESM1)){
  ssp_ <- ssp[i]
  product_folder <- paste("D:/ISIMIP_netCDF/models/UKESM1-ESM2-0", ssp_, sep="/")
  if (!dir.exists(product_folder)) dir.create(paste(product_folder, sep = ""), recursive = TRUE)  # create output directory if does not exist
  print(paste("Downloading", UKESM1$link[i]))  
  filename <-gsub('.*/ ?(\\w+)', '\\1', UKESM1$link[i])
  download.file(url = UKESM1$link[i], destfile = paste0(product_folder, sep="","/", filename),  method = "libcurl", mode = "wb")
}


print("UKESM1-0-LL downloaded")
print("All data downloaded")
