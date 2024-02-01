
library(ncdf4)
library(raster)
library(data.table)
library(dplyr)
library(lubridate)
library(tidync)



# Empty data frames creation ----------------------------------------------

# GFDL-ESM4
monthly_mean_var_126_GFDL <- data.frame(date = Date(),
                               variable = character(),
                               mean_var = numeric(),
                               ssp = character(),
                               model = character())

monthly_mean_var_370_GFDL <- data.frame(date = Date(),
                                      variable = character(),
                                      mean_var = numeric(),
                                      ssp = character(),
                                      model = character())

monthly_mean_var_585_GFDL <- data.frame(date = Date(),
                                         variable = character(),
                                         mean_var = numeric(),
                                         ssp = character(),
                                         model = character())


# IPSL-CM6A-LR
monthly_mean_var_126_IPSL <- data.frame(date = Date(),
                                        variable = character(),
                                        mean_var = numeric(),
                                        ssp = character(),
                                        model = character())

monthly_mean_var_370_IPSL <- data.frame(date = Date(),
                                         variable = character(),
                                         mean_var = numeric(),
                                         ssp = character(),
                                         model = character())

monthly_mean_var_585_IPSL <- data.frame(date = Date(),
                                         variable = character(),
                                         mean_var = numeric(),
                                         ssp = character(),
                                         model = character())
# MPI-ESM1-2-HR
monthly_mean_var_126_MPI <- data.frame(date = Date(),
                                        variable = character(),
                                        mean_var = numeric(),
                                        ssp = character(),
                                        model = character())

monthly_mean_var_370_MPI <- data.frame(date = Date(),
                                         variable = character(),
                                         mean_var = numeric(),
                                         ssp = character(),
                                         model = character())

monthly_mean_var_585_MPI <- data.frame(date = Date(),
                                         variable = character(),
                                         mean_var = numeric(),
                                         ssp = character(),
                                         model = character())

# MRI-ESM2-0
monthly_mean_var_126_MRI <- data.frame(date = Date(),
                                       variable = character(),
                                       mean_var = numeric(),
                                       ssp = character(),
                                       model = character())

monthly_mean_var_370_MRI <- data.frame(date = Date(),
                                        variable = character(),
                                        mean_var = numeric(),
                                        ssp = character(),
                                        model = character())

monthly_mean_var_585_MRI <- data.frame(date = Date(),
                                        variable = character(),
                                        mean_var = numeric(),
                                        ssp = character(),
                                        model = character())



# UKESM1-ESM2-0

monthly_mean_var_126_UKESM1 <- data.frame(date = Date(),
                                       variable = character(),
                                       mean_var = numeric(),
                                       ssp = character(),
                                       model = character())

monthly_mean_var_370_UKESM1 <- data.frame(date = Date(),
                                        variable = character(),
                                        mean_var = numeric(),
                                        ssp = character(),
                                        model = character())

monthly_mean_var_585_UKESM1 <- data.frame(date = Date(),
                                        variable = character(),
                                        mean_var = numeric(),
                                        ssp = character(),
                                        model = character())



# Loops -------------------------------------------------------------------

# nc_var <- "tos"
# nc_var <- "chl"
# nc_var <- "o2"
# nc_var <- "siconc"
# nc_var <- "so-surf"
# nc_var <- "thetao"
# nc_var <- "uo"
# nc_var <- "vo"
# nc_var <- "ph-surf"

# nc_var <- "so-surf"

ISIMIP_models <- dir("D:/ISIMIP_netCDF/models/", recursive = F, full.names = T )

ISIMIP_models <- dir("/Volumes/MyPassport_/ISIMIP_netCDF/models/", recursive = F, full.names = T )


for (z in 1:length(ISIMIP_models)){
      print(z)
      model <- gsub('.*/ ?(\\w+)', '\\1', ISIMIP_models[z])
      if((nc_var == "siconc" | nc_var == "chl"|nc_var == "o2"| nc_var == "so-surf" | nc_var == "uo" | nc_var == "vo" | nc_var == "ph-surf")  & model == "MRI-ESM2-0") next
      ssp_folders<- dir(ISIMIP_models[z], recursive = F, full.names = T )
      ssp <- gsub('.*/ ?(\\w+)', '\\1', ssp_folders)
      
      for (q in 1:length(ssp)){
      ISIMIP_files <- dir(ssp_folders[q], recursive = T, full.names = T )
      ISIMIP_files_var <- ISIMIP_files[ISIMIP_files %like% nc_var]
      filename <- gsub('.*/ ?(\\w+)', '\\1', ISIMIP_files_var)
            nc <- nc_open(ISIMIP_files_var)
            
            if (nc_var == "chl"){
              var <-ncvar_get(nc, varid=nc_var, start=c(1,1,1,1), count=c(-1,-1, 1, -1))
            } else (var <-ncvar_get(nc, varid=nc_var))
            
            lat<-ncvar_get(nc, varid="lat")
            lon<-ncvar_get(nc, varid="lon")
            time<-ncvar_get(nc, varid="time")
            time<-floor(time)
          
            if (model == "MRI-ESM2-0"){
              date <-as.POSIXct(days(time), origin = "2015-01-01", format ="%Y-%m-%d")
            } else {date <-as.POSIXct(months(time), origin = "1601-01-01", format ="%Y-%m-%d")
            }
            
            var_r<-brick(var)
            var_r<-(flip(var_r, direction="x"))
            var_r<-t(flip(var_r, direction="x"))
            bb<-extent(min(lon),max(lon),min(lat),max(lat))
            var_r<-setExtent(var_r,bb,keepres = F, snap = F)
            projection(var_r)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
            
            antarctica <- extent(-35.493, -8.188972, 2.316739, 25.73697) # MCP SSM tracks + 5 degrees
            
            # antarctica <- extent(-99.5, 16.5, -79.05833, -45)
            # antarctica <- extent(-90, -20, -80, -40)
            var_r_antarctica <- crop(var_r, antarctica)
            months<-seq(1: nlayers(var_r_antarctica))

        
            for (k in 1:length(months)){
              print(k)
              if (ssp[q] == "ssp126" & model == "GFDL-ESM4"){
                monthly_mean_var_126_GFDL[k,1] = as.POSIXct(date[[k]])
                var_r_antarctica_vals <- getValues(var_r_antarctica[[k]])
                monthly_mean_var_126_GFDL[[k,2]] <- nc_var
                mean_var <-mean(var_r_antarctica_vals, na.rm =T)
                monthly_mean_var_126_GFDL[[k,3]] <- mean(mean_var, na.rm = T)
                monthly_mean_var_126_GFDL[[k,4]] <- ssp[q] 
                monthly_mean_var_126_GFDL[[k,5]] <- model
                yearly_mean_var_126_GFDL <- monthly_mean_var_126_GFDL %>%
                  group_by(year(date)) %>%
                  summarize(mean_var = mean(mean_var, na.rm=TRUE)) %>%
                  mutate(variable = nc_var) %>%
                  mutate(ssp = ssp[q]) %>%
                  mutate(model = model) 
                colnames(yearly_mean_var_126_GFDL) <- c("year","mean_var","variable",  "ssp", "model" )
                

                
              } else if (ssp[q] == "ssp370" & model == "GFDL-ESM4"){
                  monthly_mean_var_370_GFDL[k,1] = as.POSIXct(date[[k]])
                  var_r_antarctica_vals <- getValues(var_r_antarctica[[k]])
                  monthly_mean_var_370_GFDL[[k,2]] <- nc_var
                  mean_var <-mean(var_r_antarctica_vals, na.rm =T)
                  monthly_mean_var_370_GFDL[[k,3]] <- mean(mean_var, na.rm = T)
                  monthly_mean_var_370_GFDL[[k,4]] <- ssp[q] 
                  monthly_mean_var_370_GFDL[[k,5]] <- model
                  yearly_mean_var_370_GFDL <- monthly_mean_var_370_GFDL %>%
                    group_by(year(date)) %>%
                    summarize(mean_var = mean(mean_var, na.rm=TRUE)) %>%
                    mutate(variable = nc_var) %>%
                    mutate(ssp = ssp[q]) %>%
                    mutate(model = model) 
                  colnames(yearly_mean_var_370_GFDL) <- c("year","mean_var","variable",  "ssp", "model" )
            
              } else if (ssp[q] == "ssp585" & model == "GFDL-ESM4"){
                  monthly_mean_var_585_GFDL[k,1] = as.POSIXct(date[[k]])
                  var_r_antarctica_vals <- getValues(var_r_antarctica[[k]])
                  monthly_mean_var_585_GFDL[[k,2]] <- nc_var
                  mean_var <-mean(var_r_antarctica_vals, na.rm =T)
                  monthly_mean_var_585_GFDL[[k,3]] <- mean(mean_var, na.rm = T)
                  monthly_mean_var_585_GFDL[[k,4]] <- ssp[q] 
                  monthly_mean_var_585_GFDL[[k,5]] <- model
                  yearly_mean_var_585_GFDL <- monthly_mean_var_585_GFDL %>%
                    group_by(year(date)) %>%
                    summarize(mean_var = mean(mean_var, na.rm=TRUE)) %>%
                    mutate(variable = nc_var) %>%
                    mutate(ssp = ssp[q]) %>%
                    mutate(model = model) 
                  colnames(yearly_mean_var_585_GFDL) <- c("year","mean_var","variable",  "ssp", "model" )
                
               
                  
           # IPSL-CM6A-LR
                  
              } else if (ssp[q] == "ssp126" & model == "IPSL-CM6A-LR"){
                    monthly_mean_var_126_IPSL[k,1] = as.POSIXct(date[[k]])
                    var_r_antarctica_vals <- getValues(var_r_antarctica[[k]])
                    monthly_mean_var_126_IPSL[[k,2]] <- nc_var
                    mean_var <-mean(var_r_antarctica_vals, na.rm =T)
                    monthly_mean_var_126_IPSL[[k,3]] <- mean(mean_var, na.rm = T)
                    monthly_mean_var_126_IPSL[[k,4]] <- ssp[q] 
                    monthly_mean_var_126_IPSL[[k,5]] <- model
                    yearly_mean_var_126_IPSL <- monthly_mean_var_126_IPSL %>%
                      group_by(year(date)) %>%
                      summarize(mean_var = mean(mean_var, na.rm=TRUE)) %>%
                      mutate(variable = nc_var) %>%
                      mutate(ssp = ssp[q]) %>%
                      mutate(model = model) 
                    colnames(yearly_mean_var_126_IPSL) <- c("year","mean_var","variable",  "ssp", "model" )

                  } else if (ssp[q] == "ssp370" & model == "IPSL-CM6A-LR"){
                    monthly_mean_var_370_IPSL[k,1] = as.POSIXct(date[[k]])
                    var_r_antarctica_vals <- getValues(var_r_antarctica[[k]])
                    monthly_mean_var_370_IPSL[[k,2]] <- nc_var
                    mean_var <-mean(var_r_antarctica_vals, na.rm =T)
                    monthly_mean_var_370_IPSL[[k,3]] <- mean(mean_var, na.rm = T)
                    monthly_mean_var_370_IPSL[[k,4]] <- ssp[q] 
                    monthly_mean_var_370_IPSL[[k,5]] <- model
                    yearly_mean_var_370_IPSL <- monthly_mean_var_370_IPSL %>%
                      group_by(year(date)) %>%
                      summarize(mean_var = mean(mean_var, na.rm=TRUE)) %>%
                      mutate(variable = nc_var) %>%
                      mutate(ssp = ssp[q]) %>%
                      mutate(model = model) 
                    colnames(yearly_mean_var_370_IPSL) <- c("year","mean_var","variable",  "ssp", "model" )
                    
                  } else if (ssp[q] == "ssp585" & model == "IPSL-CM6A-LR"){
                    monthly_mean_var_585_IPSL[k,1] = as.POSIXct(date[[k]])
                    var_r_antarctica_vals <- getValues(var_r_antarctica[[k]])
                    monthly_mean_var_585_IPSL[[k,2]] <- nc_var
                    mean_var <-mean(var_r_antarctica_vals, na.rm =T)
                    monthly_mean_var_585_IPSL[[k,3]] <- mean(mean_var, na.rm = T)
                    monthly_mean_var_585_IPSL[[k,4]] <- ssp[q] 
                    monthly_mean_var_585_IPSL[[k,5]] <- model
                    yearly_mean_var_585_IPSL <- monthly_mean_var_585_IPSL %>%
                      group_by(year(date)) %>%
                      summarize(mean_var = mean(mean_var, na.rm=TRUE)) %>%
                      mutate(variable = nc_var) %>%
                      mutate(ssp = ssp[q]) %>%
                      mutate(model = model) 
                    colnames(yearly_mean_var_585_IPSL) <- c("year","mean_var","variable",  "ssp", "model" )
                    
                    
          # MPI-ESM1-2-HR
                    
              } else if (ssp[q] == "ssp126" & model == "MPI-ESM1-2-HR"){
                    monthly_mean_var_126_MPI[k,1] = as.POSIXct(date[[k]])
                    var_r_antarctica_vals <- getValues(var_r_antarctica[[k]])
                    monthly_mean_var_126_MPI[[k,2]] <- nc_var
                    mean_var <-mean(var_r_antarctica_vals, na.rm =T)
                    monthly_mean_var_126_MPI[[k,3]] <- mean(mean_var, na.rm = T)
                    monthly_mean_var_126_MPI[[k,4]] <- ssp[q] 
                    monthly_mean_var_126_MPI[[k,5]] <- model
                    yearly_mean_var_126_MPI <- monthly_mean_var_126_MPI %>%
                      group_by(year(date)) %>%
                      summarize(mean_var = mean(mean_var, na.rm=TRUE)) %>%
                      mutate(variable = nc_var) %>%
                      mutate(ssp = ssp[q]) %>%
                      mutate(model = model) 
                    colnames(yearly_mean_var_126_MPI) <- c("year","mean_var","variable",  "ssp", "model" )

                  } else if (ssp[q] == "ssp370" & model == "MPI-ESM1-2-HR"){
                    monthly_mean_var_370_MPI[k,1] = as.POSIXct(date[[k]])
                    var_r_antarctica_vals <- getValues(var_r_antarctica[[k]])
                    monthly_mean_var_370_MPI[[k,2]] <- nc_var
                    mean_var <-mean(var_r_antarctica_vals, na.rm =T)
                    monthly_mean_var_370_MPI[[k,3]] <- mean(mean_var, na.rm = T)
                    monthly_mean_var_370_MPI[[k,4]] <- ssp[q] 
                    monthly_mean_var_370_MPI[[k,5]] <- model
                    yearly_mean_var_370_MPI <- monthly_mean_var_370_MPI %>%
                      group_by(year(date)) %>%
                      summarize(mean_var = mean(mean_var, na.rm=TRUE)) %>%
                      mutate(variable = nc_var) %>%
                      mutate(ssp = ssp[q]) %>%
                      mutate(model = model) 
                    colnames(yearly_mean_var_370_MPI) <- c("year","mean_var","variable",  "ssp", "model" )
                    
                  } else if (ssp[q] == "ssp585" & model == "MPI-ESM1-2-HR"){
                    monthly_mean_var_585_MPI[k,1] = as.POSIXct(date[[k]])
                    var_r_antarctica_vals <- getValues(var_r_antarctica[[k]])
                    monthly_mean_var_585_MPI[[k,2]] <- nc_var
                    mean_var <-mean(var_r_antarctica_vals, na.rm =T)
                    monthly_mean_var_585_MPI[[k,3]] <- mean(mean_var, na.rm = T)
                    monthly_mean_var_585_MPI[[k,4]] <- ssp[q] 
                    monthly_mean_var_585_MPI[[k,5]] <- model
                    yearly_mean_var_585_MPI <- monthly_mean_var_585_MPI %>%
                      group_by(year(date)) %>%
                      summarize(mean_var = mean(mean_var, na.rm=TRUE)) %>%
                      mutate(variable = nc_var) %>%
                      mutate(ssp = ssp[q]) %>%
                      mutate(model = model) 
                    colnames(yearly_mean_var_585_MPI) <- c("year","mean_var","variable",  "ssp", "model" )
                    

                # MRI-ESM2-0 (2 deg; only tos & thetao). Need to comment it out when processing other variables than 
                # thetao or tos

                  # } else if (ssp[q] == "ssp126" & model == "MRI-ESM2-0"){
                  #   monthly_mean_var_126_MRI[k,1] = as.POSIXct(date[[k]])
                  #   var_r_antarctica_vals <- getValues(var_r_antarctica[[k]])
                  #   monthly_mean_var_126_MRI[[k,2]] <- nc_var
                  #   mean_var <-mean(var_r_antarctica_vals, na.rm =T)
                  #   monthly_mean_var_126_MRI[[k,3]] <- mean(mean_var, na.rm = T)
                  #   monthly_mean_var_126_MRI[[k,4]] <- ssp[q]
                  #   monthly_mean_var_126_MRI[[k,5]] <- model
                  #   yearly_mean_var_126_MRI <- monthly_mean_var_126_MRI %>%
                  #     group_by(year(date)) %>%
                  #     summarize(mean_var = mean(mean_var, na.rm=TRUE)) %>%
                  #     mutate(variable = nc_var) %>%
                  #     mutate(ssp = ssp[q]) %>%
                  #     mutate(model = model)
                  #   colnames(yearly_mean_var_126_MRI) <- c("year","mean_var","variable",  "ssp", "model" )
                  # 
                  # } else if (ssp[q] == "ssp370" & model == "MRI-ESM2-0"){
                  #   monthly_mean_var_370_MRI[k,1] = as.POSIXct(date[[k]])
                  #   var_r_antarctica_vals <- getValues(var_r_antarctica[[k]])
                  #   monthly_mean_var_370_MRI[[k,2]] <- nc_var
                  #   mean_var <-mean(var_r_antarctica_vals, na.rm =T)
                  #   monthly_mean_var_370_MRI[[k,3]] <- mean(mean_var, na.rm = T)
                  #   monthly_mean_var_370_MRI[[k,4]] <- ssp[q]
                  #   monthly_mean_var_370_MRI[[k,5]] <- model
                  #   yearly_mean_var_370_MRI <- monthly_mean_var_370_MRI %>%
                  #     group_by(year(date)) %>%
                  #     summarize(mean_var = mean(mean_var, na.rm=TRUE)) %>%
                  #     mutate(variable = nc_var) %>%
                  #     mutate(ssp = ssp[q]) %>%
                  #     mutate(model = model)
                  #   colnames(yearly_mean_var_370_MRI) <- c("year","mean_var","variable",  "ssp", "model" )
                  # 
                  # } else if (ssp[q] == "ssp585" & model == "MRI-ESM2-0"){
                  #   monthly_mean_var_585_MRI[k,1] = as.POSIXct(date[[k]])
                  #   var_r_antarctica_vals <- getValues(var_r_antarctica[[k]])
                  #   monthly_mean_var_585_MRI[[k,2]] <- nc_var
                  #   mean_var <-mean(var_r_antarctica_vals, na.rm =T)
                  #   monthly_mean_var_585_MRI[[k,3]] <- mean(mean_var, na.rm = T)
                  #   monthly_mean_var_585_MRI[[k,4]] <- ssp[q]
                  #   monthly_mean_var_585_MRI[[k,5]] <- model
                  #   yearly_mean_var_585_MRI <- monthly_mean_var_585_MRI %>%
                  #     group_by(year(date)) %>%
                  # 
                  #     summarize(mean_var = mean(mean_var, na.rm=TRUE)) %>%
                  #     mutate(variable = nc_var) %>%
                  #     mutate(ssp = ssp[q]) %>%
                  #     mutate(model = model)
                  #   colnames(yearly_mean_var_585_MRI) <- c("year","mean_var","variable",  "ssp", "model" )
                    
                 # UKESM1-ESM2-0   
                    
                  } else if (ssp[q] == "ssp126" & model == "UKESM1-ESM2-0"){
                    monthly_mean_var_126_UKESM1[k,1] = as.POSIXct(date[[k]])
                    var_r_antarctica_vals <- getValues(var_r_antarctica[[k]])
                    monthly_mean_var_126_UKESM1[[k,2]] <- nc_var
                    mean_var <-mean(var_r_antarctica_vals, na.rm =T)
                    monthly_mean_var_126_UKESM1[[k,3]] <- mean(mean_var, na.rm = T)
                    monthly_mean_var_126_UKESM1[[k,4]] <- ssp[q] 
                    monthly_mean_var_126_UKESM1[[k,5]] <- model
                    yearly_mean_var_126_UKESM1 <- monthly_mean_var_126_UKESM1 %>%
                      group_by(year(date)) %>%
                      summarize(mean_var = mean(mean_var, na.rm=TRUE)) %>%
                      mutate(variable = nc_var) %>%
                      mutate(ssp = ssp[q]) %>%
                      mutate(model = model) 
                    colnames(yearly_mean_var_126_UKESM1) <- c("year","mean_var","variable",  "ssp", "model" )
                    
                  } else if (ssp[q] == "ssp370" & model == "UKESM1-ESM2-0"){
                    monthly_mean_var_370_UKESM1[k,1] = as.POSIXct(date[[k]])
                    var_r_antarctica_vals <- getValues(var_r_antarctica[[k]])
                    monthly_mean_var_370_UKESM1[[k,2]] <- nc_var
                    mean_var <-mean(var_r_antarctica_vals, na.rm =T)
                    monthly_mean_var_370_UKESM1[[k,3]] <- mean(mean_var, na.rm = T)
                    monthly_mean_var_370_UKESM1[[k,4]] <- ssp[q] 
                    monthly_mean_var_370_UKESM1[[k,5]] <- model
                    yearly_mean_var_370_UKESM1 <- monthly_mean_var_370_UKESM1 %>%
                      group_by(year(date)) %>%
                      summarize(mean_var = mean(mean_var, na.rm=TRUE)) %>%
                      mutate(variable = nc_var) %>%
                      mutate(ssp = ssp[q]) %>%
                      mutate(model = model) 
                    colnames(yearly_mean_var_370_UKESM1) <- c("year","mean_var","variable",  "ssp", "model" )
                    
                  } else if (ssp[q] == "ssp585" & model == "UKESM1-ESM2-0"){
                    monthly_mean_var_585_UKESM1[k,1] = as.POSIXct(date[[k]])
                    var_r_antarctica_vals <- getValues(var_r_antarctica[[k]])
                    monthly_mean_var_585_UKESM1[[k,2]] <- nc_var
                    mean_var <-mean(var_r_antarctica_vals, na.rm =T)
                    monthly_mean_var_585_UKESM1[[k,3]] <- mean(mean_var, na.rm = T)
                    monthly_mean_var_585_UKESM1[[k,4]] <- ssp[q] 
                    monthly_mean_var_585_UKESM1[[k,5]] <- model
                    yearly_mean_var_585_UKESM1 <- monthly_mean_var_585_UKESM1 %>%
                      group_by(year(date)) %>%
                      summarize(mean_var = mean(mean_var, na.rm=TRUE)) %>%
                      mutate(variable = nc_var) %>%
                      mutate(ssp = ssp[q]) %>%
                      mutate(model = model) 
                    colnames(yearly_mean_var_585_UKESM1) <- c("year","mean_var","variable",  "ssp", "model" )
                    
            }
          }
        }
      }
                    
 # Comment MRI line when processing other variables than thetao or tos data                   
   conditions_mon_df <- rbind(monthly_mean_var_126_GFDL, monthly_mean_var_370_GFDL, monthly_mean_var_585_GFDL,
                             monthly_mean_var_126_IPSL, monthly_mean_var_370_IPSL, monthly_mean_var_585_IPSL,
                             monthly_mean_var_126_MPI, monthly_mean_var_370_MPI, monthly_mean_var_585_MPI,
                             # monthly_mean_var_126_MRI, monthly_mean_var_370_MRI, monthly_mean_var_585_MRI,
                             monthly_mean_var_126_UKESM1, monthly_mean_var_370_UKESM1, monthly_mean_var_585_UKESM1
                             )

      
  conditions_year_df <- rbind(yearly_mean_var_126_GFDL, yearly_mean_var_370_GFDL, yearly_mean_var_585_GFDL,
                              yearly_mean_var_126_IPSL, yearly_mean_var_370_IPSL, yearly_mean_var_585_IPSL,
                              yearly_mean_var_126_MPI, yearly_mean_var_370_MPI, yearly_mean_var_585_MPI,
                              # yearly_mean_var_126_MRI, yearly_mean_var_370_MRI, yearly_mean_var_585_MRI,
                              yearly_mean_var_126_UKESM1, yearly_mean_var_370_UKESM1, yearly_mean_var_585_UKESM1
  )
  
  
  
# write.csv(conditions_mon_df, "D:/ISIMIP_netCDF/output/monthly_conditions-90-20-80-40.csv")
# write.csv(conditions_year_df, "D:/ISIMIP_netCDF/output/yearly_conditions-90-20-80-40.csv")


library(ggplot2)
library(lubridate)
library(fishualize)

theme2Review<-theme_bw()+theme(axis.text.x=element_text(size=16),
                               axis.text.y = element_text(size=16),
                               axis.title.y=element_text(size=19),
                               axis.title.x = element_text(size=19),
                               plot.title = element_text(size=18),
                               legend.text = element_text(size=11),
                               legend.title = element_text(face="bold", size=13),
                               legend.key.size = unit(0.9, "cm"),
                               legend.background = element_rect(colour ="black", size=0.5, linetype = "blank"),
                               legend.direction = "vertical",
                               legend.position = "right",
                               plot.margin = margin(2, 2, 2, 2, "cm"), strip.background =element_rect(fill="gray59"),
                               strip.text = element_blank(),
                               strip.placement = "right",
                               panel.background = element_blank(),
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(), 
                               panel.border = element_rect(colour = "black", fill = NA, size = 2))

# Palette for allmodels_year plot when not working with tos or thetao
my_colors <- subset(fishualize::fishcolors, fishualize::fishcolors == "Koumansetta_rainfordi")
my_colors <-my_colors[c(1,2,3,5),]

# Plot points and path along them of the obtained mean
allmodels_year <- ggplot(conditions_year_df, aes(x=year, y= mean_var, group=interaction(model, ssp), colour = model)) +
  geom_smooth(method = "loess", span = 0.06, se=F)+
  xlab("\nYear")+
  ylab("mean Sea Surface Temperature (ºC)\n")+ # tos
  # ylab("mean Sea Ice Concentration (%)\n")+ # siconc
  # ylab("mean Sea Surface Salinity (psu)\n")+
  # ylab("mean Chlorophyll in sea water (Kg/m3)\n")+
  # ylab("mean surface pH\n")+
  scale_colour_manual(values = my_colors$hex)+
  # scale_color_fish_d(option ="Koumansetta_rainfordi")+
  labs(title = "All ISIMIP models")+
  # scale_y_continuous(limits = c(25, 31)) + # tos wafrica
  # scale_y_continuous(limits = c(3, 7)) + # tos
  # scale_y_continuous(limits = c(5, 30)) + # siconc
  # scale_y_continuous(limits = c(33.4, 34)) + # so-surf
  theme2Review 
allmodels_year



gfdl_year <- ggplot(subset(conditions_year_df, model=="GFDL-ESM4"), aes(x=year, y= mean_var, group=interaction(model, ssp), colour = ssp)) +
  geom_smooth(method = "loess", span = 0.06, se=F)+
  xlab("\nYear")+
  ylab("mean Sea Surface Temperature (ºC)\n")+
  # ylab("mean Sea Ice Concentration (%)\n")+
  # ylab("mean Sea Surface Salinity (psu)\n")+
  ylab("mean Chlorophyll in sea water (Kg/m3)\n")+
  # ylab("mean surface pH\n")+
  scale_color_fish_d(option ="Hypsypops_rubicundus")+
  labs(title = "GFDL-ESM4")+
  # scale_y_continuous(limits = c(25, 31)) + # tos wafrica
  # scale_y_continuous(limits = c(3, 7)) + #tos
  # scale_y_continuous(limits = c(5, 30)) + # siconc
  # scale_y_continuous(limits = c(33.4, 34)) + # so-surf
  theme2Review 
gfdl_year

# ipsl_year <- ggplot(subset(conditions_year_df, model=="IPSL-CM6A-LR"), aes(x=year, y= mean_var, group=interaction(model, ssp), colour = ssp)) +
# when running chl for IPSL
ipsl_year <- ggplot(subset(conditions_year_df, model=="IPSL-CM6A-LR"), aes(x=year, y= mean_var*0.001 , group=interaction(model, ssp), colour = ssp)) +
  
  geom_smooth(method = "loess", span = 0.06, se=F)+
  xlab("\nYear")+
  # ylab("mean Sea Surface Temperature (ºC)\n")+
  # ylab("mean Sea Ice Concentration (%)\n")+
  # ylab("mean Sea Surface Salinity (psu)\n")+
  ylab("mean Chlorophyll in sea water (Kg/m3)\n")+
  # ylab("mean surface pH\n")+
  scale_color_fish_d(option ="Hypsypops_rubicundus")+
  labs(title = "IPSL-CM6A-LR")+
  # scale_y_continuous(limits = c(25, 31)) + # tos wafrica
  # scale_y_continuous(limits = c(3, 7)) +
  # scale_y_continuous(limits = c(5, 30)) + # siconc
  # scale_y_continuous(limits = c(33.4, 34)) + # so-surf
  theme2Review 
ipsl_year

mpi_year <- ggplot(subset(conditions_year_df, model=="MPI-ESM1-2-HR"), aes(x=year, y= mean_var, group=interaction(model, ssp), colour = ssp)) +
  geom_smooth(method = "loess", span = 0.06, se=F)+
  xlab("\nYear")+
  # ylab("mean Sea Surface Temperature (ºC)\n")+
  # ylab("mean Sea Ice Concentration (%)\n")+
  # ylab("mean Sea Surface Salinity (psu)\n")+
  ylab("mean Chlorophyll in sea water (Kg/m3)\n")+
  # ylab("mean surface pH\n")+
  scale_color_fish_d(option ="Hypsypops_rubicundus")+
  labs(title = "MPI-ESM1-2-HR")+
  # scale_y_continuous(limits = c(25, 31)) + # tos wafrica
  # scale_y_continuous(limits = c(3, 7)) + # tos
  # scale_y_continuous(limits = c(5, 30)) + # siconc
  # scale_y_continuous(limits = c(33.4, 34)) + # so-surf
  theme2Review 
mpi_year

mri_year <- ggplot(subset(conditions_year_df, model=="MRI-ESM2-0"), aes(x=year, y= mean_var, group=interaction(model, ssp), colour = ssp)) +
  geom_smooth(method = "loess", span = 0.06, se=F)+
  xlab("\nYear")+
  ylab("mean Sea Surface Temperature (ºC)\n")+
  # ylab("mean Sea Ice Concentration (%)\n")+
  # ylab("mean Sea Surface Salinity (psu)\n")+
  scale_color_fish_d(option ="Hypsypops_rubicundus")+
  labs(title = "MRI-ESM2-0")+
  # scale_y_continuous(limits = c(3, 7)) + 
  theme2Review 
mri_year


ukesm1_year <- ggplot(subset(conditions_year_df, model=="UKESM1-ESM2-0"), aes(x=year, y= mean_var, group=interaction(model, ssp), colour = ssp)) +
  geom_smooth(method = "loess", span = 0.06, se=F)+
  xlab("\nYear")+
  # ylab("mean Sea Surface Temperature (ºC)\n")+
  # ylab("mean Sea Ice Concentration (%)\n")+
  # ylab("mean Sea Surface Salinity (psu)\n")+
  ylab("mean Chlorophyll in sea water (Kg/m3)\n")+
  # ylab("mean surface pH\n")+
  scale_color_fish_d(option ="Hypsypops_rubicundus")+
  labs(title = "UKESM1-ESM2-0")+
  # scale_y_continuous(limits = c(25, 31)) + # tos wafrica
  # scale_y_continuous(limits = c(3, 7)) + # tos
  # scale_y_continuous(limits = c(5, 30)) + # siconc
  # scale_y_continuous(limits = c(33.4, 34)) + # so-surf
  theme2Review 
ukesm1_year


# list_models_year <- list(allmodels_year, gfdl_year, ipsl_year, mpi_year, mri_year, ukesm1_year)

# use this one when working with other variables than tos or thetao
list_models_year <- list(allmodels_year, gfdl_year, ipsl_year, mpi_year, ukesm1_year)


library(gridExtra)
ggsave(
  filename = here::here("00output/00enviro/04trends/trends_chl_WestAfrica.pdf"),
  plot = gridExtra::marrangeGrob(list_models_year, nrow=5, ncol=1),
  width = 13, height = 36, limitsize = F
)

# ggsave(
#   filename = "trends_ph.pdf", 
#   plot = gridExtra::marrangeGrob(list_models_year, nrow=2, ncol=3), 
#   width = 20, height = 20, limitsize = F
# )




levels(factor(conditions_year_df$model))

chl_monthly <- ggplot(conditions_mon_df, aes(x=conditions_mon_df$date, y= conditions_mon_df$mean_var)) +
  geom_point(aes(group=year(date))) + geom_path()+scale_x_date(labels = scales::date_format("%m-%Y"))+
  xlab("\nMonth-Year")+
  ylab(expression(paste("Total Chlorophyll ", (mg/m^{3}), "\n\n")))+
  theme2Review




