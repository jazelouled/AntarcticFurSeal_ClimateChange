#---------------------------------------------------------------------------------------------------
# predict_brt          Predict BRT
#---------------------------------------------------------------------------------------------------
mod_code <- "brt"
cores <- 5
bootstrap <- T

source("setup.R")
source("scr/fun_habitat_plot.R")
#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------

# Create dates
output_data <- here::here("InputOutput/00output/")
sp_code <- "GAZ"
stack_repo <- stack_repo
sp_name <- "A. gazella"
sexseason_ <- c("FS", "MW", "FW")
sexseason_ <- c("FScpf")

cols <- hcl.colors(100)
cols <- cols[15:100]


for (k in 1:length(sexseason_)){
      print(k)
      sexseason <- sexseason_[k]
      dates <- seq.Date(as.Date("2006-01-01"), as.Date("2100-12-01"), by="month")  # Define sequence
      ssp_folders<- dir(stack_repo, recursive = F, full.names = T )
      ssp <- gsub('.*/ ?(\\w+)', '\\1', ssp_folders)
    
            if (sexseason == "FS"){
              ssp <- ssp
              dates_ <- dates[month(dates) == 12 | month(dates) == 1 | month(dates) == 2 | month(dates) == 3]
              dates_ <- dates_[year(dates_) >= 2014]
              dates_hist <- dates_[dates_ <= "2014-12-01"]
              dates_ssp <- dates_[dates_ > "2014-12-01"]

            } else if (sexseason == "FW"){
              ssp <- ssp
              dates_ <- dates[year(dates) >= 2006]
              dates_hist <- dates_[dates_ <= "2014-12-01"]
              dates_ssp <- dates_[dates_ > "2014-12-01"]
              
            } else if (sexseason == "FScpf"){
              ssp <- ssp
              dates_ <- dates[month(dates) == 12 | month(dates) == 1 | month(dates) == 2 | month(dates) == 3]
              dates_ <- dates_[year(dates_) >= 2014]
              dates_hist <- dates_[dates_ <= "2014-12-01"]
              dates_ssp <- dates_[dates_ > "2014-12-01"]
              
              
            } else if(sexseason == "MW"){
              ssp <- ssp[2:4]
              dates_ <- dates[month(dates) == 2 | month(dates) == 3 | month(dates) == 4 | month(dates) == 5 | month(dates) == 6| month(dates) == 7| month(dates) == 8| month(dates) == 9]
              dates_ <- dates_[year(dates_) >= 2019]
              dates_hist <- dates_[dates_ <= "2014-12-01"]
              dates_ssp <- dates_[dates_ > "2014-12-01"]
              
            }
      

                for (q in 1:length(ssp)){
                      print(q)
                      ssp_ <- ssp[q]
                      brtDir <- paste0(output_data, "02habitatModel/", sexseason, "/BoostedRegressionTrees/", "full_model/bootstrap", sep ="/")
                      accessDir <- paste0(output_data, "02habitatModel/", sexseason, "/BoostedRegressionTrees/", "full_model/accessibilityModel_SeaIce", sep ="/")
                      outdir <-  paste0(output_data, "02habitatModel/", sexseason, "/BoostedRegressionTrees/", "full_model/predictions/", ssp[q], sep ="/") 
                      # outdir <-  paste0(output_data, "02habitatModel/", sexseason, "/BoostedRegressionTrees/", "full_model/predictions/NoAcessColonyDistance", ssp[q], sep ="/") # path for running predictions without using d2col accessibility model. Only ice. For FScpf non-philopatric scenario
                      if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
                        
                      world <- ne_countries(scale = "medium", returnclass = "sp") # Import landmask
                    # land <- raster::crop(world, raster::extent(-90, -20, -80, -50))
                      if(sexseason == "FS" | sexseason == "MW" | sexseason == "FScpf" ){
                        
                      land <- raster::crop(world, raster::extent(-90, -20, -80, -40))
                      
                      } else if (sexseason == "FW"){
                        
                        land <- raster::crop(world, raster::extent(-180, 16.884, -83.606, -29.243))}
                        
                      # List of bootstrap models
                        outdir_bootstrap <- paste0(brtDir, sep ="/")
                        boots_files <- list.files(outdir_bootstrap, full.names = T, pattern = ".rds")
                        
                      # Batch import of bootstrap models
                        brt_models <- lapply(boots_files, readRDS)
                        
                      # List of accessibility models
                        access_files <- list.files(accessDir, full.names = TRUE, pattern = ".rds")
                        
                      # Batch import of bootstrap models
                        access_models <- lapply(access_files, readRDS)
                    
                      # Prepare cluster
                        cl <- makeCluster(cores)
                        registerDoParallel(cl)
                    
                      # Get time information
                        if (ssp_ == "histor"){
                          
                          dates_loop <- dates_hist
                          
                        } else if (ssp_ %like% "ssp") {
                          
                          dates_loop <- dates_ssp
                          
                        }
                        
    
                                foreach(i=1:length(dates_loop), .packages=c("lubridate", "raster", "stringr", "dplyr", "pals", "dismo", "gbm", "scam", "data.table")) %dopar% {
                                            date <- dates_loop[i]
                                            YYYY <- year(date)
                                            MM <- sprintf("%02d", month(date))
                                            
                                          # Locate file
                                            pat <- format(as.Date(date), "%Y%m")
                                            pattern1 = c(pat,".grd")
                                            grdfile <- list.files(stack_repo, recursive = TRUE, full.names = TRUE, pattern = pattern1)
                                            grdfile <- grdfile[grdfile %like% ".grd" & grdfile %like% ssp_]
                                
                                          # Import environmental stack
                                            s <- raster::stack(grdfile)
                                            s <- s+0
                                
                                            names(s) <- ifelse(names(s) %like% "BAT", "BAT",
                                                        ifelse(names(s) %like% "SLP", "SLP",
                                                        ifelse(names(s) %like% "SDIST", "SDIST",
                                                        ifelse(names(s) %like% "D2COL_DEC", "D2COL_DEC",
                                                        ifelse(names(s) %like% "D2COL_CSH", "D2COL_CSH",
                                                        ifelse(names(s) %like% "CHL", "CHL",
                                                        ifelse(names(s) %like% "EDGE", "EDGE",
                                                        ifelse(names(s) %like% "EKE", "EKE",
                                                        ifelse(names(s) %like% "MLD", "MLD",
                                                        ifelse(names(s) %like% "O2", "O2",
                                                        ifelse(names(s) %like% "DIAT", "DIAT",
                                                        ifelse(names(s) %like% "SIC", "SIC",
                                                        ifelse(names(s) %like% "SALg", "SALg",
                                                        ifelse(names(s) %like% "SAL", "SAL",
                                                        ifelse(names(s) %like% "THETAO", "THETAO",
                                                        ifelse(names(s) %like% "SSTg", "SSTg",
                                                        ifelse(names(s) %like% "SST", "SST",
                                                        ifelse(names(s) %like% "UO", "UO",
                                                        ifelse(names(s) %like% "VO", "VO", "")))))))))))))))))))
                                
                                            # Transform variables
                                              s$EDGE[s$EDGE < 0] <- 0
                                            
                                            # Ice mask
                                              iceMask <- reclassify(s$SIC, c(-Inf,0.15,1, 0.15,Inf,0))
                                            
                                            # d2col mask. Only for FS and FW!!! I COMMENTED IT TO RUN FS CPF. UNCOMMENT FOR OTHER CASES!!!
                                            # if(sexseason == "FS" | sexseason == "FW" | sexseason == "FScpf"){
                                              
                                            # d2col <- raster(paste(output_data, "habitat-model-cc", sp_code, sexseason, mod_code, "full_model/access_d2col_boost/GAZd2col_access.grd", sep = "/"))
                                            
                                              # }
                                              
                                              
                                              d2col <- 1
                                
                                            # Model prediction (BRT)
                                              stack_list <- list()
                                    
                                                  for(j in 1:length(brt_models)){  
                                                    print(j)
                                                    # predict BRT
                                                    
                                                    # Run these lines if errors saying that predict cannot be used on gbm objects appear
                                                    # library(gbm)
                                                    # pred_brt <- predict.gbm(brt_models[[j]], s, n.trees=brt_models[[j]]$gbm.call$best.trees, type="response")
                                                      pred_brt <- raster::predict(model = brt_models[[j]], object = s, n.trees=brt_models[[j]]$gbm.call$best.trees, type="response")
                                                    
                                                   
                                                   
                                                    # Run this line if errors saying "Error in x$qr %||% stop..." appear
                                                    # library(scam)
                                                    
                                                    # Predict accessibility
                                                      pred_access <- raster::predict(object = s, model = access_models[[j]], type="response")
                          
                                                      if(sexseason == "MW"){
                                                         pred_access <- pred_access * iceMask } else { pred_access <- pred_access * iceMask * d2col }
                                                    
                                                    # Combined prediction
                                                      pred <- pred_brt * pred_access
                                                      stack_list[[j]] <- pred

                                                                                    }
                                      
                                          # create stack from list
                                          pred_stack <- raster::stack(stack_list)
                                          
                                          # Average predictions
                                          pred_med <- raster::calc(pred_stack, median)
                                        
                                          #confidence interval 95% range 
                                          pred_cil <- raster::calc(pred_stack, fun = function(x){quantile(x, probs = c(0.025),na.rm=TRUE)})
                                          pred_ciu <- raster::calc(pred_stack, fun = function(x){quantile(x, probs = c(0.975),na.rm=TRUE)})
                                          pred_cir <- pred_ciu - pred_cil
                                          
                                          # set/create folder
                                          # product_folder <- paste(outdir, YYYY, MM, sep="/")  # Set folder
                                          # if (!dir.exists(product_folder)) dir.create(product_folder, recursive = TRUE)  # create output directory if does not exist
                                          
                                          product_folder <- paste0(outdir, sep="/", YYYY)
                                          if (!dir.exists(product_folder)) dir.create(product_folder, recursive = TRUE) 
                                          
                                          rgdal::setCPLConfigOption("GDAL_PAM_ENABLED", "FALSE")
                                          
                                          sp_name <- "A. gazella"
                                          
                                          sp_code <- "GAZ"
                                          # store file
                                          outfile <- paste0(product_folder, "/", format(date, "%Y%m%d"),"_", sp_code, "_", mod_code, "_", ssp_,  "_pred.tif")
                                          writeRaster(pred_med, filename=outfile, overwrite=TRUE)
                                        
                                          # store file
                                          outfile <- paste0(product_folder, "/", format(date, "%Y%m%d"),"_", sp_code, "_", mod_code,"_", ssp_, "_pred_cir.tif")
                                          writeRaster(pred_cir, filename=outfile, overwrite=TRUE)
                                          
                                          # export plot
                                          pngfile <- paste0(product_folder, "/", format(date, "%Y%m%d"),"_", sp_code, "_", mod_code,"_", ssp_,  "_pred.png")
                                          png(pngfile, width=560, height=600, res=100)
                                          pred_med <- crop(pred_med, extent(land))
                                          plot(pred_med, main = paste(sp_name, "   Model:", mod_code, "\n", date), zlim=c(0,1), col = cols)
                                          plot(land, col="grey80", border="gray34", add=TRUE)
                                          text(x = -3.5, y = 44, labels = date)
                                          box()
                                          dev.off()
                                          
                                          # export plot
                                          pngfile <- paste0(product_folder, "/", format(date, "%Y%m%d"),"_", sp_code, "_", mod_code, "_", ssp_, "_pred_cir.png")
                                          png(pngfile, width=560, height=600, res=100)
                                          pred_cir <- crop(pred_cir, extent(land))
                                          # plot(pred_cir, main = paste(sp_name, "   Model:", mod_code, "\n", date), zlim=c(0,0.3), col = viridis(100))
                                          plot(pred_cir, main = paste(sp_name, "   Model:", mod_code, "\n", date), zlim=c(0,1), col = cols) # FScpf
                                          # plot(pred_cir, main = paste(sp_name, "   Model:", mod_code, "\n", date), zlim=c(0,0.6), col = cols)
                                          plot(land, col="grey80", border="gray34", add=TRUE)
                                          text(x = -3.5, y = 44, labels = date)
                                          box()
                                          dev.off()
                                      
                                
                                      }
                                
                                 }

                           }



# cols <- c('#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#253494','#081d58')
# cols <- c('#081d58', '#253494','#225ea8', '#1d91c0', '#41b6c4', '#7fcdbb','#c7e9b4', '#edf8b1', '#ffffd9')
# cols <-rev(colorRampPalette(brewer.pal(9,"Blues"))(100))
# cols <- c(cols, '#ffffd9')

#---------------------------------------------------------------
# Stop cluster
#---------------------------------------------------------------
stopCluster(cl)

print("Prediction ready")  

