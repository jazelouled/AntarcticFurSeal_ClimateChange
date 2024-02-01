library(ggplot2)
library(sf)
library(ggallin)
# Functions -------------------------------------------------------------------

fun_slope <- function(y) { 
  if(all(is.na(y))) {
    NA
  } else {
    m = lm(y ~ time_section); summary(m)$coefficients[2] 
  }
}



fun_pvalue <- function(y) { 
  if(all(is.na(y))) {
    NA
  } else {
    m = lm(y ~ time_section); summary(m)$coefficients[8] 
  }
}


flattenlist <- function(x){  
  morelists <- sapply(x, function(xprime) {
    'list' %in% class(xprime) & !('gg' %in% class(xprime))
  })
  out <- c(x[!morelists], unlist(x[morelists], recursive=FALSE))
  if(sum(morelists)){ 
    Recall(out)
  }else{
    return(out)
  }
}


# Get world shapefile from naturalEarth
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")



themeMapsCC<-theme_bw()+
  theme(axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.y=element_text(size=12),
        axis.title.x = element_text(size=12),
        legend.text = element_text(size=8),
        legend.title = element_text(face="bold", size=10),
        legend.key.size = unit(0.5, "cm"),
        # legend.background = element_rect(colour ="black", size=0.5, linetype = "dotted"),
        legend.direction = "vertical",
        legend.position = "right",
        plot.margin = margin(0, 0, 0, 0, "cm"), strip.background =element_rect(fill="gray59"),
        strip.text = element_blank(),
        strip.placement = "right",
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


dates <- seq(1,1032, by=120) # every 10 years (120 months)
dates <- seq(1,1032, by=300) # every 25 years (300 months)
dates <- seq(1,1032, by=600) # every 50 years (120 months)
# dates <- seq(1,1032, by=516) # 1/3 period
dates <- seq(1,1032, by=516) # half period
dates <- seq(1,1032, by=1032) # all period


myplots_j <- list()  # new empty list
myplots <- list()  # new empty list

for (i in seq_along(bricks_list)){
  print(i)
  brick <- bricks_list[[i]]
  
  
  for (j in seq_along(dates)){
       print(j)
       start<-dates[j]
   #  # 10 years sections
   #     if (dates[j] < 961){
   #     end<-dates[j]+119
   #     } else {end<-dates[j]+71}
   # #      
   # # 25 years sections
   #     if (dates[j] < 901){
   #       end<-dates[j]+299
   #     } else {end<-dates[j]+131}

   # # 50 years sections  
   #     if (dates[j] < 601){
   #       end<-dates[j]+599
   #     } else {end<-dates[j]+431}
   #     
   # # half and half study period 
       if (dates[j] <= 516){
         end<-516
       } else {end<-1032}

       
         # all period 
        # end <- 1032
        
        
        time_section<-seq(start, end)  
        time_section_brick <- brick[[start:end]]
        brick_slope <- calc(time_section_brick, fun_slope)
        # brick_slope <- setNames(brick[[j]], as.character(names(bricks_list[j])))
        brick_slope_df <- as.data.frame(brick_slope, xy = T)
        brick_slope_df$name <- names(bricks_list[j])
        brick_pvalue <- calc(time_section_brick,fun_pvalue)
        brick_pvalue_df <- as.data.frame(brick_pvalue, xy = T)
        brick_pvalue_df$name <- names(bricks_list[j])
        brick_mean <- calc(time_section_brick,mean)
        brick_mean_df <- as.data.frame(brick_mean, xy = T)
        brick_mean_df$name <- names(bricks_list[j])
        brick_sd <- calc(time_section_brick,sd)
        brick_sd_df <- as.data.frame(brick_sd, xy = T)
        brick_sd_df$name <- names(bricks_list[j])
        

        
min_max_df <-data.frame(min = numeric(), max = numeric())
for (q in 1:length(brick_slope)){
          # print(q)
          vals <- getValues(brick_slope)  
          min_max_df[q,1] = min(vals, na.rm = T)
          min_max_df[q,2] = max(vals, na.rm = T)
          min_max_abs <- data.frame(min(min_max_df$min), max(min_max_df$max))
          colnames(min_max_abs) <- c("min", "max")
        }
        

 # min_max_df <-data.frame(min = numeric(), max = numeric())
 #  for (q in seq_along(bricks_list)){
 #    # print(q)
 #    # vals <- getValues(bricks_list[q])
 #    min_max_df[q,1] = min(minValue(bricks_list[[q]]))
 #    min_max_df[q,2] = max(maxValue(bricks_list[[q]]))
 #    min_max_abs <- data.frame(min(min_max_df$min), max(min_max_df$max))
 #    colnames(min_max_abs) <- c("min", "max")
 #  }

# aquí ajustant l'escala per la salinitat
    ww <-ggplot()+
    geom_raster(aes(x=x,y=y, fill=layer),data=brick_slope_df)+
    # geom_raster(aes(x=x,y=y, fill=layer),data=brick_pvalue_df)+
    # geom_raster(aes(x=x,y=y, fill=layer),data=brick_mean_df)+
    # geom_raster(aes(x=x,y=y, fill=layer),data=brick_sd_df)+
      geom_sf(fill='grey',data=world)+
    # lims(x = c(-90, -20), y = c(-80, -40))+
    scale_x_continuous(limits = c(-89.25139, -20.44306), expand=c(0,0), breaks = seq(-80, -20, by = 15)) +
    scale_y_continuous(limits = c(-79.05833, -40.275),expand=c(0,0)) +
    # coord_sf(xlim = c(-90, -20), ylim = c(-80, -40), expand=T)+
    # scale_fill_viridis_c('Slope', direction = 1, option = "viridis", na.value="white", limits = c(min_max_abs$min, min_max_abs$max))+
    # scale_fill_viridis_c('Slope', direction = 1, option = "viridis", na.value="white", limits = c(-0.2, 0.05))+ # slope100 tos
    # scale_fill_viridis_c('Slope', direction = 1, option = "viridis", na.value="white", limits = c(-0.015,0.017))+ # slope10 tos
    # scale_fill_viridis_c('Slope', direction = 1, option = "viridis", na.value="white", limits = c(-0.0045,0.015))+ # slopehalf tos
    # scale_fill_viridis_c('Slope', direction = 1, option = "viridis", na.value="white", limits = c(-0.12,0.1))+ # slopehalf siconc
      # scale_fill_viridis_c('Slope', direction = 1, option = "viridis", na.value="white", limits = c(-0.01,0.01))+ # slopehalf siconc
      scale_fill_viridis_c('Slope', direction = 1, option = "viridis", na.value="white", limits = c(-0.00000000090,0.00000000075))+ # slopehalf chl
      
      
    # scale_fill_viridis_c('p-value', direction = 1, option = "viridis", na.value="white", limits = c(-0.2, 0.05))+ # p-value tos
    # scale_fill_viridis_c('mean', direction = 1, option = "viridis", na.value="white", limits = c(-1, 25))+ # mean tos
    # scale_fill_viridis_c('SD', direction = 1, option = "viridis", na.value="white", limits = c(0, 6))+ # SD tos
    labs(x='Longitude\n',y='Latitude')+
    themeMapsCC
    
          myplots_j[[j]] <- ww 
          myplots[[i]] <- list(myplots_j)
  }
 }
 



# trans =pseudolog10_trans

myplots_f=flattenlist(myplots)#large list of 234 elements?
myplots_f=flattenlist(myplots_f)


subtitles1<- c(paste0('GFDL-ESM4. SSP126 (2015-2100)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP370 (2015-2100)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP585 (2015-2100)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP126 (2015-2100)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP370 (2015-2100)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP585 (2015-2100)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP126 (2015-2100)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP370 (2015-2100)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP585 (2015-2100)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP126 (2015-2100)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP370 (2015-2100)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP585 (2015-2100)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP126 (2015-2100)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP370 (2015-2100)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP585 (2015-2100)', sep=" ", nc_var))
                
                
                
subtitles43<- c(paste0('GFDL-ESM4. SSP126 (2015-2058)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP126 (2059-2100)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP370 (2015-2058)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP370 (2059-2100)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP585 (2015-2058)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP585 (2059-2100)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP126 (2015-2058)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP126 (2059-2100)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP370 (2015-2058)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP370 (2059-2100)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP585 (2015-2058)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP585 (2059-2100)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP126 (2015-2058)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP126 (2059-2100)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP370 (2015-2058)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP370 (2059-2100)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP585 (2015-2058)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP585 (2059-2100)', sep=" ", nc_var),
                # paste0('MRI-ESM2-0. SSP126 (2015-2058)', sep=" ", nc_var),
                # paste0('MRI-ESM2-0. SSP126 (2059-2100)', sep=" ", nc_var),
                # paste0('MRI-ESM2-0. SSP370 (2015-2058)', sep=" ", nc_var),
                # paste0('MRI-ESM2-0. SSP370 (2059-2100)', sep=" ", nc_var),
                # paste0('MRI-ESM2-0. SSP585 (2015-2058)', sep=" ", nc_var),
                # paste0('MRI-ESM2-0. SSP585 (2059-2100)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP126 (2015-2058)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP126 (2059-2100)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP370 (2015-2058)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP370 (2059-2100)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP585 (2015-2058)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP585 (2059-2100)', sep=" ", nc_var))
                
                
                
                
                
                






subtitles10<- c(paste0('GFDL-ESM4. SSP126 (2015-2024)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP126 (2025-2034)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP126 (2035-2044)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP126 (2045-2054)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP126 (2055-2064)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP126 (2065-2074)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP126 (2075-2084)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP126 (2085-2095)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP126 (2095-2100)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP370 (2015-2024)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP370 (2025-2034)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP370 (2035-2044)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP370 (2045-2054)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP370 (2055-2064)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP370 (2065-2074)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP370 (2075-2084)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP370 (2085-2095)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP370 (2095-2100)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP585 (2015-2024)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP585 (2025-2034)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP585 (2035-2044)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP585 (2045-2054)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP585 (2055-2064)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP585 (2065-2074)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP585 (2075-2084)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP585 (2085-2095)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP585 (2095-2100)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP126 (2015-2024)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP126 (2025-2034)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP126 (2035-2044)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP126 (2045-2054)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP126 (2055-2064)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP126 (2065-2074)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP126 (2075-2084)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP126 (2085-2095)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP126 (2095-2100)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP370 (2015-2024)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP370 (2025-2034)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP370 (2035-2044)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP370 (2045-2054)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP370 (2055-2064)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP370 (2065-2074)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP370 (2075-2084)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP370 (2085-2095)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP370 (2095-2100)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP585 (2015-2024)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP585 (2025-2034)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP585 (2035-2044)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP585 (2045-2054)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP585 (2055-2064)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP585 (2065-2074)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP585 (2075-2084)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP585 (2085-2095)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP585 (2095-2100)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP126 (2015-2024)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP126 (2025-2034)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP126 (2035-2044)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP126 (2045-2054)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP126 (2055-2064)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP126 (2065-2074)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP126 (2075-2084)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP126 (2085-2095)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP126 (2095-2100)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP370 (2015-2024)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP370 (2025-2034)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP370 (2035-2044)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP370 (2045-2054)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP370 (2055-2064)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP370 (2065-2074)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP370 (2075-2084)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP370 (2085-2095)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP370 (2095-2100)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP585 (2015-2024)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP585 (2025-2034)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP585 (2035-2044)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP585 (2045-2054)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP585 (2055-2064)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP585 (2065-2074)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP585 (2075-2084)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP585 (2085-2095)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP585 (2095-2100)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP126 (2015-2024)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP126 (2025-2034)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP126 (2035-2044)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP126 (2045-2054)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP126 (2055-2064)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP126 (2065-2074)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP126 (2075-2084)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP126 (2085-2095)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP126 (2095-2100)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP370 (2015-2024)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP370 (2025-2034)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP370 (2035-2044)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP370 (2045-2054)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP370 (2055-2064)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP370 (2065-2074)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP370 (2075-2084)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP370 (2085-2095)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP370 (2095-2100)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP585 (2015-2024)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP585 (2025-2034)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP585 (2035-2044)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP585 (2045-2054)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP585 (2055-2064)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP585 (2065-2074)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP585 (2075-2084)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP585 (2085-2095)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP585 (2095-2100)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP126 (2015-2024)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP126 (2025-2034)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP126 (2035-2044)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP126 (2045-2054)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP126 (2055-2064)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP126 (2065-2074)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP126 (2075-2084)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP126 (2085-2095)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP126 (2095-2100)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP370 (2015-2024)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP370 (2025-2034)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP370 (2035-2044)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP370 (2045-2054)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP370 (2055-2064)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP370 (2065-2074)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP370 (2075-2084)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP370 (2085-2095)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP370 (2095-2100)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP585 (2015-2024)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP585 (2025-2034)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP585 (2035-2044)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP585 (2045-2054)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP585 (2055-2064)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP585 (2065-2074)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP585 (2075-2084)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP585 (2085-2095)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP585 (2095-2100)', sep=" ", nc_var))





subtitles25<- c(paste0('GFDL-ESM4. SSP126 (2015-2040)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP126 (2041-2065)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP126 (2066-2090)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP126 (2091-2100)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP370 (2015-2040)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP370 (2041-2065)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP370 (2066-2090)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP370 (2091-2100)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP585 (2015-2040)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP585 (2041-2065)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP585 (2066-2090)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP585 (2091-2100)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP126 (2015-2040)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP126 (2041-2065)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP126 (2066-2090)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP126 (2091-2100)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP370 (2015-2040)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP370 (2041-2065)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP370 (2066-2090)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP370 (2091-2100)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP585 (2015-2040)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP585 (2041-2065)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP585 (2066-2090)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP585 (2091-2100)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP126 (2015-2040)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP126 (2041-2065)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP126 (2066-2090)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP126 (2091-2100)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP370 (2015-2040)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP370 (2041-2065)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP370 (2066-2090)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP370 (2091-2100)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP585 (2015-2040)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP585 (2041-2065)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP585 (2066-2090)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP585 (2091-2100)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP126 (2015-2040)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP126 (2041-2065)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP126 (2066-2090)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP126 (2091-2100)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP370 (2015-2040)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP370 (2041-2065)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP370 (2066-2090)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP370 (2091-2100)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP585 (2015-2040)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP585 (2041-2065)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP585 (2066-2090)', sep=" ", nc_var),
                paste0('MRI-ESM2-0. SSP585 (2091-2100)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP126 (2015-2040)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP126 (2041-2065)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP126 (2066-2090)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP126 (2091-2100)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP370 (2015-2040)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP370 (2041-2065)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP370 (2066-2090)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP370 (2091-2100)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP585 (2015-2040)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP585 (2041-2065)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP585 (2066-2090)', sep=" ", nc_var),
                paste0('UKESM1-ESM2-0. SSP585 (2091-2100)', sep=" ", nc_var))



subtitles50<- c(paste0('GFDL-ESM4. SSP126 (2015-2065)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP126 (2041-2065)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP126 (2066-2090)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP126 (2091-2100)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP370 (2015-2040)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP370 (2041-2065)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP370 (2066-2090)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP370 (2091-2100)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP585 (2015-2040)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP585 (2041-2065)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP585 (2066-2090)', sep=" ", nc_var),
                paste0('GFDL-ESM4. SSP585 (2091-2100)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP126 (2015-2040)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP126 (2041-2065)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP126 (2066-2090)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP126 (2091-2100)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP370 (2015-2040)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP370 (2041-2065)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP370 (2066-2090)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP370 (2091-2100)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP585 (2015-2040)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP585 (2041-2065)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP585 (2066-2090)', sep=" ", nc_var),
                paste0('IPSL-CM6A-LR. SSP585 (2091-2100)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP126 (2015-2040)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP126 (2041-2065)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP126 (2066-2090)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP126 (2091-2100)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP370 (2015-2040)', sep=" ", nc_var),
                paste0('MPI-ESM1-2-HR. SSP370 (2041-2065)', sep=" ", nc_var))
                











myplots_f_list <- list()
for (y in 1:length(myplots_f)){
      myplots_f_ <- myplots_f[[y]]+ labs(subtitle = subtitles43[y])
      myplots_f_list[[y]] <- myplots_f_ 
      # + theme(legend.position = "none")
      }



# Reorder plots to save pdf. Whole period (2015-2100)
myplots_reorder <- myplots_f_list[c(1, 4, 7 , 10, 13, 2, 5, 8, 11, 14, 3, 6, 9, 12, 15)]
myplots_reorder[[1]]

# Reorder plots to save pdf half period (2015-2058 2059-2100). TOS & THETAO
myplots_reorder2 <- myplots_f_list[c(1, 3, 5 , 2, 4, 6, 7, 9, 11, 8, 10, 12, 13, 15, 17, 14, 16,
                                     18, 19, 21, 23, 20, 22, 24, 25, 27, 29, 26, 28, 30)]



# Reorder plots to save pdf half period (2015-2058 2059-2100). All vars but TOS & THETAO
myplots_reorder2 <- myplots_f_list[c(1, 3, 5 , 2, 4, 6, 7, 9, 11, 8, 10, 12, 13, 15, 17, 14, 16,
                                     18, 19, 21, 23, 20, 22, 24)]





library(gridExtra)
ggsave(
  filename = "so-surf-trends-slope.pdf", 
  plot = gridExtra::marrangeGrob(myplots_f_list, nrow=9, ncol=3), 
  width = 13, height = 36, limitsize = F
)




ggsave(
  filename = "ttt.pdf", 
  plot = gridExtra::marrangeGrob(myplots_f_list, nrow=4, ncol=3), 
  width = 15, height = 15, limitsize = F
)


myplots_reorder2[[24]]

ggsave(
  filename = "chl-surf-trends-slope.pdf", 
  plot = gridExtra::marrangeGrob(myplots_reorder2, nrow=3, ncol=2), 
  width = 15, height = 15, limitsize = F
)

ggsave(
  filename = "tttsstytrt.pdf", 
  plot = gridExtra::marrangeGrob(myplots_reorder, nrow=10, ncol=3), 
  width = 15, height = 15, limitsize = F
)


# Reorder plots to save pdf. Whole period (2015-2100)
myplots_reorder <- myplots_f_list[c(1, 4, 7 , 10, 13, 2, 5, 8, 11, 14, 3, 6, 9, 12, 15)]
myplots_reorder[[1]]

# Reorder plots to save pdf half period (2015-2058 2059-2100). TOS & THETAO
myplots_reorder2 <- myplots_f_list[c(1, 3, 5 , 2, 4, 6, 7, 9, 11, 8, 10, 12, 13, 15, 17, 14, 16,
                                     18, 19, 21, 23, 20, 22, 24, 25, 27, 29, 26, 28, 30)]



# Reorder plots to save pdf half period (2015-2058 2059-2100). All vars but TOS & THETAO
myplots_reorder2 <- myplots_f_list[c(1, 3, 5 , 2, 4, 6, 7, 9, 11, 8, 10, 12, 13, 15, 17, 14, 16,
                                     18, 19, 21, 23, 20, 22, 24)]





















rr<-bricks_list[[1]]
   min_max_df[q,1] = min(minValue(rr))
   min_max_df[q,2] = max(maxValue(rr))
   min_max_abs <- data.frame(min(min_max_df$min), max(min_max_df$max))


myplots_f_list[[135]]
names(myplots_f)<-subtitles






for (c in 1:length(myplots_f_list)){
  plotsSequence <- seq(1,135, by=10)+c
  
  
}

library(patchwork)

multiplot(myplots_f_list)
library(Rmisc)
install.packages("Rmisc")
multiplot(plotlist = myplots_f_list, cols=5)






??marrangeGrob





















plotsSequence+
  
  myplots_f[1]



9*15

if (brick_slope_df$name[j] == "brick_126_GFDL") {brick_126_GFDL_pl_j[[j]] <- ww
brick_126_GFDL_pl[[i]] <- list(brick_126_GFDL_pl_j)
myplots_f=flattenlist(brick_126_GFDL_pl)#large list of 234 elements?
myplots_f=flattenlist(myplots_f)

} else if (brick_slope_df$name[j] == "brick_370_GFDL") {brick_370_GFDL_pl[[i]] <- ww
} else if (brick_slope_df$name[j] == "brick_585_GFDL") {brick_585_GFDL_pl[[i]] <- ww
} else if (brick_slope_df$name[j] == "brick_126_IPSL") {brick_126_IPSL_pl[[i]] <- ww
} else if (brick_slope_df$name[j] == "brick_370_IPSL") {brick_370_IPSL_pl[[i]] <- ww
} else if (brick_slope_df$name[j] == "brick_585_IPSL") {brick_585_IPSL_pl[[i]] <- ww
} else if (brick_slope_df$name[j] == "brick_126_MPI") {brick_126_MPI_pl[[i]] <- ww
} else if (brick_slope_df$name[j] == "brick_370_MPI") {brick_370_MPI_pl[[i]] <- ww
} else if (brick_slope_df$name[j] == "brick_585_MPI") {brick_585_MPI_pl[[i]] <- ww
} else if (brick_slope_df$name[j] == "brick_126_MRI") {brick_126_MRI_pl[[i]] <- ww
} else if (brick_slope_df$name[j] == "brick_370_MRI") {brick_370_MRI_pl[[i]] <- ww
} else if (brick_slope_df$name[j] == "brick_585_MRI") {brick_585_MRI_pl[[i]] <- ww
} else if (brick_slope_df$name[j] == "brick_126_UKESM1") {brick_126_UKESM1_pl[[i]] <- ww
} else if (brick_slope_df$name[j] == "brick_370_UKESM1") {brick_370_UKESM1_pl[[i]] <- ww
} else if (brick_slope_df$name[j] == "brick_585_UKESM1") {brick_585_UKESM1_pl[[i]] <- ww
}

# title="Salinity",




















source("https://bioconductor.org/biocLite.R")
biocLite("scater")

library(JLutils)

devtools::install_github("davismcc/scater", ref = "release-R-3.2", build_vignettes = TRUE)
library(scater)


devtools::install_github("larmarange/JLutils")




y=3





, subtitle = 
  if (brick_slope_df$name[j] %like% "GFDL" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 1 & dates[j] <= 120)){
    paste0('Linear regression slope. GFDL-ESM4. SSP126 (2015-2024)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "GFDL" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 121 & dates[j] <= 240)){
    paste0('Linear regression slope. GFDL-ESM4. SSP126 (2025-2034)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "GFDL" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 241 & dates[j] <= 360)){
    paste0('Linear regression slope. GFDL-ESM4. SSP126 (2035-2044)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "GFDL" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 361 & dates[j] <= 480)){
    paste0('Linear regression slope. GFDL-ESM4. SSP126 (2045-2054)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "GFDL" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 481 & dates[j] <= 600)){
    paste0('Linear regression slope. GFDL-ESM4. SSP126 (2055-2064)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "GFDL" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 601 & dates[j] <= 720)){
    paste0('Linear regression slope. GFDL-ESM4. SSP126 (2065-2074)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "GFDL" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 721 & dates[j] <= 840)){
    paste0('Linear regression slope. GFDL-ESM4. SSP126 (2075-2084)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "GFDL" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 841 & dates[j] <= 960)){
    paste0('Linear regression slope. GFDL-ESM4. SSP126 (2085-2095)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "GFDL" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 961 & dates[j] <= 1032)){
    paste0('Linear regression slope. GFDL-ESM4. SSP126 (2095-2100)', sep=" ", nc_var)
    
  } else if (brick_slope_df$name[j] %like% "GFDL" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 1 & dates[j] <= 120)){
    paste0('Linear regression slope. GFDL-ESM4. SSP370 (2015-2024)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "GFDL" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 121 & dates[j] <= 240)){
    paste0('Linear regression slope. GFDL-ESM4. SSP370 (2025-2034)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "GFDL" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 241 & dates[j] <= 360)){
    paste0('Linear regression slope. GFDL-ESM4. SSP370 (2035-2044)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "GFDL" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 361 & dates[j] <= 480)){
    paste0('Linear regression slope. GFDL-ESM4. SSP370 (2045-2054)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "GFDL" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 481 & dates[j] <= 600)){
    paste0('Linear regression slope. GFDL-ESM4. SSP370 (2055-2064)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "GFDL" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 601 & dates[j] <= 720)){
    paste0('Linear regression slope. GFDL-ESM4. SSP370 (2065-2074)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "GFDL" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 721 & dates[j] <= 840)){
    paste0('Linear regression slope. GFDL-ESM4. SSP370 (2075-2084)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "GFDL" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 841 & dates[j] <= 960)){
    paste0('Linear regression slope. GFDL-ESM4. SSP370 (2085-2095)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "GFDL" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 961 & dates[j] <= 1032)){
    paste0('Linear regression slope. GFDL-ESM4. SSP370 (2095-2100)', sep=" ", nc_var)
    
  } else if (brick_slope_df$name[j] %like% "GFDL" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 1 & dates[j] <= 120)){
    paste0('Linear regression slope. GFDL-ESM4. SSP585 (2015-2024)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "GFDL" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 121 & dates[j] <= 240)){
    paste0('Linear regression slope. GFDL-ESM4. SSP585 (2025-2034)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "GFDL" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 241 & dates[j] <= 360)){
    paste0('Linear regression slope. GFDL-ESM4. SSP585 (2035-2044)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "GFDL" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 361 & dates[j] <= 480)){
    paste0('Linear regression slope. GFDL-ESM4. SSP585 (2045-2054)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "GFDL" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 481 & dates[j] <= 600)){
    paste0('Linear regression slope. GFDL-ESM4. SSP585 (2055-2064)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "GFDL" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 601 & dates[j] <= 720)){
    paste0('Linear regression slope. GFDL-ESM4. SSP585 (2065-2074)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "GFDL" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 721 & dates[j] <= 840)){
    paste0('Linear regression slope. GFDL-ESM4. SSP585 (2075-2084)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "GFDL" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 841 & dates[j] <= 960)){
    paste0('Linear regression slope. GFDL-ESM4. SSP585 (2085-2095)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "GFDL" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 961 & dates[j] <= 1032)){
    paste0('Linear regression slope. GFDL-ESM4. SSP585 (2095-2100)', sep=" ", nc_var)
    
    
  } else if (brick_slope_df$name[j] %like% "IPSL" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 1 & dates[j] <= 120)){
    paste0('Linear regression slope. IPSL-CM6A-LR. SSP126 (2015-2024)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "IPSL" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 121 & dates[j] <= 240)){
    paste0('Linear regression slope. IPSL-CM6A-LR. SSP126 (2025-2034)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "IPSL" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 241 & dates[j] <= 360)){
    paste0('Linear regression slope. IPSL-CM6A-LR. SSP126 (2035-2044)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "IPSL" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 361 & dates[j] <= 480)){
    paste0('Linear regression slope. IPSL-CM6A-LR. SSP126 (2045-2054)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "IPSL" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 481 & dates[j] <= 600)){
    paste0('Linear regression slope. IPSL-CM6A-LR. SSP126 (2055-2064)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "IPSL" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 601 & dates[j] <= 720)){
    paste0('Linear regression slope. IPSL-CM6A-LR. SSP126 (2065-2074)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "IPSL" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 721 & dates[j] <= 840)){
    paste0('Linear regression slope. IPSL-CM6A-LR. SSP126 (2075-2084)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "IPSL" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 841 & dates[j] <= 960)){
    paste0('Linear regression slope. IPSL-CM6A-LR. SSP126 (2085-2095)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "IPSL" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 961 & dates[j] <= 1032)){
    paste0('Linear regression slope. IPSL-CM6A-LR. SSP126 (2095-2100)', sep=" ", nc_var)
    
  } else if (brick_slope_df$name[j] %like% "IPSL" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 1 & dates[j] <= 120)){
    paste0('Linear regression slope. IPSL-CM6A-LR. SSP370 (2015-2024)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "IPSL" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 121 & dates[j] <= 240)){
    paste0('Linear regression slope. IPSL-CM6A-LR. SSP370 (2025-2034)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "IPSL" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 241 & dates[j] <= 360)){
    paste0('Linear regression slope. IPSL-CM6A-LR. SSP370 (2035-2044)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "IPSL" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 361 & dates[j] <= 480)){
    paste0('Linear regression slope. IPSL-CM6A-LR. SSP370 (2045-2054)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "IPSL" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 481 & dates[j] <= 600)){
    paste0('Linear regression slope. IPSL-CM6A-LR. SSP370 (2055-2064)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "IPSL" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 601 & dates[j] <= 720)){
    paste0('Linear regression slope. IPSL-CM6A-LR. SSP370 (2065-2074)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "IPSL" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 721 & dates[j] <= 840)){
    paste0('Linear regression slope. IPSL-CM6A-LR. SSP370 (2075-2084)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "IPSL" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 841 & dates[j] <= 960)){
    paste0('Linear regression slope. IPSL-CM6A-LR. SSP370 (2085-2095)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "IPSL" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 961 & dates[j] <= 1032)){
    paste0('Linear regression slope. IPSL-CM6A-LR. SSP370 (2095-2100)', sep=" ", nc_var)
    
  } else if (brick_slope_df$name[j] %like% "IPSL" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 1 & dates[j] <= 120)){
    paste0('Linear regression slope. IPSL-CM6A-LR. SSP585 (2015-2024)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "IPSL" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 121 & dates[j] <= 240)){
    paste0('Linear regression slope. IPSL-CM6A-LR. SSP585 (2025-2034)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "IPSL" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 241 & dates[j] <= 360)){
    paste0('Linear regression slope. IPSL-CM6A-LR. SSP585 (2035-2044)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "IPSL" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 361 & dates[j] <= 480)){
    paste0('Linear regression slope. IPSL-CM6A-LR. SSP585 (2045-2054)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "IPSL" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 481 & dates[j] <= 600)){
    paste0('Linear regression slope. IPSL-CM6A-LR. SSP585 (2055-2064)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "IPSL" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 601 & dates[j] <= 720)){
    paste0('Linear regression slope. IPSL-CM6A-LR. SSP585 (2065-2074)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "IPSL" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 721 & dates[j] <= 840)){
    paste0('Linear regression slope. IPSL-CM6A-LR. SSP585 (2075-2084)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "IPSL" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 841 & dates[j] <= 960)){
    paste0('Linear regression slope. IPSL-CM6A-LR. SSP585 (2085-2095)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "IPSL" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 961 & dates[j] <= 1032)){
    paste0('Linear regression slope. IPSL-CM6A-LR. SSP585 (2095-2100)', sep=" ", nc_var)
    
    
  } else if (brick_slope_df$name[j] %like% "MPI" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 1 & dates[j] <= 120)){
    paste0('Linear regression slope. MPI-ESM1-2-HR. SSP126 (2015-2024)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MPI" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 121 & dates[j] <= 240)){
    paste0('Linear regression slope. MPI-ESM1-2-HR. SSP126 (2025-2034)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MPI" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 241 & dates[j] <= 360)){
    paste0('Linear regression slope. MPI-ESM1-2-HR. SSP126 (2035-2044)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MPI" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 361 & dates[j] <= 480)){
    paste0('Linear regression slope. MPI-ESM1-2-HR. SSP126 (2045-2054)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MPI" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 481 & dates[j] <= 600)){
    paste0('Linear regression slope. MPI-ESM1-2-HR. SSP126 (2055-2064)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MPI" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 601 & dates[j] <= 720)){
    paste0('Linear regression slope. MPI-ESM1-2-HR. SSP126 (2065-2074)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MPI" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 721 & dates[j] <= 840)){
    paste0('Linear regression slope. MPI-ESM1-2-HR. SSP126 (2075-2084)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MPI" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 841 & dates[j] <= 960)){
    paste0('Linear regression slope. MPI-ESM1-2-HR. SSP126 (2085-2095)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MPI" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 961 & dates[j] <= 1032)){
    paste0('Linear regression slope. MPI-ESM1-2-HR. SSP126 (2095-2100)', sep=" ", nc_var)
    
  } else if (brick_slope_df$name[j] %like% "MPI" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 1 & dates[j] <= 120)){
    paste0('Linear regression slope. MPI-ESM1-2-HR. SSP370 (2015-2024)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MPI" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 121 & dates[j] <= 240)){
    paste0('Linear regression slope. MPI-ESM1-2-HR. SSP370 (2025-2034)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MPI" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 241 & dates[j] <= 360)){
    paste0('Linear regression slope. MPI-ESM1-2-HR. SSP370 (2035-2044)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MPI" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 361 & dates[j] <= 480)){
    paste0('Linear regression slope. MPI-ESM1-2-HR. SSP370 (2045-2054)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MPI" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 481 & dates[j] <= 600)){
    paste0('Linear regression slope. MPI-ESM1-2-HR. SSP370 (2055-2064)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MPI" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 601 & dates[j] <= 720)){
    paste0('Linear regression slope. MPI-ESM1-2-HR. SSP370 (2065-2074)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MPI" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 721 & dates[j] <= 840)){
    paste0('Linear regression slope. MPI-ESM1-2-HR. SSP370 (2075-2084)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MPI" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 841 & dates[j] <= 960)){
    paste0('Linear regression slope. MPI-ESM1-2-HR. SSP370 (2085-2095)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MPI" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 961 & dates[j] <= 1032)){
    paste0('Linear regression slope. MPI-ESM1-2-HR. SSP370 (2095-2100)', sep=" ", nc_var)
    
  } else if (brick_slope_df$name[j] %like% "MPI" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 1 & dates[j] <= 120)){
    paste0('Linear regression slope. MPI-ESM1-2-HR. SSP585 (2015-2024)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MPI" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 121 & dates[j] <= 240)){
    paste0('Linear regression slope. MPI-ESM1-2-HR. SSP585 (2025-2034)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MPI" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 241 & dates[j] <= 360)){
    paste0('Linear regression slope. MPI-ESM1-2-HR. SSP585 (2035-2044)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MPI" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 361 & dates[j] <= 480)){
    paste0('Linear regression slope. MPI-ESM1-2-HR. SSP585 (2045-2054)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MPI" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 481 & dates[j] <= 600)){
    paste0('Linear regression slope. MPI-ESM1-2-HR. SSP585 (2055-2064)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MPI" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 601 & dates[j] <= 720)){
    paste0('Linear regression slope. MPI-ESM1-2-HR. SSP585 (2065-2074)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MPI" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 721 & dates[j] <= 840)){
    paste0('Linear regression slope. MPI-ESM1-2-HR. SSP585 (2075-2084)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MPI" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 841 & dates[j] <= 960)){
    paste0('Linear regression slope. MPI-ESM1-2-HR. SSP585 (2085-2095)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MPI" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 961 & dates[j] <= 1032)){
    paste0('Linear regression slope. MPI-ESM1-2-HR. SSP585 (2095-2100)', sep=" ", nc_var)
    
    
  } else if (brick_slope_df$name[j] %like% "MRI" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 1 & dates[j] <= 120)){
    paste0('Linear regression slope. MRI-ESM2-0. SSP126 (2015-2024)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MRI" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 121 & dates[j] <= 240)){
    paste0('Linear regression slope. MRI-ESM2-0. SSP126 (2025-2034)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MRI" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 241 & dates[j] <= 360)){
    paste0('Linear regression slope. MRI-ESM2-0. SSP126 (2035-2044)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MRI" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 361 & dates[j] <= 480)){
    paste0('Linear regression slope. MRI-ESM2-0. SSP126 (2045-2054)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MRI" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 481 & dates[j] <= 600)){
    paste0('Linear regression slope. MRI-ESM2-0. SSP126 (2055-2064)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MRI" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 601 & dates[j] <= 720)){
    paste0('Linear regression slope. MRI-ESM2-0. SSP126 (2065-2074)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MRI" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 721 & dates[j] <= 840)){
    paste0('Linear regression slope. MRI-ESM2-0. SSP126 (2075-2084)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MRI" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 841 & dates[j] <= 960)){
    paste0('Linear regression slope. MRI-ESM2-0. SSP126 (2085-2095)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MRI" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 961 & dates[j] <= 1032)){
    paste0('Linear regression slope. MRI-ESM2-0. SSP126 (2095-2100)', sep=" ", nc_var)
    
  } else if (brick_slope_df$name[j] %like% "MRI" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 1 & dates[j] <= 120)){
    paste0('Linear regression slope. MRI-ESM2-0. SSP370 (2015-2024)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MRI" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 121 & dates[j] <= 240)){
    paste0('Linear regression slope. MRI-ESM2-0. SSP370 (2025-2034)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MRI" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 241 & dates[j] <= 360)){
    paste0('Linear regression slope. MRI-ESM2-0. SSP370 (2035-2044)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MRI" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 361 & dates[j] <= 480)){
    paste0('Linear regression slope. MRI-ESM2-0. SSP370 (2045-2054)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MRI" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 481 & dates[j] <= 600)){
    paste0('Linear regression slope. MRI-ESM2-0. SSP370 (2055-2064)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MRI" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 601 & dates[j] <= 720)){
    paste0('Linear regression slope. MRI-ESM2-0. SSP370 (2065-2074)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MRI" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 721 & dates[j] <= 840)){
    paste0('Linear regression slope. MRI-ESM2-0. SSP370 (2075-2084)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MRI" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 841 & dates[j] <= 960)){
    paste0('Linear regression slope. MRI-ESM2-0. SSP370 (2085-2095)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MRI" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 961 & dates[j] <= 1032)){
    paste0('Linear regression slope. MRI-ESM2-0. SSP370 (2095-2100)', sep=" ", nc_var)
    
  } else if (brick_slope_df$name[j] %like% "MRI" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 1 & dates[j] <= 120)){
    paste0('Linear regression slope. MRI-ESM2-0. SSP585 (2015-2024)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MRI" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 121 & dates[j] <= 240)){
    paste0('Linear regression slope. MRI-ESM2-0. SSP585 (2025-2034)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MRI" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 241 & dates[j] <= 360)){
    paste0('Linear regression slope. MRI-ESM2-0. SSP585 (2035-2044)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MRI" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 361 & dates[j] <= 480)){
    paste0('Linear regression slope. MRI-ESM2-0. SSP585 (2045-2054)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MRI" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 481 & dates[j] <= 600)){
    paste0('Linear regression slope. MRI-ESM2-0. SSP585 (2055-2064)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MRI" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 601 & dates[j] <= 720)){
    paste0('Linear regression slope. MRI-ESM2-0. SSP585 (2065-2074)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MRI" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 721 & dates[j] <= 840)){
    paste0('Linear regression slope. MRI-ESM2-0. SSP585 (2075-2084)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MRI" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 841 & dates[j] <= 960)){
    paste0('Linear regression slope. MRI-ESM2-0. SSP585 (2085-2095)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "MRI" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 961 & dates[j] <= 1032)){
    paste0('Linear regression slope. MRI-ESM2-0. SSP585 (2095-2100)', sep=" ", nc_var)
    
    
  } else if (brick_slope_df$name[j] %like% "UKESM1" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 1 & dates[j] <= 120)){
    paste0('Linear regression slope. UKESM1-ESM2-0. SSP126 (2015-2024)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "UKESM1" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 121 & dates[j] <= 240)){
    paste0('Linear regression slope. UKESM1-ESM2-0. SSP126 (2025-2034)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "UKESM1" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 241 & dates[j] <= 360)){
    paste0('Linear regression slope. UKESM1-ESM2-0. SSP126 (2035-2044)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "UKESM1" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 361 & dates[j] <= 480)){
    paste0('Linear regression slope. UKESM1-ESM2-0. SSP126 (2045-2054)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "UKESM1" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 481 & dates[j] <= 600)){
    paste0('Linear regression slope. UKESM1-ESM2-0. SSP126 (2055-2064)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "UKESM1" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 601 & dates[j] <= 720)){
    paste0('Linear regression slope. UKESM1-ESM2-0. SSP126 (2065-2074)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "UKESM1" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 721 & dates[j] <= 840)){
    paste0('Linear regression slope. UKESM1-ESM2-0. SSP126 (2075-2084)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "UKESM1" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 841 & dates[j] <= 960)){
    paste0('Linear regression slope. UKESM1-ESM2-0. SSP126 (2085-2095)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "UKESM1" & brick_slope_df$name[j] %like% "126" & (dates[j] >= 961 & dates[j] <= 1032)){
    paste0('Linear regression slope. UKESM1-ESM2-0. SSP126 (2095-2100)', sep=" ", nc_var)
    
  } else if (brick_slope_df$name[j] %like% "UKESM1" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 1 & dates[j] <= 120)){
    paste0('Linear regression slope. UKESM1-ESM2-0. SSP370 (2015-2024)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "UKESM1" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 121 & dates[j] <= 240)){
    paste0('Linear regression slope. UKESM1-ESM2-0. SSP370 (2025-2034)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "UKESM1" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 241 & dates[j] <= 360)){
    paste0('Linear regression slope. UKESM1-ESM2-0. SSP370 (2035-2044)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "UKESM1" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 361 & dates[j] <= 480)){
    paste0('Linear regression slope. UKESM1-ESM2-0. SSP370 (2045-2054)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "UKESM1" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 481 & dates[j] <= 600)){
    paste0('Linear regression slope. UKESM1-ESM2-0. SSP370 (2055-2064)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "UKESM1" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 601 & dates[j] <= 720)){
    paste0('Linear regression slope. UKESM1-ESM2-0. SSP370 (2065-2074)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "UKESM1" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 721 & dates[j] <= 840)){
    paste0('Linear regression slope. UKESM1-ESM2-0. SSP370 (2075-2084)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "UKESM1" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 841 & dates[j] <= 960)){
    paste0('Linear regression slope. UKESM1-ESM2-0. SSP370 (2085-2095)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "UKESM1" & brick_slope_df$name[j] %like% "370" & (dates[j] >= 961 & dates[j] <= 1032)){
    paste0('Linear regression slope. UKESM1-ESM2-0. SSP370 (2095-2100)', sep=" ", nc_var)
    
  } else if (brick_slope_df$name[j] %like% "UKESM1" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 1 & dates[j] <= 120)){
    paste0('Linear regression slope. UKESM1-ESM2-0. SSP585 (2015-2024)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "UKESM1" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 121 & dates[j] <= 240)){
    paste0('Linear regression slope. UKESM1-ESM2-0. SSP585 (2025-2034)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "UKESM1" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 241 & dates[j] <= 360)){
    paste0('Linear regression slope. UKESM1-ESM2-0. SSP585 (2035-2044)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "UKESM1" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 361 & dates[j] <= 480)){
    paste0('Linear regression slope. UKESM1-ESM2-0. SSP585 (2045-2054)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "UKESM1" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 481 & dates[j] <= 600)){
    paste0('Linear regression slope. UKESM1-ESM2-0. SSP585 (2055-2064)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "UKESM1" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 601 & dates[j] <= 720)){
    paste0('Linear regression slope. UKESM1-ESM2-0. SSP585 (2065-2074)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "UKESM1" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 721 & dates[j] <= 840)){
    paste0('Linear regression slope. UKESM1-ESM2-0. SSP585 (2075-2084)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "UKESM1" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 841 & dates[j] <= 960)){
    paste0('Linear regression slope. UKESM1-ESM2-0. SSP585 (2085-2095)', sep=" ", nc_var)
  } else if (brick_slope_df$name[j] %like% "UKESM1" & brick_slope_df$name[j] %like% "585" & (dates[j] >= 961 & dates[j] <= 1032)){
    paste0('Linear regression slope. UKESM1-ESM2-0. SSP585 (2095-2100)', sep=" ", nc_var)}






extract_plots_from_list <- function(x) {
  
  if (!is.list(x)) {
    stop("`x` must be a list.")
  }
  
  if (length(x) < 1) {
    return(x)
  }
  
  more_lists <- sapply(x, function(object_to_check) {
    'list' %in% class(object_to_check) & !(any(c('gg', "gTree", "gtable") %in% class(object_to_check)))
  })
  
  result <- c(x[!more_lists],
              unlist(x[more_lists],
                     recursive = FALSE))
  
  if (sum(more_lists)) { 
    
    Recall(result)
    
  } else {
    
    is_plot_obj <- sapply(result, function(result_object) {
      any(c('gg', "gTree", "gtable") %in% class(result_object))
    })
    
    result <- result[is_plot_obj]
    return(result)
  }
}



aa<-extract_plots_from_list(brick_126_GFDL_pl)

lapply(brick_126_GFDL_pl, rapply, f = c)


list.flatten(brick_126_GFDL_pl, classes = "ggplot")



plts <- flattenlist(brick_126_GFDL_pl)

names(plts)

lapply(plts, class)

??list.flatten
brick_126_GFDL_pl[[i]] <-list(brick_126_GFDL_pl)


myplots_j[[j]] <- p1 }
myplots[[i]] <- list(myplots_j)
??flatten
  
library(jsonlite)
myplots_f=flatten(brick_126_IPSL_pl)#large list of 234 elements?

brick_126_IPSL_pl[[15]]

library(gridExtra)
ggsave(
  filename = ".pdf", 
  plot = gridExtra::marrangeGrob(myplots_f, nrow=2, ncol=3), 
  width = 15, height = 9
)


  
  






#els teus nivells
types <- c("squid_jigger"    ,   "drifting_longlines" ,"trawlers"         ,  "seiners" ,           "pole_and_line","set_longlines"    ,  "fixed_gear",   "fishing"     ,  "pots_and_traps", "set_gillnets",    "dredge_fishing"  ,   "trollers")
#les teves dates
effort_dates=jazz



myplots_j <- list()  # new empty list
myplots <- list()  # new empty list

for (i in seq_along (effort_dates)){
  #i=1
  print(length(effort_dates)-i)
  index_date=effort_dates[i]
  for (j in seq_along (types)){
    #j=3
    index_type=types[j]
    print(index_type)
    r <- raster(paste0(WD,"LEIA/PROJECTS_LEIA/Radars_2020/Radar_gfw_outputs/",index_type,"_",index_date,".tif"))
    r_pts <- rasterToPoints(r, spatial = TRUE)
    r_df  <- data.frame(r_pts)
    colnames(r_df)[1] <- "dens"
    gfw_ss=gfw%>%
      dplyr::filter(Date==index_date,
                    type==index_type)%>%
      dplyr::select(Longitude,Latitude,value)
    
    red_ss=red%>%
      dplyr::filter(Date==index_date)%>%
      dplyr::select(Longitude,Latitude)
    
    p1=ggplot(data = msk_valid) + 
      geom_raster(data = r_df , aes(x=x,y=y,fill = dens))+
      geom_sf() + 
      geom_point(data=gfw_ss,aes(x=Longitude,y=Latitude,fill=value),colour="black",shape=21,size=2,show.legend = TRUE)+
      geom_point(data=red_ss,aes(x=Longitude,y=Latitude),colour="black",fill="green",shape=21,size=2,show.legend = TRUE)+
      scale_fill_viridis_c(trans = "log",direction=-1, option="magma")+
      coord_sf(xlim = c(0, 5), ylim = c(38, 43)) +
      theme_bw() +
      xlab("Longitude") + ylab("Latitude")+
      ggtitle(paste(index_type))
    
    myplots_j[[j]] <- p1 }
  myplots[[i]] <- list(myplots_j)
}


myplots_f=flatten(myplots)#large list of 234 elements?
myplots_f=flatten(myplots_f)


library(gridExtra)
ggsave(
  filename = "plots_cv.pdf", 
  plot = gridExtra::marrangeGrob(myplots_f, nrow=2, ncol=3), 
  width = 15, height = 9
)

