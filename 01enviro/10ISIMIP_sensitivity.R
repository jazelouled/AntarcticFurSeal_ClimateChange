library(ncdf4)
library(lubridate)
library(raster)
library(data.table)


ISIMIP_models <- dir("D:/ISIMIP_netCDF/models/", recursive = F, full.names = T )

nc_var <- "tos"
ssp <- c("histor", "ssp126", "ssp370", "ssp585")



# Loop to obtain data by historical/ssp and date -----------------------------------

listlist <- list()
for (z in 1:length(ISIMIP_models)){
  # z=1
  print(z)
  model <- gsub('.*/ ?(\\w+)', '\\1', ISIMIP_models[z])
  if((nc_var == "siconc" | nc_var == "chl"|nc_var == "o2"| nc_var == "so-surf" | nc_var == "uo" | nc_var == "vo")  & model == "MRI-ESM2-0") next
  ssp_folders<- dir(ISIMIP_models[z], recursive = F, full.names = T )
  ssp <- gsub('.*/ ?(\\w+)', '\\1', ssp_folders)

  
  
  
  
  # models <- gsub('.*/ ?(\\w+)', '\\1', ISIMIP_models)
  # 
  # models <- models[-4]
  # 
  # model <- model[1]
  # # model <- model[2]
  # model <- model[3]
  # model <- model[4]
  # 
  # buffers_AGAZ <- c("cp.buf_malesW", "cp.buf_femalesW", "cp.buf_femalesS")
  # 
  # buffer_AGAZ <- buffers_AGAZ[1]
  # # buffer_AGAZ <- buffers_AGAZ[2]
  # # buffer_AGAZ <- buffers_AGAZ[3]
  # # buffer_AGAZ <- buffers_AGAZ[4]
  
  
   for (q in 1:length(ssp)){
    # q=1
     print(q)
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
    
    date <- as.Date((date))
    var_r<-brick(var)
    var_r<-(flip(var_r, direction="x"))
    var_r<-t(flip(var_r, direction="x"))
    bb<-extent(min(lon),max(lon),min(lat),max(lat))
    var_r<-setExtent(var_r,bb,keepres = F, snap = F)
    projection(var_r)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    antarctica <- extent(-136.04 , -34.09 , -70.02 , -42.84)
    var_r_antarctica <- crop(var_r, antarctica)
    var_r_antarctica_ <- raster::mask(var_r_antarctica, cp.buf_femalesS)
    months<-seq(1: nlayers(var_r_antarctica_))
    months12<- rep(1:12, times= nlayers(var_r_antarctica_)/12)
   
     
    for (k in 1: length(months)){

      var_r_antarctica_vals <- getValues(var_r_antarctica_[[k]])
    
      emptydf[[k]] <- var_r_antarctica_vals 
      
      }
     
    
      big_data = do.call(cbind, emptydf)
      ee <-   as.data.frame(big_data)
      colnames(ee) <- date
      rr <-  melt(ee)
      
      
      if (ssp[q] == "histor"){
        datafeameHist <- rr
        datafeameHist$variable <- as.Date(datafeameHist$variable)
        datafeameHist <-subset(datafeameHist, datafeameHist$variable >= "2006-01-01")
        datafeameHist$ssp <- "hist"
        
        
      } else if (ssp[q] == "ssp126"){
        dataframe126 <- rr
        dataframe126$variable <- as.Date(dataframe126$variable)
        dataframe126 <-subset(dataframe126, dataframe126$variable <= "2019-12-01")
        dataframe126$ssp <- "ssp126"
        
    
      
       } else if (ssp[q] == "ssp370"){
         dataframe370 <- rr
         dataframe370$variable <- as.Date(dataframe370$variable)
         dataframe370 <-subset(dataframe370, dataframe370$variable <= "2019-12-01")
         dataframe370$ssp <- "ssp370"
     
   
    
      } else if (ssp[q] == "ssp585"){
        dataframe585 <- rr
        dataframe585$variable <- as.Date(dataframe585$variable)
        dataframe585 <-subset(dataframe585, dataframe585$variable <= "2019-12-01")  
        dataframe585$ssp <- "ssp585"

         } 
 
      kk <- rbind(datafeameHist, dataframe126, dataframe370, dataframe585)
      kk <-kk[complete.cases(kk), ]
      kk$model <- model
      
      
      
      
      
        } 

  listlist[[z]] <- kk
  



}

# pp_MW <- do.call(rbind, listlist)
# pp_FW <- do.call(rbind, listlist)
# pp_FS <- do.call(rbind, listlist)

# write.csv(pp_MW, "C:/Users/Jazel Ouled/Dropbox/PhD/Arctocephalus-gazella/00output/sensitivity/malesWinter_SST_subset.csv")
# write.csv(pp_FW, "C:/Users/Jazel Ouled/Dropbox/PhD/Arctocephalus-gazella/00output/sensitivity/femalesWinter_SST_subset.csv")
write.csv(pp_FS, "C:/Users/Jazel Ouled/Dropbox/PhD/Arctocephalus-gazella/00output/sensitivity/femalesSummer_SST_subset.csv")






      
theme2Review<-theme_bw()+theme(axis.text.x=element_text(size=16),
                                     axis.text.y = element_text(size=16),
                                     axis.title.y=element_text(size=19),
                                     axis.title.x = element_text(size=19),
                                     plot.title = element_text(size=18),
                                     legend.text = element_text(size=16),
                                     legend.title = element_text(face="bold", size=20),
                                     legend.key.size = unit(0.9, "cm"),
                                     legend.background = element_rect(colour ="black", size=0.5, linetype = "blank"),
                                     legend.direction = "vertical",
                                     legend.position = "right",
                                     plot.margin = unit(c(2, 2, 2, 2), "cm"), strip.background =element_rect(fill="gray59"),
                                     strip.text = element_text(),
                                     strip.placement = "right",
                                     panel.background = element_blank(),
                                     panel.grid.major = element_line(),
                                     panel.grid.minor = element_blank(), 
                                     panel.border = element_rect(colour = "black", fill = NA, size = 0.5))
      
kk <-kk[complete.cases(kk), ]



# Boxplots by month and ssp/historical ------------------------------------

BB <-ggplot(kk, aes(x=ssp, y=value, fill=ssp)) + 
            facet_wrap(~month(kk$variable)) +
            geom_boxplot(alpha = 0.8, width = 0.4)+
            stat_summary(fun=mean, geom="point", shape=20, size=5, color="black", fill="black") +
            scale_fill_manual(values=c("#bdbab9", "#7db3e2", "#fcd723", "#db3d06"))+
            theme2Review+
            ylab("Sea Ice Concentration (%)\n")+
            xlab("\nShared Socioeconomic Pathway (CMIP6)")+
            ylim(0,100)


AGAZ <- c("males_winter", "females_winter", "females_summer")

list_differences_month <- list()
list_differences <- list()



# pp <- pp_MW
# pp <- pp_FW
pp <- pp_FS

pp <-pp[complete.cases(pp), ]



for (r in 1:length(models)) {
  model_ <- models[r]


for (i in 1:12){
  print(i)
  
nn <-subset(pp, month(pp$variable) == i & pp$model == model_)
nn <- nn %>% drop_na()

mm <- nn %>% 
          dplyr::group_by(nn$ssp)%>%
          dplyr::summarize(Mean = mean(value))
          

 emptydff <- data.frame(hist = numeric(), ssp126 = numeric(), ssp370 = numeric(), ssp585 = numeric())
 colnames(mm) <- c("ssp", "value")

    for (j in 1:length(ssp)){
      
       tt <- mm$value[j] - mm$value
       emptydff[j,] <- tt
     }
 
max(abs(emptydff))
min(abs(emptydff[emptydff >0]))

rownames(emptydff) <- c("hist","ssp126","ssp370","ssp585")
aa <- melt(emptydff)
aa$scenario <-  c("hist","ssp126","ssp370","ssp585")  
aa$difference <- paste(aa$variable, aa$scenario, sep ="-")
bb <- subset(aa, aa$value != 0)
cc <- bb[,c(-1, -3)]
cc_ <- subset(cc, cc$difference == "hist-ssp126" | cc$difference == "hist-ssp370" 
              | cc$difference == "hist-ssp585" | cc$difference == "ssp126-ssp370" | 
                cc$difference == "ssp126-ssp585" | cc$difference == "ssp370-ssp585"  )
cc_$value <- abs(cc_$value)

cc_$month <- i  
cc_$model <- model_ 
cc_$AGAZ <- AGAZ[3]

list_differences_month[[i]] <- cc_
list_differences_month_ <- do.call(rbind, list_differences_month)

}  
  list_differences[[r]] <- list_differences_month_
  
}


# alldiferencesMW_ <- do.call(rbind, list_differences)
# write.csv(alldiferencesMW_, "C:/Users/Jazel Ouled/Dropbox/PhD/Arctocephalus-gazella/00output/sensitivity/malesWinter_SST_allDiferences.csv")
alldiferencesMW_ <- read.csv("C:/Users/Jazel Ouled/Dropbox/PhD/Arctocephalus-gazella/00output/sensitivity/malesWinter_SST_allDiferences.csv")

# alldiferencesFW_ <- do.call(rbind, list_differences)
# write.csv(alldiferencesFW_, "C:/Users/Jazel Ouled/Dropbox/PhD/Arctocephalus-gazella/00output/sensitivity/femalesWinter_SST_allDiferences.csv")
alldiferencesFW_ <- read.csv("C:/Users/Jazel Ouled/Dropbox/PhD/Arctocephalus-gazella/00output/sensitivity/femalesWinter_SST_allDiferences.csv")


# alldiferencesFS_ <- do.call(rbind, list_differences)
# write.csv(alldiferencesFS_, "C:/Users/Jazel Ouled/Dropbox/PhD/Arctocephalus-gazella/00output/sensitivity/femalesSummer_SST_allDiferences.csv")
alldiferencesFS_ <- read.csv("C:/Users/Jazel Ouled/Dropbox/PhD/Arctocephalus-gazella/00output/sensitivity/femalesSummer_SST_allDiferences.csv")


alldiferences_MW_winter <- subset(alldiferencesMW_, alldiferencesMW_$month >= 2 & alldiferencesMW_$month <= 9)

alldiferences_FW_winter <- subset(alldiferencesFW_, alldiferencesFW_$month >= 2 & alldiferencesFW_$month <= 12)

alldiferences_FS_summer <- subset(alldiferencesFS_, alldiferencesFS_$month <= 3 | alldiferencesFS_$month == 12)


alldifferences_stages <- rbind(alldiferences_MW_winter, alldiferences_FW_winter, alldiferences_FS_summer)




par(mfrow = c(1,3))

model_biases <- read.csv("C:/Users/Jazel Ouled/Dropbox/PhD/Arctocephalus-gazella/errorModelsCMIP6.csv")

ggplot(alldifferences_stages, aes(x = as.factor(month), y = value, fill = difference))+
  geom_col(position = position_dodge())+
  scale_fill_brewer(type = "qual", palette = 3) +

  # geom_errorbar(aes(ymin = emmean-SE, ymax = emmean+SE), width  =0.2, position = position_dodge(0.9))+
  facet_wrap(~model*AGAZ, ncol = 3)+
  theme2Review+
  theme(strip.placement = "outside")+
  labs(x = "\nMonth", y = "SST difference (ºC)\n")+
  geom_hline(data = model_biases,
             aes(yintercept = rmse), linetype = 2, size =0.5)




