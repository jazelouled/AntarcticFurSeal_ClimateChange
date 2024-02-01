library(splitstackshape)
library(dplyr)



plot_multi_histogram <- function(df, col.feature, col.label) {
  plt <- ggplot2::ggplot(df,
                         ggplot2::aes(
                           x = .data[[col.feature]],
                           fill = .data[[col.label]])) +
    # ggplot2::geom_histogram(alpha = 0.5, position = "identity",
    #                         ggplot2::aes(y = ..density..), color = "black") +
    ggplot2::geom_density(alpha = 0.5) +
    labs(x = col.feature, y = "Density") + 
    ggplot2::guides(fill = ggplot2::guide_legend(title = col.label))
  return(plt)
  
}


extractFiles <- list.files("C:/Users/Jazel Ouled/Dropbox/PhD/Arctocephalus-gazella/00output/tracking/GAZ/PresAbsExtractEnsemble/", recursive = T, pattern = "ssp", full.names = T)

ssp126 <- read.csv(extractFiles[1])
ssp370 <- read.csv(extractFiles[2])
ssp585 <- read.csv(extractFiles[3])

ssp585_ <- ssp585[var]
ssp370_ <- ssp370[var]
ssp126_ <- ssp585[var]
aa <- cbind(ssp126_, ssp370_, ssp585_)



ggplot(aa, aes(x=SST))+
  geom_density(color="darkblue", fill="lightblue")





listDF <- list()
list_var <- list()
vars <- c("BAT", "SLP", "SST", "SAL", "EKE", "CHL", "SIC", "EDGE")  # list of all predictors


for (i in 1:length(extractFiles)){
  
 train <- read.csv(extractFiles[i])
 presences <- filter(train, occ==1)
 n_occ <- nrow(presences)
 # absences
 absences <- filter(train, occ==0)
 abs_prop <- nrow(presences)/nrow(absences)
 absences <- stratified(absences, c("id"), abs_prop)
 # combine presence-absences
 train <- bind_rows(presences, absences)
 # check number of occurrence per type
 table(train$occ)

 
 listDF[[i]] <- train
 
 
 
    for (j in 1:length(vars)){
      
           var <- vars[j]
           var_col <-  select(listDF[[i]],  var)
           list_var[[j]] <- var_col
           
           
           
           
           }
 
 
 
 
 
           
           
           
           
           
 class(listDF[i])
 
 
 for (j in length(vars))
   
   var <- vars[j]
   
   train_ <- train[vars]
   
   SST[,1] <- train_
   
   
   train_ <- train_[var]
   
    SST[train_,]
     
 
 
 
   vars <- c("BAT", "SLP", "SST", "SAL", "EKE", "CHL", "SIC", "EDGE")  # list of all predictors
 

 

 
 
 
 
  
  
}





pred_cir.png


files <- list.files("C:/Users/Jazel Ouled/Dropbox/PhD/Arctocephalus-gazella/00output/habitat-model-v2/GAZ/brt/males/ensemble/NOSurfaceTrend/ssp585/", recursive = T,  pattern = "pred.tif", full.names = T )
dates <- seq.Date(as.Date("2015-01-01"), as.Date("2100-12-01"), by="month")  # define sequence


1032/2

 aa <- stack(files[1:516])
 bb <- stack(files[517:1032])
 
 mean <- stackApply(aa, indices =  rep(1,nlayers(aa)), fun = "mean", na.rm = T)
 mean2 <- stackApply(bb, indices =  rep(1,nlayers(aa)), fun = "mean", na.rm = T)
 
bb <- calc(aa, mean)



datestring<-unique(substring(basename(files), 1, 8))

# date in ymd format = dates
dates<-ymd(datestring)
dates<-unique(round_date(dates, "month"))
datestring <- gsub("-", "", dates)


dates_int1 <- seq.Date(as.Date("2015-01-01"), as.Date("2050-12-01"), by="month")  # define sequence
datestring1 <- gsub("-", "", dates_int1)


files[dates_int1 %like% files]


files[which(datestring1 %like% files)]

intersect(unique(substring(basename(files), 1, 8)), datestring1)

aa <- mapply(intersect, files, datestring1)

dates_int2 <- seq.Date(as.Date("2051-01-01"), as.Date("2100-12-01"), by="month")  # define sequence
datestring2 <- gsub("-", "", dates)



Reduce(intersect, list(datestring1,files))



intersect_all <- function(a,b,...){
  all_data <- c(a,b,...)
  require(plyr)
  count_data<- length(list(a,b,...))
  freq_dist <- count(all_data)
  intersect_data <- freq_dist[which(freq_dist$freq==count_data),"x"]
  intersect_data
}



intersect_all(datestring1, files)


lapply(files, function(i) {
  i[which(i %in% datestring1)]
})

df %>%
  group_by(dr = cut(dates, breaks = c(range(dates), 
                                     as.Date(c("2016-04-10", "2016-04-24"))), include.lowest=TRUE) ) %>% 
  summarise(ost =sum(ost))


library(raster)
r <- raster::raster(files[[1]])

plot(r)


plot_multi_histogram(train_, var)