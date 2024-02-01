#---------------------------------------------------------------------------------------------------
# fit_brt             Fit Species distribution model
#---------------------------------------------------------------------------------------------------
# create a grid of hyper-parameters (learning rate, depth, min obs in nodes, etc...)
# and build a full model on each combination of parameters in parallel. 

install.packages("devtools") # in case "devtools" has not already been installed
devtools::install_github("JBjouffray/ggBRT") # will take several minutes to install
library(ggBRT)
library(splitstackshape)
library(corrplot)
library(fmsb)
library(fuzzySim)

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







# Model optiomization prioritized:
# models with larger learning rates, smaller tree complexities and fewer trees to reduce overfitting (Elith et al. 2008)
# ensure that at least 1,000 trees were included in the final model configuration (Elith et al., 2008).                                                                                                     
library(groupdata2)
library(dplyr)
library(data.table)
library(splitstackshape)
sp_code <- "GAZ"
mod_code <- "brt"
sex_code <- "males"



#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------
output_data <- "C:/Users/Jazel Ouled/Dropbox/PhD/Arctocephalus-gazella/00output"
indir <- paste0(output_data, "/tracking/", sp_code, "/", "PresAbsExtractEnsemble/")
outdir <- paste(output_data, "habitat-model-v2", sp_code, mod_code, sex_code, sep="/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
vars <- c("BAT", "SLP", "SST", "SAL", "EKE", "CHL", "SIC", "EDGE")  # list of all predictors
# vars <- c("BAT", "SLP", "SST", "SAL", "EKE", "CHL")  # list of all predictors

# vars <- c("BAT", "SLP", "SST", "SSTg", "SAL", "SALg", "EKE", "CHL")  # list of all predictors




#-----------------------------------------------------------------
# Prepare data
#-----------------------------------------------------------------
# We also included a random number between 1 and 100 to serve as an indicator for variables that
# have influence greater or less than random (Scales et al., 2017; Soykan, Eguchi, Kohin, & Dewar, 2014);
# only variables with influence greater than the random number were included in the final models.


ssp <- c("ssp126", "ssp370", "ssp585")

for (q in 1:length(ssp)){
 
  print(q)
  vars <- c("BAT", "SLP", "SST", "SAL", "EKE", "CHL", "SIC", "EDGE")  # list of all predictors
  obs_file <-  dir(paste0("C:/Users/Jazel Ouled/Dropbox/PhD/Arctocephalus-gazella/00output/tracking/GAZ/PresAbsExtractEnsemble/", ssp[q]), recursive = T, full.names = T )
  
  outdir <- paste(output_data, "habitat-model-v2", sp_code, mod_code, sex_code, sep="/")
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  
  product_folder <- paste0(outdir, "/ensemble/NOSurfaceTrend/", ssp[q])
  if (!dir.exists(product_folder)) dir.create(product_folder, recursive = TRUE)  # create output directory if does not exist
  
  
## Data (tracking data was gridded at 1º)
train <- read.csv(obs_file)
env_columns <- subset(train, select = vars)
train<- train[1:9]
train <-cbind(train, env_columns)





# presences
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

# tsa <- multTSA(train, sp.cols = "occ", 
#                coord.cols = c("lon", "lat"), id.col = 2)
# train$TSA <- tsa$occ_TS


# write.csv(train, paste0(indir, "train_pres_abs_even.csv"))
# # Identify skewed variables
# par(mfrow=c(3,5))
# hist(train$EKE)
# hist(train$BAT)
# hist(train$SLP)
# hist(train$SDIST)
# hist(train$D2COL)
# hist(train$CHL)
# hist(train$EDGE)
# hist(log(train$EKE))
# hist(train$SIC)
# hist(train$SAL)
# hist(train$SST)
# hist(train$UO)
# hist(train$VO)

# Transform skewed variables
# train$EKE <- log1p(train$EKE)
# train$CHL <- log1p(train$CHL)

# Transform ice edge distance (change negative values to zero)
train$EDGE[train$EDGE < 0] <- 0

# Generate Random Number
train$RN <- sample.int(100, size=nrow(train), replace=T, prob=NULL)


#-----------------------------------------------------------------
# Prepare folds
#-----------------------------------------------------------------
# leave-one-out strategy
n.folds <- 14

set.seed(123)
train$id <- as.factor(train$id)
f <- fold(data = train, id_col = "id", method = "n_dist", k = n.folds)

# prepare
train <- f %>%
  dplyr::rename(fold = .folds) %>%
  dplyr::mutate(fold = as.numeric(fold)) %>%
  as.data.frame()


table(train$fold)

train %>%
  group_by(id, fold) %>%
  dplyr::summarize(n = n())



#-----------------------------------------------------------------
# Boosted Regression Tree - Optimization of hyper-parameters
#-----------------------------------------------------------------
# Add the RN to the list of variables
vars <- c(vars, "RN")

# Define number of trees
ini.nt = 50
max.nt = 10000
step.nt = 50
tree.list <- seq(ini.nt,max.nt,by=step.nt) #list of trees for evaluation

# Define combination of hyper-parameters
comb <- expand.grid(lr=c(0.001, 0.005, 0.01, 0.05), tc=c(1,3,5), bf=c(0.5, 0.6, 0.7)) #combination



# train$TSA <- as.numeric(tsa$occ_TS)
# t_s_r <- rasterFromXYZ(train[c(5,6,21)])
# 
# plot(t_s_r)
# tsa_resize <- resample(t_s_r, var_stack_1$p_d)
# tsa_resize[is.na(tsa_resize[])] <- 0





# Prepare clusters
# cores <- nrow(comb)  # detectCores()
cores <- 5  # detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)

set.seed(131)
system.time(
all_list <- foreach(i=1:nrow(comb), .packages=c("dismo", "gbm", "dplyr", "doParallel", "data.table")) %dopar% {
                # Fit model
                # Uses a block cross-validation
                # faster learning rate means larger values
                mod <- tryCatch(
               dismo::gbm.step(data = train,             # data.frame with data
                                  gbm.x = vars,          # predictor variables
                                  gbm.y = 8,            # response variable
                                  family = "bernoulli",  # the nature of errror structure
                                  tree.complexity = comb$tc[i],   # tree complexity
                                  learning.rate = comb$lr[i],  # learning rate
                                  bag.fraction = comb$bf[i],    # bag fraction
                                  fold.vector = train$fold,
                                  n.folds = length(unique(train$fold)),
                                  n.trees = ini.nt, step.size = step.nt, max.trees = max.nt),
                                  offset = train$TSA,
                                  error = function(e) return(NULL))

              if(!is.null(mod)) {
  
                # Keep CV parameters
                mod_out <- data.frame(
                                tree.complexity = mod$interaction.depth,
                                learning.rate = mod$shrinkage,
                                bag.fraction = mod$bag.fraction,
                                n.trees = mod$n.trees,
                                AUC = mod$self.statistics$discrimination,
                                cv.AUC = mod$cv.statistics$discrimination.mean,
                                deviance = mod$self.statistics$mean.resid,
                                cv.deviance = mod$cv.statistics$deviance.mean,
                                PER = (1-mod$self.statistics$mean.resid/mod$self.statistics$mean.null)*100,
                                cv.PER = (1-mod$cv.statistics$deviance.mean/mod$self.statistics$mean.null)*100) 
                              
                              # keep deviance values for all trees
                              cv_deviance <- mod$cv.values
                              cv_deviance <- c(cv_deviance, rep(NA, length(tree.list) - length(cv_deviance)))  #fill with NA
                              
                              # selected variables
                              pred_order <- summary(mod)$var
                              rn_position <- which(pred_order == "RN")
                              pred_list <- as.character(pred_order[1:(rn_position-1)])
                              list(mod_out = mod_out, cv_deviance = cv_deviance, pred_list = pred_list)
                             
                               }
                            }
)



# combine model outputs
mod_list <- foreach(i=1:nrow(comb)) %dopar% all_list[[i]]$mod_out
mod_list[!lengths(mod_list)] <-  list(data.frame(tree.complexity = NA))
mod_out <- rbindlist(mod_list, fill = TRUE)
mod_out <- bind_cols(comb,
                      dplyr::select(mod_out, -c(tree.complexity, learning.rate, bag.fraction))) %>%
                         dplyr::mutate(id = 1:n())
  

# combine deviance outputs
deviance_list <- list()
    for(i in 1:nrow(mod_out)){
        # extract deviance data
        dev <- all_list[[i]]$cv_deviance
        
        # check that there is no null data
        if(is.null(dev)) dev <- rep(NA,length(tree.list))
        
        # make data.frame with number of trees
        df <- data.frame(id = mod_out$id[i], lr = mod_out$lr[i], tc = mod_out$tc[i], bf = mod_out$bf[i], ntrees = tree.list, cv_deviance = dev)
        deviance_list[[i]] <- df
    }


cv_deviance <- rbindlist(deviance_list)

# get selected variables
predict_list <- foreach(i=1:nrow(comb)) %dopar% all_list[[i]]$pred_list

# stop clusters
stopCluster(cl)


theme_article<-  theme(
        legend.position = "none",
        strip.text.y.right = element_blank(),
        panel.grid.major.y = element_line( size=.1, color="grey50"),
        axis.title.x = element_text(margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0))
      )
    
    
# plot profiles
p <- ggplot(data = cv_deviance) +
     geom_line(data = dplyr::rename(cv_deviance, comb = id), aes(x = ntrees, y = cv_deviance, group = comb), color = "grey80") +
     geom_line(aes(x = ntrees, y = cv_deviance, group = id), color = "firebrick3") +
     scale_x_continuous(limits = c(0, max(cv_deviance$ntrees[!is.na(cv_deviance$cv_deviance)]))) +
     facet_wrap(id ~.,) 
      
outfile <- paste0(product_folder, "/GAZ_BRT_optim_params_", ssp[q],".png")

ggsave(outfile, p, width=25, height=14, units="cm", dpi=300)
    

    
# mod_out[,] <- do.call(rbind, mod_out)
    

# summary_df <-
#   mod_out_ %>% 
#   bind_rows(mod_out[,])
# 
#  # mod_out_[,] <- mod_out

     

# export outputs
outfile <- paste0(product_folder, "/GAZ_BRT_optim_params_", ssp[q], ".csv")

write.csv(mod_out, outfile, row.names = FALSE)

outfile <- paste0(product_folder, "/GAZ_BRT_cv_deviance_", ssp[q],".csv")
write.csv(cv_deviance, outfile, row.names = FALSE)

outfile <- paste0(product_folder, "/GAZ_BRT_predlist_", ssp[q], ".rds")
saveRDS(predict_list, outfile)

}




#-----------------------------------------------------------------
# Boosted Regression Tree - Fit full model
#-----------------------------------------------------------------

ssp <- c("ssp126", "ssp370", "ssp585")



for (k in 1:length(ssp)){

models <-  dir(paste0("C:/Users/Jazel Ouled/Dropbox/PhD/Arctocephalus-gazella/00output/habitat-model-v2/GAZ/brt/males/ensemble/NOSurfaceTrend/", ssp[k]), recursive = T, full.names = T )
product_folder <- paste0(outdir, "/ensemble/NOSurfaceTrend/", ssp[k], "/")
full_model_folders <- paste0(outdir, "/ensemble/NOSurfaceTrend/", ssp[k], "/", "full_model/")
if (!dir.exists(full_model_folders)) dir.create(full_model_folders, recursive = TRUE)


mod_out <- read.csv(paste0(product_folder,  sp_code, "_", mod_code, "_optim_params_", ssp[k],".csv"))
predict_list <- readRDS(paste0(product_folder,  sp_code, "_", mod_code, "_predlist_", ssp[k],".rds"))


#the step of choosing the model is more or less manual. I checked the one with highest cv AUC that had > 1000 trees and had
# a quickly descending deviance curve. 
if (ssp[k] == "ssp126"){
  select_model_id <- 10 # selection is based on parameters, criteria and checking curves 
} else if (ssp[k] == "ssp370"){
  select_model_id <- 11} else if (ssp[k] == "ssp585"){
  select_model_id <- 11}


tc <- mod_out$tc[select_model_id]
lr <- mod_out$lr[select_model_id]
bf <- mod_out$bf[select_model_id]
ntrees <- mod_out$n.trees[select_model_id]
pred_list <- vars[vars %in% predict_list[[select_model_id]]]

# remove variables not selected
# fir BRT with selected parameters
mod_full <- dismo::gbm.fixed(data = train,  # data.frame with data
                gbm.x = pred_list,          # predictor variables
                gbm.y = 8,                  # response variable
                family = "bernoulli",       # the nature of errror structure
                tree.complexity = tc,       # tree complexity
                learning.rate = lr,         # learning rate
                bag.fraction = bf,          # bag fraction
                n.trees = ntrees)           # number of trees

# Save model
saveRDS(mod_full, file = paste0(outdir, "/ensemble/NOSurfaceTrend/",ssp[k], "/full_model/", sp_code, "_", mod_code, "_", ssp[k],".rds")) 

# save model
mod_full <- readRDS(paste0(full_model_folders, sp_code, "_", mod_code, "_", ssp[k],".rds"))




# Plot variable contribution using radar plot
var_imp <- summary(mod_full)$rel.inf
names(var_imp) <- summary(mod_full)$var
pngfile <- paste0(full_model_folders, sp_code, "_", mod_code, "_", ssp[k], "_var_radar.png")
png(pngfile, width=1000, height=1000, res=150)
radarPlot(var_imp, var_order=mod_full$contributions$var)
dev.off()

# Plot variable contribution using bar plot
pngfile <- paste0(full_model_folders, sp_code, "_", mod_code, "_", ssp[k], "_var_influence.png")
png(pngfile, width=1000, height=1000, res=150)
ggBRT::ggInfluence(mod_full, show.signif = F, col.bar = "skyblue3")
dev.off()

# Plot response curves
pngfile <- paste0(full_model_folders, sp_code, "_", mod_code, "_", ssp[k], "_response.png")
png(pngfile, width=1500, height=1500, res=200)
names(mod_full$gbm.call)[1] <- "dataframe"
ggBRT::ggPD(mod_full, n.plots =10, smooth = F, rug = F, ncol=2, col.line = "skyblue3")
dev.off()


}

# mod_full<-readRDS("C:/Users/Jazel Ouled/Dropbox/PhD/Arctocephalus-gazella/00output/habitat-model-v2/GAZ/brt/males/full_model/GAZ_brt.rds")



#-----------------------------------------------------------------
# Boosted Regression Tree - Interactions
#-----------------------------------------------------------------
# seems it is not working with fixed objects, try to gbm.step

# there is a bug to get interaction for gbm.fixed, unlike gbm.stem
# changing the name of one variable fixes the problem
names(mod_full$gbm.call)[1] <- "dataframe"

find.int <- dismo::gbm.interactions(mod_full)
find.int$interactions
find.int$rank.list

dismo::gbm.perspec(mod_full, 5, 9)
gbm.perspec(mod_full, 12, 9)
gbm.perspec(mod_full, 12, 10)
dismo::gbm.perspec(mod_full, 14, 12)



#-----------------------------------------------------------------
# Boosted Regression Tree - Predict (Bootstrap approach)
#-----------------------------------------------------------------
# For each habitat selection model (i.e., each life-history stage of each species), we fitted the model 50 times.
# For each of the 50 iterations, we used the parameter values chosen for the final model, but we sampled
# half the data (with replacement) to fit the model. (Hindell et al. 2020).

ssp <- c("ssp126", "ssp370", "ssp585")

for (j in 1:length(ssp)){
  
  
  
  full_model_folders <- paste0(outdir, "/ensemble/NOSurfaceTrend/", ssp[j], "/", "full_model/")
  if (!dir.exists(full_model_folders)) dir.create(full_model_folders, recursive = TRUE)
  


# Set output directory
# Each bootstrap model is stored here
outdir_bootstrap <- paste0(full_model_folders, "bootstrap/")
if (!dir.exists(outdir_bootstrap)) dir.create(outdir_bootstrap, recursive = TRUE)


# Define number of bootstrap models
n.boot <- 50  # number of model fits

## Prepare clusters
cores <- 10
cl <- makeCluster(cores)
registerDoParallel(cl)

foreach(i=1:n.boot, .packages=c("dismo", "gbm", "dplyr", "splitstackshape", "stringr")) %dopar% {

  # sampled half the data (with replacement) to fit the model (Hindell et al. 2020)
  idata <- stratified(train, c("occ", "id", "date"), 0.5, replace = TRUE)
  
  # fit BRT
  mod_boot <- dismo::gbm.fixed(data = idata,             # data.frame with data
                       gbm.x = pred_list,          # predictor variables
                       gbm.y = 8,            # response variable
                       family = "bernoulli",  # the nature of errror structure
                       tree.complexity = tc,   # tree complexity
                       learning.rate = lr,  # learning rate
                       bag.fraction = bf,    # bag fraction
                       n.trees = ntrees) 
  
  # store model
  outfile <- paste0(outdir_bootstrap, "/", str_pad(i, 2, pad = "0"), "_", sp_code, "_", mod_code, "_boot.rds")
  saveRDS(mod_boot, file = outfile)  # save model
}

}


## stop clusters
stopCluster(cl)


graphics.off()
