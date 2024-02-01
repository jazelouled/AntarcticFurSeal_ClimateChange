#---------------------------------------------------------------------------------------------------
# fit_brt             Fit Species distribution model
#---------------------------------------------------------------------------------------------------
# create a grid of hyper-parameters (learning rate, depth, min obs in nodes, etc...) and build a full model on each combination of parameters in parallel. 

# Model optiomization prioritized:
# Models with larger learning rates, smaller tree complexities and fewer trees to reduce overfitting (Elith et al. 2008)
# Ensure that at least 1,000 trees were included in the final model configuration (Elith et al., 2008).                                                                                                     

mod_code <- "brt"
sp_code <- "GAZ"
sexseason <- "MW"
# sexseason <- "FW"
# sexseason <- "FS"
# sexseason <- "FScpf"



# ---------------------------------------------------------------
# Set data repository
# ---------------------------------------------------------------
indir <- here::here("InputOutput/00output/01tracking/L4_locations_extract/")
outdir <- paste0(here::here("InputOutput/00output/02habitatModel/"), sexseason, "/", "BoostedRegressionTrees")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# For females in summer (central-place foraging)
# indir <- here::here("InputOutput/00output/01tracking/L4_locations_extract_FScpf//")
# outdir <- paste0(here::here("InputOutput/00output/02habitatModel/"), sexseason, "/", "BoostedRegressionTrees")
# if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# MW
# Keep: SST, CHL, SAL, BAT, EKE, SLP
# Get rid: EDGE, DIAT, MLD, SDIST, SIC

# FW
# Keep: SLP, D2COL_CSH, MLD, SAL, BAT, EKE, SST, CHL, SDIST
# Get rid: EDGE, DIAT, SIC

# FS
# Keep: SST, CHL, DIAT, SLP, D2COL_CSH, BAT, EKE, MLD, SAL
# Get rid: SIC, SDIST, EDGE

# FScpf (we are not using D2COL_CSH to avoid it explaining all the variability and allow future environmental conditions to shape where could colonies establish)
# Keep: DIAT, SST, SLP, MLD, SAL, EKE, BAT
# Get rid: SIC, SDIST, EDGE, CHL
# (cpf = central place foraging)



# ---------------------------------------------------------------
# Predictor lists
# ---------------------------------------------------------------
# Males winter
# vars <- c("BAT", "SLP", "SDIST", "EDGE", "SST", "SIC", "MLD", "SAL", "EKE", "CHL", "DIAT")  # predictors MW
vars <- c("BAT", "SLP", "SST", "SAL", "EKE", "CHL")  # predictors MW

# Females winter
# vars <- c("BAT", "SLP", "SDIST", "D2COL_CSH", "EDGE", "SST", "SIC", "MLD", "SAL", "EKE", "CHL", "DIAT")  # predictors FW
vars <- c("BAT", "SLP", "SDIST", "D2COL_CSH", "SST", "MLD", "SAL", "EKE", "CHL")  # predictors FW

# Females summer
# vars <- c("BAT", "SLP", "SDIST", "D2COL_CSH", "EDGE", "SST", "SIC", "MLD", "SAL", "EKE", "CHL", "DIAT")  # predictors FS
vars <- c("BAT", "SLP", "D2COL_CSH", "SST", "MLD", "SAL", "EKE", "CHL", "DIAT")  # predictors FS

# Females summer CPF
vars <- c("BAT", "SLP", "SST", "MLD", "SAL", "EKE", "DIAT")  # predictors FScpf



#-----------------------------------------------------------------
# Prepare data
#-----------------------------------------------------------------
# We also included a random number between 1 and 100 to serve as an indicator for variables that
# have influence greater or less than random (Scales et al., 2017; Soykan, Eguchi, Kohin, & Dewar, 2014);
# only variables with influence greater than the random number were included in the final models.
obs_file <- paste0(indir, "extract_GAZ_L4_PresAbs.csv")
s <- read.csv(obs_file)
s <- s[s$id %like% sexseason, ]
namesCols <- names(s[,1:8])

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

colnames(s)[1:8] <- namesCols
LocsAndEnvDF <- s
LocsAndEnvDF <- LocsAndEnvDF[LocsAndEnvDF$id %like% sexseason, ]

# Transform ice edge distance (change negative values to zero)
LocsAndEnvDF$EDGE[LocsAndEnvDF$EDGE < 0] <- 0

# Generate Random Number
LocsAndEnvDF$RN <- sample.int(100, size=nrow(LocsAndEnvDF), replace=T, prob=NULL)



#-----------------------------------------------------------------
# Prepare folds
#-----------------------------------------------------------------
# Leave-one-out strategy
# MW
n.folds <- 14

# FW
# Delete FW_39, it does not have absences, so the AUC.cv returned is -Inf 
# LocsAndEnvDF <- subset(LocsAndEnvDF, LocsAndEnvDF$id != "FW_39")
# n.folds <- 38

# FS
# n.folds <- 23

set.seed(123)
LocsAndEnvDF$id <- as.factor(LocsAndEnvDF$id)
f <- fold(data = LocsAndEnvDF, id_col = "id", method = "n_dist", k = n.folds)
unique(LocsAndEnvDF$id)

# Prepare
LocsAndEnvDF <- f %>%
                    dplyr::rename(fold = .folds) %>%
                    dplyr::mutate(fold = as.numeric(fold)) %>%
                    as.data.frame()

table(LocsAndEnvDF$fold)

LocsAndEnvDF %>%
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
tree.list <- seq(ini.nt,max.nt,by=step.nt) # List of trees for evaluation

# Define combination of hyper-parameters
comb <- expand.grid(lr=c(0.001, 0.005, 0.01, 0.05), tc=c(1,3,5), bf=c(0.5, 0.6, 0.7)) # Combination

# Prepare clusters
# detectCores()
cores <- 5 
cl <- makeCluster(cores)
registerDoParallel(cl)

# Presences
presences <- filter(LocsAndEnvDF, occ==1)
n_occ <- nrow(presences)
    
# Absences
absences <- filter(LocsAndEnvDF, occ==0)
abs_prop <- nrow(presences)/nrow(absences)
absences <- stratified(absences, c("id"), abs_prop)
    
# Combine presence-absences
LocsAndEnvDF <- bind_rows(presences, absences)
    
# Check number of occurrence per type
table(LocsAndEnvDF$occ)

# Convert to df, function does not like the tibble and shows a warning about some column not being numerical
LocsAndEnvDF <- as.data.frame(LocsAndEnvDF)
str(LocsAndEnvDF)

# Subsets for testing
# LocsAndEnvDF <- subset(LocsAndEnvDF, train$id== "FW_01")
# LocsAndEnvDF <- LocsAndEnvDF %>% group_by(occ, id) %>% slice_sample(n = 6)

# Fit BRT with all the combinations
set.seed(131)
system.time(
all_list <- foreach(i=1:nrow(comb), .packages=c("dismo", "gbm", "dplyr", "doParallel", "data.table")) %dopar% {
                      # Fit model
                      # Uses a block cross-validation
                      # Faster learning rate means larger values
                        mod <- tryCatch(dismo::gbm.step(data = LocsAndEnvDF, # data.frame with data
                                        gbm.x = vars,                        # Predictor variables
                                        gbm.y = 8,                           # Response variable
                                        family = "bernoulli",                # The nature of error structure
                                        tree.complexity = comb$tc[i],        # Tree complexity
                                        learning.rate = comb$lr[i],          # Learning rate
                                        bag.fraction = comb$bf[i],           # Bag fraction
                                        fold.vector = train$fold,
                                        n.folds = length(unique(train$fold)),
                                        n.trees = ini.nt, step.size = step.nt, max.trees = max.nt),
                                        error = function(e) return(NULL))
                                           
                                           # Keep CV parameters
                                             if(!is.null(mod)) {
                                                 mod_out <- data.frame(
                                                 tree.complexity = mod$interaction.depth,
                                                 learning.rate = mod$shrinkage,
                                                 ag.fraction = mod$bag.fraction,
                                                 n.trees = mod$n.trees,
                                                 AUC = mod$self.statistics$discrimination,
                                                 cv.AUC = mod$cv.statistics$discrimination.mean,
                                                 deviance = mod$self.statistics$mean.resid,
                                                 cv.deviance = mod$cv.statistics$deviance.mean,
                                                 PER = (1-mod$self.statistics$mean.resid/mod$self.statistics$mean.null)*100,
                                                 cv.PER = (1-mod$cv.statistics$deviance.mean/mod$self.statistics$mean.null)*100) 
                                                    
                                               # Keep deviance values for all trees
                                                 cv_deviance <- mod$cv.values
                                                 cv_deviance <- c(cv_deviance, rep(NA, length(tree.list) - length(cv_deviance)))  # Fill with NA
                                                    
                                               # Selected variables
                                                 pred_order <- summary(mod)$var
                                                 rn_position <- which(pred_order == "RN")
                                                 pred_list <- as.character(pred_order[1:(rn_position-1)])
                                                 list(mod_out = mod_out, cv_deviance = cv_deviance, pred_list = pred_list)
                             
                          }
                      }
                  )            
    
# Stop clusters
# stopCluster(cl)   
    
# Combine model outputs
mod_list <- foreach(i=1:nrow(comb)) %dopar% all_list[[i]]$mod_out
mod_list[!lengths(mod_list)] <-  list(data.frame(tree.complexity = NA))
mod_out <- rbindlist(mod_list, fill = TRUE)
mod_out <- bind_cols(comb,
                     dplyr::select(mod_out, -c(tree.complexity, learning.rate, bag.fraction))) %>%
                     dplyr::mutate(id = 1:n())
  
# Store file in csv format
outfile <- paste0(outdir, sep ="/",  "GAZ_BRT_settings_", sexseason, ".csv")
write.csv(mod_out,outfile)
    
# Combine deviance outputs
deviance_list <- list()

for(i in 1:nrow(mod_out)){
                          # Extract deviance data
                            dev <- all_list[[i]]$cv_deviance
                          # Check that there is no null data
                            if(is.null(dev)) dev <- rep(NA,length(tree.list))
                          # Make data.frame with number of trees
                            df <- data.frame(id = mod_out$id[i], lr = mod_out$lr[i], tc = mod_out$tc[i], bf = mod_out$bf[i], ntrees = tree.list, cv_deviance = dev)
                            deviance_list[[i]] <- df
                         }
   
cv_deviance <- rbindlist(deviance_list)
outfile <- paste0(outdir, sep ="/","GAZ_BRT_CVdeviance_", sexseason, ".csv")
write.csv(cv_deviance, outfile)
    
# Get selected variables
predict_list <- foreach(i=1:nrow(comb)) %dopar% all_list[[i]]$pred_list
outfile <- paste0(product_folder, sep ="/", "GAZ_BRT_predictList_", sexseason, ".rds")
saveRDS(predict_list, outfile)

theme_article<-  theme(legend.position = "none",
                        strip.text.y.right = element_blank(),
                        panel.grid.major.y = element_line( size=.1, color="grey50"),
                        axis.title.x = element_text(margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0)),
                        axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0)))
    
# Plot profiles trees vs. deviance
p <- ggplot(data = cv_deviance) +
            geom_line(data = dplyr::rename(cv_deviance, comb = id), aes(x = ntrees, y = cv_deviance, group = comb), color = "grey80") +
            geom_line(aes(x = ntrees, y = cv_deviance, group = id), color = "firebrick3") +
            scale_x_continuous(limits = c(0, max(cv_deviance$ntrees[!is.na(cv_deviance$cv_deviance)]))) +
            facet_wrap(id ~.,) 
      
outfile <- paste0(product_folder, sep ="/", "GAZ_BRT_optim_params_", sexseason, ".png")
ggsave(outfile, p, width=25, height=14, units="cm", dpi=300)
  
# Export outputs
outfile <- paste0(outdir, "/", sp_code, "_", mod_code, "_optim_params_", sexseason, ".csv")
write.csv(mod_out, outfile, row.names = FALSE)

outfile <- paste0(outdir, "/", sp_code, "_", mod_code, "_cv_deviance_", sexseason, ".csv")
write.csv(cv_deviance, outfile, row.names = FALSE)

outfile <- paste0(outdir, "/", sp_code, "_", mod_code, "_predlist_", sexseason, ".rds")
saveRDS(predict_list, outfile)


#-----------------------------------------------------------------
# Boosted Regression Tree - Fit full model
#-----------------------------------------------------------------
mod_out <- read.csv(paste0(product_folder, "/", sp_code, "_", mod_code, "_optim_params_", sexseason, ".csv"))
predict_list <- readRDS(paste0(product_folder, "/", sp_code, "_", mod_code, "_predlist_", sexseason, ".rds"))

# Tree ID for each sexseason combination 
# MW = 34
# FS = 33
# FW = 11
# FScpf = 10

# Get models with more than 1000 trees
bb <- subset(mod_out, mod_out$n.trees > 1000)
# From the ones with more than 1000 trees, get the one with the largest cross-validated AUC
cc <- bb[which.max(bb$cv.AUC),]
# Select model. Based on the parameters bur also by checking the tress vs. deviation curves
select_model_id <- 10 

# Get parameters from the selected model
tc <- mod_out$tc[select_model_id]
lr <- mod_out$lr[select_model_id]
bf <- mod_out$bf[select_model_id]
ntrees <- mod_out$n.trees[select_model_id]
pred_list <- vars[vars %in% predict_list[[select_model_id]]]

# Fit BRT with selected parameters
mod_full <- dismo::gbm.fixed(data = train,  # data.frame with data
                             gbm.x = pred_list,          # Predictor variables
                             gbm.y = 8,                  # Response variable
                             family = "bernoulli",       # The nature of errror structure
                             tree.complexity = tc,       # Tree complexity
                             learning.rate = lr,         # Learning rate
                             bag.fraction = bf,          # Bag fraction
                             n.trees = ntrees)           # Max number of trees

# Save model
outdir <- paste0(product_folder, "/", "full_model")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

saveRDS(mod_full, file = paste0(outdir, "/", sp_code, "_", mod_code, "_", sexseason, ".rds"))  # save model
mod_full <- readRDS(paste0(outdir, "/", sp_code, "_", mod_code, "_", sexseason, ".rds"))

# Plot variable contribution using radar plot
var_imp <- summary(mod_full)$rel.inf
names(var_imp) <- summary(mod_full)$var
pngfile <- paste0(outdir, "/", sp_code, "_", mod_code, "_var_radar_", sexseason, ".png")
png(pngfile, width=1000, height=1000, res=150)
radarPlot(var_imp, var_order=pred_list)
dev.off()

# Plot variable contribution using bar plot
pngfile <- paste0(outdir, "/", sp_code, "_", mod_code, "_var_influence_", sexseason, ".png")
png(pngfile, width=1000, height=1000, res=150)
ggBRT::ggInfluence(mod_full, show.signif = F, col.bar = "skyblue3")
dev.off()

# Plot response curves
pngfile <- paste0(outdir, "/", sp_code, "_", mod_code, "_response_", sexseason, ".png")
png(pngfile, width=1500, height=1500, res=200)
names(mod_full$gbm.call)[1] <- "dataframe"
ggBRT::ggPD(mod_full, n.plots =10, smooth = F, rug = F, ncol=2, col.line = "skyblue3")
dev.off()



#-----------------------------------------------------------------
# Boosted Regression Tree - Predict (Bootstrap approach)
#-----------------------------------------------------------------
# For each habitat selection model (i.e., each life-history stage of each species), we fitted the model 50 times.
# For each of the 50 iterations, we used the parameter values chosen for the final model, but we sampled
# half the data (with replacement) to fit the model. (Hindell et al. 2020).

# Set output directory
# Each bootstrap model is stored here
outdir_bootstrap <- paste0(outdir,"/bootstrap/")
if (!dir.exists(outdir_bootstrap)) dir.create(outdir_bootstrap, recursive = TRUE)

# Define number of bootstrap models
n.boot <- 50  # number of model fits

# Prepare clusters
cores <- 5
cl <- makeCluster(cores)
registerDoParallel(cl)

foreach(i=1:n.boot, .packages=c("dismo", "gbm", "dplyr", "splitstackshape", "stringr")) %dopar% {
                # sampled half the data (with replacement) to fit the model (Hindell et al. 2020)
                  idata <- stratified(train, c("occ", "id", "date"), 0.5, replace = TRUE)
                
                # fit BRT
                  mod_boot <- dismo::gbm.fixed(data = idata,             # data.frame with data
                                               gbm.x = pred_list,        # Predictor variables
                                               gbm.y = "occ",            # Response variable
                                               family = "bernoulli",     # The nature of errror structure
                                               tree.complexity = tc,     # Tree complexity
                                               learning.rate = lr,       # Learning rate
                                               bag.fraction = bf,        # Bag fraction
                                               n.trees = ntrees)         # Max number of trees
  
                # Store model
                  outfile <- paste0(outdir_bootstrap, str_pad(i, 2, pad = "0"), "_", sp_code, "_", mod_code, "_boot_", sexseason, ".rds")
                  saveRDS(mod_boot, file = outfile)  # save model
}

# Stop clusters
stopCluster(cl)
