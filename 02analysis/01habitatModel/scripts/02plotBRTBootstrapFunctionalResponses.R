#---------------------------------------------------------------------------------------------------
# plot_brt_boot_partial          Plot BRT partial effects using bootstrap
#---------------------------------------------------------------------------------------------------
mod_code <- "BoostedRegressionTrees"
bootstrap <- T
n_boot <- 50
sp_code <- "GAZ" 
sexseason <- "MW"
# sexseason <- "FW"
# sexseason <- "FS"
# sexseason <- "FScpf"

#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------
indir <- here::here("InputOutput/00output/02habitatModel",  sexseason, mod_code, "full_model/")
outdir <- paste0(indir, "bootstrap")
if (!dir.exists(outdir_bootstrap)) dir.create(outdir_bootstrap, recursive = TRUE)

# Import model full model
mod <- readRDS(paste0(dirname(outdir), paste0("/GAZ_brt_", sexseason, ".rds")))

# List of bootstrap models
indir_bootstrap <- paste0(indir, "bootstrap")
boots_files <- list.files(indir_bootstrap, full.names = T, pattern = ".rds")

# Batch import of bootstrap models
models <- lapply(boots_files, readRDS)
n_models <- length(models)

# Make a list of values to predict per variable from a single model
n_res <- 100
gbm_list <- ggBRT::plot.gbm.4list(models[[1]], continuous.resolution = n_res)

# Get predictor names
pred.names <- models[[1]]$var.names
n_var <- length(pred.names)

# Create empty matrix to store data
boot_mat <- array(NA, dim=c(n_res, n_var, n_boot))

# This error:
# Error in .Call("gbm_plot", X = as.double(data.matrix(X)), cRows = as.integer(nrow(X)),:"gbm_plot" not available for .Call() for package "gbm"
# was solved by doing library(gbm)

for(i in 1:length(models)){
  # Get model
    mi <- models[[i]]
  # Predict values for list of values
    ipred_boot <- ggBRT::plot.gbm.boot(mi, list.4.preds = gbm_list, continuous.resolution = n_res)
  # Append data
    boot_mat[,,i] <- ipred_boot

                      }

# calculate median and CI per variable
boot_med <- apply(boot_mat, c(1,2), median, na.rm=T)
boot_cil <- apply(boot_mat, c(1,2), quantile, prob = 0.025, na.rm=T)
boot_ciu <- apply(boot_mat, c(1,2), quantile, prob = 0.975, na.rm=T)

# create a data.frame
data_list <- list()

for(i in 1:n_var){
    # Create data.frame
      idf <- data.frame(var = pred.names[i],
                        xval = gbm_list[[i]]$X1,
                        med = boot_med[,i],
                        cil = boot_cil[,i],
                        ciu = boot_ciu[,i])
    # Append
      data_list[[i]] <- idf
                    
                   }

# Combine data
data <- rbindlist(data_list)

# Relative importance
data$var <- factor(data$var, levels = mod$contributions$var)
relinf <- round(mod$contributions$rel.inf, 1)
labels <- paste0(mod$contributions$var, " (", relinf, "%)")
names(labels) <- mod$contributions$var

# Select number of variables to plot
n_plots <- 9
data2 <- filter(data, var %in% mod$contributions$var[1:n_plots])

# Plot
 p <- ggplot(data2, aes(x = xval)) +
            geom_ribbon(aes(ymin = cil, ymax = ciu), fill="steelblue", alpha=.2, linetype=0) +
            geom_line(aes(y = med), color="steelblue") +
            ylab("Fitted function") + xlab("") +
            facet_wrap(var~., scales = "free_x", ncol =2, strip.position = "top", labeller=labeller(var=labels)) +
            # theme_article(base_size = 14) +
            theme(strip.placement = "outside",
                  plot.margin = unit(c(10,10,10,10), "points"),
                  axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0)))+
                  theme_bw()
          
# Export plot
outfile <- paste0(indir_bootstrap, "/plots/", sp_code, "_", mod_code, "_response_boot_", sexseason,".png")
ggsave(outfile, p, width=18, height=22, units="cm", dpi=300)