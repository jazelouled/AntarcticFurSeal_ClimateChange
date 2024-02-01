#---------------------------------------------------------------------------------------------------
# 03plotBRTBootstrapVariableInfluence.R       Plot BRT variable relative incluence using bootstrap
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
indir <- here::here("InputOutput/00output/02habitatModel",  sexseason, mod_code, "full_model/bootstrap")
outdir <- paste0(indir, "/plots")
if (!dir.exists(outdir_bootstrap)) dir.create(outdir_bootstrap, recursive = TRUE)

# List of bootstrap models
boots_files <- list.files(indir, full.names = T, pattern = ".rds")

# Batch import of bootstrap models
models <- lapply(boots_files, readRDS)

# Create empty list to store data
data_list <- list()

for(i in 1:length(models)){
    # Get model
      mi <- models[[i]]
    # Get relative importance for variables
      df <- data.frame(boot = i, var = summary(mi)$var, rel.inf = summary(mi)$rel.inf)
    # Append data
      data_list[[i]] <- df
                           }

# Combine all data
data <- rbindlist(data_list)

# Calculate median and CI per variable
data <- data %>%
  dplyr::group_by(var) %>%
  dplyr::summarize(median = median(rel.inf),
                   cil = quantile(rel.inf, prob = 0.025),
                   ciu = quantile(rel.inf, prob = 0.975)) %>%
                   arrange(median)

# Reorder factors for plot in descending order
data$var <- factor(data$var, levels = data$var)

# Plot
p <- ggplot(data=data, mapping=aes(x=var, y=median, ymin=cil, ymax=ciu)) + 
            geom_pointrange(col="#377EB8") +
            coord_flip() +
            ylab("Relative influence (%)") + xlab("") + 
          # theme_article(base_size = 14) +
            theme(panel.grid.major.y = element_line( size=.1, color="grey50"),axis.title.x = element_text(margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0))) +
            theme_bw()

# Export plot
p_png <- paste0(outdir, "/", sp_code, "_", mod_code, "_var_influence_boot_", sexseason, ".png")
ggsave(p_png, p, width=14, height=12, units="cm", dpi=300)