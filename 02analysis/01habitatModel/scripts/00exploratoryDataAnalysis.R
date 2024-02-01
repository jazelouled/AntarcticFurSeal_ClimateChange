# Script hard-coded by Dr. David March (https://zenodo.org/badge/latestdoi/235335964). Adapted by Jazel Ouled-Cheikh. 

#---------------------------------------------------------------------------------------------------
# eda.R          Exploratory data analysis
#---------------------------------------------------------------------------------------------------

#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------
# Set sex-season combination. You need to set the one you are going to be working with before continuing
sexseason <- "MW"
# sexseason <- "FW"
# sexseason <- "FS"
# sexseason <- "FScpf" 

indir <- here::here("InputOutput/00output/01tracking/L4_locations_extract/")
outdir <- paste0(here::here("InputOutput/00output/02habitatModel/"), sexseason, "/", "ExploratoryDataAnalysis")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# Females in summer central place foraging
# indir <- here::here("InputOutput/00output/01tracking/L4_locations_extract_FScpf/")
# outdir <- paste0(here::here("InputOutput/00output/02habitatModel/"), sexseason, "/", "ExploratoryDataAnalysis")
# if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)



#-----------------------------------------------------------------
# Import observations
#-----------------------------------------------------------------
obs_file <- paste0(indir, "extract_GAZ_L4_PresAbs.csv")
s <- read.csv(obs_file)
s <- s[s$id %like% sexseason, ]
namesCols <- names(s[,1:8])


#-----------------------------------------------------------------
# Explore missing data in observed data
#-----------------------------------------------------------------
# names(s) <- ifelse(names(train) %like% "BAT", "BAT",
#             ifelse(names(s) %like% "SLP", "SLP",
#             ifelse(names(s) %like% "SDIST", "SDIST",
#             ifelse(names(s) %like% "D2COL_DEC", "D2COL_DEC",
#             ifelse(names(s) %like% "D2COL_CSH", "D2COL_CSH",
#             ifelse(names(s) %like% "chl", "CHL",
#             ifelse(names(s) %like% "edgedist", "EDGE",
#             ifelse(names(s) %like% "eke", "EKE",
#             ifelse(names(s) %like% "mlotstmax", "MLD",
#             ifelse(names(s) %like% "o2.surf", "O2",
#             ifelse(names(s) %like% "phydiat", "DIAT",
#             ifelse(names(s) %like% "siconc", "SIC",
#             ifelse(names(s) %like% "so.surf_gr", "SALg",
#             ifelse(names(s) %like% "so.surf", "SAL",
#             ifelse(names(s) %like% "thetao", "THETAO",
#             ifelse(names(s) %like% "tos_gr", "SSTg",
#             ifelse(names(s) %like% "tos", "SST",
#             ifelse(names(s) %like% "uo", "UO", 
#             ifelse(names(s) %like% "vo", "VO", "")))))))))))))))))))

vars <- c("BAT", "SLP", "SDIST", "D2COL_CSH", "EDGE", "SST", "SIC", "MLD", "SAL", "EKE", "CHL", "DIAT")  

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

data <- s

# Select observed data. We delete D2COL_CSH in the case of FScpf to avoid it explaining all of the variability. 
# df <- data %>% 
#   #dplyr::filter(occ == 1) %>%
#   dplyr::select(vars) %>% 
#   dplyr::select(-"D2COL_CSH")

# Plot missing data per variable
pngfile <- paste0(outdir, "/", sp_code, "_eda_missing_", sexseason, ".png")
png(pngfile, width=1500, height=1000, res=200)
p <- plot_Missing(df)
print(p)
dev.off()

# Calculate proportion of missing data
purrr::map(df, ~mean(is.na(.))*100) 

#-----------------------------------------------------------------
# Explore the correlation between variables in observed data
#-----------------------------------------------------------------
# Calculate correlations using Pearson
vars <- c("BAT", "SLP", "SDIST", "EDGE", "SST", "SIC", "MLD", "SAL", "EKE", "CHL", "DIAT")  # predictors MW

# vars <- c("BAT", "SLP", "SDIST", "D2COL_CSH", "EDGE", "SST", "SIC", "MLD", "SAL", "EKE", "CHL", "DIAT")  # predictors FS

# vars <- c("BAT", "SLP", "SDIST", "D2COL_CSH", "EDGE", "SST", "SIC", "MLD", "SAL", "EKE", "CHL", "DIAT")  # predictors FW

# FScpf We are not using D2COL_CSH to avoid it explaining all the variability and allow future environmental conditions to shape where could colonies establish
# vars <- c("BAT", "SLP", "SDIST", "EDGE", "SST", "SIC", "MLD", "SAL", "EKE", "CHL", "DIAT")  # predictors FScpf

correlations <- cor(na.omit(dplyr::select(df, vars)), method="pearson")

# Plot correlations
pngfile <- paste0(outdir, "/", sp_code, "_eda_corplot_", sexseason, ".png")
png(pngfile, width=2500, height=2000, res=300)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(correlations, method="color",col=col(200), tl.col = "black", order = "original", diag=FALSE, type="upper", 
         addCoef.col = "black") # Add coefficient of correlation
dev.off()

# Calcualate correlations using Spearman and clustering analysis
pngfile <- paste0(outdir, "/", sp_code, "_eda_cluster_", sexseason , ".png")
png(pngfile, width=2500, height=2000, res=300)
op <- par(mar=c(0.5,5,0.5,0.5))
v <- as.formula(paste("~", vars, collapse="+"))
plot(Hmisc::varclus(v, similarity=c("spearman"),data=df),cex=.8) # plot cluster
abline(a=0.30,0,col="grey70",lty=1,lwd=5)
par(op)
dev.off()



#-----------------------------------------------------------------
# Explore distribution of data per variables in both observed and simulated
#-----------------------------------------------------------------
# Convert to data.frame
data <- as.data.frame(data)
data$type <- NA
data$type[data$occ == 1] <- "pres"
data$type[data$occ == 0] <- "abs"

# Create plot per variable
p1 <- density.plot(title="", xlab="BAT", legend="", alpha=0.35, data=data, var=data$BAT, group=data$type)
p2 <- density.plot(title="", xlab="SLP", legend="", alpha=0.35, data=data, var=data$SLP, group=data$type)
p3 <- density.plot(title="", xlab="SDIST", legend="", alpha=0.35, data=data, var=data$SDIST, group=data$type)
p4 <- density.plot(title="", xlab="D2COL_CSH", legend="", alpha=0.35, data=data, var=data$D2COL_CSH, group=data$type)
p5 <- density.plot(title="", xlab="SST", legend="", alpha=0.35, data=data, var=data$SST, group=data$type)
p6 <- density.plot(title="", xlab="SAL", legend="", alpha=0.35, data=data, var=data$SAL, group=data$type)
p7 <- density.plot(title="", xlab="EKE", legend="", alpha=0.35, data=data, var=log1p(data$EKE), group=data$type)
p8 <- density.plot(title="", xlab="CHL", legend="", alpha=0.35, data=data, var=log1p(data$CHL), group=data$type)
p9 <- density.plot(title="", xlab="DIAT", legend="", alpha=0.35, data=data, var=log1p(data$DIAT), group=data$type)
p10 <- density.plot(title="", xlab="SIC", legend="", alpha=0.35, data=data, var=data$SIC, group=data$type)
p11 <- density.plot(title="", xlab="MLD", legend="", alpha=0.35, data=data, var=data$MLD, group=data$type)
p12 <- density.plot(title="", xlab="EDGE", legend="", alpha=0.35, data=data, var=data$EDGE, group=data$type)


# Create layout
lay <- rbind(c(1,2),
             c(3,4),
             c(5,6),
             c(7,8),
             c(9,10),
             c(11,12),
             13)

p <- grid.arrange(p1, p2, p4, p5,
                  p6, p7, p8, 
                  p9, p10, p11,p12,
                  layout_matrix = lay)


p <- grid.arrange(p1, p2, p3, p5,
                  p6, p7, p8, 
                  p9, p10, p11,p12,
                  layout_matrix = lay)


# Plot
pngfile <- paste0(outdir, "/", sp_code, "_eda_density_", sexseason, ".png")
png(pngfile, width=3000, height=3500, res=300)
grid.draw(p)
dev.off()



# Results of which variables to keep in each model:
# MW
# Keep: SST, CHL, SAL, BAT, EKE, SLP
# Get rid: EDGE, DIAT, MLD, SDIST, SIC

# FW
# Keep: SLP, D2COL_CSH, MLD, SAL, BAT, SDIST, EKE, SST, CHL
# Get rid: EDGE, DIAT, SIC 

# FS
# Keep: SST, CHL, DIAT, SLP, D2COL_CSH, BAT, EKE, MLD, SAL
# Get rid: SIC, SDIST, EDGE

# FScpf (we are not using D2COL_CSH to avoid it explaining all the variability and allow future environmental conditions to shape where could colonies establish)
# Keep: DIAT, SST, SLP, MLD, SAL, EKE, BAT
# Get rid: SIC, SDIST, EDGE, CHL