library(shiny)
library(shinydashboard)
d <- readRDS("data/stats.rds")
source("functions.R")
source("bootstrapmod.R")
source("mod.R")
domains <- c("Alaska"="AK", "Alaska (land)"="AK_land", "Alaska (ocean)"="AK_water", 
             "Canada"="CAN", "Alaska-Canada"="AKCAN", "60-90 North"="6090N",
             "20-90 North"="2090N", "Lower 48 states"="low48", "Pacific islands"="pacif")
err_stats <- c("RMSE", "RMSE (bias removed)"="RMSE0", "MAE", "MAE (bias removed)"="MAE0")
variables <- c("Integrated", "Temperature"="tas", "Precipitation"="pr", "Sea level pressure"="psl")
grp_vars <- c("", "Spatial domain"="Domain", "Error statistic"="Stat", "Climate variable"="Var")
