library(shiny)
library(shinyBS)
library(shinydashboard)
library(rintrojs)

d <- readRDS("data/stats.rds")
source("functions.R")
source("bootstrapmod.R")
source("compositemod.R")

domains <- c("Alaska"="AK", "Alaska (land)"="AK_land", "Alaska (ocean)"="AK_water", 
             "Canada"="CAN", "Alaska-Canada"="AKCAN", "60-90 North"="6090N",
             "20-90 North"="2090N", "Lower 48 states"="low48", "Pacific islands"="pacif")
err_stats <- c("RMSE", "RMSE (bias removed)"="RMSE0", "MAE", "MAE (bias removed)"="MAE0")
variables <- c("Integrated"="integrated", "Temperature"="tas", "Precipitation"="pr", "Sea level pressure"="psl")
grp_vars <- c("", "Spatial domain"="Domain", "Error statistic"="Stat", "Climate variable"="Var")
gcm_order <- c("Fixed (alphabetical)"="fixed", "Reorder each data set (mean rank)"="mean")

gcm_inclusion <- tagList(
  p("The order in which GCMs are including in composite models of increasing of size uses individual GCM rankings.
    Rank is based on mean annual estimated error for the selected climate variable over the selected
    spatial domain using the selected error statistic.", style="text-align: justify;")
)
