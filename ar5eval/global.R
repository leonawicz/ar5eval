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
  p("The composite GCMs shown here use a fixed ensemble membership order that does not vary with climate variable.
    Order of inclusion follows individual GCM rankings based on mean annual estimated error over the selected
    spatial domain using the selected error statistic for the integrated climate variable.
    This is why when viewing precipitation, sea level pressure or temperature, test results may appear
    less optimal and the orange line representing individual GCM error in the second permutation results plot 
    does not always increase monotonically like it does when viewing results for the integrated variable.", style="text-align: justify;"),
  p("Results would be different for the three distinct climate variables if the order of GCM inclusion in ensembles of
    increasing size were based on the respective climate variable-specific GCM performance rankings.
    However, this evaluation does not aim to rank and select GCMs based on a single climate variable.
    Instead, what is more clear is how cross-variable model selection merges GCM performance from all three
    climate variables and that it may yield a composite model set that is notably different 
    from those that would be chosen using a single climate variable.", style="text-align: justify;")
)
