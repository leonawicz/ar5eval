setwd("/workspace/UA/mfleonawicz/projects/ar5eval/data")
library(raster)
library(parallel)
library(dplyr)
library(purrr)
eraDir <- "/Data/Base_Data/Climate/World/ERA40/historical_tifs/climatologies_1958_2001"
gcmDir <- "/Data/Base_Data/Climate/World/GCM_raw/IPCC_AR5_monthly/historical_tifs/era40grid_climatologies_1958_2001"
gcms <- list.files(gcmDir) %>% setdiff(c("CRU_TS31", "GISS-E2-H"))
era.lsm <- raster("/Data/Base_Data/Climate/World/ERA40/era40_LSM.nc")
source("../functions.R")

domain <- list(
  "AK_water"=extent(188,230,52,72),
  "AK_land"=extent(188,230,52,72),
  "AK"=extent(188,230,52,72),
  "CAN"=extent(219,308,49,72),
  "AKCAN"=extent(188,308,49,72),
  "6090N"=extent(-1.25,358.75,60,90),
  "2090N"=extent(-1.25,358.75,20,90),
  "low48"=extent(235,294,25,49),
  "pacif"=extent(132,208,-17,25)
)
type <- c("water", "land", rep("all", 7))

set.seed(358)
system.time( x <- mclapply(seq_along(domain), gcmEval, gcmDir=gcmDir, baseDir=eraDir,
                           surface.mask=era.lsm, bbox=domain, gcms=gcms, n=1000, type=type,
                           exact=FALSE, return.data=TRUE, mc.cores=length(domain)) )

system.time( d <- mclapply(seq_along(domain), gcmEval, gcmDir=gcmDir, baseDir=eraDir,
                           surface.mask=era.lsm, bbox=domain, gcms=gcms, n=1000, type=type, 
                           exact=FALSE, data=x, mc.cores=length(domain)) )
d <- bind_rows(d)
saveRDS(d, file="bootstrap_error.rds")

d.std <- d %>% group_by(Domain, Stat, Sample, Var) %>% mutate(Val=(Val-mean(Val))/sd(Val))
d.means <- d.std %>% group_by(Domain, Stat, GCM) %>% summarise(Mean=mean(Val)) %>% arrange(Domain, Stat, Mean)

# AK RMSE only
gcms.akrmse <- d.means %>% filter(Stat=="RMSE" & Domain=="AK") %>% ungroup %>% select(GCM) %>% unlist

system.time( d2 <- gcmEval(3, gcmDir=gcmDir, baseDir=eraDir, surface.mask=era.lsm, bbox=domain, 
                           gcms=gcms.akrmse, n=1000, type=type, composite=TRUE, exact=FALSE, data=x) )
saveRDS(d2, file="booterr_composites_fixed_ak_rmse.rds")

system.time( d3 <- gcmEval(3, gcmDir=gcmDir, baseDir=eraDir, surface.mask=era.lsm, bbox=domain,
                           gcms=gcms, n=1000, type=type, composite=TRUE, composite.size=1:21, exact=FALSE, data=x) )
saveRDS(d3, file="booterr_composites_random_ak_rmse.rds")
# End AK RMSE only

for(i in unique(d.means$Domain)){
  for(j in unique(d.means$Stat)){
    print(paste(i, ":", j))
    gcms.ordered <- d.means %>% filter(Stat==j & Domain==i) %>% ungroup %>% select(GCM) %>% unlist
    system.time( dx <- gcmEval(3, gcmDir=gcmDir, baseDir=eraDir, surface.mask=era.lsm, bbox=domain,
                               gcms=gcms.ordered, n=1000, type=type, composite=TRUE, composite.size=1:21, exact=TRUE, data=x) )
    saveRDS(dx, file=paste0("booterr_exact_", tolower(j), "_", tolower(gsub("_", "", i)), ".rds"))
  }
}
