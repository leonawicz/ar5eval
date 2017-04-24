library(dplyr)
library(purrr)
library(ggplot2)
library(aws.s3)
source("ar5eval/aws_key.R") # authentication to AWS

source("functions.R")
d <- readRDS("data/bootstrap_error.rds") %>% integrateVars

stats.mon <- d %>% group_by(Domain, Stat, Var, Month, Sample) %>%
  mutate(Rank=rank(Val)) %>% group_by(Domain, Stat, Var, Month, GCM) %>% 
  summarise(PropTop5=sum(Rank <= 5)/n(), Mean_Rank=mean(Rank), Min_Rank=min(Rank), Max_Rank=max(Rank))

stats.ann <- d %>% group_by(Domain, Stat, Var, Sample) %>% 
  mutate(Val=(Val-mean(Val))/sd(Val)) %>% group_by(Domain, Stat, Var, Sample, GCM) %>%
  summarise(Val=mean(Val)) %>% mutate(Rank=rank(Val)) %>% group_by(Domain, Stat, Var, GCM) %>% 
  summarise(PropTop5=sum(Rank <= 5)/n(), Mean_Rank=mean(Rank), Min_Rank=min(Rank), Max_Rank=max(Rank))

stats <- bind_rows(
  ungroup(stats.mon) %>% rename(Period=Month),
  ungroup(stats.ann) %>% mutate(Period=factor("Annual", levels=levels(d$Month)))
)

saveRDS(stats, file="ar5eval/data/stats.rds")

getSamples <- function(d, domain, stat){
  d %>% group_by(Domain, Var, Stat, Sample) %>% mutate(Val=(Val-mean(Val))/sd(Val)) %>%
  group_by(Domain, Stat, Sample, GCM) %>% summarise(Val=mean(Val)) %>% mutate(Rank=rank(Val)) %>% 
  filter(Domain==domain & Stat==stat)
}

prepAppData <- function(data, domain, stat, monthly, local=TRUE){
  samples <- getSamples(data, domain, stat)
  vars=c("integrated", "pr", "psl", "tas")
  
  joinStats <- function(x, y, var){
    lev <- levels(x$GCM)
    left_join(
      filter(x, Var==var & Group=="Individual"),
      filter(y, Var==var) %>% mutate(GCM=factor(GCM, levels=lev)), by=c("Var", "Month", "GCM")
    )
  }
  
  d.re <- d.sp <- d.sp2 <- vector("list", length(vars))
  for(i in seq_along(vars)){
  file <- paste0("data/booterr_exact_", tolower(stat), "_", vars[i], "_", tolower(gsub("_", "", domain)), ".rds")
  d.comp <- readRDS(file) %>% filter(Stat==stat)
  d.re[[i]] <- map(vars[2:4], ~gcmPrep(filter(d.comp, Var==.x), "random ensembles") %>% mutate(Var=.x)) %>%
    bind_rows(gcmPrep(d.comp, "random ensembles") %>% mutate(Var="integrated")) %>%
    ungroup %>% mutate(Var=factor(Var, levels=vars)) %>% filter_(.dots=paste0("Var=='", vars[i], "'"))
  d.sp[[i]] <- map(vars[2:4], ~gcmPrep(filter(d.comp, Var==.x), "spatial bootstrap", offset=FALSE) %>% mutate(Var=.x)) %>%
    bind_rows(gcmPrep(d.comp, "spatial bootstrap", offset=FALSE) %>% mutate(Var="integrated")) %>% 
    select(-Domain, -Stat, -Sample) %>% ungroup %>% mutate(Var=factor(Var, levels=vars)) %>%
    filter_(.dots=paste0("Var=='", vars[i], "'"))
  
  monthly.tmp <   - filter(monthly, Domain==domain & Stat==stat & Var==vars[i])
  d.sp2[[i]] <- joinStats(d.sp[[i]], monthly.tmp, vars[i])
  }
  d.re <- bind_rows(d.re)
  d.sp <- bind_rows(d.sp)
  d.sp2 <- bind_rows(d.sp2) %>% mutate(Var=factor(Var, levels=vars))
  data <- list(samples=samples, sb.hm1=d.sp, sb.hm2=d.sp2, re=d.re)
  file <- paste0(stat, "_", domain, ".rds")
  if(local) saveRDS(data, file=paste0("ar5eval/data/", file))
  if(!local) s3saveRDS(data, object=paste0("s3://mleonawicz/apps/ar5eval/", file))
  cat(paste(file, "saved\n"))
}

walk2(rep(levels(d$Domain), 4), rep(levels(d$Stat), each=9), ~prepAppData(d, .x, .y, stats.mon, local=FALSE))
