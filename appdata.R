library(dplyr)
library(purrr)
library(ggplot2)

source("functions.R")
d <- readRDS("data/bootstrap_error.rds") %>% integrateVars

stats.mon <- d %>% group_by(Domain, Stat, Var, Month, Sample) %>%
  mutate(Rank=rank(Val)) %>% group_by(Domain, Stat, Var, Month, GCM) %>% 
  summarise(PropTop5=sum(Rank <= 5)/n(), Mean_Rank=mean(Rank), Min_Rank=min(Rank), Max_Rank=max(Rank))

stats.ann <- d %>% group_by(Domain, Stat, Var, Sample) %>% 
  mutate(Val=(Val-mean(Val))/sd(Val)) %>% group_by(Domain, Stat, Var, Sample, GCM) %>%
  summarise(Val=mean(Val)) %>% mutate(Rank=rank(Val)) %>% group_by(Domain, Stat, Var, GCM) %>% 
  summarise(PropTop5=sum(Rank <= 5)/n(), Mean_Rank=mean(Rank), Min_Rank=min(Rank), Max_Rank=max(Rank))

getSamples <- function(d, domain, stat){
  d %>% group_by(Domain, Var, Stat, Sample) %>% mutate(Val=(Val-mean(Val))/sd(Val)) %>%
  group_by(Domain, Stat, Sample, GCM) %>% summarise(Val=mean(Val)) %>% mutate(Rank=rank(Val)) %>% 
  filter(Domain==domain & Stat==stat)
}

prepAppData <- function(data, domain, stat, monthly){
  lev <- c("Random", "Selected", "Individual")
  file <- paste0("data/booterr_exact_", tolower(stat), "_", tolower(gsub("_", "", domain)), ".rds")
  d.comp <- readRDS(file) %>% filter(Stat==stat) %>% mutate(Group=factor(Group, levels=lev))
  samples <- getSamples(data, domain, stat)
  vars <- c("pr", "psl", "tas")
  d.re <- map(vars, ~gcmPrep(filter(d.comp, Var==.x), "random ensembles") %>% mutate(Var=.x)) %>%
    bind_rows(gcmPrep(d.comp, "random ensembles") %>% mutate(Var="Integrated"))
  
  d.sp <- map(vars, ~gcmPrep(filter(d.comp, Var==.x), "spatial bootstrap", offset=FALSE) %>%
                mutate(Var=.x)) %>% bind_rows(
                  gcmPrep(d.comp, "spatial bootstrap", offset=FALSE) %>% mutate(Var="Integrated")
                  ) %>% select(-Domain, -Stat, -Sample)
  
  joinStats <- function(x, y, var){
    lev <- levels(x$GCM)
    left_join(
      filter(x, Var==var & Group=="Individual"),
      filter(y, Var==var) %>% mutate(GCM=factor(GCM, levels=lev)), by=c("Var", "Month", "GCM")
    )
  }
  
  monthly <- filter(monthly, Domain==domain & Stat==stat)
  d.sp2 <- map(c(vars, "Integrated"), ~joinStats(d.sp, monthly, .x)) %>% bind_rows
  data <- list(samples=samples, sb.hm1=d.sp, sb.hm2=d.sp2, re=d.re)
  saveRDS(data, file=paste0("ar5eval/data/", stat, "_", domain, ".rds"))
}

walk2(rep(unique(d$Domain), 4), rep(as.character(unique(d$Stat)), each=9), ~prepAppData(d, .x, .y, stats.mon))
d <- list(sb.mon=stats.mon, sb.ann=stats.ann)
saveRDS(d, file="ar5eval/data/stats.rds")
