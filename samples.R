library(dplyr)
library(purrr)
library(ggplot2)

source("functions.R")
d <- readRDS("data/bootstrap_error.rds") %>% integrateVars
d.comp <- readRDS("data/booterr_exact_rmse_ak.rds") %>% filter(Stat=="RMSE")

.plottheme <- theme(panel.grid.major=element_line(size = .5, color = "grey"),
                    plot.title=element_text(hjust=0),
                    axis.line=element_line(size=.7, color="black"),
                    axis.ticks.length=unit(0.35,"cm"),
                    legend.position="bottom",
                    text = element_text(size=14),
                    panel.spacing.x=unit(0.25,"cm"),
                    plot.margin=unit(c(0.5, 1, 0.5, 0.5),"cm"),
                    strip.text=element_text(size=14))

stats.mon <- d %>% group_by(Domain, Stat, Var, Month, Sample) %>%
  mutate(Rank=rank(Val)) %>% group_by(Domain, Stat, Var, Month, GCM) %>% 
  summarise(PropTop5=sum(Rank <= 5)/n(), Mean_Rank=mean(Rank), Min_Rank=min(Rank), Max_Rank=max(Rank))

stats.ann <- d %>% group_by(Domain, Stat, Var, Sample) %>% 
  mutate(Val=(Val-mean(Val))/sd(Val)) %>% group_by(Domain, Stat, Var, Sample, GCM) %>%
  summarise(Val=mean(Val)) %>% mutate(Rank=rank(Val)) %>% group_by(Domain, Stat, Var, GCM) %>% 
  summarise(PropTop5=sum(Rank <= 5)/n(), Mean_Rank=mean(Rank), Min_Rank=min(Rank), Max_Rank=max(Rank))

samples.ann <- d %>% group_by(Domain, Var, Stat, Sample) %>% mutate(Val=(Val-mean(Val))/sd(Val)) %>%
  group_by(Domain, Stat, Sample, GCM) %>% summarise(Val=mean(Val)) %>% mutate(Rank=rank(Val)) %>% 
  filter(Domain=="AK" & Stat=="RMSE")

# Plots
vars <- c("pr", "psl", "tas")
d.re <- map(vars, ~gcmPrep(filter(d.comp, Var==.x), "random ensembles") %>% mutate(Var=.x)) %>%
  bind_rows(gcmPrep(d.comp, "random ensembles") %>% mutate(Var="Integrated"))
gcmPlot(d.re, "Integrated", "histogram", size=5)
gcmPlot(d.re, "Integrated", "line")

d.sp <- map(vars, ~gcmPrep(filter(d.comp, Var==.x), "spatial bootstrap", offset=FALSE) %>% mutate(Var=.x)) %>%
  bind_rows(gcmPrep(d.comp, "spatial bootstrap", offset=FALSE) %>% mutate(Var="Integrated")) %>%
  select(-Domain, -Stat, -Sample)
gcmHeatmap(filter(d.sp, Var=="Integrated" & Group=="Individual"), "Month", "GCM")
gcmHeatmap(filter(d.sp, Var=="Integrated" & Group=="Selected"), "Month", "Composite")
gcmHeatmap(filter(d.sp, Var=="Integrated" & Group=="Random"), "Month", "Composite")

gcmHeatmap(filter(d.sp, Var=="Integrated" & Group=="Individual"), "Month", "GCM", lab="Val")

joinStats <- function(x, y, var){
  lev <- levels(x$GCM)
  left_join(
    filter(x, Var==var & Group=="Individual"),
    filter(y, Domain=="AK" & Stat=="RMSE" & Var==var) %>%
      mutate(GCM=factor(GCM, levels=lev)), by=c("Var", "Month", "GCM")
  )
}

d.sp2 <- map(c(vars, "Integrated"), ~joinStats(d.sp, stats.mon, .x)) %>% bind_rows
gcmHeatmap(filter(d.sp2, Var=="Integrated"), "Month", "GCM", lab="PropTop5")
