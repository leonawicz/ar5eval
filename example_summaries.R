library(dplyr)
library(purrr)

d <- readRDS("data/bootstrap_error.rds")

# Distributions
# Monthly error distribution by climate variable
d

# Mean annual error distribution by climate variable
d %>% group_by(Domain, Stat, GCM, Sample, Var) %>% summarise(Val=mean(Val))

# Standardized monthly error distribution by climate variable
d2 <- d %>% group_by(Domain, Stat, Sample, Var) %>% mutate(Val=(Val-mean(Val))/sd(Val))

# Standardized monthly error distribution
d2 %>% group_by(Domain, Stat, Month, GCM, Sample) %>% summarise(Val=mean(Val))

# Standardized mean annual error distribution by climate variable
d2 %>% group_by(Domain, Stat, GCM, Sample, Var) %>% summarise(Val=mean(Val))

# Standardized mean annual error distribution
d2 %>% group_by(Domain, Stat, GCM, Sample) %>% summarise(Val=mean(Val)) %>%
  arrange(Domain, Stat, Sample, Val)

# Means
# Mean monthly error by climate variable
d %>% group_by(Domain, Stat, Var, Month, GCM) %>% summarise(Mean=mean(Val)) %>% arrange(Domain, Stat, Var, Month, Mean)

# Mean annual error by climate variable
d %>% group_by(Domain, Stat, Var, GCM) %>% summarise(Mean=mean(Val)) %>% arrange(Domain, Stat, Var, Mean)

# Standardized mean annual error
d2 %>% group_by(Domain, Stat, GCM) %>% summarise(Mean=mean(Val)) %>% arrange(Domain, Stat, Mean)
