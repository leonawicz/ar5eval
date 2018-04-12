library(dplyr)
library(ggplot2)

d <- readRDS("ar5eval/data/stats.rds") # load any default local data sets
dataloc <- "s3://leonawicz/apps/ar5eval" # specify location for any external data sets
datasrc <- if(substr(dataloc, 1, 5)=="s3://") "aws" else "local" # Amazon Web Services or local data files
source("ar5eval/aws_key.R") # authentication to AWS
source("ar5eval/functions.R")

.theme <- snapplot::theme_snap(base_size = 10)

d_jant <- filter(d, Domain == "AK" & Stat == "RMSE" & Var == "tas" & Period == "Jan") %>%
    group_by(Domain, Stat, Var) %>% filter(rank(Mean_Rank) <= 21) %>% ungroup %>%
    mutate(GCM = factor(GCM, levels = GCM[order(Mean_Rank)]))
d_julp <- filter(d, Domain == "AK" & Stat == "RMSE" & Var == "pr" & Period == "Jul") %>%
  group_by(Domain, Stat, Var) %>% filter(rank(Mean_Rank) <= 21) %>% ungroup %>%
  mutate(GCM = factor(GCM, levels = GCM[order(Mean_Rank)]))

subtitle <- "based on January error metric"
g1a <- ggplot(d_jant, aes(x = GCM, y = Mean_Rank, ymin = Min_Rank, ymax = Max_Rank)) +
  geom_point() + geom_crossbar(width = 0.75) + .theme + 
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(title = "Spatial bootstrap GCM performance rankings", 
       subtitle = bquote(italic(.(subtitle))), x = NULL, y = "Bootstrap GCM rank range and mean")

subtitle <- "January spatial bootstrap of GCM ranking fifth or better"
g1b <- ggplot(d_jant, aes(x = GCM, y = PropTop5, ymin = Min_Rank, ymax = Max_Rank)) +
  geom_bar(stat = "identity", colour = "black", width = 0.75) + .theme + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(title = "Probability of GCM among top five performers", 
       subtitle = bquote(italic(.(subtitle))), x = "GCM", y = "P(among top five performing GCMs)")

subtitle <- "July spatial bootstrap of GCM ranking fifth or better"
g2 <- ggplot(d_julp, aes(x = GCM, y = PropTop5, ymin = Min_Rank, ymax = Max_Rank)) +
  geom_bar(stat = "identity", colour = "black", width = 0.75) + .theme + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(title = "Probability of GCM among top five performers", 
       subtitle = bquote(italic(.(subtitle))), x = "GCM", y = "P(among top five performing GCMs)")

png("plots/fig4a.png", width = 3000, height = 1000, res = 300, type = "cairo")
g1a
dev.off()

png("plots/fig4b.png", width = 3000, height = 1000, res = 300, type = "cairo")
g1b
dev.off()

png("plots/fig5.png", width = 3000, height = 1000, res = 300, type = "cairo")
g2
dev.off()

lab <- "Mean_Rank"
hm_title <- "Estimated monthly error by GCM"
hm_subtitle <- "Integrated variables are normalized"
lab_rnd <- 0

d2 <- aws.s3::s3readRDS(object = "s3://leonawicz/apps/ar5eval/RMSE_AK.rds")
d2a <- filter(d2$sb.hm2, Var == "integrated") %>% 
      mutate(GCM = factor(GCM, levels = rev(unique(GCM))))

.theme <- snapplot::theme_snap(base_size = 14)

g3 <- gcmHeatmap(d2a, "Month", "GCM", lab = lab, lab.rnd = lab_rnd,
                 title = hm_title, subtitle = hm_subtitle, xlb = "Month", ylb = "GCM") + 
  .theme + scale_y_discrete(position = "left", expand = c(0, 0))
  

png("plots/fig6.png", width = 2000, height = 3000, res = 300, type = "cairo")
g3
dev.off()

x <- d2$re
prime.lab <- "Estimated mean annual RMSE"
x <- group_by(x, Var, Composite, Group) %>% 
  summarise(LB = min(Val), p05 = quantile(Val, 0.05), Mean = mean(Val), UB = max(Val))

facet_labels <- c(integrated = "Integrated variable", pr = "Precipitation", 
                     psl = "Sea level pressure", tas = "Temperature")

.theme <- snapplot::theme_snap(base_size = 10)

g4 <- ggplot(x, aes(Composite, Mean, colour = Group, group = Group)) + 
  geom_ribbon(data=filter(x, Group == "Random"), aes(ymin = LB, ymax = UB), alpha = 0.3, colour = "white") +
  geom_line(data = filter(x, Group == "Random"), size = 1) +
  geom_line(data = filter(x, Group == "Selected"), size = 1) +
  geom_line(data = filter(x, Group == "Individual"), size = 1) +
  facet_wrap(~Var, scales = "free_y", labeller = labeller(Var = facet_labels)) +
  labs(title = "Estimated error", subtitle = expression(italic("by composite GCM size")),
       x = "Number of GCMs in composite", y = prime.lab) +
  scale_colour_manual(values = c("firebrick1", "black", "cornflowerblue")) +
  scale_x_continuous(breaks = 1:21) +
  geom_vline(xintercept = 5, size = 1, linetype = 2) + .theme

png("plots/fig8.png", width = 3000, height = 2000, res = 300, type = "cairo")
g4
dev.off()
