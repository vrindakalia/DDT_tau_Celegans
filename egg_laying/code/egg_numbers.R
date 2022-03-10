################################################
# DDT and egg laying behavior
# 02.16.2022
################################################
library(tidyverse)
library(readxl)
library(lmtest)

raw_1 <- read_excel("egg_laying/data/DDT egg-counting_raw counts_2-15-22_VK.xlsx", sheet = 1) %>% 
    gather(key = group, value = count) %>% 
    separate(group, into = c("strain", "trt"), sep = " ", remove = FALSE) %>% 
    mutate(run = 1)

raw_2 <- read_excel("egg_laying/data/DDT egg-counting_raw counts_2-15-22_VK.xlsx", sheet = 3) %>% 
    gather(key = group, value = count) %>% 
    separate(group, into = c("strain", "trt"), sep = " ", remove = FALSE) %>% 
    mutate(run = 2)

raw_3 <- read_excel("egg_laying/data/DDT egg-counting_raw counts_2-15-22_VK.xlsx", sheet = 4) %>% 
    gather(key = group, value = count) %>% 
    separate(group, into = c("strain", "trt"), sep = " ", remove = FALSE) %>% 
    mutate(run = 3)

raw_4 <- read_excel("egg_laying/data/DDT egg-counting_raw counts_2-15-22_VK.xlsx", sheet = 5) %>% 
    gather(key = group, value = count) %>% 
    separate(group, into = c("strain", "trt"), sep = " ", remove = FALSE) %>% 
    mutate(run = 4)

data_all <- rbind(raw_1, raw_2, raw_3, raw_4)

data_all$strain <- factor(data_all$strain, levels = c("N2","BR5271", "BR5270"))
data_all$trt <- factor(data_all$trt, levels = c("DMSO", "DDT"))

eggs.fit <- glm(count ~ factor(strain)*factor(trt) + factor(run), family="poisson", data = data_all)
#summary(eggs.fit)

eggs.fit.No.int <- glm(count ~ factor(strain) + factor(trt) + factor(run), family="poisson", data = data_all)
#lrtest(eggs.fit, eggs.fit.No.int) #interaction term not significant 

data.plot <- data_all %>% 
    group_by(strain,trt) %>% 
    summarise(mean.count = mean(count), sd.count = sd(count)) %>% 
    ungroup() 

eggs <- data.plot %>% 
    ggplot(aes(x = strain, y = mean.count, fill = trt)) +
    geom_errorbar(aes(ymin = mean.count - 1, ymax = mean.count + sd.count),  position = position_dodge(width = 0.58), width = 0.2) +
    geom_bar(stat = "identity", position = "dodge", width = 0.6) +
    scale_fill_manual(values = c( "#50BFC3", "#214D72")) +
    theme_classic() +
    theme(legend.position = "none", 
          axis.text.x = element_text(size = 9)) +
    labs(x = "",
         y = "Number of eggs", fill= "") +
    guides(fill = guide_legend(override.aes = list(size = 0.5))) +
    theme(legend.text = element_text(size = 7), axis.text.x = element_text(size = 7),
          plot.title = element_text(size = 10)) +
    geom_segment(aes(x=2.85, xend=3.25, y=12, yend=12)) +
    annotate("text", x = 3.05, y = 12.5, label = "*", size = 4) +
    geom_segment(aes(x=1.85, xend=2.25, y=18, yend=18)) +
    annotate("text", x = 2.05, y = 18.5, label = "***", size = 4) +
    geom_segment(aes(x=0.85, xend=1.25, y=19, yend=19)) +
    annotate("text", x = 1.05, y = 20, label = "***", size = 4) +
    geom_segment(aes(x=1.85, xend=2.85, y=21, yend=21)) +
    geom_segment(aes(x=1.85, xend=1.85, y = 20.5, yend= 21.1)) +
    geom_segment(aes(x=2.85, xend=2.85, y=20.5, yend=21.1)) +
    annotate("text", x = 2.35, y = 21.5, label = "***", size = 4) +
    scale_x_discrete(labels = c("N2 \n(Control)", "BR5271 \n(Non-agg)", "BR5270 \n(Agg)")) +
    ggtitle("Egg laying at Day 1")

#pdf("egg_laying/figures/main_egg_lay.pdf", width = 4, height = 3)
egg_lay <- plot_grid(eggs, nrow = 1, labels = c("B"))
#dev.off()
