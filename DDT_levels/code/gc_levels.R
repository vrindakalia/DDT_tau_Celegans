#################
# LEVELS OF DDT AND METABOLITES MEASURED IN WORMS
#################

# THERE ARE TWO TYPES OF MEASUREMENTS:
# 1. SAMPLE CONCENTRATION (NG OF CHEMICAL / WORM)
# 2. EXTRACT CONCENTRATION (AMOUNT/L, PPB)

# THERE ARE TWO TYPES OF COMPARISONS:
# 1. THREE LEVELS OF EXPOSURE IN WILDTYPE WORMS: 0, 0.3, 3, AND 30 uM
# 2. LEVELS OF 0.3uM DDT IN ALL THREE STRAINS COMPARED TO THE DMSO CONTROL

# Call in file for sample concentration analysis

library(tidyverse)
library(cowplot)
options(scipen=999)


sample.conc <- read_csv("DDT_levels/data/sample_conc.csv") %>% 
    gather(key = "chemical", value = "levels", -sample) %>% 
    mutate(group = paste0(unlist(lapply(str_split(sample, "_"), function(x) x[1])), "_",
                          unlist(lapply(str_split(sample, "_"), function(x) x[2])))) %>% 
    mutate(strain = unlist(lapply(str_split(sample, "_"), function(x) x[1]))) %>% 
    mutate_if(is.character, str_replace_all, pattern = "<LOD", replacement = "0") %>% 
    modify_at(c("levels"), as.numeric)

sample.conc.avg <- sample.conc %>% 
    mutate(levels.pg = levels * 1000) %>% 
    group_by(chemical, group) %>% 
    summarise(avg.level = mean(levels.pg), sd.level = sd(levels.pg))



sample.conc.avg$chemical <- factor(sample.conc.avg$chemical, levels = c("p,p'-DDT",
                                                                        "p,p'-DDE",
                                                                        "p,p'-DDD",
                                                                        "o,p'-DDT",
                                                                        "o,p'-DDE",
                                                                        "o,p'-DDD"))

n2_figure <- sample.conc.avg %>% 
    mutate(strain = unlist(lapply(str_split(group, "_"), function(x) x[1]))) %>% 
    mutate(exposure = unlist(lapply(str_split(group, "_"), function(x) x[2]))) %>% 
    filter(strain == "N2") %>% 
    filter(chemical %in% c("p,p'-DDT",
                           "p,p'-DDE")) %>% 
    mutate(pgmean = avg.level * 1000) %>% 
    ggplot(aes(x = fct_reorder(exposure, avg.level, mean), y = avg.level, fill = exposure)) +
    geom_errorbar(aes(ymin = avg.level - sd.level, ymax = avg.level + sd.level), width = 0.35) +
    geom_bar(stat = "identity") +
    facet_wrap(~chemical) +
    theme_bw() +
    labs(x = "",
         y = "Level of metabolite \n(pg/worm)",
         fill = "") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none",
          axis.text.x = element_text(size = 7)) +
    scale_fill_manual(values = c("grey79", "grey39", "grey59", "grey99")) 


n2.1 <- sample.conc.avg %>% 
    mutate(strain = unlist(lapply(str_split(group, "_"), function(x) x[1]))) %>% 
    mutate(exposure = unlist(lapply(str_split(group, "_"), function(x) x[2]))) %>% 
    filter(strain == "N2") %>% 
    filter(chemical %in% c("p,p'-DDT",
                           "p,p'-DDE",
                           "p,p'-DDD")) %>% 
    ggplot(aes(x = fct_reorder(exposure, avg.level, mean), y = avg.level, fill = exposure)) +
    geom_errorbar(aes(ymin = avg.level - sd.level, ymax = avg.level + sd.level), width = 0.25) +
    geom_bar(stat = "identity", width = 0.7) +
    facet_wrap(~chemical) +
    theme_bw() +
    labs(x = "",
         y = "Level of metabolite \n(pg/worm)",
         fill = "") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none",
          axis.text.x = element_text(size = 6, angle = 30, hjust = 1)) +
    scale_fill_manual(values = c("grey79", "grey39", "grey59", "grey99")) 

n2.2 <- sample.conc.avg %>% 
    mutate(strain = unlist(lapply(str_split(group, "_"), function(x) x[1]))) %>% 
    mutate(exposure = unlist(lapply(str_split(group, "_"), function(x) x[2]))) %>% 
    filter(strain == "N2") %>% 
    filter(chemical %in% c("o,p'-DDT",
                           "o,p'-DDE",
                           "o,p'-DDD")) %>% 
    ggplot(aes(x = fct_reorder(exposure, avg.level, mean), y = avg.level, fill = exposure)) +
    geom_errorbar(aes(ymin = avg.level - sd.level, ymax = avg.level + sd.level), width = 0.25) +
    geom_bar(stat = "identity", width = 0.7) +
    facet_wrap(~chemical) +
    theme_bw() +
    labs(x = "",
         y = "Level of metabolite \n(pg/worm)",
         fill = "") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none",
          axis.text.x = element_text(size = 6, angle = 30, hjust = 1)) +
    scale_fill_manual(values = c("grey79", "grey39", "grey59", "grey99")) 


all.1 <- sample.conc.avg %>% 
    mutate(strain = unlist(lapply(str_split(group, "_"), function(x) x[1]))) %>% 
    mutate(exposure = unlist(lapply(str_split(group, "_"), function(x) x[2]))) %>% 
    filter(chemical %in% c("p,p'-DDT",
                           "p,p'-DDE",
                           "p,p'-DDD")) %>% 
    filter(exposure %in% c("3uM")) %>% 
    ggplot(aes(x = fct_reorder(strain, avg.level, mean), y = avg.level)) +
    geom_errorbar(aes(ymin = avg.level - sd.level, ymax = avg.level + sd.level), width = 0.2) +
    geom_bar(stat = "identity", width = 0.6) +
    facet_wrap(~chemical) +
    theme_bw() +
    labs(x = "",
         y = "Level of metabolite \n(pg/worm)",
         fill = "") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none",
          axis.text.x = element_text(size = 6, angle = 30, hjust = 1)) +
    scale_x_discrete(labels = c("Agg", "N2", "Non-agg"))

all.2 <- sample.conc.avg %>% 
    mutate(strain = unlist(lapply(str_split(group, "_"), function(x) x[1]))) %>% 
    mutate(exposure = unlist(lapply(str_split(group, "_"), function(x) x[2]))) %>% 
    filter(chemical %in% c("o,p'-DDT",
                           "o,p'-DDE",
                           "o,p'-DDD")) %>% 
    filter(exposure %in% c("3uM")) %>% 
    ggplot(aes(x = fct_reorder(strain, avg.level, mean), y = avg.level)) +
    geom_errorbar(aes(ymin = avg.level - sd.level, ymax = avg.level + sd.level), width = 0.2) +
    geom_bar(stat = "identity", width = 0.6) +
    facet_wrap(~chemical) +
    theme_bw() +
    labs(x = "",
         y = "Level of metabolite \n(pg/worm)",
         fill = "") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none",
          axis.text.x = element_text(size = 6, angle = 30, hjust = 1)) +
    scale_x_discrete(labels = c("Agg", "N2", "Non-agg"))

n2 <- plot_grid(n2.1, n2.2, nrow = 2)
all <- plot_grid(all.1, all.2, nrow = 2)

#pdf("DDT_levels/figures/supplemental.pdf", width = 7, height = 6)
#plot_grid(n2, all, nrow = 1, labels = c("A", "B"))
#dev.off()


#pdf("DDT_levels/figures/main_n2.pdf", width = 4, height = 3)
#plot_grid(n2_figure, nrow = 1, labels = c("A"))
#dev.off()


