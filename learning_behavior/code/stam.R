######
# Plot results from STAM for BR5270, BR5271 and N2
###### 
library(tidyverse)
library(cowplot)

stam.both <- read_tsv("learning_behavior/data/raw_data.txt")

stam.both$time.point <- factor(stam.both$time.point, levels = c("Naive", "0hr"))

stam.both$trt <- factor(stam.both$trt, levels = c("DMSO", "DDT"))

stam.both$strain <- factor(stam.both$strain, levels = c("N2", "BR5271", "BR5270"))

mean.stam <- stam.both %>% 
    group_by(strain, trt, time.point) %>% 
    summarise(ci.avg = mean(ci), ci.sd = sd(ci)) %>% 
    mutate(mean.group = paste0(strain,  "_",  trt))

mean.stam$strain <- factor(mean.stam$strain, levels = c("N2", "BR5271", "BR5270"))


strain.labs = c("N2 (Control)", "BR5271 (Non-agg)", "BR5270 (Agg)")
names(strain.labs) <- c("N2", "BR5271", "BR5270")

line.plot <- stam.both %>% 
    ggplot() +
    geom_point(aes(x = time.point, y = ci, color = trt),alpha = 0.5, size = 1) +
    geom_line(aes(x = time.point, y = ci, group = group, color = trt), alpha = 0.4, linetype = "dotted") +
    geom_point(data = mean.stam, aes(x = time.point, y = ci.avg, color = trt), shape = 15, size = 3) +
    geom_line(data = mean.stam, aes(x = time.point, y = ci.avg, group = mean.group, color = trt), size = 0.8) +
    facet_wrap(~strain, labeller = labeller(strain = strain.labs)) +
    scale_color_manual(values = c("#50BFC3", "#214D72")) +
    theme_bw() +
    theme(legend.position = "bottom", panel.grid = element_blank(),
          panel.grid.major.x = element_line(colour = "grey86")) +
    labs(y = "Chemotaxis index",
         x = "Time point", 
         color  = "") +
    scale_x_discrete(labels = c("Naive", "Trained"))

summary(stam.both$tot)

bars.data <- read_tsv("learning_behavior/data/for_bars.txt")

naive <- bars.data %>%  
    mutate(group = paste0(strain,  "_",  trt, "_", rep, "_", run)) %>% 
    select(time.point, ci, group) %>% 
    filter(time.point == "Naive")%>% 
    filter(ci < 0.4 & ci > -0.2) %>% 
    rename(naive.ci = ci)

trained <- bars.data %>%  
    mutate(group = paste0(strain,  "_",  trt, "_", rep, "_", run)) %>% 
    select(time.point, ci, group) %>% 
    filter(time.point == "0hr") %>% 
    rename(trained.ci = ci)

bars <- merge(naive, trained, by = "group") %>% 
    mutate(li = trained.ci - naive.ci) %>%
    separate(group, into = c("strain", "trt", "rep", "run")) %>%
    group_by(strain, trt) %>% 
    summarise(mean.li = mean(li), sd.li = sd(li)) %>% 
    ungroup() 


bars$trt <- factor(bars$trt, levels = c("DMSO","DDT"))
bars$strain <- factor(bars$strain, levels = c("N2", "BR5271", "BR5270"))


bar.plot <- bars %>% 
    ggplot(aes(x = strain, y = mean.li, fill = trt)) +
    geom_errorbar(aes(ymin = mean.li - sd.li, ymax = mean.li + sd.li), position = position_dodge(width = 0.6), width = 0.15) +
    geom_bar(stat="identity", position = "dodge", width = 0.6) +
    scale_fill_manual(values = c("#50BFC3", "#214D72")) +
    theme_classic() +
    theme(legend.position = "bottom",
          legend.key.size = unit(0.8, "lines"),
          legend.text = element_text(size = 7.5)) +
    labs(x = "",
         y = "Learning Index", 
         fill = "") +
    scale_x_discrete(labels = c("N2 \n(Control)", "BR5271 \n(Non-agg)", "BR570 \n(Agg)"))

#pdf("learning_behavior/figures/lines_bars.pdf", width = 8, height = 4)
#plot_grid(line.plot, bar.plot, rel_widths = c(2, 1), labels = c("A", "B"), label_size = 12,
          #align = "hv", axis = "t")
#dev.off()

stam.plot <- plot_grid(line.plot, bar.plot, rel_widths = c(2, 1), labels = c("A", "B"), label_size = 12,
          align = "hv", axis = "t")
