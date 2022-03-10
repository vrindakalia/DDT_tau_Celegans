############################################################
# ANALYZE WORM SIZE DATA
############################################################

library(tidyverse)
library(ggpubr)
library(gridExtra)
library(cowplot)
# CALL IN L4 DATA

all_l4s <- read_tsv("worm_size/data/L4s_bars.txt") %>% 
    mutate(time = "46-to-50")

all_adults <- read_tsv("worm_size/data/adults_bars_size.txt") %>% 
    mutate(time = "70-to-72")

all_ever <- rbind(all_l4s, all_adults)

all_ever$trt <- factor(all_ever$trt, levels = c("DMSO", "DDT"))
all_ever$strain <- factor(all_ever$strain, levels = c("N2", "BR5271", "BR5270"))

l4s <- all_ever %>% 
    filter(time == "46-to-50") %>% 
    ggplot(aes(x = strain, y = mean_ext, fill = trt)) +
    geom_errorbar(aes(ymin = mean_ext - sd_ext, ymax = mean_ext + sd_ext),  position = position_dodge(width = 0.58), width = 0.2) +
    geom_bar(stat = "identity", position = "dodge", width = 0.6) +
    scale_fill_manual(values = c( "#50BFC3", "#214D72")) +
    theme_classic() +
    #theme(legend.position = c(0.94,0.98), 
    # axis.text.x = element_text(size = 9)) +
    labs(x = "",
         y = "Extinction", fill= "") +
    guides(shape = guide_legend(override.aes = list(size = 0.7))) +
    guides(fill = guide_legend(override.aes = list(size = 0.5))) +
    theme(legend.text = element_text(size = 7), axis.text.x = element_text(size = 7),
          plot.title = element_text(size = 10)) +
    geom_segment(aes(x=1.85, xend=2.25, y=1150, yend=1150)) +
    annotate("text", x = 2.05, y = 1175, label = "***", size = 4) +
    geom_segment(aes(x=0.85, xend=1.25, y=1080, yend=1080)) +
    annotate("text", x = 1.05, y = 1105, label = "***", size = 4) +
    geom_segment(aes(x=1.85, xend=2.85, y = 1300, yend = 1300)) +
    geom_segment(aes(x=1.85, xend=1.85, y = 1250, yend=1300)) +
    geom_segment(aes(x=2.85, xend=2.85, y=1250, yend=1300)) +
    annotate("text", x = 2.35, y = 1330, label = "***", size = 4) +
    geom_segment(aes(x=2.85, xend=3.25, y=875, yend=875)) +
    annotate("text", x = 3.05, y = 900, label = "***", size = 4) +
    scale_x_discrete(labels = c("N2 \n(Control)", "BR5271 \n(Non-agg)", "BR5270 \n(Agg)")) +
    ggtitle("Larval stage 4")

adults <- all_ever %>% 
    filter(time == "70-to-72") %>% 
    ggplot(aes(x = strain, y = mean_ext, fill = trt)) +
    geom_errorbar(aes(ymin = mean_ext - sd_ext, ymax = mean_ext + sd_ext),  position = position_dodge(width = 0.58), width = 0.2) +
    geom_bar(stat = "identity", position = "dodge", width = 0.6) +
    scale_fill_manual(values = c( "#50BFC3", "#214D72")) +
    theme_classic() +
    #theme(legend.position = c(0.94,0.98), 
    # axis.text.x = element_text(size = 9)) +
    labs(x = "",
         y = "Extinction", fill= "") +
    guides(shape = guide_legend(override.aes = list(size = 0.7))) +
    guides(fill = guide_legend(override.aes = list(size = 0.5))) +
    theme(legend.text = element_text(size = 7), axis.text.x = element_text(size = 7),
          plot.title = element_text(size = 10)) +
    geom_segment(aes(x=0.85, xend=3.25, y=2100, yend=2100)) +
    annotate("text", x = 2.05, y = 2125, label = "***", size = 4) +
    scale_x_discrete(labels = c("N2 \n(Control)", "BR5271 \n(Non-agg)", "BR5270 \n(Agg)")) +
    ggtitle("Day 1 adult")

l4s.tof <- all_ever %>% 
    filter(time == "46-to-50") %>% 
    ggplot(aes(x = strain, y = mean_tof, fill = trt)) +
    geom_errorbar(aes(ymin = mean_tof - sd_tof, ymax = mean_tof + sd_tof),  position = position_dodge(width = 0.7), width = 0.15) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    scale_fill_manual(values = c( "#50BFC3", "#214D72")) +
    theme_classic() +
    #theme(legend.position = c(0.94,0.98), 
    # axis.text.x = element_text(size = 9)) +
    labs(x = "",
         y = "Time of flight (TOF)", fill= "") +
    guides(shape = guide_legend(override.aes = list(size = 0.7))) +
    guides(fill = guide_legend(override.aes = list(size = 0.5))) +
    theme(legend.text = element_text(size = 7), axis.text.x = element_text(size = 7),
          plot.title = element_text(size = 10)) +
    geom_segment(aes(x=1.85, xend=2.25, y=1680, yend=1680)) +
    annotate("text", x = 2.05, y = 1700, label = "***", size = 4) +
    geom_segment(aes(x=0.85, xend=1.25, y=1680, yend=1680)) +
    annotate("text", x = 1.05, y = 1700, label = "***", size = 4) +
    geom_segment(aes(x=1.85, xend=2.85, y = 1900, yend = 1900)) +
    geom_segment(aes(x=1.85, xend=1.85, y = 1850, yend=1900)) +
    geom_segment(aes(x=2.85, xend=2.85, y=1850, yend=1900)) +
    annotate("text", x = 2.35, y = 1930, label = "***", size = 4) +
    geom_segment(aes(x=2.85, xend=3.25, y=1450, yend=1450)) +
    annotate("text", x = 3.05, y = 1480, label = "***", size = 4) +
    scale_x_discrete(labels = c("N2 \n(Control)", "BR5271 \n(Non-agg)", "BR5270 \n(Agg)"))+
    ggtitle("Larval stage 4")

adults.tof <- all_ever %>% 
    filter(time == "70-to-72") %>% 
    ggplot(aes(x = strain, y = mean_tof, fill = trt)) +
    geom_errorbar(aes(ymin = mean_tof - sd_tof, ymax = mean_tof + sd_tof),  position = position_dodge(width = 0.7), width = 0.15) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    scale_fill_manual(values = c( "#50BFC3", "#214D72")) +
    theme_classic() +
    #theme(legend.position = c(0.94,0.98), 
    # axis.text.x = element_text(size = 9)) +
    labs(x = "",
         y = "Time of flight (TOF)", fill= "") +
    ggtitle("70-72 hours") +
    guides(shape = guide_legend(override.aes = list(size = 0.7))) +
    guides(fill = guide_legend(override.aes = list(size = 0.5))) +
    theme(legend.text = element_text(size = 7), axis.text.x = element_text(size = 7),
          plot.title = element_text(size = 10)) +
    geom_segment(aes(x=0.85, xend=3.25, y=2500, yend=2500)) +
    annotate("text", x = 2.05, y = 2525, label = "***", size = 4) +
    scale_x_discrete(labels = c("N2 \n(Control)", "BR5271 \n(Non-agg)", "BR5270 \n(Agg)"))+
    ggtitle("Day 1 adult")


prow2 <- plot_grid(
    l4s + theme(legend.position="none"),
    l4s.tof + theme(legend.position="none"),
    align = 'vh',
    labels = c("C", "D"),
    hjust = -1,
    nrow = 1
)

prow3 <- plot_grid(
    adults + theme(legend.position="none"),
    adults.tof + theme(legend.position="none"),
    align = 'vh',
    labels = c("E", "F"),
    hjust = -1,
    nrow = 1
)

legend <- get_legend(
    # create some space to the left of the legend
    l4s +
        guides(color = guide_legend(nrow = 1)) +
        theme(legend.position = "bottom"))


size <- plot_grid(prow2, prow3, legend, ncol = 1, rel_heights = c(1,1,0.1))

# To save plot as a pdf
#pdf("worm_size/figures/extinction_l4_adult.pdf", width = 3, height = 6)
#plot_grid(prow, legend, ncol = 1, rel_heights = c(1, .1))
#dev.off()


