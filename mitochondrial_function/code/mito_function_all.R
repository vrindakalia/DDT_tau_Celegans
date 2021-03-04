#####################
# Analysis of mitochondrial function data across 5 different runs
#####################
# SETUP
library(tidyverse)
library(ggpubr)
library(cowplot)

# Call in cleaned data
resp <- read_tsv("mitochondrial_function/data/raw_data.txt") %>% 
    group_by(strain, treatment) %>% 
    summarise(mito.mean = mean(mito), mito.sd = sd(mito),
              max.mean = mean(max), max.sd = sd(max),
              azide.mean = mean(azide), azide.sd = sd(azide),
              spare.mean = mean(spare), spare.sd = sd(spare)) %>% 
    ungroup() %>% 
    mutate(gt = case_when(strain == "161" ~ "BR5270",
                          strain == "162" ~ "BR5271",
                          strain == "N2" ~ "N2")) 

resp$gt = factor(resp$gt, levels = c("N2","BR5271","BR5270"))

resp$treatment = factor(resp$treatment, levels = c("DMSO", "DDT"))

# Figures 
# Basal mitochondrial respiration    
mito <- resp %>% 
    ggplot(aes(x = gt, y = mito.mean, fill = treatment))  +
    geom_errorbar(aes(ymax = (mito.mean + mito.sd), ymin = (mito.mean - mito.sd)), position = position_dodge(width = 0.58), width = 0.2) +
    geom_bar(aes(fill = treatment), stat = "identity", position = "dodge", width = 0.6)  +
    scale_fill_manual(values = c( "#50BFC3", "#214D72")) +
    theme_classic() +
    theme(legend.position = "bottom", 
          axis.text.x = element_text(size = 9)) +
    ylab("Basal Respiration \n(pmol/min per worm)") +
    xlab("") +
    ylim(0, 25) +
    guides(fill = guide_legend("")) +
    theme(legend.position = "none") +
    geom_segment(aes(x=2.85, xend=3.25, y=12, yend=12)) +
    annotate("text", x = 3.05, y = 12.5, label = "*", size = 5) +
    geom_segment(aes(x=1.85, xend=2.25, y=18, yend=18)) +
    annotate("text", x = 2.05, y = 18.5, label = "***", size = 5) +
    geom_segment(aes(x=0.85, xend=1.25, y=23, yend=23)) +
    annotate("text", x = 1.05, y = 23.5, label = "***", size = 5) +
    geom_segment(aes(x=1.85, xend=2.85, y=23, yend=23)) +
    geom_segment(aes(x=1.85, xend=1.85, y = 22, yend= 23)) +
    geom_segment(aes(x=2.85, xend=2.85, y=22, yend=23)) +
    annotate("text", x = 2.35, y = 23.5, label = "***", size = 5) +
    scale_x_discrete(labels = c("N2 \n(Control)", "BR5271 \n(Non-agg)", "BR5270 \n(Agg)"))

# Maximal respiration
max <- resp %>% 
    ggplot(aes(x = gt, y = max.mean, fill = treatment))  +
    geom_errorbar(aes(ymax = (max.mean + max.sd), ymin = (max.mean - max.sd)), position = position_dodge(width = 0.58), width = 0.2) +
    geom_bar(aes(fill = treatment), stat = "identity", position = "dodge", width = 0.6)  +
    scale_fill_manual(values = c( "#50BFC3", "#214D72")) +
    theme_classic() +
    theme(legend.position = "bottom", 
          axis.text.x = element_text(size = 9)) +
    ylab("Maximal Respiration \n(pmol/min per worm)") +
    xlab("") +
    ylim(0, 45) +
    guides(fill = guide_legend("")) +
    theme(legend.position = "none") +
    geom_segment(aes(x=1.85, xend=2.25, y=38.5, yend=38.5)) +
    annotate("text", x = 2.05, y = 39.5, label = "***", size = 5) +
    geom_segment(aes(x=0.85, xend=1.25, y=38, yend=38)) +
    annotate("text", x = 1.05, y = 39.5, label = "***", size = 5) +
    geom_segment(aes(x=1.85, xend=2.85, y=43, yend=43)) +
    geom_segment(aes(x=1.85, xend=1.85, y = 41.5, yend = 43)) +
    geom_segment(aes(x=2.85, xend=2.85, y= 41.5, yend= 43)) +
    annotate("text", x = 2.35, y = 43.5, label = "***", size = 5) +
    geom_segment(aes(x=2.85, xend=3.25, y=23, yend=23)) +
    annotate("text", x = 3.05, y = 25.5, label = "ns", size = 3) +
    scale_x_discrete(labels = c("N2 \n(Control)", "BR5271 \n(Non-agg)", "BR5270 \n(Agg)"))

# Spare capacity
spare <- resp %>% 
    ggplot(aes(x = gt, y = spare.mean, fill = treatment))  +
    geom_errorbar(aes(ymax = (spare.mean + spare.sd), ymin = (spare.mean - spare.sd)), position = position_dodge(width = 0.6), width = 0.2) +
    geom_bar(aes(fill = treatment), stat = "identity", position = "dodge", width = 0.6)  +
    scale_fill_manual(values = c( "#50BFC3", "#214D72")) +
    theme_classic() +
    theme(legend.position = "bottom", 
          axis.text.x = element_text(size = 9)) +
    ylab("Spare capacity \n(pmol/min per worm)") +
    xlab("") +
    ylim(0, 35) +
    guides(fill = guide_legend("")) +
    theme(legend.position = "none") +
    geom_segment(aes(x=1.85, xend=2.25, y=28, yend=28)) +
    annotate("text", x = 2.05, y = 28.5, label = "***", size = 5) +
    geom_segment(aes(x=0.85, xend=1.25, y=24, yend=24)) +
    annotate("text", x = 1.05, y = 24.5, label = "***", size = 5) +
    geom_segment(aes(x=1.85, xend=2.85, y=32, yend=32)) +
    geom_segment(aes(x=1.85, xend=1.85, y = 31, yend=32)) +
    geom_segment(aes(x=2.85, xend=2.85, y=31, yend=32)) +
    annotate("text", x = 2.35, y = 32.5, label = "***", size = 5) +
    geom_segment(aes(x=2.85, xend=3.25, y=15, yend=15)) +
    annotate("text", x = 3.05, y = 17, label = "ns", size = 3) +
    geom_segment(aes(x=2.15, xend=2.15, y=3.883473, yend=3.883473+4.630356)) +
    scale_x_discrete(labels = c("N2 \n(Control)", "BR5271 \n(Non-agg)", "BR5270 \n(Agg)")) 

# Non-mitochondrial respiration
azide <- resp %>% 
    ggplot(aes(x = gt, y = azide.mean, fill = treatment))  +
    geom_errorbar(aes(ymax = (azide.mean + azide.sd), ymin = (azide.mean - azide.sd)), position = position_dodge(width = 0.58), width = 0.2) +
    geom_bar(aes(fill = treatment), stat = "identity", position = "dodge", width = 0.6)  +
    scale_fill_manual(values = c( "#50BFC3", "#214D72")) +
    theme_classic() +
    theme(legend.position = "bottom", 
          axis.text.x = element_text(size = 9)) +
    ylab("Non-mitochondrial\n(pmol/min per worm)") +
    xlab("") +
    ylim(0, 10) +
    guides(fill = guide_legend("")) +
    theme(legend.position = "bottom") +
    geom_segment(aes(x=1.85, xend=2.25, y=7.3, yend=7.3)) +
    annotate("text", x = 2.05, y = 7.4, label = "***", size = 5) +
    geom_segment(aes(x=0.85, xend=1.25, y=7.3, yend=7.3)) +
    annotate("text", x = 1.05, y = 7.4, label = "***", size = 5) +
    geom_segment(aes(x=1.85, xend=2.85, y=9, yend=9)) +
    geom_segment(aes(x=1.85, xend=1.85, y = 8.6, yend=9)) +
    geom_segment(aes(x=2.85, xend=2.85, y = 8.6, yend = 9)) +
    annotate("text", x = 2.35, y = 9.2, label = "***", size = 5) +
    geom_segment(aes(x=2.85, xend=3.25, y=2.7, yend=2.7)) +
    annotate("text", x = 3.05, y = 3.3, label = "ns", size = 3) +
    scale_x_discrete(labels = c("N2 \n(Control)", "BR5271 \n(Non-agg)", "BR5270 \n(Agg)")) 


##### Stats using all wells
all <- read_tsv("mitochondrial_function/data/raw_data.txt")

fit.max.all <- aov(max ~ group + factor(run), data = all)
anova(fit.max.all)
TukeyHSD(fit.max.all, "group")


fit.mito.all <- aov(mito ~ group + factor(run), data = all)
anova(fit.mito.all)
TukeyHSD(fit.mito.all, "group")


fit.azide.all <- aov(azide ~ group + factor(run), data = all)
anova(fit.azide.all)
TukeyHSD(fit.azide.all, "group")


fit.spare.all <- aov(spare ~ group + factor(run), data = all)
anova(fit.spare.all)
TukeyHSD(fit.spare.all, "group")

# Figure of respresentative OCR profile
#Read  in  the ocr file

ocr_raw <- read.table("mitochondrial_function/data/seahorse_08062020_ocr.txt",  header = T,  sep = "\t")

#Extract the wells of interest
wells <- c("B", "C", "D", "E", "F", "G")

ocr <- ocr_raw[grep(paste(wells, collapse="|"), ocr_raw$Well),]

# Worm count
worms <- c(13, 20, 8, 17, 24, 24, 21, 19, 12, 12, 28, 19,
           8, 5, 10, 8, 10, 12, 20, 33, 22, 28, 29, 30,
           15, 11, 5, 12, 12, 4, 13, 27, 21, 21, 32, 6,
           12, 14,13, 10, 6, 22, 35, 13, 19, 7, 5, 4, 
           6, 2, 6, 8, 10, 8, 15, 16, 14, 17, 13, 11,
           17, 11, 11, 9, 7, 4, 14, 22, 15, 10,12, 6)

# extract well ids to create dataframe with worm number data
well_id <- ocr$Well[grep(paste(wells, collapse="|"), ocr$Well)]

wells_worms <- cbind.data.frame(as.character(well_id), (worms))

names(wells_worms) <- c("Well", "no.worms")

ocr_worms <- ocr %>% 
    left_join(wells_worms, by ="Well") %>% 
    select(Measurement, OCR, Group, no.worms, Well) %>%
    mutate(ocr.p.worm = OCR/no.worms) %>%
    filter(no.worms >= 3 & no.worms <=31 ) 

ocr_worms_plot <- ocr_worms %>% 
    group_by(Group, Measurement)%>%
    summarize(mean.ocr.p.worm = mean(ocr.p.worm),  sd.ocr.p.worm = sd(ocr.p.worm),
              count = n()) %>%
    ungroup() %>%
    group_by() %>%
    mutate(group.plot = case_when(Group == "161 Control" ~ "BR5270 DMSO",
                                  Group == "WT Control" ~ "N2 DMSO",
                                  Group == "WT DDT" ~ "N2 DDT",
                                  Group == "mev-1 Control" ~ "mev-1 DMSO",
                                  Group == "161 DDT" ~ "BR5270 DDT",
                                  Group == "mev-1 DDT" ~ "mev-1 DDT",
                                  Group == "162 DDT" ~ "BR5271 DDT",
                                  Group == "162 Control" ~ "BR5271 DMSO")) %>%
    mutate(group.plot = fct_reorder(group.plot, mean.ocr.p.worm, .desc = T)) %>% 
    mutate(treatment = case_when(grepl("DMSO", group.plot) ~ "DMSO",
                                 grepl("DDT", group.plot) ~ "DDT")) %>% 
    mutate(strain = unlist(lapply(str_split(.$group.plot, " "), function(x) x[1])))  %>% 
    filter(strain != "mev-1")

ocr_control_plot <- ocr_worms_plot %>% 
    filter(treatment == "DMSO")

ocr_worms_plot$strain = factor(ocr_worms_plot$strain,  levels  = c("N2", "BR5271", "BR5270"))

ocr_worms_plot$treatment = factor(ocr_worms_plot$treatment,  levels  = c("DMSO", "DDT"))

strain.labs = c("N2 (Control)", "BR5271 (Non-agg)", "BR5270 (Agg)")
names(strain.labs) <- c("N2", "BR5271", "BR5270")

profile <- ggplot(data = ocr_worms_plot, mapping = aes(x = Measurement, y = mean.ocr.p.worm)) + 
    geom_line(aes(color = treatment))+
    #geom_errorbar(aes(ymin = mean.ocr.p.worm - sd.ocr.p.worm, ymax = mean.ocr.p.worm + sd.ocr.p.worm, color = group.plot), 
    # width = 0.2, alpha = 0.45) +
    geom_point(aes(color = treatment), shape = 1) +
    ylim(0, 48) +
    labs(
        x = "Measurement number",
        y = "Oxygen consumption rate\n(pmol/min per worm)"
    ) +
    scale_linetype_manual(values = c(2,1)) +
    scale_color_manual(values = c("#50BFC3", "#214D72")) +
    annotate("text",x = 5, y = 40, label = "FCCP", size = 3) +
    annotate("text", x = 15, y = 43, label = "Sodium \nazide", size = 3)+
    geom_segment(aes(x=5, xend=5, y=38, yend=0), color = "black", linetype = "dashed")+
    geom_segment(aes(x=15, xend=15, y=38, yend=0), color = "black", linetype = "dashed") +
    scale_x_continuous(breaks = seq(0,18,by=2)) +
    facet_wrap(~strain, nrow = 1, ncol = 3, scales = "free", labeller = labeller(strain = strain.labs)) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    theme(legend.position = "bottom") +
    theme(legend.title = element_blank(), axis.text.x = element_text(size = 5)) 

# Use cowplot to create a grid of figures for publication

legend_b <- get_legend(
    azide +
        guides(color = guide_legend(nrow = 1)) +
        theme(legend.position = "bottom")
)

legend_p <- get_legend(
    profile +
        guides(color = guide_legend(nrow = 1)) +
        theme(legend.position = "bottom")
)

bottom_row <- plot_grid(mito, max, spare, azide + theme(legend.position="none"), labels = c('B', 'C', "D", "E"), label_size = 12)
row_legend <- plot_grid(bottom_row, legend_b, ncol = 1, rel_heights = c(1, .1))

prof <- profile + theme(legend.position="none")
prof_legend <- plot_grid(prof, legend_p,  ncol = 1, rel_heights = c(1, .1))

seahorse <- plot_grid(prof_legend, row_legend, nrow = 2, rel_heights = c(1.5,3), labels = c("A", ""), label_size = 12)

# To save plot as a pdf
#pdf("mitochondrial_function/figures/ocr_bars.pdf", width = 7.5, height = 10)
#plot_grid(prof_legend, row_legend, nrow = 2, rel_heights = c(1.5,3), labels = c("A", ""), label_size = 12)
#dev.off()

