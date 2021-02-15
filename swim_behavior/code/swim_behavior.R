############################################################
# ANALYZE SWIM BEHAVIOR
# THERE ARE 4-5 RUNS PER STRAIN EXPOSED AND UNEXPOSED TO DDT
# FIGURES FOR THE MANUSCRIPT ARE SAVED
# AS ARE RESULTS OF TUKEY HSD POST AN ANOVA
############################################################

library(tidyverse)
library(gridExtra)
library(ggpubr)
library(cowplot)

three <- read_tsv("swim_behavior/data/raw_data_aggregated.txt")

# SEM to reduce day-to-day variability
run.summarised <- three %>% group_by(run, assay.name, strain.name, trt) %>% 
    summarise(mean = mean(measure, na.rm = T), sd = sd(measure, na.rm = T), n()) 

sem <- run.summarised %>% group_by(assay.name, strain.name, trt) %>% 
    summarise(sum.mean = mean(mean), se = sd(mean)/sqrt(length(mean)), n()) 

#sem <- read_tsv("swim_behavior/data/all_strains_sem.txt")

waveint <- sem %>% 
    filter(assay.name %in% c("WaveInitRate")) %>% 
    ggplot(aes(x=strain.name, y=sum.mean, fill=trt)) + 
    theme_classic() + 
    scale_fill_manual(values = c("#50BFC3", "#214D72"))+
    geom_errorbar(aes(ymin=sum.mean-se, ymax=sum.mean+se), width=.2, position = position_dodge(width = 0.58)) +
    geom_bar(position= "dodge", stat="identity", width = 0.6) +
    xlab("") +
    ylab("Wave initiation rate \n(bends/min)") +
    theme(plot.title = element_text(size = 11)) +
    guides(fill = guide_legend("")) +
    geom_segment(aes(x=0.85, xend=2.85, y=125, yend=125)) +
    annotate("text", x = 1.85, y = 130, label = "*", size = 4) +
    geom_segment(aes(x = 0.85, xend = 0.85, y = 120, yend = 125)) +
    geom_segment(aes(x = 2.85, xend = 2.85, y = 120, yend = 125)) +
    theme(legend.position = "bottom",
          legend.margin = margin(0,0,0,0),
          axis.text.x = element_text(size = 7.5)) +
    scale_x_discrete(labels = c("N2 \n(Control)", "BR5271 \n(Non-agg)", "BR5270 \n(Agg)"))


tr.speed <- sem %>% 
    filter(assay.name %in% c("TravelSpeed")) %>% 
    ggplot(aes(x=strain.name, y=sum.mean, fill=trt)) + 
    theme_classic() + 
    scale_fill_manual(values = c("#50BFC3", "#214D72"))+
    geom_errorbar(aes(ymin=sum.mean-se, ymax=sum.mean+se), width=.2, position = position_dodge(width = 0.58)) +
    geom_bar(position= "dodge", stat="identity", width = 0.6) +
    xlab("") +
    ylab("Travel speed \n(mm/min)") +
    theme(plot.title = element_text(size = 11)) +
    guides(fill = guide_legend("")) +
    geom_segment(aes(x=0.85, xend=2.85, y=12, yend=12)) +
    annotate("text", x = 1.85, y = 12.5, label = "*", size = 4) +
    geom_segment(aes(x = 0.85, xend = 0.85, y = 12, yend = 11.5)) +
    geom_segment(aes(x = 2.85, xend = 2.85, y = 12, yend = 11.5)) +
    theme(legend.position = "bottom",
          legend.margin = margin(0,0,0,0),
          axis.text.x = element_text(size = 7.5)) +
    scale_x_discrete(labels = c("N2 \n(Control)", "BR5271 \n(Non-agg)", "BR5270 \n(Agg)"))


curling <- sem %>% 
    filter(assay.name %in% c("Curling")) %>% 
    ggplot(aes(x=strain.name, y=sum.mean, fill=trt)) + 
    theme_classic() + 
    scale_fill_manual(values = c("#50BFC3", "#214D72"))+
    geom_errorbar(aes(ymin=sum.mean-se, ymax=sum.mean+se), width=0.2, position = position_dodge(width = 0.58)) +
    geom_bar(position= "dodge", stat="identity", width = 0.6) +
    xlab("") +
    ylab("Curling \n(% of time)") +
    theme(plot.title = element_text(size = 11)) +
    guides(fill = guide_legend("")) +
    geom_segment(aes(x=2.85, xend=3.15, y=2.7, yend=2.7)) +
    annotate("text", x = 3, y = 2.8, label = "*", size = 4) +
    theme(legend.position = "bottom",
          legend.margin = margin(0,0,0,0),
          axis.text.x = element_text(size = 7.5)) +
    scale_x_discrete(labels = c("N2 \n(Control)", "BR5271 \n(Non-agg)", "BR5270 \n(Agg)"))


activity <- sem %>% 
    filter(assay.name %in% c("ActivityIndex")) %>% 
    ggplot(aes(x=strain.name, y=sum.mean, fill=trt)) + 
    theme_classic() + 
    scale_fill_manual(values = c("#50BFC3", "#214D72"))+
    geom_errorbar(aes(ymin=sum.mean-se, ymax=sum.mean+se), width=.2, position = position_dodge(width = 0.58)) +
    geom_bar(position= "dodge", stat="identity", width = 0.6) +
    xlab("") +
    ylab("Activity Index \n(Body area/min)") +
    theme(plot.title = element_text(size = 11)) +
    guides(fill = guide_legend("")) +
    geom_segment(aes(x= 0.85, xend=2.85, y=505, yend=505)) +
    annotate("text", x = 1.85, y = 520, label = "*", size = 4) +
    geom_segment(aes(x = 0.85, xend = 0.85, y = 490, yend = 505)) +
    geom_segment(aes(x = 2.85, xend = 2.85, y = 490, yend = 505)) +
    theme(legend.position = "bottom",
          legend.margin = margin(0,0,0,0),
          axis.text.x = element_text(size = 7.5)) +
    scale_x_discrete(labels = c("N2 \n(Control)", "BR5271 \n(Non-agg)", "BR5270 \n(Agg)"))

prow <- plot_grid(
    waveint + theme(legend.position="none"),
    tr.speed + theme(legend.position="none"),
    curling + theme(legend.position="none"),
    activity + theme(legend.position="none"),
    align = 'vh',
    labels = c("A", "B", "C", "D"),
    label_size = 12,
    hjust = -1,
    nrow =2
)

legend_b <- get_legend(
    curling +
        guides(color = guide_legend(nrow = 1)) +
        theme(legend.position = "bottom")
)

plot_grid(prow, legend_b, ncol = 1, rel_heights = c(1, .1))

# To save plot as a pdf
#pdf("swim_behavior/figures/swim_main_figure.pdf", width = 6, height = 6)
#plot_grid(prow, legend_b, ncol = 1, rel_heights = c(1, .1))
#dev.off()

######################
# SUPPLEMENTAL FIGURE
######################
assay.labs = c(c("Body wave number \n(waves)", "Stretch \ncurvature range (/mm)", "Attenuation \n Amplitude loss (in %)", 
                 "Asymmetry \nAverage body curvature (/mm)", "ReverseSwim \nTime spent reversing (in %)", "BrushStroke \n(body area/stroke)"))
names(assay.labs) <- c("WaveNumber", "Stretch", "Attenuation", "Asymmetry", "ReverseSwim", "BrushStroke")

supplemental <- sem %>%
    filter(assay.name %in% c("WaveNumber", "Stretch", "Attenuation", "Asymmetry", "ReverseSwim", "BrushStroke")) %>% 
    ggplot(aes(x=strain.name, y= sum.mean, fill=trt)) + 
    theme_bw() + 
    scale_fill_manual(values = c("#50BFC3", "#214D72"))+
    geom_errorbar(aes(ymin=sum.mean-se, ymax=sum.mean+se), width=.2, position = position_dodge(width = 0.58)) +
    geom_bar(position= "dodge", stat="identity", width = 0.6) +
    xlab("") +
    ylab("") +
    theme(plot.title = element_text(size = 11)) +
    guides(fill = guide_legend("")) +
    theme(legend.position = "bottom",
          legend.margin = margin(0,0,0,0)) +
    facet_wrap(~assay.name, nrow  = 3, scales = "free", labeller = labeller(assay.name = assay.labs)) +
    scale_x_discrete(labels = c("N2 \n(Control)", "BR5271 \n(Non-agg)", "BR5270 \n(Agg)"))

# To save plot as a pdf
#pdf("swim_behavior/figures/swim_supplemental.pdf", width = 6, height = 10)
#supplemental
#dev.off()
