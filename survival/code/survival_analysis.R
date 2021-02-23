#########
# Arrange data for analyses
#########
library(tidyverse)
library(survival)
library(survminer)
library(cowplot)

dat <- read_tsv("survival/data/data_dwnld_day37.txt")

head(dat)

# Create separate dataframes for the 6 different groups
all <- data.frame(worm_number = NULL, time.event = NULL, status = NULL, strain = NULL)

exp <- unique(dat$experiment)

for(i in 1:length(exp)) {
    dat_sub <- dat %>% 
        filter(experiment == exp[i]) # Create vector to contain names of the different groups
    
    survival = data.frame(c(1:dat_sub$at_risk[1])) # make length equal the number of worms in each group
    names(survival) <- "worm_number" # Will hold the worms number
    survival$time.event <- NA # Holds information on time when event happened
    survival$status <- NA # status of event, dead or censored?
    survival$strain <- NA # strain information
    
    j = 1 # First position in vector to be filled
    
    for(k in 1:length(dat_sub$time)){
        if(dat_sub$dead[k] != 0){ # if worm died
            survival$time.event[j:(j + dat_sub$dead[k]-1)] <- rep(dat_sub$time[k], times = dat_sub$dead[k]) #repeat the time for the number of worms that died
            j = j + dat_sub$dead[k] #increment the position to be filled next
        }
    }
    survival$status[1:(j-1)] <- 2 # indicator for death = 2
    
    for(k in 1:length(dat_sub$time)){
        if(dat_sub$censored[k] != 0){ # if worms was censored
            survival$time.event[j:(j+dat_sub$censored[k]-1)] <- rep(dat_sub$time[k], times = dat_sub$censored[k]) 
            j = j + dat_sub$censored[k]
        }
    }
    survival$status[(sum(dat_sub$dead)+1):(j-1)] <- 1  # indicator for censoring
    survival$strain = exp[i]
    
    all <- rbind(all, survival) # bind all groups together
}

#########
# TEST SURVIVAL ANALYSES PACKAGES
#########

all_analyses <- all %>% 
    separate(strain, into = c("strain", "treatment")) %>% 
    mutate(trt = case_when(treatment == "A" ~ "DMSO",
                           treatment == "B" ~ "DDT"))
all_analyses$strain <- factor(all_analyses$strain, levels = c("N2", "BR5271", "BR5270"))

all_analyses$trt <- factor(all_analyses$trt, levels = c("DMSO", "DDT"))

fit <- survfit(Surv(time.event, status) ~ strain + trt, data = all_analyses)
print(fit)
# Summary of survival curves
summary(fit)

# Access to the sort summary table
summary(fit)$table

d <- data.frame(time = fit$time,
                n.risk = fit$n.risk,
                n.event = fit$n.event,
                n.censor = fit$n.censor,
                surv = fit$surv,
                upper = fit$upper,
                lower = fit$lower
)
head(d)

plot <- ggsurvplot(fit,
                   pval = FALSE, conf.int = FALSE,
                   risk.table = FALSE, # Add risk table
                   risk.table.col = "", # Change risk table color by groups
                   #surv.median.line = "hv", # Specify median survival
                   ggtheme = theme_bw(),
                   palette = c("#50BFC3", "#214D72", "#50BFC3", "#214D72", "#50BFC3", "#214D72"))

strain.labs = c("N2 (Control)", "BR5271 (Non-agg)", "BR5270 (Agg)")
names(strain.labs) <- c("N2", "BR5271", "BR5270")

#tiff("day37_km_sttrtain_11252020.tiff", height = 4, width = 6, res = 300, units = "in")
curve <- plot$plot + facet_wrap(strain~., nrow = 1, labeller = labeller(strain = strain.labs)) +
    theme(legend.position = "bottom", panel.grid = element_blank()) +
    labs(x = "Time (days)")
#dev.off()



bars <- summary(fit)$table %>% 
    as.data.frame() %>% 
    rownames_to_column("group") %>% 
    separate(group, into = c("strain", "trt"), sep = ",") %>% 
    mutate(strain.name = case_when(grepl("N2", strain) ~ "N2",
                                   grepl("BR5271", strain) ~ "BR5271",
                                   grepl("BR5270", strain) ~ "BR5270")) %>% 
    mutate(treatment = case_when(grepl("DMSO", trt) ~ "DMSO",
                                 grepl("DDT", trt) ~ "DDT"))
bars$strain.name <- factor(bars$strain.name, levels = c("N2", "BR5271", "BR5270"))
bars$treatment <- factor(bars$treatment, levels = c("DMSO", "DDT"))

bar.plot <- bars %>% 
    ggplot(aes(x = strain.name, y = `*rmean`, fill = treatment)) +
    geom_errorbar(aes(ymin = `*rmean` - `*se(rmean)`, ymax = `*rmean` + `*se(rmean)`), width=.2, position = position_dodge(width = 0.6)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.6) +
    theme_classic() +
    scale_fill_manual(values = c("#50BFC3", "#214D72")) +
    labs(x = "",
         y = "Mean lifespan (days)", 
         fill = "") +
    scale_x_discrete(labels = c("N2 \n(Control)", "BR5271 \n(Non-agg)", "BR5270 \n(Agg)")) +
    scale_y_continuous(breaks = c(5, 10, 15, 20, 25))+
    theme(legend.position = "bottom",
          legend.key.size = unit(0.8, "lines")) +
    geom_segment(aes(x=0.85, xend=2.85, y = 27, yend= 27)) +
    geom_segment(aes(x=0.85, xend=0.85, y = 27, yend= 26)) +
    geom_segment(aes(x=2.85, xend=2.85, y = 27, yend= 26)) +
    geom_segment(aes(x=1.85, xend=1.85, y = 27, yend= 26)) +
    annotate("text", x = 1.85, y = 28, label = "*", size = 6) +
    geom_segment(aes(x=2.85, xend=3.15, y = 14, yend= 14)) +
    annotate("text", x = 3, y = 15, label = "*", size = 6)

# Create the legend for survival curves
all_analyses$strain <- factor(all_analyses$strain, levels = c("N2", "BR5271", "BR5270"))
all_analyses$strain_trt <- paste0(all_analyses$strain, "_", all_analyses$trt)
all_analyses$strain_trt <- factor(all_analyses$strain_trt, levels = c( "BR5270_DMSO","N2_DMSO", "N2_DDT", "BR5271_DMSO", "BR5271_DDT", "BR5270_DDT"))

n2 <- all_analyses %>% 
    filter(strain == "N2") 

n2$strain_trt <- factor(n2$strain_trt, levels = c("N2_DMSO", "N2_DDT"))

fit.n2 <- survfit(Surv(time.event, status) ~ trt, data = n2)
plot.n2 <- ggsurvplot(fit.n2,
                      pval = FALSE, conf.int = FALSE,
                      risk.table = FALSE, # Add risk table
                      risk.table.col = "", # Change risk table color by groups
                      #surv.median.line = "hv", # Specify median survival
                      ggtheme = theme_bw(),
                      palette = c("#50BFC3", "#214D72", "#50BFC3", "#214D72", "#50BFC3", "#214D72"))

legend <- get_legend(
    # create some space to the left of the legend
    plot.n2$plot +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        scale_color_manual(labels = c("DMSO", "DDT"), values = c("#50BFC3", "#214D72"))
)

as_ggplot(legend)

curve_legend <- plot_grid(curve +theme(legend.position = "none"), legend, ncol = 1, rel_heights = c(1, .1))

#pdf("survival/figures/curves_bars.pdf", width = 8, height = 4)
#plot_grid(curve_legend , bar.plot, labels = c("C", "D"), label_size = 12, rel_widths = c(2,1))
#dev.off()

survival.plot <- plot_grid(curve_legend , bar.plot, labels = c("C", "D"), label_size = 12, rel_widths = c(2,1))
