###############
# COMPILE PLOTS TO MAKE FIGURES FOR MANUSCRIPT
###############

###############
# FIGURE 2
###############

source("DDT_levels/code/gc_levels.R")
source("worm_size/code/worm_size.R")

pdf("manuscript_figures/figure2.pdf", width =4.3, height = 8)
plot_grid(ddt, size, ncol = 1, rel_heights = c(0.35,1))
dev.off()

###############
# FIGURE 3
###############

source("swim_behavior/code/swim_behavior.R")
source("mitochondrial_function/code/mito_function_all.R")

pdf("manuscript_figures/figure3.pdf", width = 5, height = 13)
plot_grid(swim, seahorse, ncol = 1, rel_heights = c(0.7,1))
dev.off()

###############
# FIGURE 4
###############


###############
# FIGURE 5
###############
source("survival/code/survival_analysis.R")
source("learning_behavior/code/stam.R")

pdf("manuscript_figures/figure5.pdf", width = 8, height = 7)
plot_grid(stam.plot, survival.plot, ncol = 1)
dev.off()

