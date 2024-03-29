###############
# COMPILE PLOTS TO MAKE FIGURES FOR MANUSCRIPT
###############


###############
# FIGURE 1
###############



###############
# FIGURE 2
###############

source("DDT_levels/code/gc_levels.R")
source("worm_size/code/worm_size.R")
source("egg_laying/code/egg_numbers.R")

row1 <- plot_grid(ddt_updated, egg_lay, ncol = 2)

pdf("manuscript_figures/figure2_updated.pdf", width =4.5, height = 7)
plot_grid(row1, size, ncol = 1, rel_heights = c(0.4,1))
dev.off()

###############
# FIGURE 3
###############

source("swim_behavior/code/swim_behavior.R")

pdf("manuscript_figures/figure3.pdf", width = 5, height = 5)
plot_grid(swim)
dev.off()

###############
# FIGURE 4
###############
source("mitochondrial_function/code/mito_function_all.R")

pdf("manuscript_figures/figure4.pdf", width = 5, height = 8)
plot_grid(seahorse)
dev.off()

###############
# FIGURE 5
###############

###############
# FIGURE 6
###############

###############
# FIGURE 7
###############
source("survival/code/survival_analysis.R")
source("learning_behavior/code/stam.R")

pdf("manuscript_figures/figure7.pdf", width = 8, height = 7)
plot_grid(stam.plot, survival.plot, ncol = 1)
dev.off()

