###############
# COMPILE PLOTS TO MAKE FIGURES FOR MANUSCRIPT
###############

source("DDT_levels/code/gc_levels.R")
source("worm_size/code/worm_size.R")

pdf("manuscript_figures/figure2.pdf", width =4.3, height = 8)
plot_grid(ddt, size, ncol = 1, rel_heights = c(0.35,1))
dev.off()

