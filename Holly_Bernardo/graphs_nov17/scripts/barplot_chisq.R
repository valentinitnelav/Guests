###############################################################################
# Script to make barplot with counts or percent
###############################################################################

# install.packages("ggplot2")
library(ggplot2)
# install.packages("data.table")
library(data.table)

# =============================================================================
# Read & prepare data
# =============================================================================
mytbl <- fread("data/barplot_chisq/forvalentine.csv")

# explode table
mytbl_exploded <- mytbl[rep(x = 1:.N, times = Total)]

# or in case of data.frame
# mytbl_exploded <- mytbl[rep(x = seq_len(nrow(mytbl)), times = mytbl$Total), ]

# =============================================================================
# barplot of counts
# =============================================================================
plot_counts <- 
    ggplot(data = mytbl_exploded) +
    geom_bar(aes(x = Brush, 
                 fill = Deer)) + # fill with color using Deer variable
    # set range on OY axes and adjust the distance (gap) from OX axes
    scale_y_continuous(limits = c(0, 120), # means from 0 up to 120 on OY axis
                       # or instead of 120 above use something more general like:
                       # round(max(colSums(table(mytbl_exploded[,.(Deer,Brush)])))/10)*10
                       expand = c(0, 0)) + # adjust the distance (gap) from OX axes
    # set axis labels
    labs(x = "", 
         y = "counts") +
    theme_bw() + # eliminate default background 
    theme(panel.grid = element_blank())

# save as pdf
ggsave(plot_counts, file = "output/plot_counts.pdf", width=10, height=8, units="cm")
# save as png
ggsave(plot_counts, file = "output/plot_counts.png", width=10, height=8, units="cm", dpi=300)

# =============================================================================
# barplot of percent
# =============================================================================
plot_percent <- 
    ggplot(data = mytbl_exploded) +
    geom_bar(aes(x = Brush,
                 y = ..count.. / sum(..count..), # computes percent
                 fill = Deer)) +
    # set range on OY axes and adjust the distance (gap) from OX axes
    scale_y_continuous(limits = c(0, 1),
                       expand = c(0, 0)) + # adjust the distance (gap) from OX axes
    # set axis labels
    labs(x = "", 
         y = "percent") +
    theme_bw() + # eliminate default background 
    theme(panel.grid = element_blank())

# save as pdf
ggsave(plot_percent, file = "output/plot_percent.pdf", width=10, height=8, units="cm")
# save as png
ggsave(plot_percent, file = "output/plot_percent.png", width=10, height=8, units="cm", dpi=300)

