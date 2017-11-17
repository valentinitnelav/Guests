###############################################################################
# Script to make barplot with counts or percent
###############################################################################

# install.packages("ggplot2")
library(ggplot2)
# install.packages("data.table")
library(data.table)
# install.packages("scales")
library(scales)

# =============================================================================
# Read & prepare data
# =============================================================================
mytbl <- fread("data/barplot_chisq/forvalentine.csv")

# =============================================================================
# Barplot of counts
# =============================================================================
plot_counts <- 
  ggplot(data = mytbl) +
  geom_bar(aes(x = Brush,
               y = Total,
               fill = Deer), # fill with color using Deer variable
           stat = "identity") + 
  # Set the color fill manually
  scale_fill_manual(name = "Deer",
                    breaks = c('absent', 'present'),
                    values = c('absent'  = 'gray80',
                               'present' = 'gray50')) +
  # set order of discrete values on OX axes & adjust the distance (gap) from OY axes
  scale_x_discrete(limits = c("low", "high"),
                   labels = c("low", "high"),
                   expand = c(0, 0.6)) +
  # set range on OY axes and adjust the distance (gap) from OX axes
  scale_y_continuous(limits = c(0, 120), # means from 0 up to 120 on OY axis
                     # or instead of 120 above use something more general like:
                     # round(max(colSums(table(mytbl_exploded[,.(Deer,Brush)])))/10)*10
                     expand = c(0, 0)) + # adjust the distance (gap) from OX axes
  # set axis labels
  labs(x = "", 
       y = "Counts") +
  # Set the final aspect
  # Check more examples here: http://ggplot2.tidyverse.org/reference/ggtheme.html
  theme_bw() + # eliminate default background 
  theme(panel.grid = element_blank())

# save as pdf
ggsave(plot_counts, 
       file   = "output/barplot_chisq/plot_counts.pdf",
       height = 7,
       width  = 9,
       units  = "cm")
# save as png
ggsave(plot_counts, 
       file = "output/barplot_chisq/plot_counts.png", 
       height = 7,
       width  = 9,
       units  = "cm",
       dpi    = 300)

# =============================================================================
# Barplot of percent as chunk of all data
# =============================================================================
plot_percent_all <- 
  ggplot(data = mytbl) +
  geom_bar(aes(x = Brush,
               y = Total / sum(Total), # computes percent
               fill = Deer), # fill with color using Deer variable
           stat = "identity") +
  # Set the color fill manually
  scale_fill_manual(name = "Deer",
                    breaks = c('absent', 'present'),
                    values = c('absent'  = 'gray80',
                               'present' = 'gray50')) +
  # set order of discrete values on OX axes & adjust the distance (gap) from OY axes
  scale_x_discrete(limits = c("low", "high"),
                   labels = c("low", "high"),
                   expand = c(0, 0.6)) +
  # set range on OY axes and adjust the distance (gap) from OX axes
  scale_y_continuous(labels = percent_format(),
                     limits = c(0, 1),   # 1 corresponds to 100%
                     expand = c(0, 0)) + # adjust the distance (gap) from OX axes
  # set axis labels
  labs(x = "", 
       y = "Percent of total") +
  # Set the final aspect
  # Check more examples here: http://ggplot2.tidyverse.org/reference/ggtheme.html
  theme_bw() + # eliminate default background 
  theme(panel.grid = element_blank())

# save as pdf
ggsave(plot_percent_all, 
       file   = "output/barplot_chisq/plot_percent_of_all_data.pdf",
       height = 7,
       width  = 9,
       units  = "cm")
# save as png
ggsave(plot_percent_all, 
       file   = "output/barplot_chisq/plot_percent_of_all_data.png",
       height = 7,
       width  = 9,
       units  = "cm")

# =============================================================================
# Barplot of percent as chunk of each Brush category
# =============================================================================

# -----------------------------------------------------------------------------
# Using ggplot2
# -----------------------------------------------------------------------------
# ref: https://stackoverflow.com/questions/9563368/create-stacked-barplot-where-each-stack-is-scaled-to-sum-to-100

plot_percent_brush <-
  ggplot(data = mytbl) + 
  geom_bar(aes(x = Brush, 
               y = Total,
               fill = Deer), # fill with color using Deer variable
           stat = "identity",
           # position_fill "shows relative proportions at each x 
           # by stacking the bars and then standardising each bar 
           # to have the same height."
           # from http://ggplot2.tidyverse.org/reference/geom_bar.html
           position = position_fill()) + # or position = "fill"
  # Set the color fill manually
  scale_fill_manual(name = "Deer",
                    breaks = c('absent', 'present'),
                    values = c('absent'  = 'gray80',
                               'present' = 'gray50')) +
  # Set order of discrete values on OX axes & adjust the distance (gap) from OY axes
  scale_x_discrete(limits = c("low", "high"),
                   labels = c("low", "high"),
                   expand = c(0, 0.6)) +
  # Set range on OY axes and adjust the distance (gap) from OX axes
  scale_y_continuous(labels = percent_format(),
                     oob    = rescale_none, # https://stackoverflow.com/questions/38877095/manipulating-y-axis-limits-does-not-work-ggplot2-percent-bar-plot
                     limits = c(0, 1),   # 1 corresponds to 100%; make it 1.05 for example to create a gap on top of bars if needed
                     expand = c(0, 0)) + # adjust the distance (gap) from OX axes
  # Set axis labels
  labs(x = "", 
       y = "Percent of each category") +
  # Set the final aspect
  # Check more examples here: http://ggplot2.tidyverse.org/reference/ggtheme.html
  theme_bw() + # eliminate default background 
  theme(panel.grid = element_blank())

# save as pdf
ggsave(plot_percent_brush, 
       file   = "output/barplot_chisq/plot_percent_brush.pdf",
       height = 7,
       width  = 9,
       units  = "cm")
# save as png
ggsave(plot_percent_brush, 
       file   = "output/barplot_chisq/plot_percent_brush.png",
       height = 7,
       width  = 9,
       units  = "cm")

# -----------------------------------------------------------------------------
# Using basic plot functions
# -----------------------------------------------------------------------------

# prepare data in needed format
mytbl_m <- matrix(mytbl$Total, nrow = 2, byrow = TRUE)
rownames(mytbl_m) <- c("absent", "present")
colnames(mytbl_m) <- c("low", "high")
mytbl_m
mytbl_m_prop <- prop.table(mytbl_m, 2)

par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)

barplot(mytbl_m_prop, col = c("gray80", "gray50"))
legend("topright", 
       fill = c("gray80", "gray50"),
       legend = c("absent", "present"),
       inset = c(-0.15,0))

# reset par to the default values
dev.off() 

# check otehr examples at
# http://www.thecoatlessprofessor.com/programming/creating-stacked-barplot-and-grouped-barplot-in-r-using-base-graphics-no-ggplot2/
